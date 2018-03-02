/* 
 * RPC service implementation on infiniband. 
 *
 * Tong Jin (2011) TASSL Rutgers University
 * Hoang Bui (2012-2013) TASSL Rutgers University
 * hbui@cac.rutgers.edu
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <ifaddrs.h>
#include <netdb.h>
#include <unistd.h>
#include <rdma/rdma_cma.h>
#include <sys/time.h>
#include <poll.h>
#include "list.h"
//#include "dart_rpc_ib.h"
#include "ds_base_ib.h"

#include "debug.h"
#include <pthread.h>  //Yubo

#define SYS_WAIT_COMPLETION(x)					\
	while (!(x)) {						\
		err = sys_process_event(rpc_s);			\
		if (err < 0)					\
			goto err_out;				\
	}

const int BUFFER_SIZE = 1024;	//TODO: rpc_cmd size

#define myid(rpc_s)		(rpc_s->ptlmap.id)
#define rank2id(rpc_s, rank)	((rank) + (rpc_s->app_minid))
#define id2rank(rpc_s, id)	((id) - (rpc_s->app_minid))
#define myrank(rpc_s)		id2rank(rpc_s, myid(rpc_s))

#define PORTMAX 65536
#define ERRORIP "cannot find host ip"

#define MD_USE_INC(rpc_s)	rpc_s->num_md_posted++
#define MD_USE_DEC(rpc_s)	rpc_s->num_md_unlinked++

static int num_service = 0;

static struct rpc_server *rpc_s_instance;
static struct {
	enum cmd_type rpc_cmd;
	rpc_service rpc_func;
} rpc_commands[64];


//Yubo use for profilling
double timer_timestamp_0(void)
{
        double ret;

#ifdef XT3
        ret = dclock();
#else
        struct timeval tv;

        gettimeofday( &tv, 0 );
        ret = (double) tv.tv_usec + tv.tv_sec * 1.e6;
#endif
        return ret;
}




static int sys_send(struct rpc_server *rpc_s, struct node_id *to, struct hdr_sys *hs);
/* =====================================================
	Fabric:
	(1)barrier functions
	(2)rpc operation functions
	(3)message passing & data transfering functions
	(4)network/device/IB system operation
	Public APIs
 =====================================================*/

double gettime()
{
	struct timeval tm;
	gettimeofday(&tm, NULL);
	return ((double) tm.tv_sec * 1000000 + (double) tm.tv_usec) / 1000000.0;
}


static struct node_id *rpc_get_peer(struct rpc_server *rpc_s, int peer_id);
/* -------------------------------------------------------------------
  System barrier implementation.
------------------------------------------------------------------- */
int log2_ceil(int n)		//Done
{
	unsigned int i;
	int k = -1;

	i = ~(~0U >> 1);
	while(i && !(i & n))
		i = i >> 1;
	if(i != n)
		i = i << 1;

	while(i) {
		k++;
		i = i >> 1;
	}

	return k;
}

static int sys_bar_arrive(struct rpc_server *rpc_s, struct hdr_sys *hs)	//Done
{
	rpc_s->bar_tab[hs->sys_id] = hs->sys_pad1;
	return 0;
}

struct node_id *rpc_server_find(struct rpc_server *rpc_s, int nodeid)
{
	struct node_id *temp_peer;
	list_for_each_entry(temp_peer, &rpc_s->peer_list, struct node_id, peer_entry) {
		if(temp_peer->ptlmap.id == nodeid)
			return temp_peer;
	}
	return 0;
}

void rpc_server_find_local_peers(struct rpc_server *rpc_s,
    struct node_id **peer_tab, int *num_local_peer, int peer_tab_size)
{
    // find all peers (include current peer itself) that reside on the
    // same compute node as current peer
    int i = 0;
    struct node_id *temp_peer;
    list_for_each_entry(temp_peer, &rpc_s->peer_list, struct node_id, peer_entry) {
        uint32_t my_addr = rpc_s->ptlmap.address.sin_addr.s_addr;
        uint32_t remote_addr = temp_peer->ptlmap.address.sin_addr.s_addr;
        if (my_addr == remote_addr) {
            peer_tab[i++] = temp_peer;
        }
    }
    *num_local_peer = i;
}

uint32_t rpc_server_get_nid(struct rpc_server *rpc_s)
{
    return rpc_s->ptlmap.address.sin_addr.s_addr;
} 

static int sys_bar_send(struct rpc_server *rpc_s, int peerid)	//Done
{
	struct node_id *peer = rpc_get_peer(rpc_s, peerid);
	struct hdr_sys hs;
	int err;

	memset(&hs, 0, sizeof(struct hdr_sys));
	hs.sys_cmd = sys_bar_enter;
	hs.sys_pad1 = rpc_s->bar_num;
	hs.sys_id = myrank(rpc_s);

	err = sys_send(rpc_s, peer, &hs);
	if(err != 0)
		printf("(%s) failed with (%d).\n", __func__, err);
	return err;
}

/* -------------------------------------------------------------------
  sys operation
*/
static int sys_send(struct rpc_server *rpc_s, struct node_id *peer, struct hdr_sys *hs)	//Done
{

	while(peer->sys_conn.f_connected != 1) {
		pthread_yield();
	}


	int err;

	struct sys_msg *sm;
	struct ibv_send_wr wr, *bad_wr = NULL;
	struct ibv_sge sge;

	sm = calloc(1, sizeof(struct sys_msg));
	memset(&wr, 0, sizeof(wr));

	sm->hs = *hs;
	sm->sys_mr = ibv_reg_mr(peer->sys_conn.pd, &sm->hs, sizeof(struct hdr_sys), IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ);
	if(sm->sys_mr == NULL) {
		printf("ibv_reg_mr return NULL.\n");
		err = -ENOMEM;
		goto err_out;
	}

	wr.opcode = IBV_WR_SEND;

	wr.wr_id = (uintptr_t) sm;	//use address of this hs as the unique wr id
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_SIGNALED;

	sge.addr = (uintptr_t) & sm->hs;
	sge.length = sizeof(struct hdr_sys);
	sge.lkey = sm->sys_mr->lkey;

	err = ibv_post_send(peer->sys_conn.qp, &wr, &bad_wr);
	if(err < 0) {
		printf("Fail: ibv_post_send returned error in %s. (%d)\n", __func__, err);
		goto err_out;
	}

	return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int sys_dispatch_event(struct rpc_server *rpc_s, struct hdr_sys *hs)	//Done
{
	int err = 0;

	switch (hs->sys_cmd) {
	case sys_none:
		break;

	case sys_bar_enter:
		err = sys_bar_arrive(rpc_s, hs);
		break;
	}

	if(err != 0)
		printf("(%s) failed with (%d).\n", __func__, err);

	return err;
}

//sys process use poll
static int sys_process(struct rpc_server *rpc_s, struct node_id *peer)
{

	int ret, err;
	struct ibv_cq *ev_cq;
	void *ev_ctx;
	struct ibv_wc wc;
	struct sys_msg *sm;
	err = ibv_get_cq_event(peer->sys_conn.comp_channel, &ev_cq, &ev_ctx);
	if(err == -1 && errno == EAGAIN) {
		return 0;
	}
	if(err == -1 && errno != EAGAIN) {
		printf("Failed to get cq_event with (%s).\n", strerror(errno));
		err = errno;
		goto err_out;
	}
	ibv_ack_cq_events(ev_cq, 1);
	err = ibv_req_notify_cq(ev_cq, 0);
	if(err != 0) {
		fprintf(stderr, "Failed to ibv_req_notify_cq.\n");
		printf("Failed to ibv_req_notify_cq.\n");
		goto err_out;
	}

	do {
		ret = ibv_poll_cq(ev_cq, 1, &wc);
		if(ret < 0) {
			printf("Failed to ibv_poll_cq.\n");
			fprintf(stderr, "Failed to ibv_poll_cq.\n");
			goto err_out;
		}
		if(ret == 0) {
			continue;
		}

		if(wc.status != IBV_WC_SUCCESS) {
			//printf("Status (%d) is not IBV_WC_SUCCESS.\n", wc.status);
			err = wc.status;
			return 0;
			//goto err_out;
		}
		if(wc.opcode & IBV_WC_RECV) {
			struct hdr_sys *hs = (struct hdr_sys *) (uintptr_t) wc.wr_id;
			err = sys_dispatch_event(rpc_s, hs);

			if(err != 0) {
				goto err_out;
			}



			struct ibv_recv_wr wr, *bad_wr = NULL;
			struct ibv_sge sge;
			int err = 0;


			wr.wr_id = (uintptr_t) wc.wr_id;
			wr.next = NULL;
			wr.sg_list = &sge;
			wr.num_sge = 1;

			sge.addr = (uintptr_t) wc.wr_id;
			sge.length = sizeof(struct hdr_sys);
			sge.lkey = peer->sm->sys_mr->lkey;

			err = ibv_post_recv(peer->sys_conn.qp, &wr, &bad_wr);
			if(err != 0) {
				printf("ibv_post_recv fails with %d in %s.\n", err, __func__);
			}



		} else if(wc.opcode == IBV_WC_SEND) {
			sm = (struct sys_msg *) (uintptr_t) wc.wr_id;
			if(sm->sys_mr != 0) {
				err = ibv_dereg_mr(sm->sys_mr);
				if(err != 0) {
					goto err_out;
				}
			}
			free(sm);
		} else {
			printf("Weird wc.opcode %d.\n", wc.opcode);	//debug
			err = wc.opcode;
			goto err_out;
		}
	} while(ret);
	return 0;
      err_out:
	printf("(%s): err (%d).\n", __func__, err);
	return err;

}

static int sys_process_event(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	struct pollfd my_pollfd[rpc_s->app_num_peers - 1];
	int which_peer[rpc_s->app_num_peers - 1];
	int timeout = 30;

	int i, j, err = 0;

	j = 0;
	list_for_each_entry(peer, &rpc_s->peer_list, struct node_id, peer_entry) {
		if(peer->sys_conn.f_connected == 0)
			continue;

		my_pollfd[j].fd = peer->sys_conn.comp_channel->fd;
		my_pollfd[j].events = POLLIN;
		my_pollfd[j].revents = 0;
		which_peer[j] = peer->ptlmap.id;
		j++;
	}

	if(j == 0)
		return 0;

	err = poll(my_pollfd, j, timeout);

	if(err < 0) {
		goto err_out;
	} else if(err == 0)
		return 0;
	else if(err > 0) {
		for(i = 0; i < j; i++) {
			if(my_pollfd[i].revents != 0) {
				peer = rpc_get_peer(rpc_s, which_peer[i]);
				err = sys_process(rpc_s, peer);
				if(err < 0)
					goto err_out;
			}
		}
	}

	return 0;
      err_out:
	printf("(%s): err (%d).\n", __func__, err);
	return err;
}

static int sys_cleanup(struct rpc_server *rpc_s)
{
	int i, err = 0;
	struct node_id *peer;
	list_for_each_entry(peer, &rpc_s->peer_list, struct node_id, peer_entry) {	
		if(peer->sys_conn.f_connected == 0 || peer->ptlmap.id == rpc_s->ptlmap.id)
			continue;
		err = rdma_destroy_id(peer->sys_conn.id);
		if(err < 0) {
			printf("Failed to rdma_destroy_id with err(%d).\n", err);
			goto err_out;
		}
		peer->sys_conn.f_connected = 0;
	}
	return 0;

      err_out:
	printf("(%s): err (%d).\n", __func__, err);
	return err;
}


/* -------------------------------------------------------------------
  Other Dart RPC operations
*/
// this function can only be used after rpc_server is fully initiated
static struct node_id *rpc_get_peer(struct rpc_server *rpc_s, int peer_id)
{
	return rpc_server_find(rpc_s, peer_id);
}

//added in IB version, get remote notification of finished RDMA operation, clean up memory.
static int rpc_cb_cleanup(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{

	struct ibv_wc wc;
	wc.wr_id = cmd->wr_id;

	struct rpc_request *rr = (struct rpc_request *) (uintptr_t) wc.wr_id;

	int err = -ENOMEM;

	if(*rr->cb) {
		err = (*rr->cb) (rpc_s, &wc);
		if(err != 0)
			return err;
	}

	return 0;
}

/* 
   Decode  and   service  a  command  message  received   in  the  rpc
   buffer. This routine is called by 'rpc_process_event()' in response
   to new incomming rpc request.
*/

static int rpc_cb_decode(struct rpc_server *rpc_s, struct ibv_wc *wc)	//Done
{
	struct rpc_cmd *cmd = (struct rpc_cmd *) (uintptr_t) wc->wr_id;
	int err, i;

	for(i = 0; i < num_service; i++) {
		if(cmd->cmd == rpc_commands[i].rpc_cmd) {
			err = rpc_commands[i].rpc_func(rpc_s, cmd);
			break;
		}
	}

	if(i == num_service) {
		printf("Network command unknown %d!\n", cmd->cmd);
		err = -EINVAL;
	}

	if(err < 0)
		printf("(%s) err: Peer# %d Network command %d from %d.\n", __func__, rpc_s->ptlmap.id, cmd->cmd, cmd->id);


	return err;
}



//Yubo, calling from thread
void* rpc_cb_decode_thrd(void *passArg)	//Done
{
	struct PassArg *local_passArg = (struct PassArg*)passArg;
	struct rpc_server *rpc_s = local_passArg->rpc_s;
	struct ibv_wc *wc = local_passArg->wc;


	struct rpc_cmd *cmd = (struct rpc_cmd *) (uintptr_t) wc->wr_id;
	int err, i;

	for(i = 0; i < num_service; i++) {
		if(cmd->cmd == rpc_commands[i].rpc_cmd) {
			err = rpc_commands[i].rpc_func(rpc_s, cmd);
			break;
		}
	}

	if(i == num_service) {
		printf("Network command unknown %d!\n", cmd->cmd);
		err = -EINVAL;
	}

	if(err < 0)
		printf("(%s) err: Peer# %d Network command %d from %d.\n", __func__, rpc_s->ptlmap.id, cmd->cmd, cmd->id);


	//return err;
}


/*
  Allocate   an  RPC  structure   for  communication   buffers.
*/
static struct rpc_request *rr_comm_alloc(int num_rpc)	//Done
{
	struct rpc_request *rr;
	size_t size;

	size = sizeof(*rr) + sizeof(*rr->msg) + sizeof(struct rpc_cmd) * num_rpc + 7;
	rr = malloc(size);
	if(!rr)
		return 0;

	memset(rr, 0, size);
	rr->cb = (async_callback) rpc_cb_decode;

	rr->msg = (struct msg_buf *) (rr + 1);
	rr->msg->msg_data = rr->msg + 1;
	rr->msg->msg_rpc = (struct rpc_cmd *) rr->msg + 1;
	//ALIGN_ADDR_QUAD_BYTES(rr->msg->msg_data);
	rr->msg->size = sizeof(struct rpc_cmd) * num_rpc;
	return rr;
}

/*
  Default  completion  routine for  rpc  messages  we initiate.   This
  routine is called by  'rpc_process_event()' to complete rpc requests
  which were locally initiated.
*/
static int rpc_cb_req_completion(struct rpc_server *rpc_s, struct ibv_wc *wc)	//Working
{
	int err;
	struct rpc_request *rr = (struct rpc_request *) (uintptr_t) wc->wr_id;
	if(!rr)
		return 0;
	struct node_id *peer;
	peer = (struct node_id *) rr->msg->peer;

	if(rr->msg)
		rr->msg->refcont--;

	//test
	/*      if (rr->msg->refcont != 0)
	   if(wc->opcode == IBV_WC_SEND || wc->opcode == IBV_WC_RDMA_WRITE || wc->opcode == IBV_WC_RDMA_READ || wc->opcode == IBV_WC_RECV)
	   {
	   err = rpc_process_event(rpc_s);
	   if (err != 0) 
	   {
	   printf("'%s()': failed with %d.\n", __func__, err);
	   return err;
	   }

	   return 0;
	   } */
	//test

	//send back msg to tell other side RDMA operation is finished. Since IB just generates wc on post side.
	if(rr->type == 1) {
		struct msg_buf *msg;
		//uint64_t *wrid;
		msg = msg_buf_alloc(rpc_s, peer, 1);
		if(!msg)
			goto err_out;
		msg->size = sizeof(struct rpc_cmd);
		msg->msg_rpc->cmd = peer_rdma_done;
		msg->msg_rpc->id = rpc_s->ptlmap.id;

		msg->msg_rpc->wr_id = rr->msg->id;

//                printf("Peer %d Send to peer(%d) with wrid (%ld). in %ld\n", rpc_s->ptlmap.id, peer->ptlmap.id, msg->msg_rpc->wr_id,rr);

		err = rpc_send(rpc_s, peer, msg);
		if(err < 0) {
			free(msg->msg_data);
			goto err_out;
		}
	}
//      if(rpc_s->ptlmap.appid==0&&peer->ptlmap.appid!=0)
//              printf("nextmsg %d %d %ld %d\n",rpc_s->ptlmap.id,peer->ptlmap.id,rr,rr->msg->refcont);
	if(rr->msg) {
		if(rr->msg->refcont == 0) {
			if(rr->rpc_mr != 0) {
				err = ibv_dereg_mr(rr->rpc_mr);
				if(err != 0) {
					goto err_out;
				}
			}
			if(rr->data_mr != 0) {
				err = ibv_dereg_mr(rr->data_mr);
				if(err != 0) {
					goto err_out;
				}
			}

			(*rr->msg->cb) (rpc_s, rr->msg);
			free(rr);
		}
	}
	return 0;

      err_out:
	printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}


/* ------------------------------------------------------------------
  Message Passing and Data Transfer
*/
static int rpc_prepare_buffers(struct rpc_server *rpc_s, const struct node_id *peer, struct rpc_request *rr, enum io_dir dir)	//working: TODO need 'id' information getting from initialization
{
	int err;

	rr->msg->msg_rpc->wr_id = (uintptr_t) rr;
	rr->data_mr = ibv_reg_mr(peer->rpc_conn.pd, rr->msg->msg_data, rr->msg->size, IBV_ACCESS_LOCAL_WRITE | ((dir == io_send) ? IBV_ACCESS_REMOTE_READ : IBV_ACCESS_REMOTE_WRITE));
	if(!rr->data_mr) {
		printf("ibv_reg_mr fails with (%s).\n", strerror(errno));
		err = errno;
		goto err_out;
	}

	rr->msg->msg_rpc->mr = *rr->data_mr;
	rr->msg->refcont++;

	return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int rpc_post_request(struct rpc_server *rpc_s, const struct node_id *peer, struct rpc_request *rr, const struct hdr_sys *hs)	//working 
{
	int err;
	//sending pure msg TODO 2nd parameter, 5th parameter, check manual
	struct ibv_send_wr wr, *bad_wr = NULL;
	struct ibv_sge sge;

	memset(&wr, 0, sizeof(wr));

	if(rr->type == 0) {
		rr->rpc_mr = ibv_reg_mr(peer->rpc_conn.pd, rr->data, rr->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ);
		if(rr->rpc_mr == NULL) {
			printf("ibv_reg_mr return NULL.\n");
			err = -ENOMEM;
			goto err_out;
		}


		wr.opcode = IBV_WR_SEND;
		sge.lkey = rr->rpc_mr->lkey;
	} else if(rr->type == 1) {
		rr->data_mr = ibv_reg_mr(peer->rpc_conn.pd, rr->data, rr->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ);
		if(rr->data_mr == NULL) {
			printf("ibv_reg_mr return NULL.\n");
			err = -ENOMEM;
			goto err_out;
		}


		wr.opcode = IBV_WR_RDMA_WRITE;

		wr.wr.rdma.remote_addr = (uintptr_t) rr->msg->mr.addr;
		wr.wr.rdma.rkey = rr->msg->mr.rkey;


		sge.lkey = rr->data_mr->lkey;
	}

	wr.wr_id = (uintptr_t) rr;	//use address of this rr as the unique wr id

	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_SIGNALED;

	sge.addr = (uintptr_t) rr->data;
	sge.length = rr->size;

	err = ibv_post_send(peer->rpc_conn.qp, &wr, &bad_wr);

	if(err < 0) {
		printf("Fail: ibv_post_send returned error in %s. (%d)\n", __func__, err);
		printf("Node %d has %d send_event on the queue (max %d) to peer %d.\n", rpc_s->ptlmap.id, peer->req_posted, peer->rpc_conn.qp_attr.cap.max_send_wr, peer->ptlmap.id);
		goto err_out;
	}


	if(rr) {
		if(rr->msg) {
			rr->msg->refcont++;
			}
	}

	return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int rpc_fetch_request(struct rpc_server *rpc_s, const struct node_id *peer, struct rpc_request *rr)	//TODO id
{
	int err;
	struct ibv_send_wr wr, *bad_wr = NULL;
	struct ibv_sge sge;

	if(rr->type == 1) {
		rr->data_mr = ibv_reg_mr(peer->rpc_conn.pd, rr->data, rr->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
		if(rr->data_mr == NULL) {
			printf("ibv_reg_mr return NULL.\n");
			err = -ENOMEM;
			goto err_out;
		}

		memset(&wr, 0, sizeof(wr));
		wr.wr_id = (uintptr_t) rr;	//use address of this rr as the unique wr id

		wr.opcode = IBV_WR_RDMA_READ;
		wr.sg_list = &sge;
		wr.num_sge = 1;
		wr.send_flags = IBV_SEND_SIGNALED;

		wr.wr.rdma.remote_addr = (uintptr_t) rr->msg->mr.addr;
		wr.wr.rdma.rkey = rr->msg->mr.rkey;

		sge.addr = (uintptr_t) rr->data;
		sge.length = rr->size;
		sge.lkey = rr->data_mr->lkey;

		err = ibv_post_send(peer->rpc_conn.qp, &wr, &bad_wr);
		if(err < 0) {
			printf("Fail: ibv_post_send returned error in %s. (%d)\n", __func__, err);
			goto err_out;
		}
	}

	if(rr) {
		if(rr->msg)
			rr->msg->refcont++;
	}

	return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer)	// working
{
	struct rpc_request *rr;
//      struct hdr_sys hs;
	int err, i;

	//check peer->req_list
	while(!list_empty(&peer->req_list)) {
		if(rpc_s->com_type == 1 || peer->req_posted > 100) {
			for(i = 0; i < 10; i++)	//performance here
			{
				err = rpc_process_event_with_timeout(rpc_s, 1);
				if(err != 0 && err != -ETIME)
					goto err_out;
			}
		}
		//?check credits (dont need in IB version, since IB reliable connection has credit mechanism underneath)

		// Sending credit is good, we will send the message.


		rr = list_entry(peer->req_list.next, struct rpc_request, req_entry);


		if(!rr) {
			return 0;
		}

		if(!rr->msg)
			return 0;

		if(rr->msg->msg_data) {
			err = rpc_prepare_buffers(rpc_s, peer, rr, rr->iodir);
			if(err != 0)
				goto err_out;
		}
		// Return the credits we have for peer //don't need any more

		// post request
		err = rpc_post_request(rpc_s, peer, rr, 0);


		if(err != 0)
			goto err_out;

		// Message is sent, consume one credit. (dont need anymore?)
		peer->num_msg_at_peer--;
		list_del(&rr->req_entry);

		//list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
		//rpc_s->rr_num++;////?TODO: do we need put rr to rpc_s->rpc_list? or just let it be
		peer->num_req--;
	}

	return 0;
      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/* ------------------------------------------------------------------
  Network/Device/IB system operation
*/
/*
char *ip_search(void)
{
	struct sockaddr_in address;
	memset(&address, 0, sizeof(address));

	struct ifaddrs *addrs;
	getifaddrs(&addrs);
	struct ifaddrs *head = addrs;

	int count =0;

	for (; head != NULL; head = head->ifa_next) {
        	if (head->ifa_name == NULL || strcmp(IB_INTERFACE, head->ifa_name) == 0){
               		break;
		}
		count++;
	}

	int sfd, intr;
	struct ifreq buf[16];
	struct ifconf ifc;
	sfd = socket(AF_INET, SOCK_DGRAM, 0);
	if(sfd < 0)
		return ERRORIP;
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = (caddr_t) buf;
	if(ioctl(sfd, SIOCGIFCONF, (char *) &ifc))
		return ERRORIP;
	intr = ifc.ifc_len / sizeof(struct ifreq);
	while(intr-- > 0 && ioctl(sfd, SIOCGIFADDR, (char *) &buf[intr]));
	close(sfd);

	return inet_ntoa(((struct sockaddr_in *) (&buf[count-1].ifr_addr))->sin_addr);
}
*/


char *ip_search(void)
{
	int sfd;
	struct ifreq ifr;

	sfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sfd < 0)
		return ERRORIP;

	ifr.ifr_addr.sa_family = AF_INET;
	strncpy(ifr.ifr_name, IB_INTERFACE, IFNAMSIZ-1);

	if (ioctl(sfd, SIOCGIFADDR, &ifr))
		return ERRORIP;
	close(sfd);


	return inet_ntoa(((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr);
}



// Check if the format of IP address is correct. (done)
int ip_check(const char *ip)
{
	return inet_addr(ip);
}

// Get the auto-assigned port number
int port_search(int socket)
{
	int port;
	int err;
	socklen_t len = sizeof(struct sockaddr);

	struct sockaddr_in address;
	memset(&address, 0, sizeof(struct sockaddr_in));
	err = getsockname(socket, (struct sockaddr *) &address, &len);
	if(err < 0)
		return err;
	port = ntohs(address.sin_port);
	return port;
}

// Check if the format/number of Port Number is correct. (done)
int port_check(int port)
{
	if(1024 < port && port < PORTMAX)
		return 1;
	else
		return 0;
}


//Register memory for one rpc_cmd that will store the coming rpc_cmd from peer. It is only used for ibv_post_recv.
int register_memory(struct rpc_request *rr, struct node_id *peer)
{
	rr->rpc_mr = ibv_reg_mr(peer->rpc_conn.pd, rr->msg->msg_data, rr->msg->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
	if(rr->rpc_mr == NULL) {
		printf("ibv_reg_mr return NULL.\n");
		return -ENOMEM;
	}

	return 0;
}

//post receives for RPC_CMD
int post_receives(struct rpc_request *rr, struct node_id *peer, int i)
{

	struct ibv_recv_wr wr, *bad_wr = NULL;
	struct ibv_sge sge;
	int err = 0;


	wr.wr_id = (uintptr_t) rr->msg->msg_data + sizeof(struct rpc_cmd) * i;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;

	sge.addr = (uintptr_t) rr->msg->msg_data + sizeof(struct rpc_cmd) * i;
	sge.length = sizeof(struct rpc_cmd);
	sge.lkey = rr->rpc_mr->lkey;

	err = ibv_post_recv(peer->rpc_conn.qp, &wr, &bad_wr);

	if(err != 0) {
		return err;
	}

	return 0;
}


/* =====================================================
	Public API
 =======================================================*/

/* -------------------------------------------------------------------
  System barrier implementation.
*/
int rpc_barrier(struct rpc_server *rpc_s)	//Donesys_bar_send
{
	//      struct node_id *peer;
	int round, np;
	int next, prev;
	int err;

	np = log2_ceil(rpc_s->app_num_peers);
	round = -1;

	rpc_s->bar_num = (rpc_s->bar_num + 1) & 0xFF;

//printf("%s is start\n", __func__);

	while(round < np - 1) {

		round = round + 1;

		next = (myrank(rpc_s) + (1 << round)) % rpc_s->app_num_peers;
		prev = (rpc_s->app_num_peers + myrank(rpc_s) - (1 << round)) % rpc_s->app_num_peers;

		err = sys_bar_send(rpc_s, rank2id(rpc_s, next));
		if(err < 0)
			goto err_out;

//printf("%s before while\n", __func__);
		SYS_WAIT_COMPLETION(rpc_s->bar_tab[prev] == rpc_s->bar_num || rpc_s->bar_tab[prev] == ((rpc_s->bar_num + 1) & 0xFF))	//delete ";" here
//printf("%s after while\n", __func__);
	}

//printf("%s is done\n", __func__);
	return 0;

      err_out:
	printf("(%s) failed (%d).\n", __func__, err);
	return err;

}


/* -------------------------------------------------------------------
  RPC-server instance operation. e.g init, free, etc.
*/
void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func);

struct rpc_server *rpc_server_init(int option, char *ip, int port, int num_buff, int num_peers, void *dart_ref, enum rpc_component cmp_type)
{
	struct rpc_server *rpc_s = 0;
	int err = -ENOMEM;
	static char *localip;

	if(rpc_s_instance)
		return rpc_s_instance;

	rpc_s = calloc(1, sizeof(*rpc_s));
	if(!rpc_s)
		goto err_out;

	rpc_s->dart_ref = dart_ref;
	rpc_s->num_buf = num_buff;
    rpc_s->app_num_peers = num_peers;
	rpc_s->num_rpc_per_buff = num_peers;
	rpc_s->max_num_msg = num_buff;
	rpc_s->com_type = cmp_type;

	rpc_s->listen_id = NULL;
	rpc_s->rpc_ec = NULL;

	rpc_s->alloc_pd_flag = 0;
	rpc_s->global_pd = NULL;
	rpc_s->global_ctx = NULL;

	if(option == 1) {
		localip = ip;
		rpc_s->ptlmap.address.sin_addr.s_addr = inet_addr(ip);
		rpc_s->ptlmap.address.sin_port = htons(port);
		rpc_s->ptlmap.address.sin_family = AF_INET;
	} else if(option == 0) {
		localip = ip_search();
		//uloga("%s, localip=%s\n", __func__, localip);

		if(ip_check(localip) == -1) {
			printf("local IP address error!\n");
			goto err_out;
		}
		rpc_s->ptlmap.address.sin_addr.s_addr = inet_addr(localip);
		rpc_s->ptlmap.address.sin_family = AF_INET;
	} else {
		perror("option error.\n");
		goto err_out;
	}
	// bind id and open listening id for RPC operation.
	rpc_s->rpc_ec = rdma_create_event_channel();
	if(rpc_s->rpc_ec == NULL) {
		printf("rdma_create_id %d in %s.\n", err, __func__);
		goto err_out;
	}

	err = rdma_create_id(rpc_s->rpc_ec, &rpc_s->listen_id, NULL, RDMA_PS_TCP);
	if(err != 0) {
		printf("rdma_create_id %d in %s.\n", err, __func__);
		goto err_free;
	}
	err = rdma_bind_addr(rpc_s->listen_id, (struct sockaddr *) &rpc_s->ptlmap.address);
	if(err != 0) {
		printf("rdma_bind_addr %d in %s.\n", err, __func__);
		goto err_free;
	}
	err = rdma_listen(rpc_s->listen_id, 65535);	/* backlog=65535 is backlog of incoming connect event */
	if(err != 0) {
		printf("rdma_listen %d in %s.\n", err, __func__);
		goto err_free;
	}

	rpc_s->ptlmap.address.sin_port = rdma_get_src_port(rpc_s->listen_id);

	//TODO: sys part if needed

	//other dart init operation
	INIT_LIST_HEAD(&rpc_s->rpc_list);
    INIT_LIST_HEAD(&rpc_s->peer_list);

	//TODO: for server and client, num_rpc_per_buff should be different: S, s+c; C c; NEED CHECK
	rpc_s->bar_tab = malloc(sizeof(*rpc_s->bar_tab) * rpc_s->app_num_peers);
	if(!rpc_s->bar_tab) {
		err = -ENOMEM;
		goto err_free;
	}
	memset(rpc_s->bar_tab, 0, sizeof(*rpc_s->bar_tab) * rpc_s->app_num_peers);

	rpc_add_service(peer_rdma_done, rpc_cb_cleanup);

	// Init succeeded, set the instance reference here.
	rpc_s_instance = rpc_s;
	return rpc_s;

      err_free:
	rdma_destroy_id(rpc_s->listen_id);
	rdma_destroy_event_channel(rpc_s->rpc_ec);
	free(rpc_s);

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return 0;
}

//added in IB version. It creates a rpc_request node rr; then prepares, registers, and posts buffer for future coming RPC_CMD; and then adds this rr entry to rpc_list
int rpc_post_recv(struct rpc_server *rpc_s, struct node_id *peer)
{
	int err;
	struct rpc_cmd *cmd = malloc(sizeof(struct rpc_cmd));
	memset(cmd, 0, sizeof(struct rpc_cmd));

	struct rpc_request *rr;
	rr = rr_comm_alloc(rpc_s->num_buf);

	peer->rr = rr;

	rr->peerid = peer->ptlmap.id;

	err = register_memory(rr, peer);
	if(err != 0)
		goto err_out;

	int i;

	for(i = 0; i < rpc_s->num_buf; i++) {
		rr->current_rpc_count = i + 1;

		//      list_add(&rr->req_entry, &rpc_s->rpc_list);

		//      printf("%d\n",rr->current_rpc_count);

		err = post_receives(rr, peer, rr->current_rpc_count - 1);
		if(err == 0)
			peer->num_recv_buf++;
	}

	return 0;
      err_out:
	printf("%d %d '%s()': failed with %d.\n", rpc_s->ptlmap.id, peer->ptlmap.id, __func__, err);
	return err;
}




int sys_post_recv(struct rpc_server *rpc_s, struct node_id *peer)
{

	int err = 0;
	struct sys_msg *sm;
	sm = calloc(1, sizeof(struct sys_msg));

	peer->sm = sm;

	sm->hs.sys_id = peer->ptlmap.id;

	struct ibv_recv_wr wr, *bad_wr = NULL;
	struct ibv_sge sge;
	struct hdr_sys *hs = malloc(rpc_s->num_buf * sizeof(struct hdr_sys));

	sm->real_hs = hs;

	sm->sys_mr = ibv_reg_mr(peer->sys_conn.pd, hs, rpc_s->num_buf * sizeof(struct hdr_sys), IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE);
	if(sm->sys_mr == NULL) {
		printf("ibv_reg_mr return NULL.\n");
		err = -ENOMEM;
		goto err_out;
	}
	//list_add(&rr->req_entry, &rpc_s->rpc_list);

	int i;

	for(i = 0; i < rpc_s->num_buf; i++) {
		wr.wr_id = (uintptr_t) hs + i * sizeof(struct hdr_sys);
		wr.next = NULL;
		wr.sg_list = &sge;
		wr.num_sge = 1;

		sge.addr = (uintptr_t) hs + i * sizeof(struct hdr_sys);
		sge.length = sizeof(struct hdr_sys);
		sge.lkey = sm->sys_mr->lkey;


		err = ibv_post_recv(peer->sys_conn.qp, &wr, &bad_wr);
		if(err != 0) {
			//      printf("ibv_post_recv fails with %d in %s.\n", err, __func__);
			//      goto err_out;
		} else {
			peer->num_sys_recv_buf++;
		}
	}

	//printf("host %d peer %d num_sys_buffer %d\n",rpc_s->ptlmap.id,peer->ptlmap.id,peer->num_sys_recv_buf);

	return 0;
      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int rpc_server_finish(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	int i, err;

    list_for_each_entry(peer, &rpc_s->peer_list, struct node_id, peer_entry) {
		if(peer->rpc_conn.f_connected == 1) {
			while(peer->num_req) {
				err = peer_process_send_list(rpc_s, peer);
				if(err < 0) {
					printf("'%s()': encountered an error %d, skipping.\n", __func__, err);
					sleep(10);
				}
			}
		}
	}

	return 0;
}

//working on this now
int rpc_server_free(struct rpc_server *rpc_s)
{
	int err, i;
	struct rpc_request *rr, *tmp;


	rpc_server_finish(rpc_s);

	// Process any remaining events.
	list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry) {
		err = ibv_dereg_mr(rr->rpc_mr);
		if(err != 0) {
			printf("ibv_dereg_mr fail with err %d.\n", err);
			goto err_out;
		}
		list_del(&rr->req_entry);
		free(rr);
	}


	//TODO: add a sys_list for sys msg, so that it will be easy to clean up remaining sys messages.
	//disconnect all the links, deregister the memory, free all the allocation
    struct node_id *peer;
        list_for_each_entry(peer, &rpc_s->peer_list, struct node_id, peer_entry) {
        if(peer->ptlmap.id == rpc_s->ptlmap.id) {
            continue;
        }
        if(peer->rpc_conn.f_connected == 1) {
            err = rdma_disconnect(peer->rpc_conn.id);
        
            if(err != 0) {
                printf("Peer(%d) RPC rdma_disconnect err (%d). Ignore.\n", peer->ptlmap.id, err);
            }
            rdma_destroy_qp(peer->rpc_conn.id);
            rdma_destroy_id(peer->rpc_conn.id);
        }
        if(peer->sys_conn.f_connected == 1) {
            err = rdma_disconnect(peer->sys_conn.id);

            if(err != 0) {
                printf("Peer(%d) SYS rdma_disconnect err (%d). Ignore.\n", peer->ptlmap.id, err);
            }
            rdma_destroy_qp(peer->sys_conn.id);
            rdma_destroy_id(peer->sys_conn.id);
        }
    }

	//destory id, ec, and so on
	rdma_destroy_id(rpc_s->listen_id);
	rdma_destroy_event_channel(rpc_s->rpc_ec);
	//free(rpc_s->peer_tab);
	free(rpc_s->bar_tab);
	free(rpc_s);


	return 0;
      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff)	//Done
{
	rpc_s->num_rpc_per_buff = num_rpc_per_buff;
}

struct rpc_server *rpc_server_get_instance(void)	//Done
{
	/* Blindly  get  the  rpc_server   reference;  if  it  is  not
	   initialized, should call rpc_server_init() */
	return rpc_s_instance;
}

/*
  Return the id of the rpc server.
*/
int rpc_server_get_id(void)	//Done
{
	// TODO: if server is not initialized, should return -1.
	return rpc_s_instance->ptlmap.id;
}

/*
  Connection Service
*/
void build_context(struct ibv_context *verbs, struct connection *conn)
{
	int err, flags;
	conn->ctx = verbs;

	if(!rpc_s_instance->alloc_pd_flag) {
		rpc_s_instance->global_pd = ibv_alloc_pd(conn->ctx);
		rpc_s_instance->global_ctx = verbs;
		rpc_s_instance->alloc_pd_flag = 1;
	}
	conn->pd = rpc_s_instance->global_pd;
	// uloga("%s(): conn->ctx= %x, conn->pd= %x, global_ctx= %x, global_pd= %x\n"           , __func__, conn->ctx, conn->pd, rpc_s_instance->global_ctx, rpc_s_instance->global_pd);
	if(conn->pd == NULL)
		printf("ibv_alloc_pd return NULL in %s.\n", __func__);
	conn->comp_channel = ibv_create_comp_channel(conn->ctx);
	if(conn->comp_channel == NULL)
		printf("ibv_create_comp_channel return NULL in %s.\n", __func__);
	conn->cq = ibv_create_cq(conn->ctx, 65536, NULL, conn->comp_channel, 0);	/* cqe=65536 is arbitrary */
	if(conn->cq == NULL)
		printf("ibv_create_cq return NULL in %s.\n", __func__);
	err = ibv_req_notify_cq(conn->cq, 0);
	if(err != 0) {
		printf("ibv_req_notify_cq fails.\n");
		printf("'%s()': failed with %d.\n", __func__, err);
	}
//new//diff
	// change the blocking mode of the completion channel 
	flags = fcntl(conn->ctx->async_fd, F_GETFL);
	err = fcntl(conn->ctx->async_fd, F_SETFL, flags | O_NONBLOCK);
	if(err < 0) {
		fprintf(stderr, "Failed to change file descriptor of completion event channel\n");
		exit(EXIT_FAILURE);
	}
	// change the blocking mode of the completion channel 
	flags = fcntl(conn->comp_channel->fd, F_GETFL);
	err = fcntl(conn->comp_channel->fd, F_SETFL, flags | O_NONBLOCK);
	if(err < 0) {
		fprintf(stderr, "Failed to change file descriptor of completion event channel\n");
		exit(EXIT_FAILURE);
	}
//new
}

void build_qp_attr(struct ibv_qp_init_attr *qp_attr, struct connection *conn, struct rpc_server *rpc_s)
{
	memset(qp_attr, 0, sizeof(*qp_attr));

	qp_attr->send_cq = conn->cq;
	qp_attr->recv_cq = conn->cq;
	qp_attr->qp_type = IBV_QPT_RC;

	qp_attr->cap.max_send_wr = rpc_s->num_buf;
	qp_attr->cap.max_recv_wr = rpc_s->num_buf;
	//qp_attr->cap.max_recv_wr = 20;
	qp_attr->cap.max_send_sge = 1;
	qp_attr->cap.max_recv_sge = 1;
}

int rpc_connect(struct rpc_server *rpc_s, struct node_id *peer)
{
	struct addrinfo *addr;
	struct rdma_cm_event *event = NULL;
	struct rdma_conn_param cm_params;
	struct con_param conpara;

	char *ip;
	char port[32];
	int err, check;
	check = 0;

	// Resolve the IP+Port of DS_Master
	ip = inet_ntoa(peer->ptlmap.address.sin_addr);
	snprintf(port, sizeof(port), "%u", ntohs(peer->ptlmap.address.sin_port));

	err = getaddrinfo(ip, port, NULL, &addr);
	if(err != 0) {
		printf("getaddrinfo %d in %s.\n", err, __func__);
		goto err_out;
	}

	peer->rpc_ec = rdma_create_event_channel();
	if(peer->rpc_ec == NULL) {
		printf("rdma_create_event_channel return NULL in %s.\n", __func__);
		err = -ENOMEM;
		goto err_out;
	}
	err = rdma_create_id(peer->rpc_ec, &(peer->rpc_conn.id), NULL, RDMA_PS_TCP);
	if(err != 0) {
		printf("rdma_create_id %d in %s.\n", err, __func__);
		goto err_out;
	}
	err = rdma_resolve_addr(peer->rpc_conn.id, NULL, addr->ai_addr, INFINIBAND_TIMEOUT);
	if(err != 0) {
		printf("rdma_resolve_addr %d in %s.\n", err, __func__);
		goto err_out;
	}
	freeaddrinfo(addr);

    struct node_id *temp_peer;
    
	while(rdma_get_cm_event(peer->rpc_ec, &event) == 0) {
		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);

		if(event_copy.event == RDMA_CM_EVENT_ADDR_RESOLVED) {
			build_context(event_copy.id->verbs, &peer->rpc_conn);
			build_qp_attr(&peer->rpc_conn.qp_attr, &peer->rpc_conn, rpc_s);

			err = rdma_create_qp(event_copy.id, peer->rpc_conn.pd, &peer->rpc_conn.qp_attr);
			if(err != 0) {
				printf("Peer %d could not connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", rpc_s->ptlmap.id, peer->ptlmap.id, rpc_s->num_qp, err, __func__, strerror(errno));
				goto err_out;
			}
			rpc_s->num_qp++;

			event_copy.id->context = &peer->rpc_conn;
			peer->rpc_conn.id = event_copy.id;
			peer->rpc_conn.qp = event_copy.id->qp;

			err = rpc_post_recv(rpc_s, peer);
			if(err != 0) {
				goto err_out;
			}

			err = rdma_resolve_route(event_copy.id, INFINIBAND_TIMEOUT);
			if(err != 0) {
				printf("rdma_resolve_route %d in %s.\n", errno, __func__);
				goto err_out;
			}
		} else if(event_copy.event == RDMA_CM_EVENT_ROUTE_RESOLVED) {
			memset(&cm_params, 0, sizeof(struct rdma_conn_param));

			conpara.pm_cp = rpc_s->ptlmap;
			conpara.pm_sp = peer->ptlmap;
			conpara.num_cp = rpc_s->num_rpc_per_buff;
			conpara.type = 1;

			cm_params.private_data = &conpara;
			cm_params.private_data_len = sizeof(conpara);
			cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;	//diff
			cm_params.rnr_retry_count = 7;	/* infinite retry */

			err = rdma_connect(event_copy.id, &cm_params);
			if(err != 0) {
				printf("rdma_connect %d in %s.\n", err, __func__);
				goto err_out;
			}
		} else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			if(peer->ptlmap.id == 0) {
				if(rpc_s->ptlmap.appid != 0) {
                    //Here is a tricky design. Master Server puts app_minid into this 'type' field and return.
					rpc_s->app_minid = ((struct con_param *) event_copy.param.conn.private_data)->type;
					rpc_s->ptlmap.id = ((struct con_param *) event_copy.param.conn.private_data)->pm_sp.id;
					peer->ptlmap = ((struct con_param *) event_copy.param.conn.private_data)->pm_cp;
                    //only the master client will get the total number of peers from the master server
                    if(rpc_s->ptlmap.id == rpc_s->app_minid || rpc_s->app_minid ==0){
                        rpc_s->num_peers = rpc_s->app_num_peers + ((struct con_param *) event_copy.param.conn.private_data)->num_cp;
                    }
				} else
					rpc_s->ptlmap.id = *((int *) event_copy.param.conn.private_data);
			}
			peer->rpc_conn.f_connected = 1;
			check = 1;
		} else {
			err = event_copy.status;
			goto err_out;
		}

		if(check == 1)
			break;
	}

	return 0;
      err_out:
	return err;
}

int sys_connect(struct rpc_server *rpc_s, struct node_id *peer)
{
	struct addrinfo *addr;
	struct rdma_cm_event *event = NULL;
	struct rdma_conn_param cm_params;
	struct con_param conpara;

	char *ip;
	char port[32];
	int err, check;
	check = 0;

	// Resolve the IP+Port of DS_Master
	ip = inet_ntoa(peer->ptlmap.address.sin_addr);
	snprintf(port, sizeof(port), "%u", ntohs(peer->ptlmap.address.sin_port));

	err = getaddrinfo(ip, port, NULL, &addr);
	if(err != 0) {
		printf("getaddrinfo %d in %s.\n", err, __func__);
		goto err_out;
	}
	peer->sys_ec = rdma_create_event_channel();
	if(peer->sys_ec == NULL) {
		printf("rdma_create_event_channel return NULL in %s.\n", __func__);
		err = -ENOMEM;
		goto err_out;
	}
	err = rdma_create_id(peer->sys_ec, &(peer->sys_conn.id), NULL, RDMA_PS_TCP);
	if(err != 0) {
		printf("rdma_create_id %d in %s.\n", err, __func__);
		goto err_out;
	}
	err = rdma_resolve_addr(peer->sys_conn.id, NULL, addr->ai_addr, INFINIBAND_TIMEOUT);
	if(err != 0) {
		printf("rdma_resolve_addr %d in %s.\n", err, __func__);
		goto err_out;
	}
	freeaddrinfo(addr);

	while(rdma_get_cm_event(peer->sys_ec, &event) == 0) {
		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);

		if(event_copy.event == RDMA_CM_EVENT_ADDR_RESOLVED) {
			build_context(event_copy.id->verbs, &peer->sys_conn);
			build_qp_attr(&peer->sys_conn.qp_attr, &peer->sys_conn, rpc_s);

			err = rdma_create_qp(event_copy.id, peer->sys_conn.pd, &peer->sys_conn.qp_attr);
			if(err != 0) {
				printf("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", rpc_s->ptlmap.id, peer->ptlmap.id, rpc_s->num_qp, err, __func__, strerror(errno));
				goto err_out;
			}
			rpc_s->num_qp++;

			event_copy.id->context = &peer->sys_conn;
			peer->sys_conn.id = event_copy.id;
			peer->sys_conn.qp = event_copy.id->qp;
			
            err = sys_post_recv(rpc_s, peer);
			if(err != 0)
				goto err_out;

			err = rdma_resolve_route(event_copy.id, INFINIBAND_TIMEOUT);
			if(err != 0) {
				printf("rdma_resolve_route %d in %s.\n", err, __func__);
				goto err_out;
			}
		} else if(event_copy.event == RDMA_CM_EVENT_ROUTE_RESOLVED) {
			memset(&cm_params, 0, sizeof(struct rdma_conn_param));

			conpara.pm_cp = rpc_s->ptlmap;
			conpara.pm_sp = peer->ptlmap;
			conpara.num_cp = rpc_s->num_rpc_per_buff;
			conpara.type = 0;

			cm_params.private_data = &conpara;
			cm_params.private_data_len = sizeof(conpara);
			cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;	//diff
			cm_params.rnr_retry_count = 7;	/* infinite retry */

			err = rdma_connect(event_copy.id, &cm_params);
			if(err != 0) {
				printf("rdma_connect %d in %s.\n", err, __func__);
				goto err_out;
			}
		} else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			peer->sys_conn.f_connected = 1;
			check = 1;
		} else {
			err = event_copy.status;
			goto err_out;
		}

		if(check == 1)
			break;
	}

	return 0;
err_out:
    printf("'%s()': failed with %d peer# %d peer# %d.\n", __func__, err, rpc_s->ptlmap.id, peer->ptlmap.id);
	return err;
}

/* -------------------------------------------------------------------
  Message service
*/
struct msg_buf *msg_buf_alloc(struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs)	//Done
{
	struct msg_buf *msg;
	size_t size;

	size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd) * num_rpcs + 7;
	msg = calloc(1, size);
	if(!msg)
		return NULL;

	msg->peer = peer;
	msg->cb = default_completion_callback;

	if(num_rpcs > 0) {
		msg->msg_rpc = (struct rpc_cmd *) (msg + 1);
		ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
		msg->msg_rpc->dst = peer->ptlmap.address;
		msg->msg_rpc->src = rpc_s->ptlmap.address;
	}

	return msg;
}

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)	//Done
{
	rpc_commands[num_service].rpc_cmd = rpc_cmd;
	rpc_commands[num_service].rpc_func = rpc_func;
	num_service++;
}

static int rpc_process(struct rpc_server *rpc_s, struct node_id *peer)
{
    int ret, err;
    struct ibv_cq *ev_cq;
    void *ev_ctx;
    struct ibv_wc wc;
    struct rpc_request *rr;
    //Add for pthread
    pthread_t callThrd[1];
    struct PassArg *passArg;




//data transfer between ibv_get_cq_event and ibv_ack_cq_events
    //uloga("%s(Yubo) before ibv_get_cq_event, at time=%f\n",__func__, timer_timestamp_0());
    err = ibv_get_cq_event(peer->rpc_conn.comp_channel, &ev_cq, &ev_ctx);
    if(err == -1 && errno == EAGAIN) {
        return 0;
    }
    if(err == -1 && errno != EAGAIN) {
        printf("Failed to get cq_event: %s\n", strerror(errno));
        err = errno;
        goto err_out;
    }
    ibv_ack_cq_events(ev_cq, 1);
    

    err = ibv_req_notify_cq(ev_cq, 0);
   //uloga("%s(Yubo) after ibv_req_notify_cq, at time=%f\n",__func__, timer_timestamp_0());
    if(err != 0) {
        printf("Failed to ibv_req_notify_cq.\n");
        fprintf(stderr, "Failed to ibv_req_notify_cq.\n");
        goto err_out;
    }

    do {
        ret = ibv_poll_cq(ev_cq, 1, &wc);
        if(ret < 0) {
            printf("Failed to ibv_poll_cq.\n");
            fprintf(stderr, "Failed to ibv_poll_cq.\n");
            goto err_out;
        }

        //uloga("%s(Yubo) ret = %d\n", __func__, ret);

        if(ret == 0) {
            continue;
        }
        if(wc.status != IBV_WC_SUCCESS) {
            const char *descr;
             
            descr = ibv_wc_status_str(IBV_WC_SUCCESS);
             
            printf("The description of the enumerated value %d is %s\n", IBV_WC_SUCCESS, descr);
            err = -wc.status;
            return 0;
            goto err_out;
        }
        if(wc.opcode & IBV_WC_RECV) {
        	//uloga("%s(Yubo) before rpc_cb_decode, at time=%f\n",__func__, timer_timestamp_0());
            //rpc_cb_decode(rpc_s, &wc);
           // uloga("%s(Yubo) after rpc_cb_decode, at time=%f\n",__func__, timer_timestamp_0());

        	//Put rpc_s and wc to struct passArg, passing to pthread, Yubo
        	passArg = malloc(sizeof(struct PassArg));

        	(*passArg).rpc_s = rpc_s;
        	(*passArg).wc = &wc;

        	pthread_create(&callThrd[0], NULL, rpc_cb_decode_thrd, (void*) passArg);

        	pthread_join(callThrd[0], NULL);

        	free(passArg);


            peer->num_recv_buf--;

            struct ibv_recv_wr wr, *bad_wr = NULL;
            struct ibv_sge sge;
            int err = 0;


            wr.wr_id = (uintptr_t) wc.wr_id;
            wr.next = NULL;
            wr.sg_list = &sge;
            wr.num_sge = 1;

            sge.addr = (uintptr_t) wc.wr_id;
            sge.length = sizeof(struct rpc_cmd);
            sge.lkey = peer->rr->rpc_mr->lkey;

            err = ibv_post_recv(peer->rpc_conn.qp, &wr, &bad_wr);

            peer->num_recv_buf++;


        } else if(wc.opcode == IBV_WC_SEND || wc.opcode == IBV_WC_RDMA_WRITE || wc.opcode == IBV_WC_RDMA_READ) {
            peer->req_posted--;//debug
            rr = (struct rpc_request *) (uintptr_t) wc.wr_id;

            err = (*rr->cb) (rpc_s, &wc);
            if(err != 0) {
                goto err_out;
            }
        } else {
            printf("Weird wc.opcode %d.\n", wc.opcode);//debug
            err = wc.opcode;
            goto err_out;
        }

    } while(ret);

    return 0;

err_out:
    printf("(%s): err (%d).\n", __func__, err);
    return err;

}

//Use poll 2
static int __process_event(struct rpc_server *rpc_s, int timeout)   //Done
{
    struct node_id *peer;
    struct pollfd my_pollfd[2 * rpc_s->num_peers - 2];
    int which_peer[2 * rpc_s->num_peers - 2];
    int sys_peer[2 * rpc_s->num_peers - 2];
    // initialize file descriptor set
    int i, j, err = 0;
    
    j = 0;
    list_for_each_entry(peer, &rpc_s->peer_list, struct node_id, peer_entry) {
    
        if(peer->sys_conn.f_connected == 1) {    
            my_pollfd[j].fd = peer->sys_conn.comp_channel->fd;
			my_pollfd[j].events = POLLIN;
			my_pollfd[j].revents = 0;
			which_peer[j] = peer->ptlmap.id;
			sys_peer[j] = 1;
			j++;
		}

		if(peer->rpc_conn.f_connected == 1) {
			my_pollfd[j].fd = peer->rpc_conn.comp_channel->fd;
			my_pollfd[j].events = POLLIN;
			my_pollfd[j].revents = 0;
			which_peer[j] = peer->ptlmap.id;
			sys_peer[j] = 0;
			j++;
		}
	}

	if(j == 0)
		return 0;

	err = poll(my_pollfd, j, timeout);

	if(err < 0) {
		return 0;
	} else if(err == 0) {
		return -ETIME;
	} else if(err > 0) {
		for(i = 0; i < j; i++) {
            if(sys_peer[i] == 1 && my_pollfd[i].revents != 0) {
                peer = rpc_get_peer(rpc_s, which_peer[i]);
                err = sys_process(rpc_s, peer);
                if(err < 0)
                    goto err_out;
            }
            if(sys_peer[i] == 0 && my_pollfd[i].revents != 0) {
                peer = rpc_get_peer(rpc_s, which_peer[i]);
                err = rpc_process(rpc_s, peer);
                if(err < 0)
                    goto err_out;
            }
		}
	}

	return 0;

err_out:
	printf("%d (%s): err (%d). %d\n",rpc_s->ptlmap.id, __func__, err, peer->ptlmap.id);
    return err;
}

int rpc_process_event(struct rpc_server *rpc_s)	//Done
{
	int err;

	err = __process_event(rpc_s, 100);
	if(err != 0 && err != -ETIME)
		goto err_out;

	return 0;

      err_out:
	printf("(%s): err (%d).\n", __func__, err);
	return err;
}

int rpc_process_event_with_timeout(struct rpc_server *rpc_s, int timeout)	//Done
{
	int err;

	err = __process_event(rpc_s, timeout);
	if(err != 0 && err != -ETIME)
		goto err_out;

	return err;

      err_out:
	printf("(%s): err (%d).\n", __func__, err);
	return err;
}

/* -------------------------------------------------------------------
  Data operation
*/
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)	//Done
{
	if(peer->rpc_conn.f_connected != 1) {
		rpc_connect(rpc_s, peer);
	}

	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

	rr->type = 0;
	rr->msg = msg;
	rr->iodir = io_send;
	rr->cb = (async_callback) rpc_cb_req_completion;

	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);

	list_add_tail(&rr->req_entry, &peer->req_list);

	peer->num_req++;
	peer->req_posted++;

	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;

      err_out:
	printf("%d to %d '%s()': failed with %d.\n", rpc_s->ptlmap.id, peer->ptlmap.id, __func__, err);
	return err;
}


inline static int __send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg, flag_t f_vec)	//Done
{
	if(peer->rpc_conn.f_connected != 1) {
		rpc_connect(rpc_s, peer);
		//printf("RPC channel has not been established  %d %d\n", rpc_s->ptlmap.id, peer->ptlmap.id);
		//goto err_out;
	}

	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;


	rr->type = 1;		//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->iodir = io_send;
	rr->cb = (async_callback) rpc_cb_req_completion;
	rr->data = msg->msg_data;
	rr->size = msg->size;
	rr->f_vec = 0;

	//list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
	//rpc_s->rr_num++;

	peer->req_posted++;

	err = rpc_post_request(rpc_s, peer, rr, 0);
	if(err != 0) {
		free(rr);
		goto err_out;
	}

	return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)	//Done
{
	int err;

	err = __send_direct(rpc_s, peer, msg, unset);
	if(err == 0)
		return 0;

	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

inline static int __receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg, flag_t f_vec)	//Done
{
	if(peer->rpc_conn.f_connected != 1) {
		rpc_connect(rpc_s, peer);
	}

	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

	rr->type = 0;		//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->iodir = io_receive;
	rr->cb = (async_callback) rpc_cb_req_completion;

	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
	rr->f_vec = 0;


	list_add_tail(&rr->req_entry, &peer->req_list);

	peer->num_req++;
	peer->req_posted++;

	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)	//Done
{
	int err;

	err = __receive(rpc_s, peer, msg, unset);
	if(err == 0)
		return 0;

	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)	//Done
{
	if(peer->rpc_conn.f_connected != 1) {
		printf("RPC channel has not been established  %d %d\n", rpc_s->ptlmap.id, peer->ptlmap.id);
		goto err_out;
	}

	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

	rr->type = 1;		//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->cb = (async_callback) rpc_cb_req_completion;
	rr->data = msg->msg_data;
	rr->size = msg->size;


	peer->req_posted++;

	err = rpc_fetch_request(rpc_s, peer, rr);
	if(err == 0)
		return 0;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}



//Not be used in InfiniBand version. Just keep an empty func here.
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	printf("%s.\n", __func__);
	return 0;
}

//Not be used in InfiniBand version. Just keep an empty func here.
int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	printf("%s.\n", __func__);
	return 0;
}

/*
	Other operations
*/
int rpc_read_config(int appid, struct sockaddr_in *address)	
{
	char *ip;
	char *port;
	FILE *f;
	int err;
	char ipstore[16];	//? need check if IPv6 works in IB
	char *tmp_ip;
	tmp_ip = ipstore;
	int tmp_port;
    char conf_filename[20];
    char version[16];

	ip = getenv("P2TNID");
	port = getenv("P2TPID");

	if(ip && port) {
		address->sin_addr.s_addr = inet_addr(ip);
		address->sin_port = htons(atoi(port));

		return 0;
	}

    sprintf(conf_filename, "conf.%d", appid);
    f = fopen(conf_filename, "rt");
	if(!f) {
		err = -ENOENT;
		goto err_out;
	}

    err = fscanf(f, "P2TNID=%s\nP2TPID=%d\n%s\n", tmp_ip, &tmp_port,version);
        

    if(strcmp(version,VERSION)!=0) {
        printf("Warning: DataSpaces sever(s) and client(s) have mis-matched version\n");
    }


	address->sin_addr.s_addr = inet_addr(tmp_ip);
	address->sin_port = htons(tmp_port);

	fclose(f);
	if(err == 3)
		return 0;

	err = -EIO;

      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_write_config(int appid, struct rpc_server *rpc_s)
{
	FILE *f;
	int err;
    char conf_filename[20];

	sprintf(conf_filename, "conf.%d", appid);
    f = fopen(conf_filename, "wt");
    if(!f) {
		goto err_out;
    }
    err = fprintf(f, "P2TNID=%s\nP2TPID=%d\n%s\n", inet_ntoa(rpc_s->ptlmap.address.sin_addr), ntohs(rpc_s->ptlmap.address.sin_port),VERSION);

	if(err < 0)
		goto err_out_close;
	fclose(f);

	return 0;

      err_out_close:
	fclose(f);
      err_out:
	printf("'%s()' failed with %d.", __func__, err);
	return -EIO;
}

/*
void rpc_mem_info_cache(struct rpc_server *rpc_s, struct node_id *peer, struct rpc_cmd *cmd)
{
	peer->peer_mr.mr = cmd->mr;	//Modified by Tong Jin for IB
	peer->peer_mr.wrid = cmd->wr_id;	//Modified by Tong Jin for IB

	return;
}
*/


void rpc_report_md_usage(struct rpc_server *rpc_s)
{
	//printf("'%s()': MD posted %d, MD released %d, MD in use %d.\n", __func__, rpc_s->num_md_posted, rpc_s->num_md_unlinked, rpc_s->num_md_unlinked - rpc_s->num_md_posted);
}

void rpc_print_connection_err(struct rpc_server *rpc_s, struct node_id *peer, struct rdma_cm_event event)
{

	if(event.event == RDMA_CM_EVENT_DISCONNECTED)
		printf("Peer Disconnected ");
	else if(event.event == RDMA_CM_EVENT_REJECTED)
		printf("Connection Rejected ");
	else if(event.event == RDMA_CM_EVENT_ADDR_ERROR)
		printf("Connection Address Error ");
	else if(event.event == RDMA_CM_EVENT_ROUTE_ERROR)
		printf("Connection Route Error ");
// %s .\n", inet_ntoa(peer->ptlmap.address.sin_addr));
	else if(event.event == RDMA_CM_EVENT_CONNECT_RESPONSE)
		printf("Connection Connect Response ");
	else if(event.event == RDMA_CM_EVENT_CONNECT_ERROR)
		printf("Connection Connect Error ");
	else if(event.event == RDMA_CM_EVENT_UNREACHABLE)
		printf("Connection Unreachable ");
	else if(event.event == RDMA_CM_EVENT_DEVICE_REMOVAL)
		printf("Connection Device Removal ");
	else if(event.event == RDMA_CM_EVENT_MULTICAST_JOIN)
		printf("Connection Multicast Join ");
	else if(event.event == RDMA_CM_EVENT_MULTICAST_ERROR)
		printf("Connection Multicast Error ");
	else if(event.event == RDMA_CM_EVENT_ADDR_CHANGE)
		printf("Connection Addr Changed ");
	else if(event.event == RDMA_CM_EVENT_TIMEWAIT_EXIT)
		printf("Connection Timewait Exit ");
	else if(event.event == RDMA_CM_EVENT_CONNECT_REQUEST)
		printf("Connection Timewait Exit ");
	printf("peer# %d (%s)  to peer# %d (%s)\n", rpc_s->ptlmap.id, inet_ntoa(rpc_s->ptlmap.address.sin_addr), peer->ptlmap.id, inet_ntoa(peer->ptlmap.address.sin_addr));
    return;
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
	msg->id = (uintptr_t) cmd->wr_id;
	msg->mr = cmd->mr;

	return;
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
	return;
}

void rpc_cache_msg(struct msg_buf *msg, struct rpc_cmd *cmd)
{
	msg->id = cmd->wr_id;
	msg->mr = cmd->mr;

	return;
}
