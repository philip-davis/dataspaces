/*
 *  Base implementation of DART client.
 *
 *  Tong Jin 
 *  TASSL Rutgers University
 *  Hoang Bui (2012-2013) TASSL Rutgers University
 *  hbui@cac.rutgers.edu
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/stat.h>

#include "debug.h"
#include "dc_base_ib.h"

/* ---------------------------------------------------------------------------------
  DC Barrier
*/
static int barrier_broadcast(struct dart_client *dc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int i, err;
	for(i = 1; i < dc->cp_in_job; i++) {
		peer = dc_get_peer(dc, dc->self->ptlmap.id + i);
		err = -ENOMEM;
		msg = msg_buf_alloc(dc->rpc_s, peer, 1);
		if(!msg)
			break;
		msg->msg_rpc->cmd = cp_barrier;
		msg->msg_rpc->id = dc->self->ptlmap.id;
		err = rpc_send(dc->rpc_s, peer, msg);
		if(err < 0) {
			free(msg);
			break;
		}
	}
	dc->cp_barrier_req = 0;
	dc->f_bar = 1;
	if(i == dc->cp_in_job)
		return 0;
	return err;
}


/* 
   Implement a simple 'start' barrier across the nodes in an app job.
*/
static int dcrpc_barrier(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);

	//        struct msg_buf *msg;
	int err;
	if(dc->self->ptlmap.id == dc->cp_min_rank) {

		/* I am the master peer in this job. */
		dc->cp_barrier_req++;
		if(dc->cp_barrier_req < dc->cp_in_job)
			return 0;
		err = barrier_broadcast(dc);
		if(err < 0)
			goto err_out;
	}

	else {

		/* Non master peer in this job. */
		dc->f_bar = 1;
	}
	return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}




static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct ptlid_map *pm;
	int i;
	pm = (struct ptlid_map *) msg->msg_data;
//	dc->peer_tab = rpc_s->peer_tab;
//	dc->cn_peers = dc_get_peer(dc, rpc_s->app_minid);
//	peer = dc->peer_tab;

//	peer++;
//	pm++;

	for(i = 0; i < dc->num_sp + dc->num_cp; i++) {
                struct node_id *temp_peer = peer_alloc();

		temp_peer->ptlmap = *pm;

      		printf("Client %d peer %d:  %s %d \n",rpc_s->ptlmap.id, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
		if(temp_peer->ptlmap.id == 0)
			continue;

		if(temp_peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr && temp_peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port)
			dc->self = temp_peer;

        	list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
		

	        INIT_LIST_HEAD(&temp_peer->req_list);
	        temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
        	temp_peer->num_msg_ret = 0;


		pm = pm + 1;
	}


i = 0;
struct node_id *temp_peer;
list_for_each_entry(temp_peer, &rpc_s->peer_list, struct node_id, peer_entry) {
                i++;
                printf("after %d peer# %d (%s:%d)\n",i, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
        if(i==100) break;
}


	dc->cp_min_rank = dc->rpc_s->app_minid;
	dc->f_reg = 1;
	free(msg->msg_data);
	free(msg);
	return 0;
}

/*
static int announce_cp_completion_all(struct rpc_server *rpc_s, struct msg_buf *msg)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct ptlid_map *pm;
	int i;
	pm = (struct ptlid_map *) msg->msg_data;
	dc->peer_tab = rpc_s->peer_tab;
//	dc->cn_peers = dc_get_peer(dc, rpc_s->app_minid);
	peer = dc->peer_tab;

	peer++;
	pm++;
	for(i = 0; i < dc->num_sp + dc->num_cp - 1; i++) {
		peer++;
	}
	for(i = 0; i < dc->num_sp + dc->num_cp_all - 1; i++) {
		peer->ptlmap = *pm;
		if(peer->ptlmap.appid == rpc_s->ptlmap.appid || peer->ptlmap.appid == 0) {
			pm = pm + 1;
			continue;
		}
		//        printf("2X Client %d peer %d:  %s %d \n",rpc_s->ptlmap.id,peer->ptlmap.id, inet_ntoa(peer->ptlmap.address.sin_addr),ntohs(peer->ptlmap.address.sin_port));


                struct node_id *temp_peer = peer_alloc();
                list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);

                temp_peer->ptlmap = *pm;

                INIT_LIST_HEAD(&temp_peer->req_list);
                temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
                temp_peer->num_msg_ret = 0;



		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = rpc_s->max_num_msg;
		peer->num_msg_ret = 0;
		peer++;
		pm = pm + 1;
	}
	dc->cp_min_rank = dc->rpc_s->app_minid;
	dc->f_reg_all = 1;
	free(msg->msg_data);
	free(msg);
	return 0;
}
*/

static int dcrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	dc->num_sp = hreg->num_sp;
	dc->num_cp = hreg->num_cp;
//	peer = rpc_s->peer_tab;
	peer = dc_get_peer(dc, 0);

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (hreg->num_cp + hreg->num_sp);
	msg->msg_data = malloc(msg->size);
	if(!msg->msg_data) {
		free(msg);
		goto err_out;
	}
	msg->cb = announce_cp_completion;	//

	msg->id = cmd->wr_id;
	msg->mr = cmd->mr;


	err = rpc_receive_direct(rpc_s, peer, msg);

	if(err < 0) {
		free(msg);
		goto err_out;
	}
	return 0;
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}

/*
static int dcrpc_announce_cp_all(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	dc->num_sp = hreg->num_sp;
	dc->num_cp_all = hreg->num_cp;
	peer = rpc_s->peer_tab;
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (hreg->num_cp + hreg->num_sp);
	msg->msg_data = malloc(msg->size);
	if(!msg->msg_data) {
		free(msg);
		goto err_out;
	}
	msg->cb = announce_cp_completion_all;	//

	msg->id = cmd->wr_id;
	msg->mr = cmd->mr;


	err = rpc_receive_direct(rpc_s, peer, msg);

	if(err < 0) {
		free(msg);
		goto err_out;
	}
	return 0;
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}
*/

//RPC routine  to wait for  server confirmation that it  processed ourunregister message and all our other messages.
static int dcrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	dc->f_reg = 0;
	return 0;
}

static int dc_unregister(struct dart_client *dc)	//Done
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;
	sp_index = dc->self->ptlmap.id % dc->num_sp;
	peer = dc_get_peer(dc, sp_index);
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->msg_rpc->cmd = cn_unregister;
	msg->msg_rpc->id = dc->self->ptlmap.id;
	hreg = (struct hdr_register *) msg->msg_rpc->pad;
	hreg->num_sp = dc->num_sp;
	hreg->num_cp = 1;
	hreg->pm_cp = dc->self->ptlmap;
	hreg->pm_sp = peer->ptlmap;
	err = rpc_send(dc->rpc_s, peer, msg);
	if(err < 0)
		goto err_out_free;

	// Should wait here for 'unregister' confirmation. 
	while(dc->f_reg) {
		err = rpc_process_event(dc->rpc_s);
		if(err < 0)
			goto err_out;
	}
	return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()': failed.\n", __func__);
	return err;
}


/*COMMENTED
static int data_transfer_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);

        dc->num_posted--;

        free(msg->msg_data);
        free(msg);

        // uloga("'%s()': transfer complete.\n", __func__);
        return 0;
}

int log2_ceil2(int n)		//Done
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
*/

static void *dc_listen(void *client)
{
	struct dart_client *dc = (struct dart_client *) client;
	struct node_id *peer = dc_get_peer(dc, 0);
	struct rdma_conn_param cm_params;
	struct con_param conpara;
	struct connection *con;
	struct rdma_cm_event *event = NULL;
	int j, err, check, connected, connect_count = 0;
	check = 0;
	connected = 0;
	peer = NULL;
	while(dc->rpc_s->thread_alive && (rdma_get_cm_event(dc->rpc_s->rpc_ec, &event) == 0)) {

		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);
		if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
			conpara = *(struct con_param *) event_copy.param.conn.private_data;
			peer = dc_get_peer(dc, conpara.pm_cp.id);
			if(conpara.type == 0) {
                               // printf("Client %d: SYS connect request from %d\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
				con = &peer->sys_conn;
				if(con->f_connected ==1)
				{
	                                printf("Client %d: SYS connect request from %d, but I am already connected\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
				}
				dc->rpc_s->sys_conn_count++;
			} else {
//                                printf("Client %d: RPC connect request from %d\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
				con = &peer->rpc_conn;
			}
			build_context(event_copy.id->verbs, con);
			build_qp_attr(&con->qp_attr, con, dc->rpc_s);
			err = rdma_create_qp(event_copy.id, con->pd, &con->qp_attr);
			if(err != 0) {
				printf("rdma_create_qp %d in %s.\n", err, __func__);
				goto err_out;
			}
			event_copy.id->context = con;
			con->id = event_copy.id;
			con->qp = event_copy.id->qp;
			if(conpara.type == 0) {
				err = sys_post_recv(dc->rpc_s, peer);
			} else {
				err = rpc_post_recv(dc->rpc_s, peer);
			}
			if(err != 0)
				goto err_out;

			memset(&cm_params, 0, sizeof(struct rdma_conn_param));
			cm_params.private_data = &peer->ptlmap.id;
			cm_params.private_data_len = sizeof(int);
			cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;
			cm_params.rnr_retry_count = 7;	//infinite retry
			err = rdma_accept(event_copy.id, &cm_params);
			if(err != 0) {
				printf("rdma_accept %d in %s.\n", err, __func__);
				goto err_out;
			}
			con->f_connected = 1;
			connect_count++;
		} else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			dc->connected++;
			connected++;
		} else if(event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
		} else {
			rpc_print_connection_err(dc->rpc_s, peer, event_copy);
			printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
			err = event_copy.status;
		}

	}

	pthread_exit(0);
	return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	pthread_exit(0);
	return 0;
}




static int dc_boot(struct dart_client *dc, int appid)	//Done
{
	struct node_id *peer = malloc(sizeof(struct node_id));
	memset(peer, 0, sizeof(struct node_id));
//	dc->peer_tab = peer;
	dc->rpc_s->cur_num_peer = 2;	//diff another 2
	struct rdma_conn_param cm_params;
	struct con_param conpara;
	struct connection *con;
	struct rdma_cm_event *event = NULL;
	int i, err, check, connected = 0;
	int connect_count = 0;
	check = 0;
	err = 0;


        struct node_id *temp_peer = peer_alloc();
        list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);

        err = rpc_read_config(&temp_peer->ptlmap.address);   ////
	temp_peer->ptlmap.id = 0;

        INIT_LIST_HEAD(&temp_peer->req_list);
        temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
        temp_peer->num_msg_ret = 0;



	err = rpc_read_config(&temp_peer->ptlmap.address);	////
	if(err < 0)
		goto err_out;
	//Connect to master server, build rpc channel and sys channel;
	err = rpc_connect(dc->rpc_s, temp_peer);
	if(err != 0) {
		printf("rpc_connect err %d in %s.\n", err, __func__);
		goto err_out;
	}

	dc->cp_min_rank = dc->rpc_s->app_minid;
//        dc->rpc_s->peer_tab = peer;       //diff

//	dc->rpc_s->peer_tab[0] = peer[0];	//diff
//	dc->rpc_s->peer_tab[1].ptlmap = dc->rpc_s->ptlmap;
//	INIT_LIST_HEAD(&dc->rpc_s->peer_tab[0].req_list);

//	free(peer);

	//Waiting for dissemination msg from master server;
	while(dc->f_reg == 0) {
		err = rpc_process_event_with_timeout(dc->rpc_s, 1);
		if(err != 0 && err != -ETIME)
			goto err_out;
	}


	sleep(5);

	int rc;
	dc->rpc_s->thread_alive = 1;
	rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_listen, (void *) dc);
	if(rc) {
		printf("ERROR; return code from pthread_create() is %d\n", rc);
		exit(-1);
	}

	int n = log2_ceil(dc->peer_size);
	int *check_cp = malloc(sizeof(int) * (dc->peer_size + dc->cp_min_rank));

	int j;

	for(j = 0; j < dc->peer_size + dc->cp_min_rank; j++)
		check_cp[j] = 0;

	int *a = malloc(sizeof(int) * n);


	int k;
	int smaller_cid = 0;
	int greater_cid = 0;

        printf("MIN %d %d\n", dc->cp_min_rank, dc->peer_size + dc->cp_min_rank);


	for(k = dc->cp_min_rank; k < dc->peer_size + dc->cp_min_rank; k++) {

		a[0] = 1;
		for(j = 1; j < n; j++) {
			a[j] = a[j - 1] * 2;
		}

		for(j = 0; j < n; j++) {
			a[j] = (a[j] + k - dc->cp_min_rank);
			if(a[j] > dc->peer_size - 1)
				a[j] = a[j] % dc->peer_size;

			if(k == dc->rpc_s->ptlmap.id) {
				check_cp[a[j] + dc->cp_min_rank] = 1;

			}
			if(a[j] + dc->cp_min_rank == dc->rpc_s->ptlmap.id) {
				check_cp[k] = 1;

			}
		}
	}
	for(k = dc->cp_min_rank; k < dc->peer_size + dc->cp_min_rank; k++) {
		if(check_cp[k] == 1) {
			if(k < dc->rpc_s->ptlmap.id)
				smaller_cid++;
			else
				greater_cid++;
		}
	}


	for(i = 1; i < dc->self->ptlmap.id; i++) {
		if(i > dc->num_sp - 1 && i < dc->cp_min_rank)
			continue;

		int count = 0;
		peer = dc_get_peer(dc, i);
		if(i < dc->cp_min_rank) {
			do {
				err = rpc_connect(dc->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("rpc_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}

		}
		if(check_cp[peer->ptlmap.id] == 1) {
			count = 0;
			do {
                              printf("SYS connect %d %d\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
				err = sys_connect(dc->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("sys_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}
		}
	}

sleep(15);

/*
	while(dc->rpc_s->sys_conn_count!= greater_cid){
	//	err = rpc_process_event_with_timeout(dc->rpc_s, 1);
        //        if(err != 0 && err != -ETIME)
        //                goto err_out;
        	printf("I am %d %d %d\n",dc->self->ptlmap.id, dc->rpc_s->sys_conn_count, greater_cid);
        
        	sleep(1);
        }
*/
		


	//Function: connect to all other nodes except MS_Server.

/*
	for(i = 1; i < dc->self->ptlmap.id; i++) {
		if(i > dc->num_sp - 1 && i < dc->cp_min_rank)
			continue;

		int count = 0;
		peer = dc_get_peer(dc, i);
		//if(i < dc->cp_min_rank) {
			do {
				err = rpc_connect(dc->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("rpc_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}

		//}
		if(peer->ptlmap.id >=dc->num_sp){
			count = 0;
			do {
				err = sys_connect(dc->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("sys_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}
		}
	}

*/


/*
	if(i == dc->self->ptlmap.id && dc->self->ptlmap.id != (dc->cp_min_rank + dc->num_cp - 1)) {
		peer = NULL;
		while(rdma_get_cm_event(dc->rpc_s->rpc_ec, &event) == 0) {
			struct rdma_cm_event event_copy;
			memcpy(&event_copy, event, sizeof(*event));
			rdma_ack_cm_event(event);
			if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {

				//printf("received connection request.\n");//debug
				conpara = *(struct con_param *) event_copy.param.conn.private_data;
				peer = dc_get_peer(dc, conpara.pm_cp.id);
				if(conpara.type == 0)
					con = &peer->sys_conn;

				else
					con = &peer->rpc_conn;
				build_context(event_copy.id->verbs, con);
				build_qp_attr(&con->qp_attr, con, dc->rpc_s);
				err = rdma_create_qp(event_copy.id, con->pd, &con->qp_attr);
				if(err != 0) {
					printf("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, dc->rpc_s->num_qp, err, __func__, strerror(errno));
					goto err_out;
				}
				dc->rpc_s->num_qp++;
				event_copy.id->context = con;
				con->id = event_copy.id;	//diff
				con->qp = event_copy.id->qp;
				if(conpara.type == 0) {

//                                      for(j=0;j<SYS_NUM;j++){
					err = sys_post_recv(dc->rpc_s, peer);
					if(err != 0)
						goto err_out;

//                                      }
				}

				else {

					//for(j=0;j<RPC_NUM;j++){
					err = rpc_post_recv(dc->rpc_s, peer);

//                                              printf("I am %d to peer %d done preapreing buffer\n",dc->self->ptlmap.id,peer->ptlmap.id);
					if(err != 0)
						goto err_out;

					//}
				}
				memset(&cm_params, 0, sizeof(struct rdma_conn_param));
				cm_params.private_data = &peer->ptlmap.id;
				cm_params.private_data_len = sizeof(int);
				cm_params.initiator_depth = cm_params.responder_resources = 1;
				cm_params.retry_count = 7;
				cm_params.rnr_retry_count = 7;	
				err = rdma_accept(event_copy.id, &cm_params);
				if(err != 0) {
					printf("rdma_accept %d in %s.\n", err, __func__);
					goto err_out;
				}
				con->f_connected = 1;
				connect_count++;
			}

			else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {

				//printf("Connection Established.\n");
				connected++;

				//printf("peer %d %d %d\n",dc->rpc_s->ptlmap.id, connected,2*(dc->num_cp-dc->self->ptlmap.id+dc->cp_min_rank-1));
			}

			else {
				rpc_print_connection_err(dc->rpc_s, peer, event_copy);
				printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
				err = event_copy.status;
				goto err_out;
			}

			//if(connected == 2*(dc->num_cp-dc->self->ptlmap.id+dc->cp_min_rank-1)){
			//if(connected == (dc->num_cp-dc->self->ptlmap.id+dc->cp_min_rank-1) + greater_cid){
			if(connected == 2*(dc->num_cp + dc->cp_min_rank  - dc->self->ptlmap.id - 1)) {
				break;
			}
		}


/*
		if(connected != connect_count){
			printf("Connected number doesn't match needed.\n");
			err = -1;
			goto err_out;
		}
*/
//      }

/*
	for(i=0;i<dc->num_sp + dc->num_cp;i++){
		peer = &dc->peer_tab[i];
		if((peer->rpc_conn.f_connected && peer->sys_conn.f_connected) || peer->ptlmap.id == dc->self->ptlmap.id)
			continue;
		printf("Peer %d is not connected!\n", peer->ptlmap.id);
		err = -1;
		goto err_out;
	}
*/


//	while(dc->f_reg_all == 0) {
//		err = rpc_process_event_with_timeout(dc->rpc_s, 1);
/*
	printf("waitting %d\n",dc->connected);

         struct node_id *temp_peer;
         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
                printf("Clientpeer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
         }

        if(dc->connected>4){
        temp_peer =dc_node_find(dc, 2);

        	printf("peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
	}


        printf("***********************************\n");



*/


//		if(err != 0 && err != -ETIME)
//			goto err_out;
//	}


//         struct node_id *temp_peer;
         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
                printf("Client peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
         }

        if(dc->connected>4){
        temp_peer =rpc_server_find(dc->rpc_s, 2);

                printf("peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
        }


        printf("***********************************\n");



	dc->rpc_s->cur_num_peer = dc->num_sp + dc->num_cp_all;

	dc->rpc_s->num_sp = dc->num_sp;

//        if(dc->rpc_s->ptlmap.id == 7 || dc->rpc_s->ptlmap.id == 17){
//                printf("Client %d %d %d %d\n",dc->rpc_s->ptlmap.id,smaller_cid, greater_cid,greater_cid + smaller_cid);
//        }
	return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}


/*
  Public API starts here.
*/
struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref)
{
	struct dart_client *dc;

	//        size_t size;
	int err;
	dc = calloc(1, sizeof(*dc));
	if(!dc)
		return NULL;
	dc->dart_ref = dart_ref;
	dc->cp_in_job = num_peers;

//        INIT_LIST_HEAD(&dc->rpc_s->peer_list);

	dc->connected = 0;
	// dc->peer_tab = (struct node_id *) (dc+1);
	 dc->peer_size = num_peers;

////    rpc_add_service(cn_register, dcrpc_register);
	rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);
	rpc_add_service(sp_announce_cp, dcrpc_announce_cp);
//	rpc_add_service(sp_announce_cp_all, dcrpc_announce_cp_all);

	dc->rpc_s = rpc_server_init(0, NULL, 0, 10, num_peers, dc, DART_CLIENT);
	if(!dc->rpc_s) {
		free(dc);
		return NULL;
	}
	dc->rpc_s->ptlmap.appid = appid;
	err = dc_boot(dc, appid);

	//printf("peer# %d address %s\n", dc->rpc_s->ptlmap.id,inet_ntoa(dc->rpc_s->ptlmap.address.sin_addr));


	if(err < 0) {
		rpc_server_free(dc->rpc_s);
		free(dc);
		goto err_out;
	}
	dc->rpc_s->app_num_peers = num_peers;
//	dc->peer_size = dc->rpc_s->cur_num_peer;

	dc->rpc_s->num_peers = dc->num_cp + dc->num_sp;

//	printf("dc_alloc succeed %d.\n", dc->rpc_s->ptlmap.id);
	err = dc_barrier(dc);
	if(err < 0) {
		free(dc);
		goto err_out;
	}

        printf("dc_alloc succeed %d.\n", dc->rpc_s->ptlmap.id);

//printf("barrier is ok.\n");
	return dc;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return NULL;
}

void dc_free(struct dart_client *dc)
{
	int err;

	/* comment
	   while (dc->num_posted)
	   rpc_process_event(dc->rpc_s);
	 */
	dc_unregister(dc);
	err = rpc_server_free(dc->rpc_s);
	if(err != 0)
		printf("rpc_server_free err in %s.\n", __func__);
//	if(dc->peer_tab)
//		free(dc->peer_tab);
	free(dc);
}

int dc_process(struct dart_client *dc)
{
	return rpc_process_event(dc->rpc_s);
}


/*
int dc_send(struct dart_client *dc, void *buf, size_t size)
{
        struct node_id *peer = dc_get_peer(dc, 0);
        struct msg_buf *msg;
        struct rfshdr *rs;
        int err;

        msg = msg_buf_alloc(dc->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = cn_data;
        msg->msg_rpc->id = dc->self->ptlmap.id;

        rs = (struct rfshdr *) msg->msg_rpc->pad;
        strcpy((char *) rs->fname, "m3d.in");
        rs->size =size;
        rs->base_offset = 0;

        msg->msg_data = buf;
        msg->size = size;
        msg->cb = data_transfer_completion;

        err = rpc_send(dc->rpc_s, peer, msg);
        if (err == 0) {
                dc->num_posted++;
                return 0;
        }

        free(msg);
 err_out:
        uloga("'%s()' failed\n", __func__);
        return -1;
}*/
