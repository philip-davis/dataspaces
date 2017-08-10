#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "dc_base_pami.h"
#include "debug.h"

static int check_data(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	printf("receive msg_data size = %llu\n", msg->size);
	size_t i;
	size_t count = 0;
	//int len = 'Z' - 'A' + 1;
	//size_t num_elem = msg->size/sizeof(char);
	size_t num_elem = msg->size/sizeof(double);
	printf("num_elem = %llu\n", num_elem);

	for(i = 0; i < num_elem; i++){
		printf("---3---------\n");
		printf("%f\n", *((double*)msg->msg_data + i));
		//if(*((char*)msg->msg_data + i) != 'A' + (i % len))
		if(*((double*)msg->msg_data + i) != 1)//rpc_s->ptlmap.rank_pami/10)
			count++;
	}
	printf("error=%d\n", count);
}

static int data_read_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	//printf("get into data_read_completion\n");
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        dc->num_posted--;
        //dc->read_complete = 1;

        if(msg && msg->msg_data){
                //int offset = msg->size - 1;
                //*((char*)(msg->msg_data)+offset ) = 0;
                //uloga("%s(): #%u, read data '%s'\n", __func__, rpc_s->ptlmap.rank_pami, msg->msg_data);

		check_data(rpc_s, msg);
                free(msg->msg_data);
                free(msg);
        }

        return 0;
}

int dc_read_test(struct dart_client *dc, size_t size)
{
	uloga("get into dc_read_test\n");
        struct node_id *peer = dc_get_peer(dc, 0);
        struct msg_buf  *msg;
        int err;

	printf("send msg to rank%d, msg->size=%d\n", peer->ptlmap.rank_pami, size);
        msg = msg_buf_alloc(dc->rpc_s, peer, 1);
        if(!msg)
                goto err_out;

        msg->msg_rpc->cmd = cn_read;
        msg->msg_rpc->id = dc->self->ptlmap.id;
        msg->size = size;
        msg->msg_data = calloc(1, msg->size);
        if(!msg->msg_data)
                goto err_out_free;

        msg->cb = data_read_completion;

        err = rpc_receive(dc->rpc_s, peer, msg);
        if(err < 0){
                if(msg->msg_data)
                        free(msg->msg_data);
                goto err_out_free;
        }
        dc->num_posted++;
//        dc->read_complete = 0;

        return 0;
err_out_free:
        if(msg)
                free(msg);
err_out:
        uloga("%s(): #%u, failed\n", __func__, dc->self->ptlmap.rank_pami);
        return -1;

}

int dc_read_test_old(struct rpc_server *rpc_s, size_t size, int rank)
{
	//printf("size=%llu\n", size);
	//size = size * 1024 * 1024;
	//printf("size=%llu\n", size);
	
	int num_peers = 2;

	struct node_id peer;
	memset(&peer, 0, sizeof(struct node_id));
	int cur_rank = rpc_s->ptlmap.rank_pami;
	int target = (cur_rank > 0 ? cur_rank - 1 : num_peers - 1);
	peer.ptlmap.rank_pami = target;
	INIT_LIST_HEAD(&peer.req_list);

	struct msg_buf *msg;
	int err;
	msg = msg_buf_alloc(rpc_s, &peer, 1);
	msg->msg_rpc->cmd = cn_read;
	msg->msg_rpc->id = cur_rank;
	msg->size = size; //size of msg 
	msg->msg_data = calloc(1, msg->size);

	msg->cb = data_read_completion;

	err = rpc_receive(rpc_s, &peer, msg);
	//dc->num_posted++;

	return 0;
}

static int register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	//printf("get into register_completion\n");
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct node_id *peer;
        struct ptlid_map *pptlmap;
        int i, id_min;

        peer = dc->peer_tab;
        pptlmap = msg->msg_data;
        id_min = dc->cp_min_rank;

        for (i = 0; i < dc->peer_size; i++) {
                peer->ptlmap = *pptlmap;
                peer++;
                pptlmap++;
        }

        rpc_s->app_minid = dc->cp_min_rank;
        rpc_s->app_num_peers = dc->cp_in_job;//important

        free(msg->msg_data);
        free(msg);
        dc->f_reg = 1;

#ifdef DEBUG
        uloga("%s(): #%u(dart_id=%d), job has %d peers, starting at %d.\n",
                __func__, rpc_s->ptlmap.rank_pami, rpc_s->ptlmap.id,
                dc->cp_in_job, dc->cp_min_rank);
#endif

        return 0;

}

/*
 *RPC routine to "cn_register"
 */
static int dcrpc_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//printf("get into dcrpc_register\n");
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        size_t size_peertab;
        int i, err = -ENOMEM;

        dc->num_sp = hreg->num_sp;
        dc->num_cp = hreg->num_cp;
        dc->peer_size = dc->num_sp + dc->num_cp;
        dc->cp_min_rank = hreg->id_min;

        /* Allocate peer resources (storage). */
        size_peertab = sizeof(*peer) * dc->peer_size;
        dc->peer_tab = malloc(size_peertab);
        if (!dc->peer_tab)
                        goto err_out;
        memset(dc->peer_tab, 0, size_peertab);
        dc->cn_peers = dc->peer_tab + dc->num_sp;

        peer = dc->peer_tab;
        for (i = 0; i < dc->peer_size; i++) {
                        INIT_LIST_HEAD(&peer->req_list);
                        peer->num_msg_at_peer = rpc_s->max_num_msg;
                        peer->num_msg_ret = 0;
                        peer++;
        }

        peer = dc->peer_tab;
        /* To register and get an ID I sent a message to the servers
 *            group master, adjust its credits here.  */
        peer->num_msg_at_peer--;

        rpc_server_set_peer_ref(rpc_s, dc->peer_tab, dc->peer_size);
        rpc_server_set_rpc_per_buff(rpc_s, dc->num_sp + dc->cp_in_job);

        /* Setup self reference. */
        dc->self = dc_get_peer(dc, hreg->pm_cp.id);
        dc->self->ptlmap = hreg->pm_cp;
        rpc_s->ptlmap = hreg->pm_cp;

        /* Setup server reference, note that cmd->id can be different
 *            than 0 when running with multiple servers. */
        peer = dc_get_peer(dc, cmd->id);
        peer->ptlmap = hreg->pm_sp;
        peer->num_msg_ret++;
        if (peer == dc->peer_tab)
                        dc->peer_tab->num_msg_at_peer++;

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if(!msg)
                goto err_out;

        size_peertab = sizeof(struct ptlid_map) * dc->peer_size;
        msg->msg_data = malloc(size_peertab);
        if(!msg->msg_data){
                free(msg);
                goto err_out;
        }
        msg->size = size_peertab;
        msg->cb = register_completion;

        rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_receive_direct(rpc_s, peer, msg);
        if(err<0)
                goto err_out_free;

        return 0;
err_out_free:
        free(msg->msg_data);
        free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

static int file_exist_nonempty(const char *path)
{
        int err;
        struct stat st_buf;
        err = stat(path, &st_buf);
        if (err != 0)
                return 0;

        if (st_buf.st_size == 0)
                return 0;

        return 1;
}

static int dc_register_at_master(struct dart_client *dc, int appid)
{
	//printf("get into dc_register_at_master\n");
        struct msg_buf *msg;
        struct hdr_register *hr;
        struct node_id peer;
        char conf_file[] = "conf";
        int err;

        // Check if the file "conf" exists 
        while (!file_exist_nonempty(conf_file)) {
                usleep(1000); // Sleep for 1ms
        }

        memset(&peer, 0, sizeof(struct node_id));
        err = rpc_read_config(&(peer.ptlmap.rank_pami), conf_file);
        if (err < 0)
                goto err_out;

        INIT_LIST_HEAD(&peer.req_list);
        peer.num_msg_at_peer = dc->rpc_s->max_num_msg;
        peer.num_msg_ret = 0;

        msg = msg_buf_alloc(dc->rpc_s, &peer, 1);
        if (!msg)
                goto err_out;
        msg->msg_rpc->cmd = cn_register;

        hr = (struct hdr_register *) msg->msg_rpc->pad;
        hr->pm_sp = peer.ptlmap;
        hr->pm_cp.rank_pami = dc->rpc_s->ptlmap.rank_pami;
        hr->pm_cp.id = -1;
        hr->pm_cp.appid = appid;
        hr->num_cp = dc->cp_in_job;

        err = rpc_send(dc->rpc_s, &peer, msg);
        if (err < 0)
                goto err_out_free;

        do {
                rpc_process_event(dc->rpc_s);
        }
        while (!dc->f_reg);

        return 0;
 err_out_free:
        free(msg);
 err_out:
        uloga("'%s()': failed.\n", __func__);
        return -1;

}

/*
 *Public API starts here
 */

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm)
{
	//printf("get into dc_alloc\n");
        struct dart_client *dc;
        size_t size;
        int err;
	MPI_Comm *dc_comm = malloc(sizeof(MPI_Comm));

        dc = calloc(1, sizeof(*dc));
        if(!dc)
                return NULL;
        dc->dart_ref = dart_ref;
        dc->cp_in_job = num_peers;
	
	MPI_Comm_dup(*(MPI_Comm*)comm, dc_comm);
	dc->comm = dc_comm;

        //Register rpc msg handler functions
        rpc_add_service(cn_register, dcrpc_register);

        dc->rpc_s = rpc_server_init(10, num_peers, dc, DART_CLIENT);
        if(!dc->rpc_s){
                free(dc);
                return NULL;
        }

        err = dc_register_at_master(dc, appid);
        if(err<0){
                rpc_server_free(dc->rpc_s);
                free(dc);
                return NULL;
        }

        return dc;
}

int dc_process(struct dart_client *dc)
{
	return rpc_process_event(dc->rpc_s);
}

static int dc_unregister(struct dart_client *dc)
{
	//printf("get into dc_unregister\n");
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
	hreg->num_cp = 1;	//sent from client, one client want to unreg
        hreg->num_sp = dc->num_sp;
        hreg->pm_cp = dc->self->ptlmap;
        hreg->pm_sp = peer->ptlmap;
#ifdef DEBUG
        uloga("%s(): #%u(dart_id=%d) send cn_unregister to #%u(dart_id=%d), hreg->num_sp=%d\n",
                __func__,dc->self->ptlmap.rank_pami,dc->self->ptlmap.id,
                peer->ptlmap.rank_pami,peer->ptlmap.id,hreg->num_sp);
#endif

        err = rpc_send(dc->rpc_s, peer, msg);
        if(err < 0)
                goto err_out_free;

        /* this loop is for clean-up? */
        int i = 0;
        while (i++ < dc->rpc_s->max_num_msg)
                rpc_process_event(dc->rpc_s);

        return 0;
 err_out_free:
        free(msg);
 err_out:
        uloga("'%s()': failed.\n", __func__);
        return err;

}

void dc_free(struct dart_client *dc)
{
#ifdef DEBUG
        uloga("'%s()': #%u num_posted = %d.\n", __func__,dc->self->ptlmap.rank_pami, dc->num_posted);
#endif

	while(dc->num_posted)
		rpc_process_event(dc->rpc_s);
	dc_unregister(dc);

	rpc_server_free(dc->rpc_s);
	if(dc->peer_tab)
		free(dc->peer_tab);

	free(dc);
	uloga("%s(): OK\n", __func__);
}
