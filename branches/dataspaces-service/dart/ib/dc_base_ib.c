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


static int sp_rank_cnt = 1;
static int cp_rank_cnt = 0;




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

	for(i = 0; i < dc->num_sp + dc->num_cp - dc->peer_size - 1; i++) {
                struct node_id *temp_peer = peer_alloc();

		temp_peer->ptlmap = *pm;

//      		printf("AAA Client %d peer %d:  %s %d \n",rpc_s->ptlmap.id, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));

//		if(temp_peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr && temp_peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port)
//			dc->self = temp_peer;

        	list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
		

	        INIT_LIST_HEAD(&temp_peer->req_list);
	        temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
        	temp_peer->num_msg_ret = 0;


		pm = pm + 1;
	}

/*
i = 0;
struct node_id *temp_peer;
list_for_each_entry(temp_peer, &rpc_s->peer_list, struct node_id, peer_entry) {
                i++;
                printf("after %d peer# %d (%s:%d)\n",i, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
        if(i==100) break;
}
*/

	dc->cp_min_rank = dc->rpc_s->app_minid;
	dc->f_reg = 1;
	//free(msg->msg_data);
	//free(msg);
	return 0;
}

static int announce_cp_completion_all(struct rpc_server *rpc_s, struct msg_buf *msg)        //Done
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct node_id *peer;
        struct ptlid_map *pm;
        int i;
        pm = (struct ptlid_map *) msg->msg_data;



        for(i = 0; i < dc->num_sp + dc->num_cp; i++) {
                struct node_id *temp_peer = peer_alloc();

                temp_peer->ptlmap = *pm;

//                printf("HELLO Client %d peer %d:  %s %d \n",rpc_s->ptlmap.id, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));

              if(temp_peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr && temp_peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port){

                      dc->self = temp_peer;
			dc->rpc_s->ptlmap.id = temp_peer->ptlmap.id;
	}
		if(temp_peer->ptlmap.id==dc->cp_min_rank)
			{ pm = pm+1;continue;}

                list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);


                INIT_LIST_HEAD(&temp_peer->req_list);
                temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
                temp_peer->num_msg_ret = 0;


                pm = pm + 1;
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



static int dcrpc_announce_cp_all(struct rpc_server *rpc_s, struct rpc_cmd *cmd)     //Done
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        int err = -ENOMEM;
        dc->num_sp = hreg->num_sp;
        dc->num_cp = hreg->num_cp;

//        printf("Me is %d about to get peers from client master %d, #server %d # of clients # %d of my peer size %d\n", rpc_s->ptlmap.id,hreg->pm_cp.id, hreg->num_sp , hreg->num_cp , dc->peer_size);
//      peer = rpc_s->peer_tab;
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
        msg->cb = announce_cp_completion_all;       //

        msg->id = cmd->wr_id;
        msg->mr = cmd->mr;

	struct node_id *temp_peer;

//        err = rpc_receive_direct(rpc_s, peer, msg);
        list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//               printf("BEEPA I am now %d peer# %d (%s:%d)\n",dc->rpc_s->ptlmap.id, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
}

	rpc_s->ptlmap.id = rpc_s->ptlmap.id + hreg->pm_cp.id;

        peer->ptlmap.id = hreg->pm_cp.id;

	rpc_s->num_peers = peer->ptlmap.id+1;

//	rpc_s->num_peers=60;



        err = rpc_receive_direct(rpc_s, peer, msg);

        list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//               printf("BEEPB I am now %d peer# %d (%s:%d)\n",dc->rpc_s->ptlmap.id, temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
}	
        dc->cp_min_rank = dc->rpc_s->app_minid =  peer->ptlmap.id;

        if(err < 0) {
                free(msg);
                goto err_out;
        }  
        return 0;
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}



static int dcrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	dc->num_sp = hreg->num_sp;
	dc->num_cp = hreg->num_cp;

//	printf("about to get peers from master, #server %d # of clients # %d of my peer size %d\n", hreg->num_sp , hreg->num_cp , dc->peer_size);
//	peer = rpc_s->peer_tab;
	peer = dc_get_peer(dc, 0);

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (hreg->num_cp + hreg->num_sp - dc->peer_size - 1 );
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

			int kk=0;	
	
			while(peer==NULL){
				if(kk<4)
					printf("STUCK %d, waitting for %d %d\n",dc->rpc_s->ptlmap.id,conpara.pm_cp.id, dc->rpc_s->num_peers);
				kk++;
				rpc_process_event_with_timeout(dc->rpc_s, 1);
				//sleep(1);
				peer = dc_get_peer(dc, conpara.pm_cp.id);
			}
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


static void *dc_master_listen(void *server)
{
        struct rdma_cm_event *event = NULL;
        int connect_count = 0, err;
        struct rdma_conn_param cm_params;
        struct hdr_register hdr;
        struct connection *conn;
        struct dart_client *dc = (struct dart_client *) server;

	struct node_id *peer = NULL;
        while(dc->rpc_s->thread_alive && (rdma_get_cm_event(dc->rpc_s->rpc_ec, &event) == 0)) {
                struct con_param conpara;
                struct rdma_cm_event event_copy;
                memcpy(&event_copy, event, sizeof(*event));
                rdma_ack_cm_event(event);
                if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {

//                        printf("received connection request.\n");//debug
                        conpara = *(struct con_param *) event_copy.param.conn.private_data;
                        if(conpara.type == 0) {
                                peer = dc_get_peer(dc, conpara.pm_cp.id);
                                conn = &peer->sys_conn;
                        }

                        else {  
                                if(conpara.pm_cp.appid == dc->rpc_s->ptlmap.appid) {
                                        //peer = ds_get_peer(ds, sp_rank_cnt);
                                        //peer->ptlmap = conpara.pm_cp;
                                        //peer->ptlmap.id = sp_rank_cnt;
                                        struct node_id *temp_peer = peer_alloc();
                                        list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
                                        temp_peer->ptlmap = conpara.pm_cp;
                                        temp_peer->ptlmap.id = sp_rank_cnt;

                                        INIT_LIST_HEAD(&temp_peer->req_list);
                                        temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
                                        temp_peer->num_msg_ret = 0;


                                        peer = temp_peer;

                                        sp_rank_cnt++;
                                }

                                else {  
                                        hdr.pm_cp = conpara.pm_cp;
                                        hdr.pm_sp = conpara.pm_sp;
                                        hdr.num_cp = conpara.num_cp;
//                                        dsrpc_cn_register(dc->rpc_s, &hdr);

//                                      struct node_id *temp_peer = peer_alloc();
//                                      list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);

                                       peer = dc_get_peer(dc, hdr.pm_cp.id);
                                        conpara.pm_cp.id = peer->ptlmap.id;


                                }
                                conn = &peer->rpc_conn;
                        }
                        build_context(event_copy.id->verbs, conn);
                        build_qp_attr(&conn->qp_attr, conn, dc->rpc_s);
                        err = rdma_create_qp(event_copy.id, conn->pd, &conn->qp_attr);
                        if(err != 0) {
                                printf("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, dc->rpc_s->num_qp, err, __func__, strerror(errno));
                                goto err_out;
                        }
                        dc->rpc_s->num_qp++;
                        event_copy.id->context = conn;  //diff
                        conn->id = event_copy.id;       //diff
                        conn->qp = event_copy.id->qp;

                        if(conpara.type == 0) {
                                err = sys_post_recv(dc->rpc_s, peer);
                                if(err != 0)
                                        goto err_out;
                        }

                        else {
			err = rpc_post_recv(dc->rpc_s, peer);
                                if(err != 0)
                                        goto err_out;
                        }
                        memset(&cm_params, 0, sizeof(struct rdma_conn_param));
			
                        if(conpara.pm_cp.appid == dc->rpc_s->ptlmap.appid && conpara.type == 1) {
			//if(0){
                                memset(&conpara, 0, sizeof(struct con_param));
                                conpara.pm_sp = peer->ptlmap;
                                conpara.pm_cp = dc->rpc_s->ptlmap;
                                conpara.num_cp = dc->num_sp + dc->num_cp;
                                conpara.type = hdr.id_min;

                                //printf("id min is %d.\n", conpara.type); 
                                cm_params.private_data = &conpara;
                                cm_params.private_data_len = sizeof(conpara);
                        }

                        else {
                                cm_params.private_data = &peer->ptlmap.id;
                                cm_params.private_data_len = sizeof(int);
                        } cm_params.initiator_depth = cm_params.responder_resources = 1;
                        cm_params.retry_count = 7;      //diff
                        cm_params.rnr_retry_count = 7;  //infinite retry
                        err = rdma_accept(event_copy.id, &cm_params);
                        if(err != 0) {
                                printf("rdma_accept %d in %s.\n", err, __func__);
                                goto err_out;
                        }
                        connect_count++;
                        conn->f_connected = 1;
                }

                else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
//                       printf("Connection is established. %d\n",dc->connected);//DEBUG
                       dc->connected++;
                }

                else {
//                        rpc_print_connection_err(dc->rpc_s, peer, event_copy);
//                        printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
                        err = event_copy.status;
                //      goto err_out;
                }
//                if(connected == 2 * (dc->peer_size - 1) - dc->size_cp)
//                        break;
        }

        pthread_exit(0);
        return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
        pthread_exit(0);
        return 0;


}


static int dc_disseminate(struct dart_client *dc)
{
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct node_id *peer;//, *cpeer;
        struct ptlid_map *pptlmap;
        struct app_info *app;
        int i, k, err;

        peer = dc_get_peer(dc, 0);
        err = -ENOMEM;
        msg = msg_buf_alloc(dc->rpc_s, peer, 1);
        if(!msg)
		goto err_out;
                
	msg->cb = default_completion_with_data_callback;
                msg->size = sizeof(struct ptlid_map) * (dc->peer_size);
                pptlmap = msg->msg_data = malloc(msg->size);
                if(!msg->msg_data)
                        goto err_out_free;
                //for(k = 0; k < dc->peer_size; k++){
                struct node_id *temp_peer;
                list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
			if(temp_peer->ptlmap.id!=0)
	                        *pptlmap++ = temp_peer->ptlmap;
                }

                //cpeer = dc->peer_tab;
                msg->msg_rpc->cmd = cp_disseminate_cs;
                msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
                hreg = (struct hdr_register *) msg->msg_rpc->pad;
                hreg->pm_cp = dc->self->ptlmap;
                hreg->num_cp = dc->peer_size;
                hreg->num_sp = dc->num_sp;
                err = rpc_send(dc->rpc_s, peer, msg);
                if(err < 0) {
                        free(msg->msg_data);
                        goto err_out_free;
                }
		printf("send %d peers list to %d \n",dc->peer_size,peer->ptlmap.id);
                err = rpc_process_event(dc->rpc_s);
                if(err != 0)
                        goto err_out_free;
        return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}

static int dc_disseminate_dc(struct dart_client *dc)       //Done
{
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct node_id *peer;//, *cpeer;
        struct ptlid_map *pptlmap;
        struct app_info *app;
        int i, k, err;
//      cpeer = ds->peer_tab;

//	printf("Current %d %d min rank %d, peer size %d\n",dc->num_sp, dc->num_cp, dc->cp_min_rank, dc->peer_size);
        //send to slave servers all the peers' info
        for(i = 1; i < dc->peer_size; i++) {
                peer = dc_get_peer(dc, i+ dc->cp_min_rank);
                err = -ENOMEM;
                msg = msg_buf_alloc(dc->rpc_s, peer, 1);
                if(!msg)
                        goto err_out;
                msg->cb = default_completion_with_data_callback;
                msg->size = sizeof(struct ptlid_map) * (dc->num_sp + dc->num_cp);
                pptlmap = msg->msg_data = malloc(msg->size);
                if(!msg->msg_data)
                        goto err_out_free;
                struct node_id *temp_peer;
		k = 0;
                list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
			//memcpy(&pptlmap[k],&temp_peer->ptlmap,sizeof(struct ptlid_map));
                        *pptlmap++ = temp_peer->ptlmap;
                }


                msg->msg_rpc->cmd = cp_announce_cp;
                msg->msg_rpc->id = dc->self->ptlmap.id;
                hreg = (struct hdr_register *) msg->msg_rpc->pad;
                hreg->pm_cp = dc->self->ptlmap;
                hreg->num_cp = dc->num_cp;
                hreg->num_sp = dc->num_sp;

//                printf("sending to peer %d\n",peer->ptlmap.id);

                err = rpc_send(dc->rpc_s, peer, msg);
                if(err < 0) {
                        free(msg->msg_data);
                        goto err_out_free;
                }
                err = rpc_process_event(dc->rpc_s);
                if(err != 0)
                        goto err_out_free;
        }
        return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}


int dc_boot_master(struct dart_client *dc)
{
	struct rdma_cm_event *event = NULL;
        int connect_count = 0, err;
        int connected = 0;
        struct rdma_conn_param cm_params;
        struct hdr_register hdr;
        struct node_id *peer;
        struct connection *conn;



       int rc;
        dc->rpc_s->thread_alive = 1;
        rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_master_listen, (void *) dc);
        if(rc) {
                printf("ERROR; return code from pthread_create() is %d\n", rc);
                exit(-1);
        }

	while(dc->connected != dc->peer_size - 1){
                sleep(1);
	}

	dc->rpc_s->ptlmap.id = 0;

        printf("'%s()': all the peer in my app are registered. %d\n", __func__, dc->peer_size);
        dc->rpc_s->cur_num_peer = dc->peer_size;//dc->rpc_s->num_rpc_per_buff;

	//err = ds_disseminate(dc);
	

         struct node_id *temp_peer;
/*         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
                printf("Clientpeer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
         }
*/

	struct node_id *master_peer = peer_alloc();

	master_peer->ptlmap.id = 0;

        INIT_LIST_HEAD(&master_peer->req_list);
        master_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
        master_peer->num_msg_ret = 0;
	err = rpc_read_config(0,&master_peer->ptlmap.address);
        if(err < 0)
                goto err_out;
        //Connect to master server, build rpc channel and sys channel;
        err = rpc_connect(dc->rpc_s, master_peer);
	if(err != 0) {
        	printf("rpc_connect err %d in %s.\n", err, __func__);
                goto err_out;
	}
       
	dc->cp_min_rank = dc->rpc_s->app_minid; 

         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
		temp_peer->ptlmap.id = temp_peer->ptlmap.id + dc->cp_min_rank;
//                printf("Clientpeer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
         }




        list_add(&master_peer->peer_entry, &dc->rpc_s->peer_list);

//         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//                printf("Clientpeer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
//         }


	dc_disseminate(dc);

	while(dc->f_reg==0){
		err = rpc_process_event_with_timeout(dc->rpc_s, 1);
	}	

	//

//	printf("here is the list of my peers\n");
//         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//                printf("Clientpeer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
//         }

	//send peers info in my app to master server


	dc_disseminate_dc(dc);


        if(err != 0)
                goto err_out;
        dc->f_reg = 1;

        return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
        return err;
}




int dc_boot_slave(struct dart_client *dc)       //Done
{
//      struct node_id *peer = ds_get_peer(ds, 0);
        struct rdma_conn_param cm_params;
        struct con_param conpara;
        struct connection *con;
        struct rdma_cm_event *event = NULL;
        int i, err, check, connected, connect_count = 0;
        check = 0;
        connected = 0;


        struct node_id *peer = peer_alloc();
        INIT_LIST_HEAD(&peer->req_list);
        peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
        peer->num_msg_ret = 0;
        err = rpc_read_config(dc->rpc_s->ptlmap.appid,&peer->ptlmap.address);
        peer->ptlmap.id = 0;

        list_add(&peer->peer_entry, &dc->rpc_s->peer_list);


	struct node_id *temp_peer;
/*         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
                printf("BEGIN peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
}*/
        if(err < 0)
                goto err_out;
        if(peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr && peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port) {

                // This is the master server! the config file may be
                // around from a previous run 
                dc->self = peer;
                dc->self->ptlmap = peer->ptlmap;
                printf("'%s()': WARNING! config file exists, but I am the master server\n", __func__);
                err = dc_boot_master(dc);
                if(err < 0)
                        goto err_out;
                return 0;
        }

        //Connect to master server, build rpc channel and sys channel;
	err = rpc_connect(dc->rpc_s, peer);
        if(err != 0) {
                printf("rpc_connect err %d in %s.\n", err, __func__);
                goto err_out;
        }

//        list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//             printf("BEGIN peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
//}
//	printf("BEGIN I am peer# %d (%s:%d)\n",dc->rpc_s->ptlmap.id, inet_ntoa(dc->rpc_s->ptlmap.address.sin_addr),ntohs(dc->rpc_s->ptlmap.address.sin_port));
/*
        err = sys_connect(dc->rpc_s, peer);
        if(err != 0) {
                printf("sys_connect err %d in %s.\n", err, __func__);
                goto err_out;
        }
*/
//      dc->rpc_s->peer_tab[dc->rpc_s->ptlmap.id].ptlmap = dc->rpc_s->ptlmap;
//      dc->rpc_s->peer_tab[1].ptlmap = dc->rpc_s->ptlmap;      //diff


        int rc;
        dc->rpc_s->thread_alive = 1;

        rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_listen, (void *) dc);
        if(rc) {
                printf("ERROR; return code from pthread_create() is %d\n", rc);
                exit(-1);
        }



        //Waiting for dissemination msg from master server;
        while(dc->f_reg == 0) {
         //list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//                printf("peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
//         }
//	sleep(2);

		
                err = rpc_process_event_with_timeout(dc->rpc_s, 1);
                if(err != 0 && err != -ETIME)
                        goto err_out;
        }


        //list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
//               printf("FINISH I am now peer# %d (%s:%d)\n",dc->rpc_s->ptlmap.id, inet_ntoa(dc->rpc_s->ptlmap.address.sin_addr),ntohs(dc->rpc_s->ptlmap.address.sin_port));
//}


/*
        struct node_id *temp_peer = peer_alloc();

         list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
                printf("peer# %d (%s:%d)\n",temp_peer->ptlmap.id, inet_ntoa(temp_peer->ptlmap.address.sin_addr),ntohs(temp_peer->ptlmap.address.sin_port));
         }



        printf("***********************************\n");

*/

        return 0;
      err_out:
        printf("'%s()': failed with %d.\n", __func__, err);
        return err;
}




static int dc_boot(struct dart_client *dc, int appid)
{
	struct stat st_buff;
        char fil_lock[10];
	sprintf(fil_lock, "srv.lock.%d" ,appid);
        char fil_conf[10];
        sprintf(fil_conf,"conf.%d",appid);

        int fd, err;
        memset(&st_buff, 0, sizeof(st_buff));
        err = fd = open(fil_lock, O_WRONLY | O_CREAT, 0644);
        if(err < 0) 
                goto err_out;

        /* File locking does not work on the login nodes :-( *///?test
        err = file_lock(fd, 1);
        if(err < 0)
                goto err_fd;
        err = stat(fil_conf, &st_buff);
        if(err < 0 && errno != ENOENT) 
                goto err_flock;
        if(st_buff.st_size == 0 || dc->peer_size == 1) {
                err = rpc_write_config(appid,dc->rpc_s);
                if(err != 0)
                        goto err_flock;
                file_lock(fd, 0);
                struct node_id *temp_peer = peer_alloc();

                temp_peer->ptlmap = dc->rpc_s->ptlmap;

                dc->self = temp_peer;

                list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
                INIT_LIST_HEAD(&temp_peer->req_list);
                temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
                temp_peer->num_msg_ret = 0;


                err = dc_boot_master(dc);
                if(err != 0)
                        goto err_flock;
	}
	else{
                file_lock(fd, 0);
                err = dc_boot_slave(dc);
                if(err != 0)
                        goto err_flock;
        }
        //dc->rpc_s->cur_num_peer = dc->rpc_s->num_rpc_per_buff;
        close(fd);
        remove(fil_lock);

        dc->rpc_s->cur_num_peer = dc->num_sp + dc->num_cp;

        dc->rpc_s->num_sp = dc->num_sp;


int i;

 int n = log2_ceil(dc->peer_size);
        int *check_cp = malloc(sizeof(int) * (dc->peer_size + dc->cp_min_rank));

        int j;

        for(j = 0; j < dc->peer_size + dc->cp_min_rank; j++)
                check_cp[j] = 0;

        int *a = malloc(sizeof(int) * n);


        int k;
        int smaller_cid = 0;
        int greater_cid = 0;


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

	struct node_id *peer;
        for(i = 1; i < dc->self->ptlmap.id; i++) {
                if(i > dc->num_sp - 1 && i < dc->cp_min_rank)
                        continue;

                int count = 0;
                peer = dc_get_peer(dc, i);
          /*      if(i < dc->cp_min_rank) {
                        do {
                                err = rpc_connect(dc->rpc_s, peer);
                                count++;
                        } while(count < 3 && err != 0);
                        if(err != 0) {
                                printf("rpc_connect err %d in %s.\n", err, __func__);
                                goto err_out;
                        }

                }
	*/
                if(check_cp[peer->ptlmap.id] == 1) {
                        count = 0;
                        do {
//                                printf("SYS connect %d %d\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
                                err = sys_connect(dc->rpc_s, peer);
                                count++;
                        } while(count < 3 && err != 0);
                        if(err != 0) {
                                printf("sys_connect err %d in %s.\n", err, __func__);
                                goto err_out;
                        }
                }
        }


//sleep(10);


        return 0;
      err_flock:file_lock(fd, 0);
      err_fd:close(fd);
        remove(fil_lock);
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
        return err;
}

	





static int dc_boot2(struct dart_client *dc, int appid)	//Done
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

        err = rpc_read_config(0,&temp_peer->ptlmap.address);   ////
	temp_peer->ptlmap.id = 0;

        INIT_LIST_HEAD(&temp_peer->req_list);
        temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
        temp_peer->num_msg_ret = 0;



	err = rpc_read_config(0,&temp_peer->ptlmap.address);	////
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


	//sleep(5);

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
//                                printf("SYS connect %d %d\n",dc->rpc_s->ptlmap.id,peer->ptlmap.id);
				err = sys_connect(dc->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("sys_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}
		}
	}


//sleep(10);

/*

	while(dc->rpc_s->sys_conn_count< greater_cid){
	//	err = rpc_process_event_with_timeout(dc->rpc_s, 1);
        //        if(err != 0 && err != -ETIME)
        //                goto err_out;
       // 	printf("I am %d %d %d\n",dc->self->ptlmap.id, dc->rpc_s->sys_conn_count, greater_cid);
        
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

*/

	dc->rpc_s->cur_num_peer = dc->num_sp + dc->num_cp_all;

	dc->rpc_s->num_sp = dc->num_sp;

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

        rpc_add_service(cp_announce_cp, dcrpc_announce_cp_all);

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

	printf("dc_alloc succeed %d.\n", dc->rpc_s->ptlmap.id);
	err = dc_barrier(dc);
	if(err < 0) {
		free(dc);
		goto err_out;
	}


//	printf("%d barrier is ok.\n",dc->rpc_s->ptlmap.id);
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
