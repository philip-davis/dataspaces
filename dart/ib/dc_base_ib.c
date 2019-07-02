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
#include <sched.h>
#include <sys/stat.h>

#include "debug.h"
#include "dc_base_ib.h"
#include "mpi.h"

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

static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct ptlid_map *pm;
	int i;
	pm = (struct ptlid_map *) msg->msg_data;

	for(i = 0; i < dc->num_sp + dc->num_cp - dc->peer_size - 1; i++) {
        struct node_id *temp_peer = peer_alloc();

		temp_peer->ptlmap = *pm;
        list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
	    INIT_LIST_HEAD(&temp_peer->req_list);
	    temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
        temp_peer->num_msg_ret = 0;

		pm++;
	}

	dc->cp_min_rank = dc->rpc_s->app_minid;
	dc->f_reg = 1;
	
    return 0;
}

static int announce_cp_completion_all(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct node_id *peer;
        struct ptlid_map *pm;
        int i;
        pm = (struct ptlid_map *) msg->msg_data;

        for(i = 0; i < dc->num_sp + dc->num_cp; i++) {
            struct node_id *temp_peer = peer_alloc();

            temp_peer->ptlmap = *pm;
            if(temp_peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr 
                && temp_peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port) {
                dc->self = temp_peer;
			    dc->rpc_s->ptlmap.id = temp_peer->ptlmap.id;
	        }
		    if(temp_peer->ptlmap.id == dc->cp_min_rank) { 
                pm++;
                continue;
            }

            list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);

		    INIT_LIST_HEAD(&temp_peer->req_list);
	    	temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
		    temp_peer->num_msg_ret = 0;
		    pm++;
	    }
 
        dc->cp_min_rank = dc->rpc_s->app_minid;
        dc->f_reg = 1;
        free(msg->msg_data);
        free(msg);
        return 0;
}

static int dcrpc_announce_cp_all(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);
    struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
    struct node_id *peer;
    struct msg_buf *msg;
    int err = -ENOMEM;
        
    dc->num_sp = hreg->num_sp;
    dc->num_cp = hreg->num_cp;

    peer = dc_get_peer(dc, 0);

    msg = msg_buf_alloc(rpc_s, peer, 0);
    if(!msg)
        goto err_out;
    msg->size = sizeof(struct ptlid_map) * (hreg->num_cp + hreg->num_sp);
    msg->msg_data = calloc(1, msg->size);
    if(!msg->msg_data) {
        free(msg);
        goto err_out;
    }  
    msg->cb = announce_cp_completion_all;

    msg->id = cmd->wr_id;
    msg->mr = cmd->mr;

	struct node_id *temp_peer;

	rpc_s->ptlmap.id = rpc_s->ptlmap.id + hreg->pm_cp.id;
    peer->ptlmap.id = hreg->pm_cp.id;
	rpc_s->num_peers = peer->ptlmap.id+1;
    err = rpc_receive_direct(rpc_s, peer, msg);
	dc->cp_min_rank = dc->rpc_s->app_minid = peer->ptlmap.id;

    if(err < 0) {
        free(msg);
        goto err_out;
    }  
    return 0;
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}



static int dcrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	dc->num_sp = hreg->num_sp;
	dc->num_cp = hreg->num_cp;

	if(hreg->num_cp + hreg->num_sp - dc->peer_size - 1 == 0) {
		dc->cp_min_rank = dc->rpc_s->app_minid;
	        dc->f_reg = 1;
		return 0;
	}

	peer = dc_get_peer(dc, 0);

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (hreg->num_cp + hreg->num_sp - dc->peer_size - 1 );
	msg->msg_data = calloc(1, msg->size);
	if(!msg->msg_data) {
		free(msg);
		goto err_out;
	}
	msg->cb = announce_cp_completion;

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

static int dc_disseminate_app(struct dart_client *dc, struct node_id *peer, struct hdr_register *recv_hreg, void *pmap_array)
{

    struct msg_buf *msg;
    struct hdr_register *hreg;
    struct ptlid_map *pptlmap;
    int err;

    err = -ENOMEM;
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if(!msg) {
        goto err_out;
    }
    msg->cb = default_completion_with_data_callback;
    msg->size = sizeof(struct ptlid_map) * recv_hreg->num_cp;
    pptlmap = msg->msg_data = malloc(msg->size);
    if(!msg->msg_data) {
        goto err_out_free;
    }
    memcpy(pptlmap, pmap_array, msg->size);
    
    msg->msg_rpc->cmd = sp_announce_app;
    msg->msg_rpc->id = dc->self->ptlmap.id;
    hreg = (struct hdr_register *)msg->msg_rpc->pad;
    *hreg = *recv_hreg;

    err = rpc_send(dc->rpc_s, peer, msg);
    if(err < 0) {
        goto err_out_free;
    }
    err = rpc_process_event(dc->rpc_s);
    if(err != 0)
        goto err_out_free;

    return(0);

err_out_free:
    if(msg->msg_data) {
        free(msg->msg_data);
    }
    free(msg);
err_out:
    fprintf(stderr, "(%s): err (%d).\n", __func__, err);
    return(err);

}

static int install_app_cp(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct ptlid_map *pm = msg->msg_data;
	struct node_id *temp_peer;
	struct hdr_register *priv_hreg = (struct hdr_register *)msg->private;
	int i, err = 0;

	for(i = 0; i < priv_hreg->num_cp; i++) {
		temp_peer = peer_alloc();

		temp_peer->ptlmap = *pm;
		list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
		INIT_LIST_HEAD(&temp_peer->req_list);
		temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
		temp_peer->num_msg_ret = 0;

		dc->num_cp++;
		pm++;
	}

	if(dc->self->ptlmap.id == dc->cp_min_rank) {
		list_for_each_entry(temp_peer, &dc->rpc_s->peer_list,
				struct node_id, peer_entry) {
			if(temp_peer->ptlmap.appid == dc->self->ptlmap.appid &&
					temp_peer->ptlmap.id != dc->self->ptlmap.id) {
				dc_disseminate_app(dc, temp_peer, priv_hreg, msg->msg_data);
			}
		}
	}

	free(msg->private);
	free(msg->msg_data);
	free(msg);

	return(0);
}

static int announce_app_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);

    if(dc->f_reg_all || dc->self->ptlmap.id == dc->cp_min_rank) {
    	install_app_cp(rpc_s, msg);
    } else {
        struct msg_list *app_reg = malloc(sizeof(*app_reg));

        app_reg->msg = msg;
        list_add(&app_reg->list_entry, &dc->deferred_app_msg);
    }

    return(0);
 
} 

/* Receive info about a new group of nodes */
static int dcrpc_sp_announce_app(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);
    struct hdr_register *hreg = (struct hdr_register *) cmd->pad;

    struct node_id *peer;
    struct msg_buf *msg;
    int err = -ENOMEM;

    //wait until registration completes to procede.
    while(!dc->f_reg) {
        err = rpc_process_event_with_timeout(dc->rpc_s, 1);
        if(err != 0 && err != -ETIME)
        	goto err_out;
    }

    // master clients receive from master server, then disseminate to slave clients
    if(dc->self->ptlmap.id == dc->cp_min_rank) {
        peer = dc_get_peer(dc, 0);
    } else {
        peer = dc_get_peer(dc, dc->cp_min_rank);
    }
    
    msg = msg_buf_alloc(rpc_s, peer, 0);
    if(!msg) {
        goto err_out;
    }
    msg->size = sizeof(struct ptlid_map) * hreg->num_cp;
    msg->msg_data = calloc(1, msg->size);
    if(!msg->msg_data) {
        goto err_out_free;
    }
    msg->cb = announce_app_completion;

    msg->id = cmd->wr_id;
    msg->mr = cmd->mr;

    /* dart clients don't keep the same app structure as the server. We will need to remember 
     * the size of the app and the appid later, when we are actually adding these nodes to
     * the peer list / forwarding on to other peer clients. Stash this for a while.
     */
    msg->private = malloc(sizeof(*hreg));
    memcpy(msg->private, hreg, sizeof(*hreg));

    err = rpc_receive_direct(rpc_s, peer, msg);
    if(err < 0) {
        goto err_out_free;
    }

    return(0);

err_out_free:
    if(msg->private) {
        free(msg->private);
    }
    if(msg->msg_data) {
        free(msg->msg_data);
    }
    free(msg);
err_out:
    fprintf(stderr, "(%s): err (%d).\n", __func__, err);
    return(err);

}

//RPC routine  to wait for  server confirmation that it  processed our unregister message and all our other messages.
static int dcrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	dc->f_reg = 0;
	return 0;
}

static int dcrpc_unregister_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	dc->num_unreg++;
	struct msg_buf *msg;
	int err;

	struct node_id *peer = dc_get_peer(dc, cmd->id);

	msg = msg_buf_alloc(rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->msg_rpc->id = dc->self->ptlmap.id;
	msg->msg_rpc->cmd = cn_unregister;
	peer->f_unreg = 1;


	err = rpc_send(rpc_s, peer, msg);



	if(err < 0) {
		free(msg);
		goto err_out;
	}

	return 0;
      err_out:printf("(%s): failed. (%d)\n", __func__, err);
	return err;


}

static int dc_unregister_mpi(struct dart_client *dc)	//Done
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;

	MPI_Barrier(*(dc->comm));

	if(dc->rpc_s->ptlmap.id == dc->cp_min_rank) {
		peer = dc_get_peer(dc, 0);
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
		while(dc->f_reg) {
			err = rpc_process_event(dc->rpc_s);
			if(err < 0)
				goto err_out;
		}
	}

	return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()': failed.\n", __func__);
	return err;
}

static int dc_unregister(struct dart_client *dc)	//Done
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;

	if(dc->rpc_s->ptlmap.id == dc->cp_min_rank) {
		dc->num_unreg++;
		while(dc->num_unreg != dc->peer_size) {
			err = rpc_process_event(dc->rpc_s);
			if(err < 0)
				goto err_out;
		}
		peer = dc_get_peer(dc, 0);
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
//              dc->f_reg = 0;
		while(dc->f_reg) {
			err = rpc_process_event(dc->rpc_s);
			if(err < 0)
				goto err_out;
		}
	
		return 0;
	}

	//need to sleep here, otherwise the master peer will get too many rpc call
	//It is a bug somewhere that will need to be corrected
	sleep((dc->rpc_s->ptlmap.id-dc->cp_min_rank)/100.0);

	peer = dc_get_peer(dc, dc->cp_min_rank);
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->msg_rpc->cmd = cn_unregister_cp;
	msg->msg_rpc->id = dc->self->ptlmap.id;
	hreg = (struct hdr_register *) msg->msg_rpc->pad;
	hreg->num_sp = dc->num_sp;
	hreg->num_cp = 1;
	hreg->pm_cp = dc->self->ptlmap;
	hreg->pm_sp = peer->ptlmap;
	err = rpc_send(dc->rpc_s, peer, msg);
	if(err < 0)
		goto err_out_free;

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



static int dc_unregister2(struct dart_client *dc)	//Done
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
	void *priv_data;

	while(dc->rpc_s->thread_alive && (rdma_get_cm_event(dc->rpc_s->rpc_ec, &event) == 0)) {

		struct rdma_cm_event event_copy;
		priv_data = NULL;
		if(event->param.conn.private_data) {
			priv_data = malloc(event->param.conn.private_data_len);
			memcpy(priv_data, event->param.conn.private_data, event->param.conn.private_data_len);
		}
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);
		if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
			conpara = *(struct con_param *) priv_data;
			peer = dc_get_peer(dc, conpara.pm_cp.id);

			int kk = 0;

			while(peer == NULL) {
				rpc_process_event_with_timeout(dc->rpc_s, 1);
				sleep(1);
				peer = dc_get_peer(dc, conpara.pm_cp.id);
			}
			if(conpara.type == 0) {
				con = &peer->sys_conn;
				if(con->f_connected == 1) {
                    build_context(event_copy.id->verbs, con);
                    build_qp_attr(&con->qp_attr, con, dc->rpc_s);
                    err = rdma_create_qp(event_copy.id, con->pd, &con->qp_attr);
                    if(err != 0) {
                        uloga("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, dc->rpc_s->num_qp, err, __func__, strerror(errno));
                        goto err_out;
                    }
                    dc->rpc_s->num_qp++;
                    event_copy.id->context = con;  //diff
                    con->id = event_copy.id;       //diff
                    con->qp = event_copy.id->qp;

                    err = sys_post_recv(dc->rpc_s, peer);
                    if(err != 0)
                        goto err_out;

                    memset(&cm_params, 0, sizeof(struct rdma_conn_param));
                    cm_params.private_data = &peer->ptlmap.id;
                    cm_params.private_data_len = sizeof(int);
                    cm_params.initiator_depth = cm_params.responder_resources = 1;
                    cm_params.retry_count = 7;
                    cm_params.rnr_retry_count = 7;  //infinite retry
                    err = rdma_accept(event_copy.id, &cm_params);
                    if(err != 0) {
                        uloga("rdma_accept %d in %s.\n", err, __func__);
                        goto err_out;
                    }

			        dc->s_connected++;

			        continue;
				}
			} else {
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
			dc->s_connected++;
			connect_count++;
		} else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			dc->connected++;
			connected++;
		} else if(event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
		    //placeholder
        } else {
			peer->sys_conn.f_connected = 0;
			dc->s_connected--;
			err = event_copy.status;
		}
		if(priv_data) {
			free(priv_data);
		}

	}

	pthread_exit(0);
	return 0;
err_out:
	if(priv_data) {
		free(priv_data);
	}
	printf("'%s()': failed with %d.\n", __func__, err);
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
	void *priv_data;

    while(dc->rpc_s->thread_alive && (rdma_get_cm_event(dc->rpc_s->rpc_ec, &event) == 0)) {
        struct con_param conpara;
        struct rdma_cm_event event_copy;
        
        priv_data = NULL;
        if(event->param.conn.private_data) {
        	priv_data = malloc(event->param.conn.private_data_len);
        	memcpy(priv_data, event->param.conn.private_data, event->param.conn.private_data_len);
        }
        memcpy(&event_copy, event, sizeof(*event));
        rdma_ack_cm_event(event);
        if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
			conpara = *(struct con_param *) priv_data;
			if(conpara.type == 0) {
				peer = dc_get_peer(dc, conpara.pm_cp.id);
				conn = &peer->sys_conn;
                if(conn->f_connected == 1) {
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

                    err = sys_post_recv(dc->rpc_s, peer);
                    if(err != 0)
                        goto err_out;

			        memset(&cm_params, 0, sizeof(struct rdma_conn_param));
                    cm_params.private_data = &peer->ptlmap.id;
                    cm_params.private_data_len = sizeof(int);
                    cm_params.initiator_depth = cm_params.responder_resources = 1;
                    cm_params.retry_count = 7;
                    cm_params.rnr_retry_count = 7;  //infinite retry
                    err = rdma_accept(event_copy.id, &cm_params);
                    if(err != 0) {
                        printf("rdma_accept %d in %s.\n", err, __func__);
                        goto err_out;
                    }
                    dc->s_connected++;

			        continue;
                }
			} else {
				if(conpara.pm_cp.appid == dc->rpc_s->ptlmap.appid) {
					struct node_id *temp_peer = peer_alloc();
					list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
					temp_peer->ptlmap = conpara.pm_cp;
					temp_peer->ptlmap.id = sp_rank_cnt;

					INIT_LIST_HEAD(&temp_peer->req_list);
					temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
					temp_peer->num_msg_ret = 0;

					peer = temp_peer;
					peer->sys_conn.f_connected = 0;
                    peer->rpc_conn.f_connected = 0;

					sp_rank_cnt++;
				} else {
					hdr.pm_cp = conpara.pm_cp;
					hdr.pm_sp = conpara.pm_sp;
					hdr.num_cp = conpara.num_cp;
					peer = dc_get_peer(dc, hdr.pm_cp.id);
                    while(peer == NULL) {
                        rpc_process_event_with_timeout(dc->rpc_s, 1);
                        sleep(1);
                        peer = dc_get_peer(dc, conpara.pm_cp.id);
                    }
					conpara.pm_cp.id = peer->ptlmap.id;
                    printf("%s, got connect from id = %d\n", __func__, peer->ptlmap.id);
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
			event_copy.id->context = conn;	//diff
			conn->id = event_copy.id;	//diff
			conn->qp = event_copy.id->qp;

			if(conpara.type == 0) {
				err = sys_post_recv(dc->rpc_s, peer);
				if(err != 0)
					goto err_out;
			} else {
				err = rpc_post_recv(dc->rpc_s, peer);
				if(err != 0)
					goto err_out;
			}
			memset(&cm_params, 0, sizeof(struct rdma_conn_param));

			if(conpara.pm_cp.appid == dc->rpc_s->ptlmap.appid && conpara.type == 1) {
				memset(&conpara, 0, sizeof(struct con_param));
				conpara.pm_sp = peer->ptlmap;
				conpara.pm_cp = dc->rpc_s->ptlmap;
				conpara.num_cp = dc->num_sp + dc->num_cp;
				conpara.type = hdr.id_min;

				cm_params.private_data = &conpara;
				cm_params.private_data_len = sizeof(conpara);
			} else {
				cm_params.private_data = &peer->ptlmap.id;
				cm_params.private_data_len = sizeof(int);
			}
			cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;	//diff
			cm_params.rnr_retry_count = 7;	//infinite retry
			err = rdma_accept(event_copy.id, &cm_params);
			if(err != 0) {
				printf("rdma_accept %d in %s.\n", err, __func__);
				goto err_out;
			}
			connect_count++;
			conn->f_connected = 1;
			dc->s_connected++;
		} else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			dc->connected++;
		} else if(event_copy.event != RDMA_CM_EVENT_DISCONNECTED) {
            //placeholder
        } else {
			peer->sys_conn.f_connected = 0;
			dc->s_connected--;
            err = event_copy.status;
        }
        if(priv_data) {
        	free(priv_data);
        }
    }
    pthread_exit(0);
    return 0;
err_out:
	if(priv_data) {
		free(priv_data);
	}
    printf("'%s()': failed with %d.\n", __func__, err);
    pthread_exit(0);
    return 0;
}


static int dc_disseminate(struct dart_client *dc)
{
    struct msg_buf *msg;
    struct hdr_register *hreg;
    struct node_id *peer;
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
	struct node_id *temp_peer;
	list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
		if(temp_peer->ptlmap.id != 0) {
			*pptlmap++ = temp_peer->ptlmap;
        }
	}
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
	err = rpc_process_event(dc->rpc_s);
	if(err != 0)
		goto err_out_free;
	return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}

static int dc_disseminate_dc_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);

    if(msg->msg_data)
        free(msg->msg_data);
    free(msg);


    if(++dc->num_posted == (dc->peer_size - 1)) {
        dc->f_reg_all = 1;
    }

    return 0;
}

static int gather_peer_list(struct dart_client *dc){
	int rank, gsize;
	struct ptlid_map *pptlmap;
	struct ptlid_map local_map = dc->rpc_s->ptlmap;
	MPI_Comm_rank(*(dc->comm), &rank);
	 if(rank==0){
		 MPI_Comm_size(*(dc->comm), &gsize);
		 pptlmap = malloc(sizeof(struct ptlid_map) * gsize);
	 }
	MPI_Gather(&local_map, sizeof(struct ptlid_map), MPI_CHAR, pptlmap, sizeof(struct ptlid_map), MPI_CHAR, 0, *(dc->comm));
	if(rank==0){
		pptlmap++;
		for(int i=1; i<gsize; i++){
			struct node_id *temp_peer = peer_alloc();
			list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
			temp_peer->ptlmap = *pptlmap;
			temp_peer->ptlmap.id = i;
			INIT_LIST_HEAD(&temp_peer->req_list);
			temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
			temp_peer->num_msg_ret = 0;

			pptlmap++;
		}

	}

	return 0;
}

static int dc_disseminate_dc_mpi(struct dart_client *dc)
{

	struct msg_buf *msg;
    struct hdr_register *hreg;
    struct node_id *peer;
    struct ptlid_map *pptlmap;
    struct ptlid_map *new_pptlmap;
    struct app_info *app;
    int i, k, err;
    int rank;

    int bcast_array[2]={0,0};
    MPI_Comm_rank(*(dc->comm), &rank);
    if (rank==0){
    	bcast_array[0] = dc->num_cp;
    	bcast_array[1] = dc->num_sp;
    }
    MPI_Bcast(bcast_array, 2, MPI_INT, 0, *(dc->comm));
    //printf("NUM CP: %d, NUM SP :%d\n", bcast_array[0], bcast_array[1]);
    pptlmap = malloc(sizeof(struct ptlid_map) * (bcast_array[0] + bcast_array[1]));
    new_pptlmap = pptlmap;

    if(rank==0){
		struct node_id *temp_peer;
		list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
			*pptlmap++ = temp_peer->ptlmap;
		}
		pptlmap = new_pptlmap;
    }

    MPI_Bcast(pptlmap, sizeof(struct ptlid_map) * (bcast_array[0] + bcast_array[1]), MPI_CHAR, 0, *(dc->comm));
    if(rank!=0){
    	dc->num_cp = bcast_array[0];
    	dc->num_sp = bcast_array[1];
      	for(i = 0; i < bcast_array[0] + bcast_array[1]; i++) {
			struct node_id *temp_peer = peer_alloc();

			temp_peer->ptlmap = *pptlmap;
			if(temp_peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr
				&& temp_peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port) {
				dc->self = temp_peer;
				dc->rpc_s->ptlmap.id = temp_peer->ptlmap.id;
			}

			list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);

			INIT_LIST_HEAD(&temp_peer->req_list);
			temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
			temp_peer->num_msg_ret = 0;

			pptlmap++;
		}
      	dc->cp_min_rank = dc->rpc_s->app_minid;
      	dc->f_reg = 1;

    }
    dc->f_reg_all=1;
    return 0;
}


static int dc_disseminate_dc(struct dart_client *dc) 
{
    struct msg_buf *msg;
    struct hdr_register *hreg;
    struct node_id *peer;
    struct ptlid_map *pptlmap;
    struct app_info *app;
    int i, k, err;

	for(i = 1; i < dc->peer_size; i++) {
		peer = dc_get_peer(dc, i + dc->cp_min_rank);
		if(!peer)
			continue;
		err = -ENOMEM;
		msg = msg_buf_alloc(dc->rpc_s, peer, 1);
		if(!msg)
			goto err_out;
		msg->cb = dc_disseminate_dc_completion;
		msg->size = sizeof(struct ptlid_map) * (dc->num_sp + dc->num_cp);
		pptlmap = msg->msg_data = malloc(msg->size);
		if(!msg->msg_data)
			goto err_out_free;
		
        struct node_id *temp_peer;
		k = 0;
        list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
            *pptlmap++ = temp_peer->ptlmap;
        }

        msg->msg_rpc->cmd = cp_announce_cp;
        msg->msg_rpc->id = dc->self->ptlmap.id;
        hreg = (struct hdr_register *) msg->msg_rpc->pad;
        hreg->pm_cp = dc->self->ptlmap;
        hreg->num_cp = dc->num_cp;
        hreg->num_sp = dc->num_sp;

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

err_out_free:
    free(msg);
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
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
    int dimes_flag = 0;
        
    dc->rpc_s->thread_alive = 1;
    rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_master_listen, (void *) dc);
    if(rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
        exit(-1);
    }

#ifndef DS_HAVE_DIMES
	if(dc->comm != NULL){
		gather_peer_list(dc);
		dimes_flag=1;
	}
#endif
	if(dimes_flag==0){
		while(dc->connected != dc->peer_size - 1) {
			sched_yield();
		}
	}
	dc->rpc_s->ptlmap.id = 0;

    INIT_LIST_HEAD(&dc->deferred_app_msg);

	struct node_id *master_peer = peer_alloc();

	master_peer->ptlmap.id = 0;

	INIT_LIST_HEAD(&master_peer->req_list);
	master_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
	master_peer->num_msg_ret = 0;
	err = rpc_read_config(0, &master_peer->ptlmap.address);
	if(err < 0)
		goto err_out;

	//Connect to master server, build rpc channel and sys channel;
	do{
		err = rpc_connect(dc->rpc_s, master_peer);
	}while(err != 0);
	dc->cp_min_rank = dc->rpc_s->app_minid;

    struct node_id *temp_peer;
	list_for_each_entry(temp_peer, &dc->rpc_s->peer_list, struct node_id, peer_entry) {
        temp_peer->ptlmap.id = temp_peer->ptlmap.id + dc->cp_min_rank;
	}
       
	dc->cp_min_rank = dc->rpc_s->app_minid; 

	list_add(&master_peer->peer_entry, &dc->rpc_s->peer_list);

	dc_disseminate(dc);

	while(dc->f_reg == 0){
		err = rpc_process_event_with_timeout(dc->rpc_s, 1);
	}	
	

    //send peers info in my app to master server
#ifndef DS_HAVE_DIMES
	if(dc->comm != NULL){
		dc_disseminate_dc_mpi(dc);
		dimes_flag=1;
	}
#endif

	if(dimes_flag==0){
		dc_disseminate_dc(dc);
		while(dc->f_reg_all == 0){
				err = rpc_process_event_with_timeout(dc->rpc_s, 1);
			}
	}

    if(!list_empty(&dc->deferred_app_msg)) {
        struct msg_buf *msg;
        struct msg_list *deferred, *it;
        struct node_id *temp_peer;        

        list_for_each_entry_safe(deferred, it, &dc->deferred_app_msg, struct msg_list, list_entry) {
            msg = deferred->msg;
            install_app_cp(dc->rpc_s, msg);
            list_del(&deferred->list_entry);
            free(deferred);
        }
    }


    if(err != 0)
        goto err_out;

    return 0;
err_out:
    printf("'%s()': failed with %d.\n", __func__, err);
    return err;
}


int dc_boot_slave_mpi(struct dart_client *dc, struct node_id *peer)
{
    struct rdma_conn_param cm_params;
    struct con_param conpara;
    struct connection *con;
    struct rdma_cm_event *event = NULL;
    int i, err;

    list_add(&peer->peer_entry, &dc->rpc_s->peer_list);

	gather_peer_list(dc);

    int rc;
    dc->rpc_s->thread_alive = 1;


    INIT_LIST_HEAD(&dc->deferred_app_msg);

    rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_listen, (void *) dc);
    if(rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
        exit(-1);
    }
    dc_disseminate_dc_mpi(dc);


    dc->f_reg_all = 1;
    return 0;
      
}


int dc_boot_slave(struct dart_client *dc)
{
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
	do{
		err = rpc_read_config(dc->rpc_s->ptlmap.appid, &peer->ptlmap.address);
	} while(err!=0);
	peer->ptlmap.id = 0;

    list_add(&peer->peer_entry, &dc->rpc_s->peer_list);

	struct node_id *temp_peer;
	if(err < 0)
		goto err_out;
	if(peer->ptlmap.address.sin_addr.s_addr == dc->rpc_s->ptlmap.address.sin_addr.s_addr
        && peer->ptlmap.address.sin_port == dc->rpc_s->ptlmap.address.sin_port) {

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
	do{
		err = rpc_connect(dc->rpc_s, peer);
	}while(err != 0);

    int rc;
    dc->rpc_s->thread_alive = 1;

    INIT_LIST_HEAD(&dc->deferred_app_msg);

    rc = pthread_create(&(dc->rpc_s->comm_thread), NULL, dc_listen, (void *) dc);
    if(rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
        exit(-1);
    }

    //Waiting for dissemination msg from master server;
    while(dc->f_reg == 0) {
        err = rpc_process_event_with_timeout(dc->rpc_s, 1);
        if(err != 0 && err != -ETIME)
            goto err_out;
    }

    dc->f_reg_all = 1;

    return 0;

err_out:
    printf("'%s()': failed with %d.\n", __func__, err);
    return err;
}

static int boot_client_processes(struct dart_client *dc, int appid, int rank)
{
	int err;
	char *buffer;
	int portid;
	char nodeid[16];
	int buff_len = sizeof(int) + 16; //uint16_t for port number, 16 characters for node id
	buffer = (char*) malloc(buff_len);
	if(rank==0){
		portid = ntohs(dc->rpc_s->ptlmap.address.sin_port);
		memcpy( &buffer[0], &portid, sizeof(int));
		sprintf(nodeid, "%s", inet_ntoa(dc->rpc_s->ptlmap.address.sin_addr));
		memcpy(&buffer[sizeof(int)], &nodeid[0], 16);

	}
	//printf("Peer %d, before bcast\n", rank);
	MPI_Bcast(buffer, buff_len, MPI_CHAR, 0, *(dc->comm));
	//printf("Peer %d, After bcast\n", rank);
	if(rank!=0){
		memcpy(&portid,  &buffer[0], sizeof(int));
		memcpy(&nodeid[0], &buffer[sizeof(int)], 16);

	}
	if(rank==0){
		struct node_id *temp_peer = peer_alloc();
		temp_peer->ptlmap = dc->rpc_s->ptlmap;
		dc->self = temp_peer;
		list_add(&temp_peer->peer_entry, &dc->rpc_s->peer_list);
		INIT_LIST_HEAD(&temp_peer->req_list);
		temp_peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
		temp_peer->num_msg_ret = 0;
		err = dc_boot_master(dc);
	}else{
		struct node_id *peer = peer_alloc();
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
		peer->num_msg_ret = 0;
		peer->ptlmap.address.sin_addr.s_addr= inet_addr(nodeid);
		peer->ptlmap.address.sin_port = htons(portid);
		peer->ptlmap.id = 0;
		err= dc_boot_slave_mpi(dc, peer);

	}
	return err;

}

static int dc_boot(struct dart_client *dc, int appid)
{
	struct stat st_buff;
	char fil_lock[20];
	sprintf(fil_lock, "srv.lock.%d", appid);
	char fil_conf[20];
	sprintf(fil_conf, "conf.%d", appid);
	int fd, err;
	int rank;
	int dimes_flag = 0;

#ifndef DS_HAVE_DIMES
	if(dc->comm != NULL){
			MPI_Comm_rank(*(dc->comm), &rank);
			boot_client_processes(dc, appid, rank);
			dimes_flag=1;
	}
#endif
	if(dimes_flag==0){
		 MPI_Comm_rank(MPI_COMM_WORLD, &rank);
		 if(rank == 0 || dc->peer_size == 1) {
			err = rpc_write_config(appid, dc->rpc_s);
			if(err != 0)
				goto err_flock;
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
		 } else {
			sleep(1);
			 err = dc_boot_slave(dc);
		 }

	}

    if(dc->comm != NULL){
    	MPI_Barrier(*(dc->comm));
	}else{
		MPI_Barrier(MPI_COMM_WORLD);
	}


    dc->rpc_s->num_sp = dc->num_sp;

    return 0;

err_flock:
    file_lock(fd, 0);
err_fd:
    close(fd);
    remove(fil_lock);
err_out:
    printf("'%s()': failed with %d.\n", __func__, err);
    return err;
}

/*
  Public API starts here.
*/

int dc_barrier(struct dart_client *dc)
{
	if(dc->comm != NULL)
		MPI_Barrier(*(dc->comm));
	else
		rpc_barrier(dc->rpc_s);
}


struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm)
{
	struct dart_client *dc;

	int err;
	dc = calloc(1, sizeof(*dc));
	if(!dc)
		return NULL;
	dc->dart_ref = dart_ref;
	dc->cp_in_job = num_peers;
	dc->connected = 0;
	dc->peer_size = num_peers;

	rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);
    rpc_add_service(cn_unregister_cp, dcrpc_unregister_cp);
	rpc_add_service(sp_announce_cp, dcrpc_announce_cp);
    rpc_add_service(cp_announce_cp, dcrpc_announce_cp_all);
    rpc_add_service(sp_announce_app, dcrpc_sp_announce_app);

	if(comm != NULL) {
        dc->comm = malloc(sizeof(*dc->comm));
		err = MPI_Comm_dup(*(MPI_Comm *) comm, dc->comm);
		if(err < 0) {
			printf("MPI_Comm_dup failed\n");
			goto err_out;
		}
		//printf("MPI communicator is copied into dc->comm\n");
	}
	//printf("Init the server: rpc_server_init\n");
	dc->rpc_s = rpc_server_init(0, NULL, 0, 100, num_peers, dc, DART_CLIENT);
	//printf("After rpc init the server: rpc_server_init\n");
	if(!dc->rpc_s) {
		free(dc);
		return NULL;
	}

	dc->rpc_s->ptlmap.appid = appid;
	//printf("Before dc_boot\n");
	err = dc_boot(dc, appid);
	//printf("After dc_boot\n");
	if(err < 0) {
		rpc_server_free(dc->rpc_s);
		free(dc);
		goto err_out;
	}
	dc->rpc_s->app_num_peers = num_peers;

	dc->rpc_s->num_peers = dc->num_cp + dc->num_sp;
	dc->num_unreg = 0;
	err = dc_barrier(dc);
    if(err < 0) {
		free(dc);
		goto err_out;
	}
	return dc;

err_out:
    printf("'%s()': failed with %d.\n", __func__, err);
	return NULL;
}

void dc_free(struct dart_client *dc)
{
	int err;
	int dimes_flag = 0;
#ifndef DS_HAVE_DIMES
	if(dc->comm != NULL){
		dc_unregister_mpi(dc);
		dimes_flag = 1;
	}
#endif
	if(dimes_flag == 0){
		dc_unregister(dc);
	}
	dc_barrier(dc);
	err = rpc_server_free(dc->rpc_s);
    if(dc->comm) {
        free(dc->comm);
    }
	if(err != 0)
		printf("rpc_server_free err in %s.\n", __func__);
	free(dc);
}

int dc_process(struct dart_client *dc)
{
	return rpc_process_event(dc->rpc_s);
}

int on_same_node(struct node_id *local, struct node_id *remote){
    if(local->ptlmap.address.sin_addr.s_addr == remote->ptlmap.address.sin_addr.s_addr)
        return 1;
    return 0;
}
