/*
 *  Base implementation of DART server.
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
#include "ds_base_ib.h"

#define PEER_ID(peer)		peer->ptlmap.id
static int sp_rank_cnt = 1;
static int cp_rank_cnt = 0;
extern void rpc_server_dec_reply(struct rpc_server *);

static struct app_info *app_alloc()
{
	struct app_info *app = 0;
	app = calloc(1, sizeof(*app));
	if(!app)
		return 0;
	return app;
}

static struct app_info *app_find(struct dart_server *ds, int appid)
{
	struct app_info *app;
	list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
		if(app->app_id == appid)
			return app;
	}
	return 0;
}

static int dsrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	ds->f_s_unreg = 0;
	return 0;
}

static int ds_remove_app(struct dart_server *ds, struct node_id *peer, int appid)
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct ptlid_map *pptlmap;
	struct app_info *app;
	int i, k, err;

	app = app_find(ds, appid);
	if(!app) {
		goto err_out;
	}
	err = -ENOMEM;
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->msg_rpc->id = appid;
	msg->msg_rpc->cmd = cn_unregister_app;
	peer->f_unreg = 1;

	err = rpc_send(ds->rpc_s, peer, msg);
	if(err < 0) {
		free(msg);
		goto err_out;
	}




	err = rpc_send(ds->rpc_s, peer, msg);
	if(err < 0) {
		free(msg->msg_data);
		goto err_out_free;
	}

	return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
	return err;


}


static int dsrpc_cn_unregister_app(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);

	int appid = cmd->id;

//      printf("I am peer %d removing app %d before I have %d cp\n", rpc_s->ptlmap.id, appid,ds->num_cp);

	struct node_id *temp_peer, *t;
	list_for_each_entry_safe(temp_peer, t, &ds->rpc_s->peer_list, struct node_id, peer_entry) {
		if(temp_peer->ptlmap.appid == appid) {

			if(temp_peer->rpc_conn.f_connected){
                        	rdma_destroy_qp(temp_peer->rpc_conn.id);
	                        ibv_destroy_cq(temp_peer->rpc_conn.cq);
        	                rdma_destroy_id(temp_peer->rpc_conn.id);
                                ibv_destroy_comp_channel(temp_peer->rpc_conn.comp_channel);

			}

			list_del(&temp_peer->peer_entry);
			ds->peer_size--;
			ds->num_cp--;
			ds->size_cp--;
			ds->current_client_size--;


		}
	}

        struct app_info *app = app_find(ds, appid);
        if(app){
                list_del(&app->app_entry);
	}

	return 0;

}

static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) (cmd->pad);
	struct msg_buf *msg;
	struct node_id *peer;

	int err = -ENOMEM;
	int i;

	hreg->num_cp = 0;
	peer = ds_get_peer(ds, cmd->id);

	int appid = peer->ptlmap.appid;
	msg = msg_buf_alloc(rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->msg_rpc->id = ds->self->ptlmap.id;
	msg->msg_rpc->cmd = cn_unregister;
	peer->f_unreg = 1;

	err = rpc_send(rpc_s, peer, msg);
	if(err < 0) {
		free(msg);
		goto err_out;
	}
	struct node_id *temp_peer, *t;
	list_for_each_entry_safe(temp_peer, t, &ds->rpc_s->peer_list, struct node_id, peer_entry) {

		if(temp_peer == NULL)
			continue;

		if(temp_peer->ptlmap.id != 0 && temp_peer->ptlmap.appid == 0)
			ds_remove_app(ds, temp_peer, peer->ptlmap.appid);


                if(temp_peer->ptlmap.appid == peer->ptlmap.appid) {
                        ds->peer_size--;
                        ds->num_cp--;
                        ds->size_cp--;
                        ds->current_client_size--;
		
                        if(temp_peer->rpc_conn.f_connected&&temp_peer->ptlmap.id != peer->ptlmap.id){
                                rdma_destroy_qp(temp_peer->rpc_conn.id);
                                ibv_destroy_cq(temp_peer->rpc_conn.cq);
                                rdma_destroy_id(temp_peer->rpc_conn.id);
				ibv_destroy_comp_channel(temp_peer->rpc_conn.comp_channel);
                        }
			list_del(&temp_peer->peer_entry);

		}

	}

	struct app_info *app = app_find(ds, appid);
	if(app){
		list_del(&app->app_entry);
	}

	return 0;
      err_out:printf("(%s): failed. (%d) %d\n", __func__, err, appid);
	return err;


}

/* 
  RPC routine to serve a compute node registration request.
*/
static int dsrpc_cn_register(struct rpc_server *rpc_s, struct hdr_register *hdr)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app;
	struct node_id *peer;
	int err = -ENOMEM;
	struct node_id *temp_peer = peer_alloc();
	
    list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);

    temp_peer->ptlmap = hdr->pm_cp;
    INIT_LIST_HEAD(&temp_peer->req_list);
    temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
    temp_peer->num_msg_ret = 0;

	ds->current_client_size++;
	ds->peer_size = ds->peer_size + hdr->num_cp;
	ds->num_cp = ds->num_cp + hdr->num_cp;
	ds->size_cp = ds->size_cp + hdr->num_cp;
	ds->rpc_s->num_peers = ds->rpc_s->num_peers + hdr->num_cp;
	
	app = app_find(ds, hdr->pm_cp.appid);
	if(!app) {
		app = app_alloc();
		if(!app)
			goto err_out;
		app->app_id = hdr->pm_cp.appid;
		app->app_num_peers = hdr->num_cp;
		app->app_min_id = hdr->id_min = hdr->pm_cp.id = cp_rank_cnt;

        list_add(&app->app_entry, &ds->app_list);
        cp_rank_cnt = cp_rank_cnt + app->app_num_peers;
    }

    if(app->app_cnt_peers == app->app_num_peers) {
        printf("app cp is full.\n");
        goto err_out;
    }

	hdr->id_min = app->app_min_id;
	hdr->pm_cp.id = app->app_min_id + app->app_cnt_peers;
	
	temp_peer->ptlmap.id = app->app_min_id + app->app_cnt_peers;
	app->app_cnt_peers++;

	return 0;
    
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}


static int ds_disseminate2(struct dart_server *ds, struct node_id *peer)       //Done
{
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct ptlid_map *pptlmap;
        struct app_info *app;
        int i, k, err;

                err = -ENOMEM;
                msg = msg_buf_alloc(ds->rpc_s, peer, 1);
                if(!msg)
                        goto err_out;
                msg->cb = default_completion_with_data_callback;
                msg->size = sizeof(struct ptlid_map) * (ds->size_sp);
                pptlmap = msg->msg_data = malloc(msg->size);
                if(!msg->msg_data)
                        goto err_out_free;
                //for(k = 0; k < ds->peer_size; k++){
                struct node_id *temp_peer;
                list_for_each_entry(temp_peer, &ds->rpc_s->peer_list, struct node_id, peer_entry) {
			if(temp_peer->ptlmap.appid == 0)
	                        *pptlmap++ = temp_peer->ptlmap;
                }

                msg->msg_rpc->cmd = sp_announce_cp;
                msg->msg_rpc->id = ds->self->ptlmap.id;
                hreg = (struct hdr_register *) msg->msg_rpc->pad;
                hreg->pm_cp = ds->self->ptlmap;
                hreg->num_cp = ds->size_sp;
                hreg->num_sp = ds->size_sp;
                err = rpc_send(ds->rpc_s, peer, msg);
                if(err < 0) {
                        free(msg->msg_data);
                        goto err_out_free;
                }
                err = rpc_process_event(ds->rpc_s);
                if(err != 0)
                        goto err_out_free;
        return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}

static int announce_app_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{

    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct app_info *app;
    struct ptlid_map *pm = msg->msg_data;
    int i, err = 0;
    int appid = 0;
    struct node_id *temp_peer = peer_alloc();

    temp_peer->ptlmap = *pm;

    app = app_find(ds,temp_peer->ptlmap.appid);
    if(!app) {
        goto err_out;
    }

    ds->peer_size = ds->peer_size + app->app_num_peers;
    ds->num_cp = ds->num_cp + app->app_num_peers;
    ds->size_cp = ds->size_cp + app->app_num_peers;
	ds->rpc_s->num_peers = ds->rpc_s->num_peers + app->app_num_peers;
 
    for(i = 0; i < app->app_num_peers; i++) {
	    temp_peer = peer_alloc();
        temp_peer->ptlmap = *pm;
        list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);
        INIT_LIST_HEAD(&temp_peer->req_list);
        temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
        temp_peer->num_msg_ret = 0;
                
        pm++;
    }

    free(msg->msg_data);
    free(msg);
        
    return 0;
      
err_out:
    printf("(%s): err (%d).\n", __func__, err);
    return err;
}


static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app;
	struct ptlid_map *pm;
	int i, err = 0;
	int appid = 0;
	pm = msg->msg_data;

	for(i = 0; i < ds->num_sp; i++) {
        struct node_id *temp_peer = peer_alloc();
        temp_peer->ptlmap = *pm;
        if(temp_peer->ptlmap.id == 0) {
            pm++;
            continue;
        }

        if(temp_peer->ptlmap.address.sin_addr.s_addr == ds->rpc_s->ptlmap.address.sin_addr.s_addr 
                && temp_peer->ptlmap.address.sin_port == ds->rpc_s->ptlmap.address.sin_port) {
             ds->self = temp_peer;
        }

        list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);
        INIT_LIST_HEAD(&temp_peer->req_list);
        temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
        temp_peer->num_msg_ret = 0;
        pm++;
	}
	
    ds->f_reg = 1;
	free(msg->msg_data);
	free(msg);
	return 0;

err_out:
    printf("(%s): err (%d).\n", __func__, err);
	return err;
}

static int ds_disseminate_app(struct dart_server *ds, struct node_id *peer, int appid)
{
    struct msg_buf *msg;
    struct hdr_register *hreg;
    struct ptlid_map *pptlmap;
    struct app_info *app;
    int i, k, err;

    app = app_find(ds,appid);
    if(!app) {
        goto err_out;
    }

    err = -ENOMEM;
    msg = msg_buf_alloc(ds->rpc_s, peer, 1);
    if(!msg) {
        goto err_out;
    }
    msg->cb = default_completion_with_data_callback;
    msg->size = sizeof(struct ptlid_map) * app->app_num_peers;
    pptlmap = msg->msg_data = malloc(msg->size);
    if(!msg->msg_data) {
        goto err_out_free;
    }
    struct node_id *temp_peer;
    list_for_each_entry(temp_peer, &ds->rpc_s->peer_list, struct node_id, peer_entry) {
        if(temp_peer->ptlmap.appid == app->app_id) {
            *pptlmap++ = temp_peer->ptlmap;
        } 
    }         
    msg->msg_rpc->cmd = sp_announce_app;
    msg->msg_rpc->id = ds->self->ptlmap.id;
    hreg = (struct hdr_register *) msg->msg_rpc->pad;
    hreg->pm_cp = ds->self->ptlmap;
    hreg->num_cp = app->app_num_peers;
    hreg->num_sp = appid;

    err = rpc_send(ds->rpc_s, peer, msg);
    if(err < 0) {
        free(msg->msg_data);
        goto err_out_free;
    }
    err = rpc_process_event(ds->rpc_s);
    if(err != 0)
        goto err_out_free;

    return 0;
      
err_out_free:
    free(msg);
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}
	

static int cs_disseminate_cp(struct dart_server *ds, struct node_id *peer)
{

    struct msg_buf *msg;
    struct hdr_register *hreg;
    struct ptlid_map *pptlmap;
    struct app_info *app;
    int i, k, err;

    app = app_find(ds,peer->ptlmap.appid);
    if(!app) {
        goto err_out;
    }

	err = -ENOMEM;
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	msg->cb = default_completion_with_data_callback;
	msg->size = sizeof(struct ptlid_map) * (ds->num_sp + ds->current_client_size - app->app_num_peers);
	pptlmap = msg->msg_data = malloc(msg->size);
	if(!msg->msg_data)
		goto err_out_free;
	struct node_id *temp_peer;
	list_for_each_entry(temp_peer, &ds->rpc_s->peer_list, struct node_id, peer_entry) {
        if(temp_peer->ptlmap.id != 0 && temp_peer->ptlmap.appid != app->app_id) {
	        *pptlmap++ = temp_peer->ptlmap;
		}
	}

    msg->msg_rpc->cmd = sp_announce_cp;
    msg->msg_rpc->id = ds->self->ptlmap.id;
    hreg = (struct hdr_register *) msg->msg_rpc->pad;
    hreg->pm_cp = ds->self->ptlmap;
    hreg->num_cp = ds->current_client_size;
    hreg->num_sp = ds->size_sp;

    err = rpc_send(ds->rpc_s, peer, msg);
    if(err < 0) {
        free(msg->msg_data);
        goto err_out_free;
    }
    err = rpc_process_event(ds->rpc_s);
    if(err != 0)
        goto err_out_free;
        return 0;
      
err_out_free:
    free(msg);
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}


static int cp_disseminate_cs_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct app_info *app, *temp_app;
    struct ptlid_map *pm = msg->msg_data;
    int i, err = 0;
    int appid = 0;
    struct node_id *temp_peer = peer_alloc();

    temp_peer->ptlmap = *pm;

	app = app_find(ds, temp_peer->ptlmap.appid);
	if(!app) {
		goto err_out;
	}

    for(i = 1; i < app->app_num_peers; i++) {
		temp_peer = peer_alloc();

        temp_peer->ptlmap = *pm;

		list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);
		INIT_LIST_HEAD(&temp_peer->req_list);
		temp_peer->num_msg_at_peer = rpc_s->max_num_msg;
		temp_peer->num_msg_ret = 0;
		app->app_cnt_peers++;
		ds->current_client_size++;

		pm++;
    }
    free(msg->msg_data);
    free(msg);

    list_for_each_entry(temp_peer, &ds->rpc_s->peer_list, struct node_id, peer_entry) { 
        if(temp_peer->ptlmap.id != 0 && temp_peer->ptlmap.appid == 0)
	        ds_disseminate_app(ds, temp_peer, app->app_id);
    }

    // disseminate new nodes to existing apps
#ifdef DS_HAVE_DIMES
    list_for_each_entry(temp_app, &ds->app_list, struct app_info, app_entry) {
        if(temp_app->app_id != app->app_id && temp_app->sent_announce) {
            temp_peer = ds_get_peer(ds, temp_app->app_min_id);
            ds_disseminate_app(ds, temp_peer, app->app_id);
        }
    }
#endif
	temp_peer = ds_get_peer(ds, app->app_min_id);
	cs_disseminate_cp(ds,temp_peer);
	app->sent_announce = 1;

    return 0;
    
err_out:
    printf("(%s): err (%d).\n", __func__, err);
    return err;
}

static int dsrpc_cp_disseminate_cs(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct node_id *peer;
    struct msg_buf *msg;
    int err = -ENOMEM;
    
    peer = ds_get_peer(ds, cmd->id);
    msg = msg_buf_alloc(rpc_s, peer, 0);
    if(!msg)
        goto err_out;
	struct app_info *app;

	app = app_find(ds, peer->ptlmap.appid);
	if(!app) {
		goto err_out;
	}
	
    msg->size = sizeof(struct ptlid_map) * app->app_num_peers;
    msg->msg_data = malloc(msg->size);
    if(!msg->msg_data) {
        free(msg);
        goto err_out;
    }
    msg->cb = cp_disseminate_cs_completion; 

    msg->id = cmd->wr_id;
    msg->mr = cmd->mr;

    err = rpc_receive_direct(rpc_s, peer, msg);
    if(err < 0) {
        free(msg);
        goto err_out;
    }
    return 0;
      
err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}


static int dsrpc_announce_app(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	int appid = hreg->num_sp; // abuse of the num_sp field
	struct app_info *app = app_alloc();
        
    app->app_id = appid;
    app->app_num_peers = app->app_cnt_peers = hreg->num_cp;
    list_add(&app->app_entry, &ds->app_list);

    struct node_id *peer;
    struct msg_buf *msg;
    int err = -ENOMEM;
    peer = ds_get_peer(ds, 0);
    msg = msg_buf_alloc(rpc_s, peer, 0);
    if(!msg)
        goto err_out;
    msg->size = sizeof(struct ptlid_map) * (hreg->num_cp);
    msg->msg_data = malloc(msg->size);
    if(!msg->msg_data) {
        free(msg);
        goto err_out;
    }
    msg->cb = announce_app_completion;       //

    msg->id = cmd->wr_id;
    msg->mr = cmd->mr;

    err = rpc_receive_direct(rpc_s, peer, msg);
    if(err < 0) {
        free(msg);
        goto err_out;
    }
        
    return 0;
      
err_out:    
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}



static int dsrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	
    peer = ds_get_peer(ds, 0);
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (ds->num_sp);
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


//Added in IB version
/*
	waiting for all the connection requests from all the peers (ds_slave + dc)
	allocate temp peer_tab to store all the connection info
	ds_register_peers: register all the peers including DS+DC
	Relink/copy all the info from temp peer_tab to ds->peer_tab 
	Disseminate all the register information in ds->peer_tab to other's

	system channel connection
*/


static void *ds_master_listen(void *server)
{
    struct rdma_cm_event *event = NULL;
    int connect_count = 0, err;
    struct rdma_conn_param cm_params;
    struct hdr_register hdr;
    struct connection *conn;
	struct dart_server *ds = (struct dart_server *) server;
    struct node_id *peer = NULL;
	time_t tm_st, tm_end;
	void *priv_data;

	while(ds->rpc_s->thread_alive && (rdma_get_cm_event(ds->rpc_s->rpc_ec, &event) == 0)) {
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
                peer = ds_get_peer(ds, conpara.pm_cp.id);
                conn = &peer->sys_conn;
				ds->s_connected++;
            } else {
	            if(conpara.pm_cp.appid == 0) {
					ds->s_connected++;
					struct node_id *temp_peer = peer_alloc();
					list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);
					temp_peer->ptlmap = conpara.pm_cp;
                    temp_peer->ptlmap.id = sp_rank_cnt;

				    INIT_LIST_HEAD(&temp_peer->req_list);
				    temp_peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
				    temp_peer->num_msg_ret = 0;
					peer = temp_peer;
                    sp_rank_cnt++;
                } else {  
                    hdr.pm_cp = conpara.pm_cp;
                    hdr.pm_sp = conpara.pm_sp;
                    hdr.num_cp = conpara.num_cp;

					struct app_info *app = app_find(ds, hdr.pm_cp.appid);
					if(!app){
                        dsrpc_cn_register(ds->rpc_s, &hdr);
					}

					peer = ds_get_peer(ds, hdr.pm_cp.id);
					conpara.pm_cp.id = peer->ptlmap.id;
                }
                conn = &peer->rpc_conn;
            }
            build_context(event_copy.id->verbs, conn);
            build_qp_attr(&conn->qp_attr, conn, ds->rpc_s);
            err = rdma_create_qp(event_copy.id, conn->pd, &conn->qp_attr);
            if(err != 0) {
                printf("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, ds->rpc_s->num_qp, err, __func__, strerror(errno));
                goto err_out;
            }
            ds->rpc_s->num_qp++;
            event_copy.id->context = conn;  //diff
            conn->id = event_copy.id;       //diff
            conn->qp = event_copy.id->qp;

            if(conpara.type == 0) {
                err = sys_post_recv(ds->rpc_s, peer);
                if(err != 0)
                    goto err_out;
	        } else {
	            err = rpc_post_recv(ds->rpc_s, peer);
		        if(err != 0)
			    goto err_out;
	        }
	        memset(&cm_params, 0, sizeof(struct rdma_conn_param));
	        if(conpara.pm_cp.appid != 0 && conpara.type == 1) {
		        memset(&conpara, 0, sizeof(struct con_param));
		        conpara.pm_sp = peer->ptlmap;
		        conpara.pm_cp = ds->rpc_s->ptlmap;
		        conpara.num_cp = ds->num_sp + ds->current_client_size - 1;
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
        } else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			ds->connected++;
		} else if(event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
            //placeholder
        } else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
	        ds->connected++;
	    } else if(event_copy.event != RDMA_CM_EVENT_DISCONNECTED) {
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

int ds_boot_master(struct dart_server *ds)	//Done
{
	struct rdma_cm_event *event = NULL;
	int connect_count = 0, err;
	int connected = 0;
	struct rdma_conn_param cm_params;
	struct hdr_register hdr;
	struct node_id *peer;
	struct connection *conn;

    int rc;
    ds->rpc_s->thread_alive = 1;
    rc = pthread_create(&(ds->rpc_s->comm_thread), NULL, ds_master_listen, (void *) ds);
    if(rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
        exit(-1);
    }

	while(ds->s_connected != 2 * (ds->size_sp - 1)) {
		sched_yield();
	}

	ds->rpc_s->ptlmap.id = 0;

	printf("'%s()': all the server are registered.%d %d\n", __func__, ds->peer_size, ds->size_cp);
	struct node_id *temp_peer;
	err = 0;

    if(ds->size_sp > 1) {
	    list_for_each_entry(temp_peer, &ds->rpc_s->peer_list, struct node_id, peer_entry) {
		    if(temp_peer->ptlmap.appid == 0 && temp_peer->ptlmap.id != 0)
			    err = ds_disseminate2(ds, temp_peer);
	    }
    }

	if(err != 0)
		goto err_out;
	ds->f_reg = 1;

	return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}


static void *ds_listen(void *server)
{
	struct dart_server *ds = (struct dart_server *) server;
	struct node_id *peer = ds_get_peer(ds, 0);
	struct rdma_conn_param cm_params;
	struct con_param conpara;
	struct connection *con;
	struct rdma_cm_event *event = NULL;
	int j, err, check, connected, connect_count = 0;
	check = 0;
	connected = 0;
	peer = NULL;
	void *priv_data;

	while(ds->rpc_s->thread_alive && (rdma_get_cm_event(ds->rpc_s->rpc_ec, &event) == 0)) {

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
			peer = ds_get_peer(ds, conpara.pm_cp.id);
			while(peer == NULL) {
				sched_yield();
				peer = ds_get_peer(ds, conpara.pm_cp.id);
			}

			if(conpara.type == 0)
				con = &peer->sys_conn;

			else
				con = &peer->rpc_conn;
			build_context(event_copy.id->verbs, con);
			build_qp_attr(&con->qp_attr, con, ds->rpc_s);
			err = rdma_create_qp(event_copy.id, con->pd, &con->qp_attr);
			if(err != 0) {
				printf("rdma_create_qp %d in %s.\n", err, __func__);
				goto err_out;
			}
			event_copy.id->context = con;
			con->id = event_copy.id;
			con->qp = event_copy.id->qp;
			if(conpara.type == 0) {
				err = sys_post_recv(ds->rpc_s, peer);
			} else {
				err = rpc_post_recv(ds->rpc_s, peer);
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
			connected++;
		} else if(event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
		} else {
			rpc_print_connection_err(ds->rpc_s, peer, event_copy);
			printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
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


//Added in IB version
/*
	resolve the rdma_id based on Master DS's IP+Port
	connect to DS_Master
	When connected, call ds_register to send my IP+Port/ID/APPID information to DS_Master
	Get feedback information from DS_Master: Feedback info contains (all the collected information including DS+DC)

	RPC channel connection to all the other peers
	SYS channel connection to all the peers (including ds_master node?)
*/
int ds_boot_slave(struct dart_server *ds)	//Done
{
	struct rdma_conn_param cm_params;
	struct con_param conpara;
	struct connection *con;
	struct rdma_cm_event *event = NULL;
	int i, err, check, connected, connect_count = 0;
    struct node_id *peer = peer_alloc();

	check = 0;
	connected = 0;

    INIT_LIST_HEAD(&peer->req_list);
    peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
    peer->num_msg_ret = 0;

	err = rpc_read_config(0,&peer->ptlmap.address);
    peer->ptlmap.id = 0;

	list_add(&peer->peer_entry, &ds->rpc_s->peer_list);

	if(err < 0)
		goto err_out;
	if(peer->ptlmap.address.sin_addr.s_addr == ds->rpc_s->ptlmap.address.sin_addr.s_addr 
        && peer->ptlmap.address.sin_port == ds->rpc_s->ptlmap.address.sin_port) {

		// This is the master server! the config file may be
		// around from a previous run 
		ds->self = peer;
		ds->self->ptlmap = peer->ptlmap;
		printf("'%s()': WARNING! config file exists, but I am the master server\n", __func__);
		err = ds_boot_master(ds);
		if(err < 0)
			goto err_out;
		return 0;
	}

	//Connect to master server, build rpc channel and sys channel;
	err = rpc_connect(ds->rpc_s, peer);
	if(err != 0) {
		printf("rpc_connect err %d in %s.\n", err, __func__);
		goto err_out;
	}
	
    err = sys_connect(ds->rpc_s, peer);
	if(err != 0) {
		printf("sys_connect err %d in %s.\n", err, __func__);
		goto err_out;
	}

    int rc;
    ds->rpc_s->thread_alive = 1;

    rc = pthread_create(&(ds->rpc_s->comm_thread), NULL, ds_listen, (void *) ds);
    if(rc) {
        printf("ERROR; return code from pthread_create() is %d\n", rc);
        exit(-1);
    }

	//Waiting for dissemination msg from master server;
	while(ds->f_reg == 0) {
		err = rpc_process_event_with_timeout(ds->rpc_s, 1);
		if(err != 0 && err != -ETIME)
			goto err_out;
	}

	//Connect to all other nodes except MS_Server. All the peer info have been stored in peer_tab already.
	// id will connect actively to 1 ~ id-1; get connect request from id+1 ~ id[MAX]
	int n = log2_ceil(ds->num_sp);
	int *check_sp = malloc(sizeof(int) * (ds->num_sp));
	int j;

	for(j = 0; j < ds->num_sp; j++)
		check_sp[j] = 0;

	int *a = malloc(sizeof(int) * n);
	int k;
	int smaller_cid = 0;
	int greater_cid = 0;

	for(k = 0; k < ds->num_sp; k++) {
		a[0] = 1;
		for(j = 1; j < n; j++) {
			a[j] = a[j - 1] * 2;
		}

		for(j = 0; j < n; j++) {
			a[j] = (a[j] + k);
			if(a[j] > ds->num_sp - 1)
				a[j] = a[j] % ds->num_sp;

			if(k == ds->rpc_s->ptlmap.id) {
				check_sp[a[j]] = 1;
			}
			if(a[j] == ds->rpc_s->ptlmap.id) {
				check_sp[k] = 1;
			}
		}
	}
	for(k = 1; k < ds->num_sp; k++) {
		if(check_sp[k] == 1) {
			if(k < ds->rpc_s->ptlmap.id)
				smaller_cid++;
			else
				greater_cid++;
		}
	}

	int count;
	for(i = 1; i < ds->rpc_s->ptlmap.id; i++) {
		count = 0;
		peer = ds_get_peer(ds, i);
		if(1) {
			do {
				err = rpc_connect(ds->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("rpc_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}
		}
		if(check_sp[peer->ptlmap.id] == 1) {
			count = 0;
			do {
				err = sys_connect(ds->rpc_s, peer);
				count++;
			} while(count < 3 && err != 0);
			if(err != 0) {
				printf("sys_connect err %d in %s.\n", err, __func__);
				goto err_out;
			}
		}
	}

	return 0;

err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/* Function to automatically decide if this instance should
   run as 'master' or 'slave'. */
static int ds_boot(struct dart_server *ds)
{
	struct stat st_buff;
	const char fil_lock[] = "srv.lck.0";
	const char fil_conf[] = "conf.0";
	int fd, err;
	int is_master = 0;
	int rank;

	memset(&st_buff, 0, sizeof(st_buff));

	if(ds->comm) {
		MPI_Comm_rank(*ds->comm, &rank);
		if(rank == 0) {
			is_master = 1;
		}
	} else {
		err = fd = open(fil_lock, O_WRONLY | O_CREAT, 0644);
		if(err < 0) {
			goto err_out;
		}
		err = file_lock(fd, 1);
		if(err < 0)
			goto err_fd;
		err = stat(fil_conf, &st_buff);
		if(err < 0 && errno != ENOENT) {
			goto err_flock;
		} else if(st_buff.st_size == 0) {
			is_master = 1;
		}
	}
	if(is_master || ds->size_sp == 1) {

		/* Config file is empty, should run as master. */

		err = rpc_write_config(0,ds->rpc_s);
		if(err != 0)
			goto err_flock;
		if(!ds->comm) {
			file_lock(fd, 0);
		} else {
			MPI_Barrier(*ds->comm);
		}

	    struct node_id *temp_peer = peer_alloc();

		temp_peer->ptlmap = ds->rpc_s->ptlmap;

		ds->self = temp_peer;	
			
	        list_add(&temp_peer->peer_entry, &ds->rpc_s->peer_list);
                INIT_LIST_HEAD(&temp_peer->req_list);
                temp_peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
                temp_peer->num_msg_ret = 0;
	
		err = ds_boot_master(ds);
		if(err != 0)
			goto err_flock;
	} else {
		/* Run as slave. */
		if(!ds->comm) {
			file_lock(fd, 0);
		} else {
			MPI_Barrier(*ds->comm);
		}

		err = ds_boot_slave(ds);
		if(err != 0)
			goto err_flock;
	}

	if(is_master && !ds->comm) {
		close(fd);
		remove(fil_lock);
	}

	return 0;

err_flock:
	if(!ds->comm) { file_lock(fd, 0); }
err_fd:
	if(!ds->comm) {
		close(fd);
		remove(fil_lock);
	}
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
  Public API starts here.
*/

/* 
   Allocate and initialize dart server; the server initializes rpc
   server. 
*/
struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm)
{
	struct dart_server *ds = 0;
	struct node_id *peer;
	size_t size;
	int i, err;
	size = sizeof(struct dart_server) + (num_sp + num_cp) * sizeof(struct node_id);
	ds = calloc(1, size);
	if(!ds)
		goto err_out;
	ds->dart_ref = dart_ref;
	if(comm) {
		ds->comm = malloc(sizeof(*ds->comm));
		err = MPI_Comm_dup(*(MPI_Comm *) comm, ds->comm);
		if(err < 0) {
			printf("MPI_Comm_dup failed\n");
			goto err_out;
		}
	} else {
		ds->comm = NULL;
	}

	ds->peer_size = num_sp + num_cp;
	ds->size_cp = num_cp;
	ds->size_sp = num_sp;
	ds->num_sp = num_sp;
	ds->num_cp = num_cp;
	ds->connected = 0;
	INIT_LIST_HEAD(&ds->app_list);

	cp_rank_cnt = num_sp;
	ds->rpc_s = rpc_server_init(0, NULL, 0, 100, ds->peer_size, ds, DART_SERVER);
	if(!ds->rpc_s)
		goto err_free_dsrv;


	ds->rpc_s->num_peers = ds->peer_size;

	rpc_server_set_rpc_per_buff(ds->rpc_s, ds->peer_size);
	ds->rpc_s->app_num_peers = num_sp;
	
    rpc_add_service(cn_unregister, dsrpc_cn_unregister);
	rpc_add_service(cn_unregister_app, dsrpc_cn_unregister_app);
    rpc_add_service(cn_s_unregister, dsrpc_unregister);
    rpc_add_service(cp_disseminate_cs, dsrpc_cp_disseminate_cs);
	rpc_add_service(sp_announce_cp, dsrpc_announce_cp);
    rpc_add_service(sp_announce_app, dsrpc_announce_app);

	if(num_sp == 1) {

		/* If there is a single server, mark it as registered,
		   but it should also be master! */
		ds->f_reg = 1;
	}
	err = ds_boot(ds);
	if(err != 0)
		goto err_free_dsrv;
	ds->num_charge = (ds->num_cp / ds->num_sp) + (ds->rpc_s->ptlmap.id < ds->num_cp % ds->num_sp);
	
    if(comm) {
        MPI_Barrier(*ds->comm);
    }
    printf("'%s()': init ok.\n", __func__);
	return ds;
      err_free_dsrv:free(ds);
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return NULL;
}

void ds_free(struct dart_server *ds)
{
	int err;
	struct app_info *app, *t;

	if(ds->rpc_s->thread_alive) {
		ds->rpc_s->thread_alive = 0;
		pthread_cancel(ds->rpc_s->comm_thread);
		//pthread_join(ds->rpc_s->comm_thread, NULL);
	}


	err = rpc_server_free(ds->rpc_s);
	if(err != 0)
		printf("rpc_server_free err in %s.\n", __func__);
	list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry) {
		list_del(&app->app_entry);
		free(app);
	}

	if(ds->comm) {
		free(ds->comm);
	}

	free(ds);
}

int ds_process(struct dart_server *ds)
{
	return rpc_process_event(ds->rpc_s);
}
