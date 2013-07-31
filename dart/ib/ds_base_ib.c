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
	app = malloc(sizeof(*app));
	if(!app)
		return 0;
	memset(app, 0, sizeof(*app));
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


/* 
   Routine to reject a registration request from a compute node which
   exceed the number of peers announced by an application. This is a
   protection routine for error cases.
static int ds_register_cp_reject(struct rpc_server *rpc_s, struct hdr_register *hreg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct msg_buf *msg;
	struct node_id peer;
	int err = -ENOMEM;

	msg = msg_buf_alloc(rpc_s, &peer, 1);
	if (!msg)
		goto err_out;

	memset(&peer, 0, sizeof(peer));
	INIT_LIST_HEAD(&peer.req_list);
	peer.ptlmap = hreg->pm_cp;
	peer.mb = MB_RPC_MSG;
	peer.num_msg_at_peer = 1;

	msg->msg_rpc->cmd = cn_register;
	msg->msg_rpc->id = ds->self->ptlmap.id;

	hreg = (struct hdr_register *) msg->msg_rpc->pad;
	hreg->id_min = -1;

	err = rpc_send(rpc_s, &peer, msg);
	if (err == 0)
		return 0;

	free(msg);
 err_out:
       	printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}
*/

/*
//RPC routine to unregister a compute peer.
static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//Working
{
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct msg_buf *msg;
        struct node_id *peer;
        int err = -ENOMEM;

        static int num_unreg = 0;
	// num_unreg++;
        if ((++num_unreg) == ds->num_cp)
                ds->f_stop = 1;

        ds->f_nacc = 1;

	if (hreg->num_cp) {
		hreg->num_cp = 0;
		peer = ds_get_peer(ds, cmd->id);

		msg = msg_buf_alloc(rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->id = ds->self->ptlmap.id;
		msg->msg_rpc->cmd = cn_unregister;

		err = rpc_send(rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}

	}

        hreg->num_sp--;
        if (hreg->num_sp) {
                peer = ds_get_peer(ds, (ds->self->ptlmap.id+1)%ds->num_sp);
//printf("%d send unreg to %d.\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id);
                msg = msg_buf_alloc(rpc_s, peer, 1);
                if (!msg)
			goto err_out;

                memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
                msg->msg_rpc->id = ds->self->ptlmap.id;

                err = rpc_send(rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
			goto err_out;
                }
        }

//printf("%d get %d num_unreg with ds->stop %d.\n", ds->rpc_s->ptlmap.id, num_unreg, ds->f_stop);
	return 0;
 err_out:
       	printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}
*/


static int dsrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	ds->f_s_unreg = 0;
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
	int in_charge = (ds->num_cp / ds->num_sp) + (ds->rpc_s->ptlmap.id < ds->num_cp % ds->num_sp);
	static int num_unreg = 0;
	if(ds->f_stop != 1) {
		num_unreg = num_unreg + hreg->num_cp;
//              printf("%d %d %d\n",num_unreg, hreg->num_cp, ds->num_cp);
		if(num_unreg == ds->num_cp)
			ds->f_stop = 1;

		// All compute peers  have unregistered. I should send one RPC but not respond to any.

//printf("Rank %d: num_unreg is %d from %d, ds->num_cp is %d,  has %d left.\n", ds->rpc_s->ptlmap.id, num_unreg, cmd->id , ds->num_cp,ds->num_charge);//debug
		//After  the first  compute peer  'unregister'  request, stop accepting new requests and terminate pending ones.
		ds->f_nacc = 1;
		if(cmd->id < ds->num_sp && ds->num_charge == 0) {
			rpc_s->no_more = 1;
		}

		if(hreg->num_cp && cmd->id < ds->num_sp) {
			hreg->num_cp = 0;
			peer = ds_get_peer(ds, cmd->id);
			msg = msg_buf_alloc(rpc_s, peer, 1);
			if(!msg)
				goto err_out;
			msg->msg_rpc->id = ds->self->ptlmap.id;
			msg->msg_rpc->cmd = cn_s_unregister;

			err = rpc_send(rpc_s, peer, msg);
			if(err < 0) {
				free(msg);
				goto err_out;
			}
		}


		if(hreg->num_cp && cmd->id >= ds->num_sp) {
			hreg->num_cp = 0;
			peer = ds_get_peer(ds, cmd->id);
			if(peer->f_unreg != 1) {
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
			}
			ds->num_charge--;
		}
		if(ds->num_charge == 0 && rpc_s->no_more == 0) {
			rpc_s->no_more = 1;
			for(i = 0; i < ds->num_sp; i++) {
				if(ds->self->ptlmap.id == i)
					continue;
				hreg->num_cp = in_charge;
				peer = ds_get_peer(ds, i);
				if(peer->f_unreg != 1) {
					msg = msg_buf_alloc(rpc_s, peer, 1);
					if(!msg)
						goto err_out;
					memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
					msg->msg_rpc->id = ds->self->ptlmap.id;
					peer->f_unreg = 1;

					//printf("%d passing %d to  %d that %d is unreg %ld\n",ds->self->ptlmap.id,msg->msg_rpc->cmd,peer->ptlmap.id,cmd->id,msg);
					err = rpc_send(rpc_s, peer, msg);

					ds->f_s_unreg = 1;

					while(ds->f_s_unreg) {
						err = rpc_process_event(ds->rpc_s);
						if(err < 0)
							goto err_out;
					}

					if(err < 0) {
						free(msg);
						goto err_out;
					}
				}
			}
		}
	}
//      printf("rank %d: %s ends.\n\n", ds->self->ptlmap.id, __func__);//debug
	return 0;
      err_out:printf("(%s): failed. (%d)\n", __func__, err);
	return err;
}

static int file_lock(int fd, int op)
{
	int err;
	struct flock fl = {.l_type = (op != 0) ? F_WRLCK : F_UNLCK,.l_whence = SEEK_SET,.l_start = 0,.l_len = 0,.l_pid = getpid()
	};
	err = fcntl(fd, F_SETLKW, &fl);
	if(err == 0)
		return 0;
	printf("'%s()' failed\n", __func__);
	return err;
}


//-----------------------------------------------------------------------------------------------------
//TODO:  After getting rpc reg msg, check the qp num, change corresponding peer->conn->f_connected to 1 after get this
/*
	process the CQ to get the peer registration information.
	1. fetch one CQ event (the 1st one must be the registration event) 
	2. call ds_register_peer 			
	3. Move the tmp_peer to the right position of peer_tab based on the registration info
*/

/* 
  RPC routine to serve a compute node registration request.
*/
static int dsrpc_cn_register(struct rpc_server *rpc_s, struct hdr_register *hdr)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app;
	struct node_id *peer;
	int err = -ENOMEM;
	app = app_find(ds, hdr->pm_cp.appid);
	if(!app) {
		app = app_alloc();
		if(!app)
			goto err_out;
		app->app_id = hdr->pm_cp.appid;
		app->app_num_peers = hdr->num_cp;
		hdr->id_min = hdr->pm_cp.id = cp_rank_cnt;
		app->app_peer_tab = ds_get_peer(ds, hdr->pm_cp.id);
		app->app_peer_tab[0].ptlmap = hdr->pm_cp;
		app->app_peer_tab[0].ptlmap.id = hdr->pm_cp.id;
		list_add(&app->app_entry, &ds->app_list);
		cp_rank_cnt = cp_rank_cnt + app->app_num_peers;
	}

	/* COMMENTED:
	   First check if app has registered all its compute nodes already ! 
	 */
	if(app->app_cnt_peers == app->app_num_peers) {
		printf("app cp is full.\n");

		////return ds_register_cp_reject(rpc_s, hreg);
		goto err_out;
	}

	/* COMMENTED:
	   cn_peer = app->app_peer_tab + app->app_cnt_peers;
	   cn_peer->ptlmap = hreg->pm_cp;
	   cn_peer->ptlmap.id = cn_peer - ds->peer_tab;
	   cn_peer->num_msg_ret++;//TODO:Credit

	   ds->self->num_msg_ret--;//TODO:Credit
	 */
	hdr->id_min = app->app_peer_tab[0].ptlmap.id;
	peer = ds_get_peer(ds, app->app_peer_tab[0].ptlmap.id + app->app_cnt_peers);
	peer->ptlmap = hdr->pm_cp;
	peer->ptlmap.id = hdr->pm_cp.id = app->app_peer_tab[0].ptlmap.id + app->app_cnt_peers;
	app->app_cnt_peers++;

	/* Wait for all of the peers to join in. 
	   if (app->app_cnt_peers != app->app_num_peers || !ds->f_reg)
	   return 0;
	   COMMENTED: */
	return 0;
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}

static int ds_disseminate(struct dart_server *ds)	//Done
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer, *cpeer;
	struct ptlid_map *pptlmap;
	struct app_info *app;
	int i, k, err;
	cpeer = ds->peer_tab;

	//send to slave servers all the peers' info
	for(i = 1; i < ds->size_sp; i++) {
		peer = ds_get_peer(ds, i);
		err = -ENOMEM;
		msg = msg_buf_alloc(ds->rpc_s, peer, 1);
		if(!msg)
			goto err_out;
		msg->cb = default_completion_with_data_callback;
		msg->size = sizeof(struct ptlid_map) * (ds->peer_size);
		pptlmap = msg->msg_data = malloc(msg->size);
		if(!msg->msg_data)
			goto err_out_free;
		for(k = 0; k < ds->peer_size; k++)
			*pptlmap++ = (cpeer++)->ptlmap;
		cpeer = ds->peer_tab;
		msg->msg_rpc->cmd = sp_announce_cp;
		msg->msg_rpc->id = ds->self->ptlmap.id;
		hreg = (struct hdr_register *) msg->msg_rpc->pad;
		hreg->pm_cp = cpeer->ptlmap;
		hreg->num_cp = ds->peer_size - ds->size_sp;
		hreg->num_sp = ds->size_sp;
		err = rpc_send(ds->rpc_s, peer, msg);
		if(err < 0) {
			free(msg->msg_data);
			goto err_out_free;
		}
		err = rpc_process_event(ds->rpc_s);
		if(err != 0)
			goto err_out_free;
	}

	//send to clients info of all the servers + clients in the same APP
	list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
		for(i = app->app_peer_tab[0].ptlmap.id; i < app->app_peer_tab[0].ptlmap.id + app->app_num_peers; i++) {
			cpeer = ds->peer_tab;
			peer = ds_get_peer(ds, i);

//printf("send to %d\n", peer->ptlmap.id);
			err = -ENOMEM;
			msg = msg_buf_alloc(ds->rpc_s, peer, 1);
			if(!msg)
				goto err_out;
			msg->cb = default_completion_with_data_callback;
			msg->size = sizeof(struct ptlid_map) * (ds->size_sp + app->app_num_peers);
			pptlmap = msg->msg_data = malloc(msg->size);
			if(!msg->msg_data)
				goto err_out_free;
			for(k = 0; k < ds->size_sp; k++)
				*pptlmap++ = (cpeer++)->ptlmap;
			cpeer = app->app_peer_tab;
			for(k = 0; k < app->app_num_peers; k++)
				*pptlmap++ = (cpeer++)->ptlmap;
			cpeer = ds->peer_tab;
			msg->msg_rpc->cmd = sp_announce_cp;
			msg->msg_rpc->id = ds->self->ptlmap.id;
			hreg = (struct hdr_register *) msg->msg_rpc->pad;
			hreg->pm_cp = cpeer->ptlmap;
			hreg->num_cp = app->app_num_peers;
			hreg->num_sp = ds->size_sp;
			err = rpc_send(ds->rpc_s, peer, msg);
			if(err < 0) {
				free(msg->msg_data);
				goto err_out_free;
			}
			err = rpc_process_event(ds->rpc_s);
			if(err != 0)
				goto err_out_free;
		}
	}
	return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
	return err;
}

static int ds_disseminate_all(struct dart_server *ds)       //Done
{
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct node_id *peer, *cpeer;
        struct ptlid_map *pptlmap;
        struct app_info *app;
        int i, k, err;
        cpeer = ds->peer_tab;

        list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
                for(i = app->app_peer_tab[0].ptlmap.id; i < app->app_peer_tab[0].ptlmap.id + app->app_num_peers; i++) {
                        cpeer = ds->peer_tab;
                        peer = ds_get_peer(ds, i);

                        err = -ENOMEM;
                        msg = msg_buf_alloc(ds->rpc_s, peer, 1);
                        if(!msg)
                                goto err_out;
                        msg->cb = default_completion_with_data_callback;
                        msg->size = sizeof(struct ptlid_map) * (ds->size_sp + ds->size_cp);
                        pptlmap = msg->msg_data = malloc(msg->size);
                        if(!msg->msg_data)
                                goto err_out_free;
                        for(k = 0; k < ds->size_sp + ds->size_cp; k++)
                                *pptlmap++ = (cpeer++)->ptlmap;
                        cpeer = ds->peer_tab;
                        msg->msg_rpc->cmd = sp_announce_cp_all;
                        msg->msg_rpc->id = ds->self->ptlmap.id;
                        hreg = (struct hdr_register *) msg->msg_rpc->pad;
                        hreg->pm_cp = cpeer->ptlmap;
                        hreg->num_cp = ds->size_cp;
                        hreg->num_sp = ds->size_sp;
                        err = rpc_send(ds->rpc_s, peer, msg);
                        if(err < 0) {
                                free(msg->msg_data);
                                goto err_out_free;
                        }
                        err = rpc_process_event(ds->rpc_s);
                        if(err != 0)
                                goto err_out_free;
                }
        }
        return 0;
      err_out_free:free(msg);
      err_out:printf("'%s()' failed with %d.\n", __func__, err);
        return err;
}


static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app;
	struct node_id *peer;

	struct ptlid_map *pm;
	int i, err = 0;
	int appid = 0;
	peer = (struct node_id *) (ds + 1);
	peer++;
	pm = msg->msg_data;
	pm = pm + 1;

//printf("In '%s()'.\n", __func__);
	for(i = 0; i < ds->peer_size - 1; i++) {
		peer->ptlmap = *pm;
		if(peer->ptlmap.address.sin_addr.s_addr == ds->rpc_s->ptlmap.address.sin_addr.s_addr && peer->ptlmap.address.sin_port == ds->rpc_s->ptlmap.address.sin_port)
			ds->self = peer;

//printf("In '%s()'.\n", __func__);
		if(pm->appid != appid) {
			appid = pm->appid;
			app = app_alloc();
			if(!app)
				goto err_out;
			app->app_id = pm->appid;
			app->app_num_peers = app->app_cnt_peers = 1;
			app->app_peer_tab = peer;
			list_add(&app->app_entry, &ds->app_list);
		}

		else if(pm->appid != 0) {
			app->app_cnt_peers++;
			app->app_num_peers = app->app_cnt_peers;
		}
		peer++;
		pm = pm + 1;
	}

//printf("In '%s()'.\n", __func__);
	ds->f_reg = 1;
	free(msg->msg_data);
	free(msg);
	return 0;
      err_out:printf("(%s): err (%d).\n", __func__, err);
	return err;
}

static int dsrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//Done
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);

	//struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	peer = ds_get_peer(ds, 0);
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = sizeof(struct ptlid_map) * (ds->peer_size);
	msg->msg_data = malloc(msg->size);
	if(!msg->msg_data) {
		free(msg);
		goto err_out;
	}
	msg->cb = announce_cp_completion;	//

	msg->id = cmd->wr_id;
	msg->mr = cmd->mr;


//printf("In '%s()'.\n", __func__);
	err = rpc_receive_direct(rpc_s, peer, msg);
	if(err < 0) {
		free(msg);
		goto err_out;
	}
//printf("In '%s()'.\n", __func__);
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
int ds_boot_master(struct dart_server *ds)	//Done
{
	struct rdma_cm_event *event = NULL;
	int connect_count = 0, err;
	int connected = 0;
	struct rdma_conn_param cm_params;
	struct hdr_register hdr;
	struct node_id *peer;
	struct connection *conn;
	while(rdma_get_cm_event(ds->rpc_s->rpc_ec, &event) == 0) {
		struct con_param conpara;
		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);
		if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {

			//printf("received connection request.\n");//debug
			conpara = *(struct con_param *) event_copy.param.conn.private_data;
			if(conpara.type == 0) {
				peer = ds_get_peer(ds, conpara.pm_cp.id);
				conn = &peer->sys_conn;
			}

			else {
				if(conpara.pm_cp.appid == 0) {
					peer = ds_get_peer(ds, sp_rank_cnt);
					peer->ptlmap = conpara.pm_cp;
					peer->ptlmap.id = sp_rank_cnt;
					sp_rank_cnt++;
				}

				else {
					hdr.pm_cp = conpara.pm_cp;
					hdr.pm_sp = conpara.pm_sp;
					hdr.num_cp = conpara.num_cp;
					dsrpc_cn_register(ds->rpc_s, &hdr);
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
			event_copy.id->context = conn;	//diff
			conn->id = event_copy.id;	//diff
			conn->qp = event_copy.id->qp;

//testing here:
/*struct ibv_qp_attr qp_attr;
			memset(&qp_attr, 0, sizeof(struct ibv_qp_attr));
			qp_attr.cap.max_send_wr = 10;
			qp_attr.cap.max_recv_wr = 10;
			qp_attr.cap.max_send_sge = 1;
			qp_attr.cap.max_recv_sge = 1;
			qp_attr.cap.max_inline_data = 28;
			err = ibv_modify_qp(conn->qp, &qp_attr, IBV_QP_CAP);
			if(err !=0){
				printf("ibv_modify_qp err %d in %s.\n", err, __func__);
				goto err_out;
			}
*/
			if(conpara.type == 0) {
				err = sys_post_recv(ds->rpc_s, peer);
				if(err != 0)
					goto err_out;
			}

			else {
				err = rpc_post_recv(ds->rpc_s, peer);
				if(err != 0)
					goto err_out;
			}
			memset(&cm_params, 0, sizeof(struct rdma_conn_param));
			if(conpara.pm_cp.appid != 0 && conpara.type == 1) {
				memset(&conpara, 0, sizeof(struct con_param));
				conpara.pm_sp = peer->ptlmap;
				conpara.pm_cp = ds->rpc_s->ptlmap;
				conpara.num_cp = ds->num_sp + ds->num_cp;
				conpara.type = hdr.id_min;

				//printf("id min is %d.\n", conpara.type); 
				cm_params.private_data = &conpara;
				cm_params.private_data_len = sizeof(conpara);
			}

			else {
				cm_params.private_data = &peer->ptlmap.id;
				cm_params.private_data_len = sizeof(int);
			} cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;	//diff
			cm_params.rnr_retry_count = 7;	//infinite retry
			err = rdma_accept(event_copy.id, &cm_params);
			if(err != 0) {
				printf("rdma_accept %d in %s.\n", err, __func__);
				goto err_out;
			}
			connect_count++;
			conn->f_connected = 1;
		}

		else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
			//printf("Connection is established.\n");//DEBUG
			connected++;
		}

		else {
			rpc_print_connection_err(ds->rpc_s, peer, event_copy);
			printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
			err = event_copy.status;
			goto err_out;
		}
		if(connected == 2 * (ds->peer_size - 1) - ds->size_cp)
			break;
	}


	//printf("prepare to check.\n");
	if(connected != 2 * (ds->peer_size - 1) - ds->size_cp || connected != connect_count) {
		printf("Connected number doesn't match needed.\n");
		err = -1;
		goto err_out;
	}


	printf("'%s()': all the peer are registered.%d %d\n", __func__, ds->peer_size, ds->size_cp);
	ds->rpc_s->cur_num_peer = ds->rpc_s->num_rpc_per_buff;	//diff    

	err = ds_disseminate(ds);

	sleep(10);

	err = ds_disseminate_all(ds);

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
	while(ds->rpc_s->thread_alive && (rdma_get_cm_event(ds->rpc_s->rpc_ec, &event) == 0)) {

		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event, sizeof(*event));
		rdma_ack_cm_event(event);
		if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
			conpara = *(struct con_param *) event_copy.param.conn.private_data;
			peer = ds_get_peer(ds, conpara.pm_cp.id);
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

	}

	pthread_exit(0);
	return 0;
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
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
	struct node_id *peer = ds_get_peer(ds, 0);
	struct rdma_conn_param cm_params;
	struct con_param conpara;
	struct connection *con;
	struct rdma_cm_event *event = NULL;
	int i, err, check, connected, connect_count = 0;
	check = 0;
	connected = 0;
	err = rpc_read_config(&peer->ptlmap.address);
	if(err < 0)
		goto err_out;
	if(peer->ptlmap.address.sin_addr.s_addr == ds->rpc_s->ptlmap.address.sin_addr.s_addr && peer->ptlmap.address.sin_port == ds->rpc_s->ptlmap.address.sin_port) {

		/* This is the master server! the config file may be
		   around from a previous run */
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
	ds->rpc_s->peer_tab[ds->rpc_s->ptlmap.id].ptlmap = ds->rpc_s->ptlmap;
	ds->rpc_s->peer_tab[1].ptlmap = ds->rpc_s->ptlmap;	//diff

	//Waiting for dissemination msg from master server;
	while(ds->f_reg == 0) {
		err = rpc_process_event_with_timeout(ds->rpc_s, 1);
		if(err != 0 && err != -ETIME)
			goto err_out;
	}

//printf("howdy 0.\n");
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


	int rc;
	ds->rpc_s->thread_alive = 1;
	rc = pthread_create(&(ds->rpc_s->comm_thread), NULL, ds_listen, (void *) ds);
	if(rc) {
		printf("ERROR; return code from pthread_create() is %d\n", rc);
		exit(-1);
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
/*
	if(i == ds->rpc_s->ptlmap.id) {
		peer = NULL;
		while(rdma_get_cm_event(ds->rpc_s->rpc_ec, &event) == 0) {
			struct rdma_cm_event event_copy;
			memcpy(&event_copy, event, sizeof(*event));
			rdma_ack_cm_event(event);
			// printf("Server %d %d \n",ds->rpc_s->ptlmap.id,connected);
			if(event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST)
			 {
				conpara = *(struct con_param *) event_copy.param.conn.private_data;
				peer = ds_get_peer(ds, conpara.pm_cp.id);
				if(conpara.type == 0)
					con = &peer->sys_conn;
				
				else
					con = &peer->rpc_conn;
				build_context(event_copy.id->verbs, con);
				build_qp_attr(&con->qp_attr, con, ds->rpc_s);
				err = rdma_create_qp(event_copy.id, con->pd, &con->qp_attr);
				if(err != 0) {
                                printf("Peer %d couldnot connect to peer %d. Current number of qp is  %d\n rdma_create_qp %d in %s %s.\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, ds->rpc_s->num_qp, err, __func__, strerror(errno));
					goto err_out;
				}
				ds->rpc_s->num_qp++;
				event_copy.id->context = con;
				con->id = event_copy.id;
				con->qp = event_copy.id->qp;
				if(conpara.type == 0) {
					err = sys_post_recv(ds->rpc_s, peer);
					if(err != 0)
						goto err_out;
				}
				
				else {
					err = rpc_post_recv(ds->rpc_s, peer);
					if(err != 0)
						goto err_out;
				}
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
			}
			
			else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED){
				connected++;
			}
			else {
	                        rpc_print_connection_err(ds->rpc_s,peer,event_copy);
				printf("event is %d with status %d.\n", event_copy.event, event_copy.status);
				err = event_copy.status;
				goto err_out;
			}
			//if(connected == ds->num_cp + greater_cid){
			if(connected == (ds->peer_size - ds->rpc_s->ptlmap.id - 1) + greater_cid) {
			//if(connected == (2*(ds->peer_size-ds->rpc_s->ptlmap.id-1))-ds->num_cp){
			//if(connected == (2*(ds->peer_size-ds->rpc_s->ptlmap.id-1))){
			// printf("Server %d %d \n",ds->rpc_s->ptlmap.id,connected);
				break;
			}
		}

//		if(connected != connect_count){
//			printf("Connected number doesn't match needed.\n");
//			err = -1;
//			goto err_out;
//		}
 
	}

        int rc;
        ds->rpc_s->thread_alive = 1;
        rc = pthread_create(&(ds->rpc_s->comm_thread), NULL, ds_listen, (void *) ds);
        if (rc){
                printf("ERROR; return code from pthread_create() is %d\n", rc);
                exit(-1);
        }
*/

	return 0;
      err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}


/* Function to automatically decide if this instance should
   run as 'master' or 'slave'. */
static int ds_boot(struct dart_server *ds)	//Done
{
	struct stat st_buff;
	const char fil_lock[] = "srv.lck";
	const char fil_conf[] = "conf";
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
	if(st_buff.st_size == 0 || ds->size_sp == 1) {

		/* Config file is empty, should run as master. */
		ds->self = ds->peer_tab;
		ds->self->ptlmap = ds->rpc_s->ptlmap;

		//ds->self->num = ds->size_sp;//added in IB Version
		err = rpc_write_config(ds->rpc_s);
		if(err != 0)
			goto err_flock;
		file_lock(fd, 0);

		//added in IB version
//printf("I am master server.\n");//DEBUG
		err = ds_boot_master(ds);
		if(err != 0)
			goto err_flock;
	}

	else {

		/* Run as slave. */
		file_lock(fd, 0);

//printf("I am slave server.\n");//DEBUG
		err = ds_boot_slave(ds);
		if(err != 0)
			goto err_flock;
	}


//        printf("peer# %d address %s\n", ds->rpc_s->ptlmap.id,inet_ntoa(ds->rpc_s->ptlmap.address.sin_addr));


/*
	for(i=0;i<ds->peer_size;i++){
		peer = ds_get_peer(ds, i);
		if((peer->rpc_conn.f_connected && peer->sys_conn.f_connected) || i == ds->self->ptlmap.id)
			continue;
		printf("Peer %d is not connected!\n", i);
		err = -1;
		goto err_out;
	}
*/
	ds->rpc_s->cur_num_peer = ds->rpc_s->num_rpc_per_buff;
	close(fd);
	remove(fil_lock);
	return 0;
      err_flock:file_lock(fd, 0);
      err_fd:close(fd);
	remove(fil_lock);
      err_out:printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}


/*
  Public API starts here.
*/

/* 
   Allocate and initialize dart server; the server initializes rpc
   server. 
*/
struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref)
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
	ds->peer_tab = (struct node_id *) (ds + 1);
	ds->cn_peers = ds->peer_tab + num_sp;
	ds->peer_size = num_sp + num_cp;
	ds->size_cp = num_cp;
	ds->size_sp = num_sp;
	ds->num_sp = num_sp;
	ds->num_cp = num_cp;
	INIT_LIST_HEAD(&ds->app_list);
	cp_rank_cnt = num_sp;
	ds->rpc_s = rpc_server_init(0, NULL, 0, 10, ds->peer_size, ds, DART_SERVER);
	if(!ds->rpc_s)
		goto err_free_dsrv;
	rpc_server_set_peer_ref(ds->rpc_s, ds->peer_tab, ds->peer_size);
	rpc_server_set_rpc_per_buff(ds->rpc_s, ds->peer_size);
	ds->rpc_s->app_num_peers = num_sp;
	ds->rpc_s->cur_num_peer = 2;
	peer = ds->peer_tab;
	for(i = 0; i < ds->peer_size; i++) {
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->num_msg_ret = 0;
		peer++;
	}

	//rpc_add_service(cn_register, dsrpc_cn_register);
	rpc_add_service(cn_unregister, dsrpc_cn_unregister);
	rpc_add_service(cn_s_unregister, dsrpc_unregister);

	//rpc_add_service(sp_reg_request, dsrpc_sp_register);
	//rpc_add_service(peer_reg_address, dsrpc_peer_fetch);
	//rpc_add_service(sp_reg_reply, dsrpc_sp_ack_register);
	rpc_add_service(sp_announce_cp, dsrpc_announce_cp);
	if(num_sp == 1) {

		/* If there is a single server, mark it as registered,
		   but it should also be master! */
		ds->f_reg = 1;
	}
	err = ds_boot(ds);
	if(err != 0)
		goto err_free_dsrv;
	ds->num_charge = (ds->num_cp / ds->num_sp) + (ds->rpc_s->ptlmap.id < ds->num_cp % ds->num_sp);
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
	free(ds);
}

int ds_process(struct dart_server *ds)
{
	return rpc_process_event(ds->rpc_s);
}
