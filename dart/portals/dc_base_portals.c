/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*/
/*
 * Base implementation of DART client.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "dc_base_portals.h"

static int register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct node_id *peer;
        struct ptlid_map *pptlmap;
        int i, id_min;

        peer = dc->peer_tab;
        pptlmap = msg->msg_data;
        id_min = dc->cp_min_rank;

        for (i = 0; i < dc->peer_size; i++) {
                peer->ptlmap = *pptlmap;

                /*
                if (peer->ptlmap.id >= id_min && 
                    peer->ptlmap.id < id_min + dc->cp_in_job)
                        peer->appid = dc->self->appid;
                */
                peer++;
                pptlmap++;
        }
#if 0 /* DELETE ... */
        /* Fix the number of credits for server peers. */
        for (i = 0; i < dc->num_sp; i++) {
                peer = dc->peer_tab + i;
                peer->num_msg_at_peer = rpc_s->max_num_msg;
                if (peer == msg->peer)
                        peer->num_msg_ret = 1;
                else    peer->num_msg_ret = 0;
        }

        /* Fix the number of credits for compute peers in the app job. */
        for (i = 0; i < dc->num_cp; i++) {
                peer = dc->cn_peers + i;
                if (dc->self->appid == peer->appid) {
                        /* This 'peer' is from the same app job. */
                        peer->num_msg_at_peer = rpc_s->max_num_msg;
                        peer->num_msg_ret = 0;

                        if (dc->cp_min_rank > peer->id)
                                dc->cp_min_rank = peer->id;
                }
        }
#endif
        rpc_s->app_minid = dc->cp_min_rank;
        rpc_s->app_num_peers = dc->cp_in_job;

        free(msg->msg_data);
        free(msg);
        dc->f_reg = 1;

	/* DEBUG:
        uloga("'%s': job has %d peers, staring at rank %d.\n", 
                __func__, dc->cp_in_job, dc->cp_min_rank);
	*/
        return 0;
}

/*
  RPC routine to complete the registration process with a master server
  peer.
*/
static int dcrpc_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        size_t size_peertab;
        int i, err = -ENOMEM;

	/* First check if the registration was successefull at the server. */
	if (hreg->id_min < 0) {
		uloga("'%s()': ERROR registration request was rejected by server\n",
			__func__);
		dc->f_reg = 1;
		return -1;
	}

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
                peer->mb = MB_RPC_MSG;
                peer->num_msg_at_peer = rpc_s->max_num_msg;
                peer->num_msg_ret = 0;

                peer++;
        }

        peer = dc->peer_tab;
        /* To register and get an ID I sent a message to the servers
           group master, adjust its credits here.  */
        peer->num_msg_at_peer--;

        rpc_server_set_peer_ref(rpc_s, dc->peer_tab, dc->peer_size); 
        rpc_server_set_rpc_per_buff(rpc_s, dc->num_sp + dc->cp_in_job);

        /* Setup self reference. */
        dc->self = dc_get_peer(dc, hreg->pm_cp.id);
        dc->self->ptlmap = hreg->pm_cp;
        rpc_s->ptlmap = hreg->pm_cp;

        /* Setup server reference, note that cmd->id can be different
           than 0 when running with multiple servers. */
        peer = dc_get_peer(dc, cmd->id);
        peer->ptlmap = hreg->pm_sp;
        peer->num_msg_ret++;
        if (peer == dc->peer_tab)
                dc->peer_tab->num_msg_at_peer++;

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg)
                goto err_out;

        size_peertab = sizeof(struct ptlid_map) * dc->peer_size;
        msg->msg_data = malloc(size_peertab);
        if (!msg->msg_data) {
                free(msg);
                goto err_out;
        }
        msg->size = size_peertab;
        msg->cb = register_completion;
        peer->mb = cmd->mbits;

        err = rpc_receive_direct(rpc_s, peer, msg);
        peer->mb = MB_RPC_MSG;
        if (err < 0)
                goto err_out_free;

        // uloga("'%s()': instance registered with id %d.\n", 
        //        __func__, dc->self->id);
        return 0;
 err_out_free:
        free(msg->msg_data);
        free(msg);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Choose a better name for this.
*/
static int barrier_broadcast(struct dart_client *dc)
{
        struct msg_buf *msg;
        struct node_id *peer;
        int i, err;

        for (i = 1; i < dc->cp_in_job; i++) {
                peer = dc_get_peer(dc, dc->self->ptlmap.id+i);

                err = -ENOMEM;
                msg = msg_buf_alloc(dc->rpc_s, peer, 1);
                if (!msg) 
                        break;

                msg->msg_rpc->cmd = cp_barrier;
                msg->msg_rpc->id = dc->self->ptlmap.id;

                err = rpc_send(dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        break;
                }
        }

        dc->cp_barrier_req = 0;
        dc->f_bar = 1;

        if (i == dc->cp_in_job)
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

        if (dc->self->ptlmap.id == dc->cp_min_rank) {
                /* I am the master peer in this job. */
                dc->cp_barrier_req++;
                if (dc->cp_barrier_req < dc->cp_in_job)
                        return 0;
                uloga("'%s()': I am master rank %d, and all peers joined "
                        "the barrier.\n", __func__, dc->self->ptlmap.id);

                err = barrier_broadcast(dc);
                if (err < 0)
                        goto err_out;
        }
        else {
                /* Non master peer in this job. */
                dc->f_bar = 1;
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
  RPC routine  to wait for  server confirmation that it  processed our
  unregister message and all our other messages.
*/
static int dcrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);

	dc->f_reg = 0;

#ifdef DEBUG
	uloga("'%s()': unregister confirmed and complete.\n", __func__);
#endif

	return 0;
}

static int dc_register_at_master(struct dart_client *dc, int appid)
{
        struct msg_buf *msg;
        struct hdr_register *hr;
        struct node_id peer;
        int err;

        memset(&peer, 0, sizeof(peer));
        err = rpc_read_config(&peer.ptlmap.ptlid);
        if (err < 0) 
                goto err_out;

        INIT_LIST_HEAD(&peer.req_list);
        peer.num_msg_at_peer = dc->rpc_s->max_num_msg;
        peer.num_msg_ret = 0;
        peer.mb = MB_RPC_MSG;

	err = -ENOMEM;
        msg = msg_buf_alloc(dc->rpc_s, &peer, 1);
        if (!msg) 
                goto err_out;
        msg->msg_rpc->cmd = cn_register;

        hr = (struct hdr_register *) msg->msg_rpc->pad;
        hr->pm_sp = peer.ptlmap;
        hr->pm_cp.ptlid = dc->rpc_s->ptlmap.ptlid;
        hr->pm_cp.id = -1;
        hr->pm_cp.appid = appid;
        hr->num_cp = dc->cp_in_job;

        err = rpc_send(dc->rpc_s, &peer, msg);
        if (err < 0)
                goto err_out_free;

        /* 
           If we got here, rpc_send() was successfull, and msg will be
           freed in the async path by rpc_cb_req_completion().
        */

        do {
                err = rpc_process_event(dc->rpc_s);
                if (err < 0)
                        goto err_out;
        }
        while (!dc->f_reg);

        // uloga("'%s()' successfull.\n", __func__);

        return 0;
 err_out_free:
        free(msg);
 err_out:
	ERROR_TRACE();
}

static int dc_unregister(struct dart_client *dc)
{
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct node_id *peer;
        int sp_index, err = -ENOMEM;

        sp_index = dc->self->ptlmap.id % dc->num_sp;
        peer = dc_get_peer(dc, sp_index);
        msg = msg_buf_alloc(dc->rpc_s, peer, 1);
        if (!msg) 
                goto err_out;

        msg->msg_rpc->cmd = cn_unregister;
        msg->msg_rpc->id = dc->self->ptlmap.id;

        hreg = (struct hdr_register *) msg->msg_rpc->pad;
        hreg->num_sp = dc->num_sp;
	hreg->num_cp = 1;
        hreg->pm_cp = dc->self->ptlmap;
        hreg->pm_sp = peer->ptlmap;

        err = rpc_send(dc->rpc_s, peer, msg);
        if (err < 0) 
                goto err_out_free;

	/* Should wait here for 'unregister' confirmation. */
	while (dc->f_reg) {
		err = rpc_process_event(dc->rpc_s);
		if (err < 0)
			goto err_out;
	}

        /* TODO: Why this loop here !?
        int i = 0;
        while (i++ < dc->rpc_s->max_num_msg) 
                rpc_process_event(dc->rpc_s);
	*/
        return 0;
 err_out_free:
        free(msg);
 err_out:
        uloga("'%s()': failed.\n", __func__);
        return err;
}

static int data_transfer_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);

        dc->num_posted--;

        free(msg->msg_data);
        free(msg);

        // uloga("'%s()': transfer complete.\n", __func__);
        return 0;
}

/*
  Public API starts here.
*/

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm)
{
        struct dart_client *dc;
	//        size_t size;
        int err;

        // printf("sizeof struct rpc_cmd = %d.\n", sizeof(struct rpc_cmd));

        dc = calloc(1, sizeof(*dc));
        if (!dc)
                return NULL;
        dc->dart_ref = dart_ref;
        dc->cp_in_job = num_peers;
        // dc->peer_tab = (struct node_id *) (dc+1);
        // dc->peer_size = num_peers;

        rpc_add_service(cn_register, dcrpc_register);
        rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);

        dc->rpc_s = rpc_server_init(10, num_peers, dc, DART_CLIENT);
        if (!dc->rpc_s) {
                free(dc);
                return NULL;
        }

        err = dc_register_at_master(dc, appid);
        if (err < 0) {
                rpc_server_free(dc->rpc_s);
                free(dc);
                return NULL;
        }

        return dc;
}

#if 0 // TODO: remove this
int dc_barrier(struct dart_client *dc)
{
        struct msg_buf *msg;
        struct node_id *peer;
        int err = -ENOMEM;

        dc->f_bar = 0;

        if (dc->self->id != dc->cp_min_rank) {
                peer = dc_get_peer(dc, dc->cp_min_rank);

                msg = msg_buf_alloc(dc->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_rpc->cmd = cp_barrier;
                msg->msg_rpc->id = dc->self->id;

                err = rpc_send(dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        goto err_out;
                }
        }
        else {
                dc->cp_barrier_req++;
                if (dc->cp_barrier_req == dc->cp_in_job)
                        barrier_broadcast(dc);
        }

        while (dc->f_bar == 0) {
                err = rpc_process_event(dc->rpc_s);
                if (err < 0)
                        goto err_out;
        }
        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}
#endif

void dc_free(struct dart_client *dc)
{
#ifdef DEBUG
        uloga("'%s()': num_posted = %d.\n", __func__, dc->num_posted);
#endif
        /* TODO: is this ending condition correct ? */
        while (dc->num_posted)
                rpc_process_event(dc->rpc_s);

        dc_unregister(dc);

        rpc_server_free(dc->rpc_s);
        if (dc->peer_tab)
                free(dc->peer_tab);
        free(dc);
}

int dc_process(struct dart_client *dc)
{
        return rpc_process_event(dc->rpc_s);
}

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
}
