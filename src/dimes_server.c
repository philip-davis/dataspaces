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
*  Fan Zhang (2012)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"

#include "dimes_server.h"
#include "dart.h"
#include "ds_gspace.h"
#include "ss_data.h"
#include "timer.h"

static struct dimes_server *dimes_s = NULL;

#define DIMES_SID dimes_s->dsg->ds->self->ptlmap.id

static int dsgrpc_dimes_ss_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct msg_buf *msg;
	struct node_id *peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);
	struct hdr_ss_info *hsi;
	int err = -ENOMEM;

	msg = msg_buf_alloc(rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = dimes_ss_info_msg;
	msg->msg_rpc->id = DIMES_SID;
	
	hsi = (struct hdr_ss_info*) msg->msg_rpc->pad;
	hsi->num_dims = dimes_s->dsg->ssd->dht->bb_glb_domain.num_dims;
	hsi->val_dims[bb_x] = dimes_s->dsg->ssd->dht->bb_glb_domain.ub.c[0] + 1;
	hsi->val_dims[bb_y] = dimes_s->dsg->ssd->dht->bb_glb_domain.ub.c[1] + 1;
	hsi->val_dims[bb_z] = dimes_s->dsg->ssd->dht->bb_glb_domain.ub.c[2] + 1;
	hsi->num_space_srv = dimes_s->dsg->ds->size_sp;

	err = rpc_send(rpc_s, peer, msg);
	if (err == 0)
		return 0;
err_out:
	ERROR_TRACE();
}

static int dsgrpc_dimes_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        int err;
        struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)cmd->pad;
        struct obj_descriptor *odsc = &hdr->odsc;
        odsc->owner = cmd->id; // Necessary! peer did not assign this info.

        if (hdr->has_rdma_data) {
                cmd_s_add(dimes_s->cmd_store, cmd);
        }

#ifdef DEBUG	
        uloga("%s(): get cmd from peer #%d, "
	      "with hdr->has_rdma_data=%d, "
	      "odsc->name=%s, odsc->owner=%d, data_size=%u.\n",
	      	__func__, cmd->id, hdr->has_rdma_data,
		odsc->name, odsc->owner, obj_data_size(odsc));
#endif

	return 0;
}

static int get_desc_completion_server(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);
        return 0;
}

static int dsgrpc_dimes_get_desc(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_dimes_obj_get *hdr = (struct hdr_dimes_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);
        struct msg_buf *msg;
        int err = -ENOMEM;
        int i = 0, num_cmd = 0, qid;
        struct cmd_data *cmd_data, *tmp;
        struct rpc_cmd *tab;
        struct list_head cmd_list;
        INIT_LIST_HEAD(&cmd_list);

        /* search in the rpc_cmd storage */
        err = cmd_s_find_all(dimes_s->cmd_store, &hdr->u.o.odsc, &cmd_list, &num_cmd);
        if (err < 0) // TODO(fan): What does the return value mean?
                goto err_out;

        // Send back the cmd table if there is any entries found.
        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                goto err_out;
        }
        qid = hdr->qid;
        hdr = (struct hdr_dimes_obj_get *) msg->msg_rpc->pad;
        hdr->u.o.num_de = num_cmd;
        hdr->qid = qid;

        if (num_cmd > 0) {
                tab = malloc(sizeof(struct rpc_cmd) * num_cmd);
                if (!tab)
                        goto err_out;

                i = 0;
                list_for_each_entry_safe(cmd_data,tmp,&cmd_list,struct cmd_data, cmd_entry) {
                        list_del(&cmd_data->cmd_entry);
                        tab[i++] = cmd_data->cmd; //copy
                        free(cmd_data);
                }

                msg->size = sizeof(struct rpc_cmd) * num_cmd;
                msg->msg_data = tab;
                msg->cb = get_desc_completion_server;

                hdr->rc = 0;
        } else {
                hdr->rc = -1;
        }

        msg->msg_rpc->cmd = dimes_get_desc_msg;
        msg->msg_rpc->id = DIMES_SID;

        err = rpc_send(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(tab);
        free(msg);
err_out:
        ERROR_TRACE();
}

/*
  Public API starts here.
*/
struct dimes_server *dimes_server_alloc(int num_sp, int num_cp, char *conf_name)
{
	struct dimes_server *dimes_s_l;
	int err = -ENOMEM;

	if (dimes_s)
		return dimes_s;

	dimes_s_l = calloc(1, sizeof(*dimes_s_l));
	dimes_s_l->dsg = dsg_alloc(num_sp, num_cp, conf_name);
	if (!dimes_s_l->dsg) {
		free(dimes_s_l);	
		goto err_out;
	}

	rpc_add_service(dimes_put_msg, dsgrpc_dimes_put);
	rpc_add_service(dimes_get_desc_msg, dsgrpc_dimes_get_desc);
	rpc_add_service(dimes_ss_info_msg, dsgrpc_dimes_ss_info);
	
	dimes_s_l->cmd_store = cmd_s_alloc(dimes_s_l->dsg->ssd->storage->size_hash);
	if (!dimes_s_l->cmd_store) {
		free(dimes_s_l->dsg);
		free(dimes_s_l);
		goto err_out;
	}

	dimes_s = dimes_s_l;

	uloga("%s(): #%d complete!\n", __func__, DIMES_SID);
	return dimes_s;
err_out:
	uloga("%s(): failed with %d.\n", __func__, err);
	dimes_s = NULL;
	return NULL;	
}

void dimes_server_free(struct dimes_server *dimes_s_l)
{
	dsg_free(dimes_s_l->dsg);
	cmd_s_free(dimes_s_l->cmd_store);
	free(dimes_s_l);
}

