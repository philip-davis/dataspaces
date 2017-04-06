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

#include "config.h"
#include "debug.h"
#include "dart.h"
#include "ds_gspace.h"
#include "ss_data.h"

#ifdef DS_HAVE_DIMES
#include "dimes_server.h"

static struct dimes_server *dimes_s = NULL;

#define DIMES_SID dimes_s->dsg->ds->self->ptlmap.id

static int locate_data_completion_server(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	free(msg->msg_data);
	free(msg);
	return 0;
}

static int locate_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd,
		enum cmd_type reply_msg_type)
{
	struct hdr_dimes_get *hdr = (struct hdr_dimes_get *) cmd->pad;
	struct node_id *peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);
	struct msg_buf *msg;
	int err = -ENOMEM;
	int qid;
    int num_obj = 0;
	struct rpc_cmd *tab;
	struct list_head obj_loc_wrapper_list;
	INIT_LIST_HEAD(&obj_loc_wrapper_list);

#ifdef DEBUG
	uloga("%s(): get request from peer #%d "
	"name= %s version= %d data_size= %u\n",
		__func__, cmd->id, hdr->odsc.name,
		hdr->odsc.version, obj_data_size(&hdr->odsc));
#endif

    struct hdr_dimes_get *hdr2 = hdr;

	// Search in the metadata storage
    err = metadata_s_find_obj_location(dimes_s->meta_store,
                                       &hdr->odsc,
                                       &obj_loc_wrapper_list,
                                       &num_obj);
	if (err < 0)
		goto err_out;

	// Send back the cmd table if there is any entries found.
	msg = msg_buf_alloc(rpc_s, peer, 1);
	if (!msg) {
		goto err_out;
	}
	qid = hdr->qid;
	hdr = (struct hdr_dimes_get *) msg->msg_rpc->pad;
	hdr->num_obj = num_obj;
	hdr->qid = qid;

	if (num_obj > 0) {
		tab = malloc(sizeof(struct rpc_cmd) * num_obj);
		if (!tab)
			goto err_out;

		int i = 0;
        struct obj_location_wrapper *obj_loc_w, *tmp;
		list_for_each_entry_safe(obj_loc_w, tmp, &obj_loc_wrapper_list,
								 struct obj_location_wrapper, entry) {
            list_del(&obj_loc_w->entry);
			tab[i++] = *(obj_loc_w->data_ref);
			free(obj_loc_w);
		}

		msg->size = sizeof(struct rpc_cmd) * num_obj;
		msg->msg_data = tab;
		msg->cb = locate_data_completion_server;

		hdr->rc = 0;
	} else {
		hdr->rc = -1;
        uloga("%s: WARNING #%d num_obj= %d for request from #%d.\n",
            __func__, DIMES_SID, num_obj, cmd->id);
	}

	msg->msg_rpc->cmd = reply_msg_type;
	msg->msg_rpc->id = DIMES_SID;

#ifdef DEBUG
	uloga("%s(): #%d num_obj= %d for request from #%d\n",
		__func__, DIMES_SID, num_obj, cmd->id);
#endif

	err = rpc_send(rpc_s, peer, msg);
    //uloga("%s(): %lf name=%s version=%d num_obj=%d process_time %lf\n",
    //    __func__, t1, hdr2->odsc.name, hdr2->odsc.version, num_obj,
    //    t2-t1);

	if (err == 0)
		return 0;

	free(tab);
	free(msg);
err_out:
	ERROR_TRACE();
}

static int dsgrpc_dimes_locate_data(struct rpc_server *rpc_s,
				       struct rpc_cmd *cmd)
{
	return locate_data(rpc_s, cmd, dimes_locate_data_msg);
}

static int dsgrpc_dimes_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_dimes_put *hdr = (struct hdr_dimes_put*)cmd->pad;
	struct obj_descriptor *odsc = &hdr->odsc;
	// odsc->owner = cmd->id;
	
    metadata_s_add_obj_location(dimes_s->meta_store, cmd);

#ifdef DEBUG    
	uloga("%s(): get request from peer #%d "
           "name= %s version= %d data_size= %u\n",
            __func__, cmd->id, odsc->name, odsc->version, obj_data_size(odsc));
#ifdef DS_HAVE_DIMES_SHMEM
    if (hdr->has_shmem_data) {
        uloga("%s(): #%d get update from peer #%d shmem_desc: size= %u "
            "offset= %u shmem_obj_id= %d owner_node_rank= %d\n",
            __func__, DIMES_SID, cmd->id, hdr->shmem_desc.size,
            hdr->shmem_desc.offset, hdr->shmem_desc.shmem_obj_id,
            hdr->shmem_desc.owner_node_rank);
    }
#endif
#endif
    
    //uloga("%s(): %lf name=%s version=%d process_time %lf from peer %d\n", __func__,
    //        t1, odsc->name, odsc->version, t2-t1, cmd->id);
	return 0;
}

#ifdef DS_HAVE_DIMES_SHMEM
static int dsgrpc_dimes_shmem_reset_server(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    metadata_s_free(dimes_s->meta_store);
    dimes_s->meta_store = metadata_s_alloc(dimes_s->dsg->ls->size_hash);
    
    return 0;
}

static int dsgrpc_dimes_shmem_update_server(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_dimes_put *hdr = (struct hdr_dimes_put*)cmd->pad;
    metadata_s_add_obj_location(dimes_s->meta_store, cmd);
    return 0;
}
#endif

/*
  Public API starts here.
*/
struct dimes_server *dimes_server_alloc(int num_sp, int num_cp, char *conf_name, void *comm)
{
	struct dimes_server *dimes_s_l;
	int err = -ENOMEM;

	if (dimes_s)
		return dimes_s;

	dimes_s_l = calloc(1, sizeof(*dimes_s_l));
	dimes_s_l->dsg = dsg_alloc(num_sp, num_cp, conf_name, comm);
	if (!dimes_s_l->dsg) {
		free(dimes_s_l);	
		goto err_out;
	}

	rpc_add_service(dimes_put_msg, dsgrpc_dimes_put);
	rpc_add_service(dimes_locate_data_msg, dsgrpc_dimes_locate_data);
#ifdef DS_HAVE_DIMES_SHMEM
    rpc_add_service(dimes_shmem_reset_server_msg, dsgrpc_dimes_shmem_reset_server);
    rpc_add_service(dimes_shmem_update_server_msg, dsgrpc_dimes_shmem_update_server);
#endif

	dimes_s_l->meta_store =
        metadata_s_alloc(dimes_s_l->dsg->ls->size_hash);
	if (!dimes_s_l->meta_store) {
		free(dimes_s_l->dsg);
		free(dimes_s_l);
		goto err_out;
	}

	dimes_s = dimes_s_l;

#ifdef DEBUG
	uloga("%s(): #%d complete!\n", __func__, DIMES_SID);
#endif
	return dimes_s;
err_out:
	uloga("%s(): failed with %d.\n", __func__, err);
	dimes_s = NULL;
	return NULL;	
}

void dimes_server_free(struct dimes_server *dimes_s_l)
{
	dsg_free(dimes_s_l->dsg);
	metadata_s_free(dimes_s_l->meta_store);
	free(dimes_s_l);
}

int dimes_server_process(struct dimes_server *dimes_s_l)
{
    return dsg_process(dimes_s_l->dsg);
}

int dimes_server_complete(struct dimes_server *dimes_s_l)
{
    return dsg_complete(dimes_s_l->dsg);
}

int dimes_server_barrier(struct dimes_server *dimes_s_l)
{
    return dsg_barrier(dimes_s_l->dsg);
}

#endif // end of #ifdef DS_HAVE_DIMES
