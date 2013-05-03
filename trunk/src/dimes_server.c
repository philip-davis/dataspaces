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

#ifdef DS_HAVE_DIMES

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

static int get_dht_peers_completion_server(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);
        
        return 0;
}   

/*
  RPC routine to return the peer ids corresponding to DHT entries that 
  have object descriptors for the data object being queried.
*/
static int dsgrpc_dimes_get_dht_peers(struct rpc_server *rpc_s,
					struct rpc_cmd *cmd)
{
        struct hdr_dimes_obj_get *oh = (struct hdr_dimes_obj_get *) cmd->pad;
        struct node_id *peer;
        struct dht_entry *de_tab[dimes_s->dsg->ssd->dht->num_entries];
        struct msg_buf *msg;
        int *peer_id_tab, peer_num, i;
        int err = -ENOMEM;

        static int num = 1;

        peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);

        peer_num = ssd_hash(dimes_s->dsg->ssd, &oh->u.o.odsc.bb, de_tab);
        peer_id_tab = malloc(sizeof(int) * (peer_num+1));
        if (!peer_id_tab)
                goto err_out;
        for (i = 0; i < peer_num; i++)
                peer_id_tab[i] = de_tab[i]->rank;
        /* The -1 here  is a marker for the end of the array. */
        peer_id_tab[peer_num] = -1;

#ifdef DEBUG
	uloga("%s(): #%d ssd_hash return peer_num= %d for request from #%d\n",
		__func__, DIMES_SID, peer_num, cmd->id);
#endif

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(peer_id_tab);
                goto err_out;
        }

        msg->msg_data = peer_id_tab;
        msg->size = sizeof(int) * (peer_num+1);
        msg->cb = get_dht_peers_completion_server;

        rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_send_direct(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(peer_id_tab);
        free(msg);
 err_out:
        ERROR_TRACE();
}

/*
  RPC routine to update an object descriptor in the dht table.
*/
static int dsgrpc_dimes_update_dht(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
	struct hdr_dimes_obj_get *hdr = (struct hdr_dimes_obj_get*)cmd->pad;
	int err;
#ifdef DEBUG
 {
         char *str;

         asprintf(&str, "S%2d: update obj_desc '%s' ver %d from S%2d for  ",
                DIMES_SID, hdr->u.o.odsc.name, hdr->u.o.odsc.version, cmd->id);
         str = str_append(str, bbox_sprint(&hdr->u.o.odsc.bb));

         uloga("'%s()': %s\n", __func__, str);
         free(str);
 }
#endif
	// Set the location peer for the object descriptor
	hdr->u.o.odsc.owner = cmd->id;
	err = dht_add_entry(dimes_s->dsg->ssd->ent_self, &hdr->u.o.odsc);
	if (err < 0)
		goto err_out;

	return 0;
err_out:
	ERROR_TRACE();	
}

 
/*
  Update the DHT metadata with the new obj_descriptor information.
*/
static int dimes_update_dht(struct ds_gspace *dsg, struct obj_descriptor *odsc)
{
        struct dht_entry *dht_tab[dsg->ssd->dht->num_entries];
	struct hdr_dimes_obj_get *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int num_de, i, min_rank, err;
	
	/* Compute object distribution to nodes in the space */
	num_de = ssd_hash(dsg->ssd, &odsc->bb, dht_tab);
	if (num_de == 0) {
		uloga("%s(): should not happen, num_de==0?\n", __func__);
	}

	min_rank = dht_tab[0]->rank;
	/* Update the DHT entries on corresponding nodes. */
	for (i = 0; i < num_de; i++) {
		peer = ds_get_peer(dsg->ds, dht_tab[i]->rank);
		if (peer == dsg->ds->self) {
#ifdef DEBUG
                        char *str;

                        asprintf(&str, "S%2d: update obj_desc '%s' ver %d for ",
                                 DIMES_SID, odsc->name, odsc->version);
                        str = str_append(str, bbox_sprint(&odsc->bb));

                        uloga("'%s()': %s\n", __func__, str);
                        free(str);
#endif
			// Set location peer (itself) for the object descriptor
			odsc->owner = DIMES_SID;
			dht_add_entry(dsg->ssd->ent_self, odsc);
		} else {
			err = -ENOMEM;
			msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
			if (!msg)
				goto err_out;
			msg->msg_rpc->cmd = dimes_update_dht_msg;
			msg->msg_rpc->id = DIMES_SID;

			hdr = (struct hdr_dimes_obj_get *)msg->msg_rpc->pad;
			hdr->u.o.odsc = *odsc;
			hdr->rank = min_rank;	
					 
			err = rpc_send(dsg->ds->rpc_s, peer, msg);
			if (err < 0) {
				free(msg);
				goto err_out;
			}
		}
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static int dsgrpc_dimes_put_v2(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        int err;
        struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)cmd->pad;
        struct obj_descriptor *odsc = &hdr->odsc;
        odsc->owner = cmd->id; // Necessary! peer did not assign this info.

        if (hdr->has_rdma_data) {
		hdr->has_new_owner = 0;
		hdr->new_owner_id = -1;
                cmd_s_add(dimes_s->cmd_store, cmd);
        }

#ifdef DEBUG
        uloga("%s(): get cmd from peer #%d, "
	      "with hdr->has_rdma_data=%d, "
	      "name=%s, owner=%d, version=%d, data_size=%u.\n",
	      	__func__, cmd->id, hdr->has_rdma_data,
		odsc->name, odsc->owner, odsc->version, obj_data_size(odsc));
#endif

	err = dimes_update_dht(dimes_s->dsg, odsc);
	if (err == 0)
		return 0;

err_out:
	ERROR_TRACE();
}

static int dsgrpc_dimes_put_v2_1(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        int err;
        struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)cmd->pad;
        struct obj_descriptor *odsc = &hdr->odsc;
	odsc->owner = cmd->id;

        if (hdr->has_rdma_data) {
                hdr->has_new_owner = 0;
                hdr->new_owner_id = -1;
                cmd_s_add(dimes_s->cmd_store, cmd);
        }

#ifdef DEBUG
        uloga("%s(): get cmd from peer #%d, "
	      "with hdr->has_rdma_data=%d, hdr->location_peer_id= %d"
	      "name=%s, owner=%d, version=%d, data_size=%u.\n",
	      	__func__, cmd->id, hdr->has_rdma_data, hdr->location_peer_id,
		odsc->name, odsc->owner, odsc->version, obj_data_size(odsc));
#endif

	// Update the DHT table
	struct obj_descriptor obj_desc = hdr->odsc;
	obj_desc.owner = hdr->location_peer_id; 
	dht_add_entry(dimes_s->dsg->ssd->ent_self, &obj_desc);

	return 0;	
err_out:
	ERROR_TRACE();
}

static int locate_data_completion_server(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);
        return 0;
}

static int locate_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd,
		enum cmd_type reply_msg_type, int update_owner, int new_owner_id)
{
        struct hdr_dimes_obj_get *hdr = (struct hdr_dimes_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);
        struct msg_buf *msg;
        int err = -ENOMEM;
        int num_cmd = 0, qid;
        struct rpc_cmd *tab;
        struct list_head cmd_list;
        INIT_LIST_HEAD(&cmd_list);

#ifdef DEBUG
        uloga("%s(): get cmd from peer #%d, "
	      "with name=%s, owner=%d, version=%d, data_size=%u.\n",
	      	__func__, cmd->id, hdr->u.o.odsc.name, hdr->u.o.odsc.owner,
		hdr->u.o.odsc.version, obj_data_size(&hdr->u.o.odsc));
#endif

        // Search in the rpc_cmd storage
	if (update_owner) {
		err = cmd_s_find_all_with_update(dimes_s->cmd_store,
						 &hdr->u.o.odsc,
						 new_owner_id,
						 &cmd_list,
						 &num_cmd);
	} else {
		err = cmd_s_find_all(dimes_s->cmd_store, &hdr->u.o.odsc,
				     &cmd_list, &num_cmd);
	}

        if (err < 0)
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

                int i = 0;
                struct cmd_data *cmd_data, *tmp;
                // TODO(fan): remove the double copy of the cmd_data
                // use the linked list cmd_list only to store the pointer values
                list_for_each_entry_safe(cmd_data,tmp,&cmd_list,
                                         struct cmd_data, entry) {
                        list_del(&cmd_data->entry);
                        tab[i++] = cmd_data->cmd;
                        free(cmd_data);
                }

                msg->size = sizeof(struct rpc_cmd) * num_cmd;
                msg->msg_data = tab;
                msg->cb = locate_data_completion_server;

                hdr->rc = 0;
        } else {
                hdr->rc = -1;
        }

        msg->msg_rpc->cmd = reply_msg_type;
        msg->msg_rpc->id = DIMES_SID;

#ifdef DEBUG
        uloga("%s(): #%d num_cmd= %d for request from #%d\n",
                __func__, DIMES_SID, num_cmd, cmd->id);
#endif

        err = rpc_send(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(tab);
        free(msg);
err_out:
        ERROR_TRACE();
}

static int dsgrpc_dimes_locate_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	return locate_data(rpc_s, cmd, dimes_locate_data_msg, 0, -1);
}

static int dsgrpc_dimes_locate_data_v2(struct rpc_server *rpc_s,
				       struct rpc_cmd *cmd)
{
#ifdef DEBUG
	uloga("%s(): #%d get request from #%d\n", __func__,
		DIMES_SID, cmd->id);
#endif
	return locate_data(rpc_s, cmd, dimes_locate_data_v2_msg, 1, cmd->id);
}

static int dsgrpc_dimes_locate_data_v3(struct rpc_server *rpc_s,
				       struct rpc_cmd *cmd)
{
#ifdef DEBUG
	uloga("%s(): #%d get request from #%d\n", __func__,
		DIMES_SID, cmd->id);
#endif
	return locate_data(rpc_s, cmd, dimes_locate_data_v3_msg, 0, -1);
}

static int get_location_peers_completion_server(struct rpc_server *rpc_s,
						struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);
        return 0;
}

static int dsgrpc_dimes_get_location_peers(struct rpc_server *rpc_s,
					struct rpc_cmd *cmd)
{
        struct hdr_dimes_obj_get *oh = (struct hdr_dimes_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dimes_s->dsg->ds, cmd->id);
        struct obj_descriptor odsc, *odsc_tab;
        const struct obj_descriptor *podsc[dimes_s->dsg->ssd->ent_self->odsc_num];
        int num_odsc, i;
        struct msg_buf *msg;
        int err = -ENOENT;

        num_odsc = dht_find_entry_all(dimes_s->dsg->ssd->ent_self,
					&oh->u.o.odsc, podsc);
        if (!num_odsc) {
		uloga("%s(): #%d obj desc name=%s ver=%d from #%d not found.\n",
		 __func__, DIMES_SID, oh->u.o.odsc.name, oh->u.o.odsc.version,
		 cmd->id);
                goto err_out;
        }

        err = -ENOMEM;
        odsc_tab = malloc(sizeof(*odsc_tab) * num_odsc);
        if (!odsc_tab)
                goto err_out;

        for (i = 0; i < num_odsc; i++) {
                odsc = *podsc[i];
                /* Preserve storage type at the destination. */
                odsc.st = oh->u.o.odsc.st;
                bbox_intersect(&oh->u.o.odsc.bb, &odsc.bb, &odsc.bb);
                odsc_tab[i] = odsc;
        }

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(odsc_tab);
                goto err_out;
        }

        msg->size = sizeof(*odsc_tab) * num_odsc;
        msg->msg_data = odsc_tab;
        msg->cb = get_location_peers_completion_server;

        msg->msg_rpc->cmd = dimes_get_location_peers_msg;
        msg->msg_rpc->id = DIMES_SID;

        i = oh->qid;
        oh = (struct hdr_dimes_obj_get *) msg->msg_rpc->pad;
        oh->u.o.num_de = num_odsc;
        oh->qid = i;
	oh->rc = 0;

#ifdef DEBUG
	uloga("%s(): #%d dht_find_entry_all return num_odsc= %d "
	      "for request from #%d\n",
		__func__, DIMES_SID, num_odsc, cmd->id);
#endif

        err = rpc_send(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(odsc_tab);
        free(msg);
 err_out:
	ERROR_TRACE();
}

static int dsgrpc_dimes_put_v3(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_dimes_put *hdr = (struct hdr_dimes_put*)cmd->pad;
	struct obj_descriptor *odsc = &hdr->odsc;
	odsc->owner = cmd->id;
	
	if (hdr->has_rdma_data) {
		cmd_s_add(dimes_s->cmd_store, cmd);
	}

#ifdef DEBUG    
        uloga("%s(): get cmd from peer #%d, "
              "with has_rdma_data=%d, "
              "name=%s, owner=%d, version=%d, data_size=%u.\n",
                __func__, cmd->id, hdr->has_rdma_data,
                odsc->name, odsc->owner, odsc->version, obj_data_size(odsc));
#endif

	return 0;
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

	rpc_add_service(dimes_ss_info_msg, dsgrpc_dimes_ss_info);
	rpc_add_service(dimes_put_msg, dsgrpc_dimes_put);
	rpc_add_service(dimes_put_v2_msg, dsgrpc_dimes_put_v2);	
	rpc_add_service(dimes_put_v2_1_msg, dsgrpc_dimes_put_v2_1);
	rpc_add_service(dimes_put_v3_msg, dsgrpc_dimes_put_v3);
	rpc_add_service(dimes_locate_data_msg, dsgrpc_dimes_locate_data);
	rpc_add_service(dimes_locate_data_v2_msg, dsgrpc_dimes_locate_data_v2);
	rpc_add_service(dimes_locate_data_v3_msg, dsgrpc_dimes_locate_data_v3);
	rpc_add_service(dimes_update_dht_msg, dsgrpc_dimes_update_dht);
	rpc_add_service(dimes_get_dht_peers_msg, dsgrpc_dimes_get_dht_peers);
	rpc_add_service(dimes_get_location_peers_msg,
			dsgrpc_dimes_get_location_peers);

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

#endif // end of #ifdef DS_HAVE_DIMES
