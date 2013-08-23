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
#include <errno.h>

#include "dimes_data.h"
#include "debug.h"
#include "ss_data.h"

////////////////////////////////////////////////////////////////////////
static struct cmd_data *
cmd_s_find_no_version(struct cmd_storage *s, struct obj_descriptor *odsc)
{
	struct cmd_data *cmd;
	struct list_head *list;
	int index;

	if (s->size_hash <= 0 ) {
		uloga("%s(): cmd_storage not init.\n", __func__);
		return NULL;
	}

	index = odsc->version % s->size_hash;
	list = &s->cmd_hash[index];

	list_for_each_entry(cmd, list, struct cmd_data, entry) {
		struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)
														(cmd->cmd.pad);
		if (obj_desc_by_name_intersect(odsc, &hdr->odsc))
			return cmd;
	}

	return NULL;
}


struct cmd_storage *cmd_s_alloc(int max_versions)
{
	struct cmd_storage *cmd_s = 0;
	int i;
	
	cmd_s = malloc(sizeof(*cmd_s) + sizeof(struct list_head)*max_versions);
	if (!cmd_s) {
		errno = ENOMEM;
		return cmd_s;
	}

	memset(cmd_s, 0, sizeof(*cmd_s));
	for (i=0; i<max_versions; i++)
		INIT_LIST_HEAD(&cmd_s->cmd_hash[i]);
	cmd_s->size_hash = max_versions;

	return cmd_s;
}


int cmd_s_free(struct cmd_storage *s)
{
	int max_versions = s->size_hash;
	int i;
	
	for(i=0; i<max_versions; i++){
		struct cmd_data *cmd, *tmp;
		list_for_each_entry_safe(cmd,tmp,&s->cmd_hash[i],
					 struct cmd_data, entry){
			list_del(&cmd->entry);			
			free(cmd);
		}
	}	

	return 0;
}

int cmd_s_find_all_with_update(struct cmd_storage *s,
		struct obj_descriptor *odsc,
		int peer_id,
		struct list_head *out_list,
		int *out_num_cmd) {
	struct cmd_data *cmd;
	struct list_head *list;
	int index;

	if (s->size_hash <= 0) {
		uloga("%s(): cmd_storage not init.\n", __func__);
		return -1;
	}

	index = odsc->version % s->size_hash;
	list = &s->cmd_hash[index];

	*out_num_cmd = 0;
	list_for_each_entry(cmd, list, struct cmd_data, entry) {
		struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)
														(cmd->cmd.pad);
		if (obj_desc_equals_intersect(odsc, &hdr->odsc)) {
			if (!hdr->has_new_owner) {
				// Assign the new owner for the rdma buffer
				hdr->has_new_owner = 1;
				hdr->new_owner_id = peer_id;
			}

			struct cmd_data *tmp_cmd =
					malloc(sizeof(struct cmd_data));
			memcpy(tmp_cmd, cmd, sizeof(struct cmd_data));
			list_add(&tmp_cmd->entry, out_list);
			(*out_num_cmd)++;
		}
	}

	return 0;
}

int cmd_s_find_all(struct cmd_storage *s, struct obj_descriptor *odsc,
		struct list_head *out_list, int *out_num_cmd)
{
	struct cmd_data *cmd;
	struct list_head *list;
	int index;

	if (s->size_hash <= 0) {
		uloga("%s(): cmd_storage not init.\n", __func__);
		return -1;
	}

	index = odsc->version % s->size_hash;
	list = &s->cmd_hash[index];
		
	*out_num_cmd = 0;
	list_for_each_entry(cmd, list, struct cmd_data, entry) {
		struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)
					(cmd->cmd.pad);
		if (obj_desc_equals_intersect(odsc, &hdr->odsc)) {
			struct cmd_data *tmp_cmd =
					malloc(sizeof(struct cmd_data));
			memcpy(tmp_cmd, cmd, sizeof(struct cmd_data));
			list_add(&tmp_cmd->entry, out_list);
			(*out_num_cmd)++;
		}
	}

	return 0;
}

void cmd_s_add(struct cmd_storage *s, struct rpc_cmd *cmd)
{
	int index;
	struct list_head *bin;
	struct cmd_data *existing;
	struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)cmd->pad;
	struct obj_descriptor *odsc = &hdr->odsc;
	
	if (s->size_hash <= 0) {
		uloga("%s(): cmd_storage not initialized.\n",__func__);
		return;
	}

	existing = cmd_s_find_no_version(s, odsc);
	if (existing) {
		// Remove the duplicate item (if any).
		list_del(&existing->entry);
		s->num_cmd--;
	}

	index = odsc->version % s->size_hash;
	bin = &s->cmd_hash[index];

	struct cmd_data *new = calloc(1, sizeof(struct cmd_data));
	new->cmd = *cmd; //copy the rpc_cmd!!
	list_add(&new->entry, bin);
	s->num_cmd++;
}

/*
  Allocate and init the local storage structure.
*/
struct ss_storage *dimes_ls_alloc(int max_versions)
{
	struct ss_storage *ls = 0;
	int i;

	ls = malloc(sizeof(*ls) + sizeof(struct list_head) * max_versions);
	if (!ls) {
		errno = ENOMEM;
		return ls;
	}

	memset(ls, 0, sizeof(*ls));
	for (i = 0; i < max_versions; i++)
		INIT_LIST_HEAD(&ls->obj_hash[i]);
	ls->size_hash = max_versions;

	return ls;
}

/*
  Free the memory of stored data objects 
*/
int dimes_ls_free(struct ss_storage *ls)
{
	int max_versions = ls->size_hash;
	int i;

	for ( i = 0; i < max_versions; i++) {
		struct obj_data_wrapper *od_w, *tmp;
		list_for_each_entry_safe(od_w, tmp, &ls->obj_hash[i],
				struct obj_data_wrapper, obj_entry){
			list_del(&od_w->obj_entry);
			obj_data_free(od_w->od);
			free(od_w);
		}
	}

	return 0;
}

/*
  Find an object in the local storage that has the same name and
  version with the object descriptor 'odsc'.
*/
struct obj_data_wrapper*
dimes_ls_find(struct ss_storage *ls, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int index;

	//obj versions start from 0 !
	index = odsc->version % ls->size_hash;
	list = &ls->obj_hash[index];

	list_for_each_entry(od_w, list, struct obj_data_wrapper, obj_entry) {
		if (obj_desc_equals_intersect(odsc, &od_w->od->obj_desc))
			return od_w;
	}

	return NULL;
}

void dimes_ls_add_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w)
{
	int index;
	struct list_head *bin;
	struct obj_data_wrapper *od_w_existing;

	od_w_existing = dimes_ls_find_no_version(ls, &od_w->od->obj_desc);
	if (od_w_existing) {
#ifdef DEBUG
		uloga("%s(): to evict data obj, name=%s, version=%u\n", 
					__func__, od_w_existing->od->obj_desc.name,
		od_w_existing->od->obj_desc.version);
#endif

		od_w_existing->od->f_free = 1;
		if (od_w_existing->od->refcnt == 0){
			dimes_ls_remove_obj(ls, od_w_existing);
			obj_data_free(od_w_existing->od);
			free(od_w_existing);
		} else {
			uloga("'%s()': object eviction delayed.\n", __func__);
		}
	}

	index = od_w->od->obj_desc.version % ls->size_hash;
	bin = &ls->obj_hash[index];

	//NOTE: new object comes first in the list.
	list_add(&od_w->obj_entry, bin);
	ls->num_obj++;
#ifdef DEBUG
	uloga("%s(): add new obj, name=%s, version=%u, ls->num_obj=%d\n",
				__func__, od_w->od->obj_desc.name, od_w->od->obj_desc.version,
	ls->num_obj);
#endif
}

/*
  Search for an object in the local storage that is mapped to the same
  bin, and that has the same  name and object descriptor, but may have
  different version.
*/
struct obj_data_wrapper *
dimes_ls_find_no_version(struct ss_storage *ls, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int index;

	index = odsc->version % ls->size_hash;
	list = &ls->obj_hash[index];

	list_for_each_entry(od_w, list, struct obj_data_wrapper, obj_entry) {
		if (obj_desc_by_name_intersect(odsc, &od_w->od->obj_desc))
		return od_w;
	}

	return NULL;
}

void dimes_ls_remove_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w)
{      
	list_del(&od_w->obj_entry);
	ls->num_obj--;
}

int dimes_ls_count_obj_no_version(struct ss_storage *ls, int query_tran_id)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int cnt = 0;
	int version;

	for (version = 0; version < ls->size_hash; version++) {
		int i = version % ls->size_hash;
		list = &ls->obj_hash[i];

		list_for_each_entry(od_w, list, struct obj_data_wrapper,
				    obj_entry) {
			if (od_w->q_id == query_tran_id)
				cnt++;
		}
	}

	return cnt;
}
#endif // end of #ifdef DS_HAVE_DIMES
