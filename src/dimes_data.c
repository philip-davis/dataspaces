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
#include <errno.h>

#include "dimes_data.h"
#include "debug.h"
#include "ss_data.h"

////////////////////////////////////////////////////////////////////////

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
					 struct cmd_data, cmd_entry){
			list_del(&cmd->cmd_entry);			
			free(cmd);
		}
	}	

	return 0;
}

int cmd_s_find_all(struct cmd_storage *s, struct obj_descriptor *odsc,
		struct list_head *out_list, int *out_num_cmd)
{
	struct cmd_data *cmd, *new;
	struct list_head *list;
	int index;

	if (s->size_hash <= 0) {
		uloga("%s(): cmd_storage not init.\n", __func__);
		return -1;
	}

	index = odsc->version % s->size_hash;
	list = &s->cmd_hash[index];
		
	*out_num_cmd = 0;
	list_for_each_entry(cmd, list, struct cmd_data, cmd_entry) {
		struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)
					(cmd->cmd.pad);
		if (obj_desc_equals_intersect(odsc, &hdr->odsc)) {
			new = malloc(sizeof(struct cmd_data));
			new->cmd = cmd->cmd; // Important;
			list_add(&new->cmd_entry, out_list);
			(*out_num_cmd)++;
		}
	}

	return 0;
}

struct cmd_data *
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

	list_for_each_entry(cmd, list, struct cmd_data, cmd_entry) {
		struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)
					(cmd->cmd.pad);
		if (obj_desc_by_name_intersect(odsc, &hdr->odsc))
			return cmd;
	}

	return NULL;
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
		// uloga("%s(): has duplicate cmd?\n", __func__);
		list_del(&existing->cmd_entry);
		s->num_cmd--;
	}

	index = odsc->version % s->size_hash;
	bin = &s->cmd_hash[index];

	struct cmd_data *new = malloc(sizeof(struct cmd_data));
	new->cmd = *cmd; //copy the rpc_cmd!!
	list_add(&new->cmd_entry, bin);
	s->num_cmd++;
}
