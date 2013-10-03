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
*  Fan Zhang (2011) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/

#ifndef __DART_RDMA_DCMF_H__
#define __DART_RDMA_DCMF_H__

#ifdef DS_HAVE_DIMES
#include "dart_rpc_dcmf.h"

struct dart_rdma_mem_handle {
	DCMF_Memregion_t memregion;
	void *base_addr; // Only used for local data copy
	size_t size;
};

struct dart_rdma_read_op {
	struct list_head entry;
	int tran_id;
	size_t src_offset;
	size_t dst_offset;
	size_t bytes;
	int ret;
};

struct dart_rdma_read_tran {
	struct list_head entry;
	struct list_head read_ops_list;
	int tran_id;
	struct node_id *remote_peer;
	struct dart_rdma_mem_handle src;
	struct dart_rdma_mem_handle dst;
};

struct dart_rdma_handle {
	struct rpc_server *rpc_s;
	DCMF_Protocol_t dcmf_get_protocol;
	struct list_head read_tran_list;
};

int dart_rdma_init(struct rpc_server *rpc_s);
int dart_rdma_finalize();

int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl,
                           void *data, size_t bytes);
int dart_rdma_deregister_mem(struct dart_rdma_mem_handle *mem_hndl);

int dart_rdma_set_memregion_to_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                   struct rpc_cmd *cmd);
int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                   struct rpc_cmd *cmd);

// int dart_rdma_gen_read_tran_id();
int dart_rdma_create_read_tran(struct node_id *remote_peer,
                struct dart_rdma_read_tran **pp);
int dart_rdma_delete_read_tran(int tran_id);

int dart_rdma_schedule_read(int tran_id, size_t src_offset, size_t dst_offset,
                size_t bytes);
int dart_rdma_perform_reads(int tran_id);
int dart_rdma_process_reads();
int dart_rdma_check_reads(int tran_id);

#endif

#endif
