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
#ifndef __DIMES_CLIENT_H__
#define __DIMES_CLIENT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "debug.h"
#include "dart.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"

#include "dimes_data.h"

// Status of a data fetch operation. 
enum fetch_status {
    fetch_ready = 0,
    fetch_posted,
    fetch_done
};

struct fetch_entry {
    struct list_head entry;
    struct dimes_obj_id remote_obj_id;
    struct obj_descriptor src_odsc;
    struct obj_descriptor dst_odsc;
    struct dart_rdma_tran *read_tran;
};

struct query_dht_d {
    int                     qh_size, qh_num_peer;
    int                     qh_num_req_posted;
    int                     qh_num_req_received;
    int                     *qh_peerid_tab;
};

/* 
   A query transaction serves a dimes_get request.
   This structure keeps query info to assemble the result.
*/
struct query_tran_entry_d {
    struct list_head        q_entry;

    struct obj_data         *data_ref;
    int                     q_id;
    struct obj_descriptor   q_obj;

    int                     num_fetch;
    struct list_head        fetch_list;

    struct query_dht_d        *qh;

    unsigned int    f_locate_data_complete:1,
                    f_complete:1;
};

struct query_tran_d {
	struct list_head        q_list;
	int                     num_entry;
};

// Key parameters for DIMES.
// TODO: Most of these parameters are currently set at compile time. Can we
// use dspaces_init() API's parameters list to pass their values?
struct dimes_client_option {
    int enable_pre_allocated_rdma_buffer;
    size_t pre_allocated_rdma_buffer_size;
    struct dart_rdma_mem_handle pre_allocated_rdma_handle;
    size_t rdma_buffer_size;
    size_t rdma_buffer_usage;
    int max_num_concurrent_rdma_read_op;
};

struct dimes_client {
	struct dcg_space *dcg;
	struct sspace *default_ssd;
    struct list_head sspace_list;
    struct list_head gdim_list;
	struct query_tran_d qt;
    struct list_head storage;
};

struct dimes_client* dimes_client_alloc(void *);
void dimes_client_free(void);
void dimes_client_set_storage_type (int fst);
int dimes_client_get (const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, 
        uint64_t *ub,
        void *data);
int dimes_client_put (const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data);
int dimes_client_put_sync_all(void); //TODO: rename to dimes_client_delete_all?
int dimes_client_put_set_group(const char *group_name, int step);
int dimes_client_put_unset_group();
int dimes_client_put_sync_group(const char *group_name, int step); //TODO: rename to dimes_client_delete_group?

#ifdef __cplusplus
}
#endif

#endif
