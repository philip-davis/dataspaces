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


#ifndef __REXEC_H_
#define __REXEC_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct rexec_args {
	/* Input arguments. */
	void			*ptr_data_in;
	int			ni, nj, nk;	/* Sizes in each dimension. */
	int			im, jm, km;	/* Common region with the request. */
	int			iM, jM, kM;

	/* Output arguments. */
	void			*ptr_data_out;
	size_t			size_res;
	int			rc;
};

// extern struct hdr_bin_code;
// typedef void * (*bin_test_t)(struct hdr_bin_code *, void *, const struct node_id *);
typedef int (*bin_code_fn_t)(struct rexec_args *);

/* ss_data.h */
extern struct obj_descriptor;
extern struct dht_entry;
extern struct ss_storage;
extern struct obj_data;


extern int dht_find_entry_all(struct dht_entry *, struct obj_descriptor *, const struct obj_descriptor *[]);
extern struct obj_data *ls_find(struct ss_storage *, const struct obj_descriptor *);

/* ds_gspace.c */
extern struct dht_entry *dsg_dht_get_self_entry(void);
extern void dsg_dht_print_descriptors(const struct obj_descriptor *[], int);
extern struct ss_storage *dsg_dht_get_storage(void);

/* bbox.c */
extern struct bbox;
extern int bbox_dist(struct bbox *, int);

/* c_module.c - DEBUG purposes only */
// extern int sort_particles(struct rexec_args *);
// extern int max_value(struct rexec_args *);

typedef typeof(malloc)			*fn_malloc_t;
typedef typeof(memset)			*fn_memset_t;
typedef typeof(free)			*fn_free_t;
typedef typeof(printf)			*fn_printf_t;

typedef typeof(msg_buf_alloc)		*fn_msg_buf_alloc_t;
typedef typeof(rpc_server_get_id)	*fn_rpc_server_get_id_t;
typedef typeof(rpc_server_get_instance) *fn_rpc_server_get_instance_t;
typedef typeof(rpc_send)		*fn_rpc_send_t;
typedef typeof(dht_find_entry_all)	*fn_dht_find_entry_all_t;
typedef typeof(dsg_dht_get_self_entry)	*fn_dsg_dht_get_self_entry_t;
typedef typeof(dsg_dht_print_descriptors) *fn_dsg_dht_print_descriptors_t;
typedef typeof(ls_find)			*fn_ls_find_t;
typedef typeof(dsg_dht_get_storage)	*fn_dsg_dht_get_storage_t;
typedef typeof(bbox_dist)		*fn_bbox_dist_t;
// typedef typeof(sort_particles)	*fn_sort_particles_t;
// typedef typeof(max_value)		*fn_max_value_t;

#define MALLOC(size)		((fn_malloc_t) plt[fi_malloc])(size)
#define FREE(ptr)		((fn_free_t) plt[fi_free])(ptr)
#define MEMSET(ptr,ch,size)	((fn_memset_t) plt[fi_memset]) (ptr, ch, size)
#define PRINTF(fmt, ...)	((fn_printf_t) plt[fi_printf])(fmt, __VA_ARGS__)

#define MSG_BUF_ALLOC(rpc_s, peer, num_peers)			\
	((fn_msg_buf_alloc_t) plt[fi_msg_buf_alloc])(rpc_s, peer, num_peers)

#define RPC_SERVER_GET_ID()					\
	((fn_rpc_server_get_id_t) plt[fi_rpc_server_get_id])()

#define RPC_SERVER_GET_INSTANCE()				\
	((fn_rpc_server_get_instance_t) plt[fi_rpc_server_get_instance])()

#define RPC_SEND(rpc_s, peer, msg)				\
	((fn_rpc_send_t) plt[fi_rpc_send])(rpc_s, peer, msg)

#define DHT_FIND_ENTRY_ALL(de, q_odsc, odsc_tab)		\
	((fn_dht_find_entry_all_t) plt[fi_dht_find_entry_all])(de, q_odsc, odsc_tab)

#define DSG_DHT_GET_SELF_ENTRY()				\
	((fn_dsg_dht_get_self_entry_t) plt[fi_dsg_dht_get_self_entry])()

#define DSG_DHT_PRINT_DESCRIPTORS(odsc_tab, n)			\
	((fn_dsg_dht_print_descriptors_t) plt[fi_dsg_dht_print_descriptors])(odsc_tab, n)

#define LS_FIND(ls, odsc)					\
	((fn_ls_find_t) plt[fi_ls_find])(ls, odsc)

#define DSG_DHT_GET_STORAGE()					\
	((fn_dsg_dht_get_storage_t) plt[fi_dsg_dht_get_storage])()

#define BBOX_DIST(bb, dim)					\
	((fn_bbox_dist_t) plt[fi_bbox_dist])(bb, dim)

// #define SORT_PARTICLES(rargs)				\
//	((fn_sort_particles_t) plt[fi_sort_particles])(rargs)

// #define MAX_VALUE(rargs)					\
//	((fn_max_value_t) plt[fi_max_value])(rargs)

enum fn_index {
	fi_invalid = -1,
	fi_malloc = 0,
	fi_free,
	fi_memset,
	fi_printf,
	fi_msg_buf_alloc,
	fi_rpc_server_get_id,
	fi_rpc_server_get_instance,
	fi_rpc_send,
	fi_dht_find_entry_all,
	fi_dsg_dht_get_self_entry,
	fi_dsg_dht_print_descriptors,
	fi_ls_find,
	fi_dsg_dht_get_storage,
	fi_bbox_dist,
	// fi_sort_particles,
	// fi_max_value,
	fi_count
};


#define PLT_TAB void *plt[] = {					\
	[fi_malloc] = &malloc,					\
	[fi_free] = &free,					\
	[fi_memset] = &memset,					\
	[fi_printf] = &printf,					\
	[fi_msg_buf_alloc] = &msg_buf_alloc,			\
	[fi_rpc_server_get_instance] = &rpc_server_get_instance,\
	[fi_rpc_server_get_id] = &rpc_server_get_id,		\
	[fi_rpc_send] = &rpc_send,				\
	[fi_dht_find_entry_all] = &dht_find_entry_all,		\
	[fi_dsg_dht_get_self_entry] = &dsg_dht_get_self_entry,	\
	[fi_dsg_dht_print_descriptors] = &dsg_dht_print_descriptors, \
	[fi_ls_find] = &ls_find,				\
	[fi_dsg_dht_get_storage] = &dsg_dht_get_storage,	\
	[fi_bbox_dist] = &bbox_dist,				\
	[fi_count] = NULL					\
}
	// [fi_max_value] = &max_value,				\
	// [fi_sort_particles] = &sort_particles,		\

#endif
