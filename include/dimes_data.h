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
 * Fan Zhang (2012) TASSL Rutgers University
 * zhangfan@cac.rutgers.edu
 */
#ifndef __DIMES_DATA_H__
#define __DIMES_DATA_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "dart.h"
#include "ss_data.h"

#ifdef DS_HAVE_DIMES_SHMEM
#include "mpi.h"
#define PAGE_SIZE sysconf(_SC_PAGE_SIZE)
#define MAX_NUM_PEER_PER_NODE 64
#define SHMEM_OBJ_PATH_PREFIX "/dataspaces_shared_memory_segment"
#define SHMEM_OBJ_PATH_MAX_LEN 255
#endif

struct ptlid_map;

struct box_2pointers{
	void *ptr1;
	void *ptr2;
};

#ifdef DS_HAVE_DIMES_SHMEM
struct shared_memory_obj {
    struct list_head    entry;
    int id;
    int owner_node_rank;
    char path[SHMEM_OBJ_PATH_MAX_LEN+1];
    size_t size;
    void *ptr;
    int fd;
};
struct dimes_shmem_descriptor {
    size_t size;
    size_t offset;
    int shmem_obj_id;
    int owner_node_rank;
} __attribute__((__packed__));

// Checkpoint/Restart (CR) of DIMES:
// (1) node-local shared memory object information
// (2) (client) in-memory objects information
// (3) (client) memory allocator information 
struct dimes_cr_shmem_obj_info
{
    int id;
    size_t size;
    size_t storage_restart_buf_size;
    size_t allocator_restart_buf_size;
    char path[SHMEM_OBJ_PATH_MAX_LEN+1];
    char path_storage_restart_buf[SHMEM_OBJ_PATH_MAX_LEN+1];
    char path_allocator_restart_buf[SHMEM_OBJ_PATH_MAX_LEN+1];
} __attribute__((__packed__));

struct dimes_cr_shmem_info
{
    uint32_t num_shmem_obj;
} __attribute__((__packed__));

struct dimes_cr_storage_info
{
     uint32_t num_group;
     int shmem_obj_id;
} __attribute__((__packed__));

struct dimes_cr_group_info
{
     char name[256];
     uint32_t num_obj;
} __attribute__((__packed__));

struct dimes_cr_mem_obj_info
{
    struct obj_descriptor obj_desc;
    struct global_dimension gdim;
    size_t size;
    size_t offset;
    unsigned char mem_type;
} __attribute__((__packed__));

struct dimes_cr_allocator_info
{
     uint32_t num_used_blocks;
     uint32_t num_free_blocks;
     int shmem_obj_id;
} __attribute__((__packed__));

struct dimes_cr_allocator_block_info
{
     size_t size;
     size_t offset;
     uint32_t block_status;
} __attribute__((__packed__));
#endif

struct dimes_obj_id {
    int dart_id;
    uint32_t local_obj_index; 
};
int equal_dimes_obj_id(const struct dimes_obj_id *oid1, const struct dimes_obj_id *oid2);

struct dimes_memory_obj {
    struct list_head entry;
    struct dimes_obj_id obj_id;
    struct obj_descriptor obj_desc;
#ifdef DS_HAVE_DIMES_SHMEM
    struct dimes_shmem_descriptor shmem_desc;
#endif
    struct global_dimension gdim;
    struct dart_rdma_mem_handle rdma_handle;
};

#define STORAGE_GROUP_NAME_MAXLEN 256
struct dimes_storage_group {
    struct list_head entry;
    char name[STORAGE_GROUP_NAME_MAXLEN+1];
    struct list_head *version_tab;
};

struct obj_data_wrapper {
	struct list_head obj_entry;
	struct obj_data *od;
	struct rpc_cmd cmd;
	int q_id;
};

// Header structure for dimes_put request.
struct hdr_dimes_put { // TODO: more comments
    struct ptlid_map ptlmap;
	struct dimes_obj_id obj_id;
	struct obj_descriptor odsc;
#ifdef DS_HAVE_DIMES_SHMEM
    unsigned char has_shmem_data;
    struct dimes_shmem_descriptor shmem_desc;
#endif
} __attribute__((__packed__));

// Header structure
struct hdr_dimes_get {
	int qid;
	int rc;
    int num_obj;
	struct obj_descriptor odsc;
} __attribute__((__packed__));

struct obj_location_wrapper {
    struct list_head entry;
    struct rpc_cmd *data_ref;
};

struct obj_location_list_node {
    struct list_head entry;
    struct rpc_cmd cmd; 
};

struct var_list_node {
    struct list_head entry;
    char name[256];
    struct list_head obj_location_list;    
};

struct metadata_storage {
    int max_versions;
    struct list_head version_tab[1];
};

struct metadata_storage *metadata_s_alloc(int max_versions);
int metadata_s_free(struct metadata_storage *s);
int metadata_s_add_obj_location(struct metadata_storage *s,
                                struct rpc_cmd *cmd);
int metadata_s_find_obj_location(struct metadata_storage *s,
                                struct obj_descriptor *odsc,
                                struct list_head *out_list, int *out_num_items);


struct ss_storage * dimes_ls_alloc(int);
int dimes_ls_free(struct ss_storage *);
struct obj_data_wrapper* dimes_ls_find(struct ss_storage *,struct obj_descriptor *);
struct obj_data_wrapper* dimes_ls_find_no_version(struct ss_storage *,struct obj_descriptor *);
void dimes_ls_add_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w);
void dimes_ls_remove_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w);
int dimes_ls_count_obj(struct ss_storage *ls, int query_tran_id);


// Init DIMES Buffer
int dimes_buffer_init(uint64_t base_addr, size_t size);
// Finalize DIMES Buffer
int dimes_buffer_finalize(void);
size_t dimes_buffer_total_size(void);
// Allocates a block of memory from DIMES Buffer, returning a pointer to
// the beginning of the block.
void dimes_buffer_alloc(size_t , uint64_t *);
// Free a memory block previously allocated from DIMES Buffer 
void dimes_buffer_free(uint64_t addr);

#ifdef DS_HAVE_DIMES_SHMEM
size_t estimate_allocator_restart_buf_size(int dart_id);

int dimes_client_shmem_checkpoint_allocator(int shmem_obj_id, void *restart_buf);
int dimes_client_shmem_restart_allocator(void *restart_buf, int dart_id);
#endif

#ifdef __cplusplus
}
#endif

#endif
