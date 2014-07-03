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

#ifdef DS_HAVE_DIMES_SHMEM
#include "mpi.h"
#define MAX_NUM_PEER_PER_NODE 64
#define PAGE_SIZE sysconf(_SC_PAGE_SIZE)
#define SHMEM_OBJ_PATH_PREFIX "/dataspaces_shared_memory_segment"
#define SHMEM_OBJ_PATH_MAX_LEN 255
#endif

struct query_tran_d {
	struct list_head        q_list;
	int                     num_ent;
};

#ifdef DS_HAVE_DIMES_SHMEM
struct shared_memory_obj {
    struct list_head    entry;
    int id;
    int owner_dart_id; 
    char path[SHMEM_OBJ_PATH_MAX_LEN+1];
    size_t size;
    void *ptr;
    int fd;    
};
#endif

struct dimes_client {
	struct dcg_space *dcg;
	struct sspace *ssd;
    struct list_head sspace_list;
    struct list_head gdim_list;
	struct bbox domain;
	struct query_tran_d qt;
	int    f_ss_info;
#ifdef DS_HAVE_DIMES_SHMEM
    struct list_head shmem_obj_list;
    struct node_id* local_peer_tab[MAX_NUM_PEER_PER_NODE];
    int num_local_peer;
    int node_master_dart_id;
    uint32_t node_id;
    MPI_Comm node_mpi_comm;
    // TODO: put struct list_head storage; into dimes_client
#endif
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
int dimes_client_put_sync_all(void);
int dimes_client_put_set_group(const char *group_name, int step);
int dimes_client_put_unset_group();
int dimes_client_put_sync_group(const char *group_name, int step);

#ifdef DS_HAVE_DIMES_SHMEM
int dimes_client_init_shmem(void *comm, size_t shmem_obj_size);
int dimes_client_finalize_shmem();
#endif

#ifdef __cplusplus
}
#endif

#endif
