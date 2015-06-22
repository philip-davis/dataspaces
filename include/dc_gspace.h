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
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

#ifndef __DCG_SPACE_H_
#define __DCG_SPACE_H_

#include "ss_data.h"
#include "dart.h"
#include "mpi.h"

struct query_cache {
        struct list_head        q_list;
        int                     num_ent;
};

struct query_tran {
        struct list_head        q_list;
        int                     num_ent;
};

/* Shared space info. */
struct ss_info {
        int                     num_dims;
        int                     num_space_srv;
};

struct dcg_space {
        struct dart_client      *dc;

        /* Query cache list. */
        struct query_cache      qc;
        struct query_tran       qt;

        int                     f_ss_info;
        struct ss_info          ss_info;
        struct bbox             ss_domain;
        struct global_dimension default_gdim;

        /* List of 'struct dcg_lock' */
        struct list_head        locks_list;
        /* List of 'struct gdim_list_entry' */
        struct list_head        gdim_list;

        int                     num_pending;

        enum sspace_hash_version    hash_version;
        int    max_versions; 
        /* Version bookeeping for objects available in the space. */
        int                     num_vers;
        int                     versions[64];
};

struct dcg_space * dcg_alloc(int, int, void*);
int dcg_barrier(struct dcg_space *);
void dcg_free(struct dcg_space *);
int dcg_obj_put(struct obj_data *);
int dcg_obj_get(struct obj_data *);
int dcg_get_versions(int **);
int dcg_obj_filter(struct obj_data *);
int dcg_obj_cq_register(struct obj_data *);
int dcg_obj_cq_update(int);
int dcg_obj_sync(int);

int dcg_lock_on_read(const char *, void *comm);
int dcg_unlock_on_read(const char *, void *comm);
int dcg_lock_on_write(const char *, void *comm);
int dcg_unlock_on_write(const char *, void *comm);

int dcg_remove(const char *var_name, unsigned int ver);


int dcghlp_get_id(struct dcg_space *);
int dcg_get_rank(struct dcg_space *);
int dcg_get_num_peers(struct dcg_space *);
int dcg_get_num_servers(struct dcg_space *);
int dcg_get_num_space_peers(struct dcg_space *);
int dcg_ss_info(struct dcg_space *, int *);

int dcg_time_log(double [], int);

int dcg_code_send(const void *, /* int, int,*/ struct obj_data *);

int dcg_collect_timing(double, double *);
int dcg_get_num_space_srv(struct dcg_space *);

void dcg_set_mpi_rank_hint(int rank);
void dcg_unset_mpi_rank_hint();
#endif /* __DCG_SPACE_H_ */
