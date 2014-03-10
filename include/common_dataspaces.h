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
*  Hoang Bui (2012-2013) TASSL Rutgers University
*  hbui@cac.rutgers.edu
*  Fan Zhang (2013) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*
*/

#ifndef __COMMON_DATASPACES_H_
#define __COMMON_DATASPACES_H_

#include <stdint.h>

int common_dspaces_init(int num_peers, int appid);
void common_dspaces_set_storage_type (int fst);
int common_dspaces_rank(void);
int common_dspaces_peers(void);
void common_dspaces_barrier(void);
void common_dspaces_lock_on_read(const char *lock_name, void *comm);
void common_dspaces_unlock_on_read(const char *lock_name, void *comm);
void common_dspaces_lock_on_write(const char *lock_name, void *comm);
void common_dspaces_unlock_on_write(const char *lock_name,void *comm);
int common_dspaces_get (const char *var_name, 
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl, 
        uint64_t *ub, //int xu, int yu, int zu, 
        uint64_t *gdim,
        void *data);
int common_dspaces_get_versions(int **);
int common_dspaces_put (const char *var_name, 
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        uint64_t *gdim,
        void *data);

int common_dspaces_select(char *var_name, unsigned int ver,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        uint64_t *gdim,
        void *data);
int common_dspaces_cq_register(char *var_name,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        void *data);
int common_dspaces_cq_update (void);
int common_dspaces_put_sync(void);
void common_dspaces_finalize (void);

/* CCGrid'11 demo */
int common_dspaces_collect_timing(double, double *);
int common_dspaces_num_space_srv(void);

#ifdef DS_HAVE_DIMES
void common_dimes_set_storage_type(int fst);
int common_dimes_get(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        void *data);
int common_dimes_put(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        void *data);
int common_dimes_put_sync_all(void);
int common_dimes_put_set_group(const char *group_name, int step);
int common_dimes_put_unset_group();
int common_dimes_put_sync_group(const char *group_name, int step);
#endif

void common_dspaces_set_mpi_rank(int rank);
#endif
