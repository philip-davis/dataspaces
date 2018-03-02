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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "debug.h"
#include "common_dataspaces.h"
#include "config.h"

#ifdef DS_HAVE_DIMES
#include "dimes_interface.h"
#endif

#if HAVE_UGNI
#include <pmi.h>
#include <rca_lib.h>

int get_topology_info(int *out_pmi_rank, int *out_nid, 
    rca_mesh_coord_t *out_xyz)
{
    int rc;
    int rank, size;

    rc = PMI_Get_rank(&rank);
    if (rc!=PMI_SUCCESS)
        PMI_Abort(rc,"PMI_Get_rank failed");

    rc = PMI_Get_size(&size);
    if (rc!=PMI_SUCCESS)
        PMI_Abort(rc,"PMI_Get_size failed");

    int nid;
    rc = PMI_Get_nid(rank, &nid);
    if (rc!=PMI_SUCCESS)
        PMI_Abort(rc,"PMI_Get_nid failed");

    rca_get_meshcoord( (uint16_t) nid, out_xyz);
    *out_pmi_rank = rank;
    *out_nid = nid; 

    fflush(stdout);
    return 0;
}
#endif
/* 
   C interface for DataSpaces.
*/

int dspaces_init(int num_peers, int appid, void *comm, const char *parameters)
{
	return common_dspaces_init(num_peers, appid, comm, parameters);
}

int dspaces_rank(void)
{
	return common_dspaces_rank();
}

int dspaces_peers(void)
{
	return common_dspaces_peers();
}

int dspaces_get_num_space_server(void)
{
	return common_dspaces_get_num_space_server();
}

void dspaces_barrier(void)
{
	common_dspaces_barrier();
}

void dspaces_lock_on_read(const char *lock_name, void *comm)
{
	common_dspaces_lock_on_read(lock_name, comm);
}

void dspaces_unlock_on_read(const char *lock_name, void *comm)
{
	common_dspaces_unlock_on_read(lock_name, comm);
}

void dspaces_lock_on_write(const char *lock_name, void *comm)
{
	common_dspaces_lock_on_write(lock_name, comm);
}

void dspaces_unlock_on_write(const char *lock_name, void *comm)
{
	common_dspaces_unlock_on_write(lock_name, comm);
}

void dspaces_define_gdim (const char *var_name,
        int ndim, uint64_t *gdim)
{
    common_dspaces_define_gdim(var_name, ndim, gdim);
}

int dspaces_put (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        const void *data)
{
    return common_dspaces_put(var_name, ver, size, ndim, lb, ub, data);
}

int dspaces_get (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        void *data)
{
    return common_dspaces_get(var_name, ver, size, ndim, lb, ub, data);    
}

int dspaces_remove (const char *var_name,
        unsigned int ver)
{
    return common_dspaces_remove(var_name, ver);
}



int dspaces_put_sync(void)
{
	return common_dspaces_put_sync();
}

void dspaces_finalize(void)
{
	common_dspaces_finalize();
}

#ifdef DS_HAVE_DIMES
int dimes_put_sync_all(void)
{
	return common_dimes_put_sync_all();
}

void dimes_define_gdim (const char *var_name,
        int ndim, uint64_t *gdim)
{
    common_dimes_define_gdim(var_name, ndim, gdim);
}

int dimes_get (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        void *data)
{
    return common_dimes_get(var_name, ver, size, ndim, lb, ub, data);
}

int dimes_put (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        void *data)
{
    return common_dimes_put(var_name, ver, size, ndim, lb, ub, data);
}

int dimes_put_set_group(const char *group_name, int step)
{
    return common_dimes_put_set_group(group_name, step);
}

int dimes_put_unset_group()
{
    return common_dimes_put_unset_group();
}

int dimes_put_sync_group(const char *group_name, int step)
{
    return common_dimes_put_sync_group(group_name, step);
}
#ifdef DS_HAVE_DIMES_SHMEM
int dimes_shmem_init(void *comm, size_t shmem_obj_size)
{
        return common_dimes_shmem_init(comm, shmem_obj_size);
}

int dimes_shmem_finalize(unsigned int unlink)
{
        return common_dimes_shmem_finalize(unlink);
}

int dimes_shmem_checkpoint()
{
        return common_dimes_shmem_checkpoint();
}

int dimes_shmem_restart(void *comm)
{
        return common_dimes_shmem_restart(comm);
}

int dimes_shmem_clear()
{
        return common_dimes_shmem_clear();
}

int dimes_shmem_reset_server_state(int server_id)
{
        return common_dimes_shmem_reset_server_state(server_id);
}

int dimes_shmem_update_server_state()
{
        return common_dimes_shmem_update_server_state();
}

uint32_t dimes_shmem_get_nid()
{
        return common_dimes_shmem_get_nid();
}

int dimes_shmem_get_node_rank()
{
        return common_dimes_shmem_get_node_rank();
}

int dimes_shmem_put_local(const char *var_name,
                unsigned int ver, int size,
                        int ndim, uint64_t *lb, uint64_t *ub,
                                void *data)
{
        return common_dimes_shmem_put_local(var_name, ver, size, ndim, lb, ub, data);
}

int dimes_shmem_get_local(const char *var_name,
                unsigned int ver, int size,
                        int ndim, uint64_t *lb, uint64_t *ub,
                                void *data)
{
        return common_dimes_shmem_get_local(var_name, ver, size, ndim, lb, ub, data);
}
#endif
#endif

void dspaces_set_mpi_rank_hint(int rank)
{
    common_dspaces_set_mpi_rank_hint(rank);
}

void dspaces_unset_mpi_rank_hint()
{
    common_dspaces_unset_mpi_rank_hint();
}
