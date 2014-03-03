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

#ifdef HAVE_UGNI
#include <pmi.h>
#include <rca_lib.h>
#endif

static int mpi_rank = 0;

#if defined(HAVE_UGNI)
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

int dspaces_init(int num_peers, int appid)
{
	return common_dspaces_init(num_peers, appid);
}

void dspaces_set_storage_type(int fst)
{
	common_dspaces_set_storage_type(fst);
}

int dspaces_rank(void)
{
	return common_dspaces_rank();
}

int dspaces_peers(void)
{
	return common_dspaces_peers();
}

int dspaces_get_num_space_peers(void)
{
	return common_dspaces_get_num_space_peers();
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

int dspaces_get(const char *var_name,
	unsigned int ver, int size,
	uint64_t xl, uint64_t yl, uint64_t zl,
	uint64_t xu, uint64_t yu, uint64_t zu,
	void *data)
{
	uint64_t lb[3] = {xl, yl, zl};
	uint64_t ub[3] = {xu, yu, zu};	
    uint64_t gdim[3] = {0, 0, 0};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu\n", 
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu);
#endif
	return common_dspaces_get(var_name, ver, size, 3, lb, ub, gdim, data);
}

int dspaces_put(const char *var_name, 
    unsigned int ver, int size,
    uint64_t xl, uint64_t yl, uint64_t zl,
    uint64_t xu, uint64_t yu, uint64_t zu,
    void *data)
{
	uint64_t lb[3] = {xl, yl, zl};
	uint64_t ub[3] = {xu, yu, zu};
    uint64_t gdim[3] = {0, 0, 0};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu\n", 
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu);
#endif 
	return common_dspaces_put(var_name, ver, size, 3, lb, ub, gdim, data);
}

int dspaces_get_with_gdim(const char *var_name,
    unsigned int ver, int size,
    uint64_t xl, uint64_t yl, uint64_t zl,
    uint64_t xu, uint64_t yu, uint64_t zu,
    uint64_t gdim_x, uint64_t gdim_y, uint64_t gdim_z,
    void *data)
{
    uint64_t lb[3] = {xl, yl, zl};
    uint64_t ub[3] = {xu, yu, zu};
    uint64_t gdim[3] = {gdim_x, gdim_y, gdim_z};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu gdim= %llu %llu %llu\n",
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu, gdim_x, gdim_y, gdim_z);
#endif
    return common_dspaces_get(var_name, ver, size, 3,
        lb, ub, gdim, data);
}

int dspaces_put_with_gdim(const char *var_name,
    unsigned int ver, int size,
    uint64_t xl, uint64_t yl, uint64_t zl,
    uint64_t xu, uint64_t yu, uint64_t zu,
    uint64_t gdim_x, uint64_t gdim_y, uint64_t gdim_z,
    void *data)
{
    uint64_t lb[3] = {xl, yl, zl};
    uint64_t ub[3] = {xu, yu, zu};
    uint64_t gdim[3] = {gdim_x, gdim_y, gdim_z};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu gdim= %llu %llu %llu\n",
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu, gdim_x, gdim_y, gdim_z);
#endif
    return common_dspaces_put(var_name, ver, size, 3,
        lb, ub, gdim, data);
}

int dspaces_put_sync(void)
{
	return common_dspaces_put_sync();
}

void dspaces_finalize(void)
{
	common_dspaces_finalize();
}

int dspaces_collect_timing(double time, double *sum_ptr)
{
	return common_dspaces_collect_timing(time, sum_ptr);
}

int dspaces_num_space_srv(void)
{
	return common_dspaces_num_space_srv();
}

#ifdef DS_HAVE_DIMES
void dimes_set_storage_type (int fst)
{
	common_dimes_set_storage_type(fst);
}

int dimes_put_sync_all(void)
{
	return common_dimes_put_sync_all();
}

int dimes_get (const char *var_name,
    unsigned int ver, int size,
    uint64_t xl, uint64_t yl, uint64_t zl,
    uint64_t xu, uint64_t yu, uint64_t zu,
    void *data)
{
    uint64_t lb[3] = {xl, yl, zl};
    uint64_t ub[3] = {xu, yu, zu};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu\n",
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu);
#endif
    return common_dimes_get(var_name, ver, size, 3, lb, ub, data);
}

int dimes_put (const char *var_name,
    unsigned int ver, int size,
    uint64_t xl, uint64_t yl, uint64_t zl,
    uint64_t xu, uint64_t yu, uint64_t zu,
    void *data)
{
    uint64_t lb[3] = {xl, yl, zl};
    uint64_t ub[3] = {xu, yu, zu};
#if defined(TIMING_PERF) && defined(HAVE_UGNI)
    int nid, pmi_rank;
    rca_mesh_coord_t xyz;
    get_topology_info(&pmi_rank, &nid, &xyz);
    uloga("%s(): pmi_rank %d mpi_rank %d nid %d coord %u %u %u var %s ver %d elem_size %d lb= %llu %llu %llu ub= %llu %llu %llu\n",
        __func__, pmi_rank, mpi_rank, nid, xyz.mesh_x, xyz.mesh_y, xyz.mesh_z,
        var_name, ver, size, xl, yl, zl, xu, yu, zu);
#endif
    return common_dimes_put(var_name, ver, size, 3, lb, ub, data);
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
#endif

void dspaces_set_mpi_rank(int rank)
{
    mpi_rank = rank;
    common_dspaces_set_mpi_rank(rank);
}
