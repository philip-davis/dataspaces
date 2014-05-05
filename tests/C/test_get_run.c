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
*  Fan Zhang (2013)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "debug.h"
#include "ss_data.h"
#include "mpi.h"

//# of processors in x-y-z direction
static int npx_, npy_, npz_;
//block size per processor per direction
static uint64_t spx_, spy_, spz_;
//# of iterations
static int timestep_;
//# of processors in the application
static int npapp_;

static int rank_, nproc_;

static uint64_t offx_, offy_, offz_;

static struct timer timer_;

static MPI_Comm gcomm_;

static size_t elem_size_;

static char transport_type_str_[256];
/*
Matrix representation
+-----> (x)
|
|
v (y)
*/
static double* allocate_2d(uint64_t x, uint64_t y)
{
    double* tmp = NULL;
    tmp = (double*)malloc(elem_size_ * x * y);
    return tmp;
}

static void set_offset_2d(int rank, int npx, int npy, uint64_t spx, uint64_t spy)
{
    offx_ = (rank % npx) * spx;
    offy_ = (rank / npx) * spy;
}

static double* allocate_3d(uint64_t x, uint64_t y, uint64_t z)
{
    double *tmp = NULL;
    tmp = (double*)malloc(elem_size_* x * y * z);
    return tmp;
}

static void set_offset_3d(int rank, int npx, int npy, int npz, uint64_t spx, uint64_t spy, uint64_t spz)
{
    offx_ = (rank % npx) * spx;
    offy_ = (rank / npx % npy) * spy;
    offz_ = (rank / npx / npy) * spz;
}

static int couple_read_2d(unsigned int ts, int ndim, int num_vars, enum transport_type type)
{
    double **data_tab = (double **)malloc(sizeof(double*) * num_vars);
    char var_name[128];
    int i;
    for (i = 0; i < num_vars; i++) {
        data_tab[i] = NULL;
    }

	common_lock_on_read("m2d_lock", &gcomm_);

	//get data from space
    set_offset_2d(rank_, npx_, npy_, spx_, spy_);
	int elem_size = elem_size_;
	uint64_t xl = offx_;
	uint64_t yl = offy_;
	uint64_t zl = 0;
	uint64_t xu = offx_ + spx_ - 1;
	uint64_t yu = offy_ + spy_ - 1;
	uint64_t zu = 0;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m2d: {(%llu,%llu),(%llu,%llu)} from space\n",
		ts, rank_, yl,xl, yu,xu);
#endif

    // Allocate data
    double *data;
    for (i = 0; i < num_vars; i++) {
        data = allocate_2d(spx_, spy_);
        memset(data, 0, elem_size_*spx_*spy_);
        data_tab[i] = data;
    }

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);
    for (i = 0; i < num_vars; i++) {
        sprintf(var_name, "m2d_%d", i);
        // Note: change dimensions ordering to column-major: y,x
        common_get(var_name, ts, ndim, elem_size,
            yl, xl, zl, yu, xu, zu, data_tab[i], type);
    }	
	tm_end = timer_read(&timer_);
	common_unlock_on_read("m2d_lock", &gcomm_);
	
	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

#ifdef TIMING_PERF
    uloga("TIMING_PERF get_data ts %u peer %d time %lf\n",
            ts, common_rank(), tm_diff);
#endif
	if (rank_ == root) {
		uloga("TS= %u TRANSPORT_TYPE= %s read MAX time= %lf\n",
			ts, transport_type_str_, tm_max);
	}

    for (i = 0; i < num_vars; i++) {
        sprintf(var_name, "m2d_%d", i);
        check_data(var_name, data_tab[i], 
                   (spx_*spy_*elem_size_)/sizeof(double), rank_, ts);
        free(data_tab[i]);
    }
    free(data_tab);

	return 0;
}

static int couple_read_3d(unsigned int ts, int ndim, int num_vars, enum transport_type type)
{
    double **data_tab = (double **)malloc(sizeof(double*) * num_vars);
    char var_name[128];
    int i;
    for (i = 0; i < num_vars; i++) {
        data_tab[i] = NULL;
    }

	common_lock_on_read("m3d_lock", &gcomm_);

	//get data from space
    set_offset_3d(rank_, npx_, npy_, npz_, spx_, spy_, spz_);
	int elem_size = elem_size_;
	uint64_t xl = offx_;
	uint64_t yl = offy_;
	uint64_t zl = offz_;
	uint64_t xu = offx_ + spx_ - 1;
	uint64_t yu = offy_ + spy_ - 1;
	uint64_t zu = offz_ + spz_ - 1;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m3d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, zl,yl,xl, zu,yu,xu);
#endif

    // Allocate data
    double *data;
    for (i = 0; i < num_vars; i++) {
        data = allocate_3d(spx_, spy_, spz_);
        memset(data, 0, elem_size_*spx_*spy_*spz_);
        data_tab[i] = data;
    }

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);
    for (i = 0; i < num_vars; i++) {
        sprintf(var_name, "m3d_%d", i);
        // Note: change dimension ordering to column-major: z,y,x
        common_get(var_name, ts, ndim, elem_size,
            zl, yl, xl, zu, yu, xu, data_tab[i], type);
    }	
	tm_end = timer_read(&timer_);
    common_unlock_on_read("m3d_lock", &gcomm_);
	
	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

#ifdef TIMING_PERF
    uloga("TIMING_PERF get_data ts %u peer %d time %lf\n",
            ts, common_rank(), tm_diff);
#endif
	if (rank_ == root) {
		uloga("TS= %u TRANSPORT_TYPE= %s read MAX time= %lf\n",
			ts, transport_type_str_, tm_max);
	}

    for (i = 0; i < num_vars; i++) {
        sprintf(var_name, "m3d_%d", i);
        check_data(var_name, data_tab[i], 
                   (spx_*spy_*spz_*elem_size_)/sizeof(double), rank_, ts);
        free(data_tab[i]);
    }
    free(data_tab);

	return 0;
}

int test_get_run(enum transport_type type, int npapp, int npx, int npy, int npz,
    uint64_t spx, uint64_t spy, uint64_t spz, int timestep, int appid, int dims, size_t elem_size,
    int num_vars, MPI_Comm gcomm)
{
	gcomm_ = gcomm;
	elem_size_ = elem_size;
	timestep_ = timestep;
	npapp_ = npapp;
    // Note: dimensions reodering
    if (2 == dims) {
        npx_ = npy;
        npy_ = npx;
        npz_ = npz;

        spx_ = spy;
        spy_ = spx;
    }
    if (3 == dims) {
        npx_ = npz;
        npy_ = npy;
        npz_ = npx;
    
        spx_ = spz;
        spy_ = spy;
        spz_ = spx;
    }

	timer_init(&timer_, 1);
	timer_start(&timer_);
 
	int app_id = appid;
    double tm_st, tm_end;
    tm_st = timer_read(&timer_);
	common_init(npapp_, app_id);
    tm_end = timer_read(&timer_);
    common_set_storage_type(row_major, type);
    common_get_transport_type_str(type, transport_type_str_);

    MPI_Comm_rank(gcomm_, &rank_);
    MPI_Comm_size(gcomm_, &nproc_);

#ifdef TIMING_PERF
    uloga("TIMING_PERF init_dspaces peer %d time %lf\n", common_rank(), tm_end-tm_st);
#endif

    unsigned int ts;
	if (dims == 2) {
        for (ts = 1; ts <= timestep_; ts++){
            couple_read_2d(ts, dims, num_vars, type);
        }
	} else if (dims == 3) {
        for (ts = 1; ts <= timestep_; ts++) {
            couple_read_3d(ts, dims, num_vars, type);
        }
	} else {
        uloga("%s(): error dims= %d\n", __func__, dims);
    }

    if (rank_ == 0) {
        uloga("%s(): done\n", __func__);
    }

	//common_barrier();
    MPI_Barrier(gcomm_);
    int ds_rank = common_rank();
    tm_st = timer_read(&timer_);
	common_finalize();
    tm_end = timer_read(&timer_);

#ifdef TIMING_PERF
    uloga("TIMING_PERF fini_dspaces peer %d time= %lf\n", ds_rank, tm_end-tm_st);
#endif
	return 0;
}

