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
#include "mpi.h"

//# of processors in x-y-z direction
static int npx_, npy_, npz_;
//block size per processor per direction
static int spx_, spy_, spz_;
//# of iterations
static int timestep_;
//# of processors in the application
static int npapp_;

static int rank_, nproc_;

static int offx_, offy_, offz_;

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
// TODO(fan): split the function into 2

static double* allocate_2d(int x, int y)
{
	double* tmp = NULL;
	tmp = (double*)malloc(elem_size_ * x * y);
	return tmp;
}

static void set_offset_2d(int rank, int npx, int npy, int spx, int spy)
{
	offx_ = (rank % npx) * spx;
	offy_ = (rank / npx) * spy;
}

static int generate_2d(double *m2d, unsigned int ts, int spx, int spy)
{
    int num_double = (spx*spy*elem_size_) / sizeof(double);
	// double value = 1.0*(rank_) + 0.0001*ts;
	double value = ts;
	int i;
	for (i = 0; i < num_double; i++) {
		m2d[i] = value;
	}

	return 0;
}

static double* allocate_3d(int x, int y, int z)
{
	double *tmp = NULL;
	tmp = (double*)malloc(elem_size_* x * y * z);
	return tmp;
}

static void set_offset_3d(int rank, int npx, int npy, int npz, int spx, int spy, int spz)
{
	offx_ = (rank % npx) * spx;
	offy_ = (rank / npx % npy) * spy;
	offz_ = (rank / npx / npy) * spz;
}

static int generate_3d(double *m3d, unsigned int ts, int spx, int spy, int spz)
{
    int num_double = (spx*spy*spz*elem_size_) / sizeof(double);
	double value = ts;
	int i;
	for (i = 0; i < num_double; i++) {
		m3d[i] = value;
	}

	return 0;
}



static int generate_multi_vars_3d(void *mltvars, unsigned int ts, int spx, int spy, int spz)
{
        int nelem = spx * spy * spz;
        //int elem_size = sizeof(double) * 2; //try 2-double/elem

        int i, j;
	int nvar = elem_size_ / sizeof(double);
        for(i = 0; i < nelem; i++){
		for(j = 0; j < nvar; j++)
                	*(double*)(mltvars + i*elem_size_ + 8*j) = ts + j;
        }
        return 0;
}

//organize mutiple variables as one element for input
static int generate_multi_vars_2d(void *data, unsigned int ts, int spx, int spy)
{
	int i = 0;
	int elem_size = elem_size_;

	//double+int
	if(elem_size == sizeof(double) + sizeof(int)){
		for(i = 0; i < spx*spy; i++){
			*((double*)(data + i*elem_size)) = ts*0.1;
			*((int*)(data + i*elem_size + sizeof(double))) = ts+i;
		}
	}

	//double+float+int
	else if(elem_size == sizeof(double) + sizeof(float) + sizeof(int)){
		for(i = 0; i < spx*spy; i++){
			*((double*)(data + i*elem_size)) = ts*0.1;
			*((float*)(data + i*elem_size + sizeof(double))) = ts*1.1;
			*((int*)(data + i*elem_size + sizeof(double) + sizeof(float))) = ts+i;
		}	
	}
	
	return 0;
}

static int couple_write_2d_multi_var(unsigned int ts, int num_vars, enum transport_type type)
{
	common_lock_on_write("m2d_lock", &gcomm_);
    	if (type == USE_DIMES) {
        	common_put_sync(type);
    	}

	//put the m2d into the space
	set_offset_2d(rank_, npx_, npy_, spx_, spy_);
	int elem_size = elem_size_;
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d write m2d:{(%d,%d,%d),(%d,%d,%d)} into space\n",
		ts, rank_, xl,yl,zl,xu,yu,zu);
#endif
	
	//Allocate enough space
	void *data = NULL;
	int nelem = spx_ * spy_;
	data = malloc(nelem * elem_size);
	if (data == NULL) {
            	uloga("%s(): allocate_2d() failed.\n", __func__);
            	return -1; // TODO: free buffers
        }

	//generate data
	generate_multi_vars_2d(data, ts, spx_, spy_);

	MPI_Barrier(gcomm_);
        tm_st = timer_read(&timer_);

        common_put("m2d", ts, elem_size,
            	xl, yl, zl, xu, yu, zu, data, type);
        if (type == USE_DSPACES) {
            	common_put_sync(type);
        }
    
    	tm_end = timer_read(&timer_);

    	sleep(2);
    	common_unlock_on_write("m2d_lock", &gcomm_);

	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

	if (rank_ == root) {
		uloga("TS= %u TRANSPORT_TYPE= %s write MAX time= %lf\n",
			ts, transport_type_str_, tm_max);
	}

    	free(data);

	return 0;
}


static int couple_write_2d(unsigned int ts, int num_vars, enum transport_type type)
{
    double **data_tab = (double **)malloc(sizeof(double *) * num_vars);
    char var_name[128];
    int i;
    for (i = 0; i < num_vars; i++) {
        data_tab[i] = NULL;
    }

	common_lock_on_write("m2d_lock", &gcomm_);
    if (type == USE_DIMES) {
        common_put_sync(type);
    }

	//put the m2d into the space
	set_offset_2d(rank_, npx_, npy_, spx_, spy_);
	int elem_size = elem_size_;
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d write m2d:{(%d,%d,%d),(%d,%d,%d)} into space\n",
		ts, rank_, xl,yl,zl,xu,yu,zu);
#endif

    // Allocate data
    //double *data = NULL;
    void *data = NULL;
    for (i = 0; i < num_vars; i++) {
        data = allocate_2d(spx_, spy_); 
        //data = allocate_multi_vars(2, spx_, spy_); //2 --number of vars
        if (data == NULL) {
            uloga("%s(): allocate_2d() failed.\n", __func__);
            return -1; // TODO: free buffers
        }
        //generate_2d(data, ts, spx_, spy_);
        generate_multi_vars_2d(data, ts, spx_, spy_);
        data_tab[i] = data;
    }

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);
    for (i = 0; i < num_vars; i++) {
        //sprintf(var_name, "m2d_%d", i);
        sprintf(var_name, "m2d");
        common_put(var_name, ts, elem_size,
            xl, yl, zl, xu, yu, zu, data_tab[i], type);
        if (type == USE_DSPACES) {
            common_put_sync(type);
        }
    }
    tm_end = timer_read(&timer_);

    sleep(2);
    common_unlock_on_write("m2d_lock", &gcomm_);

	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

	if (rank_ == root) {
		uloga("TS= %u TRANSPORT_TYPE= %s write MAX time= %lf\n",
			ts, transport_type_str_, tm_max);
	}

    for (i = 0; i < num_vars; i++) {
        if (data_tab[i]) {
            free(data_tab[i]);
        }
    }
    free(data_tab);

	return 0;
}


static int couple_write_3d(unsigned int ts, int num_vars, enum transport_type type)
{
    double **data_tab = (double **)malloc(sizeof(double *) * num_vars);
    char var_name[128];
    int i;
    for (i = 0; i < num_vars; i++) {
        data_tab[i] = NULL;
    }

    common_lock_on_write("m3d_lock", &gcomm_);
    if (type == USE_DIMES) {
        common_put_sync(type);
    }

	//put the m3d into the space
	set_offset_3d(rank_, npx_, npy_, npz_, spx_, spy_, spz_);
	int elem_size = elem_size_;
	int xl = offx_;
	int yl = offy_;
	int zl = offz_;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = offz_ + spz_ - 1;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d write m3d:{(%d,%d,%d),(%d,%d,%d)} into space\n",
		ts, rank_, xl,yl,zl,xu,yu,zu);
#endif

    // Allocate data
    double *data = NULL;
    for (i = 0; i < num_vars; i++) {
        data = allocate_3d(spx_, spy_, spz_);
        if (data == NULL) {
            uloga("%s(): allocate_3d() failed.\n", __func__);
            return -1; // TODO: free buffers
        }
        //generate_3d(data, ts, spx_, spy_, spz_);
        generate_multi_vars_3d(data, ts, spx_, spy_, spz_);
        data_tab[i] = data;
    }

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);
    for (i = 0; i < num_vars; i++) {
        sprintf(var_name, "m3d_%d", i);
        common_put(var_name, ts, elem_size,
            xl, yl, zl, xu, yu, zu, data_tab[i], type);
        if (type == USE_DSPACES) {
            common_put_sync(type);
        }
    }
    tm_end = timer_read(&timer_);
    common_unlock_on_write("m3d_lock", &gcomm_);

	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

	if (rank_ == root) {
		uloga("TS= %u TRANSPORT_TYPE= %s write MAX time= %lf\n",
			ts, transport_type_str_, tm_max);
	}

    for (i = 0; i < num_vars; i++) {
        if (data_tab[i]) {
            free(data_tab[i]);
        }
    }
    free(data_tab);

	return 0;
}

int test_put_run(enum transport_type type, int npapp, int npx, int npy, int npz,
    int spx, int spy, int spz, int timestep, int dims, size_t elem_size,
    int num_vars, MPI_Comm gcomm)
{
	gcomm_ = gcomm;
	elem_size_ = elem_size;
	timestep_ = timestep;
	npapp_ = npapp;
	npx_ = npx;
	npy_ = npy;
	npz_ = npz;
	if (npx_)
		spx_ = spx;
	if (npy_)
		spy_ = spy;
	if (npz_)
		spz_ = spz;

	timer_init(&timer_, 1);
	timer_start(&timer_);

	int app_id = 1;
    	common_init(npapp_, app_id);
    	common_set_storage_type(row_major, type);
    	common_get_transport_type_str(type, transport_type_str_);

	MPI_Comm_rank(gcomm_, &rank_);
	MPI_Comm_size(gcomm_, &nproc_);

    	unsigned int ts;
	if (dims == 2) {
        	for (ts = 1; ts <= timestep_; ts++){
            		//couple_write_2d(ts, num_vars, type);
            		couple_write_2d_multi_var(ts, num_vars, type);
        	}

        	if (type == USE_DIMES) {
            		// Wait for the reader to finish the last step
            		common_lock_on_write("m2d_lock", &gcomm_);
            		common_put_sync(type);
            		common_unlock_on_write("m2d_lock", &gcomm_);
        	}
	} else if (dims == 3) {
        	for (ts = 1; ts <= timestep_; ts++) {
        	    	couple_write_3d(ts, num_vars, type);
        	}

        	if (type == USE_DIMES) {
       	     		// Wait for the reader to finish the last step
            		common_lock_on_write("m3d_lock", &gcomm_);
            		common_put_sync(type);
            		common_unlock_on_write("m3d_lock", &gcomm_);
        	}
	} else {
		uloga("%s(): error dims= %d\n", __func__, dims);
	}

    if (rank_ == 0) {
        uloga("%s(): done\n", __func__);
    }

    MPI_Barrier(gcomm_);
    common_finalize();

	return 0;	
}
