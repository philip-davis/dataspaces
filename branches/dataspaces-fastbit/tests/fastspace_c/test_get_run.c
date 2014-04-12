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

static int couple_read_2d(unsigned int ts, int num_vars, enum transport_type type)
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
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m2d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, xl,yl,zl, xu,yu,zu);
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
        common_get(var_name, ts, elem_size,
            xl, yl, zl, xu, yu, zu, data_tab[i], type);
    }	
	tm_end = timer_read(&timer_);
	common_unlock_on_read("m2d_lock", &gcomm_);
	
	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

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

static int couple_read_3d(unsigned int ts, int num_vars, enum transport_type type)
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
	int xl = offx_;
	int yl = offy_;
	int zl = offz_;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = offz_ + spz_ - 1;
	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m3d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, xl,yl,zl, xu,yu,zu);
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
        common_get(var_name, ts, elem_size,
            xl, yl, zl, xu, yu, zu, data_tab[i], type);
    }	
	tm_end = timer_read(&timer_);
    common_unlock_on_read("m3d_lock", &gcomm_);
	
	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

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

static int couple_value_query_2d(double *m2d, unsigned int ts, enum transport_type type, void* qcond)
{
	int i;
	int qret_size = 0;
        common_lock_on_read("m2d_lock", &gcomm_);

        //get data from space
        int elem_size = sizeof(double);
        double tm_st, tm_end1, tm_end2;

        MPI_Barrier(gcomm_);
        tm_st = timer_read(&timer_);

	/*for(i = 0; i < num_vars; i++){
		sprintf(var_name, "m2d_%d", i);
	        common_value_query(var_name, ts, elem_size, qcond);
	}*/
	qret_size = common_value_query("m2d", ts, elem_size, qcond, m2d);
//      char *qcond1 = "select name where name>300";
//      common_value_query("m2d", ts, elem_size, qcond1);
        tm_end1 = timer_read(&timer_);

        MPI_Barrier(gcomm_);
        tm_end2 = timer_read(&timer_);

        uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
                ts, type, rank_, tm_end1-tm_st);
        if (rank_ == 0) {
                uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
                        ts, type, tm_end2-tm_st);
        }

        common_unlock_on_read("m2d_lock", &gcomm_);

//      check_data("m2d", m2d, spx_*spy_, rank_, ts);

	return qret_size;
}

static int couple_value_query_3d(double *m3d, unsigned int ts, enum transport_type type, void* qcond)
{
	int i;
        common_lock_on_read("m3d_lock", &gcomm_);

        //get data from space
        int elem_size = sizeof(double);
        double tm_st, tm_end1, tm_end2;

        MPI_Barrier(gcomm_);
        tm_st = timer_read(&timer_);

	/*for(i = 0; i < num_vars; i++){
		sprintf(var_name, "m3d_%d", i);
	        common_value_query(var_name, ts, elem_size, qcond);
	}*/
	common_value_query("m3d", ts, elem_size, qcond);
//      char *qcond1 = "select name where name>300";
//      common_value_query("m2d", ts, elem_size, qcond1);
        tm_end1 = timer_read(&timer_);

        MPI_Barrier(gcomm_);
        tm_end2 = timer_read(&timer_);

        uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
                ts, type, rank_, tm_end1-tm_st);
        if (rank_ == 0) {
                uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
                        ts, type, tm_end2-tm_st);
        }

        common_unlock_on_read("m3d_lock", &gcomm_);

//      check_data("m2d", m2d, spx_*spy_, rank_, ts);

	return 0;
}

int test_get_run(enum transport_type type, int npapp, int npx, int npy, int npz,
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
	if(npx_)
		spx_ = spx;
	if(npy_)
		spy_ = spy;
	if(npz_)
		spz_ = spz;

	timer_init(&timer_, 1);
	timer_start(&timer_);
 
	int app_id = 2;
	common_init(npapp_, app_id);
    common_set_storage_type(row_major, type);
    common_get_transport_type_str(type, transport_type_str_);

    MPI_Comm_rank(gcomm_, &rank_);
    MPI_Comm_size(gcomm_, &nproc_);

   /* unsigned int ts;
	if (dims == 2) {
        for (ts = 1; ts <= timestep_; ts++){
            couple_read_2d(ts, num_vars, type);
        }
	} else if (dims == 3) {
        for (ts = 1; ts <= timestep_; ts++) {
            couple_read_3d(ts, num_vars, type);
        }
	} else {
        uloga("%s(): error dims= %d\n", __func__, dims);
    }

    if (rank_ == 0) {
        uloga("%s(): done\n", __func__);
    }
    */
	//malloc enough space for the query result
        double *m2d = NULL;
	m2d = malloc(spx_*spy_*elem_size);

	int qret_size = 0;
        
        if (m2d) {
                unsigned int ts;
                for (ts = 1; ts <= timestep_; ts++){
                        if (rank_ == 0)
                                uloga("%s: At timestep %u\n", __func__, ts);
			//char* qcond = "select var1 where var2>3 and var2<9";	//for var1:d, var2:i case

			char* qcond = "select var1 where var3>3 and var3<9 and var2<5";	//for var1:d, var2:f, var3:i case
                        qret_size = couple_value_query_2d(m2d, ts, USE_DSPACES, (void*)qcond);

			printf("qret_size = %d\n", qret_size);
			/*if(qret_size > 0)
				printf("the last elem=%lf\n", *((double*)m2d+qret_size/8-1));*/
		}
	}	

	//common_barrier();
    	MPI_Barrier(gcomm_);
	common_finalize();
	if(m2d)
		free(m2d);
	
	return 0;
}

