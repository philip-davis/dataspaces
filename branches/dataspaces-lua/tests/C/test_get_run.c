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
#include "lua_rexec.h"
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

/*
Matrix representation
+-----> (x)
|
|
v (y)
*/
static double* allocate_2d()
{
	double* tmp = NULL;
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_) * spy_;

	tmp = (double*)malloc(sizeof(double)*spx_*spy_);
	return tmp;
}

static double* allocate_3d()
{
	double *tmp = NULL;
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_ % npy_) * spy_;
	offz_ = (rank_ / npx_ / npy_) * spz_;

	tmp = (double*)malloc(sizeof(double)*spx_*spy_*spz_);
	return tmp;
}

static int couple_read_2d_multi_var(unsigned int ts, enum transport_type type,
					int num_vars)
{
	char var_name[128];
	double *data = NULL;
	int i;

	data = allocate_2d();
	if (data == NULL) {
		uloga("%s(): failed to alloc buffer\n", __func__);
		return -1;
	}

	common_lock_on_read("m2d_lock", &gcomm_);

	//get data from space
	int elem_size = sizeof(double);
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end1, tm_end2;

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	for (i = 0; i < num_vars; i++) {
		sprintf(var_name, "m2d_%d", i);
		memset(data, 0, sizeof(double)*spx_*spy_);
		common_get(var_name, ts, elem_size,
				xl, yl, zl, xu, yu, zu,
				data, type);
		check_data(var_name, data, spx_*spy_, rank_, ts);
	}

	tm_end1 = timer_read(&timer_);

	MPI_Barrier(gcomm_);
	tm_end2 = timer_read(&timer_);

#ifdef DEBUG
	uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
			ts, type, rank_, tm_end1-tm_st);
	if (rank_ == 0) {
		uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
				ts, type, tm_end2-tm_st);
	}
#endif

	common_unlock_on_read("m2d_lock", &gcomm_);

	if (data)
		free(data);

	return 0;
}

static int couple_read_2d(double *m2d, unsigned int ts, enum transport_type type)
{
	common_lock_on_read("m2d_lock", &gcomm_);

	//get data from space
	int elem_size = sizeof(double);
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end1, tm_end2;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m2d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, xl,yl,zl, xu,yu,zu);
#endif

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	common_get("m2d", ts, elem_size,
		xl, yl, zl, xu, yu, zu,
		m2d, type);	
	tm_end1 = timer_read(&timer_);
	
	MPI_Barrier(gcomm_);
	tm_end2 = timer_read(&timer_);

#ifdef DEBUG
	uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
		ts, type, rank_, tm_end1-tm_st);	
	if (rank_ == 0) {
		uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
			ts, type, tm_end2-tm_st);
	}
#endif

	common_unlock_on_read("m2d_lock", &gcomm_);

	check_data("m2d", m2d, spx_*spy_, rank_, ts);

	return 0;
}

static int couple_read_3d(double *m3d, unsigned int ts, enum transport_type type)
{
	common_lock_on_read("m3d_lock", &gcomm_);

	//get data from space
	int elem_size = sizeof(double);
	int xl = offx_;
	int yl = offy_;
	int zl = offz_;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = offz_ + spz_ - 1;
	double tm_st, tm_end1, tm_end2;

#ifdef DEBUG
	uloga("Timestep=%u, %d read m3d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, xl,yl,zl, xu,yu,zu);
#endif

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	common_get("m3d", ts, elem_size,
		xl, yl, zl, xu, yu, zu,
		m3d, type);	
	tm_end1 = timer_read(&timer_);
	
	MPI_Barrier(gcomm_);
	tm_end2 = timer_read(&timer_);

#ifdef DEBUG
	uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
		ts, type, rank_, tm_end1-tm_st);	
	if (rank_ == 0) {
		uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
			ts, type, tm_end2-tm_st);
	}
#endif

	common_unlock_on_read("m3d_lock", &gcomm_);

	check_data("m3d", m3d, spx_*spy_*spz_, rank_, ts);

	return 0;
}

int test_get_run(int npapp, int npx, int npy, int npz,
        int spx, int spy, int spz, int timestep, int dims, MPI_Comm gcomm)
{
	gcomm_ = gcomm;
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
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();

	double *databuf = NULL;
	if (dims == 2) {
		databuf = allocate_2d();
		if (databuf) {
			unsigned int ts;
			for (ts = 1; ts <= timestep_; ts++){
				memset(databuf, 0, sizeof(double)*spx_*spy_);
#ifdef DS_HAVE_DIMES
				if (ts % 2 == 0)	
					couple_read_2d(databuf, ts, USE_DIMES);
				else if (ts % 2 == 1)
					couple_read_2d(databuf, ts, USE_DSPACES);
#else
				couple_read_2d(databuf, ts, USE_DSPACES);
#endif
			}
		}
	} else if (dims == 3) {
		databuf = allocate_3d();
		if (databuf) {
			unsigned int ts;
			for (ts = 1; ts <= timestep_; ts++) {
				memset(databuf, 0, sizeof(double)*spx_*spy_*spz_);
#ifdef DS_HAVE_DIMES
				if (ts % 2 == 0)	
					couple_read_3d(databuf, ts, USE_DIMES);
				else if (ts % 2 == 1)
					couple_read_3d(databuf, ts, USE_DSPACES);
#else
				couple_read_3d(databuf, ts, USE_DSPACES);
#endif
			}
		}
	}

	common_barrier();
	common_finalize();

	if (databuf)
		free(databuf);	

	return 0;
}

#ifdef DS_HAVE_LUA_REXEC
int send_lua_script(const char *fname, const char *vname,
                unsigned int version, int size_elem,
                int xl, int yl, int zl,
                int xu, int yu, int zu)
{
	int err;
	int num_obj;
	size_t size = dspaces_get_num_space_peers()
			* LUA_BYTES_RESULT_PAD;
	char* temp_res = (char*) malloc(size);
	memset(temp_res, 0, size);

	double tm_st, tm_end, tm_max, tm_diff;
	int root = 0;

	MPI_Barrier(gcomm_);

	tm_st = timer_read(&timer_);
	num_obj = dspaces_lua_rexec(fname, vname, version, size_elem,
		xl, yl, zl, xu, yu, zu, temp_res);
	tm_end = timer_read(&timer_);

	MPI_Barrier(gcomm_);

	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);
	
	uloga("TS= %u RANK= %d lua_rexec time= %lf\n", version, rank_, tm_diff);
	if (rank_ == root) {
		uloga("TS= %u lua_rexec MAX time= %lf\n", version, tm_max);
	}

	if (num_obj >= 0) {
		int i;
		for (i = 0; i < num_obj; ++i) {
			double *ptr = (double*)(temp_res + i*LUA_BYTES_RESULT_PAD);
			uloga("%s(): ts= %u, rank= %d obj= %d Min: %.4f, Max: %.4f, Avg: %.4f\n", 
				__func__, version, rank_, i, ptr[0], ptr[1], ptr[2]);
		}
		err = 0;
	}

	free(temp_res);
	return err;	
}

static int couple_lua_rexec_2d(unsigned int ts)
{
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_) * spy_;

	common_lock_on_read("m2d_lock", &gcomm_);

	//send lua script to the space
	int elem_size = sizeof(double);
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;

	uloga("Timestep=%u, %d lua rexec on m2d: {(%d,%d,%d),(%d,%d,%d)}\n",
			ts, rank_, xl,yl,zl, xu,yu,zu);

	send_lua_script("simple_stat.lua", "m2d", ts, elem_size,
		xl, yl, zl, xu, yu, zu);

	common_unlock_on_read("m2d_lock", &gcomm_);

	return 0;
}

static int couple_local_exec_2d(double *m2d, unsigned int ts, enum transport_type type)
{
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_) * spy_;

	common_lock_on_read("m2d_lock", &gcomm_);

	//send lua script to the space
	int elem_size = sizeof(double);
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;
	double tm_st, tm_end, tm_diff, tm_max;
	int root = 0;

	uloga("Timestep=%u, %d local exec on m2d: {(%d,%d,%d),(%d,%d,%d)}\n",
			ts, rank_, xl,yl,zl, xu,yu,zu);

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

    common_get("m2d", ts, elem_size,
        xl, yl, zl, xu, yu, zu,
        m2d, type);
	// check_data("m2d", m2d, spx_*spy_, rank_, ts);
	// Read lua script file
	void *code_buf = NULL;
	size_t code_size;
	int err = lua_load_script_file("simple_stat.lua", &code_buf, &code_size);
	if (err < 0) {
		return err;
	}

	double output_data[3];
	lua_exec(code_buf, code_size, m2d, spx_*spy_, output_data, 3);
	free(code_buf);

	tm_end = timer_read(&timer_);
	MPI_Barrier(gcomm_);

	tm_diff = tm_end-tm_st;
	MPI_Reduce(&tm_diff, &tm_max, 1, MPI_DOUBLE, MPI_MAX, root, gcomm_);

	uloga("TS= %u RANK= %d local_exec time= %lf\n", ts, rank_, tm_diff);
	if (rank_ == root) {
		uloga("TS= %u local_exec MAX time= %lf\n", ts, tm_max);
	}

	common_unlock_on_read("m2d_lock", &gcomm_);

	return 0;
}

int test_lua_rexec(int npapp, int npx, int npy, int npz,
		int spx, int spy, int spz, int timestep, int dims, MPI_Comm gcomm)
{
	gcomm_ = gcomm;
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

	int app_id = 2;
	common_init(npapp_, app_id);
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();

	double *databuf = NULL;
	if (dims == 2) {
		databuf = allocate_2d();
		if (databuf) {
			unsigned int ts;
			for (ts = 1; ts <= timestep_; ts++){
				if (rank_ == 0)
					uloga("%s: At timestep %u\n", __func__, ts);
				if (ts % 2 == 1)
					couple_lua_rexec_2d(ts);
				else
					couple_local_exec_2d(databuf, ts, USE_DSPACES);
			}
		}
	} else {
		uloga("%s(): error dims= %d\n", __func__, dims);
		return -1;
	}

	common_barrier();
	common_finalize();
	if (databuf)
		free(databuf);

	return 0;
}
#endif
