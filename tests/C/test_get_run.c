#include <stdio.h>
#include "common.h"
#include "debug.h"
#include "ss_data.h"
#include "mpi.h"

//# of processors in x-y-z direction
static int npx_, npy_, npz_;
//block size per processor per direction
static int spx_, spy_, spz_;
//# of iterations
static int timesteps_;
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
	//init off_x_ and off_y_ values
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_) * spy_;

	tmp = (double*)malloc(sizeof(double)*spx_*spy_);
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

        uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
                ts, type, rank_, tm_end1-tm_st);
        if (rank_ == 0) {
                uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
                        ts, type, tm_end2-tm_st);
        }

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

	uloga("Timestep=%u, %d read m2d: {(%d,%d,%d),(%d,%d,%d)} from space\n",
		ts, rank_, xl,yl,zl, xu,yu,zu);

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	common_get("m2d", ts, elem_size,
		xl, yl, zl, xu, yu, zu,
		m2d, type);	
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

	check_data("m2d", m2d, spx_*spy_, rank_, ts);

	return 0;
}

int test_get_run(int num_ts, int num_process,int process_x,int process_y,
	int process_z,int dims,int dim_x,int dim_y,int dim_z,MPI_Comm gcomm)
{
	gcomm_ = gcomm;
	timesteps_ = num_ts;
	npapp_ = num_process;
	npx_ = process_x;
	npy_ = process_y;
	npz_ = process_z;
	if(process_x)
		spx_ = dim_x/process_x;
	if(process_y)
		spy_ = dim_y/process_y;
	if(process_z)
		spz_ = dim_z/process_z;

	timer_init(&timer_, 1);
	timer_start(&timer_);
 
	int app_id = 2;
	common_init(npapp_, app_id);
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();

	double *m2d = NULL;
	m2d = allocate_2d();
	if (m2d) {
		unsigned int ts;
		for (ts = 1; ts <= timesteps_; ts++){
			memset(m2d, 0, sizeof(double)*spx_*spy_);
			if (rank_ == 0)
				uloga("%s: At timestep %u\n", __func__, ts);
			//if (ts % 2 == 0)	
			// 	couple_read_2d(m2d, ts, USE_DIMES);
			 	//couple_read_2d_multi_var(ts, USE_DIMES, 10);
			//else if (ts % 2 == 1)
			  	couple_read_2d(m2d, ts, USE_DSPACES);
			  	//couple_read_2d_multi_var(ts, USE_DSPACES, 10);
		}
	}

	common_barrier();
	common_finalize();
	if (m2d)
		free(m2d);	

	return 0;
}
