#include <stdio.h>
#include "common.h"
#include "debug.h"
#include "mpi.h"

//# of processors in x-y-z direction
static int np[10] = {0};
//block size per processor per direction
static int sp[10] = {0};
//# of iterations
static int timesteps_;
//# of processors in the application
static int npapp_;

static int rank_, nproc_;

static int off[10] = {0};

static struct timer timer_;

static MPI_Comm gcomm_;

/*
Matrix representation
+-----> (x)
|
|
v (y)
*/
static double* allocate_nd(int dims)
{
	double* tmp = NULL;
	int i = 0, j = 0; 
	int size = 1;
	for(i = 0; i < dims; i++){
		int tmp = rank_;
		for(j = 0; j < i; j++)
			tmp /= np[j];
		off[i] = tmp % np[i] * sp[i];
		size *= sp[i];
	}
	tmp = (double*)malloc(sizeof(double)*size);
	return tmp;	
}

static int generate_nd(double *mnd, unsigned int ts, int dims)
{
	double value = 1.0*(rank_) + 0.0001*ts;
	int i, mnd_size = 1;
	for(i = 0; i < dims; i++)
		mnd_size *= sp[i];
	for(i = 0; i < mnd_size; i++)
		*(mnd+i) = value;
	return 0;
}

static int couple_write_nd(double *mnd, unsigned int ts, enum transport_type type, int dims)
{
	common_lock_on_write("mnd_lock", &gcomm_);

	//put the m2d into the space
	int elem_size = sizeof(double);
	/*set the two coordinates for the box*/
	int lb[10] = {0}, ub[10] = {0};
	for(int i = 0; i < dims; i++){
		lb[i] = off[i];
	 	ub[i] = off[i] + sp[i] - 1;	
	}
	double tm_st, tm_end1, tm_end2;

	//uloga("Timestep=%u, %d write m2d:{(%d,%d,%d,%d),(%d,%d,%d,%d)} into space\n",
	//	ts, rank_, xl,yl,zl,l4,xu,yu,zu,u4);
	uloga("Timestep=%u, write mnd to spaces\n", ts);

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	common_put("mnd", ts, elem_size, 
		//xl, yl, zl, l4, xu, yu, zu, u4,
		dims,
		lb, ub,
		mnd, type);

	if ( type == USE_DSPACES ) {
		common_put_sync(type);

		tm_end1 = timer_read(&timer_);
		MPI_Barrier(gcomm_);
		tm_end2 = timer_read(&timer_);

		common_unlock_on_write("mnd_lock", &gcomm_);
	} else if (type == USE_DIMES) {
		tm_end1 = timer_read(&timer_);
		MPI_Barrier(gcomm_);
		tm_end2 = timer_read(&timer_);

		sleep(1);
		common_unlock_on_write("m2d_lock", &gcomm_);
		common_put_sync(type);
	}

	uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d write time= %lf\n",
		ts, type, rank_, tm_end1-tm_st);
	if (rank_ == 0) {
		uloga("TS= %u TRANSPORT_TYPE= %d write MAX time= %lf\n",
			ts, type, tm_end2-tm_st);
	}

	return 0;
}

int test_put_run(int num_ts,int num_process,
	//int process_x,int process_y,int process_z,int p4,
	int* process,
	int ndims, int* dim,
	//int dims,int dim_x,int dim_y,int dim_z,int dim4,
	MPI_Comm gcomm)
{
	gcomm_ = gcomm;
	timesteps_ = num_ts;
	npapp_ = num_process;

	for(int i = 0; i < ndims; i++){
		np[i] = process[i];
		//sp[i] = dim[i]/process[i];
		sp[i] = dim[i];
	}

	timer_init(&timer_, 1);
	timer_start(&timer_);

	int app_id = 1;
	common_init(npapp_, app_id);
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();
	double *mnd = NULL;
	mnd = allocate_nd(ndims);
	if(mnd){
		unsigned int ts;
		for (ts = 1; ts <= timesteps_; ts++){
			generate_nd(mnd, ts, ndims);
			couple_write_nd(mnd, ts, USE_DSPACES, ndims);

                        //int size = 1;
                        //for(int i = 0; i < ndims; i++)
                        //        size *= sp[i];
                        //check_data(mnd, size, rank_);
		}
	}
	if(mnd == NULL)
		printf("fail to malloc mnd\n");
	common_barrier();
	common_finalize();
	
	if (mnd)
		free(mnd);

	return 0;	
}
