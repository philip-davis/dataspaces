#include <stdio.h>
#include "common.h"
#include "debug.h"
#include "ss_data.h"
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

//static int offx_, offy_, offz_, off4_;
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

static int couple_read_nd(double *mnd, unsigned int ts, enum transport_type type, int ndims)
{
	common_lock_on_read("mnd_lock", &gcomm_);

	//get data from space
	int elem_size = sizeof(double);

	int lb[10] = {0}, ub[10] = {0};	
	for(int i = 0; i < ndims; i++){
		lb[i] = off[i];
		ub[i] = off[i] + sp[i] - 1;
	}
	double tm_st, tm_end1, tm_end2;

	//uloga("Timestep=%u, %d read m2d: {(%d,%d,%d,%d),(%d,%d,%d,%d)} from space\n",
	//	ts, rank_, xl,yl,zl,l4, xu,yu,zu,u4);

	MPI_Barrier(gcomm_);
	tm_st = timer_read(&timer_);

	common_get("mnd", ts, elem_size,
		//xl, yl, zl, l4, xu, yu, zu,u4,
		ndims,
		lb, ub,
		mnd, type);	
	tm_end1 = timer_read(&timer_);
	
	MPI_Barrier(gcomm_);
	tm_end2 = timer_read(&timer_);

	uloga("TS= %u TRANSPORT_TYPE= %d RANK= %d read time= %lf\n",
		ts, type, rank_, tm_end1-tm_st);	
	if (rank_ == 0) {
		uloga("TS= %u TRANSPORT_TYPE= %d read MAX time= %lf\n",
			ts, type, tm_end2-tm_st);
	}

	common_unlock_on_read("mnd_lock", &gcomm_);

	return 0;
}

#ifdef USE_LUA_REXEC
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

	num_obj = dspaces_lua_rexec(fname, vname, version, size_elem,
		xl, yl, zl, xu, yu, zu, temp_res);

	if (num_obj >= 0) {
                int i;
                for (i = 0; i < num_obj; ++i) {
			double *ptr = (double*)(temp_res + i*LUA_BYTES_RESULT_PAD);
                        uloga("%s: Min: %.4f, Max: %.4f, Avg: %.4f\n", __func__,
				ptr[0], ptr[1], ptr[2]);
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

        common_lock_on_read("m2d_lock");

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

        common_unlock_on_read("m2d_lock");

        return 0;
}
#endif

int test_get_run(int num_ts, int num_process,
	//int process_x,int process_y,int process_z,int p4,
	int *process,
	//int dims,int dim_x,int dim_y,int dim_z,int dim4,
	int ndims, int *dim,
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
 
	int app_id = 2;
	common_init(npapp_, app_id);
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();

	double *mnd = NULL;
	mnd = allocate_nd(ndims);
	if(mnd){
		unsigned int ts;
		for (ts = 1; ts <= timesteps_; ts++){
			if (rank_ == 0)
				uloga("%s: At timestep %u\n", __func__, ts);
			couple_read_nd(mnd, ts, USE_DSPACES, ndims);
			
			int size = 1;
			for(int i = 0; i < ndims; i++)
				size *= sp[i];
			check_data(mnd, size, rank_);
		}
	}
	common_barrier();
	common_finalize();
	if (mnd)
		free(mnd);	

	return 0;
}

#ifdef USE_LUA_REXEC
int test_lua_rexec(int num_ts, int num_process,int process_x,int process_y,int process_z,
                int dims, int dim_x, int dim_y, int dim_z)
{
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

	int app_id = 2;
        common_init(npapp_, app_id);
        rank_ = common_rank();
        nproc_ = common_peers();

        double *m2d = NULL;
        m2d = allocate_2d();
        if(m2d){
                unsigned int ts;
                for(ts=1; ts <= timesteps_; ts++){
                        if(rank_ == 0)
                                uloga("%s: At timestep %u\n", __func__, ts);
                        couple_lua_rexec_2d(ts);
                }
        }

        common_barrier();
        common_finalize();
        if(m2d)
                free(m2d);

        return 0;
}
#endif
