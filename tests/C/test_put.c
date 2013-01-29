#include <stdio.h>

#include "common.h"

#include "dataspaces.h"
#include "debug.h"

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

static int generate_2d(double *m2d, unsigned int ts)
{
	double value = 1.0*(rank_) + 0.0001*ts;
	int m2d_size = spx_ * spy_;
	for(int i=0; i<m2d_size; i++){
		*(m2d+i) = value;
	}

	return 0;
}

static int couple_write_2d(double *m2d, unsigned int ts, enum transport_type type)
{
	common_lock_on_write("m2d_lock");

	//put the m2d into the space
	int elem_size = sizeof(double);
	/*set the two coordinates for the box*/
	int xl = offx_;
	int yl = offy_;
	int zl = 0;
	int xu = offx_ + spx_ - 1;
	int yu = offy_ + spy_ - 1;
	int zu = 0;

	common_put("m2d", ts, elem_size, 
		xl, yl, zl, xu, yu, zu,
		m2d, type);

	uloga("Timestep=%u, %d write m2d:{(%d,%d,%d),(%d,%d,%d)} into space\n",
		ts, rank_, xl,yl,zl,xu,yu,zu);

    	if ( type == USE_DSPACES ) {
                common_put_sync(type);
                common_unlock_on_write("m2d_lock");
        } else if (type == USE_DIMES) {
                common_unlock_on_write("m2d_lock");
                common_put_sync(type);
        }

	return 0;
}

static int test_put_run(int num_ts, int num_process,int process_x,int process_y,int process_z,
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

	int app_id = 1;
	common_init(npapp_, app_id);
	common_set_storage_type(row_major);
	rank_ = common_rank();
	nproc_ = common_peers();

	double *m2d = NULL;
	m2d = allocate_2d();
	if(m2d){
		unsigned int ts;
		for(ts=1; ts <= timesteps_; ts++){
			generate_2d(m2d, ts);
                        if (ts % 2 == 0)
                          couple_write_2d(m2d, ts, USE_DIMES);
                        else if (ts % 2 == 1)
                          couple_write_2d(m2d, ts, USE_DSPACES);
		}
	}

	common_barrier();
	common_finalize();

	if(m2d)
		free(m2d);
	
	return 0;	
}

extern int read_config_file(const char *fname,
        int *num_sp, int *num_cp, int *iter,
        int *num_writer, int *writer_x, int *writer_y, int *writer_z,
        int *num_reader, int *reader_x, int *reader_y, int *reader_z,
        int *dims, int *dim_x, int *dim_y, int *dim_z);

int main(int argc, char **argv)
{
        int err;

        int num_sp, num_cp, iter;
        int num_writer,writer_x,writer_y,writer_z;
        int num_reader,reader_x,reader_y,reader_z;
        int dims, dim_x, dim_y, dim_z;
        if(read_config_file("computenode.conf",
                &num_sp, &num_cp, &iter,
                &num_writer, &writer_x, &writer_y, &writer_z,
                &num_reader, &reader_x, &reader_y, &reader_z,
                &dims, &dim_x, &dim_y, &dim_z) != 0) {
                goto err_out;
        }
	
	// Run as data writer
	sleep(10);//wait for the dataspace servers to init.

	test_put_run(iter,num_writer,writer_x,writer_y,writer_z,
		dims,dim_x,dim_y,dim_z);

	return 0;	
err_out:
	uloga("error out!\n");
	return -1;	
}

