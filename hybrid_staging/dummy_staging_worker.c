#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "hybrid_staging_api.h"
#include "mpi.h"

//# of processors in x-y-z direction
static int npx_, npy_, npz_;
//block size per processor per direction
static int spx_, spy_, spz_;
//# of iterations
//static int timestep_;
//# of processors in the application
//static int nproc_;
//# of dimensions
static int dims_;

static int rank_;

static int offx_, offy_, offz_;

static int validate_data(void *data, int num_double, int step)
{
	double value = step;
	double *tmp = (double *)data;
	int i, ret = 1;
	for (i=0; i<num_double; i++) {
		if (tmp[i] != value)
			ret = 0;
	}

	return ret;
}

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

static int generate_bbox(int *xl, int *yl, int *zl, int *xu, int *yu, int *zu)
{
	if (dims_ == 2) {
		*xl = offx_;
		*yl = offy_;
		*zl = 0;
		*xu = offx_ + spx_ - 1;
		*yu = offy_ + spy_ - 1;
		*zu = 0;
	} else if (dims_ == 3) {
		*xl = offx_;
		*yl = offy_;
		*zl = offz_;
		*xu = offx_ + spx_ - 1;
		*yu = offy_ + spy_ - 1;
		*zu = offz_ + spz_ - 1;
	}

	return 0;
}

static void set_data_decomposition(struct task_descriptor *t, struct var_descriptor *var_desc)
{
	rank_ = t->rank;
	dims_ = var_desc->bb.num_dims;

	if (var_desc->bb.num_dims == 2) {
		npx_ = t->nproc;
		npy_ = 1;
		npz_ = 0;
		spx_ = (var_desc->bb.ub.c[0]+1)/npx_;
		spy_ = (var_desc->bb.ub.c[1]+1)/npy_;
		spz_ = 0;
	} else if (var_desc->bb.num_dims == 3) {
		npx_ = t->nproc;
		npy_ = 1;
		npz_ = 1;
		spx_ = (var_desc->bb.ub.c[0]+1)/npx_;
		spy_ = (var_desc->bb.ub.c[1]+1)/npy_;
		spz_ = (var_desc->bb.ub.c[2]+1)/npz_;
	}

	uloga("%s(): npx_= %d npy_= %d npz_= %d spx_= %d spy_= %d spz_= %d\n",
		__func__,npx_,npy_,npz_,spx_,spy_,spz_);
}

static double* allocate_data(struct var_descriptor *var_desc)
{	
	if (var_desc->bb.num_dims == 2) {
		return allocate_2d();					
	} else if (var_desc->bb.num_dims == 3) {
		return allocate_3d();
	}		
}

int perform_processing_op(MPI_Comm comm, struct task_descriptor *t)
{
	double *databuf = NULL;
	int xl, yl, zl, xu, yu, zu;
	size_t elem_size;
	int i;
	int err;

	for (i = 0; i < t->num_input_vars; i++) {
		struct var_descriptor *var_desc = &(t->input_vars[i]);
		set_data_decomposition(t, var_desc);
		databuf = allocate_data(var_desc);
		elem_size = sizeof(double);
		generate_bbox(&xl, &yl, &zl, &xu, &yu, &zu);
		err = hstaging_get_var(var_desc->var_name, t->step, elem_size,
				xl, yl, zl, xu, yu, zu, databuf, NULL);	
		if (err < 0) {
			uloga("%s(): failed to get var\n", __func__);
			return -1;
		}

		if (var_desc->bb.num_dims == 2) {
			err = validate_data(databuf, spx_*spy_, t->step);
		} else if (var_desc->bb.num_dims == 3) {
			err = validate_data(databuf, spx_*spy_*spz_, t->step);
		}

		if (!err) {
			uloga("%s(): tid= %d step= %d rank= %d nproc= %d "
				"var '%s' validate_data() failed\n", __func__,
				t->tid, t->step, t->rank, t->nproc, var_desc->var_name);
		}

		free(databuf);
	}

	uloga("%s(): execute task tid= %d step= %d rank= %d nproc= %d "
		"num_input_vars= %d\n",
		__func__, t->tid, t->step, t->rank, t->nproc, t->num_input_vars);
	
	return 0;
}

int dummy_s3d_staging_parallel_job(MPI_Comm comm)
{
	int i, err;
	int nprocs, mpi_rank;
	
	MPI_Comm_size(comm, &nprocs);
	MPI_Comm_rank(comm, &mpi_rank);
	if (mpi_rank == 0) {
		uloga("Dummy S3D staging: total %d workers\n", nprocs);
	}

	struct task_descriptor t;
	while ( !hstaging_request_task(&t)) {
		err = perform_processing_op(comm, &t);	
		if (err < 0) {
			if (t.num_input_vars > 0 && t.input_vars) {
				free(t.input_vars);
			}
			return err;
		}
	}

	return 0;
 err_out:
	return -1;
}
