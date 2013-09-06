#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "common.h"
#include "hybrid_staging_api.h"
#include "mpi.h"

static struct timer timer;

#define SIZE_OBJ 16*1024
#define NUM_ROWS 3
#define FREQ 1 

static const int millisec = 1000;

//# of processors in x-y-z direction
static int npx_, npy_, npz_;
//block size per processor per direction
static int spx_, spy_, spz_;
//# of iterations
static int timestep_;
//# of processors in the application
static int nproc_;
//# of dimensions
static int dims_;

static int rank_;

static int offx_, offy_, offz_;

static double* allocate_2d()
{
	double* tmp = NULL;
	offx_ = (rank_ % npx_) * spx_;
	offy_ = (rank_ / npx_) * spy_;

	tmp = (double*)malloc(sizeof(double)*spx_*spy_);
	return tmp;
}

static int generate_2d(double *buf, unsigned int ts)
{
	// double value = 1.0*(rank_) + 0.0001*ts;
	double value = ts;
	int num_elem = spx_ * spy_;
	int i;
	for (i = 0; i < num_elem; i++) {
		buf[i] = value;
	}

	return 0;
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

static int generate_3d(double *buf, unsigned int ts)
{
	double value = ts;
	int num_elem = spx_ * spy_ * spz_;
	int i;
	for (i = 0; i < num_elem; i++) {
		buf[i] = value;
	}

	return 0;
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

static int update_var(const char *var_name, int ts)
{
	int err;
	struct var_descriptor var_desc;
	int *c = NULL;
	size_t elem_size = sizeof(double);

	strcpy(var_desc.var_name, var_name);		
	var_desc.step = ts;
	var_desc.bb.num_dims = dims_;
	var_desc.size = elem_size;
	c = var_desc.bb.lb.c;
	c[0] = 0;
	c[1] = 0;
	c[2] = 0;
	c = var_desc.bb.ub.c;	
	c[0] = (npx_ * spx_) - 1;
	c[1] = (npy_ * spy_) - 1;
	c[2] = (npz_ * spz_) - 1;
	err = hstaging_update_var(&var_desc, OP_PUT);
	return err;
}

/* dummy code, exchange data with neighbors */
static int perform_computing(MPI_Comm comm_new, int numprocs, int myid, int ts)
{
	usleep(millisec*1000);

	// TODO: could this be delivered in order?
/*
	int left, right;
	int buffer[100], buffer2[100];
	MPI_Request req, req2;
	MPI_Status status;

	MPI_Comm_size(comm_new, &numprocs);
	right = (myid + 1) % numprocs;
	left = myid - 1;
	if ( left < 0 )
		left = numprocs - 1;
	MPI_Irecv(buffer, 10, MPI_INT, left, 123, comm_new, &req);
	MPI_Isend(buffer2, 10, MPI_INT, right, 123, comm_new, &req2);
	MPI_Wait(&req, &status);
	MPI_Wait(&req2, &status);
*/
	return 0;
}

static int perform_topology(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	double *databuf = NULL;
	int elem_size, xl, yl, zl, xu, yu, zu;
	int err;
	
	if (dims_ == 2) {
		databuf = allocate_2d();
		generate_2d(databuf, ts);
	} else if (dims_ == 3) {
		databuf = allocate_3d();
		generate_3d(databuf, ts);
	}

	elem_size = sizeof(double);
	generate_bbox(&xl, &yl, &zl, &xu, &yu, &zu);

	err = hstaging_put_var("topology_var1", ts, elem_size,
			xl, yl, zl, xu, yu, zu, databuf, &comm);
	if (err < 0) {
		free(databuf);
		return err;
	}

	if (rank_ == 0) {
		update_var("topology_var1", ts);
	}

	free(databuf);
	return err;
}

static int perform_stat(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	double *databuf = NULL;
	int elem_size, xl, yl, zl, xu, yu, zu;
	int err;
	
	if (dims_ == 2) {
		databuf = allocate_2d();
		generate_2d(databuf, ts);
	} else if (dims_ == 3) {
		databuf = allocate_3d();
		generate_3d(databuf, ts);
	}

	elem_size = sizeof(double);
	generate_bbox(&xl, &yl, &zl, &xu, &yu, &zu);

	err = hstaging_put_var("stat_var1", ts, elem_size,
			xl, yl, zl, xu, yu, zu, databuf, &comm);
	if (err < 0) {
		free(databuf);
		return err;
	}

	if (rank_ == 0) {
		update_var("stat_var1", ts);
	}

	free(databuf);
	return err;
}
static int perform_viz(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	double *databuf = NULL;
	int elem_size, xl, yl, zl, xu, yu, zu;
	int err;
	
	if (dims_ == 2) {
		databuf = allocate_2d();
		generate_2d(databuf, ts);
	} else if (dims_ == 3) {
		databuf = allocate_3d();
		generate_3d(databuf, ts);
	}

	elem_size = sizeof(double);
	generate_bbox(&xl, &yl, &zl, &xu, &yu, &zu);

	err = hstaging_put_var("viz_var1", ts, elem_size,
			xl, yl, zl, xu, yu, zu, databuf, &comm);
	if (err < 0) {
		free(databuf);
		return err;
	}

	if (rank_ == 0) {
		update_var("viz_var1", ts);
	}

	free(databuf);
	return err;
}

int dummy_s3d_simulation(MPI_Comm comm, int num_ts, int npx, int npy, int npz, int spx, int spy, int spz, int dims)
{
	int err, i;

	MPI_Comm_size(comm, &nproc_);
	MPI_Comm_rank(comm, &rank_);
	if (rank_ == 0) {
		uloga("Dummy S3D: total %d workers\n", nproc_);
	}

	timestep_ = num_ts;
	npx_ = npx;
	npy_ = npy;
	npz_ = npz;
	if (npx_)
		spx_ = spx;
	if (npy_)
		spy_ = spy;
	if (npz_)
		spz_ = spz;
	dims_ = dims;

	double tm_st, tm_end;
	timer_init(&timer, 1);
	timer_start(&timer);

	for (i=1; i<=timestep_; i++) {
		tm_st = timer_read(&timer);

		MPI_Barrier(comm);

		/* Do computation */
		perform_computing(comm, nproc_,  rank_, i);

		if (i % FREQ != 0)
				continue;

		hstaging_put_sync_all();

		/* Do insitu analysis */
		err = perform_topology(comm, nproc_, rank_, i);
		if (err < 0) break;

		err = perform_stat(comm, nproc_, rank_, i);
		if (err < 0) break;

		err = perform_viz(comm, nproc_, rank_, i);
		if (err < 0) break;

		tm_end = timer_read(&timer);
		if (rank_ == 0) {
			uloga("Dummy S3D rank= %d ts= %d , time= %lf\n",
				rank_, i, tm_end-tm_st);
		}
	}

	hstaging_put_sync_all();

	MPI_Barrier(comm);
	return 0;
err_out:
	return -1;
}
