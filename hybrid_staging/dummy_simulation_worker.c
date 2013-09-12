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
static struct g_info g;

static int update_var(const char *var_name, int ts)
{
	int err;
	struct var_descriptor var_desc;
	int *c = NULL;
	size_t elem_size = sizeof(double);

	strcpy(var_desc.var_name, var_name);		
	var_desc.step = ts;
	var_desc.bb.num_dims = g.dims;
	var_desc.size = elem_size;
	c = var_desc.bb.lb.c;
	c[0] = 0;
	c[1] = 0;
	c[2] = 0;
	c = var_desc.bb.ub.c;	
	c[0] = (g.npx * g.spx) - 1;
	c[1] = (g.npy * g.spy) - 1;
	c[2] = (g.npz * g.spz) - 1;
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

static int write_output_data(MPI_Comm comm, int ts, const char *var_name)
{
	double *databuf = NULL;
	int elem_size, xl, yl, zl, xu, yu, zu;
	int err;
	int num_elem;
	
	if (g.dims == 2) {
		num_elem = g.spx*g.spy;
	} else if (g.dims == 3) {
		num_elem = g.spx*g.spy*g.spz;
	}
	databuf = allocate_data(num_elem);
	generate_data(databuf, ts, num_elem);
	elem_size = sizeof(double);
	generate_bbox(&g, &xl, &yl, &zl, &xu, &yu, &zu);

	err = hstaging_put_var(var_name, ts, elem_size,
			xl, yl, zl, xu, yu, zu, databuf, &comm);
	if (err < 0) {
		free(databuf);
		return err;
	}

	if (g.rank == 0) {
		update_var(var_name, ts);
	}

	free(databuf);
	return err;
}

static int perform_topology(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);
	return write_output_data(comm, ts, "topology_var1");
}

static int perform_stat(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);
	return write_output_data(comm, ts, "stat_var1");
}
static int perform_viz(MPI_Comm comm, int num_peers, int rank, int ts)
{
	usleep(millisec*1000);
	return write_output_data(comm, ts, "viz_var1");
}

int dummy_s3d_simulation(MPI_Comm comm, int num_ts, int npx, int npy, int npz, int spx, int spy, int spz, int dims)
{
	int err, i;

	MPI_Comm_size(comm, &g.nproc);
	MPI_Comm_rank(comm, &g.rank);
	if (g.rank == 0) {
		uloga("Dummy S3D: total %d workers\n", g.nproc);
	}

	g.timestep = num_ts;
	g.npx = npx;
	g.npy = npy;
	g.npz = npz;
	if (g.npx)
		g.spx = spx;
	if (g.npy)
		g.spy = spy;
	if (g.npz)
		g.spz = spz;
	g.dims = dims;

	double tm_st, tm_end;
	timer_init(&timer, 1);
	timer_start(&timer);

	for (i=1; i<=g.timestep; i++) {
		tm_st = timer_read(&timer);

		MPI_Barrier(comm);

		/* Do computation */
		perform_computing(comm, g.nproc,  g.rank, i);

		if (i % FREQ != 0)
				continue;

		hstaging_put_sync_all();

		/* Do insitu analysis */
		err = perform_topology(comm, g.nproc, g.rank, i);
		if (err < 0) break;

		err = perform_stat(comm, g.nproc, g.rank, i);
		if (err < 0) break;

		err = perform_viz(comm, g.nproc, g.rank, i);
		if (err < 0) break;

		tm_end = timer_read(&timer);
		if (g.rank == 0) {
			uloga("Dummy S3D rank= %d ts= %d , time= %lf\n",
				g.rank, i, tm_end-tm_st);
		}
	}

	hstaging_put_sync_all();

	MPI_Barrier(comm);
	return 0;
err_out:
	return -1;
}
