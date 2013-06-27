#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "common.h"
#include "dataspaces_api.h"
#include "mpi.h"

static struct timer timer;

#define SIZE_OBJ 16*1024
#define NUM_ROWS 3
#define FREQ 1 

static const int millisec = 1000;

double * alloc_data(size_t size, int type, int rank, int ts)
{
	unsigned int num_double = size/sizeof(double);
	double value = type*10000 + rank + 0.00001*ts;
	size_t d = num_double * sizeof(double);
	double *tmp = (double *)malloc(d);
	if(tmp == NULL)
		printf("malloc error.\n");

	unsigned int i;
	for (i=0; i<num_double; i++)
		tmp[i] = value;

	return tmp;
}

/* dummy code, exchange data with neighbors */
static int perform_computing(MPI_Comm comm_new, int numprocs, int myid, int ts)
{
	usleep(millisec*1000);
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

static int perform_topology(int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	//put data
	struct data_descriptor desc;
	//size varies from 8kb ~ 64kb
	desc.size = SIZE_OBJ; //(1 + rank % 8) * SIZE_OBJ;
	desc.rank = rank;
	desc.tstep = ts;
	desc.type = TOPOLOGY;
	desc.num_obj = num_peers;

	double *data = alloc_data(desc.size, TOPOLOGY, rank, ts);

	ds_put_obj_data((void *)data, &desc);

	free(data);
	return 0;
}

static int perform_stat_v1(int num_peers, int rank, int ts)
{
	usleep(millisec*1000);
	//package up data
	struct stats {
		double mean;
		double mom2;
		double mom3;
		double mom4;
		double minF;
		double maxF;
	};

	struct variables {
		int numSamples;
		int numStats;
		struct stats st[0];
	};

	struct data_descriptor desc;
	desc.size = 2*sizeof(int) + 6*NUM_ROWS*sizeof(double);
	desc.rank = rank;
	desc.tstep = ts;
	desc.type = DESCRIPTIVE_STATS;
	desc.num_obj = num_peers;

	void *buf = malloc(desc.size);
	//char *buf = new char[desc.size];
	struct variables *data = (struct variables *)buf;
	data->numSamples = rank;
	data->numStats = NUM_ROWS;

	int r;
	for ( r = 0; r < NUM_ROWS; ++ r )
	{
		data->st[r].mean = ts + 0.00001*rank;;
		data->st[r].mom2 = ts + 0.00001*rank;;
		data->st[r].mom3 = ts + 0.00001*rank;;
		data->st[r].mom4 = ts + 0.00001*rank;;
		data->st[r].minF = ts + 0.00001*rank;;
		data->st[r].maxF = ts + 0.00001*rank;;
	}

	ds_put_obj_data((void *)data, &desc);

	free(buf);
	//delete []buf;
	return 0;
}

static int perform_stat(int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	//put data
	struct data_descriptor desc;
	//size varies from 8kb ~ 64kb
	desc.size = SIZE_OBJ; //SIZE_OBJ * (1 + rank % 8);
	desc.rank = rank;
	desc.tstep = ts;
	desc.type = DESCRIPTIVE_STATS;
	desc.num_obj = num_peers;

	double *data = alloc_data(desc.size, DESCRIPTIVE_STATS, rank, ts);

	ds_put_obj_data((void *)data, &desc);

	free(data);
	return 0;

}

static int perform_viz(int num_peers, int rank, int ts)
{
	usleep(millisec*1000);

	//put data
	struct data_descriptor desc;
	//size varies from 8kb ~ 64kb
	desc.size = SIZE_OBJ; //SIZE_OBJ * (1 + rank % 8);
	desc.rank = rank;
	desc.tstep = ts;
	desc.type = VISUALIZATION;
	desc.num_obj = num_peers;

	double *data = alloc_data(desc.size, VISUALIZATION, rank, ts);

	ds_put_obj_data((void *)data, &desc);

	free(data);
	return 0;
}

int dummy_s3d_simulation(MPI_Comm comm, int num_peers, int num_ts)
{
	int err, i;
	int nprocs, mpi_rank;

	MPI_Comm_size(comm, &nprocs);
	MPI_Comm_rank(comm, &mpi_rank);
	uloga("Dummy S3D: mpi_rank= %d of total %d\n", mpi_rank, nprocs);

	err = ds_init(num_peers, IN_SITU);

	double tm_st, tm_end;
	timer_init(&timer, 1);
	timer_start(&timer);

	for (i=1; i<=num_ts; i++) {
		tm_st = timer_read(&timer);

		MPI_Barrier(comm);

		/* Do computation */
		perform_computing(comm, nprocs,  mpi_rank, i);

		if (i % FREQ != 0)
				continue;

		/* Do insitu analysis */
		perform_topology(num_peers, mpi_rank, i);

		perform_stat(num_peers, mpi_rank, i);
		//perform_stat_v1(num_peers, mpi_rank, i);

		perform_viz(num_peers, mpi_rank, i);

		tm_end = timer_read(&timer);
		if (mpi_rank == 0)
			uloga("Dummy S3D rank= %d ts= %d , time= %lf\n",
					mpi_rank, i, tm_end-tm_st);
	}

	MPI_Barrier(comm);

	ds_finalize();

	return 0;
err_out:
	return -1;
}
