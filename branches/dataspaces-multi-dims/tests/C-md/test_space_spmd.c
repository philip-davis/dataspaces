#include <stdio.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

//extern int test_put_run(int num_ts, int num_process,int process_x,int process_y,int process_z, int p4, int dims, int dim_x, int dim_y, int dim_z, int dim4, MPI_Comm);
extern int test_put_run(int num_ts, int num_process, int* process, int dims, int* dim, MPI_Comm);

//extern int test_get_run(int num_ts, int num_process,int process_x,int process_y,int process_z, int p4, int dims, int dim_x, int dim_y, int dim_z, int dim4, MPI_Comm);
extern int test_get_run(int num_ts, int num_process, int* process, int dims, int* dim, MPI_Comm);

int main(int argc, char **argv)
{
	int err;
	int mpi_nprocs, mpi_rank;
	
	int num_sp, num_cp, iter;
	int num_writer, writer[10] = {0}; //writer_x,writer_y,writer_z, w4; 
	int num_reader, reader[10] = {0}; //reader_x,reader_y,reader_z, r4;
	int dims, dim[10] = {0}; //dim_x, dim_y, dim_z, dim4;
	MPI_Comm gcomm;

        if(read_config_file("computenode.conf",
                &num_sp, &num_cp, &iter,
                &num_writer, writer, //&writer_x, &writer_y, &writer_z, &w4,
                &num_reader, reader, //&reader_x, &reader_y, &reader_z, &r4,
                &dims, dim//&dim_x, &dim_y, &dim_z, &dim4
	) != 0) {
                goto err_out;
        }
	// Using SPMD style programming
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);	
	MPI_Barrier(MPI_COMM_WORLD);

        //printf("double=%d, unsigned longlong=%d, unsigned long=%d, unsigned int=%d, int=%d, unsigned char=%d\n",
        //        sizeof(double), sizeof(unsigned long long), sizeof(unsigned long), sizeof(unsigned int), sizeof(int), sizeof(unsigned char));

	if (mpi_rank < num_sp){
		MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, &gcomm);
		// Run as data space servers
		printf("start common_run_server\n");
		common_run_server(num_sp, num_cp, USE_DSPACES);
		//common_run_server(num_sp, num_cp, USE_DIMES);
	} else if (num_sp <= mpi_rank && mpi_rank < num_sp+num_writer) {
		MPI_Comm_split(MPI_COMM_WORLD, 1, mpi_rank, &gcomm);
		// Run as data writer
		sleep(15); // wait for the dataspace servers to init.
	
		printf("start test_put_run\n");
		test_put_run(iter,num_writer,writer,//writer_x,writer_y,writer_z,w4,
			dims, dim ,gcomm);
	} else if (num_sp+num_writer <= mpi_rank &&
		   mpi_rank < num_sp+num_writer+num_reader) {
		MPI_Comm_split(MPI_COMM_WORLD, 2, mpi_rank, &gcomm);
		// Run as data reader
		sleep(15); // wait for the dataspace servers to init.
	
#ifdef USE_LUA_REXEC
		test_lua_rexec(iter,num_reader,reader_x,reader_y,reader_z,
			dims,dim_x,dim_y,dim_z);
#else
		printf("start test_get_run\n");
		test_get_run(iter,num_reader, reader, //reader_x,reader_y,reader_z,r4,
			dims,dim,gcomm);
#endif
	} else {
		MPI_Comm_split(MPI_COMM_WORLD, 3, mpi_rank, &gcomm);
		uloga("Idle process!\n");
	}

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
