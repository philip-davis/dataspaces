#include <stdio.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

extern int test_get_run(int num_ts, int num_process,int process_x,int process_y,int process_z, int dims, int dim_x, int dim_y, int dim_z, MPI_Comm);

int main(int argc, char **argv)
{
        int err;
        int mpi_nprocs, mpi_rank;

        int num_sp, num_cp, iter;
        int num_writer,writer_x,writer_y,writer_z;
        int num_reader,reader_x,reader_y,reader_z;
        int dims, dim_x, dim_y, dim_z;
        MPI_Comm gcomm;

        if(read_config_file("computenode.conf",
                &num_sp, &num_cp, &iter,
                &num_writer, &writer_x, &writer_y, &writer_z,
                &num_reader, &reader_x, &reader_y, &reader_z,
                &dims, &dim_x, &dim_y, &dim_z) != 0) {
                goto err_out;
        }

        // Using SPMD style programming
        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        MPI_Barrier(MPI_COMM_WORLD);

#ifdef HAVE_DCMF
	if ( mpi_rank >= (num_sp+num_writer) &&
	     mpi_rank < (num_sp+num_writer+num_reader) ) {
                MPI_Comm_split(MPI_COMM_WORLD, 2, mpi_rank, &gcomm);
                // Run as data reader
                sleep(15); // wait for the dataspace servers to init.

                test_get_run(iter,num_reader,reader_x,reader_y,reader_z,
                        dims,dim_x,dim_y,dim_z,gcomm);
	} else {
		uloga("Error: test_get_run wrong number processes\n");
	}
#else
	if (mpi_rank < num_reader) {
                MPI_Comm_split(MPI_COMM_WORLD, 2, mpi_rank, &gcomm);
                // Run as data reader
                test_get_run(iter,num_reader,reader_x,reader_y,reader_z,
                        dims,dim_x,dim_y,dim_z,gcomm);
	}
#endif
	
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}

