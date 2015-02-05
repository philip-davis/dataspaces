#include <stdio.h>

#include "dcmf.h"
#include "common.h"

#include "mpi.h"

int main(int argc, char **argv)
{
        int err;
        int mpi_nprocs, mpi_rank;
        MPI_Comm gcomm;

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

        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        MPI_Barrier(MPI_COMM_WORLD);

#ifdef HAVE_DCMF
        MPI_Comm_split(MPI_COMM_WORLD, 4, mpi_rank, &gcomm);
	uloga("Idle process ...\n");
#endif

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();

        return 0;
err_out:
        uloga("error out!\n");
        return -1;
}
