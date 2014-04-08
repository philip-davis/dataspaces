/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Fan Zhang (2013)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#include <stdio.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

//extern int test_put_run(int npapp, int npx, int npy, int npz,
  //              int spx, int spy, int spz, int timestep, int dims, MPI_Comm);
//extern int test_get_run(int npapp, int npx, int npy, int npz,
  //              int spx, int spy, int spz, int timestep, int dims, MPI_Comm);
extern int test_put_run(enum transport_type type, int npapp, int npx, int npy, 
		int npz, int spx, int spy, int spz, int timestep, int dims,
		size_t elem_size, int num_vars, MPI_Comm gcomm);
extern int test_get_run(enum transport_type type, int npapp, int npx, int npy, 
		int npz, int spx, int spy, int spz, int timestep, int dims, 
		size_t elem_size, int num_vars, MPI_Comm gcomm);

int main(int argc, char **argv)
{
	int err;
	int mpi_nprocs, mpi_rank;

        int npapp, npx, npy, npz;
        int spx, spy, spz;
	
	int num_sp, num_cp;
	int num_writer,writer_x,writer_y,writer_z; 
	int num_reader,reader_x,reader_y,reader_z;
	int timestep, dims, dim_x, dim_y, dim_z;
	MPI_Comm gcomm;

        if(read_config_file("computenode.conf",
                &num_sp, &num_cp, &timestep,
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

	if (mpi_rank < num_sp){
		// Run as data space servers
		MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, &gcomm);
#ifdef DS_HAVE_DIMES
		common_run_server(num_sp, num_cp, USE_DIMES, &gcomm);
#else
		common_run_server(num_sp, num_cp, USE_DSPACES, &gcomm);
#endif
	} else if (num_sp <= mpi_rank && mpi_rank < num_sp+num_writer) {
		// Run as data writer
		MPI_Comm_split(MPI_COMM_WORLD, 1, mpi_rank, &gcomm);

		npapp = num_writer;
		npx = writer_x;
		npy = writer_y;
		npz = writer_z;
		if (dims >= 2) {
			spx = dim_x / npx;
			spy = dim_y / npy;
		}

		if (dims >= 3) {
			spz = dim_z/ npz;
		}
	
		//test_put_run(USE_DSPACES, npapp, npx, npy, npz,
		test_put_run(USE_DIMES, npapp, npx, npy, npz,
			spx, spy, spz, timestep, dims, 8, 1, gcomm);
	} else if (num_sp+num_writer <= mpi_rank &&
		   mpi_rank < num_sp+num_writer+num_reader) {
		// Run as data reader
		MPI_Comm_split(MPI_COMM_WORLD, 2, mpi_rank, &gcomm);

		npapp = num_reader;
		npx = reader_x;
		npy = reader_y;
		npz = reader_z;
		if (dims >= 2) {
			spx = dim_x / npx;
			spy = dim_y / npy;
		}

		if (dims >= 3) {
			spz = dim_z/ npz;
		}

		//test_get_run(USE_DSPACES, npapp, npx, npy, npz,
		test_get_run(USE_DIMES, npapp, npx, npy, npz,
			spx, spy, spz, timestep, dims, 8, 1, gcomm);	
	} else {
		MPI_Comm_split(MPI_COMM_WORLD, 3, mpi_rank, &gcomm);
		uloga("Idle process!\n");
	}

	uloga("before barrier rank %d is finished\n", mpi_rank);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
