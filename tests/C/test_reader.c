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
#include <stdint.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

extern int test_get_run(enum transport_type, int npapp, int npx,int npy,int npz,
        uint64_t spx, uint64_t spy, uint64_t spz, int timestep, int dims, size_t elem_size,
        int num_vars, MPI_Comm gcomm);

int main(int argc, char **argv)
{
	int err;
	int nprocs, rank;
	MPI_Comm gcomm;

    // Usage: ./test_reader type npapp npx npy npz spx spy spz timestep dims elem_size num_vars
    // Command line arguments
    enum transport_type type; // DATASPACES or DIMES
	int npapp; // number of application processes
    int npx, npy, npz; // number of processes in x,y,z direction
	uint64_t spx, spy, spz; // block size per process in x,y,z direction
    int timestep; // number of iterations
	int dims; // Optional: 2 or 3. Default value is 3.
    size_t elem_size; // Optional: size of one element in the global array. Default value is 8 (bytes).
    int num_vars; // Optional: number of variables to be shared in the testing. Default value is 1.

	if (parse_args(argc, argv, &type, &npapp, &npx, &npy, &npz,
		&spx, &spy, &spz, &timestep, &dims, &elem_size, &num_vars) != 0) {
		goto err_out;
	}

	// Using SPMD style programming
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);
	gcomm = MPI_COMM_WORLD;

	int color = 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, rank, &gcomm);

	// Run as data reader
	test_get_run(type, npapp, npx, npy, npz,
		spx, spy, spz, timestep, dims, elem_size, num_vars, gcomm);
	
	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
