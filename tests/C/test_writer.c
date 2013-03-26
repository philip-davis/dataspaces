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

extern int test_put_run(int num_ts, int num_process,int process_x,int process_y,int process_z, int dims, int dim_x, int dim_y, int dim_z, MPI_Comm);

int main(int argc, char **argv)
{
        int err;
        int nprocs, rank;

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
        MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Barrier(MPI_COMM_WORLD);
	gcomm = MPI_COMM_WORLD;

#ifdef HAVE_DCMF
	MPI_Comm_split(MPI_COMM_WORLD, 1, rank, &gcomm);
#endif
	// Run as data writer
	test_put_run(iter,num_writer,writer_x,writer_y,writer_z,
		dims,dim_x,dim_y,dim_z,gcomm);

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Finalize();

	return 0;	
err_out:
	uloga("error out!\n");
	return -1;	
}

