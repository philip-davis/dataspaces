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

//extern int test_get_run(int npapp, int npx, int npy, int npz,
//                int spx, int spy, int spz, int timestep, int dims, MPI_Comm);

extern int test_get_run(int num_ts, int num_process, int* process, int dims, int* dim, MPI_Comm);


int main(int argc, char **argv)
{
        int err;
        int nprocs, rank;
/*
        int npapp, npx, npy, npz;
        int spx, spy, spz;
        int dims, timestep;
        MPI_Comm gcomm;

        if (parse_args(argc, argv, &npapp, &npx, &npy, &npz,
                &spx, &spy, &spz, &timestep) != 0) {
                goto err_out;
        }
*/
/*        int num_sp, num_cp, iter;
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
*/

        int num_reader, np[10] = {0}, sp[10] = {0};
        int ndim, iter;
        MPI_Comm gcomm;
        if(parse_args(argc, argv, &ndim, &num_reader, np, sp, &iter) != 0)
                goto err_out;


        // Using SPMD style programming
        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Barrier(MPI_COMM_WORLD);
	gcomm = MPI_COMM_WORLD;

#ifdef DEBUG
        uloga("client reader starts...\n");
#endif
	// Run as data reader
	//dims = 3;
        //test_get_run(npapp, npx, npy, npz,
        //        spx, spy, spz, timestep, dims, gcomm);
        //test_get_run(iter,num_reader,reader,dims, dim ,gcomm);
        test_get_run(iter,num_reader,np,ndim,sp,gcomm);
	

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
