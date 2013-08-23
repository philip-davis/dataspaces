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

#include "hybrid_staging_api.h"

extern int dummy_s3d_simulation(MPI_Comm comm, int num_ts);
extern int dummy_s3d_staging_serial_job(MPI_Comm comm);
extern int dummy_s3d_staging_parallel_job(MPI_Comm comm);

static enum execution_mode exemode_ = hs_hybrid_staging_mode;
static enum location_type loctype_ = hs_insitu;
static enum core_type coretype_ = hs_worker_core;
static enum worker_type workertype_ = hs_staging_worker;

int main(int argc, char **argv)
{
	int err;
	int appid, nproc;
	int num_insitu_proc, num_intransit_proc;

	if (argc != 2) {
		uloga("wrong number of args\n");
		return -1;
	}

	appid = atoi(argv[1]);

	MPI_Init(&argc, &argv);
	MPI_Comm comm = MPI_COMM_WORLD;
	MPI_Comm_size(comm, &nproc);

	err = ds_init(nproc, workertype_, appid);             
    
	uloga("in-situ staging: num_worker= %d\n", nproc);
	err = dummy_s3d_staging_parallel_job(comm);
	if (err < 0)
		goto err_out;

	ds_finalize();

	MPI_Barrier(comm);
	MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;
}
