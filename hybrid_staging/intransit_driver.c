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

#include "hstaging_partition.h"
#include "hybrid_staging_api.h"

extern int dummy_s3d_simulation(MPI_Comm comm, int num_ts);
extern int dummy_s3d_staging(MPI_Comm comm);

static enum execution_mode exemode_ = hybrid_staging_mode;
//static enum execution_mode exemode_ = staging_mode;
static enum core_type coretype_ = worker_core;
static enum location_type loctype_ = intransit;
static enum worker_type workertype_;

int main(int argc, char **argv)
{
	int err;
	int appid, num_ts;
	int nprocs, mpi_rank;

	if (argc != 2) {
		uloga("wrong number of args\n");
		return -1;
	}
	num_ts = atoi(argv[1]);
	
	hs_comm_init(argc, argv);
	nprocs = hs_comm_get_nprocs();
	mpi_rank = hs_comm_get_rank();
	appid = 2;
	err = ds_init(nprocs, IN_TRANSIT, appid);	

	err = hs_comm_perform_split(exemode_, coretype_, loctype_, &workertype_);
	if (err < 0) {
		goto err_out;
	}

	MPI_Comm comm = hs_get_communicator_l2();
	int num_worker = 0;
	MPI_Comm_size(comm, &num_worker);
	switch (workertype_) {
	case simulation_worker:
		uloga("in-transit driver: should not happen!\n");
		goto err_out;
		break;
	case staging_worker:
		uloga("in-transit staging: num_worker= %d\n", num_worker);
		err = dummy_s3d_staging(comm);
		if (err < 0)
			goto err_out;
		break;
	default:
		uloga("Wrong workertype %u\n", workertype_);
		return -1;
	}

	ds_finalize();
	hs_comm_fin();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
