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
*  Fan Zhang (2014)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#include <stdio.h>
#include <stdint.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

#include "hstaging_api.h"

extern int dummy_epsi_coupling_workflow(MPI_Comm comm);
extern int dummy_s3d_analysis_workflow(MPI_Comm comm);
extern int dummy_dns_les_workflow(MPI_Comm comm);

int main(int argc, char **argv)
{
	int err;
	int appid, rank, nproc;
    uint32_t example_workflow_id = 0;
    MPI_Comm comm;

    if (argc != 2) {
        uloga("%s: wrong number of arguments!\n", argv[0]);
        return -1;
    }
    example_workflow_id = atol(argv[1]);

	appid = 1;
	MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_split(MPI_COMM_WORLD, appid, rank, &comm);
	MPI_Comm_size(comm, &nproc);

    if (rank == 0) {
        uloga("DAG execution: workflow_id=%u num_worker= %d\n", example_workflow_id, nproc);
    }
	err = hstaging_init(nproc, appid, hs_executor); 

    switch (example_workflow_id) {
    case EPSI_WORKFLOW_ID:
        err = dummy_epsi_coupling_workflow(comm);
        break;
    case DNS_LES_WORKFLOW_ID:
        err = dummy_dns_les_workflow(comm);
        break;
    case S3D_WORKFLOW_ID:
        err = dummy_s3d_analysis_workflow(comm);
        break;
    default:
        err = 0;
        uloga("ERROR invalid workflow_id %u\n", example_workflow_id);
        break;
    }
	if (err < 0)
		goto err_out;

	hstaging_finalize();

	MPI_Barrier(comm);
	MPI_Finalize();
	return 0;	
err_out:
	uloga("error out!\n");
	return -1;	
}
