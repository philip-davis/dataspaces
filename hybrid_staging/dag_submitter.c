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

#include "debug.h"
#include "common.h"

#include "mpi.h"

#include "hstaging_api.h"

void epsi_coupling_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    uint32_t tid = 1; 
    int num_coupling_step = 5;
    int i;
    for (i = 0; i < num_coupling_step; i++) {
        hstaging_submit_task(wid, tid++, "xgc1.conf");
        hstaging_submit_task(wid, tid++, "xgca.conf");
    }  

    hstaging_set_workflow_finished(wid);
    uloga("%s: finish workflow execution\n", __func__);
}

void dns_les_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    uint32_t dns_tid = 1, les_tid = 2;
    hstaging_submit_task_nb(wid, dns_tid, "dns.conf");
    hstaging_submit_task_nb(wid, les_tid, "les.conf");
    
    hstaging_wait_submitted_task(wid, dns_tid);
    hstaging_wait_submitted_task(wid, les_tid); 

    hstaging_set_workflow_finished(wid);
    uloga("%s: finish workflow execution\n", __func__);
}

void s3d_analysis_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    uint32_t tid = 1;
    hstaging_submit_task(wid, tid, "s3d.conf");
    hstaging_set_workflow_finished(wid);

    uloga("%s: finish workflow execution\n", __func__);
}

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

	appid = 2;
	MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_split(MPI_COMM_WORLD, appid, rank, &comm);
	MPI_Comm_size(comm, &nproc);

    if (rank == 0) {
        uloga("DAG submitter: num_submitter= %d example_workflow_id= %u\n", nproc, example_workflow_id);
    }
	err = hstaging_init(nproc, appid, hs_submitter); 

    hstaging_build_staging(1, "staging.conf");

    switch (example_workflow_id) {
    case EPSI_WORKFLOW_ID:
        epsi_coupling_workflow_driver(example_workflow_id, comm);
        break;
    case DNS_LES_WORKFLOW_ID:
        dns_les_workflow_driver(example_workflow_id, comm);
        break;
    case S3D_WORKFLOW_ID:
        s3d_analysis_workflow_driver(example_workflow_id, comm);
        break;
    default:
        uloga("ERROR invalid workflow_id %u\n", example_workflow_id);
        break;
    }

    hstaging_stop_framework();
	hstaging_finalize();

	MPI_Barrier(comm);
	MPI_Finalize();
	return 0;	
err_out:
	uloga("error out!\n");
	return -1;	
}
