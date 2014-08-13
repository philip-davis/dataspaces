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

#include "cods_api.h"

void epsi_coupling_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    uint32_t tid = 1; 
    int num_coupling_step = 5;
    // set task descriptors
    struct task_descriptor xgc1_task, xgca_task;
    strcpy(xgc1_task.conf_file, "xgc1.conf");
    strcpy(xgca_task.conf_file, "xgca.conf");

    int i;
    for (i = 0; i < num_coupling_step; i++) {
        xgc1_task.tid = tid++;
        cods_exec_task(&xgc1_task);
        cods_wait_task_completion(&xgc1_task);

        xgca_task.tid = tid++;
        cods_exec_task(&xgca_task);
        cods_wait_task_completion(&xgca_task);
    }  

    uloga("%s: finish workflow execution\n", __func__);
}

void dns_les_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    uint32_t dns_tid = 1, les_tid = 2;
    // set task descriptors
    struct task_descriptor dns_task, les_task;
    dns_task.tid = dns_tid;
    les_task.tid = les_tid;
    strcpy(dns_task.conf_file, "dns.conf");
    strcpy(les_task.conf_file, "les.conf");

    cods_exec_task(&dns_task);
    cods_exec_task(&les_task);
   
    cods_wait_task_completion(&dns_task); 
    cods_wait_task_completion(&les_task); 

    uloga("%s: finish workflow execution\n", __func__);
}

void s3d_analysis_workflow_driver(uint32_t wid, MPI_Comm comm)
{
    // set task descriptor
    struct task_descriptor s3d_task;
    s3d_task.tid = 1;
    strcpy(s3d_task.conf_file, "s3d.conf");
    cods_exec_task(&s3d_task);
    cods_wait_task_completion(&s3d_task);

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
        uloga("task submitter: num_submitter= %d example_workflow_id= %u\n", nproc, example_workflow_id);
    }
	err = cods_init(nproc, appid, cods_submitter); 

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

    cods_stop_framework();
	cods_finalize();

	MPI_Barrier(comm);
	MPI_Finalize();
	return 0;	
err_out:
	uloga("error out!\n");
	return -1;	
}
