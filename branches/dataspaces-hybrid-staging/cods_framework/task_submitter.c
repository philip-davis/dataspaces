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
#include <stdlib.h>

#include "debug.h"
#include "common.h"

#include "mpi.h"

#include "cods_api.h"

int partition_task_executors(
    int num_sim_node,
    int num_intransit_node,
    int num_sim_executor_per_node,
    int num_insitu_colocated_executor_per_node)
{
    struct executor_pool_info *pool_info = NULL;
    int pool_id = 1;
    pool_info = cods_get_executor_pool_info(pool_id);
    if (!pool_info) {
        uloga("%s(): ERROR failed to get executor pool information\n", __func__);
        return -1;
    }

    int i, j, k;
    // partition task executors in programmer-specified way
    i = 0;
    for (j = 0; j < num_sim_node; j++, i++) {
        for (k = 0; k < pool_info->node_tab[i].node_num_executor; k++) {
            enum programmer_defined_partition_type part_type;
            if (k < num_sim_executor_per_node) {
                part_type = simulation_executor;
            } else { 
                part_type = insitu_colocated_executor;
            }
            pool_info->node_tab[i].node_executor_tab[k].partition_type = part_type;
        }
    }
    for (j = 0; j < num_intransit_node; j++, i++) {
        for (k = 0; k < pool_info->node_tab[i].node_num_executor; k++) {
            pool_info->node_tab[i].node_executor_tab[k].partition_type = intransit_executor;
        }
    }

    // print executor pool information
    uloga("pool_id= %d num_node= %d num_executor= %u\n", pool_info->pool_id, pool_info->num_node,
        pool_info->num_executor);
    for (i = 0; i < pool_info->num_node; i++) {
        uloga("compute node nid= %u node_num_executor= %d\n", pool_info->node_tab[i].topo_info.nid,
            pool_info->node_tab[i].node_num_executor);
        for (j = 0; j < pool_info->node_tab[i].node_num_executor; j++) {
            uloga("executor bk_idx= %d dart_id= %d node_id= %u part_type= %u\n", 
                pool_info->node_tab[i].node_executor_tab[j].bk_idx,
                pool_info->node_tab[i].node_executor_tab[j].dart_id,
                pool_info->node_tab[i].node_executor_tab[j].topo_info.nid,
                pool_info->node_tab[i].node_executor_tab[j].partition_type); 
        }
    }

    cods_build_partition(pool_info);
    free(pool_info->node_tab);
    free(pool_info->executor_tab);
    free(pool_info);
}

void epsi_coupling_workflow_driver(MPI_Comm comm)
{
    int num_coupling_step = 5;
    // set task descriptors
    struct task_descriptor xgc1_task, xgca_task;
    init_task_descriptor(&xgc1_task, 1, "xgc1.conf");
    init_task_descriptor(&xgca_task, 2, "xgca.conf");

    int i;
    for (i = 0; i < num_coupling_step; i++) {
        xgc1_task.tid += 2;
        cods_exec_task(&xgc1_task);
        cods_wait_task_completion(xgc1_task.tid);

        xgca_task.tid += 2;
        cods_exec_task(&xgca_task);
        cods_wait_task_completion(xgca_task.tid);
    }  

    uloga("%s: finish workflow execution\n", __func__);
}

void dns_les_workflow_driver(MPI_Comm comm)
{
    uint32_t dns_tid = 1, les_tid = 2;
    // set task descriptors
    struct task_descriptor dns_task, les_task;
    init_task_descriptor(&dns_task, dns_tid, "dns.conf");
    init_task_descriptor(&les_task, les_tid, "les.conf");

    cods_exec_task(&dns_task);
    cods_exec_task(&les_task);
   
    cods_wait_task_completion(dns_task.tid); 
    cods_wait_task_completion(les_task.tid); 

    uloga("%s: finish workflow execution\n", __func__);
}

void s3d_analysis_workflow_driver(MPI_Comm comm)
{
    // set task descriptor
    struct task_descriptor s3d_task;
    init_task_descriptor(&s3d_task, 1, "s3d.conf");
    s3d_task.location_hint = simulation_executor;

    cods_exec_task(&s3d_task);
    cods_wait_task_completion(s3d_task.tid);

    uloga("%s: finish workflow execution\n", __func__);
}

int main(int argc, char **argv)
{
	int err;
	int appid, rank, nproc;
    uint32_t example_workflow_id = 0;
    int num_sim_node = 0;
    int num_intransit_node = 0;
    int num_sim_executor_per_node = 0;
    int num_insitu_colocated_executor_per_node = 0;
    int does_partition = 0;
    MPI_Comm comm;

    if (argc != 2 && argc != 6) {
        uloga("%s: wrong number of arguments!\n", argv[0]);
        return -1;
    }
    example_workflow_id = atol(argv[1]);
    if (argc == 6) {
        num_sim_node = atoi(argv[2]);
        num_intransit_node = atoi(argv[3]);
        num_sim_executor_per_node = atoi(argv[4]);
        num_insitu_colocated_executor_per_node = atoi(argv[5]);
        does_partition = 1;
    }        

	appid = 2;
	MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_split(MPI_COMM_WORLD, appid, rank, &comm);
	MPI_Comm_size(comm, &nproc);

    if (rank == 0) {
        uloga("task submitter: num_submitter= %d example_workflow_id= %u\n", nproc, example_workflow_id);
    }
	err = cods_init(nproc, appid, cods_submitter); 

    if (does_partition) {
        partition_task_executors(num_sim_node, num_intransit_node,
            num_sim_executor_per_node, num_insitu_colocated_executor_per_node);
    }

    switch (example_workflow_id) {
    case DUMMY_EPSI_WORKFLOW_ID:
        epsi_coupling_workflow_driver(comm);
        break;
    case DUMMY_DNS_LES_WORKFLOW_ID:
        dns_les_workflow_driver(comm);
        break;
    case DUMMY_S3D_WORKFLOW_ID:
        s3d_analysis_workflow_driver(comm);
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
