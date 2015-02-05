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
#include <getopt.h>
#include "unistd.h"
#include "mpi.h"

#include "cods_internal.h"

#include "dart.h"
#include "dimes_server.h"

#include "common.h"

static int num_sp;
static int num_cp;
static char *conf;

struct cods_information_space {
    // keep the workflow manager dart id, and enable message forwarding
    // after workflow manager dart id is disseminated to all information 
    // space servers. 
    int manager_dart_id;
    unsigned char has_manager_info;
    struct list_head pending_msg_list;
    struct dimes_server *dimes_s;
};
static struct cods_information_space *info_space = NULL;

#define DART_DS info_space->dimes_s->dsg->ds
#define DART_RPC_S info_space->dimes_s->dsg->ds->rpc_s
#define DART_ID info_space->dimes_s->dsg->ds->rpc_s->ptlmap.id

static int callback_add_pending_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    // uloga("%s(): server #%d got msg from client #%d\n", __func__, DART_ID, cmd->id);
    struct pending_msg *p = malloc(sizeof(*p));
    memcpy(&p->cmd, cmd, sizeof(struct rpc_cmd));
    list_add_tail(&p->entry, &info_space->pending_msg_list);
    return 0;
}

static int callback_cods_update_manager_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    info_space->manager_dart_id = cmd->id;
    info_space->has_manager_info = 1;
    uloga("%s(): server #%d got manager dart id %d.\n",
        __func__, DART_ID, info_space->manager_dart_id);

    return 0;
}

static int forward_msg_to_peer(struct pending_msg *p, int dart_id)
{
    int err = -ENOMEM;
    struct msg_buf *msg;
    struct node_id *peer;
    peer = ds_get_peer(DART_DS, dart_id);
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg) goto err_out;

    // copy rpc_cmd
    memcpy(msg->msg_rpc, &p->cmd, sizeof(struct rpc_cmd));
    msg->msg_rpc->id = DART_ID;    

    err = rpc_send(DART_RPC_S, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int process_pending_msg()
{
    struct pending_msg *p, *t;
    list_for_each_entry_safe(p, t, &info_space->pending_msg_list, struct pending_msg, entry)
    {
        int err = 0;
        switch (p->cmd.cmd) {
        // Messages that need to be forwarded to manager server.
        case cods_reg_resource_msg:
        case cods_get_executor_pool_info_msg:
        case cods_build_partition_msg:
        case cods_stop_framework_msg:
        case cods_submit_task_msg:
        case cods_update_var_msg:
        case cods_finish_task_msg:
            err = forward_msg_to_peer(p, info_space->manager_dart_id);
            break;
        // Other messages ...
        case cods_stop_executor_msg:
            {
                struct hdr_stop_executor *hdr = p->cmd.pad;
                err = forward_msg_to_peer(p, hdr->dart_id);
            }
            break;
        case cods_executor_pool_info_msg:
            {
                struct hdr_executor_pool_info *hdr = p->cmd.pad;
                err = forward_msg_to_peer(p, hdr->dst_dart_id);
            }
            break;
        case cods_build_partition_done_msg:
            {
                struct hdr_build_partition_done *hdr = p->cmd.pad;
                err = forward_msg_to_peer(p, hdr->dst_dart_id);
            }
            break;
        case cods_exec_task_msg:
            {
                struct hdr_exec_task *hdr = p->cmd.pad;
                err = forward_msg_to_peer(p, hdr->dart_id); 
            }
            break;
        case cods_submitted_task_done_msg: 
            {
                struct hdr_submitted_task_done *hdr = p->cmd.pad;
                err = forward_msg_to_peer(p, hdr->submitter_dart_id);
            }
            break;
        default:
            uloga("%s(): unknown message type %u\n", __func__, p->cmd.cmd);
            break;
        }

        if (err == 0) {
            list_del(&p->entry);
            free(p);
            break; // process one msg at a time.
        }
    }
    return 0;    
}

int info_space_init()
{
    info_space = malloc(sizeof(*info_space));
    if (!info_space) return -1;

    // init 
    info_space->manager_dart_id = 0;
    info_space->has_manager_info = 0;
    info_space->dimes_s = NULL;
    INIT_LIST_HEAD(&info_space->pending_msg_list);

    // Messages sent from task executors
    rpc_add_service(cods_reg_resource_msg, callback_add_pending_msg);
    rpc_add_service(cods_update_var_msg, callback_add_pending_msg);
    rpc_add_service(cods_finish_task_msg, callback_add_pending_msg);
    // Messages sent from task submitter
    rpc_add_service(cods_get_executor_pool_info_msg, callback_add_pending_msg);
    rpc_add_service(cods_build_partition_msg, callback_add_pending_msg);
    rpc_add_service(cods_stop_framework_msg, callback_add_pending_msg);
    rpc_add_service(cods_submit_task_msg, callback_add_pending_msg);
    // Messages sent from workflow manager
    rpc_add_service(cods_update_manager_info_msg, callback_cods_update_manager_info);
    rpc_add_service(cods_stop_executor_msg, callback_add_pending_msg);
    rpc_add_service(cods_executor_pool_info_msg, callback_add_pending_msg);
    rpc_add_service(cods_build_partition_done_msg, callback_add_pending_msg);
    rpc_add_service(cods_exec_task_msg, callback_add_pending_msg);
    rpc_add_service(cods_submitted_task_done_msg, callback_add_pending_msg);

    info_space->dimes_s = dimes_server_alloc(num_sp, num_cp, conf);
    if (!info_space->dimes_s) {
        free(info_space);
        return -1;
    }

    return 0;
}

int info_space_run()
{
    int err;

    while (!dimes_server_complete(info_space->dimes_s)) {
        err = dimes_server_process(info_space->dimes_s);
        if (err < 0) {
            /* If there is an error on the execution path,
               I should stop the server. */
            dimes_server_free(info_space->dimes_s);

            /* TODO:  implement an  exit method  to signal
               other servers to stop. */
            printf("Server exits due to error %d.\n", err);

            return err;
        }

        if (info_space->has_manager_info) {
            process_pending_msg();
        } 
    }

    return 0;
}

void info_space_finish()
{
    if (info_space && info_space->dimes_s) {
        dimes_server_barrier(info_space->dimes_s);
        dimes_server_free(info_space->dimes_s);
        free(info_space);
        info_space = NULL;
    }
}

void info_space_usage()
{
    printf("Usage: server OPTIONS\n"
            "OPTIONS: \n"
            "--server, -s    Number of server instance/staging nodes\n"
            "--cnodes, -c    Number of compute nodes\n"
            "--conf, -f      Define configuration file\n");
}

int info_space_parse_args(int argc, char *argv[])
{
    const char opt_short[] = "s:c:f:";
    const struct option opt_long[] = {
            {"server",      1,      NULL,   's'},
            {"cnodes",      1,      NULL,   'c'},
            {"conf",        1,      NULL,   'f'},
            {NULL,          0,      NULL,   0}
    };

    int opt;

    while ((opt = getopt_long(argc, argv, opt_short, opt_long, NULL)) != -1) {
        switch (opt) {
        case 's':
            num_sp = (optarg) ? atoi(optarg) : -1;
            break;

        case 'c':
            num_cp = (optarg) ? atoi(optarg) : -1;
            break;

        case 'f':
            conf = (optarg) ? optarg : NULL;
            break;

        default:
            printf("Unknown argument \n");
        }
    }

    if (num_sp <= 0)
        num_sp = 1;
    if (num_cp == 0)
        num_cp = 0;
    if (!conf)
        conf = "dataspaces.conf";
    return 0;
}

// Run information space 
int run_info_space(int argc, char **argv) {
	if (info_space_parse_args(argc, argv) < 0) {
		info_space_usage();
		return -1;
	}

	if (info_space_init() < 0) {
		printf("DART server init failed!\n");
		return -1;
	}

	if (info_space_run() < 0) {
		printf("DART server got an error at runtime!\n");
		return -1;
	}

	info_space_finish();

	uloga("%s(): all ok.\n", __func__);	

	return 0;
}

int main(int argc, char **argv)
{
	int err;
    int color, rank;
    MPI_Comm comm;

    color = 0;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &comm);

	err = run_info_space(argc, argv);

    MPI_Barrier(comm);
    MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;
}
