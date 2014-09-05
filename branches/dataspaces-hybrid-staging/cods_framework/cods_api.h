#ifndef __CODS_API_H__
#define __CODS_API_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "cods_partition.h"
#include "cods_def.h"

#include "mpi.h"

enum programmer_defined_partition_type {
    // Note: 0 is reserved as the default value.
    simulation_executor = 1,
    insitu_colocated_executor,
    intransit_executor
};

/* Initialize the dataspaces library. */
int cods_init(int num_peers, int appid, enum cods_pe_type pe_type);
/* Finalize the dataspaces library. */
int cods_finalize();

// Update variable information to drive the dependency resolution on
// workflow manager.
int cods_update_var(struct cods_var *var, enum cods_update_var_op op);

// Add/register new task executor.
int cods_register_executor(int pool_id, int num_executor, int mpi_rank);
// Add/register new task executors (collective operation and requires 
// a valid MPI communicator).
int cods_register_executor_v2(int pool_id, int num_executor, MPI_Comm comm);

// Request for new task from workflow manager. 
int cods_request_task(struct cods_task *t);

// Notify to workflow manager the completion of task execution.
// Note: for task running on N executors, only one executor needs to call this
// function.
int cods_set_task_finished(struct cods_task *t);

// Submit task execution request to workflow manager, and returns immediately.
int cods_exec_task(struct task_descriptor *task_desc);

// Wait for the completion of submitted task.
int cods_wait_task_completion(struct task_descriptor *task_desc);

// Check the execution status of submitted task.
int cods_get_task_status(struct task_descriptor *task_desc);

//
struct executor_pool_info* cods_get_executor_pool_info(int pool_id);

//
int cods_build_partition(struct executor_pool_info *pool_info);

// Notify the framework to shutdown itself.
int cods_stop_framework();

//
void init_task_descriptor(struct task_descriptor *task_desc, int task_id, const char* conf_file);
#ifdef __cplusplus
}
#endif

#endif
