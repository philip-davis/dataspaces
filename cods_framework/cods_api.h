#ifndef __CODS_API_H__
#define __CODS_API_H__

#ifdef __cplusplus
extern "C" {
#endif

//#include "list.h"
//#include "ss_data.h"
#include <stdint.h>

#include "cods_partition.h"
#include "cods_def.h"

#include "mpi.h"

/* Initialize the dataspaces library. */
int cods_init(int num_peers, int appid, enum cods_pe_type pe_type);
/* Finalize the dataspaces library. */
int cods_finalize();

// Update variable information to drive the dependency resolution on
// workflow manager.
int cods_update_var(struct cods_var *var, enum cods_update_var_op op);

// Add/register new task executor.
int cods_register_executor(int pool_id, int num_executor, int mpi_rank);
// Request for new task from workflow manager. 
int cods_request_task(struct task_descriptor *t);
// Notify to workflow manager the completion of task execution.
// Note: for task running on N executors, only one executor needs to call this
// function.
int cods_set_task_finished(struct task_descriptor *t);

// Submit task execution request to workflow manager, and blocks for the
// completion of task execution.
int cods_submit_task(uint32_t wid, uint32_t tid, const char* conf_file);
// Submit task execution request to workflow manager, and returns immediately.
int cods_submit_task_nb(uint32_t wid, uint32_t tid, const char* conf_file);
// Wait for the completion of submitted task.
int cods_wait_submitted_task(uint32_t wid, uint32_t tid);
// Check the execution status of submitted task.
int cods_check_submitted_task(uint32_t wid, uint32_t tid);
// Notify to worklfow manager the completion of programmer defined workflow.
int cods_set_workflow_finished(uint32_t wid);

// TODO: to be implemented.
int cods_build_staging(int pool_id, const char *conf_file);

// Notify the framework to shutdown itself.
int cods_stop_framework();

#ifdef __cplusplus
}
#endif

#endif
