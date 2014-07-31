#ifndef __HSTAGING_API_H__
#define __HSTAGING_API_H__

#ifdef __cplusplus
extern "C" {
#endif

//#include "list.h"
//#include "ss_data.h"
#include <stdint.h>

#include "hstaging_partition.h"
#include "hstaging_def.h"

#include "mpi.h"

/* Initialize the dataspaces library. */
int hstaging_init(int num_peers, int appid, enum hstaging_pe_type pe_type);
/* Finalize the dataspaces library. */
int hstaging_finalize();

// Update variable information to drive the dependency resolution on
// workflow manager.
int hstaging_update_var(struct hstaging_var *var, enum hstaging_update_var_op op);

// Add/register new task executor.
int hstaging_register_executor(int pool_id, int num_executor, int mpi_rank);
// Request for new task from workflow manager. 
int hstaging_request_task(struct task_descriptor *t);
// Notify to workflow manager the completion of task execution.
// Note: for task running on N executors, only one executor needs to call this
// function.
int hstaging_set_task_finished(struct task_descriptor *t);

// Submit task execution request to workflow manager, and blocks for the
// completion of task execution.
int hstaging_submit_task(uint32_t wid, uint32_t tid, const char* conf_file);
// Submit task execution request to workflow manager, and returns immediately.
int hstaging_submit_task_nb(uint32_t wid, uint32_t tid, const char* conf_file);
// Wait for the completion of submitted task.
int hstaging_wait_submitted_task(uint32_t wid, uint32_t tid);
// Check the execution status of submitted task.
int hstaging_check_submitted_task(uint32_t wid, uint32_t tid);
// Notify to worklfow manager the completion of programmer defined workflow.
int hstaging_set_workflow_finished(uint32_t wid);

// TODO: to be implemented.
int hstaging_build_staging(int pool_id, const char *conf_file);

// Notify the framework to shutdown itself.
int hstaging_stop_framework();

#ifdef __cplusplus
}
#endif

#endif
