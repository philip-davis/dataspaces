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

int hstaging_update_var(struct hstaging_var *var, enum hstaging_update_var_op op);

int hstaging_register_executor(int pool_id, int num_executor, int mpi_rank);
int hstaging_request_task(struct task_descriptor *t);
int hstaging_set_task_finished(struct task_descriptor *t);

int hstaging_submit_task(uint32_t wid, uint32_t tid, const char* conf_file);
int hstaging_submit_task_nb(uint32_t wid, uint32_t tid, const char* conf_file);
int hstaging_wait_submitted_task(uint32_t wid, uint32_t tid);
int hstaging_check_submitted_task(uint32_t wid, uint32_t tid);
int hstaging_set_workflow_finished(uint32_t wid);

int hstaging_build_staging(int pool_id, const char *conf_file);
int hstaging_stop_framework();

#ifdef __cplusplus
}
#endif

#endif
