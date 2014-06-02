#ifndef __DATASPACES_API_H__
#define __DATASPACES_API_H__

#ifdef __cplusplus
extern "C" {
#endif

//#include "list.h"
//#include "ss_data.h"
#include "hstaging_partition.h"
#include "hstaging_def.h"

#include "mpi.h"

/* Initialize the dataspaces library. */
int hstaging_init(int num_peers, int appid, enum hstaging_pe_type pe_type);

/* Finalize the dataspaces library. */
int hstaging_finalize();

int hstaging_put_var(const char *var_name, unsigned int ver, int size,
	int xl, int yl, int zl,
	int xu, int yu, int zu,
	void *data, MPI_Comm *comm);
int hstaging_put_sync_all();

int hstaging_get_var(const char *var_name, unsigned int ver, int size,
	int xl, int yl, int zl,
	int xu, int yu, int zu,
	void *data, MPI_Comm *comm);

int hstaging_update_var(struct var_descriptor *var_desc,
    enum hstaging_update_var_op op);

int hstaging_request_task(struct task_descriptor *t);

int hstaging_set_task_finished(struct task_descriptor *t);

int hstaging_set_workflow_finished();

int hstaging_register_executor(int pool_id, int num_executor, int mpi_rank);

int hstaging_execute_dag(const char* conf_file);

int hstaging_build_staging(int pool_id, const char *conf_file);

#ifdef __cplusplus
}
#endif

#endif
