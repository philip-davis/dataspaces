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
int hstaging_init(int num_peers, int appid, enum worker_type type);

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

int hstaging_update_var(struct var_descriptor *var_desc, enum hstaging_update_var_op op);

int hstaging_request_task(struct task_descriptor *t);
int hstaging_set_task_done(struct task_descriptor *t);
int hstaging_register_bucket_resource(enum hstaging_location_type loc_type, int num_bucket, int mpi_rank);

#ifdef __cplusplus
}
#endif

#endif
