#ifndef __DS_BASE_IB_H_
#define __DS_BASE_IB_H_

#include "dart_rpc_ib.h"
#include "mpi.h"

#define ds_barrier(ds)  rpc_barrier(ds->rpc_s)

/*
  Structure to represent an application that uses the Spaces.
*/

struct app_info {
	struct list_head app_entry;

	char *app_name;		/* Application name */
	int app_id;		/* Application identifier */

	int app_num_peers;	/* Total number of peers in app */
	struct node_id *app_peer_tab;	/* Reference to app nodes info */

	int app_cnt_peers;	/* Peers so far */
};

struct dart_server {
	struct rpc_server *rpc_s;

	/* List (array) of peer nodes; this should be of fixed size. */
	int peer_size;
	struct node_id *peer_tab;
	struct node_id *cn_peers;

	/* Number of compute node peers; number of server peers. */
	int num_cp, num_sp;
	int size_cp, size_sp;

	struct list_head app_list;	/* List of applications */

	/* Reference for self instance in 'peer_tab'. */
	struct node_id *self;

	/* 'f_reg' records if registration is complete. */
	int f_reg;
	int f_stop;

	int f_unreg;
	int num_charge;

	int f_s_unreg;

	/* Flag to accept new requests or drom exsiting ones. */
	int f_nacc:1;

	/* Reference to the front end module used. */
	void *dart_ref;
	MPI_Comm *comm;

};				// //

struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm);
void ds_free(struct dart_server *ds);	// //
int ds_process(struct dart_server *ds);

static inline struct dart_server *ds_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}				// //

static inline int ds_get_rank(struct dart_server *ds)
{
	return ds->self->ptlmap.id;
}				// //

static inline struct node_id *ds_get_peer(struct dart_server *ds, int n)
{
	return (ds->peer_tab + n);
}				// //

static inline int ds_stop(struct dart_server *ds)
{
	return ds->f_stop;
}				// //

#endif
