#ifndef __DC_BASE_IB_H_
#define __DC_BASE_IB_H_

#include "dart_rpc_ib.h"
#include "mpi.h"

struct dart_client {
	struct rpc_server *rpc_s;

	int peer_size;

	int local_id;
	struct node_id *cn_peers;

	int num_sp, num_cp;

	struct node_id *self;

    int num_unreg;
	int connected;
	/* Number of compute peers in the app job. */

	int s_connected;

	int cp_in_job;
	/* Rank of the master peer in the app job. */
	int cp_min_rank;

	int cp_barrier_req;

	/* Registration flag; 0 - unregistred, 1 - registered. */
	int f_reg:1;
	int f_reg_all:1;
	int f_bar:1;

	void *dart_ref;

    struct list_head deferred_app_msg;

	int num_posted;
	MPI_Comm *comm;
};		


static inline struct node_id *dc_get_peer(struct dart_client *dc, int n)
{
	return rpc_server_find(dc->rpc_s, n);
}

static inline struct dart_client *dc_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}			

int dc_barrier(struct dart_client *dc);
struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm);	// //
void dc_free(struct dart_client *dc);	// //
int dc_process(struct dart_client *dc);	// //
int on_same_node(struct node_id*, struct node_id*);

#endif
