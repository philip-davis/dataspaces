#ifndef __DC_BASE_IB_H_
#define __DC_BASE_IB_H_

#include "dart_rpc_ib.h"

struct dart_client {
	struct rpc_server *rpc_s;

	int peer_size;
	struct node_id *peer_tab;
	struct node_id *cn_peers;

	int num_sp, num_cp, num_cp_all;

	struct node_id *self;


        //struct list_head peer_list; //list of peers(servers and clients)

	int connected;
	/* Number of compute peers in the app job. */
	int cp_in_job;
	/* Rank of the master peer in the app job. */
	int cp_min_rank;

	int cp_barrier_req;

	/* Registration flag; 0 - unregistred, 1 - registered. */
	int f_reg:1;
	int f_reg_all:1;
	int f_bar:1;

	void *dart_ref;

	int num_posted;
};				// //

#define dc_barrier(dc)          rpc_barrier(dc->rpc_s)

static inline struct node_id *dc_get_peer(struct dart_client *dc, int n)
{
	if(n < dc->num_sp || n>=dc->cp_min_rank + dc->num_cp)
		return dc->peer_tab + n;
	else if(n < dc->cp_min_rank + dc->num_cp && n >= dc->cp_min_rank)
		return (dc->peer_tab + dc->num_sp) + (n - dc->cp_min_rank);
	else
		return (dc->peer_tab + dc->num_cp + n);
}				// //

/*
static inline struct node_id *dc_node_find(struct dart_client *dc, int nodeid)
{
         struct node_id *temp_peer;
         list_for_each_entry(temp_peer, &dc->peer_list, struct node_id, peer_entry) {
                if(temp_peer->ptlmap.id == nodeid)
                         return temp_peer;
         }
         return 0;
}
*/

static inline struct dart_client *dc_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}				// //

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref);	// //
void dc_free(struct dart_client *dc);	// //
int dc_process(struct dart_client *dc);	// //

#endif
