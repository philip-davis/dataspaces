#ifndef _DC_BASE_IB_H_
#define _DC_BASE_IB_H_

#include "dart_rpc_ib.h"

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

const char* dc_to_str(const char* type, const void* struct_);

struct dart_client {
	struct rpc_server* rpc_s;
	struct node_id* self;
  
	int peer_size;
// 	struct node_id* peer_tab; // master_server + self + servers + master_client + clients
// 	struct node_id* cn_peers;
  
	int num_sp, num_cp, num_cp_all;
  
  // Number of compute peers in the app job.
	int cp_in_job;
	// Rank of the master peer in the app job.
	int cp_min_rank;
  
	int cp_barrier_req;
  
	// Registration flag; 0: unregistred, 1: registered
	int f_reg:1;
	int f_reg_all:1;
	int f_bar:1;
  
	void* dart_ref;
  
	int num_posted;
	MPI_Comm comm;
};

// #define dc_barrier(dc) rpc_barrier(dc->rpc_s)
int dc_barrier(struct dart_client *dc);

struct node_id* dc_get_peer(struct dart_client* dc, int peer_id);

static inline struct dart_client* dc_ref_from_rpc(struct rpc_server* rpc_s) {
	return (struct dart_client*)rpc_s->dart_ref;
}

struct dart_client* dc_alloc(int num_peers, int appid, void *dart_ref, void *comm);
int dc_free(struct dart_client *dc);
int dc_process(struct dart_client *dc);

#ifdef __cplusplus
}
#endif // __cplusplus
#endif // _DC_BASE_IB_H_
