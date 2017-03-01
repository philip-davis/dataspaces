#ifndef _DS_BASE_IB_H_
#define _DS_BASE_IB_H_

#include <sys/stat.h>

#include "dart_rpc_ib.h"

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#define ds_barrier(ds_) rpc_barrier(ds_->rpc_s)

/* Structure to represent an application that uses the Spaces. */
struct app_info {
	int app_id;	/* Application identifier */
  
  // TODO change names of the properties
	short int app_num_peers;	/* total # of peers in app */
  short int base_id; /* id of any app peer = base_id + app_cnt_peers*/
	short int app_cnt_peers;	/* # of app peers reged so far */
};

struct dart_server {
	struct rpc_server* rpc_s;
	struct node_id* self; /* Reference for self peer_ */
  
	int num_cp, num_sp; /* Number of compute node peers; number of server peers. */
	int peer_size; /* = num_sp + num_cp */
	int size_cp, size_sp;
  
	int f_reg; /* records if registration is complete. */
	int f_stop;
	int f_unreg;
	int num_charge;
	int ready_to_disseminate_all;
	int num_slaves_done_interconnecting;
  
	void* dart_ref; /* Reference to the front end module used. */
};

struct dart_server* ds_alloc(int num_sp, int num_cp, void* dart_ref_, void* comm);
int ds_free(struct dart_server *ds);
int ds_process(struct dart_server *ds); // primary function for msg processing

static inline struct dart_server* ds_ref_from_rpc(struct rpc_server* rpc_s_) {
	return (struct dart_server*)rpc_s_->dart_ref;
}

static inline int ds_get_rank(struct dart_server* ds_) {
	return ds_->rpc_s->ptlmap.id;
}

static inline struct node_id* ds_get_peer(struct dart_server* ds_, int peer_id) {
  return rpc_get_peer(ds_->rpc_s, peer_id);
}

static inline int ds_stop(struct dart_server* ds_) {
	return ds_->f_stop;
}
#ifdef __cplusplus
}
#endif // __cplusplus
#endif // _DS_BASE_IB_H_
