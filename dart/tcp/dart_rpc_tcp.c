#include "dart_rpc_tcp.h"

struct rpc_server* rpc_server_init(int num_buff, int num_rpc_per_buff, void *dart_ref, enum rpc_component cmp_type) { return NULL; }
void rpc_server_set_peer_ref(struct rpc_server *rpc_s,
                        struct node_id peer_tab[], int num_peers) {}
void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff) {}
void rpc_server_free(struct rpc_server *rpc_s) {}
int rpc_process_event(struct rpc_server *rpc_s) { return 0; }
void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func) {}
int rpc_barrier(struct rpc_server *rpc_s) { return 0; }
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) { return 0; }
int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) { return 0; }
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) { return 0; }
int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) { return 0; }
int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) { return 0; }

struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, 
		const struct node_id *peer, int num_rpcs) { return NULL; }

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd) {}
void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd) {}

void rpc_report_md_usage(struct rpc_server *rpc_s) {}
