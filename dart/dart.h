#ifndef __DART_H_
#define __DART_H_

#include "config.h"

// Comment block below lists data structures and APIs that exposed
// by DART layer and would be used by DataSpaces layer (please add
// if any is missing).
//
// Note: Ideally, we should keep the definition of these data
// structures and the function prototypes of these APIs consistent
// across all network transports. This requirement maybe hard to
// maintain for data structures. Some data structures,
// e.g. struct node_id, are defined differently on different network
// transports. In this case, the DataSpaces layer should only access
// the attributes supported by all network transports. But this is
// tricky and could potentially cause problems. How we could hide
// transport-dependent details that not supposed to be exposed to
// upper layer??
 
/*
 // From dart_rpc_xxx.h
 enum rpc_component;
 enum cmd_type;
 enum lock_type;
 
 struct rpc_server;
 struct msg_buf;
 struct node_id;
 struct rpc_cmd;
 
 void rpc_add_service(enum cmd_type, rpc_service);
 int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
 void rpc_mem_info_cache(struct node_id *peer, struct rpc_cmd *cmd);
 
 // From dc_base_xxx.h
 struct dart_client;
 
 struct node_id * dc_get_peer(struct dart_client *dc, int n);
 struct dart_client* dc_alloc(int num_peers, int appid, void *dart_ref);
 void dc_free(struct dart_client *dc);
 int dc_process(struct dart_client *dc); 
 
 
 // From ds_base_xxx.h
 struct app_info;
 struct dart_server;
 
 struct dart_server* ds_alloc(int num_sp, int num_cp, void *dart_ref);
 void ds_free(struct dart_server *ds);
 int ds_process(struct dart_server *ds);
 struct dart_server * ds_ref_from_rpc(struct rpc_server *rpc_s);
 int ds_get_rank(struct dart_server *ds);
 struct node_id * ds_get_peer(struct dart_server *ds, int n);
 int ds_stop(struct dart_server *ds);
*/

#if HAVE_UGNI

#include "gni/dart_rpc_gni.h"
#include "gni/dc_base_gni.h"
#include "gni/ds_base_gni.h"

#elif HAVE_PORTALS

#include "portals/dart_rpc_portals.h"
#include "portals/dc_base_portals.h"
#include "portals/ds_base_portals.h"

#elif HAVE_INFINIBAND

#include "ib/dart_rpc_ib.h"
#include "ib/dc_base_ib.h"
#include "ib/ds_base_ib.h"

#elif HAVE_DCMF

#include "dcmf/dart_rpc_dcmf.h"
#include "dcmf/dc_base_dcmf.h"
#include "dcmf/ds_base_dcmf.h"
#include "dcmf/dart_rdma_dcmf.h"

#endif

#endif
