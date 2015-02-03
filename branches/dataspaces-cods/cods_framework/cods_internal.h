#ifndef __CODS_INTERNAL_H__
#define __CODS_INTERNAL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "debug.h"
#include "list.h"
#include "bbox.h"
#include "ss_data.h"
#include "dart.h"
#include "dc_gspace.h"
#include "dataspaces.h"

#include "cods_def.h"

/*
*/
struct pending_msg {
    struct list_head entry;
    struct rpc_cmd cmd;
};

/**
    Messaging
**/
int process_event(struct dcg_space *dcg);

struct client_rpc_send_state {
    int f_done;
};
int client_rpc_send(struct dcg_space *dcg, struct node_id *peer, struct msg_buf *msg, struct client_rpc_send_state *state);

/**
    Meta data sharing using DataSpaces
**/
int write_meta_data(const char* var_name, size_t size, void *send_buf);
int read_meta_data(const char* var_name, size_t size, void *recv_buf); 

#ifdef __cplusplus
}
#endif

#endif
