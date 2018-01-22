#ifndef __DART_RPC_H_
#define __DART_RPC_H_

#include "list.h"

#include <stdlib.h>

#if HAVE_MPI
#include<mpi.h>
#endif

// Return codes
typedef enum rpc_return {
        RPC_RC_SUCCESS = 0,
        RPC_RC_INVALID_PARAM,
        GNI_RC_ERROR_RESOURCE,
        GNI_RC_ERROR_NOMEM
} rpc_return_t;

typedef unsigned char rpc_cmd_t;
typedef unsigned char rpc_cmd_ns_t;

struct msg_buf;
struct node_id;
struct rpc_cmd;
struct rpc_request;
struct rpc_server;

/*
  Service function for rpc message
*/
typedef int (*rpc_service)(struct rpc_server*, struct rpc_cmd*);
typedef int (*completion_callback)(struct rpc_server *, struct msg_buf *);

/* RPC command structure. */
struct rpc_cmd {
		rpc_cmd_ns_t cmd_ns; // namespace of type of command
        rpc_cmd_t cmd;            // type of command
        unsigned char num_msg;
        unsigned int id; //Dart ID

        unsigned char pad[280+(BBOX_MAX_NDIM-3)*24]; // payload of the command
} __attribute__((__packed__));

struct ptlid_map {
	int id;
	int appid;
};

struct node_id {
    struct ptlid_map        ptlmap;

    /* List of pending requests. */
    struct list_head        req_list;
    int         num_req;

    int                     f_req_msg;
    int                     f_need_msg;//Indicate if remote peer needs credits

    /* Number of messages I can send to this peer without blocking. */
    int                     num_msg_at_peer;
    int                     num_msg_ret;

    int sockfd; /* Socket */
    int f_connected; /* Flag: if the peer is connected through `sockfd` */
};


struct rpc_cmd_ns {
	struct list_head rpc_ns_entry;
	rpc_cmd_ns_t ns;
	size_t num_rpc_funcs;
	rpc_service *rpc_func;
};


struct msg_buf{
    struct list_head msg_entry;
    struct rpc_cmd  *msg_rpc;
    void    *msg_data;
    size_t  size;

    int refcont;

    /* Ref to flag used for synchronization. */
    int *sync_op_id;

    /* Callback to customize completion; by default frees memory. */
    completion_callback cb;
    void    *private;
    const struct node_id    *peer;
};

struct rpc_server {
	struct list_head rpc_ns;
	size_t ns_rpc_count;
#if HAVE_MPI
    MPI_Comm *comm;
#endif
};

rpc_return_t rpc_add_service(rpc_cmd_t rpc_cmd, rpc_service rpc_func);
rpc_return_t rpc_add_service_with_namespace(rpc_cmd_ns_t rpc_ns, rpc_cmd_t rpc_cmd, rpc_service rpc_func);
rpc_return_t rpc_add_service_with_namespace_r(struct rpc_server *rpc_s, rpc_cmd_ns_t rpc_ns, rpc_cmd_t rpc_cmd, rpc_service rpc_func);

#endif /* __DART_RPC_H_ */
