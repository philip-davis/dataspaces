#ifndef __DART_RPC_TCP_H__
#define __DART_RPC_TCP_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include "config.h"
#include "list.h"

#define ALIGN_ADDR_QUAD_BYTES(a)                                \
        unsigned long _a = (unsigned long) (a);                 \
        _a = (_a + 7) & ~7;                                     \
        (a) = (void *) _a;
/*
typedef unsigned char   __u8;
typedef unsigned int    __u32;
typedef int             __s32;
typedef uint64_t    __u64;
*/

struct msg_buf;
struct rpc_server;
struct rpc_cmd;
struct node_id;
struct rpc_request;

/*
  Service function for rpc message
*/
typedef int (*rpc_service)(struct rpc_server*, struct rpc_cmd*);

typedef int (*request_callback) (struct rpc_server *rpc_s, struct rpc_request * request);
typedef int (*completion_callback)(struct rpc_server *, struct msg_buf *);

struct ptlid_map {
    int id;
    int appid;
    struct sockaddr_in address;
} __attribute__((__packed__));

/* Header for the locking service. */
#define LOCK_NAME_SIZE 64
struct lockhdr {
        int                     type;
        int                     rc;
        int                     id;
        int                     lock_num;
    char            name[LOCK_NAME_SIZE]; //lock name
} __attribute__ ((__packed__));


/* Rpc command structure. */
struct rpc_cmd {
        unsigned char            cmd;            // type of command
        unsigned char            num_msg;
        unsigned int           id; //Dart ID

        unsigned char            pad[280+(BBOX_MAX_NDIM-3)*24];// payload of the command
} __attribute__((__packed__));

/*
  Header  structure  used  to  send  timing messages.  Note:  size  of
  time_tab[] should fit into the pad field of a 'struct rpc_cmd'.
*/
struct hdr_timing {
        /* Timer offsets into the table to be determined by the
           applications. */
        int             time_num;
        double          time_tab[20];
};

/* 
   Header structure  for system commands; total  structure size should
   be 64. Fields names and size may change.
*/
struct hdr_sys {
        unsigned long sys_cmd:4;
        unsigned long sys_msg:4;
        unsigned long sys_pad0:8;
        unsigned long sys_pad1:16;
        unsigned long sys_id:32;
};

struct sys_request{
    struct list_head req_entry;
    struct hdr_sys *hs;
};

/* 
   Command values defined for the system commands, sys_count should be
   <= 16.
*/
enum sys_cmd_type {
        sys_none = 0,
        sys_msg_req,
        sys_msg_ret,
        sys_bar_enter,
        sys_count
};

enum io_dir {
        io_none = 0,
        io_send,
        io_receive,
        io_count
};

struct rpc_request{
    struct list_head req_entry;
    struct msg_buf  *msg;
    // void    *private_var; /* Seems no use??? */
    enum    io_dir iodir;
    void    *data;
    size_t  size;

    request_callback cb;
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

enum rpc_component {
    DART_SERVER,
    DART_CLIENT
};

struct rpc_server {
    enum rpc_component cmp_type; /* DART_SERVER or DART_CLIENT */
    struct ptlid_map ptlmap; /* My own ptlid_map data */
    int sockfd_s;

    int num_peers; /* Number of all peers, include server and all clients */
    struct node_id *peer_tab; /* Table of all peers */

    int app_minid; /* Min ID in app, it should be 0 for server */
    int app_num_peers; /* Number of peers in app */

    pthread_t comm_thread; /* Thread for managing connections */
    int thread_alive;

    void *dart_ref; /* Points to dart_server or dart_client struct */
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

enum cmd_type { 
    cn_data = 1,
    cn_init_read,
    cn_read,
    cn_large_file, 
    cn_register, 
    cn_route, 
    cn_unregister,
    cn_resume_transfer,     /* Hint for server to start async transfers. */
    cn_suspend_transfer,    /* Hint for server to stop async transfers. */
    sp_reg_request,
    sp_reg_reply,
    sp_announce_cp,
    cn_timing,
    /* Synchronization primitives. */
    cp_barrier,
    cp_lock,
    /* Shared spaces specific. */
    ss_obj_put,
    ss_obj_update,
    ss_obj_get_dht_peers,
    ss_obj_get_desc,
    ss_obj_query,
    ss_obj_cq_register,
    ss_obj_cq_notify,
    ss_obj_get,
    ss_obj_filter,
    ss_obj_info,
    ss_info,
    cp_remove,
#ifdef DS_HAVE_ACTIVESPACE
    ss_code_put,
    ss_code_reply,
#endif
#ifdef DS_HAVE_DIMES
    dimes_ss_info_msg,
    dimes_locate_data_msg,
    dimes_locate_data_v2_msg,
    dimes_locate_data_v3_msg,
    dimes_put_msg,
    dimes_put_v2_msg,
    dimes_put_v2_1_msg,        
    dimes_put_v3_msg,
    dimes_update_dht_msg,      
    dimes_get_dht_peers_msg,
    dimes_get_location_peers_msg,
    dimes_obj_get_msg,
    dimes_obj_get_ack_v3_msg,
    dimes_get_ack_msg,
#endif
    /* Added for CCGrid Demo. */
    CN_TIMING_AVG,
    _CMD_COUNT
};

enum lock_type {
    lk_read_get,
    lk_read_release,
    lk_write_get,
    lk_write_release,
    lk_grant
};

struct connection_info {
    enum rpc_component cmp_type;
    int id;
    int app_id;
    int app_size;
} __attribute__((__packed__));

struct payload_app_info {
    int id;
    int app_id;
    int app_size;
} __attribute__((__packed__));

struct payload_client_registration_reply {
    int id_sp;
    int id_cp;
    int size_sp;
    int size_cp;
    int id_cp_min;
} __attribute__((__packed__));

struct payload_unregistration_info {
    int num_cp;
} __attribute__((__packed__));

static int default_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg) {
    if (msg != NULL) {
        free(msg);
    }
    return 0;
}

static int default_completion_with_data_callback(struct rpc_server *rpc_s, struct msg_buf *msg) {
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return 0;
}

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func);

int rpc_send_connection_info(struct rpc_server *rpc_s, struct node_id *peer);
int rpc_recv_connection_info(int sockfd, struct connection_info *info);

struct rpc_server* rpc_server_init(const char *interface, int app_num_peers, void *dart_ref, enum rpc_component cmp_type);
void rpc_server_set_peer_ref(struct rpc_server *rpc_s, struct node_id *peer_tab, int num_peers);
int rpc_write_config(struct rpc_server *rpc_s, const char *filename);
int rpc_read_config(struct sockaddr_in *address, const char *filename);
int rpc_connect(struct rpc_server *rpc_s, struct node_id *peer);
int rpc_process_event(struct rpc_server *rpc_s);
int rpc_barrier(struct rpc_server *rpc_s, void *comm);
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_server_free(struct rpc_server *);

struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs);

int rpc_send_directv(struct rpc_server *, struct node_id *, struct msg_buf *); //uncommented by Tong.

// void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);
// void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);

// void rpc_report_md_usage(struct rpc_server *);

#ifdef __cplusplus
}
#endif

#endif 
