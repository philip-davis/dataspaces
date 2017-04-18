/* 
 * RPC service implementation on infiniband. 
 *
 * Tong Jin (2011) TASSL Rutgers University
 * Hoang Bui (2012-2013) TASSL Rutgers University
 * hbui@cac.rutgers.edu
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */
#ifndef __DART_RPC_IB_H__
#define __DART_RPC_IB_H__

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <getopt.h>
//#include <rdma/rdma_cma.h>
#include <rdma/rdma_verbs.h>
#include "list.h"
#include "../../include/bbox.h"
#include "config.h"

#include "dart_rdma_ib.h"


#define MB_SYS_MSG              0x03
#define MB_RPC_MSG              0x04
#define MB_MIN_RESERVED         0x07
#define RPC_NUM 10
#define SYS_NUM 10

#define RPC_CMD_PAD_BASE_SIZE 296 
#define RPC_CMD_PAD_SIZE RPC_CMD_PAD_BASE_SIZE+(BBOX_MAX_NDIM-3)*24

#define ALIGN_ADDR_QUAD_BYTES(a)                                \
{								\
        unsigned long _a = (unsigned long) (a);                 \
        _a = (_a + 7) & ~7;                                     \
        (a) = (void *) _a;					\
}

#define FPTR_DEF  void * _fptr[] = {&malloc, &free, &memset};

struct msg_buf;
struct rpc_server;
struct rpc_cmd;
struct node_id;

/*
   Rpc prototype function, should be called in response to a remote rpc request. 
*/
typedef int (*rpc_service) (struct rpc_server *, struct rpc_cmd *);

/* 
   Asynchronous callback functions; they are invoked when we receive events for message buffers they are registered with. 
*/
typedef int (*async_callback) (struct rpc_server * rpc_s, struct ibv_wc * wc);

/*
  Asynchronous callback function to be used when a transfer completes.
*/
typedef int (*completion_callback) (struct rpc_server *, struct msg_buf *);

/* -------------------------------------------------------------------------
Structures
*/


/* Define a type for flags. */

typedef enum {
	unset = 0,
	set
} flag_t;

enum rpc_component {
	DART_SERVER,
	DART_CLIENT
};

struct ptlid_map {
	struct sockaddr_in address;
	int id;
	int appid;
};

/* Registration header structure.  */
struct hdr_register {
	struct ptlid_map pm_sp;	/* Map of server peer. */
	struct ptlid_map pm_cp;	/* Map of compute peer/slave server. */
	int num_sp;
	int num_cp;
	int id_min;
};

struct con_param {
	struct ptlid_map pm_sp;
	struct ptlid_map pm_cp;
	int num_cp;
	int type;		//0: sys connect request; 1: rpc connect request.
};

/* Request for send file transfer structure. */
struct rfshdr {
	size_t size;
	size_t base_offset;
	unsigned int index;
	/* Number of instances to serve from the shared space. */
	unsigned int num_inst;
	unsigned char wrs;		// write source: 1 = disk, 2 = tcp
	unsigned char append;
	int rc;
	unsigned char fname[60];		// If not enough, consider 128
} __attribute__ ((__packed__));

#define LOCK_NAME_SIZE 64
/* Header for the locking service. */
struct lockhdr {
	int type;
	int rc;
	int id;
	int lock_num;
	char name[LOCK_NAME_SIZE];	/* lock name */
} __attribute__ ((__packed__));

/* Header for data kernel function remote deployment. */
struct hdr_fn_kernel {
	int fn_kernel_size;
	int fn_reduce_size;
} __attribute__ ((__packed__));


/* Rpc command structure. */
struct rpc_cmd {
	unsigned char cmd;		// type of command
	struct sockaddr_in src;
	struct sockaddr_in dst;
	unsigned char num_msg;		// # accepting messages
	unsigned int id;
	struct ibv_mr mr;
	int qp_num;
	// payload of the command 
	unsigned char pad[RPC_CMD_PAD_SIZE];
	uint64_t wr_id;
};

/*
  Header  structure  used  to  send  timing messages.  Note:  size  of
  time_tab[] should fit into the pad field of a 'struct rpc_cmd'.
*/
struct hdr_timing {
	/* Timer offsets into the table to be determined by the
	   applications. */
	int time_num;
	double time_tab[20];
};


/* 
   Header structure  for system commands; total  structure size should
   be fixed. Fields names and size may change.
*/
struct hdr_sys {
	unsigned long sys_cmd:4;
	unsigned long sys_msg:4;
	unsigned long sys_pad0:8;
	unsigned long sys_pad1:16;
	unsigned long sys_id:32;
};


struct sys_msg {
	struct hdr_sys hs;
	struct hdr_sys *real_hs;
	struct ibv_mr *sys_mr;
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
	//        sys_bar_cont,
	sys_count
};

enum io_dir {
	io_none = 0,
	io_send,
	io_receive,
	io_count
};

/* Struct to keep state information about RPC requests: send and receive. */
struct rpc_request {
	struct list_head req_entry;
	async_callback cb;

	struct msg_buf *msg;
	void *private;

	enum io_dir iodir;

	int type;

	int peerid;		//added in IB version: just for recv_rr, indicates the peer_id of coming cmd.

	/* Data and size to be sent/received. */
	void *data;
	size_t size;
	flag_t f_vec;

	struct ibv_mr *rpc_mr;
	struct ibv_mr *data_mr;

	int rpc_num;


	int current_rpc_count;
	struct rpc_request *next;

};

struct msg_buf {
	struct rpc_cmd *msg_rpc;

	void *msg_data;
	size_t size;

	int refcont;

	// Ref to flag used for synchronization. 
	int *sync_op_id;

	// Callback to customize completion; by default frees memory. 
	completion_callback cb;

	uint64_t id;

	struct ibv_mr mr;


	void *private;

	// Peer I should send this message to.
	const struct node_id *peer;
};

/* Struct to represent the connection*/
struct connection {
	struct rdma_cm_id *id;
	struct ibv_context *ctx;
	struct ibv_pd *pd;
	struct ibv_cq *cq;
	struct ibv_comp_channel *comp_channel;

	struct ibv_qp_init_attr qp_attr;
	struct ibv_qp *qp;
	int f_connected;
	struct ibv_mr peer_mr;
};

/* Data for the RPC server. */
struct rpc_server {

	struct ptlid_map ptlmap;
//IB    struct ptlid_map        tmp_map;////added in IB verstion, to store temp address for data receiving or sending

	struct rdma_cm_id *listen_id;	//added for IB version
	struct rdma_event_channel *rpc_ec;
	//struct rdma_event_channel *sys_ec;

	// Reference  to peers  table; storage  space is  allocated in dart client or server.
	int num_peers;		// total number of peers

    struct list_head peer_list; //list of peers(servers and clients)

	//Fields for barrier implementation.
	int bar_num;
	int *bar_tab;
	int app_minid, app_num_peers, num_sp; //app min id, number of peer in app (fixed during execution) and numer of server

	/* List of buffers for incoming RPC calls. */
	struct list_head rpc_list;
	int num_rpc_per_buff;
	int num_buf;
	int max_num_msg;

	void *dart_ref;
	enum rpc_component com_type;

	int num_qp;

	pthread_t comm_thread;
	int thread_alive;
	char *localip;

	int alloc_pd_flag;
	struct ibv_pd *global_pd;
	struct ibv_context *global_ctx;
};

struct rdma_mr {
	struct ibv_mr mr;
	uint64_t wrid;
};

struct node_id {
    struct list_head peer_entry;
	struct ptlid_map ptlmap;
    
    int local_id;
    int num_peer_in_app;

	// struct for peer connection
	struct rdma_event_channel *rpc_ec;
	struct rdma_event_channel *sys_ec;
	struct connection rpc_conn;
	struct connection sys_conn;
	//struct rdma_conn_param        *cm_params;

	int f_unreg;

	// List of pending requests.
	struct list_head req_list;
	int num_req;

	int req_posted;

	int f_req_msg;
	int f_need_msg;

	int ch_num;		// added for IB version, connected channel num 

	// Number of messages I can send to this peer without blocking.
	int num_msg_at_peer;
	int num_msg_ret;

	// Number of available receiving rr buffer
	int num_recv_buf;
	int num_sys_recv_buf;

	struct rpc_request *rr;
	struct sys_msg *sm;

	int f_rr;


};

/*---------------------------Have been modified below EXCEPT //TODO --------------------------------*/
enum cmd_type {
	cn_data = 1,
	cn_init_read,
	cn_read,
	cn_large_file,
	cn_register,
	cn_route,
	cn_unregister,
	cn_resume_transfer,	/* Hint for server to start async transfers. */
	cn_suspend_transfer,	/* Hint for server to stop async transfers. */

	sp_reg_request,
	sp_reg_reply,
	peer_rdma_done,		/* Added in IB version 12 */
	sp_announce_cp,
    cp_announce_cp,
    sp_announce_cp_all,
    cp_disseminate_cs,
    cn_unregister_cp,
    cn_unregister_app,
    sp_announce_app,
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
	ss_code_put,
	ss_code_reply,
	cp_remove,
#ifdef DS_HAVE_DIMES
    dimes_ss_info_msg,
    dimes_locate_data_msg,
    dimes_put_msg,
#ifdef DS_HAVE_DIMES_SHMEM
    dimes_shmem_reset_server_msg,
    dimes_shmem_update_server_msg,
#endif
#endif
    //Added for CCGrid Demo
	CN_TIMING_AVG,
	_CMD_COUNT,
	cn_s_unregister
};

enum lock_type {
	lk_read_get,
	lk_read_release,
	lk_write_get,
	lk_write_release,
	lk_grant
};

static int file_lock(int fd, int op)
{
    if(op) {
        while(lockf(fd, F_TLOCK, (off_t) 1) != 0) {
        }
        return 0;
    } else
        return lockf(fd, F_ULOCK, (off_t) 1);
}

static struct node_id *peer_alloc()
{
    struct node_id *peer = 0;
    peer = malloc(sizeof(*peer));
    if(!peer)
        return 0;
    memset(peer, 0, sizeof(*peer));
    return peer;
}

/*
  Default instance for completion_callback; it frees the memory.
*/
static int default_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	free(msg);
	return 0;
}

static int default_completion_with_data_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	if(msg->msg_data)
		free(msg->msg_data);
	free(msg);
	return 0;
}



/* -------------------------------------------------------------------------
Functions
*/
int log2_ceil(int n);
double gettime();
void build_context(struct ibv_context *verbs, struct connection *conn);	//new in IB TODO:try to encapsulate these 2 func in dart_rpc
void build_qp_attr(struct ibv_qp_init_attr *qp_attr, struct connection *conn, struct rpc_server *rpc_s);	//new in IB
int rpc_post_recv(struct rpc_server *rpc_s, struct node_id *peer);	//new in IB
int sys_post_recv(struct rpc_server *rpc_s, struct node_id *peer);	//new in IB
int rpc_connect(struct rpc_server *rpc_s, struct node_id *peer);	//new in IB
int sys_connect(struct rpc_server *rpc_s, struct node_id *peer);	//new in IB

struct rpc_server *rpc_server_init(int option, char *ip, int port, int num_buff, int num_rpc_per_buff, void *dart_ref, enum rpc_component cmp_type);
int rpc_server_free(struct rpc_server *rpc_s);
void rpc_server_set_peer_ref(struct rpc_server *rpc_s, struct node_id peer_tab[], int num_peers);
void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff);
struct rpc_server *rpc_server_get_instance(void);
int rpc_server_get_id(void);

int rpc_read_config(int appid, struct sockaddr_in *address);	//
int rpc_write_config(int appid, struct rpc_server *rpc_s);	//

int rpc_process_event(struct rpc_server *rpc_s);
int rpc_process_event_with_timeout(struct rpc_server *rpc_s, int timeout);

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func);
int rpc_barrier(struct rpc_server *rpc_s);

int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);	//API is here, but useless in InfiniBand version
int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);	//API is here, but useless in InfiniBand version

void rpc_report_md_usage(struct rpc_server *rpc_s);
void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);
void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);
struct msg_buf *msg_buf_alloc(struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs);

void rpc_print_connection_err(struct rpc_server *rpc_s, struct node_id *peer, struct rdma_cm_event event);

struct node_id *rpc_server_find(struct rpc_server *rpc_s, int nodeid);
void rpc_server_find_local_peers(struct rpc_server *rpc_s,
            struct node_id **peer_tab, int *num_local_peer, int peer_tab_size);
uint32_t rpc_server_get_nid(struct rpc_server *rpc_s);

#endif
