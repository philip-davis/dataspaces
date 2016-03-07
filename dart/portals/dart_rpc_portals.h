/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*/

#ifndef __DART_RPC_PORTALS_H_
#define __DART_RPC_PORTALS_H_

#include <stdlib.h>
#include <stdint.h>

#include "config.h"
#include "list.h"

#ifdef HAVE_CRAY_PORTALS

#include <portals/portals3.h>

#define PTL_EQ_HANDLER_NONE NULL
#define	PTL_NO_ACK_REQ	PTL_NOACK_REQ

#undef PTL_NID_ANY
#undef PTL_PID_ANY
/* Redefine these values to make the PGI compiler happy. */
#define PTL_NID_ANY ((ptl_nid_t) 0xFFFFFFFF)
#define PTL_PID_ANY ((ptl_pid_t) 0xFFFF)

#else	/* HAVE_CRAY_PORTALS */

#include <portals3.h>
#include <p3nal_tcp.h>

#define PTL_IFACE_DUP				PTL_OK
#define PTL_MD_EVENT_AUTO_UNLINK_ENABLE		0

#endif	/* HAVE_CRAY_PORTALS */

#define MB_SYS_MSG              0x03
#define MB_RPC_MSG              0x04
#define MB_MIN_RESERVED         0x07

#define RPC_CMD_PAD_BASE_SIZE 296 
#define RPC_CMD_PAD_SIZE RPC_CMD_PAD_BASE_SIZE+(BBOX_MAX_NDIM-3)*24

#define ALIGN_ADDR_QUAD_BYTES(a)                                \
{								\
        unsigned long _a = (unsigned long) (a);                 \
        _a = (_a + 7) & ~7;                                     \
        (a) = (void *) _a;					\
}

#define iovec_t			ptl_md_iovec_t

#define FPTR_DEF  void * _fptr[] = {&malloc, &free, &memset};	
/*
typedef unsigned char   __u8;
typedef unsigned int    __u32;
typedef int             __s32;
typedef uint64_t __u64;
*/
struct msg_buf;
struct rpc_server;
struct rpc_cmd;
struct node_id;

extern const char *ptl_event_str[];

/* 
   Rpc prototype function, should be called in response to a remote
   rpc request. 
*/
typedef int (*rpc_service)(struct rpc_server *, struct rpc_cmd *);

/* 
   Asynchronous callback functions; they are invoked when we receive
   events for message buffers they are registered with. 
*/
typedef int (*async_callback)(struct rpc_server *, ptl_event_t *);

/*
  Asynchronous callback function to be used when a transfer completes.
*/
typedef int (*completion_callback)(struct rpc_server *, struct msg_buf *);


/*
  Define a type for flags.
*/
typedef enum {
	unset = 0,
	set
} flag_t; // /* __attribute__((__packed__)) */ flag_t;

struct ptlid_map {
        ptl_process_id_t        ptlid;
        int                     id;
        int                     appid;
};

/* Registration header structure.  */
struct hdr_register {         
        struct ptlid_map        pm_sp;  /* Map of server peer. */
        struct ptlid_map        pm_cp;  /* Map of compute peer. */
        ptl_size_t              roff;
        ptl_size_t              maxsize;
        int                     num_sp;
        int                     num_cp;

        int                     id_min;
} __attribute__((__packed__));

/* Request for send file transfer structure. */
struct rfshdr {
        size_t                  size;
        size_t                  base_offset;
        unsigned int                   index;
        /* Number of instances to serve from the shared space. */
        unsigned int                   num_inst;
        unsigned char                    wrs;            // write source: 1 = disk, 2 = tcp
        unsigned char                    append;
        int                   rc;
        unsigned char                    fname[60];      // If not enough, consider 128
} __attribute__ ((__packed__));

/* Header for the locking service. */
#define LOCK_NAME_SIZE 64
struct lockhdr {
        int                     type;
        int                     rc;
        int                     id;
        int                     lock_num;
        char			name[LOCK_NAME_SIZE];	/* lock name */
} __attribute__ ((__packed__));

/* Header for data kernel function remote deployment. */
struct hdr_fn_kernel {
	int		fn_kernel_size;
	int		fn_reduce_size;
} __attribute__((__packed__));


/* Rpc command structure. */
struct rpc_cmd {
        unsigned char            cmd;            // type of command
        uint64_t           srcnid;
        uint64_t           dstnid;
        unsigned int           srcpid;
        unsigned int           dstpid;
        unsigned char            num_msg;        // # accepting messages
        uint64_t           mbits;
        unsigned int           id;

        // payload of the command
        unsigned char            pad[RPC_CMD_PAD_SIZE];
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

/* Struct to keep state information about RPC requests: send and
   receive. */
struct rpc_request {
        struct list_head        req_entry;

        // struct node_id          *peer;

        async_callback          cb;

        struct msg_buf          *msg;
        void                    *private;

        enum io_dir             iodir;

        /* Data and size to be sent/received. */
        void                    *data;
        size_t                  size;
	flag_t			f_vec;

        ptl_handle_md_t         mdh_rpc;
        ptl_handle_md_t         mdh_data;
};

struct msg_buf {
        struct list_head        msg_entry;

        struct rpc_cmd          *msg_rpc;

	void			*msg_data;
        size_t                  size;

        int                     refcont;

        /* Ref to flag used for synchronization. */
        int                     *sync_op_id;

        /* Callback to customize completion; by default frees memory. */
        completion_callback     cb;

        void                    *private;

        /* Peer I should send this message to. */
        const struct node_id    *peer;
};

enum rpc_component {
	DART_SERVER,
	DART_CLIENT
};

/* Data for the RPC server. */
struct rpc_server {
        ptl_handle_ni_t         nih;
        ptl_handle_eq_t         eqh;
        ptl_pt_index_t          pi;

        // ptl_process_id_t        ptlid;
        // int                     rpc_id;         /* global 'dart' id */
        struct ptlid_map        ptlmap;

        /* Global match bits used for DMA messages. */
        ptl_match_bits_t        mbits;

        /* Portals resources for system messages. */
        ptl_handle_eq_t         sys_eqh;
        ptl_handle_md_t         sys_rcv_mdh;
        ptl_handle_md_t         sys_snd_mdh;

        /* List of buffers for incoming RPC calls. */
        struct list_head        rpc_list;

        int                     num_rpc_per_buff;
        int                     num_buff;

        /* Reference  to peers  table; storage  space is  allocated in
           dart client or server. */
        int                     num_peers;      /* total number of peers */
        struct node_id          *peer_tab;
        
        void                    *dart_ref;

        /* Maximum number of messages I reserve room for, for a peer. */
        int                     max_num_msg;

        /* Fields for barrier implementation. */
        int                     bar_num;
	int			*bar_tab;
        int                     app_minid, app_num_peers;

        /* Count on the replies ... 
        int                     num_rep_posted;
        int                     num_rep_freed;
	*/
	int			num_md_posted;
	int			num_md_unlinked;

	enum rpc_component	cmp_type;
};

struct node_id {
        struct ptlid_map        ptlmap;
        ptl_match_bits_t        mb;

        /* List of pending requests. */
        struct list_head        req_list;
	int			num_req;

        int                     f_req_msg;
        int                     f_need_msg;

        /* Number of messages I can send to this peer without blocking. */
        int                     num_msg_at_peer;
        int                     num_msg_ret;
};


/* FIXME:  crappy defines, should reconsider. */
enum cmd_type { 
	_CMD_ERROR = -1,
	_CMD_UNKNOWN,
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
	ss_code_put,
	ss_code_reply,
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
        free(msg->msg_data);
        free(msg);
        return 0;
}


struct rpc_server* rpc_server_init(int, int, void *, enum rpc_component);
void rpc_server_set_peer_ref(struct rpc_server *, struct node_id [], int);
void rpc_server_set_rpc_per_buff(struct rpc_server *, int);
void rpc_server_free(struct rpc_server *);
struct rpc_server *rpc_server_get_instance(void);
int rpc_server_get_id(void);
int rpc_server_start(struct rpc_server *);
void rpc_server_stop(struct rpc_server *);
void* rpc_server_run(void *);
void rpc_server_wait(struct rpc_server *);


int rpc_read_config(ptl_process_id_t *);
int rpc_write_config(struct rpc_server *);
int rpc_process_event(struct rpc_server *);
int rpc_process_event_with_timeout(struct rpc_server *, int);
void rpc_add_service(enum cmd_type, rpc_service);

// int rpc_credits_return(struct rpc_server *, int, struct node_id *);

int rpc_barrier(struct rpc_server *);

int rpc_send(struct rpc_server *, struct node_id *, struct msg_buf *); 
int rpc_send_direct(struct rpc_server *, const struct node_id *, struct msg_buf *);
int rpc_send_directv(struct rpc_server *, const struct node_id *, struct msg_buf *);
int rpc_receive(struct rpc_server *, struct node_id *, struct msg_buf *);
int rpc_receivev(struct rpc_server *, struct node_id *, struct msg_buf *);
int rpc_receive_direct(struct rpc_server *, const struct node_id *, struct msg_buf *);

void rpc_report_md_usage(struct rpc_server *);

struct msg_buf* msg_buf_alloc(struct rpc_server *, const struct node_id *, int);

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);
void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);

#endif /* __DART_RPC_H_ */
