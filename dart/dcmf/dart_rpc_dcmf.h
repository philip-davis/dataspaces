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
*  Fan Zhang (2011) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/

#ifndef __DART_RPC_DCMF_H__
#define __DART_RPC_DCMF_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "dcmf.h"

#include "config.h"
#include "list.h"

#define RPC_CMD_PAD_BASE_SIZE 296 
#define RPC_CMD_PAD_SIZE RPC_CMD_PAD_BASE_SIZE+(BBOX_MAX_NDIM-3)*24

#define ALIGN_ADDR_QUAD_BYTES(a)                                \
        unsigned long _a = (unsigned long) (a);                 \
        _a = (_a + 7) & ~7;                                     \
        (a) = (void *) _a;

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

/*
  Service function for rpc message
*/
typedef int (*rpc_service)(struct rpc_server*, struct rpc_cmd*);

/*
  Asynchronous callback function to be used when a transfer completes.
*/
typedef int (*completion_callback)(struct rpc_server *, struct msg_buf *);

struct ptlid_map {
	size_t rank_dcmf; //ibm dcmf rank
	int id;
	int appid;
};

struct hdr_register{
	struct ptlid_map pm_sp; //map of server peer
	struct ptlid_map pm_cp; //map of compute peer
	size_t roff;
	size_t maxsize;
	int num_sp;
	int num_cp;
	int id_min;
}__attribute__((__packed__));

/* Header for the locking service. */
#define LOCK_NAME_SIZE 64
struct lockhdr {
        int                     type;
        int                     rc;
        int                     id;
        int                     lock_num;
	char			name[LOCK_NAME_SIZE]; //lock name
} __attribute__ ((__packed__));

/* Rpc command structure. */
struct rpc_cmd {
        unsigned char            cmd;            // type of command
        uint64_t           srcnid; //ibm dcmf rank id
        uint64_t           dstnid; //ibm dcmf rank id
        unsigned int           srcpid; //default is 0
        unsigned int           dstpid; //default is 0
        unsigned char            num_msg;
        unsigned int           id; //Dart ID

        DCMF_Memregion_t	mem_region; //DCMF memory region created for remote node
        size_t			mem_size; //Size for created DCMF memory region

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
	//async_callback cb;
	struct msg_buf	*msg;
	void	*private_var;
	enum 	io_dir iodir;
	void	*data;
	size_t	size;
};

struct msg_buf{
	struct list_head msg_entry;
	struct rpc_cmd	*msg_rpc;
	void	*msg_data;
	size_t	size;
	
	int	refcont;
	
	/* Ref to flag used for synchronization. */
	int	*sync_op_id;
	
	/* Callback to customize completion; by default frees memory. */
	completion_callback cb;
	void	*private;
	const struct node_id	*peer;
};

enum rpc_component {
	DART_SERVER,
	DART_CLIENT
};

//RPC Server
struct rpc_server{
	//For RPC messages
	DCMF_Send_Configuration_t rpc_send_config_dcmf;
	DCMF_Protocol_t rpc_send_protocol_dcmf;
	
	//For SYS messages
	DCMF_Send_Configuration_t sys_send_config_dcmf;
	DCMF_Protocol_t sys_send_protocol_dcmf;
	
	//For GET data from remote memory
	DCMF_Get_Configuration_t rpc_get_config_dcmf;
	DCMF_Protocol_t rpc_get_protocol_dcmf;
	
	//For PUT data into remote memory
	DCMF_Put_Configuration_t rpc_put_config_dcmf;
	DCMF_Protocol_t rpc_put_protocol_dcmf;
	
	struct ptlid_map	ptlmap;
		
	/* List of buffered incoming RPC messages*/
	struct list_head rpc_list;
	/* List of buffered outgoing RPC messages which have attached data buffers*/	
	struct list_head out_rpc_list;
	
	/* List of buffered incoming SYS messages*/
	struct list_head sys_list;
	
	int	num_rpc_per_buff;
	int	num_buff;
	
	int	num_peers;
	struct node_id	*peer_tab;
	
	void	*dart_ref;
	int	max_num_msg;
	
	/* Fields for barrier implementation*/
	int	bar_num;
	int	*bar_tab;
	int	app_minid,app_num_peers;
	
	//Count for replies
	int	num_rep_posted;
	int	num_rep_freed;

	enum rpc_component	cmp_type;
};

struct node_id {
	struct ptlid_map        ptlmap;

	/* List of pending requests. */
	struct list_head        req_list;
	int			num_req;

	int                     f_req_msg;
	int                     f_need_msg;//Indicate if remote peer needs credits

	/* Number of messages I can send to this peer without blocking. */
	int                     num_msg_at_peer;
	int                     num_msg_ret;

	/* Cached pointer value for remote memory region. */
	DCMF_Memregion_t *	cached_remote_memregion;
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
  /* Newly Added for DCMF version */
	rpc_get_finish,
	rpc_put_finish,
#ifdef DS_HAVE_DIMES
    dimes_ss_info_msg,
    dimes_locate_data_msg,
    dimes_put_msg,
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

static inline void rpc_server_inc_reply(struct rpc_server *rpc_s)
{
        rpc_s->num_rep_posted++;
}

static inline void rpc_server_dec_reply(struct rpc_server *rpc_s)
{
        rpc_s->num_rep_freed++;
}

/*
  Default instance for completion_callback; it frees the memory.
*/
static int default_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	if(msg)
        	free(msg);
        return 0;
}

static int default_completion_with_data_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	if(msg && msg->msg_data){
        	free(msg->msg_data);
        	free(msg);
	}
        return 0;
}

struct rpc_server* rpc_server_init(int, int, void *, enum rpc_component);
void rpc_server_set_peer_ref(struct rpc_server *, struct node_id [], int);
void rpc_server_set_rpc_per_buff(struct rpc_server *, int);
void rpc_server_free(struct rpc_server *);
int rpc_read_config(size_t *rank_dcmf, const char *);
int rpc_write_config(struct rpc_server *, const char *);
int rpc_process_event(struct rpc_server *);

int rpc_server_start(struct rpc_server *);
void rpc_server_stop(struct rpc_server *);
void* rpc_server_run(void *);
void rpc_server_wait(struct rpc_server *);

void rpc_add_service(enum cmd_type, rpc_service);

// int rpc_credits_return(struct rpc_server *, int, struct node_id *);

int rpc_barrier(struct rpc_server *);

int rpc_send(struct rpc_server *, struct node_id *, struct msg_buf *); 
int rpc_send_direct(struct rpc_server *, const struct node_id *, struct msg_buf *);
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg); // Not implemented in DCMF version!
int rpc_receive(struct rpc_server *, struct node_id *, struct msg_buf *);
int rpc_receive_direct(struct rpc_server *, struct node_id *, struct msg_buf *);

void rpc_report_md_usage(struct rpc_server *);

struct msg_buf* msg_buf_alloc(struct rpc_server *, const struct node_id *, int);

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg,
			struct rpc_cmd *cmd);
void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);

#ifdef __cplusplus
}
#endif

#endif 

