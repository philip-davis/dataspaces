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
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/
#ifndef __DART_RPC_GNI_H__
#define __DART_RPC_GNI_H__

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <net/if.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/utsname.h>
#include <errno.h>

#include "pmi.h"
//#include "pmi_cray_ext.h"
#include "gni_pub.h"
#include "utility_functions.h"
#include <sys/ioctl.h>

#include "config.h"
#include "list.h"

#ifdef DS_HAVE_DRC
#include "rdmacred.h"
#endif

#define RPC_CMD_PAD_BASE_SIZE 296 
#define RPC_CMD_PAD_SIZE RPC_CMD_PAD_BASE_SIZE+(BBOX_MAX_NDIM-3)*24

#define ALIGN_ADDR_QUAD_BYTES(a)				\
{								\
	unsigned long _a = (unsigned long) (a);			\
	_a = (_a + 7) & ~7;					\
	(a) = (void *) _a;					\
}

#define FPTR_DEF	void * _fptr[] = {&malloc, &free, &memset};

struct msg_buf;
struct rpc_server;
struct rpc_cmd;
struct node_id;

/*
typedef unsigned char	__u8;
typedef unsigned int	__u32;
typedef int		__s32;
typedef uint64_t __u64;
*/

// Rpc prototype function, should be called in response to a remote rpc request. 
typedef int (*rpc_service)(struct rpc_server *rpc_s, struct rpc_cmd *cmd);

// Asynchronous callback functions; they are invoked when we receive events for message buffers they are registered with. 
typedef int (*async_callback)(struct rpc_server *rpc_s, struct rpc_request *rr);

// Asynchronous callback function to be used when a transfer completes.
typedef int (*completion_callback)(struct rpc_server *rpc_s, struct msg_buf *msg);

typedef enum {
	unset = 0,
	set
} flag_t;


struct mdh_addr_t {
	gni_mem_handle_t mdh;
	uint64_t address;
	size_t	length;
	uint32_t index;
};

struct rr_index{
	struct list_head	index_entry;
	uint32_t		index;
};

struct ptlid_map{
	unsigned int	nid;
	unsigned int	pid;
	int	id;
	int	appid;
};

struct sock_addr{
	struct sockaddr_in address;
	int sockfd;
};

/*
	Header structure
*/
struct dc_reg {
	int nid;
	int pid;
	int appid;
	int num_c;
};

struct smsg_attr_reg {
	int id;
	void *msg_buffer; //(uintptr_t)(gni_smsg_attr_t->msg_buffer)
	gni_mem_handle_t mem_hndl;
};

struct peer_attr_reg {
	struct ptlid_map ptlmap;
	gni_smsg_attr_t remote_smsg_attr;
	gni_smsg_attr_t sys_remote_smsg_attr;
};

// Registration header structure. 
struct hdr_register {
        struct ptlid_map	pm_sp;
	struct ptlid_map	pm_cp; 
	int			num_sp;
	int			num_cp;
	int			id_min;
};

// Request for send file transfer structure.
struct rfshdr {
	size_t		size;
	size_t		base_offset;
	unsigned int		index;
        // Number of instances to serve from the shared space.
	unsigned int		num_inst;
	unsigned char		wrs;            // write source: 1 = disk, 2 = tcp
	unsigned char		append;
	int		rc;
	unsigned char		fname[60];      // If not enough, consider 128
};

// Header for the locking service.
#define LOCK_NAME_SIZE 64
struct lockhdr {
	int		type;
	int		rc;
	int		id;
	int		lock_num;
    char	name[LOCK_NAME_SIZE];	// lock name
};

// Header for data kernel function remote deployment.
struct hdr_fn_kernel {
	int		fn_kernel_size;
	int		fn_reduce_size;
};

//  Header  structure  used  to  send  timing messages.  Note:  size  of time_tab[] should fit into the pad field of a 'struct rpc_cmd'.
struct hdr_timing {
        /* Timer offsets into the table to be determined by the
           applications. */
	int		time_num;
	double		time_tab[20];
};

// Header structure  for system commands; total  structure size should be 64. Fields names and size may change.
struct hdr_sys {
	unsigned long sys_cmd:4;
	unsigned long sys_msg:4;
	unsigned long sys_pad0:8;
	unsigned long sys_pad1:16;
	unsigned long sys_id:32;
};

// Command values defined for the system commands, sys_count should be <= 16.
enum sys_cmd_type {
	sys_none = 0,
	sys_msg_req,
	sys_msg_ret,
	sys_msg_ack,
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

struct rpc_cmd {
	unsigned char			cmd;            // type of command
	unsigned int			srcnid;		// nid means node address in GNI
	unsigned int			dstnid;
	unsigned int			srcpid;
	unsigned int			dstpid;
	unsigned char			num_msg;        // # accepting messages
	struct mdh_addr_t	mdh_addr;
	unsigned int			id;
	// payload of the command
	unsigned char			pad[RPC_CMD_PAD_SIZE];
};

struct node_id {
	struct ptlid_map	ptlmap;
	struct mdh_addr_t	mdh_addr;

	gni_ep_handle_t		ep_hndl;
	gni_ep_handle_t		sys_ep_hndl;

	gni_smsg_attr_t		remote_smsg_attr;	
	gni_smsg_attr_t		sys_remote_smsg_attr;

	uint32_t                mbox_offset;
	uint32_t                sys_mbox_offset;

	// List of pending requests.
	struct list_head	req_list;
	int			num_req;

	int			f_req_msg;
	int			f_need_msg;

	int                     f_unreg;
	int                     f_reg;

	// Number of messages I can send to this peer without blocking.
	int			num_msg_at_peer;
	int			num_msg_ret;

	// Number of message I received from this peer (Add in Gemini Version)
	int			num_msg_recv;

	// Number of SYS messages I can send to this peer without blocking.
	int			sys_msg_at_peer;
	int			sys_msg_ret;

	// Number of SYS message I received from this peer (Add in Gemini Version)
	int			sys_msg_recv;

};

struct msg_buf {
	struct list_head	msg_entry;

	struct rpc_cmd		*msg_rpc;

    void            *original_msg_data;
	void			*msg_data;
	size_t			size;

  //int			refconf;

	int			*sync_op_id;

	completion_callback	cb;

	void			*private;

	// Peer that I should send this message to.
	const struct node_id	*peer;
};

enum rpc_request_type {
    DART_RPC_SEND = 0,
    DART_RPC_RECEIVE,
    DART_RPC_SEND_DIRECT,
    DART_RPC_RECEIVE_DIRECT
};

struct rpc_request {
	struct list_head	req_entry;
	uint32_t       		index;	
    int                     type; //0 for cmd, 1 for data
    int f_data; //1: rr->mdh_data is using.

	async_callback		cb;
    int                     refcont;

	struct msg_buf		*msg;
	void			*private;

	enum io_dir		iodir;

	// Data and size to be sent/received.
	void			*data;
	size_t			size;
	flag_t			f_vec;

	// Handles for rdma_post
	gni_mem_handle_t	mdh_rpc;
	gni_mem_handle_t	mdh_data;

	//?gni_post_descriptor_t	rdma_data_desc;
};

enum rpc_component {
	DART_SERVER,
	DART_CLIENT
};

//RPC Server
struct rpc_server{
	struct ptlid_map	ptlmap;
	
	gni_cdm_handle_t	cdm_handle;
	gni_nic_handle_t	nic_hndl;

	gni_cq_handle_t		src_cq_hndl;
	gni_cq_handle_t		dst_cq_hndl;

	gni_smsg_attr_t		local_smsg_attr;
	gni_smsg_attr_t		*remote_smsg_attr;

	gni_cq_handle_t		sys_cq_hndl;		// completion queue for system message
	gni_smsg_attr_t		sys_local_smsg_attr;	// local system message attributes

    gni_mem_handle_t    dart_mem_mdh;

	unsigned int		*all_nic_addresses;

	void			*sys_mem;
	void			*rpc_mem;

	struct list_head	rpc_list;
	int			rr_num;

	int			num_rpc_per_buff;
	int			num_buf;

	int			num_peers;	// total number of peers
	struct node_id		*peer_tab;

	void			*dart_ref;

	int			max_num_msg;

	int			bar_num;
	int			*bar_tab;
	int			app_minid, app_num_peers;

	int			num_md_posted;
	int			num_md_unlinked;

	uint32_t drc_credential_id; //used for DRC

	enum rpc_component	cmp_type;

	// socket address for init connection
	struct sock_addr 	address;
};

enum cmd_type { 
	cn_data = 1,
	cn_init_read,
	cn_read,
	cn_large_file, 
	cn_register, 
	cn_route, 
	cn_unregister,
	cn_resume_transfer,     // Hint for server to start async transfers.
	cn_suspend_transfer,    // Hint for server to stop async transfers.
	sp_reg_request,
	sp_reg_reply,
	sp_announce_cp,
	cn_timing,
	// Upper layer credit control Added in Gemini Version
	cn_ack_credit,
	// Synchronization primitives.
	cp_barrier,
	cp_lock,
	// Shared spaces specific.
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
#endif
	//Added for CCGrid Demo
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

//------------------
int rpc_smsg_init(struct rpc_server *rpc_s, int num);
int rpc_smsg_config(struct rpc_server *rpc_s, struct node_id *peer);
int rpc_ep_smsg_init(struct rpc_server *rpc_s, struct node_id *peer);
void rpc_smsg_check(struct rpc_server *rpc_s);

int sys_smsg_init(struct rpc_server *rpc_s, int num);
int sys_smsg_config(struct rpc_server *rpc_s, struct node_id *peer);
int sys_ep_smsg_init(struct rpc_server *rpc_s, struct node_id *peer);
void sys_smsg_check(struct rpc_server *rpc_s);

void peer_smsg_check(struct rpc_server *rpc_s, struct node_id *peer, gni_smsg_attr_t *smsg_attr);
//------------------


struct node_id *gather_node_id(int appid, void *comm);

int rpc_read_config(struct ptlid_map *ptlmap);
int rpc_write_config(struct rpc_server *rpc_s);

int rpc_read_socket(struct sockaddr_in *address);
int rpc_write_socket(struct rpc_server *rpc_s);

struct rpc_server *rpc_server_init (int num_buff, int num_rpc_per_buff, void *dart_ref, enum rpc_component cmp_type, int appid, void *comm);
void rpc_server_set_peer_ref(struct rpc_server *rpc_s, struct node_id peer_tab[], int num_peers);
void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff);
int rpc_server_free(struct rpc_server *rpc_s, void *commm);
struct rpc_server *rpc_server_get_instance(void);
int rpc_server_get_id(void);

int rpc_process_event(struct rpc_server *rpc_s);
int rpc_process_event_with_timeout(struct rpc_server *rpc_s, int timeout);

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func);
int rpc_barrier(struct rpc_server *rpc_s);

int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);
int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg);

void rpc_report_md_usage(struct rpc_server *rpc_s);
struct msg_buf *msg_buf_alloc(struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs);

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);
void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd);

#endif
