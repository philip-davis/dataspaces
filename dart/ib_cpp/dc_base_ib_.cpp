/*
 *  Base implementation of DART client.
 *
 *  Tong Jin 
 *  TASSL Rutgers University
 *  Hoang Bui (2012-2013) TASSL Rutgers University, hbui@cac.rutgers.edu
 *  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/stat.h>

#include <vector>
#include <string>
#include <sstream>

#include "mpi.h"
#include "debug.h"
#include "dc_base_ib.h"
#include "patch_ib_cpp.h"

#define speer_in_charge_id(rpc_s_, dc_) (rpc_s_->ptlmap.id % dc_->num_sp)

static const int to_str_length = 500;
static char to_str_char_[to_str_length];
const char* dc_to_str(const char* type, const void* struct_)
{
  std::stringstream ss;
  if (cstr_cstr_equals(type, (const char*)"dart_client") ) {
    struct dart_client* p_ = (struct dart_client*)struct_;
    ss << "rpc_s= { \n" << rpc_to_str("rpc_server", p_->rpc_s) << "\n\t } \n"
       << "peer_size= " << p_->peer_size << "\n"
       << "num_sp= " << p_->num_sp << ", num_cp= " << p_->num_cp << ", num_cp_all= " << p_->num_cp_all << "\n"
       << "cp_in_job= " << p_->cp_in_job << ", cp_min_rank= " << p_->cp_min_rank << "\n"
       << "f_reg= " << p_->f_reg << ", f_reg_all= " << p_->f_reg_all << ", f_bar= " << p_->f_bar << "\n"
       << "num_posted= " << p_->num_posted << "\n";
  }
  else {
    log(ERROR, "unknown type= " << type)
    return NULL;
  }
  
  const std::string& str = ss.str();
  if (str.length() >= to_str_length) {
    log(ERROR, "str.length()= " << str.length() << " > to_str_length= " << to_str_length)
    return NULL;
  }
  memcpy(to_str_char_, str.c_str(), str.length() );
  to_str_char_[str.length() ] = '\0';
  return to_str_char_;
}

struct node_id* dc_get_peer(struct dart_client* dc_, int peer_id) {
  // log(DEBUG, "called for; peer_id= " << peer_id)
  return rpc_get_peer(dc_->rpc_s, peer_id);
}

// Called back by rpc_cb_req_completion which is called back by rpc_process_event upon completion
// of rpc_receive_direct'ing peers info from DS_Master
static int announce_cp_completion(struct rpc_server* rpc_s_, struct msg_buf* msg_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id)
  int err;
	struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
	
	struct ptlid_map* pm_ = (struct ptlid_map*)msg_->msg_data;
	for (int i = 0; i < dc_->num_sp + dc_->num_cp; i++, pm_++) {
	  if (is_peer_in(pm_->id) )
      continue;
    log(DEBUG, "adding to ptl; rpc_s_id= " << rpc_s_->ptlmap.id << ", pm_= \n" << rpc_to_str("ptlid_map", pm_) )
		struct node_id* peer_;
		return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), peer_, ==, 0, -ENOMEM)
		memset(peer_, 0, sizeof(*peer_) );
		peer_->ptlmap = *pm_;
		INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
		peer_->num_msg_at_peer = rpc_s_->max_num_msg;
		peer_->num_msg_ret = 0;
		add_to_id_peer_map(peer_->ptlmap.id, peer_);
		// if (peer_->ptlmap.type == PEER_SERVER && peer_->ptlmap.id == speer_in_charge_id(rpc_s_, dc_) ) {
		//   log(DEBUG, "rpc_connecting peer_id= " << peer_->ptlmap.id)
		//   try_n_times__return_if_err(rpc_connect(rpc_s_, peer_), err, 3)
		// }
	}
	dc_->cp_min_rank = dc_->rpc_s->app_minid;
	dc_->f_reg = 1;
	
	free(msg_->msg_data);
	free(msg_);
	log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
	return 0;
}

// Called back by rpc_cb_req_completion which is called back by rpc_process_event upon completion
// of recving sp_announce_cp from DS_Master
int dcrpc_announce_cp(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", cmd_= \n" << rpc_to_str("rpc_cmd", cmd_) )
	int err;
	struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
	struct hdr_register* hreg_ = (struct hdr_register*)cmd_->pad;
	dc_->num_sp = hreg_->num_sp;
	dc_->num_cp = hreg_->num_cp;
	
	struct node_id* peer_ = rpc_get_peer(rpc_s_, cmd_->id); // cmd_->id is supposed to be master_server id
	struct msg_buf* msg_;
	return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 0), msg_, ==, 0, -ENOMEM)
	msg_->size = sizeof(struct ptlid_map) * (hreg_->num_cp + hreg_->num_sp);
	return_err_if_ret_cond_flag(malloc(msg_->size), msg_->msg_data, ==, 0, -ENOMEM, free(msg_);)
	msg_->cb = announce_cp_completion;
	msg_->id = cmd_->wr_id;
	msg_->mr = cmd_->mr;
  log(DEBUG, "rpc_receive_direct announce_cp_completion from peer_id= " << peer_->ptlmap.id)
	return_err_if_ret_cond_flag(rpc_receive_direct(rpc_s_, peer_, msg_), err, <, 0, err, free(msg_);)
	
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
	return 0;
}

// Called back by rpc_cb_req_completion which is called back by rpc_process_event upon completion
// of rpc_receive_direct'ing info of all the peers from DS_Master
int announce_cp_completion_all(struct rpc_server* rpc_s_, struct msg_buf* msg_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id)
  int err;
  struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
  struct ptlid_map* pm_ = (struct ptlid_map*)msg_->msg_data;
  for (int i = 0; i < dc_->num_sp + dc_->num_cp_all; i++, pm_++) {
    if (is_peer_in(pm_->id) )
      continue;
    log(DEBUG, "adding to ptl; rpc_s_id= " << rpc_s_->ptlmap.id << ", pm_= \n" << rpc_to_str("ptlid_map", pm_) )
    struct node_id* peer_;
    return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), peer_, ==, 0, -ENOMEM)
    memset(peer_, 0, sizeof(*peer_) );
    peer_->ptlmap = *pm_;
    INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
    peer_->num_msg_at_peer = rpc_s_->max_num_msg;
    peer_->num_msg_ret = 0;
    add_to_id_peer_map(peer_->ptlmap.id, peer_);
    // if (peer_->ptlmap.type == PEER_SERVER && peer_->ptlmap.id == speer_in_charge_id(rpc_s_, dc_) ) {
    //   log(DEBUG, "rpc_connecting peer_id= " << peer_->ptlmap.id)
    //   try_n_times__return_if_err(rpc_connect(rpc_s_, peer_), err, 3)
    // }
  }
  dc_->cp_min_rank = dc_->rpc_s->app_minid;
  dc_->f_reg_all = 1;
  
  free(msg_->msg_data);
  free(msg_);
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
  return 0;
}

// Called back by rpc_cb_req_completion which is called back by rpc_process_event upon completion
// of recving sp_announce_cp_all from DS_Master
int dcrpc_announce_cp_all(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", cmd_= \n" << rpc_to_str("rpc_cmd", cmd_) )
	int err;
	struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
	struct hdr_register* hreg_ = (struct hdr_register*)cmd_->pad;
	dc_->num_sp = hreg_->num_sp;
	dc_->num_cp_all = hreg_->num_cp;
	
  if (speer_in_charge_id(rpc_s_, dc_) != cmd_->id) {
    log(ERROR, "recved sp_announce_cp_all from unexpected peer_id= " << cmd_->id
               << ", which is not speer_in_charge_id= " << speer_in_charge_id(rpc_s_, dc_) )
    return 1;
  }
	struct node_id* peer_ = rpc_get_peer(rpc_s_, cmd_->id);
  
	struct msg_buf* msg_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 0), msg_, ==, 0, -ENOMEM)
	msg_->size = (hreg_->num_cp + hreg_->num_sp)*sizeof(struct ptlid_map);
	return_err_if_ret_cond_flag(malloc(msg_->size), msg_->msg_data, ==, 0, -ENOMEM, free(msg_);)
	msg_->cb = announce_cp_completion_all;
	msg_->id = cmd_->wr_id;
	msg_->mr = cmd_->mr;
	log(DEBUG, "rpc_receive_direct announce_cp_completion_all from peer_id= " << peer_->ptlmap.id)
	return_err_if_ret_cond_flag(rpc_receive_direct(rpc_s_, peer_, msg_), err, <, 0, err, free(msg_);)
	
	log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
	return 0;
}

// RPC routine  to wait for  server confirmation that it  processed ourunregister message and all our other messages.
int dcrpc_unregister(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id)
  // log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", cmd_= \n" << rpc_to_str("rpc_cmd", cmd_) )
	struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
	dc_->f_reg = 0;
	
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
	return 0;
}

void* dc_listen(void* client)
{
	struct dart_client* dc_ = (struct dart_client*) client;
  log(DEBUG, "started; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
  int err;
	struct con_param conpara;
	struct connection* conn_;
	struct rdma_cm_event* event_;
	struct node_id* peer_;
	while (dc_->rpc_s->thread_alive && (rdma_get_cm_event(dc_->rpc_s->rpc_ec, &event_) == 0) ) {
		struct rdma_cm_event event_copy;
		memcpy(&event_copy, event_, sizeof(*event_) );
		rdma_ack_cm_event(event_);
		if (event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
			conpara = *(struct con_param*)event_copy.param.conn.private_data;
			peer_ = rpc_get_peer(dc_->rpc_s, conpara.pm_cp.id);
			log(DEBUG, "recved RDMA_CM_EVENT_CONNECT_REQUEST from server_id= " << peer_->ptlmap.id)
			if (conpara.type == 0) {
			  log(WARNING, "DEPRECATED; should not have come here!")
				conn_ = &(peer_->sys_conn);
			}
			else {
				conn_ = &(peer_->rpc_conn);
			}
			build_context(event_copy.id->verbs, conn_);
			build_qp_attr(&(conn_->qp_attr), conn_, dc_->rpc_s);
			return_err_if_ret_cond_flag(rdma_create_qp(event_copy.id, conn_->pd, &(conn_->qp_attr) ), err, !=, 0, NULL)
			event_copy.id->context = conn_;
			conn_->id = event_copy.id;
			conn_->qp = event_copy.id->qp;
			if (conpara.type == 0) {
			  log(WARNING, "DEPRECATED; should not have come here!")
			  return_err_if_ret_cond_flag(sys_post_recv(dc_->rpc_s, peer_), err, !=, 0, NULL)
			}
			else {
			  return_err_if_ret_cond_flag(rpc_post_recv(dc_->rpc_s, peer_), err, !=, 0, NULL)
			}
			
      struct rdma_conn_param cm_params;
			memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
			cm_params.private_data = &(peer_->ptlmap.id);
			cm_params.private_data_len = sizeof(int);
			cm_params.initiator_depth = cm_params.responder_resources = 1;
			cm_params.retry_count = 7;
			cm_params.rnr_retry_count = 7;	//infinite retry
		  return_err_if_ret_cond_flag(rdma_accept(event_copy.id, &cm_params), err, !=, 0, NULL)
			conn_->f_connected = 1;
		}
		else if (event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
		}
		else if (event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
		} 
		else {
		  // err = event_copy.status;
		  log(ERROR, "unknown event= " << rpc_event_to_str(&event_copy) << ", event status= " << event_copy.status)
			return NULL;
		}
	}
  
  log(DEBUG, "done; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
	return 0;
}

int dc_boot(struct dart_client* dc_, int app_id)
{
  log(DEBUG, "started; rpc_s_id= " << dc_->rpc_s->ptlmap.id << ", app_id= " << app_id)
	int err;
	
	struct node_id* master_peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), master_peer_, ==, 0, NULL)
	memset(master_peer_, 0, sizeof(*master_peer_) );
	INIT_LIST_HEAD(patch_ib::to_str<>(master_peer_->ptlmap.id) + "_req_list", &(master_peer_->req_list) );
  master_peer_->num_msg_at_peer = dc_->rpc_s->max_num_msg;
  master_peer_->num_msg_ret = 0;
	master_peer_->ptlmap.type = PEER_SERVER;
	
	return_err_if_ret_cond_flag(rpc_read_config(&(master_peer_->ptlmap.address) ), err, <, 0, err)
  // Connect to master server, build rpc channel and sys channel;
  log(DEBUG, "rpc_connecting to master_peer_id= " << master_peer_->ptlmap.id << ", master_peer_->ptlmap= " << rpc_to_str("ptlid_map", &(master_peer_->ptlmap) ) )
  try_n_times__return_if_err(rpc_connect(dc_->rpc_s, master_peer_), err, 3)
  log(DEBUG, "adding master_peer_= \n" << rpc_to_str("node_id", master_peer_) )
  add_to_id_peer_map(master_peer_->ptlmap.id, master_peer_);
  log(DEBUG, "after connecting; master_peer_id= " << master_peer_->ptlmap.id << "\n"
            // << ", master_peer_->ptlmap= " << rpc_to_str("ptlid_map", &(master_peer_->ptlmap) ) << "\n"
             << "\t master_peer_->rpc_conn.f_connected= " << master_peer_->rpc_conn.f_connected << "\n"
             << "\t rpc_s_id= " << dc_->rpc_s->ptlmap.id)
	
	dc_->cp_min_rank = dc_->rpc_s->app_minid; // TODO
  
  struct node_id* this_peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), this_peer_, ==, 0, NULL)
  memset(this_peer_, 0, sizeof(*this_peer_) );
  INIT_LIST_HEAD(patch_ib::to_str<>(this_peer_->ptlmap.id) + "_req_list", &(this_peer_->req_list) );
  this_peer_->num_msg_at_peer = dc_->rpc_s->max_num_msg;
  this_peer_->num_msg_ret = 0;
  this_peer_->ptlmap = dc_->rpc_s->ptlmap;
  log(DEBUG, "adding this_peer_= \n" << rpc_to_str("node_id", this_peer_) )
  add_to_id_peer_map(this_peer_->ptlmap.id, this_peer_);
  dc_->self = this_peer_;
  
  // For incoming conns
  return_if_err(pthread_create(&(dc_->rpc_s->comm_thread), NULL, dc_listen, (void*)dc_), err)
  dc_->rpc_s->thread_alive = 1;
  
	// Waiting for dissemination msg from master server -- when announce_cp_completion is called back
	log(DEBUG, "WAITING for dissemination msg from master server...")
	while (dc_->f_reg == 0) {
	  return_if_err(rpc_process_event_with_timeout(dc_->rpc_s, 1), err)
	}
  
  // struct node_id **client_peer_head__, **server_peer_head__;
  // return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  // for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++) {
  //   struct node_id* peer_ = *t_server_peer_head__;
  //   if (peer_->rpc_conn.f_connected == 1)
  //     continue;
    
  //   log(DEBUG, "rpc_connecting to server peer_id= " << peer_->ptlmap.id)
  //   try_n_times__return_if_err(rpc_connect(dc_->rpc_s, peer_), err, 3)
  // }
  // free(client_peer_head__);
  // free(server_peer_head__);
  
  struct node_id* speer_in_charge_ = rpc_get_peer(dc_->rpc_s, speer_in_charge_id(dc_->rpc_s, dc_) );
  struct msg_buf* msg_;
  return_err_if_ret_cond_flag(msg_buf_alloc(dc_->rpc_s, speer_in_charge_, 1), msg_, ==, 0, -ENOMEM)
  msg_->msg_rpc->id = dc_->rpc_s->ptlmap.id;
  msg_->msg_rpc->cmd = ready_for_disseminate_all;
  log(DEBUG, "rpc_sending ready_for_disseminate_all to speer_in_charge_id= " << speer_in_charge_->ptlmap.id)
  return_err_if_ret_cond_flag(rpc_send(dc_->rpc_s, speer_in_charge_, msg_), err, <, 0, err, free(msg_);)
  
  log(DEBUG, "WAITING for dissemination_all msg from server peer_id= " << speer_in_charge_id(dc_->rpc_s, dc_) )
	while (dc_->f_reg_all == 0) {
	  return_if_err(rpc_process_event_with_timeout(dc_->rpc_s, 1), err)
	}
  // For put/get performance
  struct node_id **client_peer_head__, **server_peer_head__;
  return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++) {
    struct node_id* peer_ = *t_server_peer_head__;
    if (peer_->rpc_conn.f_connected == 1)
      continue;
    
    log(DEBUG, "rpc_connecting to server peer_id= " << peer_->ptlmap.id)
    try_n_times__return_if_err(rpc_connect(dc_->rpc_s, peer_), err, 3)
  }
  free(client_peer_head__);
  free(server_peer_head__);
	
	dc_->rpc_s->cur_num_peer = dc_->num_sp + dc_->num_cp_all;
	dc_->rpc_s->num_sp = dc_->num_sp;
  log(DEBUG, "done; rpc_s_id= " << dc_->rpc_s->ptlmap.id << ", app_id= " << app_id)
	return 0;
}

//------------------------------------------  Public API  ----------------------------------------//
int barrier_broadcast(struct dart_client* dc_)
{
  log(DEBUG, "started; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
  int err;
  struct msg_buf* msg_;
  struct node_id* peer_;
  int i;
  for (i = 1; i < dc_->cp_in_job; i++) {
    peer_ = dc_get_peer(dc_, dc_->self->ptlmap.id + i);
    return_err_if_ret_cond_flag(msg_buf_alloc(dc_->rpc_s, peer_, 1), msg_, ==, 0, -ENOMEM)
    msg_->msg_rpc->cmd = cp_barrier;
    msg_->msg_rpc->id = dc_->self->ptlmap.id;
    log(DEBUG, "rpc_sending cp_barrier to peer_id= " << peer_->ptlmap.id << ", msg_= \n" << rpc_to_str("msg_buf", msg_) )
    return_err_if_ret_cond_flag(rpc_send(dc_->rpc_s, peer_, msg_), err, <, 0, err, free(msg_);)
  }
  // if (i == dc_->cp_in_job) {
  log(DEBUG, "done; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
  return 0;
  // }
}

// Implements a simple 'start' barrier across the nodes in an app job.
// Called back by rpc_process_event upon receiving cp_barrier from another client -- master client sends this to others in barrier_broadcast
// TODO
int dcrpc_barrier(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id)
  struct dart_client* dc_ = dc_ref_from_rpc(rpc_s_);
  int err;
  if (dc_->self->ptlmap.id == dc_->cp_min_rank) { // This is the master peer in this job.
    dc_->cp_barrier_req++;
    if (dc_->cp_barrier_req < dc_->cp_in_job)
      return 0;
    // Received from all the client peers, ready to release barrier on other clients
    return_err_if_ret_cond_flag(barrier_broadcast(dc_), err, <, 0, err)
    dc_->cp_barrier_req = 0;
    dc_->f_bar = 1;
  }
  else
    dc_->f_bar = 1; // Non-master peer in this job.
  
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
  return 0;
}

int dc_barrier(struct dart_client* dc_)
{
	if (dc_->comm != 0)
		MPI_Barrier(dc_->comm);
	else {
	  int err;
		return_if_err(rpc_barrier(dc_->rpc_s), err)
	}
	return 0;
}

struct dart_client* dc_alloc(int num_peers, int app_id, void* dart_ref_, void *comm_)
{
  log(DEBUG, "started; num_peers= " << num_peers << ", app_id= " << app_id)
	struct dart_client* dc_;
  
	int err;
	return_err_if_ret_cond_flag((struct dart_client*)calloc(1, sizeof(*dc_) ), dc_, ==, 0, NULL)
	dc_->dart_ref = dart_ref_;
	dc_->cp_in_job = num_peers;
  
  // rpc_add_service(cn_register, dcrpc_register);
	rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);
	rpc_add_service(sp_announce_cp, dcrpc_announce_cp);
	rpc_add_service(sp_announce_cp_all, dcrpc_announce_cp_all);
	if (comm_ != NULL) {
		return_err_if_ret_cond_flag(MPI_Comm_dup(*(MPI_Comm*)comm_, &(dc_->comm) ), err, <, 0, NULL)
	}
  
  return_err_if_ret_cond_flag(rpc_server_init(NULL, 0, 10, num_peers, dc_, DART_CLIENT), dc_->rpc_s, ==, 0, NULL)
	dc_->rpc_s->ptlmap.appid = app_id;
	dc_->rpc_s->ptlmap.type = PEER_CLIENT;
	dc_->rpc_s->app_num_peers = num_peers;
	dc_->rpc_s->cur_num_peer = 2; // TODO
	dc_->rpc_s->num_peers = dc_->peer_size = dc_->rpc_s->cur_num_peer;
	if (dc_boot(dc_, app_id) < 0) {
	  log(ERROR, "dc_boot failed; app_id= " << app_id)
	  return_err_if_ret_cond_flag(rpc_server_free(dc_->rpc_s), err, !=, 0, NULL)
	  return NULL;
	}
	log(DEBUG, "dc_alloc succeeded; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
	return_err_if_ret_cond_flag(dc_barrier(dc_), err, <, 0, NULL, free(dc_);)
  
  log(DEBUG, "done; dc_= \n" << dc_to_str("dart_client", dc_) )
	return dc_;
}

int dc_unregister(struct dart_client* dc_)
{
  log(DEBUG, "started; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
  struct msg_buf* msg_;
  int err = -ENOMEM;
  
  struct node_id* speer_in_charge_ = rpc_get_peer(dc_->rpc_s, speer_in_charge_id(dc_->rpc_s, dc_) );
  return_err_if_ret_cond_flag(msg_buf_alloc(dc_->rpc_s, speer_in_charge_, 1), msg_, ==, 0, err)
  msg_->msg_rpc->cmd = cn_unregister;
  msg_->msg_rpc->id = dc_->rpc_s->ptlmap.id;
  struct hdr_register* hreg_ = (struct hdr_register*) msg_->msg_rpc->pad;
  hreg_->num_sp = dc_->num_sp;
  hreg_->num_cp = 1;
  hreg_->pm_cp = dc_->rpc_s->ptlmap;
  hreg_->pm_sp = speer_in_charge_->ptlmap;
  log(DEBUG, "rpc_sending cn_unregister to peer_id= " << speer_in_charge_->ptlmap.id << ", msg_= " << rpc_to_str("msg_buf", msg_) )
  return_if_err(rpc_send(dc_->rpc_s, speer_in_charge_, msg_), err)
  // Should wait here for 'unregister' confirmation. 
  while (dc_->f_reg) {
    return_if_err(rpc_process_event(dc_->rpc_s), err)
  }
  
  log(DEBUG, "done; rpc_s_id= " << dc_->rpc_s->ptlmap.id)
  return 0;
}

// TODO
int dc_free(struct dart_client* dc_)
{
	int err;
	return_if_err(dc_unregister(dc_), err)
	return_if_err(rpc_server_free(dc_->rpc_s), err)
	
	free(dc_);
	return 0;
}

int dc_process(struct dart_client* dc_) { return rpc_process_event(dc_->rpc_s); }
