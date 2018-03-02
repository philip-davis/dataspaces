/*
 *  Base implementation of DART server.
 *
 *  Tong Jin 
 *  TASSL Rutgers University
 *  Hoang Bui (2012-2013) TASSL Rutgers University, hbui@cac.rutgers.edu
 *  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
 * 
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */
#include "ds_base_ib.h"
#include "patch_ib_cpp.h"

static int sp_rank_cnt = 0;
static int cp_rank_cnt = 0;

// patch_ib::thread_safe_map<int, struct app_info*> app_id__app_map;
patch_ib::map<int, struct app_info*> app_id__app_map;

int add_to_app_id__app_map(int app_id, struct app_info* app_)
{
  if (app_id__app_map.contains(app_id) ) {
    log(WARNING, "already in; app_id= " << app_id)
    return 1;
  }
  app_id__app_map[app_id] = app_;
  return 0;
}

static const int to_str_length = 2000;
static char to_str_char_[to_str_length];
const char* ds_to_str(const char* type, const void* struct_)
{
  std::stringstream ss;
  if (cstr_cstr_equals(type, (const char*)"app_info") ) {
    struct app_info* p_ = (struct app_info*)struct_;
    ss << "app_id= " << p_->app_id << "\n"
       << "app_num_peers= " << p_->app_num_peers << "\n"
       << "base_id= " << p_->base_id << ", app_cnt_peers= " << p_->app_cnt_peers << "\n";
  }
  else if (cstr_cstr_equals(type, (const char*)"dart_server") ) {
    struct dart_server* p_ = (struct dart_server*)struct_;
    ss << "rpc_s= { \n" << rpc_to_str("rpc_server", p_->rpc_s) << "\n\t } \n"
       << "peer_size= " << p_->peer_size << "\n"
       << "num_sp= " << p_->num_sp << ", num_cp= " << p_->num_cp << "\n"
       << "f_reg= " << p_->f_reg << ", f_stop= " << p_->f_stop << ", f_unreg= " << p_->f_unreg << "\n"
       << "num_charge= " << p_->num_charge << "\n"
       << "app_id__app_map= \n";
    for (std::map<int, struct app_info*>::iterator it = app_id__app_map.begin(); it != app_id__app_map.end(); it++)
      ss << "\t app_id= " << it->first << " : app_info_= \n" << ds_to_str("app_info", it->second) << "\n";
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

int dsrpc_unregister(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id)
  struct dart_server* ds_ = ds_ref_from_rpc(rpc_s_);
  ds_->f_reg = 0;
  ds_->f_unreg = 1;
  ds_->f_stop = 1;
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
  return 0;
}

int dsrpc_cn_unregister(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  struct dart_server* ds_ = ds_ref_from_rpc(rpc_s_);
  if (ds_->f_stop == 1) {
    log(WARNING, "rpc_s_id= " << rpc_s_->ptlmap.id << " is already unreged; ds_->f_stop= " << ds_->f_stop)
    return 0;
  }
  int err;
  log(DEBUG, "rpc_s_id= " << ds_->rpc_s->ptlmap.id << ", from peer_id= " << cmd_->id << "\n"
            << "\t ds_->num_cp= " << ds_->num_cp << ", (left num_client)ds_->num_charge= " << ds_->num_charge)
  struct hdr_register* hreg_ = (struct hdr_register*)(cmd_->pad);
  struct msg_buf* msg_;
  struct node_id* peer_ = rpc_get_peer(rpc_s_, cmd_->id);
  if (peer_->ptlmap.type == PEER_SERVER) {
    return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_, ==, 0, -ENOMEM)
    msg_->msg_rpc->id = rpc_s_->ptlmap.id;
    msg_->msg_rpc->cmd = cn_s_unregister;
    
    log(DEBUG, "rpc_sending cn_s_unregister to peer_id= " << peer_->ptlmap.id)
    return_err_if_ret_cond_flag(rpc_send(rpc_s_, peer_, msg_), err, <, 0, err, free(msg_);)
    peer_->f_unreg = 1;
    return_if_err(del_from_id_peer_map(peer_->ptlmap.id), err)
  }
  else if (peer_->ptlmap.type == PEER_CLIENT) {
    if (peer_->f_unreg != 1) {
      return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_, ==, 0, err)
      msg_->msg_rpc->id = rpc_s_->ptlmap.id;
      msg_->msg_rpc->cmd = cn_unregister;
      
      log(DEBUG, "rpc_sending cn_unregister to peer_id= " << peer_->ptlmap.id)
      return_err_if_ret_cond_flag(rpc_send(rpc_s_, peer_, msg_), err, <, 0, err, free(msg_);)
      peer_->f_unreg = 1;
      return_if_err(del_from_id_peer_map(peer_->ptlmap.id), err)
    }
    --ds_->num_charge;
  }
  else {
    log(ERROR, "unexpected peer_type= " << peer_->ptlmap.type)
    return 1;
  }
  
  if (ds_->num_charge == 0) {
    log(WARNING, "all peers I am in charge unreged.")
    if (ds_->num_sp < 2) {
      log(WARNING, "I was the only server, no need for me anymore, killing myself...")
      ds_->f_stop = 1;
      // return_if_err(ds_free(ds_), err) // this is already called via dsg_free(dsg) in common_run_server in common.c
      return 0;
    }
  }
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

// Broadcast sp_announce_cp_all cmd to every client the server is in charge with
int ds_disseminate_all(struct dart_server* ds_)
{
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  int err;
  struct node_id* peer_;
  struct msg_buf* msg_;
  struct ptlid_map* pptlmap_;
  // To clients
  struct node_id **client_peer_head__, **server_peer_head__;
  return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  for (struct node_id** t_client_peer_head__ = client_peer_head__; *t_client_peer_head__ != NULL; t_client_peer_head__++, pptlmap_++) {
    peer_ = *t_client_peer_head__;
    
    if (peer_->ptlmap.id % ds_->size_sp != ds_->rpc_s->ptlmap.id) // is server in charge?
      continue;
    // while (peer_->rpc_conn.f_connected != 1) {
    //   return_if_err(pthread_yield(), err)
    // }
    
    return_err_if_ret_cond_flag(msg_buf_alloc(ds_->rpc_s, peer_, 1), msg_, ==, 0, -ENOMEM)
    msg_->cb = default_completion_with_data_callback;
    msg_->size = (ds_->size_sp + ds_->size_cp)*sizeof(struct ptlid_map);
    return_err_if_ret_cond_flag((struct ptlid_map*)malloc(msg_->size), msg_->msg_data, ==, 0, -ENOMEM, free(msg_);)
    pptlmap_ = (struct ptlid_map*)msg_->msg_data;
    for (struct node_id** t2_server_peer_head__ = server_peer_head__; *t2_server_peer_head__ != NULL; t2_server_peer_head__++, pptlmap_++)
      *pptlmap_ = (*t2_server_peer_head__)->ptlmap;
    for (struct node_id** t2_client_peer_head__ = client_peer_head__; *t2_client_peer_head__ != NULL; t2_client_peer_head__++, pptlmap_++)
      *pptlmap_ = (*t2_client_peer_head__)->ptlmap;
    
    msg_->msg_rpc->cmd = sp_announce_cp_all;
    msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
    struct hdr_register* hreg_ = (struct hdr_register*)msg_->msg_rpc->pad;
    hreg_->pm_cp = ds_->rpc_s->ptlmap;
    hreg_->num_cp = ds_->size_cp;
    hreg_->num_sp = ds_->size_sp;
    log(DEBUG, "rpc_sending sp_announce_cp_all to peer_id= " << peer_->ptlmap.id)
    return_err_if_ret_cond_flag(rpc_send(ds_->rpc_s, peer_, msg_), err, <, 0, err, free(msg_->msg_data); free(msg_);)
    return_if_err(rpc_process_event(ds_->rpc_s), err, free(msg_);)
  }
  free(client_peer_head__);
  free(server_peer_head__);
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

int ds_disseminate(struct dart_server* ds_)
{
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  int err;
  struct node_id* peer_;
  struct ptlid_map* pptlmap_;
  struct hdr_register* hreg_;
  struct msg_buf* msg_;
  struct app_info* app_;
  // To servers
  struct node_id **client_peer_head__, **server_peer_head__;
  return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++) {
    peer_ = *t_server_peer_head__;
    if (peer_->ptlmap.id == ds_->rpc_s->ptlmap.id)
      continue;
    err = -ENOMEM;
    return_err_if_ret_cond_flag(msg_buf_alloc(ds_->rpc_s, peer_, 1), msg_, ==, 0, err)
    msg_->cb = default_completion_with_data_callback;
    msg_->size = (ds_->peer_size)*sizeof(struct ptlid_map);
    return_err_if_ret_cond_flag((struct ptlid_map*)malloc(msg_->size), msg_->msg_data, ==, 0, err, free(msg_);)
    pptlmap_ = (struct ptlid_map*)msg_->msg_data;
    std::stringstream log_ss;
    for (struct node_id** t2_server_peer_head__ = server_peer_head__; *t2_server_peer_head__ != NULL; t2_server_peer_head__++, pptlmap_++) {
      *pptlmap_ = (*t2_server_peer_head__)->ptlmap;
      log_ss << "\t server peer_id= " << pptlmap_->id << "\n";
    }
    for (struct node_id** t2_client_peer_head__ = client_peer_head__; *t2_client_peer_head__ != NULL; t2_client_peer_head__++, pptlmap_++) {
      *pptlmap_ = (*t2_client_peer_head__)->ptlmap;
      log_ss << "\t client peer_id= " << pptlmap_->id << "\n";
    }
    log(DEBUG, "rpc_s_id= " << ds_->rpc_s->ptlmap.id << ", added to sp_announce_cp the peers= \n" << log_ss.str() )
    
    msg_->msg_rpc->cmd = sp_announce_cp;
    msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
    hreg_ = (struct hdr_register*)msg_->msg_rpc->pad;
    hreg_->pm_cp = ds_->rpc_s->ptlmap;
    hreg_->num_cp = ds_->peer_size - ds_->size_sp;
    hreg_->num_sp = ds_->size_sp;
    log(DEBUG, "rpc_sending sp_announce_cp to peer_id= " << peer_->ptlmap.id)
    return_err_if_ret_cond_flag(rpc_send(ds_->rpc_s, peer_, msg_), err, <, 0, err, free(msg_->msg_data); free(msg_);)
    return_if_err(rpc_process_event(ds_->rpc_s), err, free(msg_);)
  }
  free(client_peer_head__);
  free(server_peer_head__);
  // To clients
  for (std::map<int, struct app_info*>::iterator it = app_id__app_map.begin(); it != app_id__app_map.end(); it++) {
    app_ = it->second;
    for (short int peer_id = app_->base_id; peer_id < app_->base_id + app_->app_cnt_peers; peer_id++) {
      return_err_if_ret_cond_flag(rpc_get_peer(ds_->rpc_s, peer_id), peer_, ==, NULL, 1)
      
      err = -ENOMEM;
      return_err_if_ret_cond_flag(msg_buf_alloc(ds_->rpc_s, peer_, 1), msg_, ==, 0, err)
      msg_->cb = default_completion_with_data_callback;
      msg_->size = (ds_->size_sp + app_->app_num_peers)*sizeof(struct ptlid_map);
      return_err_if_ret_cond_flag((struct ptlid_map*)malloc(msg_->size), msg_->msg_data, ==, 0, err, free(msg_);)
      pptlmap_ = (struct ptlid_map*)msg_->msg_data;
      
      struct node_id **client_peer_head__, **server_peer_head__;
      return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
      for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++, pptlmap_++) {
        *pptlmap_ = (*t_server_peer_head__)->ptlmap;
        // log(DEBUG, "server; pptlmap_= \n" << rpc_to_str("ptlid_map", pptlmap_) )
      }
      free(client_peer_head__);
      free(server_peer_head__);
      for (short int _peer_id = app_->base_id; _peer_id < app_->base_id + app_->app_cnt_peers; _peer_id++, pptlmap_++) {
        struct node_id* t_peer_;
        return_err_if_ret_cond_flag(rpc_get_peer(ds_->rpc_s, _peer_id), t_peer_, ==, NULL, 1)
        *pptlmap_ = t_peer_->ptlmap;
        // log(DEBUG, "client; pptlmap_= \n" << rpc_to_str("ptlid_map", pptlmap_) )
      }
      
      msg_->msg_rpc->cmd = sp_announce_cp;
      msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
      hreg_ = (struct hdr_register*)msg_->msg_rpc->pad;
      hreg_->pm_cp = ds_->rpc_s->ptlmap;
      hreg_->num_cp = app_->app_num_peers;
      hreg_->num_sp = ds_->size_sp;
      log(DEBUG, "rpc_sending sp_announce_cp to peer_id= " << peer_->ptlmap.id)
      return_err_if_ret_cond_flag(rpc_send(ds_->rpc_s, peer_, msg_), err, <, 0, err, free(msg_->msg_data); free(msg_);)
      return_if_err(rpc_process_event(ds_->rpc_s), err, free(msg_);)
    }
  }
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

/*****************************************  ds_rel  ***********************************************/
//TODO:  After getting rpc reg msg, check the qp num, change corresponding peer->conn->f_connected to 1 after get this
/*
  process the CQ to get the peer registration information.
  1. fetch one CQ event (the 1st one must be the registration event) 
  2. call ds_register_peer       
  3. Move the tmp_peer to the right position of peer_tab based on the registration info
*/
// RPC routine to serve a compute node registration request.
// Called within ds_boot_master upon collecting client infos once they register
int dsrpc_cn_register(struct rpc_server* rpc_s_, struct hdr_register* hdr_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << "\n"
             << "\t app_id= " << hdr_->pm_cp.appid << ", peer_id= " << hdr_->pm_cp.id)
  struct dart_server* ds_ = ds_ref_from_rpc(rpc_s_);
  int err = -ENOMEM;
  
  struct app_info* app_;
  if (!app_id__app_map.contains(hdr_->pm_cp.appid) ) {
    return_err_if_ret_cond_flag((struct app_info*)malloc(sizeof(struct app_info) ), app_, ==, 0, err)
    app_->app_id = hdr_->pm_cp.appid;
    app_->app_num_peers = hdr_->num_cp;
    app_->app_cnt_peers = 0;
    app_->base_id = cp_rank_cnt;
    log(DEBUG, "adding app_= \n" << ds_to_str("app_info", app_) )
    add_to_app_id__app_map(app_->app_id, app_);
    cp_rank_cnt = cp_rank_cnt + app_->app_num_peers;
  }
  else
    app_ = app_id__app_map[hdr_->pm_cp.appid];
  
  if (app_->app_cnt_peers == app_->app_num_peers) { // check if app has registered all its compute nodes already
    log(ERROR, "App cp is full; app_->app_cnt_peers= app_->app_num_peers= " << app_->app_num_peers)
    return err;
  }
  
  short int peer_id = app_->base_id + app_->app_cnt_peers;
  if (is_peer_in(peer_id) ) {
    log(WARNING, "peer already in; peer_id= " << peer_id)
    return 0;
  }
  struct node_id* peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), peer_, ==, 0, -ENOMEM)
  memset(peer_, 0, sizeof(*peer_) );
  peer_->ptlmap = hdr_->pm_cp;
  hdr_->id_min = hdr_->pm_cp.id = peer_->ptlmap.id = peer_id;
  INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
  peer_->num_msg_at_peer = rpc_s_->max_num_msg;
  peer_->num_msg_ret = 0;
  log(DEBUG, "adding peer_= \n" << rpc_to_str("node_id", peer_) )
  add_to_id_peer_map(peer_->ptlmap.id, peer_);
  
  app_->app_cnt_peers++;
  
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id)
  return 0;
}

/*
  waiting for all the connection requests from all the peers (ds_slave + dc)
  // allocate temp peer_tab to store all the connection info
  ds_register_peers: register all the peers including DS+DC
  // Relink/copy all the info from temp peer_tab to ds->peer_tab 
  Disseminate all the register information in ds->peer_tab to other's
  system channel connection
*/

// int dsrpc_unregister(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
int handle_slave_done_interconnecting(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  ((struct dart_server*)rpc_s_->dart_ref)->num_slaves_done_interconnecting++;
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  return 0;
}

int ds_boot_master(struct dart_server* ds_)
{
  ds_->rpc_s->ptlmap.id = sp_rank_cnt++;
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  
  struct node_id* this_peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), this_peer_, ==, 0, NULL)
  memset(this_peer_, 0, sizeof(*this_peer_) );
  INIT_LIST_HEAD(patch_ib::to_str<>(this_peer_->ptlmap.id) + "_req_list", &(this_peer_->req_list) );
  this_peer_->num_msg_at_peer = ds_->rpc_s->max_num_msg;
  this_peer_->num_msg_ret = 0;
  this_peer_->ptlmap = ds_->rpc_s->ptlmap;
  log(DEBUG, "adding this_peer_= \n" << rpc_to_str("node_id", this_peer_) )
  add_to_id_peer_map(this_peer_->ptlmap.id, this_peer_);
  ds_->self = this_peer_;
  
  int err, connect_count = 0, connected = 0;
  struct hdr_register hdr;
  struct node_id* peer_;
  struct connection* conn_;
  struct rdma_cm_event* event_;
  while (rdma_get_cm_event(ds_->rpc_s->rpc_ec, &event_) == 0) {
    struct rdma_cm_event event_copy;
    memcpy(&event_copy, event_, sizeof(*event_) );
    rdma_ack_cm_event(event_);
    if (event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
      struct con_param conpara = *((struct con_param*)event_copy.param.conn.private_data);
      // log(DEBUG, "conpara= \n" << rpc_to_str("con_param", &conpara) )
      if (conpara.type == 0) { // deprecated
        // peer_ = ds_get_peer(ds_, conpara.pm_cp.id);
        // conn_ = &(peer_->sys_conn);
        log(ERROR, "deprecated; should not have come here!")
      }
      else {
        char peer_type = (conpara.pm_cp).type;
        if (peer_type == PEER_SERVER) {
          return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), peer_, ==, 0, -ENOMEM)
          memset(peer_, 0, sizeof(*peer_) );
          INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
          peer_->num_msg_at_peer = ds_->rpc_s->max_num_msg;
          peer_->num_msg_ret = 0;
          peer_->ptlmap = conpara.pm_cp;
          peer_->ptlmap.id = sp_rank_cnt;
          log(DEBUG, "recved RDMA_CM_EVENT_CONNECT_REQUEST from server_id= " << peer_->ptlmap.id)
          log(DEBUG, "adding peer_= \n" << rpc_to_str("node_id", peer_) )
          add_to_id_peer_map(peer_->ptlmap.id, peer_);
          sp_rank_cnt++;
        }
        else if (peer_type == PEER_CLIENT) {
          hdr.pm_cp = conpara.pm_cp;
          hdr.pm_sp = conpara.pm_sp;
          hdr.num_cp = conpara.num_cp;
          dsrpc_cn_register(ds_->rpc_s, &hdr);
          peer_ = rpc_get_peer(ds_->rpc_s, hdr.pm_cp.id);
          log(DEBUG, "handled RDMA_CM_EVENT_CONNECT_REQUEST from client_id= " << peer_->ptlmap.id)
          conpara.pm_sp.id = peer_->ptlmap.id;
        }
        else {
          log(ERROR, "unexpected peer_type= " << peer_type)
          return 1;
        }
        conn_ = &(peer_->rpc_conn);
      }
      return_if_err(build_context(event_copy.id->verbs, conn_), err)
      return_if_err(build_qp_attr(&(conn_->qp_attr), conn_, ds_->rpc_s), err)
      return_if_err(rdma_create_qp(event_copy.id, conn_->pd, &(conn_->qp_attr) ), err)
      event_copy.id->context = conn_;
      conn_->id = event_copy.id;
      conn_->qp = event_copy.id->qp;
      
      if (conpara.type == 0) {
        return_if_err(sys_post_recv(ds_->rpc_s, peer_), err)
      }
      else {
        return_if_err(rpc_post_recv(ds_->rpc_s, peer_), err)
      }
      struct rdma_conn_param cm_params;
      memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
      if (conpara.pm_cp.appid != 0 && conpara.type == 1) {
        memset(&conpara, 0, sizeof(struct con_param) );
        conpara.pm_sp = peer_->ptlmap;
        conpara.pm_cp = ds_->rpc_s->ptlmap;
        conpara.num_cp = ds_->num_sp + ds_->num_cp;
        conpara.type = hdr.id_min;
        
        cm_params.private_data = &conpara;
        cm_params.private_data_len = sizeof(conpara);
      }
      else {
        cm_params.private_data = &(peer_->ptlmap.id);
        cm_params.private_data_len = sizeof(int);
      }
      cm_params.initiator_depth = cm_params.responder_resources = 1;
      cm_params.retry_count = 7;
      cm_params.rnr_retry_count = 7;
      return_if_err(rdma_accept(event_copy.id, &cm_params), err)
      connect_count++;
      conn_->f_connected = 1;
    }
    else if (event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
      log(DEBUG, "recved RDMA_CM_EVENT_ESTABLISHED.")
      connected++;
    }
    else {
      log(ERROR, "unknown event= " << rpc_event_to_str(&event_copy) )
      return event_copy.status;
    }
    if (connected == ds_->peer_size - 1)
      break;
  }
  
  if (connected != (ds_->peer_size - 1) || connected != connect_count) {
    log(ERROR, "Connected number doesn't match needed; connected= " << connected
               << ", ds_->peer_size= " << ds_->peer_size << ", connect_count= " << connect_count)
    return -1;
  }
  
  log(DEBUG, "All the peers are registered; ds_->peer_size= " << ds_->peer_size << ", ds_->size_cp= " << ds_->size_cp)
  ds_->rpc_s->cur_num_peer = ds_->rpc_s->num_rpc_per_buff;
  
  return_if_err(ds_disseminate(ds_), err)
  // TODO: Solve RDMA connecting error after servers recved disseminate and clients recved disseminate_all 
  // all will try to connect to all servers (not necessary, for performance during put/get), which will
  // cause RDMA_CM_EVENT_REJECTED error. As a solution, clients will be sent disseminate_all after servers
  // finish connecting to each other
  // log(DEBUG, "will sleep here for 10 sec...")
  // sleep(10);
  // log(DEBUG, "WAITING servers to finish inter-connecting... \n"
  //           << "\t rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  // while (ds_->num_slaves_done_interconnecting < ds_->num_sp - 1) {
  //   err = rpc_process_event_with_timeout(ds_->rpc_s, 1);
  //   if (err != 0 && err != -ETIME) {
  //     log(ERROR, "rpc_process_event_with_timeout failed!")
  //     return err;
  //   }
  // }
  // struct node_id **client_peer_head__, **server_peer_head__;
  // return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  // for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++) {
  //   struct node_id* peer_ = *t_server_peer_head__;
  //   if (peer_->ptlmap.id == ds_->rpc_s->ptlmap.id)
  //     continue;
    
  //   struct msg_buf* msg_;
  //   return_err_if_ret_cond_flag(msg_buf_alloc(ds_->rpc_s, peer_, 1), msg_, ==, 0, -ENOMEM)
  //   msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
  //   msg_->msg_rpc->cmd = u_can_disseminate_all;
  //   log(DEBUG, "rpc_sending u_can_disseminate_all to peer_id= " << peer_->ptlmap.id)
  //   return_err_if_ret_cond_flag(rpc_send(ds_->rpc_s, peer_, msg_), err, <, 0, err, free(msg_);)
  // }
  // free(client_peer_head__);
  // free(server_peer_head__);
  
  // sleep(1);
  
  return_if_err(ds_disseminate_all(ds_), err)
  ds_->f_reg = 1;
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

/**************************************  ds_Slave rel *********************************************/
struct dart_server* global_ds_ = 0;
void* handle_rdma_cm_event(void* event_)
{
  struct dart_server* ds_ = global_ds_;
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  
  int err;
  struct node_id* peer_;
  struct connection* conn_;
  
  struct rdma_cm_event* _event_ = (struct rdma_cm_event*)event_;
  struct rdma_cm_event event;
  memcpy(&event, _event_, sizeof(*_event_) );
  rdma_ack_cm_event(_event_);
  if (event.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
    struct con_param conpara = *((struct con_param*)event.param.conn.private_data);
    log(DEBUG, "conpara.pm_cp= \n" << rpc_to_str("ptlid_map", &(conpara.pm_cp) ) << "\n"
               "\t conpara.pm_sp= \n" << rpc_to_str("ptlid_map", &(conpara.pm_sp) ) )
    
    return_err_if_ret_cond_flag(rpc_get_peer(ds_->rpc_s, conpara.pm_cp.id), peer_, ==, NULL, NULL)
    log(DEBUG, "recved RDMA_CM_EVENT_CONNECT_REQUEST from peer_id= " << peer_->ptlmap.id)
    
    if (conpara.type == 0)
      conn_ = &(peer_->sys_conn);
    else
      conn_ = &(peer_->rpc_conn);
    
    return_err_if_ret_cond_flag(build_context(event.id->verbs, conn_), err, !=, 0, NULL)
    return_err_if_ret_cond_flag(build_qp_attr(&(conn_->qp_attr), conn_, ds_->rpc_s), err, !=, 0, NULL)
    return_err_if_ret_cond_flag(rdma_create_qp(event.id, conn_->pd, &(conn_->qp_attr) ), err, !=, 0, NULL)
    
    event.id->context = conn_;
    conn_->id = event.id;
    conn_->qp = event.id->qp;
    if (conpara.type == 0) {
      return_err_if_ret_cond_flag(sys_post_recv(ds_->rpc_s, peer_), err, !=, 0, NULL)
    }
    else {
      try_n_times__return_err_if_ret_cond_flag(rpc_post_recv(ds_->rpc_s, peer_), err, !=, 0, NULL, 3)
    }
    
    struct rdma_conn_param cm_params;
    memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
    cm_params.private_data = &(peer_->ptlmap.id);
    cm_params.private_data_len = sizeof(int);
    cm_params.initiator_depth = cm_params.responder_resources = 1;
    cm_params.retry_count = 7;
    cm_params.rnr_retry_count = 7;  //infinite retry
    
    return_err_if_ret_cond_flag(rdma_accept(event.id, &cm_params), err, !=, 0, NULL)
    conn_->f_connected = 1;
  }
  else if(event.event == RDMA_CM_EVENT_ESTABLISHED) {
    log(DEBUG, "recved RDMA_CM_EVENT_ESTABLISHED.")
  }
  else if(event.event == RDMA_CM_EVENT_DISCONNECTED) {
    log(DEBUG, "recved RDMA_CM_EVENT_DISCONNECTED.")
  }
  else {
    log(ERROR, "unknown event= " << rpc_event_to_str(&event) )
  }
  // rdma_ack_cm_event(_event_);
  free(_event_);
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return NULL;
}

void* ds_listen(void* server_)
{
  struct dart_server* ds_ = (struct dart_server*) server_;
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  
  int err;
  struct node_id* peer_;
  struct connection* conn_;
  while (ds_->rpc_s->thread_alive) {
    struct rdma_cm_event* event_;
    // return_err_if_ret_cond_flag((struct rdma_cm_event*)malloc(sizeof(struct rdma_cm_event) ), event_, ==, 0, NULL);
    return_err_if_ret_cond_flag(rdma_get_cm_event(ds_->rpc_s->rpc_ec, &event_), err, !=, 0, NULL)
    struct rdma_cm_event event_copy;
    memcpy(&event_copy, event_, sizeof(*event_) );
    // rdma_ack_cm_event(event_);
    
    // if (global_ds_ == 0)
    //   global_ds_ = ds_;
    // pthread_t event_handler_t;
    // return_err_if_ret_cond_flag(pthread_create(&event_handler_t, NULL, handle_rdma_cm_event, (void*)event_), err, !=, 0, NULL)
    
    if (event_copy.event == RDMA_CM_EVENT_CONNECT_REQUEST) {
      struct con_param conpara = *((struct con_param*)event_copy.param.conn.private_data);
      // log(DEBUG, "conpara.pm_cp= \n" << rpc_to_str("ptlid_map", &(conpara.pm_cp) ) << "\n"
      //           "\t conpara.pm_sp= \n" << rpc_to_str("ptlid_map", &(conpara.pm_sp) ) )
      
      peer_ = rpc_get_peer(ds_->rpc_s, conpara.pm_cp.id);
      log(DEBUG, "recved RDMA_CM_EVENT_CONNECT_REQUEST from peer_id= " << peer_->ptlmap.id)
      
      if (conpara.type == 0)
        conn_ = &(peer_->sys_conn);
      else
        conn_ = &(peer_->rpc_conn);
      
      return_err_if_ret_cond_flag(build_context(event_copy.id->verbs, conn_), err, !=, 0, NULL)
      return_err_if_ret_cond_flag(build_qp_attr(&(conn_->qp_attr), conn_, ds_->rpc_s), err, !=, 0, NULL)
      return_err_if_ret_cond_flag(rdma_create_qp(event_copy.id, conn_->pd, &(conn_->qp_attr) ), err, !=, 0, NULL)
      
      event_copy.id->context = conn_;
      conn_->id = event_copy.id;
      conn_->qp = event_copy.id->qp;
      if (conpara.type == 0) {
        return_err_if_ret_cond_flag(sys_post_recv(ds_->rpc_s, peer_), err, !=, 0, NULL)
      }
      else {
        return_err_if_ret_cond_flag(rpc_post_recv(ds_->rpc_s, peer_), err, !=, 0, NULL)
      }
      
      struct rdma_conn_param cm_params;
      memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
      cm_params.private_data = &(peer_->ptlmap.id);
      cm_params.private_data_len = sizeof(int);
      cm_params.initiator_depth = cm_params.responder_resources = 1;
      cm_params.retry_count = 7;
      cm_params.rnr_retry_count = 7;  //infinite retry
      
      return_err_if_ret_cond_flag(rdma_accept(event_copy.id, &cm_params), err, !=, 0, NULL)
      conn_->f_connected = 1;
    }
    else if(event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
      log(DEBUG, "recved RDMA_CM_EVENT_ESTABLISHED.")
    }
    else if(event_copy.event == RDMA_CM_EVENT_DISCONNECTED) {
      log(DEBUG, "recved RDMA_CM_EVENT_DISCONNECTED.")
    }
    else {
      log(ERROR, "unknown event= " << rpc_event_to_str(&event_copy) )
      return NULL;
    }
    rdma_ack_cm_event(event_);
  }
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  pthread_exit(0);
  return 0;
}

// Called back by trouble__process_event upon recving from DS_Master
// Called once received all-the-peers-info
static int announce_cp_completion(struct rpc_server* rpc_s_, struct msg_buf* msg_)
{
  int err;
  struct dart_server* ds_ = ds_ref_from_rpc(rpc_s_);
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  
  struct ptlid_map* pm_ = (struct ptlid_map*)msg_->msg_data;
  for (int i = 0; i < ds_->peer_size; i++, pm_++) {
    if (is_peer_in(pm_->id) )
      continue;
    struct node_id* peer_;
    return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), peer_, ==, 0, -ENOMEM)
    memset(peer_, 0, sizeof(*peer_) );
    peer_->ptlmap = *pm_;
    INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
    peer_->num_msg_at_peer = rpc_s_->max_num_msg;
    peer_->num_msg_ret = 0;
    log(DEBUG, "rpc_s_id= " << ds_->rpc_s->ptlmap.id << ", adding peer_= \n" << rpc_to_str("node_id", peer_) )
    add_to_id_peer_map(peer_->ptlmap.id, peer_);
    // if (peer_->ptlmap.type == PEER_CLIENT && rpc_s_->ptlmap.id == peer_->ptlmap.id % ds_->size_sp) {
    //   log(DEBUG, "rpc_connecting peer_id= " << peer_->ptlmap.id)
    //   try_n_times__return_if_err(rpc_connect(rpc_s_, peer_), err, 3)
    // }
  }
  
  log(DEBUG, "rpc_s_id= " << ds_->rpc_s->ptlmap.id << ", final id_peer_map= \n" << rpc_to_str("id_peer_map", NULL) )
  
  ds_->f_reg = 1;
  free(msg_->msg_data);
  free(msg_);
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

// Called back by trouble__process_event upon recving disseminate from DS_Master
// Called for only slave servers upon recving all-info from master server
int dsrpc_announce_cp(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  struct dart_server* ds_ = ds_ref_from_rpc(rpc_s_);
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id << ", from peer_id= " << cmd_->id)
  int err;
  struct node_id* peer_;
  peer_ = rpc_get_peer(rpc_s_, 0); // set to DS_Master
  
  struct msg_buf* msg_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 0), msg_, ==, 0, -ENOMEM)
  msg_->size = sizeof(struct ptlid_map)*(ds_->peer_size);
  return_err_if_ret_cond_flag(malloc(msg_->size), msg_->msg_data, ==, 0, -ENOMEM, free(msg_);)
  msg_->cb = announce_cp_completion;
  msg_->id = cmd_->wr_id;
  msg_->mr = cmd_->mr;
  return_if_err(rpc_receive_direct(rpc_s_, peer_, msg_), err, free(msg_);)
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

/*
  rpc_read_config: resolve the rdma_id based on Master DS's IP+Port
  rpc_connect: connect and send my IP+Port/ID/APPID information to DS_Master
  rpc_process_event_with_timeout will callback dsrpc_announce_cp: Get feedback information from DS_Master: Feedback info contains (all the collected information including DS+DC)
  
  RPC channel connection to all the other peers
  SYS channel connection to all the peers (including ds_master node?)
*/
int handle_u_can_disseminate_all(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  ((struct dart_server*)rpc_s_->dart_ref)->ready_to_disseminate_all = 1;
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  return 0;
}

int handle_ready_for_disseminate_all(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  struct dart_server* ds_ = (struct dart_server*)rpc_s_->dart_ref;
  
  int err;
  struct node_id* peer_ = rpc_get_peer(rpc_s_, cmd_->id);
  struct msg_buf* msg_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_, ==, 0, -ENOMEM)
  msg_->cb = default_completion_with_data_callback;
  msg_->size = (ds_->size_sp + ds_->size_cp)*sizeof(struct ptlid_map);
  return_err_if_ret_cond_flag((struct ptlid_map*)malloc(msg_->size), msg_->msg_data, ==, 0, -ENOMEM, free(msg_);)
  struct ptlid_map* pptlmap_ = (struct ptlid_map*)msg_->msg_data;
  struct node_id **client_peer_head__, **server_peer_head__;
  return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  for (struct node_id** t2_server_peer_head__ = server_peer_head__; *t2_server_peer_head__ != NULL; t2_server_peer_head__++, pptlmap_++)
    *pptlmap_ = (*t2_server_peer_head__)->ptlmap;
  for (struct node_id** t2_client_peer_head__ = client_peer_head__; *t2_client_peer_head__ != NULL; t2_client_peer_head__++, pptlmap_++)
    *pptlmap_ = (*t2_client_peer_head__)->ptlmap;
  
  msg_->msg_rpc->cmd = sp_announce_cp_all;
  msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
  struct hdr_register* hreg_ = (struct hdr_register*)msg_->msg_rpc->pad;
  hreg_->pm_cp = ds_->rpc_s->ptlmap;
  hreg_->num_cp = ds_->size_cp;
  hreg_->num_sp = ds_->size_sp;
  log(DEBUG, "rpc_sending sp_announce_cp_all to peer_id= " << peer_->ptlmap.id)
  return_err_if_ret_cond_flag(rpc_send(rpc_s_, peer_, msg_), err, <, 0, err, free(msg_->msg_data); free(msg_);)
  return_if_err(rpc_process_event(rpc_s_), err, free(msg_);)
  free(client_peer_head__);
  free(server_peer_head__);
  
  log(DEBUG, "done; rpc_s_id= " << rpc_s_->ptlmap.id << ", from peer_id= " << cmd_->id)
  return 0;
}

int ds_boot_slave(struct dart_server* ds_)
{
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  int err;
  
  struct node_id* master_peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), master_peer_, ==, 0, NULL)
  memset(master_peer_, 0, sizeof(*master_peer_) );
  INIT_LIST_HEAD(patch_ib::to_str<>(master_peer_->ptlmap.id) + "_req_list", &(master_peer_->req_list) );
  master_peer_->num_msg_at_peer = ds_->rpc_s->max_num_msg;
  master_peer_->num_msg_ret = 0;
  master_peer_->ptlmap.type = PEER_SERVER;
  
  return_err_if_ret_cond_flag(rpc_read_config(&(master_peer_->ptlmap.address) ), err, <, 0, err)
  if (master_peer_->ptlmap.address.sin_addr.s_addr == ds_->rpc_s->ptlmap.address.sin_addr.s_addr && master_peer_->ptlmap.address.sin_port == ds_->rpc_s->ptlmap.address.sin_port) {
    // This is the master server! the config file may be around from a previous run
    log(WARNING, "config file exists, but I am the Master server!")
    free(master_peer_);
    return_if_err(ds_boot_master(ds_), err)
    return 0;
  }
  // Connect to master server, build rpc channel
  try_n_times__return_if_err(rpc_connect(ds_->rpc_s, master_peer_), err, 3) // will set master_peer_->ptlmap.id
  log(DEBUG, "adding master_peer_= \n" << rpc_to_str("node_id", master_peer_) )
  add_to_id_peer_map(master_peer_->ptlmap.id, master_peer_);
  
  struct node_id* this_peer_;
  return_err_if_ret_cond_flag((struct node_id*)malloc(sizeof(struct node_id) ), this_peer_, ==, 0, NULL)
  memset(this_peer_, 0, sizeof(*this_peer_) );
  INIT_LIST_HEAD(patch_ib::to_str<>(this_peer_->ptlmap.id) + "_req_list", &(this_peer_->req_list) );
  this_peer_->num_msg_at_peer = ds_->rpc_s->max_num_msg;
  this_peer_->num_msg_ret = 0;
  this_peer_->ptlmap = ds_->rpc_s->ptlmap;
  log(DEBUG, "adding this_peer_= \n" << rpc_to_str("node_id", this_peer_) )
  add_to_id_peer_map(this_peer_->ptlmap.id, this_peer_);
  ds_->self = this_peer_;
  
  // For incoming conns
  return_if_err(pthread_create(&(ds_->rpc_s->comm_thread), NULL, ds_listen, (void*)ds_), err)
  ds_->rpc_s->thread_alive = 1;
  
  // Waiting for dissemination msg from master server;
  log(DEBUG, "WAITING for dissemination msg from master server... \n"
             "\t rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  while (ds_->f_reg == 0) {
    err = rpc_process_event_with_timeout(ds_->rpc_s, 1);
    if (err != 0 && err != -ETIME) {
      log(ERROR, "rpc_process_event_with_timeout failed!")
      return err;
    }
  }
  
  // Connect to all of the other servers
  struct node_id **client_peer_head__, **server_peer_head__;
  return_if_err(get_peer_head__(1, &client_peer_head__, &server_peer_head__), err)
  for (struct node_id** t_server_peer_head__ = server_peer_head__; *t_server_peer_head__ != NULL; t_server_peer_head__++) {
    struct node_id* peer_ = *t_server_peer_head__;
    if (peer_->rpc_conn.f_connected == 1)
      continue;
    
    log(DEBUG, "rpc_connecting to server peer_id= " << peer_->ptlmap.id)
    try_n_times__return_if_err(rpc_connect(ds_->rpc_s, peer_), err, 3)
  }
  // free(client_peer_head__);
  // free(server_peer_head__);
  
  // Connect to some of the other servers -- as in old ib code
  // for (int peer_id = 1; peer_id < ds_->rpc_s->ptlmap.id; peer_id++) {
  //   struct node_id* peer_ = rpc_get_peer(ds_->rpc_s, peer_id);
  //   log(DEBUG, "rpc_connecting to server peer_id= " << peer_->ptlmap.id)
  //   try_n_times__return_if_err(rpc_connect(ds_->rpc_s, peer_), err, 3)
  // }
  
  // struct msg_buf* msg_;
  // return_err_if_ret_cond_flag(msg_buf_alloc(ds_->rpc_s, master_peer_, 1), msg_, ==, 0, -ENOMEM)
  // msg_->msg_rpc->id = ds_->rpc_s->ptlmap.id;
  // msg_->msg_rpc->cmd = slave_done_interconnecting;
  // log(DEBUG, "rpc_sending slave_done_interconnecting to master_peer_id= " << master_peer_->ptlmap.id)
  // return_err_if_ret_cond_flag(rpc_send(ds_->rpc_s, master_peer_, msg_), err, <, 0, err, free(msg_);)
  
  // log(DEBUG, "WAITING for u_can_disseminate_all... \n"
  //           "\t rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  // while (ds_->ready_to_disseminate_all != 1) {
  //   err = rpc_process_event_with_timeout(ds_->rpc_s, 1);
  //   if (err != 0 && err != -ETIME) {
  //     log(ERROR, "rpc_process_event_with_timeout failed!")
  //     return err;
  //   }
  // }
  // return_if_err(ds_disseminate_all(ds_), err)
  
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

int file_lock(int fd, int op)
{
  if (op) {
    try {
      while (lockf(fd, F_TLOCK, (off_t)1) != 0)
      {}
    }
    catch (const std::exception& e) {
      log(ERROR, "Exception e= " << e.what() )
      return -1;
    }
    return 0;
  }
  else {
    int r;
    try {
      r = lockf(fd, F_ULOCK, (off_t)1);
    }
    catch (const std::exception& e) {
      log(ERROR, "Exception e= " << e.what() )
      return -1;
    }
    return r;
  }
}

// Function to automatically decide if this instance should run as 'master' or 'slave'.
int ds_boot(struct dart_server* ds_)
{
  log(DEBUG, "started; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  const char f_lock_[] = "srv.lck";
  const char f_conf_[] = "conf";
  int err, fd;
  return_err_if_ret_cond_flag(open(f_lock_, O_WRONLY | O_CREAT, 0644), fd, <, 0, fd)
  
  // Note: File locking does not work on the login nodes
  return_err_if_ret_cond_flag(file_lock(fd, 1), err, <, 0, err, close(fd); remove(f_lock_);)
  
  if (access(f_conf_, F_OK) != 0 || ds_->size_sp == 1) { // Config file is empty
    log(DEBUG, "will run as MASTER; ds_->size_sp= " << ds_->size_sp)
    return_if_err(rpc_write_config(ds_->rpc_s), err, file_lock(fd, 0); close(fd); remove(f_lock_);)
    return_err_if_ret_cond_flag(file_lock(fd, 0), err, <, 0, err, close(fd); remove(f_lock_);)
    return_if_err(ds_boot_master(ds_), err, file_lock(fd, 0); close(fd); remove(f_lock_);)
  }
  else {
    log(DEBUG, "will run as SLAVE; ds_->size_sp= " << ds_->size_sp)
    return_err_if_ret_cond_flag(file_lock(fd, 0), err, <, 0, err, close(fd); remove(f_lock_);)
    return_if_err(ds_boot_slave(ds_), err, file_lock(fd, 0); close(fd); remove(f_lock_);)
  }
  
  ds_->rpc_s->cur_num_peer = ds_->rpc_s->num_rpc_per_buff;
  close(fd);
  remove(f_lock_);
  log(DEBUG, "done; rpc_s_id= " << ds_->rpc_s->ptlmap.id)
  return 0;
}

/*****************************************  Public API  *******************************************/
// Allocate and initialize dart server; the server initializes rpc server. 
struct dart_server* ds_alloc(int num_sp, int num_cp, void* dart_ref_, void *comm)
{
  log(DEBUG, "started; num_sp= " << num_sp << ", num_cp= " << num_cp)
  int err;
  struct dart_server* ds_;
  return_err_if_ret_cond_flag((struct dart_server*)malloc(sizeof(struct dart_server) ), ds_, ==, 0, NULL)
  memset(ds_, 0, sizeof(*ds_) );
  
  ds_->dart_ref = dart_ref_;
  ds_->size_cp = num_cp;
  ds_->size_sp = num_sp;
  ds_->peer_size = ds_->size_sp + ds_->size_cp;
  ds_->num_sp = num_sp;
  ds_->num_cp = num_cp;
  cp_rank_cnt = num_sp;
  
  return_err_if_ret_cond_flag(rpc_server_init(NULL, 0, INFINIBAND_MSG_QUEUE_SIZE, ds_->peer_size, ds_, DART_SERVER), ds_->rpc_s, ==, 0, NULL, free(ds_);)
  ds_->rpc_s->num_peers = ds_->peer_size;
  ds_->rpc_s->num_rpc_per_buff = ds_->peer_size;
  ds_->rpc_s->app_num_peers = num_sp;
  ds_->rpc_s->cur_num_peer = 2;
  
  // rpc_add_service(cn_register, dsrpc_cn_register);
  rpc_add_service(cn_unregister, dsrpc_cn_unregister);
  rpc_add_service(cn_s_unregister, dsrpc_unregister);
  // rpc_add_service(sp_reg_request, dsrpc_sp_register);
  // rpc_add_service(peer_reg_address, dsrpc_peer_fetch);
  // rpc_add_service(sp_reg_reply, dsrpc_sp_ack_register);
  rpc_add_service(sp_announce_cp, dsrpc_announce_cp);
  
  rpc_add_service(slave_done_interconnecting, handle_slave_done_interconnecting);
  rpc_add_service(u_can_disseminate_all, handle_u_can_disseminate_all);
  rpc_add_service(ready_for_disseminate_all, handle_ready_for_disseminate_all);
  
  ds_->rpc_s->ptlmap.type = PEER_SERVER;
  return_err_if_ret_cond_flag(ds_boot(ds_), err, !=, 0, NULL, free(ds_);)
  ds_->num_charge = (ds_->num_cp / ds_->num_sp) + (ds_->rpc_s->ptlmap.id < (ds_->num_cp % ds_->num_sp) ); // TODO
  
  log(DEBUG, "done; ds_= \n" << ds_to_str("dart_server", ds_) )
  return ds_;
}

int ds_free(struct dart_server* ds_)
{
  int err;
  if (ds_->rpc_s->thread_alive) {
    ds_->rpc_s->thread_alive = 0;
    return_if_err(pthread_cancel(ds_->rpc_s->comm_thread), err)
  }
  
  return_if_err(rpc_server_free(ds_->rpc_s), err)
  for (std::map<int, struct app_info*>::iterator it = app_id__app_map.begin(); it != app_id__app_map.end(); it++)
    free(it->second);
  free(ds_);
  
  return 0;
}

// Called by the upper layer for sending/recving msg, data
int ds_process(struct dart_server* ds_) { return rpc_process_event(ds_->rpc_s); }
