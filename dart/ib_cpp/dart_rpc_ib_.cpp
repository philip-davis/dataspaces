/* 
 * RPC service implementation on infiniband. 
 *
 * Tong Jin (2011) TASSL Rutgers University
 * Hoang Bui (2012-2013) TASSL Rutgers University, hbui@cac.rutgers.edu
 * Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */
#include <string>
#include <sstream>
#include <pthread.h>
#include <algorithm> // random_shuffle

#include "dart_rpc_ib.h"
#include "patch_ib_cpp.h"

const int RDMA_BUFFER_SIZE = 1024*1024*1024; // 10*1024*1024;
// For large data chunking
// patch_ib::thread_safe_map<unsigned int, void*> hash__data_map;
patch_ib::map<unsigned int, void*> hash__data_map;
// patch_ib::thread_safe_map<unsigned int, uint64_t> hash__left_to_recv_size_map;
patch_ib::map<unsigned int, uint64_t> hash__left_to_recv_size_map;
typedef std::pair<struct node_id*, struct msg_buf*> peer_msg_pair;
// patch_ib::thread_safe_map<unsigned int, peer_msg_pair> hash__peer_msg_map;
patch_ib::map<unsigned int, peer_msg_pair> hash__peer_msg_map;
patch_ib::syncer<unsigned int> ib_syncer;

// #define SYS_WAIT_COMPLETION(x)          \
//   while (!(x) ) {            \
//     err = sys_process_event(rpc_s);      \
//     if (err < 0)          \
//       goto err_out;        \
//   }

const int BUFFER_SIZE = 1024;  // TODO: rpc_cmd size

#define myid(rpc_s)    (rpc_s->ptlmap.id)
#define rank2id(rpc_s, rank)  ((rank) + (rpc_s->app_minid) )
#define id2rank(rpc_s, id)  ((id) - (rpc_s->app_minid) )
#define myrank(rpc_s)    id2rank(rpc_s, myid(rpc_s) )

#define PORTMAX 65536
#define ERRORIP "cannot find host ip"

#define MD_USE_INC(rpc_s)  rpc_s->num_md_posted++
#define MD_USE_DEC(rpc_s)  rpc_s->num_md_unlinked++

// For bugless peer_id to peer_ mapping
// patch_ib::thread_safe_map<short int, struct node_id*> id_peer_map;
patch_ib::map<short int, struct node_id*> id_peer_map;
int get_peer_type(short int peer_id, char* peer_type_)
{
  if (!is_peer_in(peer_id) ) {
    log(ERROR, "NOT in; peer_id= " << peer_id)
    return 1;
  }
  *peer_type_ = id_peer_map[peer_id]->ptlmap.type;
  return 0;
}

int is_peer_in(short int peer_id)
{
  if (id_peer_map.contains(peer_id) )
    return 1;
  else
    return 0;
}

int add_to_id_peer_map(short int peer_id, struct node_id* peer_)
{
  if (id_peer_map.contains(peer_id) ) {
    log(WARNING, "already in; peer_id= " << peer_id)
    return 0;
  }
  id_peer_map[peer_id] = peer_;
  // log(DEBUG, "added peer_id= " << peer_id << ", id_peer_map=")
  // for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++) {
  //   std::cout << "\t id= " << it->first << ", peer_= " << it->second << "\n";
    // std::cout << "\t\t peer_id= " << (it->second)->ptlmap.id << "\n";
    // std::cout << "\t id= " << it->first << ", peer_id= " << (it->second)->ptlmap.id << "\n";
    // std::cout << "\t id= " << it->first << ", peer_->ptlmap= " << rpc_to_str("ptlid_map", &((it->second)->ptlmap) );
  // }
  return 0;
}

int del_from_id_peer_map(short int peer_id)
{
  if (!id_peer_map.contains(peer_id) ) {
    log(WARNING, "NOT in; peer_id= " << peer_id)
    return 1;
  }
  free(id_peer_map[peer_id] );
  id_peer_map.del(peer_id);
  // log(DEBUG, "deled peer_id= " << peer_id << ", id_peer_map=")
  // for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++)
  //   std::cout << "\t id= " << it->first << ", peer_= " << it->second << "\n";
  
  return 0;
}

// ASK: Can only be used after rpc_server is fully initiated
struct node_id* rpc_get_peer(struct rpc_server* rpc_s_, short int peer_id)
{
  if (!id_peer_map.contains(peer_id) ) {
    log(WARNING, "is NOT in; peer_id= " << peer_id)
    return NULL;
  }
  return id_peer_map[peer_id];
}

//-----------------------------------  struct funcs  ---------------------------------------------//
const int num_to_str_char_ = 5;
const int to_str_length = 1000;
char to_str_char__[num_to_str_char_][to_str_length];
int to_str_index = 0;
char* rpc_to_str(const char* type, const void* struct_)
{
  std::stringstream ss;
  if (cstr_cstr_equals(type, (const char*)"con_param") ) {
    struct con_param* p_ = (struct con_param*)struct_;
    ss << "\t type= " << p_->type << ", num_cp= " << p_->num_cp << "\n"
       << "\t pm_sp= { \n" << rpc_to_str("ptlid_map", &(p_->pm_sp) ) << "\n\t } \n"
       << "\t pm_cp= { \n" << rpc_to_str("ptlid_map", &(p_->pm_cp) ) << "\n\t } \n";
  }
  else if (cstr_cstr_equals(type, (const char*)"hdr_register") ) {
    struct hdr_register* p_ = (struct hdr_register*)struct_;
    ss << "\t num_sp= " << p_->num_sp << ", num_cp= " << p_->num_cp << ", id_min= " << p_->id_min << "\n"
       << "\t pm_sp= { \n" << rpc_to_str("ptlid_map", &(p_->pm_sp) ) << "\n\t } \n"
       << "\t pm_cp= { \n" << rpc_to_str("ptlid_map", &(p_->pm_cp) ) << "\n\t } \n";
  }
  else if (cstr_cstr_equals(type, (const char*)"rpc_cmd") ) {
    struct rpc_cmd* p_ = (struct rpc_cmd*)struct_;
    ss << "\t cmd= " << (int)p_->cmd << "\n"
       << "\t src= " << patch_ib::sockaddr_in_to_str(p_->src) << "\n"
       << "\t dst= " << patch_ib::sockaddr_in_to_str(p_->dst) << "\n"
       << "\t num_msg= " << (int)p_->num_msg << "\n"
       << "\t id= " << (int)p_->id << ", qp_num= " << p_->qp_num << "\n";
  }
  else if (cstr_cstr_equals(type, (const char*)"ptlid_map") ) {
    struct ptlid_map* p_ = (struct ptlid_map*)struct_;
    ss << "\t id= " << p_->id << ", app_id= " << p_->appid << ", type= " << p_->type << "\n"
       << "\t laddr= " << patch_ib::sockaddr_in_to_str(p_->address) << "\n";
  }
  else if (cstr_cstr_equals(type, (const char*)"node_id") ) {
    struct node_id* p_ = (struct node_id*)struct_;
    ss << "\t ptlmap= { \n" << rpc_to_str("ptlid_map", &(p_->ptlmap) ) << "\n\t } \n"
       << "\t f_unreg= " << p_->f_unreg << "\n"
      // << "\t num_req= " << p_->num_req << ", req_list= { \n";
    // list_to_str(&(p_->req_list), struct rpc_request, "rpc_request", req_entry, ss, rpc_to_str)
    // ss << "\n\t } \n"
       << "\t req_posted= " << p_->req_posted << ", f_req_msg= " << p_->f_req_msg << ", f_need_msg= " << p_->f_need_msg << "\n"
       << "\t ch_num= " << p_->ch_num << ", num_msg_at_peer= " << p_->num_msg_at_peer << ", num_msg_ret= " << p_->num_msg_ret << "\n"
       << "\t num_recv_buf= " << p_->num_recv_buf << ", num_sys_recv_buf= " << p_->num_sys_recv_buf << "\n";
  }
  else if (cstr_cstr_equals(type, (const char*)"msg_buf") ) {
    struct msg_buf* p_ = (struct msg_buf*)struct_;
    if (p_->msg_rpc)
      ss << "\t msg_rpc= { \n" << rpc_to_str("rpc_cmd", p_->msg_rpc) << "\n\t } \n";
    ss << "\t size= " << p_->size << "\n";
    // ss << "\t size= " << p_->size << ", msg_data= \n";
    // def_arr_to_str(p_->size, p_->msg_data, char, ss)
    // ss << "\n";
      // << "\t *sync_op_id= " << *(p_->sync_op_id) << "\n"
      // << "\t id= " << p_->id << "\n";
    if (p_->peer)
       ss << "\t peer= { \n" << rpc_to_str("node_id", p_->peer) << "\n\t } \n";
  }
  else if (cstr_cstr_equals(type, (const char*)"rpc_request") ) {
    struct rpc_request* p_ = (struct rpc_request*)struct_;
    ss << "\t req_entry= { \n";
    list_to_str(&(p_->req_entry), struct rpc_request, "rpc_request", req_entry, ss, rpc_to_str)
    ss << "\n\t } \n"
       << "\t msg= { \n" << rpc_to_str("msg_buf", p_->msg) << "\n\t } \n"
       << "\t iodir= " << p_->iodir << "\n"
       << "\t type= " << p_->type << "\n"
       << "\t peerid= " << p_->peerid << "\n"
       << "\t size= " << p_->size << ", data= \n";
    def_arr_to_str(p_->size, p_->data, char, ss)
    ss << "\n"
       << "\t f_vec= " << p_->f_vec << ", rpc_num= " << p_->rpc_num << "\n"
       << "\t current_rpc_count= " << p_->current_rpc_count << "\n";
  }
  else if (cstr_cstr_equals(type, (const char*)"rpc_server") ) {
    struct rpc_server* p_ = (struct rpc_server*)struct_;
    ss << "\t ptlmap= { \n" << rpc_to_str("ptlid_map", &(p_->ptlmap) ) << "\n\t } \n"
       << "\t num_peers= " << p_->num_peers << "\n"
       << "\t app_minid= " << p_->app_minid << ", app_num_peers= " << p_->app_num_peers << ", num_sp= " << p_->num_sp << "\n"
       << "\t num_rpc_per_buff= " << p_->num_rpc_per_buff << ", num_buf= " << p_->num_buf << ", max_num_msg= " << p_->max_num_msg << "\n"
       << "\t com_type= " << p_->com_type << ", cur_num_peer= " << p_->cur_num_peer << "\n"
       << "\t rpc_list= { \n";
    list_to_str(&(p_->rpc_list), struct rpc_request, "rpc_request", req_entry, ss, rpc_to_str)
    ss << "\n\t } \n";
  }
  else if (cstr_cstr_equals(type, (const char*)"id_peer_map") ) {
    for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++)
      ss << "\t id= " << it->first << ", peer_id= " << (it->second)->ptlmap.id << "\n";
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
  memcpy(to_str_char__[to_str_index], str.c_str(), str.length() );
  to_str_char__[to_str_index][str.length() ] = '\0';
  int to_str_index_to_return = to_str_index;
  ++to_str_index;
  to_str_index = (to_str_index > num_to_str_char_ - 1) ? 0 : to_str_index;
  return to_str_char__[to_str_index_to_return];
}

const char* rpc_event_to_str(struct rdma_cm_event* event_)
{
  std::stringstream ss;
  switch (event_->event) {
    case RDMA_CM_EVENT_DISCONNECTED:
      ss << "RDMA_CM_EVENT_DISCONNECTED";
      break;
    case RDMA_CM_EVENT_REJECTED:
      ss << "RDMA_CM_EVENT_REJECTED";
      break;
    case RDMA_CM_EVENT_ADDR_ERROR:
      ss << "RDMA_CM_EVENT_ADDR_ERROR";
      break;
    case RDMA_CM_EVENT_ROUTE_ERROR:
      ss << "RDMA_CM_EVENT_ROUTE_ERROR";
      break;
    case RDMA_CM_EVENT_CONNECT_RESPONSE:
      ss << "RDMA_CM_EVENT_CONNECT_RESPONSE";
      break;
    case RDMA_CM_EVENT_CONNECT_ERROR:
      ss << "RDMA_CM_EVENT_CONNECT_ERROR";
      break;
    case RDMA_CM_EVENT_UNREACHABLE:
      ss << "RDMA_CM_EVENT_UNREACHABLE";
      break;
    case RDMA_CM_EVENT_DEVICE_REMOVAL:
      ss << "RDMA_CM_EVENT_DEVICE_REMOVAL";
      break;
    case RDMA_CM_EVENT_MULTICAST_JOIN:
      ss << "RDMA_CM_EVENT_MULTICAST_JOIN";
      break;
    case RDMA_CM_EVENT_MULTICAST_ERROR:
      ss << "RDMA_CM_EVENT_MULTICAST_ERROR";
      break;
    case RDMA_CM_EVENT_ADDR_CHANGE:
      ss << "RDMA_CM_EVENT_ADDR_CHANGE";
      break;
    case RDMA_CM_EVENT_TIMEWAIT_EXIT:
      ss << "RDMA_CM_EVENT_TIMEWAIT_EXIT";
      break;
    case RDMA_CM_EVENT_CONNECT_REQUEST:
      ss << "RDMA_CM_EVENT_CONNECT_REQUEST";
      break;
    default:
      log(WARNING, "unknown event= " << event_->event)
      break;
  }
  return (ss.str() + " ").c_str();
}

//---------------------------------------  heresy  -----------------------------------------------//
static int num_service = 0;
static struct rpc_server* rpc_s_instance;
static struct {
	enum cmd_type rpc_cmd;
	rpc_service rpc_func;
} rpc_commands[64];

/* =====================================================
	Fabric:
	(1)barrier functions
	(2)rpc operation functions
	(3)message passing & data transfering functions
	(4)network/device/IB system operation
	Public APIs
 =====================================================*/

double gettime()
{
	struct timeval tm;
	gettimeofday(&tm, NULL);
	return ((double) tm.tv_sec * 1000000 + (double) tm.tv_usec) / 1000000.0;
}

/* -------------------------------------------------------------------
  System barrier implementation.
------------------------------------------------------------------- */
int log2_ceil(int n)
{
	unsigned int i;
	int k = -1;

	i = ~(~0U >> 1);
	while(i && !(i & n) )
		i = i >> 1;
	if(i != n)
		i = i << 1;

	while(i) {
		k++;
		i = i >> 1;
	}

	return k;
}

/* ------------------------------------------------------------------
  Network/Device/IB system operation
*/
// char* ip_search()
// {
//   int err;
// 	struct ifaddrs* addr_;
// 	return_err_if_ret_cond_flag(getifaddrs(&addr_), err, !=, 0, NULL);
// 	int count = 0;
// 	for (struct ifaddrs* head_ = addr_; head_ != NULL; head_ = head_->ifa_next) {
//   	if (head_->ifa_name == NULL || strcmp(IB_INTERFACE, head_->ifa_name) == 0)
//   		break;
// 		count++;
// 	}
  
// 	int sfd, intr;
// 	struct ifreq buf[16];
// 	struct ifconf ifc;
// 	sfd = socket(AF_INET, SOCK_DGRAM, 0);
// 	if (sfd < 0)
// 		return (char*)ERRORIP;
// 	ifc.ifc_len = sizeof(buf);
// 	ifc.ifc_buf = (caddr_t) buf;
// 	if (ioctl(sfd, SIOCGIFCONF, (char *) &ifc) )
// 		return (char*)ERRORIP;
// 	intr = ifc.ifc_len / sizeof(struct ifreq);
// 	while(intr-- > 0 && ioctl(sfd, SIOCGIFADDR, (char *) &buf[intr] ) );
// 	close(sfd);
  
// 	return inet_ntoa(((struct sockaddr_in *) (&buf[count-1].ifr_addr) )->sin_addr);
// }

char* ip_search()
{
  int fd;
  struct ifreq ifr;
  // 
  fd = socket(AF_INET, SOCK_DGRAM, 0);
  // Type of address to retrieve - IPv4 IP address
  ifr.ifr_addr.sa_family = AF_INET;
  // Copy the interface name in the ifreq structure
  std::memcpy(ifr.ifr_name, IB_INTERFACE, IFNAMSIZ-1);
  ioctl(fd, SIOCGIFADDR, &ifr);
  close(fd);
  // 
  return inet_ntoa( ( (struct sockaddr_in*)& ifr.ifr_addr )->sin_addr);
}

// Get the auto-assigned port number
int port_search(int socket)
{
	int port;
	int err;
	socklen_t len = sizeof(struct sockaddr);

	struct sockaddr_in address;
	memset(&address, 0, sizeof(struct sockaddr_in) );
	err = getsockname(socket, (struct sockaddr *) &address, &len);
	if(err < 0)
		return err;
	port = ntohs(address.sin_port);
	return port;
}

// Check if the format/number of Port Number is correct. (done)
int port_check(int port)
{
	if(1024 < port && port < PORTMAX)
		return 1;
	else
		return 0;
}

/***************************************  RPC_rel  ************************************************/
int rpc_read_config(struct sockaddr_in* address_)
{
  char* ip_;
  char* port_;
  
  ip_ = getenv("P2TNID");
  port_ = getenv("P2TPID");
  if (ip_ && port_) {
    address_->sin_addr.s_addr = inet_addr(ip_);
    address_->sin_port = htons(atoi(port_) );
    return 0;
  }
  
  FILE* f_;
  try_n_times__return_err_if_ret_cond_flag(fopen("conf", "rt"), f_, ==, 0, -ENOENT, 3)
  
  char tmp_ip_[16];  // TODO: need check if IPv6 works in IB
  int tmp_port;
  char version_[16];
  int err = fscanf(f_, "P2TNID=%s\nP2TPID=%d\n%s\n", tmp_ip_, &tmp_port, version_);
  if (strcmp(version_, VERSION) != 0) {
    log(WARNING, "DataSpaces server(s) and client(s) have mis-matched version; version_= " << version_ <<  ", VERSION= " << VERSION)
  }
  // log(DEBUG, "ip_= " << tmp_ip_ << ", port= " << tmp_port)
  address_->sin_addr.s_addr = inet_addr(tmp_ip_);
  return_err_if_ret_cond_flag(address_->sin_addr.s_addr, err, ==, (in_addr_t)(-1), 1)
  address_->sin_port = htons(tmp_port);
  
  fclose(f_);
  
  return 0;
}

int rpc_write_config(struct rpc_server* rpc_s_)
{
  int err;
  
  FILE* f_;
  return_err_if_ret_cond_flag(fopen("conf", "wt"), f_, ==, 0, -EIO)
  
  const char* lip_ = inet_ntoa(rpc_s_->ptlmap.address.sin_addr);
  int lport = ntohs(rpc_s_->ptlmap.address.sin_port);
  log(INFO, "lip_= " << lip_ << ", lport= " << lport)
  if (fprintf(f_, "P2TNID=%s\nP2TPID=%d\n%s\n", lip_, lport, VERSION) < 0) {
    log(ERROR, "fprintf failed!")
    fclose(f_);
    return -EIO;
  }
  
  fclose(f_);
  return 0;
}

//Not be used in InfiniBand version. Just keep an empty func here.
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
  printf("%s.\n", __func__);
  return 0;
}

// Not used in InfiniBand version. Just keep an empty func here.
int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
  printf("%s.\n", __func__);
  return 0;
}

void rpc_report_md_usage(struct rpc_server *rpc_s)
{
  //printf("'%s()': MD posted %d, MD released %d, MD in use %d.\n", __func__, rpc_s->num_md_posted, rpc_s->num_md_unlinked, rpc_s->num_md_unlinked - rpc_s->num_md_posted);
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
  msg->id = (uintptr_t)cmd->wr_id;
  msg->mr = cmd->mr;

  return;
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
  return;
}

void rpc_cache_msg(struct msg_buf *msg, struct rpc_cmd *cmd)
{
  msg->id = cmd->wr_id;
  msg->mr = cmd->mr;

  return;
}

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)
{
  rpc_commands[num_service].rpc_cmd = rpc_cmd;
  rpc_commands[num_service].rpc_func = rpc_func;
  num_service++;
}

// added in IB version, get remote notification of finished RDMA operation, clean up memory.
int rpc_cb_cleanup(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  struct ibv_wc wc;
  wc.wr_id = cmd_->wr_id;
  
  struct rpc_request* rr_ = (struct rpc_request*)(uintptr_t)wc.wr_id;
  int err;
  if (*(rr_->cb) ) {
    return_if_err((*rr_->cb)(rpc_s_, &wc), err)
  }
  // rdma_destroy_id(rpc_s_->listen_id);
  // rdma_destroy_event_channel(rpc_s_->rpc_ec);
  // free(rpc_s_);
  
  return 0;
}

int handle_large_data_chunk_cmd(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_);
int handle_large_data_cmd(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_);
int finalize_rpc_send(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_);
void* rpc_process_event_loop(void* hash_to_notify_);

struct rpc_server* rpc_server_init(char* ip_, int port, int num_buff, int num_rpc_per_buff, void* dart_ref_, enum rpc_component cmp_type)
{
	if (rpc_s_instance)
		return rpc_s_instance;
	
	int err = -ENOMEM;
	struct rpc_server* rpc_s_;
  return_err_if_ret_cond_flag((struct rpc_server*)malloc(sizeof(struct rpc_server) ), rpc_s_, ==, 0, NULL)
	memset(rpc_s_, 0, sizeof(*rpc_s_) );
  
	rpc_s_->dart_ref = dart_ref_;
	rpc_s_->num_buf = num_buff;
	rpc_s_->num_rpc_per_buff = num_rpc_per_buff;
	rpc_s_->max_num_msg = num_buff;
	rpc_s_->com_type = cmp_type;
	rpc_s_->cur_num_peer = 0;
  
	rpc_s_->listen_id = NULL;
	rpc_s_->rpc_ec = NULL;
  
	rpc_s_->alloc_pd_flag = 0;
	rpc_s_->global_pd = NULL;
	rpc_s_->global_ctx = NULL;
  
  static char* localip_;
	if (ip_ != NULL) {
		localip_ = ip_;
		rpc_s_->ptlmap.address.sin_port = htons(port);
	}
	else {
		localip_ = ip_search();
	}
	return_err_if_ret_cond_flag(inet_addr(localip_), rpc_s_->ptlmap.address.sin_addr.s_addr, ==, -1, NULL)
	rpc_s_->ptlmap.address.sin_family = AF_INET;
	log(DEBUG, "rpc_s_->ptlmap.address= " << patch_ib::sockaddr_in_to_str(rpc_s_->ptlmap.address) )
	// bind id and open listening id for RPC operation.
	return_err_if_ret_cond_flag(rdma_create_event_channel(), rpc_s_->rpc_ec, ==, NULL, 0)
  return_err_if_ret_cond_flag(rdma_create_id(rpc_s_->rpc_ec, &rpc_s_->listen_id, NULL, RDMA_PS_TCP), err, !=, 0, 0)
  return_err_if_ret_cond_flag(rdma_bind_addr(rpc_s_->listen_id, (struct sockaddr*) &(rpc_s_->ptlmap.address) ), err, !=, 0, 0)
  return_err_if_ret_cond_flag(rdma_listen(rpc_s_->listen_id, 65535), err, !=, 0, 0)
  
	rpc_s_->ptlmap.address.sin_port = rdma_get_src_port(rpc_s_->listen_id);
	log(INFO, "rcp_s_ started listening on addr= " << patch_ib::sockaddr_in_to_str(rpc_s_->ptlmap.address) )
  
  // TODO: sys part if needed
  // other dart init operation
	INIT_LIST_HEAD("rpc_list", &(rpc_s_->rpc_list) );
  
  // TODO: for server and client, num_rpc_per_buff should be different: S, s+c; C c; NEED CHECK
  return_err_if_ret_cond_flag((int*)malloc(num_rpc_per_buff*sizeof(*(rpc_s_->bar_tab) ) ), rpc_s_->bar_tab, ==, 0, 0)
	memset(rpc_s_->bar_tab, 0, num_rpc_per_buff*sizeof(*(rpc_s_->bar_tab) ) );
  
	rpc_add_service(peer_rdma_done, rpc_cb_cleanup);
	rpc_add_service(large_data_cmd, handle_large_data_cmd);
	rpc_add_service(large_data_chunk_cmd, handle_large_data_chunk_cmd);
	rpc_add_service(finished_large_data_recv_cmd, finalize_rpc_send);
	// Init succeeded, set the instance reference here.
	rpc_s_instance = rpc_s_;
	return rpc_s_;
}

int get_peer_head__(int shuffle, struct node_id*** client_peer_head___, struct node_id*** server_peer_head___)
{
  struct node_id** &client_peer_head__ = *client_peer_head___;
  struct node_id** &server_peer_head__ = *server_peer_head___;
  // TODO: computationally bad but in terms of design flexibility, good for now
  std::vector<struct node_id*> peer_v;
  int num_client = 0;
  int num_server = 0;
  for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++) {
    if ((it->second)->ptlmap.type == PEER_CLIENT)
      ++num_client;
    else if ((it->second)->ptlmap.type == PEER_SERVER)
      ++num_server;
    else {
      log(ERROR, "unexpected node type= " << (it->second)->ptlmap.type)
      return 1;
    }
    peer_v.push_back(it->second);
  }
  
  // log(DEBUG, "num_client= " << num_client << ", num_server= " << num_server
  //           << "\t client_peer_head__= " << client_peer_head__ << ", server_peer_head__= " << server_peer_head__ << ", NULL= " << NULL)
  
  struct node_id** t_client_peer_head__;
  if (num_client == 0)
    client_peer_head__ = NULL;
  else {
    return_err_if_ret_cond_flag((struct node_id**)malloc((num_client + 1)*sizeof(struct node_id*) ), client_peer_head__, ==, 0, -ENOMEM)
    t_client_peer_head__ = client_peer_head__;
  }
  struct node_id** t_server_peer_head__;
  if (num_server == 0)
    server_peer_head__ = NULL;
  else {
    return_err_if_ret_cond_flag((struct node_id**)malloc((num_server + 1)*sizeof(struct node_id*) ), server_peer_head__, ==, 0, -ENOMEM)
    t_server_peer_head__ = server_peer_head__;
  }
  
  if (client_peer_head__ != NULL || server_peer_head__ != NULL) {
    // if (shuffle)
    //   std::random_shuffle(peer_v.begin(), peer_v.end() );
    for (std::vector<struct node_id*>::iterator it = peer_v.begin(); it != peer_v.end(); it++) {
      if (client_peer_head__ != NULL && (*it)->ptlmap.type == PEER_CLIENT) {
        // log(DEBUG, "adding client_peer_id= " << (*it)->ptlmap.id)
        *t_client_peer_head__ = *it;
        ++t_client_peer_head__;
      }
      else if (server_peer_head__ != NULL && (*it)->ptlmap.type == PEER_SERVER) {
        // log(DEBUG, "adding server_peer_id= " << (*it)->ptlmap.id)
        *t_server_peer_head__ = *it;
        ++t_server_peer_head__;
      }
    }
  }
  *t_client_peer_head__ = NULL;
  *t_server_peer_head__ = NULL;
  
  // log(DEBUG, "server peers=")
  // for (struct node_id** t2_server_peer_head__ = server_peer_head__; *t2_server_peer_head__ != NULL; t2_server_peer_head__++)
  //   std::cout << "\t peer_id=" << (*t2_server_peer_head__)->ptlmap.id << "\n";
  // std::cout << "client peers= \n";
  // for (struct node_id** t2_client_peer_head__ = client_peer_head__; *t2_client_peer_head__ != NULL; t2_client_peer_head__++)
  //   std::cout << "\t peer_id=" << (*t2_client_peer_head__)->ptlmap.id << "\n";
  // std::cout << "\n";
  
  return 0;
}

int _ibv_reg_mr(struct ibv_mr*& data_mr_, struct ibv_pd* pd_, void* data_, size_t data_size, int acc)
{
  int counter = 0;
  start: ++counter;
  // LOCK_FUNC_M("ibv_reg_mr")
  data_mr_ = ibv_reg_mr(pd_, data_, data_size, acc);
  // UNLOCK_FUNC_M("ibv_reg_mr")
  if (data_mr_ == NULL) {
    if (errno == ENOMEM) {
      log(ERROR, "ibv_reg_mr failed; not enough resources; pthread_self= " << pthread_self)
      if (counter < 3) {
        log(WARNING, "trying ibv_reg_mr again...; pthread_self= " << pthread_self)
        goto start;
      }
    }
    else if (errno == EINVAL) {
      log(ERROR, "ibv_reg_mr failed; invalid access value; pthread_self= " << pthread_self)
    }
    
    return errno;
  }
  return 0;
}

int _ibv_dereg_mr(struct ibv_mr*& data_mr_)
{
  int counter = 0;
  start: ++counter;
  // LOCK_FUNC_M("ibv_dereg_mr")
  int r = ibv_dereg_mr(data_mr_);
  // UNLOCK_FUNC_M("ibv_dereg_mr")
  if (r) {
    log(ERROR, "ibv_dereg_mr failed!")
    if (counter < 3) {
      log(WARNING, "trying ibv_dereg_mr again...")
      goto start;
    }
    
    return errno;
  }
  
  return 0;
}

int rpc_prepare_buffers(struct rpc_server* rpc_s_, const struct node_id* peer_,
                               struct rpc_request* rr_, enum io_dir dir)
{
  int err;
  // Note: rr_->msg->msg_data may be empty while sending only cmd
  if (rr_->msg->msg_data && rr_->type == 0) { // rr_->data = msg->msg_rpc
    int acc = IBV_ACCESS_LOCAL_WRITE | ((dir == io_send) ? IBV_ACCESS_REMOTE_READ : IBV_ACCESS_REMOTE_WRITE);
    rr_->msg->msg_rpc->wr_id = (uintptr_t)rr_;
    return_if_err(_ibv_reg_mr(rr_->data_mr, peer_->rpc_conn.pd, rr_->msg->msg_data, rr_->msg->size, acc), err)
    // Note: cmd->mr will be used to read or write on the cmd-recving side
    rr_->msg->msg_rpc->mr = *(rr_->data_mr);
    rr_->msg->refcont++;
  }
  
  return 0;
}

int rpc_post_request(struct rpc_server* rpc_s_, const struct node_id* peer_, struct rpc_request* rr_)
{
  int err;
  // sending pure msg TODO 2nd parameter, 5th parameter, check manual
  struct ibv_send_wr wr, *bad_wr_ = NULL;
  struct ibv_sge sge;
  memset(&wr, 0, sizeof(wr) );
  
  if (rr_->type == 0) { // cmd
    return_if_err(_ibv_reg_mr(rr_->rpc_mr, peer_->rpc_conn.pd, rr_->data, rr_->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ), err)
    wr.opcode = IBV_WR_SEND;
    sge.lkey = rr_->rpc_mr->lkey;
  }
  else if (rr_->type == 1) { // data
    return_if_err(_ibv_reg_mr(rr_->data_mr, peer_->rpc_conn.pd, rr_->data, rr_->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ), err)
    wr.opcode = IBV_WR_RDMA_WRITE;
    wr.wr.rdma.remote_addr = (uintptr_t)rr_->msg->mr.addr;
    wr.wr.rdma.rkey = rr_->msg->mr.rkey;
    sge.lkey = rr_->data_mr->lkey;
  }
  
  wr.wr_id = (uintptr_t)rr_;  //use address of this rr as the unique wr_id
  wr.sg_list = &sge;
  wr.num_sge = 1;
  wr.send_flags = IBV_SEND_SIGNALED | IBV_SEND_FENCE; // Note: to make sure wr's are proced in the order they are submitted
  
  sge.addr = (uintptr_t)rr_->data;
  sge.length = rr_->size;
  
  return_err_if_ret_cond_flag(ibv_post_send(peer_->rpc_conn.qp, &wr, &bad_wr_), err, <, 0, err)
  if (rr_ && rr_->msg)
    rr_->msg->refcont++;
  
  return 0;
}

// Decode and service a command message received in the rpc buffer. 
// This routine is called by 'rpc_process_event()' in response to new incomming rpc request.
int rpc_cb_decode(struct rpc_server* rpc_s_, struct ibv_wc* wc_)
{
  struct rpc_cmd* cmd_ = (struct rpc_cmd*)(uintptr_t)wc_->wr_id;
  // log(INFO, "cmd_= \n" << rpc_to_str("rpc_cmd", cmd_) )
  int i;
  for (i = 0; i < num_service; i++) {
    // log(INFO, "rpc_commands[" << i << "].rpc_cmd= " << rpc_commands[i].rpc_cmd << "\n\t cmd_->cmd= " << int(cmd_->cmd) );
    if (cmd_->cmd == rpc_commands[i].rpc_cmd) {
      if (rpc_commands[i].rpc_func(rpc_s_, cmd_) ) {
        log(ERROR, "rpc_func failed; rcp_server_id= " << rpc_s_->ptlmap.id << " from peer_id= " << cmd_->id << " command= " << cmd_->cmd)
      }
      break;
    }
  }
  if (i == num_service) {
    log(ERROR, "unknown command= " << cmd_->cmd)
    return -EINVAL;
  }
  
  return 0;
}

// Allocate an RPC structure for communication buffers.
static struct rpc_request* rr_comm_alloc(int num_rpc)
{
  struct rpc_request* rr_;
  size_t size;

  size = sizeof(*rr_) + sizeof(*(rr_->msg) ) + num_rpc*sizeof(struct rpc_cmd) + 7;
  return_err_if_ret_cond_flag((struct rpc_request*)malloc(size), rr_, ==, 0, rr_)
  
  memset(rr_, 0, size);
  rr_->cb = (async_callback)rpc_cb_decode;
  
  rr_->msg = (struct msg_buf*)(rr_ + 1);
  rr_->msg->msg_data = rr_->msg + 1;
  rr_->msg->msg_rpc = (struct rpc_cmd*)(rr_->msg) + 1;
  rr_->msg->size = num_rpc*sizeof(struct rpc_cmd);
  
  return rr_;
}

// Post receives for RPC_CMD
int post_recv(struct rpc_request* rr_, struct node_id* peer_, int i)
{
  struct ibv_recv_wr wr, *bad_wr_ = NULL;
  struct ibv_sge sge;
  
  wr.wr_id = (uintptr_t)rr_->msg->msg_data + i*sizeof(struct rpc_cmd);
  wr.next = NULL;
  wr.sg_list = &sge;
  wr.num_sge = 1;
  
  sge.addr = (uintptr_t)rr_->msg->msg_data + i*sizeof(struct rpc_cmd);
  sge.length = sizeof(struct rpc_cmd);
  sge.lkey = rr_->rpc_mr->lkey;
  
  int err;
  // return_if_err(ibv_post_recv(peer_->rpc_conn.qp, &wr, &bad_wr_), err)
  err = ibv_post_recv(peer_->rpc_conn.qp, &wr, &bad_wr_);
  if (err == EINVAL) {
    log(ERROR, "Invalid value provided in wr; peer_id= " << peer_->ptlmap.id)
    return 1;
  }
  else if (err == ENOMEM) {
    log(ERROR, "Receive Queue is full or not enough resources to complete this operation; peer_id= " << peer_->ptlmap.id)
    return 1;
  }
  else if (err == EFAULT) {
    log(ERROR, "Invalid value provided in qp; peer_id= " << peer_->ptlmap.id)
    return 1;
  }
  else if (err) {
    log(ERROR, "unknown errno= " << errno)
    return 1;
  }
  
  return 0;
}

// It creates a rpc_request node rr; then prepares, registers, and posts buffer for future coming RPC_CMD; and then adds this rr entry to rpc_list
int rpc_post_recv(struct rpc_server* rpc_s_, struct node_id* peer_)
{
  struct rpc_request* rr_ = rr_comm_alloc(rpc_s_->num_buf);
  rr_->peerid = peer_->ptlmap.id;
  peer_->rr = rr_;
  
  // Register memory for one rpc_cmd that will store the coming rpc_cmd from peer
  int err;
  return_if_err(_ibv_reg_mr(rr_->rpc_mr, peer_->rpc_conn.pd, rr_->msg->msg_data, rr_->msg->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE), err)
  
  for (int i = 0; i < rpc_s_->num_buf; i++) {
    rr_->current_rpc_count = i + 1;
    return_if_err(post_recv(rr_, peer_, rr_->current_rpc_count - 1), err)
    peer_->num_recv_buf++;
  }
  
  return 0;
}

int sys_post_recv(struct rpc_server* rpc_s_, struct node_id* peer_)
{
	int err = 0;
	struct sys_msg* sm_ = (struct sys_msg*)calloc(1, sizeof(struct sys_msg) );
  
	peer_->sm = sm_;
	sm_->hs.sys_id = peer_->ptlmap.id;
  
	struct hdr_sys* hs_ = (struct hdr_sys*)malloc(rpc_s_->num_buf*sizeof(struct hdr_sys) );
	sm_->real_hs = hs_;
  return_if_err(_ibv_reg_mr(sm_->sys_mr, peer_->sys_conn.pd, hs_, rpc_s_->num_buf*sizeof(struct hdr_sys), IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE), err)
  
  struct ibv_recv_wr wr, *bad_wr_ = NULL;
  struct ibv_sge sge;
	for (int i = 0; i < rpc_s_->num_buf; i++) {
		wr.wr_id = (uintptr_t)hs_ + i*sizeof(struct hdr_sys);
		wr.next = NULL;
		wr.sg_list = &sge;
		wr.num_sge = 1;
    
		sge.addr = (uintptr_t)hs_ + i*sizeof(struct hdr_sys);
		sge.length = sizeof(struct hdr_sys);
		sge.lkey = sm_->sys_mr->lkey;
    
    return_if_err(ibv_post_recv(peer_->sys_conn.qp, &wr, &bad_wr_), err)
		peer_->num_sys_recv_buf++;
	}
  
	return 0;
}

// Default completion routine for rpc messages we initiate. This routine is called by 'rpc_process_event()' 
// to complete rpc requests which were locally initiated.
int rpc_cb_req_completion(struct rpc_server* rpc_s_, struct ibv_wc* wc_)
{
  int err;
  struct rpc_request* rr_;
  return_err_if_ret_cond_flag((struct rpc_request*)(uintptr_t)wc_->wr_id, rr_, ==, 0, 0)
  
  struct node_id* peer_ = (struct node_id*)rr_->msg->peer;
  if (rr_->msg)
    rr_->msg->refcont--;
  
  // Send back msg to tell other side RDMA operation is finished. Since IB just generates wc on post side.
  if (rr_->type == 1) { // data
    struct msg_buf* msg_;
    return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_, ==, 0, err)
    
    msg_->size = sizeof(struct rpc_cmd);
    msg_->msg_rpc->cmd = peer_rdma_done;
    msg_->msg_rpc->id = rpc_s_->ptlmap.id;
    msg_->msg_rpc->wr_id = rr_->msg->id;
    log(DEBUG, "will rpc_send peer_rdma_done cmd to peer_id= " << peer_->ptlmap.id)
    err = rpc_send(rpc_s_, peer_, msg_);
    if (err < 0) {
      log(ERROR, "rpc_send failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", peer_->ptlmap.id= " << peer_->ptlmap.id)
      free(msg_->msg_data);
      free(msg_);
      return err;
    }
  }
  if (rr_->msg) {
    if (rr_->msg->refcont == 0) { // WHY
      if (rr_->rpc_mr != 0) {
        return_if_err(_ibv_dereg_mr(rr_->rpc_mr), err)
      }
      if (rr_->data_mr != 0) {
        return_if_err(_ibv_dereg_mr(rr_->data_mr), err)
      }
      
      // log(INFO, "rr_->msg= \n" << rpc_to_str("msg_buf", rr_->msg) )
      if (rr_->msg->cb) {
        return_if_err((*rr_->msg->cb)(rpc_s_, rr_->msg), err)
      }
      free(rr_);
    }
  }
  
  return 0;
}

void rpc_server_set_rpc_per_buff(struct rpc_server* rpc_s_, int num_rpc_per_buff)
{
	rpc_s_->num_rpc_per_buff = num_rpc_per_buff;
}

// Blindly get the rpc_server reference; if it is not initialized, should call rpc_server_init()
struct rpc_server* rpc_server_get_instance(void)
{
	return rpc_s_instance;
}

// Return the id of the rpc server.
int rpc_server_get_id(void)
{
	// TODO: if server is not initialized, should return -1.
	return rpc_s_instance->ptlmap.id;
}

/******************************************  Conn_rel  ********************************************/
int build_context(struct ibv_context* verb_, struct connection* conn_)
{
	int err, flags;
	conn_->ctx = verb_;
  
	if (!rpc_s_instance->alloc_pd_flag) {
		rpc_s_instance->global_pd = ibv_alloc_pd(conn_->ctx);
		rpc_s_instance->global_ctx = verb_;
		rpc_s_instance->alloc_pd_flag = 1;
	}
	return_err_if_ret_cond_flag(rpc_s_instance->global_pd, conn_->pd, ==, NULL, 1)
	return_err_if_ret_cond_flag(ibv_create_comp_channel(conn_->ctx), conn_->comp_channel, ==, NULL, 1)
	// cqe=65536 is arbitrary
	return_err_if_ret_cond_flag(ibv_create_cq(conn_->ctx, 65536, NULL, conn_->comp_channel, 0), conn_->cq, ==, NULL, 1)
	return_if_err(ibv_req_notify_cq(conn_->cq, 0), err)
	// change the blocking mode of the completion channel
	flags = fcntl(conn_->ctx->async_fd, F_GETFL);
	return_err_if_ret_cond_flag(fcntl(conn_->ctx->async_fd, F_SETFL, flags | O_NONBLOCK), err, <, 0, err)
	flags = fcntl(conn_->comp_channel->fd, F_GETFL);
	return_err_if_ret_cond_flag(fcntl(conn_->comp_channel->fd, F_SETFL, flags | O_NONBLOCK), err, <, 0, err)
}

int build_qp_attr(struct ibv_qp_init_attr* qp_attr_, struct connection* conn_, struct rpc_server* rpc_s_)
{
	memset(qp_attr_, 0, sizeof(*qp_attr_) );
  
	qp_attr_->send_cq = conn_->cq;
	qp_attr_->recv_cq = conn_->cq;
	qp_attr_->qp_type = IBV_QPT_RC;
  
	qp_attr_->cap.max_send_wr = rpc_s_->num_buf;
	qp_attr_->cap.max_recv_wr = rpc_s_->num_buf;
	qp_attr_->cap.max_send_sge = 1;
	qp_attr_->cap.max_recv_sge = 1;
	
	return 0;
}

int rpc_connect(struct rpc_server* rpc_s_, struct node_id* peer_)
{
  struct addrinfo* addr_;
  struct rdma_cm_event* event_ = NULL;
  struct rdma_conn_param cm_params;
  struct con_param conpara;
  
  char* ip_;
  char port_[32];
  int err, check;
  check = 0;
  
  // Resolve the IP:Port of DS_Master
  ip_ = inet_ntoa(peer_->ptlmap.address.sin_addr);
  snprintf(port_, sizeof(port_), "%u", ntohs(peer_->ptlmap.address.sin_port) );
  // log(INFO, "ip_= " << ip_ << ", port_= " << port_)
  
  return_if_err(getaddrinfo(ip_, port_, NULL, &addr_), err)
  return_err_if_ret_cond_flag(rdma_create_event_channel(), peer_->rpc_ec, ==, NULL, -ENOMEM)
  return_if_err(rdma_create_id(peer_->rpc_ec, &(peer_->rpc_conn.id), NULL, RDMA_PS_TCP), err)
  return_if_err(rdma_resolve_addr(peer_->rpc_conn.id, NULL, addr_->ai_addr, INFINIBAND_TIMEOUT), err)
  freeaddrinfo(addr_);
  
  while (rdma_get_cm_event(peer_->rpc_ec, &event_) == 0) {
  	struct rdma_cm_event event_copy;
  	memcpy(&event_copy, event_, sizeof(*event_) );
    // rdma_ack_cm_event(event_);
    
  	if (event_copy.event == RDMA_CM_EVENT_ADDR_RESOLVED) {
      log(INFO, "recved RDMA_CM_EVENT_ADDR_RESOLVED to peer_id= " << peer_->ptlmap.id)
  		return_if_err(build_context(event_copy.id->verbs, &(peer_->rpc_conn) ), err)
  		return_if_err(build_qp_attr(&(peer_->rpc_conn.qp_attr), &(peer_->rpc_conn), rpc_s_), err)
      
  		return_if_err(rdma_create_qp(event_copy.id, peer_->rpc_conn.pd, &(peer_->rpc_conn.qp_attr) ), err)
      
  		event_copy.id->context = &(peer_->rpc_conn);
  		peer_->rpc_conn.id = event_copy.id;
  		peer_->rpc_conn.qp = event_copy.id->qp;
      
      return_if_err(rpc_post_recv(rpc_s_, peer_), err)
      // log(INFO, "done preparing buffer at id= " << rpc_s_->ptlmap.id << " for peer_id= " << peer_->ptlmap.id)
  		return_if_err(rdma_resolve_route(event_copy.id, INFINIBAND_TIMEOUT), err)
  	}
    else if (event_copy.event == RDMA_CM_EVENT_ROUTE_RESOLVED) {
      log(INFO, "recved RDMA_CM_EVENT_ROUTE_RESOLVED to peer_id= " << peer_->ptlmap.id)
  		memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
      
  		conpara.pm_cp = rpc_s_->ptlmap;
  		conpara.pm_sp = peer_->ptlmap;
  		conpara.num_cp = rpc_s_->num_rpc_per_buff;
  		conpara.type = 1;
      
  		cm_params.private_data = &conpara;
  		cm_params.private_data_len = sizeof(conpara);
  		cm_params.initiator_depth = cm_params.responder_resources = 1;
  		cm_params.retry_count = 7;
  		cm_params.rnr_retry_count = 7;	/* infinite retry */
      
  		return_if_err(rdma_connect(event_copy.id, &cm_params), err)
    }
  	else if (event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
      log(INFO, "recved RDMA_CM_EVENT_ESTABLISHED to peer_id= " << peer_->ptlmap.id)
  		if (peer_->ptlmap.id == 0) {
  			if (rpc_s_->ptlmap.appid != 0) {
  				rpc_s_->app_minid = ((struct con_param*)event_copy.param.conn.private_data)->type;
  				rpc_s_->ptlmap.id = ((struct con_param*)event_copy.param.conn.private_data)->pm_sp.id;
  				peer_->ptlmap = ((struct con_param*)event_copy.param.conn.private_data)->pm_cp;
  			}
  			else
  				rpc_s_->ptlmap.id = *((int*)event_copy.param.conn.private_data);
  		}
  		
  		rdma_ack_cm_event(event_);
  		peer_->rpc_conn.f_connected = 1;
  		return 0;
  	}
  	else {
  	  log(ERROR, "unknown event= " << rpc_event_to_str(&event_copy) << ", to peer_id= " << peer_->ptlmap.id << "\n"
  	             << "\t event status= " << event_copy.status)
  		return event_copy.status;
  	}
  	
  	rdma_ack_cm_event(event_);
  }
  
	return 0;
}

/*********************************************  Sys_rel  ******************************************/
int sys_connect(struct rpc_server* rpc_s_, struct node_id* peer_)
{
  struct addrinfo* addr_;
  struct rdma_cm_event* event_ = NULL;
  struct rdma_conn_param cm_params;
  struct con_param conpara;
  
  char* ip_;
  char port_[32];
  int err, check;
  check = 0;
  
  // Resolve the IP:Port of DS_Master
  ip_ = inet_ntoa(peer_->ptlmap.address.sin_addr);
  snprintf(port_, sizeof(port_), "%u", ntohs(peer_->ptlmap.address.sin_port) );
  log(INFO, "ip_= " << ip_ << ", port_= " << port_)
  
  return_if_err(getaddrinfo(ip_, port_, NULL, &addr_), err)
  return_err_if_ret_cond_flag(rdma_create_event_channel(), peer_->sys_ec, ==, NULL, -ENOMEM)
  return_if_err(rdma_create_id(peer_->sys_ec, &(peer_->sys_conn.id), NULL, RDMA_PS_TCP), err)
  return_if_err(rdma_resolve_addr(peer_->sys_conn.id, NULL, addr_->ai_addr, INFINIBAND_TIMEOUT), err)
  freeaddrinfo(addr_);
  
  while (rdma_get_cm_event(peer_->sys_ec, &event_) == 0) {
    struct rdma_cm_event event_copy;
    memcpy(&event_copy, event_, sizeof(*event_) );
    rdma_ack_cm_event(event_);
    
    if (event_copy.event == RDMA_CM_EVENT_ADDR_RESOLVED) {
      log(INFO, "recved RDMA_CM_EVENT_ADDR_RESOLVED.")
      return_if_err(build_context(event_copy.id->verbs, &(peer_->sys_conn) ), err)
      return_if_err(build_qp_attr(&(peer_->sys_conn.qp_attr), &(peer_->sys_conn), rpc_s_), err)
      
      return_if_err(rdma_create_qp(event_copy.id, peer_->sys_conn.pd, &(peer_->sys_conn.qp_attr) ), err)
      
      event_copy.id->context = &(peer_->sys_conn);
      peer_->sys_conn.id = event_copy.id;
      peer_->sys_conn.qp = event_copy.id->qp;
      
      return_if_err(rpc_post_recv(rpc_s_, peer_), err)
      log(INFO, "done preparing buffer at id= " << rpc_s_->ptlmap.id << " for peer_id= " << peer_->ptlmap.id)
      return_if_err(rdma_resolve_route(event_copy.id, INFINIBAND_TIMEOUT), err)
    }
    else if (event_copy.event == RDMA_CM_EVENT_ROUTE_RESOLVED) {
      log(INFO, "recved RDMA_CM_EVENT_ROUTE_RESOLVED.")
      memset(&cm_params, 0, sizeof(struct rdma_conn_param) );
      
      conpara.pm_cp = rpc_s_->ptlmap;
      conpara.pm_sp = peer_->ptlmap;
      conpara.num_cp = rpc_s_->num_rpc_per_buff;
      conpara.type = 1;
      
      cm_params.private_data = &conpara;
      cm_params.private_data_len = sizeof(conpara);
      cm_params.initiator_depth = cm_params.responder_resources = 1;
      cm_params.retry_count = 7;  //diff
      cm_params.rnr_retry_count = 7;  /* infinite retry */
      
      return_if_err(rdma_connect(event_copy.id, &cm_params), err)
    }
    else if (event_copy.event == RDMA_CM_EVENT_ESTABLISHED) {
      log(INFO, "recved RDMA_CM_EVENT_ESTABLISHED.")
      if (peer_->ptlmap.id == 0) {
        if (rpc_s_->ptlmap.appid != 0) {
          rpc_s_->app_minid = ((struct con_param*) event_copy.param.conn.private_data)->type;
          rpc_s_->ptlmap.id = ((struct con_param*) event_copy.param.conn.private_data)->pm_sp.id;
          peer_->ptlmap = ((struct con_param*) event_copy.param.conn.private_data)->pm_cp;
        }
        else
          rpc_s_->ptlmap.id = *((int*) event_copy.param.conn.private_data);
      }
      peer_->sys_conn.f_connected = 1;
      return 0;
    }
    else {
      log(ERROR, "unknown event= " << rpc_event_to_str(&event_copy) )
      return event_copy.status;
    }
  }
  
  return 0;
}

// int sys_cleanup(struct rpc_server *rpc_s)
// {
//   int i, err = 0;
//   struct node_id *peer;
//   for(i = 0; i < rpc_s->cur_num_peer - 1; i++) {
//     peer = &rpc_s->peer_tab[i];
//     if(peer->sys_conn.f_connected == 0 || peer->ptlmap.id == rpc_s->ptlmap.id)
//       continue;
//     err = rdma_destroy_id(peer->sys_conn.id);
//     if(err < 0) {
//       printf("Failed to rdma_destroy_id with err(%d).\n", err);
//       goto err_out;
//     }
//     peer->sys_conn.f_connected = 0;
//   }
//   return 0;

//       err_out:
//   printf("(%s): err (%d).\n", __func__, err);
//   return err;
// }

// int sys_bar_arrive(struct rpc_server *rpc_s, struct hdr_sys *hs)  //Done
// {
//   rpc_s->bar_tab[hs->sys_id] = hs->sys_pad1;

//   return 0;
// }

int sys_dispatch_event(struct rpc_server* rpc_s_, struct hdr_sys* hs_)
{
  int err;
  switch (hs_->sys_cmd) {
    case sys_none:
      break;
    case sys_bar_enter:
      // return_if_err(sys_bar_arrive(rpc_s_, hs_), err)
      rpc_s_->bar_tab[hs_->sys_id] = hs_->sys_pad1;
      break;
  }

  return 0;
}

//sys process use poll
int sys_process_event(struct rpc_server* rpc_s_)
{
//   struct node_id* peer_;
//   struct pollfd my_pollfd_[rpc_s_->num_rpc_per_buff - 1];
//   int peer_index_[rpc_s_->num_rpc_per_buff - 1];
  
//   int ret, j, err = 0;
//   int num_ds = rpc_s_->cur_num_peer - rpc_s_->num_rpc_per_buff;
  
//   j = 0;
//   for (int i = rpc_s_->num_sp; i < rpc_s_->num_sp + rpc_s_->app_num_peers; i++) {
//     peer_ = rpc_s_->peer_tab + i;
//     if (peer_->ptlmap.id == rpc_s_->ptlmap.id)
//       continue;
    
//     if (peer_->sys_conn.f_connected == 0)
//       continue;
//     my_pollfd_[j].fd = peer_->sys_conn.comp_channel->fd;
//     my_pollfd_[j].events = POLLIN;
//     my_pollfd_[j].revents = 0;
//     peer_index_[j] = i;
    
//     j++;
//   }
  
//   int timeout = 30;
//   err = poll(my_pollfd_, j, timeout);
//   if (err < 0) {
//     log(ERROR, "rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", poll failed!")
//     return err;
//   }
//   else if (err == 0)
//     return 0;
//   // err > 0
//   struct ibv_cq* ev_cq_;
//   void* ev_ctx_;
//   struct ibv_wc wc;
//   for (int i = 0; i < j; i++) {
//     if (my_pollfd_[i].revents == 0) // Check SYS channel
//       continue;
//     else {
//       log(INFO, "polled event for peer_index= " << peer_index_[i] )
//     }
    
//     peer_ = rpc_s_->peer_tab + peer_index_[i];
//     err = ibv_get_cq_event(peer_->sys_conn.comp_channel, &ev_cq_, &ev_ctx_); 
//     if (err == -1 && errno == EAGAIN) // no completion event to read for peer peer_index_[i]
//       break;
//     if (err == -1 && errno != EAGAIN) {
//       log(ERROR, "ibv_get_cq_event failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id)
//       return errno;
//     }
    
//     ibv_ack_cq_events(ev_cq_, 1);
//     return_if_err(ibv_req_notify_cq(ev_cq_, 0), err)
//     do {
//       return_err_if_ret_cond_flag(ibv_poll_cq(ev_cq_, 1, &wc), ret, <, 0, err)
//       if (ret == 0)
//         continue;

//       if (wc.status != IBV_WC_SUCCESS) {
//         log(ERROR, "wc.status is not IBV_WC_SUCCESS!")
//         return wc.status;
//       }
//       else if (wc.opcode & IBV_WC_RECV) {
//         return_if_err(sys_dispatch_event(rpc_s_, (struct hdr_sys*)(uintptr_t)wc.wr_id), err)
        
//         struct ibv_recv_wr wr, *bad_wr_ = NULL;
//         struct ibv_sge sge;
//         int err = 0;
        
//         wr.wr_id = (uintptr_t)wc.wr_id;
//         wr.next = NULL;
//         wr.sg_list = &sge;
//         wr.num_sge = 1;
        
//         sge.addr = (uintptr_t)wc.wr_id;
//         sge.length = sizeof(struct hdr_sys);
//         sge.lkey = peer_->sm->sys_mr->lkey;
        
//         return_if_err(ibv_post_recv(peer_->sys_conn.qp, &wr, &bad_wr_), err)
//       }
//       else if (wc.opcode == IBV_WC_SEND) {
//         struct sys_msg* sm_ = (struct sys_msg*)(uintptr_t)wc.wr_id;
//         if (sm_->sys_mr != 0)
//           return_if_err(_ibv_dereg_mr(sm_->sys_mr), err)
//         free(sm_);
//       }
//       else {
//         log(ERROR, "unknown wc.opcode= " << wc.opcode)
//         return wc.opcode;
//       }
//     } while(ret);
//   }
  
  return 0;
}

int sys_send(struct rpc_server* rpc_s_, struct node_id* peer_, struct hdr_sys* hs_)
{
  int err;
  while (peer_->sys_conn.f_connected != 1) {
    log(WARNING, "SYS channel has not been established from rpc_s_->ptlmap.id= "
                 << rpc_s_->ptlmap.id << " to peer_->ptlmap.id= " << peer_->ptlmap.id
                 << ", will try to connect now...")
    return_if_err(sys_connect(rpc_s_, peer_), err)
  }
  
  struct sys_msg* sm_;
  struct ibv_send_wr wr, *bad_wr_ = NULL;
  struct ibv_sge sge;
  
  sm_ = (struct sys_msg*)calloc(1, sizeof(struct sys_msg) );
  memset(&wr, 0, sizeof(wr) );
  
  sm_->hs = *hs_;
  return_if_err(_ibv_reg_mr(sm_->sys_mr, peer_->sys_conn.pd, &(sm_->hs), sizeof(struct hdr_sys), IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ), err)
  
  wr.opcode = IBV_WR_SEND;
  wr.wr_id = (uintptr_t)sm_; // use address of this hs as the unique wr id
  wr.sg_list = &sge;
  wr.num_sge = 1;
  wr.send_flags = IBV_SEND_SIGNALED;
  
  sge.addr = (uintptr_t) & sm_->hs;
  sge.length = sizeof(struct hdr_sys);
  sge.lkey = sm_->sys_mr->lkey;
  
  return_err_if_ret_cond_flag(ibv_post_send(peer_->sys_conn.qp, &wr, &bad_wr_), err, <, 0, err)
  
  return 0;
}

int sys_bar_send(struct rpc_server* rpc_s_, int peerid)
{
  struct node_id *peer = rpc_get_peer(rpc_s_, peerid);
  struct hdr_sys hs;
  int err;
  
  memset(&hs, 0, sizeof(struct hdr_sys) );
  hs.sys_cmd = sys_bar_enter;
  hs.sys_pad1 = rpc_s_->bar_num;
  hs.sys_id = myrank(rpc_s_);
  return_if_err(sys_send(rpc_s_, peer, &hs), err)
  
  return 0;
}

// System barrier implementation.
int rpc_barrier(struct rpc_server* rpc_s_)
{
  log(INFO, "started; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id)
  int err;
  
  int np = log2_ceil(rpc_s_->app_num_peers);
  rpc_s_->bar_num = (rpc_s_->bar_num + 1) & 0xFF; // Note: rather than using %, bitwise operation is more efficient
  
  int _round = -1;
  while (_round < np - 1) {
    _round = _round + 1;
    
    int next = (myrank(rpc_s_) + (1 << _round) ) % rpc_s_->app_num_peers;
    int prev = (rpc_s_->app_num_peers + myrank(rpc_s_) - (1 << _round) ) % rpc_s_->app_num_peers;
    
    return_err_if_ret_cond_flag(sys_bar_send(rpc_s_, rank2id(rpc_s_, next) ), err, <, 0, err)
    
    // SYS_WAIT_COMPLETION(rpc_s_->bar_tab[prev] == rpc_s->bar_num || rpc_s->bar_tab[prev] == ((rpc_s->bar_num + 1) & 0xFF) )  //delete ";" here
    while (!(rpc_s_->bar_tab[prev] == rpc_s_->bar_num || rpc_s_->bar_tab[prev] == ((rpc_s_->bar_num + 1) & 0xFF) ) )
      return_err_if_ret_cond_flag(sys_process_event(rpc_s_), err, <, 0, err)
  }
  
  log(INFO, "done; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id)
  return 0;
}

/*********************************************  Msg_rel  ******************************************/
struct msg_buf* msg_buf_alloc(struct rpc_server* rpc_s_, struct node_id* peer_, int num_rpcs)
{
	struct msg_buf* msg_;
	size_t size;
  
	size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd) * num_rpcs + 7;
	msg_ = (struct msg_buf*)calloc(1, size);
	if (!msg_)
		return NULL;
  
	msg_->peer = peer_;
	msg_->cb = default_completion_callback;
  
	if (num_rpcs > 0) {
		msg_->msg_rpc = (struct rpc_cmd*)(msg_ + 1);
		ALIGN_ADDR_QUAD_BYTES(msg_->msg_rpc, struct rpc_cmd*);
		// log(DEBUG, "rpc_s_->ptlmap= \n" << rpc_to_str("ptlid_map", &(rpc_s_->ptlmap) ) << "\n"
		//           << "peer_->ptlmap= \n" << rpc_to_str("ptlid_map", &(peer_->ptlmap) ) )
		msg_->msg_rpc->src = rpc_s_->ptlmap.address;
		msg_->msg_rpc->dst = peer_->ptlmap.address;
		// memcpy(&(msg_->msg_rpc->src), &(rpc_s_->ptlmap.address), sizeof(sockaddr_storage) );
		// memcpy(&(msg_->msg_rpc->dst), &(peer_->ptlmap.address), sizeof(sockaddr_storage) );
	}

	return msg_;
}

// Called in a while in common_run_server in common.c
int trouble__process_event(struct rpc_server* rpc_s_, int timeout)
{
  int err;
  // struct pollfd my_pollfd_[2*(rpc_s_->cur_num_peer - 1) ];
	std::vector<struct pollfd> my_pollfd_v;
	std::vector<int> peer_index_v;
	std::vector<int> sys_peer_v;
	
	// Initialize file descriptor set
	int ret, j = 0;
  for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++) {
    // log(DEBUG, "will check with peer_id= " << it->first)
    if (rpc_s_->ptlmap.id == it->first)
      continue;
    struct node_id* peer_ = it->second;
    if (peer_->sys_conn.f_connected) {
      log(WARNING, "DEPRECATED; should not have come here!")
      struct pollfd poll_fd;
      poll_fd.fd = peer_->sys_conn.comp_channel->fd;
      poll_fd.events = POLLIN;
      poll_fd.revents = 0;
      my_pollfd_v.push_back(poll_fd);
      // my_pollfd_[j].fd = peer_->sys_conn.comp_channel->fd;
      // my_pollfd_[j].events = POLLIN;
      // my_pollfd_[j].revents = 0;
      peer_index_v.push_back(it->first);
      sys_peer_v.push_back(1);
      j++;
    }
    if (peer_->rpc_conn.f_connected) {
      struct pollfd poll_fd;
      poll_fd.fd = peer_->rpc_conn.comp_channel->fd;
      poll_fd.events = POLLIN;
      poll_fd.revents = 0;
      my_pollfd_v.push_back(poll_fd);
      // my_pollfd_[j].fd = peer_->rpc_conn.comp_channel->fd;
      // my_pollfd_[j].events = POLLIN;
      // my_pollfd_[j].revents = 0;
      peer_index_v.push_back(it->first);
      sys_peer_v.push_back(0);
      j++;
    }
  }
  
	if (j == 0) {
	  log(WARNING, "rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", no peer to poll for...")
		return 0;
	}
	
  // err = poll(my_pollfd_, j, timeout);
	err = poll((struct pollfd*)&my_pollfd_v[0], j, timeout);
	if (err < 0) {
		log(ERROR, "rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", poll failed!")
		return err;
	} 
	else if (err == 0)
	  return 0; // -ETIME;
  // err > 0
	struct ibv_cq* ev_cq_;
  void* ev_ctx_;
	for (int i = 0; i < j; i++) {
	  if (my_pollfd_v[i].revents == 0)
	    continue;
		else {
		  // log(INFO, "polled event for peer_index= " << peer_index_[i] << ", sys_peer= " << sys_peer_[i] )
		}
		struct node_id* peer_ = id_peer_map[peer_index_v[i] ];
		if (sys_peer_v[i] == 1)
		  err = ibv_get_cq_event(peer_->sys_conn.comp_channel, &ev_cq_, &ev_ctx_);
		else if (sys_peer_v[i] == 0)
		  err = ibv_get_cq_event(peer_->rpc_conn.comp_channel, &ev_cq_, &ev_ctx_);
	  
    if (err == -1 && errno == EAGAIN) // no completion event to read for peer_
      break;
    if (err == -1 && errno != EAGAIN) {
      log(ERROR, "ibv_get_cq_event failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id)
      return errno;
    }
    
    ibv_ack_cq_events(ev_cq_, 1);
    return_if_err(ibv_req_notify_cq(ev_cq_, 0), err)
    struct ibv_wc wc;
    do {
      return_err_if_ret_cond_flag(ibv_poll_cq(ev_cq_, 1, &wc), ret, <, 0, err)
      if (ret == 0) // there may be an extra event with no completion in cq
        continue;
      
      if (wc.status != IBV_WC_SUCCESS) {
        log(ERROR, "wc.status is not IBV_WC_SUCCESS!")
        if (sys_peer_v[i] == 1)
          return wc.status;
        else if (sys_peer_v[i] == 0)
          return wc.status;
      }
      else if (wc.opcode & IBV_WC_RECV) {
        if (sys_peer_v[i] == 1) {
          return_if_err(sys_dispatch_event(rpc_s_, (struct hdr_sys*)(uintptr_t)wc.wr_id), err)
          
          struct ibv_recv_wr wr, *bad_wr_ = NULL;
          struct ibv_sge sge;
          
          wr.wr_id = (uintptr_t)wc.wr_id;
          wr.next = NULL;
          wr.sg_list = &sge;
          wr.num_sge = 1;
  
          sge.addr = (uintptr_t)wc.wr_id;
          sge.length = sizeof(struct hdr_sys);
          sge.lkey = peer_->sm->sys_mr->lkey;
          
          return_if_err(ibv_post_recv(peer_->sys_conn.qp, &wr, &bad_wr_), err)
        }
        else if (sys_peer_v[i] == 0) {
          // struct ibv_wc wc_copy;
          // memcpy(&wc_copy, &wc, sizeof(wc) );
          
          return_if_err(rpc_cb_decode(rpc_s_, &wc), err)
          peer_->num_recv_buf--;
          
          struct ibv_recv_wr wr, *bad_wr_ = NULL;
          struct ibv_sge sge;
          int err = 0;
          
          wr.wr_id = (uintptr_t)wc.wr_id;
          wr.next = NULL;
          wr.sg_list = &sge;
          wr.num_sge = 1;
          
          sge.addr = (uintptr_t)wc.wr_id;
          sge.length = sizeof(struct rpc_cmd);
          sge.lkey = peer_->rr->rpc_mr->lkey;
          
          return_if_err(ibv_post_recv(peer_->rpc_conn.qp, &wr, &bad_wr_), err)
          peer_->num_recv_buf++;
        }
      }
      else if (wc.opcode == IBV_WC_SEND && sys_peer_v[i] == 1) {
        struct sys_msg* sm_ = (struct sys_msg*)(uintptr_t)wc.wr_id;
        if (sm_->sys_mr != 0)
          return_if_err(_ibv_dereg_mr(sm_->sys_mr), err)
        free(sm_);
      }
      else if ((wc.opcode == IBV_WC_SEND || wc.opcode == IBV_WC_RDMA_WRITE || wc.opcode == IBV_WC_RDMA_READ) && sys_peer_v[i] == 0) {
        peer_->req_posted--;
        struct rpc_request* rr_ = (struct rpc_request*)(uintptr_t)wc.wr_id;
        return_if_err((*(rr_->cb) )(rpc_s_, &wc), err)
      }
      else {
        log(ERROR, "unknown wc.opcode= " << wc.opcode)
        return wc.opcode;
      }
    } while(ret);
	}
	
	return 0;
}

int rpc_process_event(struct rpc_server* rpc_s_)
{
	int err = trouble__process_event(rpc_s_, 100);
	if (err > 0) {
	   log(ERROR, "trouble__process_event failed!")
	  return err;
	}
  
  return 0;
}

void* rpc_process_event_loop(void* hash_to_notify_)
{
  // unsigned int hash = *((unsigned int*)hash_to_notify_);
  struct rpc_server* rpc_s_ = rpc_s_instance;
  // log(DEBUG, "started; hash= " << hash)
  int err;
  while (1) {
    return_err_if_ret_cond_flag(trouble__process_event(rpc_s_, 100), err, >, 0, NULL)
  }
  // log(DEBUG, "done; hash= " << hash)
}

int rpc_process_event_with_timeout(struct rpc_server* rpc_s_, int timeout)
{
	int err;
  return_if_err(trouble__process_event(rpc_s_, timeout), err)
  
	return 0;
}

/****************************************  Data_rel  **********************************************/
int peer_process_send_list(struct rpc_server* rpc_s_, struct node_id* peer_)
{
  int err;
  std::string list_str = patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list";
  while (!list_empty(list_str, &(peer_->req_list) ) ) {
    if (rpc_s_->com_type == DART_CLIENT || peer_->req_posted > 100) {
      for (int i = 0; i < 10; i++)  { // performance here
        err = rpc_process_event_with_timeout(rpc_s_, 1);
        if (err > 0)
          return err;
      }
    }
    struct rpc_request* rr_ = list_entry((peer_->req_list).next_, struct rpc_request, req_entry);
    if (!rr_ || !(rr_->msg) ) {
      log(WARNING, "rr is empty from peer_id= " << peer_->ptlmap.id)
      return 0;
    }
    
    return_if_err(rpc_prepare_buffers(rpc_s_, peer_, rr_, rr_->iodir), err)
    
    return_if_err(rpc_post_request(rpc_s_, peer_, rr_), err)
    peer_->num_msg_at_peer--;
    list_del(list_str, &(rr_->req_entry) );
    peer_->num_req--;
  }
  
  return 0;
}

int rpc_server_free(struct rpc_server* rpc_s_)
{
  int err;
  struct node_id* peer_;
  for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++) {
    log(DEBUG, "finishing up peer_id= " << it->first)
    peer_ = it->second;
    if (peer_->rpc_conn.f_connected == 1) {
      while (peer_->num_req) {
        return_err_if_ret_cond_flag(peer_process_send_list(rpc_s_, peer_), err, <, 0, err, continue;)
      }
    }
  }
  
  struct rpc_request *rr_, *tmp_;
  // Process any remaining events.
  // err = rpc_process_event_with_timeout(rpc_s_, 100);
  // while (err == 0 || err == -EINVAL) {
  //   printf("%d  process what?\n",rpc_s_->ptlmap.id);
  //   err = rpc_process_event_with_timeout(rpc_s_, 100);
  // }
  // if (err != -ETIME) {
  //   printf("'%s()': erro at flushing the event queue %d!\n", __func__, err);
  //   goto err_out;
  // }
  
  // log(INFO, "rpc_s_->rpc_list= ")
  // list_to_str(&(rpc_s_->rpc_list), struct rpc_request, "rpc_request", req_entry, std::cout, rpc_to_str)
  list_for_each_entry_safe(rr_, tmp_, &(rpc_s_->rpc_list), struct rpc_request, req_entry) {
    return_if_err(_ibv_dereg_mr(rr_->rpc_mr), err)
    list_del("rpc_list", &(rr_->req_entry) );
    free(rr_);
  }
  
  // TODO: add a sys_list for sys msg, so that it will be easy to clean up remaining sys messages.
  // disconnect all the links, deregister the memory, free all the allocation
  for (std::map<short int, struct node_id*>::iterator it = id_peer_map.begin(); it != id_peer_map.end(); it++) {
    if (rpc_s_->ptlmap.id == it->first)
      continue;
    log(DEBUG, "cleaning up peer_id= " << it->first)
    peer_ = it->second;
    if (peer_->rpc_conn.f_connected == 1) {
      return_if_err(rdma_disconnect(peer_->rpc_conn.id), err)
      rdma_destroy_qp(peer_->rpc_conn.id);
      return_if_err(rdma_destroy_id(peer_->rpc_conn.id), err)
    }
    if (peer_->sys_conn.f_connected == 1) {
      log(WARNING, "DEPRECATED; should not have come here!")
      return_if_err(rdma_disconnect(peer_->sys_conn.id), err)
      rdma_destroy_qp(peer_->sys_conn.id);
      return_if_err(rdma_destroy_id(peer_->sys_conn.id), err)
    }
    free(peer_);
  }
  id_peer_map.clear();
  
  return_if_err(rdma_destroy_id(rpc_s_->listen_id), err)
  rdma_destroy_event_channel(rpc_s_->rpc_ec);
  
  free(rpc_s_->bar_tab);
  free(rpc_s_);
  
  return 0;
}

// *************************************  receive / send_direct  ******************************** //
int rpc_receive(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  log(DEBUG, "started; from peer_id= " << peer_->ptlmap.id
             << ", msg_->size= " << msg_->size)
  
  int err;
  if (peer_->rpc_conn.f_connected != 1) {
    log(WARNING, "RPC channel has not been established from rpc_s_->ptlmap.id= "
                 << rpc_s_->ptlmap.id << " to peer_->ptlmap.id= " << peer_->ptlmap.id
                 << ", will try to connect now...")
    return_if_err(rpc_connect(rpc_s_, peer_), err)
    // TODO:
    // INIT_LIST_HEAD(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(peer_->req_list) );
  }
  
  // return_if_err(rpc_send(rpc_s_, peer_, msg_), err)
  struct rpc_request* rr_;
  err = -ENOMEM;
  return_err_if_ret_cond_flag((struct rpc_request*)calloc(1, sizeof(struct rpc_request) ), rr_, ==, 0, err)
  
  rr_->type = 0; // 0 for cmd, 1 for data
  rr_->msg = msg_;
  rr_->iodir = io_receive;
  rr_->cb = (async_callback)rpc_cb_req_completion;

  rr_->data = msg_->msg_rpc;
  rr_->size = sizeof(*(msg_->msg_rpc) );
  rr_->f_vec = (flag_t)0;
  
  list_add_tail(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(rr_->req_entry), &(peer_->req_list) );
  
  peer_->num_req++;
  peer_->req_posted++;
  
  return_if_err(peer_process_send_list(rpc_s_, peer_), err)
  
  log(DEBUG, "done; from peer_id= " << peer_->ptlmap.id)
  return 0;
}

int rpc_cb_req_completion_(struct rpc_server* rpc_s_, struct ibv_wc* wc_)
{
  int err;
  struct rpc_request* rr_;
  return_err_if_ret_cond_flag((struct rpc_request*)(uintptr_t)wc_->wr_id, rr_, ==, 0, 0)
  
  struct node_id* peer_ = (struct node_id*)rr_->msg->peer;
  if (rr_->msg)
    rr_->msg->refcont--;
  
  // Send back msg to tell other side RDMA operation is finished. Since IB just generates wc on post side.
  // if (rr_->type == 1) { // data
  //   struct msg_buf* msg_;
  //   return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_, ==, 0, err)
    
  //   msg_->size = sizeof(struct rpc_cmd);
  //   msg_->msg_rpc->cmd = peer_rdma_done;
  //   msg_->msg_rpc->id = rpc_s_->ptlmap.id;
  //   msg_->msg_rpc->wr_id = rr_->msg->id;
  //   log(DEBUG, "will rpc_send peer_rdma_done cmd to peer_id= " << peer_->ptlmap.id)
  //   err = rpc_send(rpc_s_, peer_, msg_);
  //   if (err < 0) {
  //     log(ERROR, "rpc_send failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", peer_->ptlmap.id= " << peer_->ptlmap.id)
  //     free(msg_->msg_data);
  //     free(msg_);
  //     return err;
  //   }
  // }
  if (rr_->msg) {
    if (rr_->msg->refcont == 0) { // WHY
      if (rr_->rpc_mr != 0) {
        return_if_err(_ibv_dereg_mr(rr_->rpc_mr), err)
      }
      if (rr_->data_mr != 0) {
        return_if_err(_ibv_dereg_mr(rr_->data_mr), err)
      }
      
      // log(INFO, "rr_->msg= \n" << rpc_to_str("msg_buf", rr_->msg) )
      if (rr_->msg->cb) {
        return_if_err((*rr_->msg->cb)(rpc_s_, rr_->msg), err)
      }
      free(rr_);
    }
  }
  
  return 0;
}

int handle_rpc_send(struct rpc_server* rpc_s_, struct msg_buf* msg_)
{
  // log(DEBUG, "started; msg_->id_= " << msg_->id_)
  // free(msg_->id_);
  if (msg_->msg_data != NULL)
    free(msg_->msg_data);
  free(msg_);
  
  return 0;
}

int _rpc_send_direct(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  log(DEBUG, "started; to peer_id= " << peer_->ptlmap.id
             << ", msg_->size= " << msg_->size)
  int err;
  struct rpc_request* rr_;
  return_err_if_ret_cond_flag((struct rpc_request*)calloc(1, sizeof(struct rpc_request) ), rr_, ==, 0, -ENOMEM)
  
  rr_->type = 1; // 0 for cmd, 1 for data
  rr_->msg = msg_;
  rr_->iodir = io_send;
  rr_->cb = (async_callback)rpc_cb_req_completion_;
  rr_->data = msg_->msg_data;
  rr_->size = msg_->size;
  rr_->f_vec = (flag_t)0;
  
  err = rpc_post_request(rpc_s_, peer_, rr_);
  if (err) {
    log(ERROR, "rpc_post_request failed; rr= " << rpc_to_str("rpc_request", rr_) )
    free(rr_);
    return err;
  }
  peer_->req_posted++;
  
  log(DEBUG, "done; to peer_id= " << peer_->ptlmap.id)
  return 0;
}

int rpc_send_direct(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  int err;
  if (peer_->rpc_conn.f_connected != 1) {
    log(WARNING, "RPC channel has not been established from rpc_s_->ptlmap.id= "
                 << rpc_s_->ptlmap.id << " to peer_->ptlmap.id= " << peer_->ptlmap.id
                 << ", will try to connect now...")
    return_if_err(rpc_connect(rpc_s_, peer_), err)
  }
  
  uint64_t data_size_to_send = msg_->size;
  if (data_size_to_send <= RDMA_BUFFER_SIZE) {
    return_if_err(_rpc_send_direct(rpc_s_, peer_, msg_), err)
    // return 0;
  }
  else {
    log(DEBUG, "started large data send_direct; to peer_id= " << peer_->ptlmap.id
               << ", data_size_to_send= " << data_size_to_send)
    void* cur_head_ = msg_->msg_data;
    int counter = 0;
    while (data_size_to_send > 0) {
      int chunk_size = (data_size_to_send > RDMA_BUFFER_SIZE) ? RDMA_BUFFER_SIZE : data_size_to_send;
      void* chunk_ = malloc(chunk_size);
      memcpy(chunk_, cur_head_, chunk_size);
      
      struct msg_buf* msg_chunk_;
      return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_chunk_, ==, 0, err)
      msg_chunk_->size = chunk_size;
      msg_chunk_->msg_data = chunk_;
      msg_chunk_->cb = handle_rpc_send;
      msg_chunk_->mr = msg_->mr;
      msg_chunk_->mr.addr = static_cast<char*>(msg_->mr.addr) + counter*chunk_size;
      log(DEBUG, "will _rpc_send_direct msg_chunk_; to peer_id= " << peer_->ptlmap.id << "\n"
                 << "\t counter= " << counter << ", chunk_size= " << chunk_size)
      err = _rpc_send_direct(rpc_s_, peer_, msg_chunk_);
      if (err) {
        log(ERROR, "_rpc_send_direct failed; to peer_id= " << peer_->ptlmap.id << ", msg_chunk_= \n" << rpc_to_str("msg_buf", msg_chunk_) )
        free(msg_chunk_->msg_data);
        free(msg_chunk_);
        return err;
      }
      // err = rpc_process_event(rpc_s_);
      // if (err) {
      //   log(ERROR, "rpc_process_event failed")
      //   free(msg_chunk_->msg_data);
      //   free(msg_chunk_);
      //   return err;
      // }
      cur_head_ = static_cast<char*>(cur_head_) + chunk_size;
      data_size_to_send -= chunk_size;
      ++counter;
    }
  }
  // Send back msg to tell other side RDMA operation is finished. Since IB just generates wc on post side.
  struct msg_buf* msg_done_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_done_, ==, 0, err)
  
  msg_done_->size = sizeof(struct rpc_cmd);
  msg_done_->msg_rpc->cmd = peer_rdma_done;
  msg_done_->msg_rpc->id = rpc_s_->ptlmap.id;
  msg_done_->msg_rpc->wr_id = msg_->id;
  log(DEBUG, "will rpc_send peer_rdma_done cmd to peer_id= " << peer_->ptlmap.id)
  err = rpc_send(rpc_s_, peer_, msg_done_);
  if (err < 0) {
    log(ERROR, "rpc_send failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", peer_->ptlmap.id= " << peer_->ptlmap.id)
    free(msg_done_->msg_data);
    free(msg_done_);
    return err;
  }
  
  log(DEBUG, "done; to peer_id= " << peer_->ptlmap.id)
  return 0;
}

// *************************************  send / receive_direct  ******************************** //
int _rpc_send(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  int err;
	if (peer_->rpc_conn.f_connected != 1) {
    log(WARNING, "RPC channel has not been established from rpc_s_->ptlmap.id= "
                 << rpc_s_->ptlmap.id << " to peer_->ptlmap.id= " << peer_->ptlmap.id
                 << ", will try to connect now...")
		return_if_err(rpc_connect(rpc_s_, peer_), err)
	}
  
	struct rpc_request* rr_;
  return_err_if_ret_cond_flag((struct rpc_request*)calloc(1, sizeof(struct rpc_request) ), rr_, ==, 0, -ENOMEM)
	
	rr_->type = 0; // 0 for cmd, 1 for data
	rr_->msg = msg_;
	rr_->iodir = io_send;
	rr_->cb = (async_callback)rpc_cb_req_completion;
	rr_->data = msg_->msg_rpc;
	rr_->size = sizeof(*(msg_->msg_rpc) );
	list_add_tail(patch_ib::to_str<>(peer_->ptlmap.id) + "_req_list", &(rr_->req_entry), &(peer_->req_list) );
  peer_->num_req++;
  peer_->req_posted++;
  
  return_if_err(peer_process_send_list(rpc_s_, peer_), err)
  
  return 0;
}

int rpc_send(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  int err;
  // return_if_err(_rpc_send(rpc_s_, peer_, msg_), err)
  
  uint64_t data_size_to_send = msg_->size;
  if (data_size_to_send <= RDMA_BUFFER_SIZE) {
    return_if_err(_rpc_send(rpc_s_, peer_, msg_), err)
    return 0;
  }
  
  log(DEBUG, "before generating hash; from_id= " << rpc_s_->ptlmap.id << ", to_id= " << peer_->ptlmap.id)
  unsigned int hash = patch_ib::hash_str(
    "from_id=" + patch_ib::to_str<>(rpc_s_->ptlmap.id) + 
    "to_id=" + patch_ib::to_str<>(peer_->ptlmap.id) );
  log(DEBUG, "started; large data_size_to_send= " << data_size_to_send << "\n"
            << "\t to peer_id= " << peer_->ptlmap.id << "\n"
            << "\t hash= " << hash)
  
  struct msg_buf* msg_large_data_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_large_data_, ==, 0, err)
  // msg_large_data_->msg_data = malloc(msg_->size);
  // if (!(msg_large_data_->msg_data) ) {
  //   log(ERROR, "malloc failed; msg_large_data_->size= " << msg_large_data_->size)
  //   free(msg_large_data_);
  //   return err;
  // }
  
  msg_large_data_->size = 0;
  msg_large_data_->msg_data = NULL;
  msg_large_data_->cb = handle_rpc_send;
  msg_large_data_->msg_rpc->cmd = large_data_cmd;
  msg_large_data_->msg_rpc->id = rpc_s_->ptlmap.id;
  // msg_large_data_->id_ = patch_ib::str_to_char_("msg_large_data_");
  struct header_chunk* header_ = (struct header_chunk*)msg_large_data_->msg_rpc->pad;
  header_->hash = hash;
  header_->total_size = msg_->size;
  log(DEBUG, "_rpc_sending _rpc_send large_data_cmd to peer_id= " << peer_->ptlmap.id << "; hash= " << hash)
  return_if_err(_rpc_send(rpc_s_, peer_, msg_large_data_), err, free(msg_large_data_);)
  // err = rpc_process_event(rpc_s_);
  // if (err) {
  //   log(ERROR, "rpc_process_event failed")
  //   free(msg_large_data_);
  //   return err;
  // }
  // 
  void* cur_head_ = msg_->msg_data;
  int counter = 0;
  while (data_size_to_send > 0) {
    int chunk_size = (data_size_to_send > RDMA_BUFFER_SIZE) ? RDMA_BUFFER_SIZE : data_size_to_send;
    void* chunk_ = malloc(chunk_size);
    memcpy(chunk_, cur_head_, chunk_size);
    
    struct msg_buf* msg_chunk_;
    return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_chunk_, ==, 0, err)
    msg_chunk_->size = chunk_size;
    msg_chunk_->msg_data = chunk_;
    msg_chunk_->cb = handle_rpc_send;
    msg_chunk_->msg_rpc->cmd = large_data_chunk_cmd;
    msg_chunk_->msg_rpc->id = rpc_s_->ptlmap.id;
    // msg_chunk_->id_ = patch_ib::str_to_char_("msg_chunk_" + patch_ib::to_str<>(counter) );
    struct header_chunk* header_ = (struct header_chunk*)msg_chunk_->msg_rpc->pad;
    header_->hash = hash;
    header_->total_size = msg_->size;
    header_->head_margin = msg_->size - data_size_to_send;
    header_->size = chunk_size;
    header_->counter = counter;
    log(DEBUG, "_rpc_sending large_data_chunk_cmd to peer_id= " << peer_->ptlmap.id << "; hash= " << hash << "\n"
              << "\t counter= " << counter << ", chunk_size= " << chunk_size)
    return_if_err(_rpc_send(rpc_s_, peer_, msg_chunk_), err, free(msg_chunk_->msg_data); free(msg_chunk_);)
    // err = rpc_process_event(rpc_s_);
    // if (err) {
    //   log(ERROR, "rpc_process_event failed")
    //   free(msg_chunk_->msg_data);
    //   free(msg_chunk_);
    //   return err;
    // }
    
    cur_head_ = static_cast<char*>(cur_head_) + chunk_size;
    data_size_to_send -= chunk_size;
    ++counter;
  }
  return_if_err(rpc_process_event(rpc_s_), err)
  hash__peer_msg_map[hash] = std::make_pair(peer_, msg_);
  
  ib_syncer.add_sync_point(hash, 1);
  pthread_t loop_thread;
  return_err_if_ret_cond_flag(pthread_create(&loop_thread, NULL, rpc_process_event_loop, NULL), err, !=, 0, NULL)
  ib_syncer.wait(hash);
  ib_syncer.del_sync_point(hash);
  // log(DEBUG, "_rpc_sending real msg to peer_id= " << peer_->ptlmap.id)
  // return_if_err(_rpc_send(rpc_s_, peer_, msg_), err)
  pthread_cancel(loop_thread);
  
  log(DEBUG, "done; large data_size_to_send= " << data_size_to_send << "\n"
            << "\t to peer_id= " << peer_->ptlmap.id << "\n" 
            << "\t hash= " << hash)
  return 0;
}

// Called back upon recving ready_for_actual_msg from the recving side
// Reason to send actual msg including large data here rather then right after sending cmd for each 
// large_data_chunk is wr's posted on the sending Q seems to be reordered causing real msg to be recved
// before recver finalizes recving all the chunks
int finalize_rpc_send(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  int err;
  struct header_chunk* header_ = (struct header_chunk*)(cmd_->pad);
  unsigned int hash = header_->hash;
  
  peer_msg_pair peer_msg  = hash__peer_msg_map[hash];
  struct node_id* peer_ = peer_msg.first;
  struct msg_buf* msg_ = peer_msg.second;
  log(DEBUG, "_rpc_sending real msg to peer_id= " << peer_->ptlmap.id)
  return_if_err(_rpc_send(rpc_s_, peer_, msg_), err)
  
  ib_syncer.notify(hash);
  
  return 0;
}

int rpc_fetch_request(struct rpc_server* rpc_s_, const struct node_id* peer_, struct rpc_request* rr_)
{
  int err;
  struct ibv_send_wr wr, *bad_wr_ = NULL;
  struct ibv_sge sge;
  
  // log(INFO, "to be fetched; from peer_id= " << peer_->ptlmap.id << ", size= " << rr_->size)
  if (rr_->type == 1) { // data
    return_if_err(_ibv_reg_mr(rr_->data_mr, peer_->rpc_conn.pd, rr_->data, rr_->size, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_WRITE), err)
    
    memset(&wr, 0, sizeof(wr) );
    wr.wr_id = (uintptr_t)rr_;  // use address of this rr_ as the unique wr_id
    
    wr.opcode = IBV_WR_RDMA_READ;
    wr.sg_list = &sge;
    wr.num_sge = 1;
    wr.send_flags = IBV_SEND_SIGNALED;
    
    wr.wr.rdma.remote_addr = (uintptr_t)rr_->msg->mr.addr;
    wr.wr.rdma.rkey = rr_->msg->mr.rkey;
    
    sge.addr = (uintptr_t)rr_->data;
    sge.length = rr_->size;
    sge.lkey = rr_->data_mr->lkey;
    
    return_err_if_ret_cond_flag(ibv_post_send(peer_->rpc_conn.qp, &wr, &bad_wr_), err, <, 0, err)
  }
  if (rr_) {
    if (rr_->msg)
      rr_->msg->refcont++;
  }
  
  return 0;
}

int _rpc_receive_direct(struct rpc_server* rpc_s_, struct node_id* peer_,
                        struct msg_buf* msg_, bool last)
{
  int err;
	if (peer_->rpc_conn.f_connected != 1) {
	  log(WARNING, "RPC channel has not been established from rpc_s_->ptlmap.id= "
                << rpc_s_->ptlmap.id << " to peer_->ptlmap.id= " << peer_->ptlmap.id
                << ", trying to connect now...")
    return_if_err(rpc_connect(rpc_s_, peer_), err)
	}
  if (msg_->size > RDMA_BUFFER_SIZE) {
	  log(ERROR, "called for msg_->size= " << msg_->size << " > RDMA_BUFFER_SIZE= " << RDMA_BUFFER_SIZE)
	  return 1;
	}
	
  struct rpc_request* rr_;
  return_err_if_ret_cond_flag((struct rpc_request*)calloc(1, sizeof(struct rpc_request) ), rr_, ==, 0, -ENOMEM)
  
  rr_->type = 1; // 0 for cmd, 1 for data
  rr_->msg = msg_;
  if (last)
    rr_->cb = (async_callback)rpc_cb_req_completion;
  else
    rr_->cb = (async_callback)rpc_cb_req_completion_;
  rr_->data = msg_->msg_data;
  rr_->size = msg_->size;
  
  peer_->req_posted++;
  return_if_err(rpc_fetch_request(rpc_s_, peer_, rr_), err)
  return 0;
}

int handle_chunk_recv(struct rpc_server* rpc_s_, struct msg_buf* msg_)
{
  int err;
  struct header_chunk* header_ = (struct header_chunk*)(msg_->msg_rpc->pad);
  unsigned int hash = header_->hash;
  
  uint64_t& left_to_recv_size = hash__left_to_recv_size_map[hash];
  void*& data_ = hash__data_map[hash];
  left_to_recv_size -= msg_->size;
  log(INFO, "hash= " << hash << "\n"
            // << "\t left_to_recv_size= " << (float)left_to_recv_size/1024/1024 << " MB \n"
            << "\t left_to_recv_size= " << left_to_recv_size << "\n"
            // << "\t recved_size= " << (float)msg_->size/1024/1024 << " MB \n"
            << "\t recved_size= " << msg_->size << "\n"
            << "\t head_margin= " << header_->head_margin << "\n"
            << "\t total_size= " << header_->total_size << "\n"
            << "\t counter= " << header_->counter << "\n"
            << "data_= " << patch_ib::arr_to_str<char>(30, (char*)msg_->msg_data) )
  memcpy(static_cast<char*>(data_) + header_->head_margin, msg_->msg_data, msg_->size);
            // << "data_= " << patch_ib::arr_to_str<char>(msg_->size, (char*)msg_->msg_data) )
  if (left_to_recv_size < 0) {
    log(ERROR, "unexpected behavior; left_to_recv_size= " << left_to_recv_size << " < 0")
    return 1;
  }
  else if (left_to_recv_size == 0) {
    log(INFO, "done with large_data_recv; hash= " << hash)
    struct node_id* peer_ = msg_->peer;
    // log(DEBUG, "msg_->to_id= " << msg_->to_id)
    // struct node_id* peer_ = rpc_get_peer(rpc_s_, msg_->to_id);
    // log(DEBUG, "creating msg_finished_...")
    struct msg_buf* msg_finished_;
    return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_finished_, ==, 0, err)
    msg_finished_->size = 0;
    msg_finished_->msg_data = NULL;
    msg_finished_->cb = handle_rpc_send;
    msg_finished_->msg_rpc->cmd = finished_large_data_recv_cmd;
    msg_finished_->msg_rpc->id = rpc_s_->ptlmap.id;
    struct header_chunk* l_header_ = (struct header_chunk*)msg_finished_->msg_rpc->pad;
    l_header_->hash = hash;
    l_header_->total_size = header_->total_size;
    log(DEBUG, "_rpc_send finished_large_data_recv_cmd to peer_id= " << peer_->ptlmap.id << "\n"
               << "\t hash= " << hash)
    err = _rpc_send(rpc_s_, peer_, msg_finished_);
    if (err) {
      log(ERROR, "_rpc_send failed; to peer_id= " << peer_->ptlmap.id << ", msg_finished_= \n" << rpc_to_str("msg_buf", msg_finished_) )
      free(msg_finished_);
      return err;
    }
    return_if_err(rpc_process_event(rpc_s_), err)
  }
  
  return 0;
}

int handle_large_data_chunk_cmd(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  log(DEBUG, "started; from peer_id= " << cmd_->id)
  struct header_chunk* header_ = (struct header_chunk*)(cmd_->pad);
  unsigned int hash = header_->hash;
  if (!hash__data_map.contains(hash) && !hash__left_to_recv_size_map.contains(hash) ) {
    log(ERROR, "hash__data_map does NOT contain hash= " << hash)
    return 1;
  }
  
  int err;
  struct node_id* peer_ = rpc_get_peer(rpc_s_, cmd_->id);
  struct msg_buf* msg_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 0), msg_, ==, 0, err)
  // msg_->to_id = cmd_->id;
  msg_->msg_rpc = cmd_; // TODO: memcpy
  msg_->size = header_->size;
  msg_->msg_data = malloc(msg_->size);
  if (!(msg_->msg_data) ) {
    log(ERROR, "malloc failed; msg_->size= " << msg_->size)
    free(msg_);
    return err;
  }
  msg_->cb = handle_chunk_recv;
  msg_->id = cmd_->wr_id;
  msg_->mr = cmd_->mr;
  log(INFO, "will _rpc_receive_direct chunk from peer_id= " << peer_->ptlmap.id << "\n"
            << "\t chunk_size= " << msg_->size)
  err = _rpc_receive_direct(rpc_s_, peer_, msg_, true);
  if (err < 0) {
    log(ERROR, "_rpc_receive_direct failed; from peer_id= " << peer_->ptlmap.id << ", msg_= \n" << rpc_to_str("msg_buf", msg_) )
    free(msg_->msg_data);
    free(msg_);
    return err;
  }
  
  log(DEBUG, "done; from peer_id= " << cmd_->id)
  return 0;
}

int handle_large_data_cmd(struct rpc_server* rpc_s_, struct rpc_cmd* cmd_)
{
  struct header_chunk* header_ = (struct header_chunk*)(cmd_->pad);
  unsigned int hash = header_->hash;
  if (hash__data_map.contains(hash) && hash__left_to_recv_size_map.contains(hash) ) {
    log(ERROR, "hash__data_map already contains hash= " << hash)
    return 1;
  }
  
  hash__left_to_recv_size_map[hash] = header_->total_size;
  hash__data_map[hash] = malloc(header_->total_size);
  
  log(INFO, "done; from peer_id= " << cmd_->id << ", hash= " << hash << "\n"
            << "\t data_size_to_recv= " << header_->total_size)
  return 0;
}

int rpc_receive_direct(struct rpc_server* rpc_s_, struct node_id* peer_, struct msg_buf* msg_)
{
  int err;
  uint64_t data_size_to_recv = msg_->size;
  log(DEBUG, "started; from peer_id= " << peer_->ptlmap.id
             << ", data_size_to_recv= " << data_size_to_recv)
  if (data_size_to_recv <= RDMA_BUFFER_SIZE) {
    return_if_err(_rpc_receive_direct(rpc_s_, peer_, msg_, true), err)
    // // To make sure this recv operation is completed before return
    // return_if_err(rpc_process_event(rpc_s_), err)
    log(DEBUG, "done; from peer_id= " << peer_->ptlmap.id)
    return 0;
  }
  // else {
  //   log(DEBUG, "started large data recv_direct; from peer_id= " << peer_->ptlmap.id
  //             << ", data_size_to_recv= " << data_size_to_recv)
  //   void* cur_head_ = msg_->msg_data;
  //   int counter = 0;
  //   while (data_size_to_recv > 0) {
  //     int chunk_size = (data_size_to_recv > RDMA_BUFFER_SIZE) ? RDMA_BUFFER_SIZE : data_size_to_recv;
      
  //     struct msg_buf* msg_chunk_;
  //     return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_chunk_, ==, 0, err)
  //     msg_chunk_->size = chunk_size;
  //     msg_chunk_->msg_data = cur_head_;
  //     // msg_chunk_->cb = handle_rpc_send;
  //     msg_chunk_->cb = 0;
  //     msg_chunk_->mr = msg_->mr;
  //     msg_chunk_->mr.addr = static_cast<char*>(msg_->mr.addr) + counter*chunk_size;
  //     log(DEBUG, "will _rpc_receive_direct msg_chunk_; from peer_id= " << peer_->ptlmap.id << "\n"
  //               << "\t counter= " << counter << ", chunk_size= " << chunk_size)
  //     data_size_to_recv -= chunk_size;
  //     if (data_size_to_recv > 0)
  //       err = _rpc_receive_direct(rpc_s_, peer_, msg_chunk_, false);
  //     else
  //       err = _rpc_receive_direct(rpc_s_, peer_, msg_chunk_, true);
  //     if (err) {
  //       log(ERROR, "_rpc_receive_direct failed; from peer_id= " << peer_->ptlmap.id << ", msg_chunk_= \n" << rpc_to_str("msg_buf", msg_chunk_) )
  //       free(msg_chunk_->msg_data);
  //       free(msg_chunk_);
  //       return err;
  //     }
  //     // err = rpc_process_event(rpc_s_);
  //     // if (err) {
  //     //   log(ERROR, "rpc_process_event failed")
  //     //   free(msg_chunk_->msg_data);
  //     //   free(msg_chunk_);
  //     //   return err;
  //     // }
  //     cur_head_ = static_cast<char*>(cur_head_) + chunk_size;
  //     ++counter;
  //   }
  // }
  log(DEBUG, "before generating hash; from_id= " << peer_->ptlmap.id << ", to_id= " << rpc_s_->ptlmap.id)
  unsigned int hash = patch_ib::hash_str(
    "from_id=" + patch_ib::to_str<>(peer_->ptlmap.id) +
    "to_id=" + patch_ib::to_str<>(rpc_s_->ptlmap.id) );
  log(INFO, "large data recv; data_size_to_recv= " << data_size_to_recv << "\n"
            << "\t hash= " << hash)
  if (!hash__data_map.contains(hash) || !hash__left_to_recv_size_map.contains(hash) ) {
    log(ERROR, "hash__data_map does not contain hash= " << hash)
    return 1;
  }
  else if (hash__left_to_recv_size_map[hash] ) {
    log(WARNING, "large data recv should've been finished; hash= " << hash << "\n"
                 << "\t left_to_recv_size= " << hash__left_to_recv_size_map[hash] )
    return 1;
    // Note: syncer won't work since rpc is single-threaded
    // log(INFO, "large data recv is not completed yet, waiting...; hash= " << hash)
    // ib_syncer.add_sync_point(hash, 1);
    // ib_syncer.wait(hash);
    // if (hash__left_to_recv_size_map[hash] ) {
    //   log(ERROR, "ib_syncer is notified but hash__left_to_recv_size_map[" << hash << "]= " << hash__left_to_recv_size_map[hash] )
    //   return 1;
    // }
    // ib_syncer.del_sync_point(hash);
  }
  
  memcpy(msg_->msg_data, hash__data_map[hash], data_size_to_recv);
  free(hash__data_map[hash] );
  hash__data_map.del(hash);
  hash__left_to_recv_size_map.del(hash);
  
  log(INFO, "large data recv done; data_size_to_recv= " << data_size_to_recv << ", hash= " << hash)
  // Send back msg to tell other side RDMA operation is finished. Since IB just generates wc on post side.
  struct msg_buf* msg_done_;
  return_err_if_ret_cond_flag(msg_buf_alloc(rpc_s_, peer_, 1), msg_done_, ==, 0, err)
  
  msg_done_->size = sizeof(struct rpc_cmd);
  msg_done_->msg_rpc->cmd = peer_rdma_done;
  msg_done_->msg_rpc->id = rpc_s_->ptlmap.id;
  msg_done_->msg_rpc->wr_id = msg_->id;
  log(DEBUG, "will rpc_send peer_rdma_done cmd to peer_id= " << peer_->ptlmap.id)
  err = rpc_send(rpc_s_, peer_, msg_done_);
  if (err < 0) {
    log(ERROR, "rpc_send failed; rpc_s_->ptlmap.id= " << rpc_s_->ptlmap.id << ", peer_->ptlmap.id= " << peer_->ptlmap.id)
    free(msg_done_->msg_data);
    free(msg_done_);
    return err;
  }
  return_if_err((*msg_->cb)(rpc_s_, msg_), err)
  
  log(DEBUG, "done; from peer_id= " << peer_->ptlmap.id)
  return 0;
}
