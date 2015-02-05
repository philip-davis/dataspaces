#include "dart_rpc_pami.h"
#include "debug.h"
//#include "mpi.h"

#define ALIGNMENT 128
typedef struct{
	void *data;
	double num;	
} msg_tmp;

static struct{
	enum cmd_type 	rpc_cmd;
	rpc_service	rpc_func;
}rpc_commands[64];

static int num_service = 0;

struct client_data_rpc_receive{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
};

struct client_data_rpc_send{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	struct node_id *peer;
};

struct client_data_rpc_get{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	pami_memregion_t *remote_memregion;
	pami_memregion_t *local_memregion;
	struct node_id *peer;
};

struct client_data_rpc_put{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	pami_memregion_t *remote_memregion;
	pami_memregion_t *local_memregion;
	struct node_id *peer;
};

/* USEFUL FUNCTIONS */
void * safemalloc(size_t n)
{
    int rc = 0;
    void * ptr = NULL;

#ifdef NO_MEMALIGN
    ptr = malloc( n );
#else
    rc = posix_memalign( &ptr , ALIGNMENT , n );
#endif

    if ( ptr == NULL || rc!=0)
    {
        fprintf( stderr , "%ld bytes could not be allocated \n" , (long)n );
        fflush(stderr);
        sleep(1);
        exit(13);
    }

    return ptr;
}

int rpc_barrier(struct rpc_server *rpc_s, void* comm)
{
	if(comm == NULL)
		uloga("error: Not support dspaces_barrier on BGQ, please use MPI_Barrier instead.\n");
	else
		MPI_Barrier(*(MPI_Comm*)comm);
	return 0;
}

void rpc_report_md_usage(struct rpc_server *rpc_s)
{}


/* Completion callback functions */
static void cb_done (void *ctxt, void * clientdata, pami_result_t err)
{
 // printf("in cb_done\n");
  int * active = (int *) clientdata;
  (*active)--;
}

static void rpc_cb_req_completion(void *ctxt, void *clientdata, pami_result_t err)
{
	//uloga("get into rpc_cb_req_completion\n");
	struct client_data_rpc_send *ptr = (struct client_data_rpc_send *)clientdata;
	if(!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	struct node_id *peer = ptr->peer;

	if(!rpc_s || !rr || !peer)
		return;

	//TODO: need rr->msg->refcont==0?
	(*rr->msg->cb)(rpc_s, rr->msg);
	
	//free the rpc_request
	if(rr)
		free(rr);
	rpc_server_dec_reply(rpc_s);

	//TODO free memory resource? ptr->pami_req
	if(ptr)
		free(ptr);

	//rpc_server_dec_reply(rpc_s);
}

static void rpc_cb_req_hasdata_completion(void *ctxt, void *clientdata, pami_result_t err)
{
	//uloga("get into rpc_cb_req_hasdata_completion\n");
	struct client_data_rpc_send *ptr = (struct client_data_rpc_send *)clientdata;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	struct node_id *peer = ptr->peer;

	if(!rpc_s ||!rr ||!peer)
		return;

	list_add_tail(&rr->req_entry, &rpc_s->out_rpc_list);

	if(ptr)
		free(ptr);
}

static int test_equal_memregion(pami_memregion_t *left, pami_memregion_t *right)
{
	int ret = 1;

	//TODO compare the memregion
        int i;
        for(i = 0; i < PAMI_CLIENT_MEMREGION_SIZE_STATIC/sizeof(uintptr_t); i++){
                if((*left)[i] != (*right)[i]){
                        ret = 0;
                        break;
		}
        }

	return ret;
}

/*
 * Handler function for RPC msg rpc_put_finish from remote node(which invokes PAMI-Rput)
 */
static int rpc_service_put_finish(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//uloga("get into rpc_service_put_finish\n");
	struct rpc_request *rr = NULL;
	int flag = 0;
	
	list_for_each_entry(rr, &rpc_s->out_rpc_list, struct rpc_request, req_entry){
		struct rpc_cmd *msg_rpc = rr->msg->msg_rpc;
		if(msg_rpc && test_equal_memregion(&msg_rpc->mem_region, &cmd->mem_region)){
			flag = 1;
			break;
		}
	}

	if(flag && rr){
		list_del(&rr->req_entry);

		pami_result_t err;
		err = PAMI_Memregion_destroy(rpc_s->contexts[0], &(rr->msg->msg_rpc->mem_region));
		if(err != PAMI_SUCCESS)
			uloga("%s(): PAMI_Memregion_destroy failed\n", __func__);
		(*rr->msg->cb)(rpc_s, rr->msg);
		if(rr)
			free(rr);

		rpc_server_dec_reply(rpc_s);
	}
		//rpc_server_dec_reply(rpc_s);
	//uloga("%s() is done\n", __func__);
	return 0;
}
	
/*
 * Handler function for RPC msg rpc_get_finish from remote node(which invokes PAMI-Rget)
 */
static int rpc_service_get_finish(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct rpc_request *rr = NULL;
	int flag = 0;
	
	list_for_each_entry(rr, &rpc_s->out_rpc_list, struct rpc_request, req_entry){
		struct rpc_cmd *msg_rpc = rr->msg->msg_rpc;
		if(msg_rpc && msg_rpc->dstnid == cmd->srcnid
			&& test_equal_memregion(&msg_rpc->mem_region, &cmd->mem_region)){
			flag = 1;
			break;
		}
	}

	if(flag && rr){
		list_del(&rr->req_entry);

		PAMI_Memregion_destroy(rpc_s->contexts[0], &(rr->msg->msg_rpc->mem_region));
		(*rr->msg->cb)(rpc_s, rr->msg);
		if(rr)
			free(rr);

//		rpc_server_dec_reply(rpc_s);
	}
		rpc_server_dec_reply(rpc_s);
	
	return 0;
}

static void rpc_cb_put_completion(void *ctxt, void *clientdata, pami_result_t err)
{
	//uloga("get into rpc_cb_put_completion\n");
	struct client_data_rpc_put *ptr = (struct client_data_rpc_put *)clientdata;
	if(!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	struct node_id *peer = ptr->peer;
	pami_memregion_t *remote_memregion = ptr->remote_memregion;
	pami_memregion_t *local_memregion = ptr->local_memregion;

        if(!rpc_s || !rr || !peer )
                return;

	struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 1);
	if(msg){
		msg->msg_rpc->cmd = rpc_put_finish;
		memcpy(&msg->msg_rpc->mem_region, remote_memregion, sizeof(pami_memregion_t));
		int ret;
		ret = rpc_send(rpc_s, peer, msg);
		if(ret < 0){
			free(msg);
			uloga("%s(): #%u, rpc_send() of RPC msg "
			"rpc_get_finish finish failed\n", __func__, rpc_s->ptlmap.rank_pami);
		}
	}
	//free(msg);

	//destroy PAMI_Memregion_destroy
	err = PAMI_Memregion_destroy(rpc_s->contexts[0], local_memregion);
	if(err != PAMI_SUCCESS){
		uloga("PAMI_Memregion_destroy failed\n");
	}
	(*rr->msg->cb)(rpc_s, rr->msg);
	if(rr)
		free(rr);
	
	rpc_server_dec_reply(rpc_s);

	//free memory resource
	if(remote_memregion)
		free(remote_memregion);
	if(local_memregion)
		free(local_memregion);
	if(ptr)
		free(ptr);
	//uloga("%s() is done\n", __func__);
}

static void rpc_cb_get_completion(void *ctxt, void *clientdata, pami_result_t err)
{
	//printf("get into rpc_cb_get_completion\n");
	struct client_data_rpc_get *ptr = (struct client_data_rpc_get *)clientdata;
	
	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	pami_memregion_t *remote_memregion = ptr->remote_memregion;
	pami_memregion_t *local_memregion = ptr->local_memregion;
	struct node_id *peer = ptr->peer;	

	//=========send notification about data receive complete============
	struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 1);
	if(msg){
		msg->msg_rpc->cmd = rpc_get_finish;
		memcpy(&msg->msg_rpc->mem_region, remote_memregion, sizeof(pami_memregion_t));
		err = rpc_send(rpc_s, peer, msg);
	}
	//free(msg);
	
        PAMI_Memregion_destroy(rpc_s->contexts[0], local_memregion);	
	(*rr->msg->cb)(rpc_s, rr->msg);
	if(rr)
		free(rr);
	
	rpc_server_dec_reply(rpc_s);

        if(remote_memregion)
               free(remote_memregion);
        if(local_memregion)
               free(local_memregion);
        if(ptr)
               free(ptr);

	//check received data
	/*int i, count = 0;
	int size = rr->size/sizeof(int);
	printf("data_size = %d\n", size);
	for(i = 0; i < size; i++){
		if(*((int*)rr->data + i) != i)
			count++;
	}*/	
}

/*for test
static void rpc_cb_req_completion(pami_context_t context, void *cookie, pami_result_t result)
{
	int *active = (int *)cookie;
	(*active)--;
	printf("after msg send to dest\n");
}
*/

static void rpc_cb_recv_done(pami_context_t context, void *cookie, pami_result_t result)
{
	struct client_data_rpc_receive *ptr = (struct client_data_rpc_receive*)cookie;
	if(!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
//	rpc_s->flag--;
//	printf("check after recv: cmd = %d\n", ((struct rpc_cmd*)rr->data)->cmd);	

	//list
	list_add_tail(&rr->req_entry, &rpc_s->rpc_list);

	if(ptr)
		free(ptr);
}


static void rpc_cb_recv(pami_context_t context, void *cookie,
			const void *header_addr, size_t header_size,
			const void *pipe_addr, size_t data_size,
			pami_endpoint_t origin, pami_recv_t *recv)
{
//	printf("get into rpc_cb_recv\n");
/*	if(recv == NULL)
		printf("recv is NULL\n");
	if(pipe_addr == NULL)
		printf("pipe_addr is NULL\n");
*/	//void ** h = (void **)header_addr;
/*	size_t task;
	size_t ctxoff;
	int err;
	err = PAMI_Endpoint_query(origin, &task, &ctxoff);
	//printf("the msg was sent from Rank%d\n", task);

	int *active = (int*)cookie;
	(*active)--;
	printf("cookie=%d\n", *(int*)cookie);
*/
	struct rpc_server *rpc_s = (struct rpc_server*)cookie;
	struct msg_buf *msg = msg_buf_alloc(rpc_s, NULL, 1);
	struct rpc_request *rr = (struct rpc_request*)calloc(1, sizeof(struct rpc_request));
	if(!msg || !rr || !rpc_s)
		goto err_out;
	
	rr->msg = msg;
	rr->iodir = io_receive;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);

	//memcpy(rr->data, (struct rpc_cmd *)header_addr, header_size);
	void **data = (void **)&header_addr;
	*data = rr->data;	

	struct client_data_rpc_receive *ptr_clientdata = (struct client_data_rpc_receive*)calloc(1, sizeof(struct client_data_rpc_receive));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
		
	recv->cookie      = (void*)ptr_clientdata; //0;
    	recv->local_fn    = rpc_cb_recv_done;//NULL;
    	recv->addr        = (void*)header_addr;
    	recv->type        = PAMI_TYPE_BYTE;
    	recv->offset      = 0;
    	recv->data_fn     = NULL; //cb_done; //msg_data_cb;//PAMI_DATA_COPY;
    	recv->data_cookie = NULL;
    	//printf("task = %ld \n", (uint32_t)recv->cookie);
	return;

err_out:
	if(msg) 
		free(msg);
	if(rr)
		free(rr);
	uloga("'%s()': failed with memory allocation\n", __func__);

}

/* Public APIs */

void rpc_server_set_peer_ref(struct rpc_server *rpc_s,
                        struct node_id peer_tab[], int num_peers)
{
        rpc_s->num_peers = num_peers;
        rpc_s->peer_tab = peer_tab;
}

void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff)
{
        rpc_s->num_rpc_per_buff = num_rpc_per_buff;
}

struct rpc_server *rpc_server_init(int num_buff, int num_rpc_per_buff,
				void *dart_ref, enum rpc_component cmp_type)
{
	struct rpc_server *rpc_s = 0;
	size_t size;
	size = sizeof(struct rpc_server);
	rpc_s = calloc(1, size);
	if(!rpc_s)
		return NULL;	
	rpc_s->dart_ref = dart_ref;
	rpc_s->num_buff = num_buff;
	rpc_s->num_rpc_per_buff = num_rpc_per_buff;
	rpc_s->max_num_msg = num_buff;
	rpc_s->cmp_type = cmp_type;
	rpc_s->ptlmap.id = 0;
	rpc_s->flag = 1;


	pami_result_t err;
	char *clientname = "dart";
	pami_client_t client;

	err = PAMI_Client_create(clientname, &client, NULL, 0);
	if(err != PAMI_SUCCESS)
		printf("pami_client_create fails\n");
	
	pami_configuration_t config[3];
	size_t num_contexts;
	
	config[0].name = PAMI_CLIENT_NUM_TASKS;
  	config[1].name = PAMI_CLIENT_TASK_ID;
  	config[2].name = PAMI_CLIENT_NUM_CONTEXTS;
	err = PAMI_Client_query(client, config, 3);
	if(err != PAMI_SUCCESS)
		printf("pami_client_create success\n");
		
	int world_size, world_rank;
	world_size = config[0].value.intval;
	world_rank = config[1].value.intval;
	num_contexts = config[2].value.intval;

//	printf("size=%d, rank=%d, num_contexts=%d\n", world_size, world_rank, num_contexts);
	
	pami_context_t *contexts = (pami_context_t *) safemalloc( num_contexts * sizeof(pami_context_t) );
	err = PAMI_Context_createv( client, NULL, 0, contexts, num_contexts );
	if(err != PAMI_SUCCESS)
		printf("fail-3\n");
	
	rpc_s->client = client;
	rpc_s->ptlmap.rank_pami = world_rank;
	rpc_s->contexts = contexts;
	rpc_s->num_contexts = num_contexts;

	//Init the list for incoming RPC msgs
	INIT_LIST_HEAD(&rpc_s->rpc_list);
	//Init the list for outgoing RPC msgs which have data
	INIT_LIST_HEAD(&rpc_s->out_rpc_list);	

	pami_dispatch_hint_t hints;
	memset(&hints, 0, sizeof(hints));

	pami_dispatch_callback_function dispatch_cb;
	size_t dispatch_id			= 37;	//TODO
	dispatch_cb.p2p				= rpc_cb_recv;
	//pami_dispatch_hint_t dispatch_hint	= {0};
	void* dispatch_cookie			= (void*)rpc_s;
	hints.recv_immediate			= PAMI_HINT_DISABLE;
	err = PAMI_Dispatch_set(contexts[0], dispatch_id, dispatch_cb, dispatch_cookie, hints);
	if(err != PAMI_SUCCESS)
		printf("fail-1\n");
//	err = PAMI_Dispatch_set(contexts[1], dispatch_id, dispatch_cb, &dispatch_cookie, dispatch_hint);
//	if(err != PAMI_SUCCESS)
//		printf("fail-2\n");

	rpc_add_service(rpc_get_finish, rpc_service_get_finish);
	rpc_add_service(rpc_put_finish, rpc_service_put_finish);

	return rpc_s;
}


void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)
{
	rpc_commands[num_service].rpc_cmd = rpc_cmd;
	rpc_commands[num_service].rpc_func = rpc_func;
	num_service++;
}


static int rpc_prepare_buffers(struct rpc_server *rpc_s, const struct node_id *peer,
				struct rpc_request *rr, enum io_dir dir)
{
	struct msg_buf *msg = rr->msg;
	size_t bytes_out;
	pami_result_t err;

	err = PAMI_Memregion_create(rpc_s->contexts[0], msg->msg_data, msg->size, &bytes_out, &(msg->msg_rpc->mem_region));
	msg->msg_rpc->mem_size = msg->size;

	if (err != PAMI_SUCCESS)
		uloga("%s(): PAMI_Memregion_create failed\n", __func__);
	//printf("registered mem size = %zu\n", msg->size);
	
	return 0;	
	
}

static void rpc_decode(struct rpc_server* rpc_s, struct rpc_request *rr)
{
//	printf("get into rpc_decode\n");
	struct rpc_cmd *cmd;
	int err, i;

	cmd = (struct rpc_cmd*)rr->data;
//	printf("cmd = %d\n", cmd->cmd);	

	for(i = 0; i < num_service; i++){
		if(cmd->cmd == rpc_commands[i].rpc_cmd){
			err = rpc_commands[i].rpc_func(rpc_s, cmd);
			return;
		}
	}
        uloga("%s(): Network command unknown %d.\n", __func__,cmd->cmd);
}

struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, 
		const struct node_id *peer, int num_rpcs)
{
	struct msg_buf *msg;
	size_t size;
	
	size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd)*num_rpcs + 7;
	msg = calloc(1, size);
	msg->peer = peer;
	msg->cb = default_completion_callback;
	
	if(num_rpcs > 0){
		msg->msg_rpc = (struct rpc_cmd*)(msg + 1);
		ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
		if(peer){
			msg->msg_rpc->dstnid = peer->ptlmap.rank_pami;
			msg->msg_rpc->dstpid = 0;
			msg->msg_rpc->srcnid = rpc_s->ptlmap.rank_pami;
			msg->msg_rpc->srcpid = 0;
		}
	}
	return msg;
}


static int rpc_post_request(struct rpc_server *rpc_s, struct node_id *peer,
			struct rpc_request *rr, const int msg_has_data)
{
//	printf("get into rpc_post_request\n");
	int err;

	struct client_data_rpc_send *ptr_clientdata = (struct client_data_rpc_send*)calloc(1, sizeof(struct client_data_rpc_send));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->peer = peer;
	
	pami_endpoint_t target_ep;
	int target = peer->ptlmap.rank_pami;	//rank
	err = PAMI_Endpoint_create(rpc_s->client , (pami_task_t)target, 0, &target_ep);
	if(err != PAMI_SUCCESS)
		printf("fail-4\n");
	
	//check before send
/*	struct rpc_cmd *rcmd;
	rcmd = (struct rpc_cmd*)rr->data;
	printf("check before send cmd=%d\n", rcmd->cmd);
	printf("check before send msg_size=%d\n", rr->size);
*/
	pami_send_hint_t dart_null_send_hint;
	memset(&dart_null_send_hint, 0, sizeof(dart_null_send_hint));

	pami_send_t parameters;
	parameters.send.header.iov_base = rr->data; //&value; //rr->data;
	parameters.send.header.iov_len 	= 0; //sizeof(double*); //rr->size;
	parameters.send.data.iov_base	= rr->data;//NULL;	//TODO:if no data
	parameters.send.data.iov_len	= rr->size;//0;
	parameters.send.dispatch	= 37; //dispatch_id;
	parameters.send.hints		= dart_null_send_hint;	
	parameters.send.dest 		= target_ep;
	parameters.events.cookie	= (void*)ptr_clientdata; //TODO argument for local_fn
	parameters.events.local_fn 	= rpc_cb_req_completion; //cb_done; 
	if(msg_has_data){
		parameters.events.local_fn = rpc_cb_req_hasdata_completion;
	}
	parameters.events.remote_fn 	= NULL;//rpc_cb_req_completion;	//TODO	
	
	
//	printf("before PAMI_Send\n");		
	err = PAMI_Send(rpc_s->contexts[0], &parameters);

	if(err != PAMI_SUCCESS){
		printf("rpc_post_request fails\n");
		return -1;
	}

	rpc_server_inc_reply(rpc_s);
	return 0;
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer)
{
	//printf("get into peer_process_send_list\n");
	struct rpc_request *rr;
	int err;
	int has_data = 0;

	//=========for test==============
	//malloc msg, rpc_request
	//put msg into rr
	//===============================
/*	struct msg_buf *msg;
	msg = msg_buf_alloc(rpc_s, peer, 1);
	msg->msg_rpc->cmd = sp_reg_request;
	struct hdr_register *hreg;
	hreg = (struct hdr_register *)msg->msg_rpc->pad;
	hreg->pm_cp = rpc_s->ptlmap;
	hreg->pm_sp = peer->ptlmap; 
	
	rr = calloc(1, sizeof(struct rpc_request));


	rr->msg = msg;
	rr->iodir = io_send;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
*/
	while(!list_empty(&peer->req_list)){
		//printf("not empty\n");
		rr = list_entry(peer->req_list.next, struct rpc_request, req_entry);

		if(rr->msg->msg_data){
			has_data = 1;
			err = rpc_prepare_buffers(rpc_s, peer, rr, rr->iodir);
		}

		rr->msg->msg_rpc->id = rpc_s->ptlmap.id;
		list_del(&rr->req_entry);	

		err = rpc_post_request(rpc_s, peer, rr, has_data);
	
		if(err < 0){
			printf("peer_process_send_list fails\n");
			return err;
		}
	}
	return 0;
}

int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
//	printf("get into rpc_send\n");
	int err;

	//struct msg_buf *msg;
/*	msg = msg_buf_alloc(rpc_s, peer, 1);
	msg->msg_rpc->cmd = sp_reg_request;
	struct hdr_register *hreg;
	hreg = (struct hdr_register *)msg->msg_rpc->pad;
	hreg->pm_cp = rpc_s->ptlmap;
	hreg->pm_sp = peer->ptlmap; 
*/
	struct rpc_request *rr;
	rr = calloc(1, sizeof(struct rpc_request));
	rr->msg = msg;
	rr->iodir = io_send;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);

	list_add(&rr->req_entry, &peer->req_list);

	err = peer_process_send_list(rpc_s, peer);
	if(err < 0){
		printf("rpc_send fails\n");
		return err;
	}
	return 0;
}

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	//printf("get into rpc_receive\n");
	int err;
	struct rpc_request *rr;
	rr = calloc(1, sizeof(struct rpc_request));
	rr->msg = msg;
	rr->iodir = io_receive;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);

	list_add(&rr->req_entry, &peer->req_list);

	err = peer_process_send_list(rpc_s, peer);
	if(err < 0){
		printf("rpc_receive fails\n");
		return err;
	}
	return 0;
}

int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
//int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg, pami_memregion_t *remote_memregion)
{
	//uloga("get into rpc_send_direct\n");
	struct rpc_request *rr;
	int err;

	rr = calloc(1, sizeof(struct rpc_request));
	rr->msg = msg;
	rr->data = msg->msg_data;
	rr->size = msg->size;

	pami_endpoint_t target_ep;
	int target = peer->ptlmap.rank_pami;
	err = PAMI_Endpoint_create(rpc_s->client, (pami_task_t)target, 0, &target_ep);
	if(err != PAMI_SUCCESS)
		printf("fails\n");

	pami_memregion_t *local_memregion = (pami_memregion_t*)safemalloc(sizeof(pami_memregion_t));
	size_t bytes_out;

//	struct timeval t1, t2;
//	gettimeofday(&t1, NULL);
	err = PAMI_Memregion_create(rpc_s->contexts[0], rr->data, rr->size, &bytes_out, local_memregion);
	if(err != PAMI_SUCCESS){
		printf("memregion create failed\n");
	}
/*	gettimeofday(&t2, NULL);
	printf("PAMI_Memregion_create time = %f seconds\n",
             (double) (t2.tv_usec - t1.tv_usec)/1000000 +
             (double) (t2.tv_sec - t1.tv_sec));
*/
	//Put data
	struct client_data_rpc_put *ptr_clientdata = NULL;
	ptr_clientdata = (struct client_data_rpc_put*)calloc(1, sizeof(struct client_data_rpc_put));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->peer = peer;
	ptr_clientdata->remote_memregion = (pami_memregion_t*)calloc(1, sizeof(pami_memregion_t));
	ptr_clientdata->local_memregion = local_memregion;
	memcpy(ptr_clientdata->remote_memregion, peer->cached_remote_memregion, sizeof(pami_memregion_t));
	//if(peer->cached_remote_memregion != remote_memregion)
	//	printf("some error!!!!!!!!!!!!!!!!!!!\n");
	//memcpy(ptr_clientdata->remote_memregion, remote_memregion, sizeof(pami_memregion_t));
	
	pami_send_hint_t dart_rdma_send_hint;
	memset(&dart_rdma_send_hint, 0, sizeof(dart_rdma_send_hint));
	dart_rdma_send_hint.buffer_registered = PAMI_HINT_ENABLE;
	dart_rdma_send_hint.use_rdma = PAMI_HINT_ENABLE;

	pami_rput_simple_t parameters;
	parameters.rma.dest 		= target_ep;
	parameters.rma.hints 		= dart_rdma_send_hint;
	parameters.rma.bytes 		= rr->size;
	parameters.rma.cookie 		= (void*)ptr_clientdata;
	parameters.rma.done_fn 		= rpc_cb_put_completion;
	parameters.rdma.local.mr 	= ptr_clientdata->local_memregion;
	parameters.rdma.local.offset 	= 0;
	parameters.rdma.remote.mr 	= ptr_clientdata->remote_memregion;
	parameters.rdma.remote.offset 	= 0;
	parameters.put.rdone_fn 	= NULL;	//TODO
	
	//printf("before PAMI_Rput\n");
	err = PAMI_Rput(rpc_s->contexts[0], &parameters);
	if(err != PAMI_SUCCESS)
		printf("fail at pami_rput\n");

	rpc_server_inc_reply(rpc_s);

	return 0;	
}

int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	//printf("get into rpc_receive_direct\n");
	struct rpc_request *rr;
	int err;

	rr = calloc(1, sizeof(struct rpc_request));
	rr->msg = msg;
	rr->data = msg->msg_data;
	rr->size = msg->size;

	pami_endpoint_t target_ep;
	int target = peer->ptlmap.rank_pami;	//rank
	err = PAMI_Endpoint_create(rpc_s->client , (pami_task_t)target, 0, &target_ep);
	if(err != PAMI_SUCCESS)
		printf("fail-4\n");

	pami_memregion_t *local_memregion = (pami_memregion_t*)safemalloc(sizeof(pami_memregion_t));
	size_t bytes_out;
	err = PAMI_Memregion_create(rpc_s->contexts[0], rr->data, rr->size, &bytes_out, local_memregion);
	if(err != PAMI_SUCCESS){
		printf("memregion create failed\n");
	}

	//Get remote data
	struct client_data_rpc_get *ptr_clientdata = NULL;
	ptr_clientdata = (struct client_data_rpc_get*)calloc(1, sizeof(struct client_data_rpc_get));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->peer = peer;
	ptr_clientdata->local_memregion = local_memregion;
	ptr_clientdata->remote_memregion = (pami_memregion_t*)calloc(1, sizeof(pami_memregion_t));
	memcpy(ptr_clientdata->remote_memregion, peer->cached_remote_memregion, sizeof(pami_memregion_t));
	
	pami_send_hint_t dart_rdma_send_hint;
	memset(&dart_rdma_send_hint, 0, sizeof(dart_rdma_send_hint));
	dart_rdma_send_hint.buffer_registered = PAMI_HINT_ENABLE;
	dart_rdma_send_hint.use_rdma = PAMI_HINT_ENABLE;

	pami_rget_simple_t parameters;
	parameters.rma.dest 		= target_ep;
	parameters.rma.hints 		= dart_rdma_send_hint;
	parameters.rma.bytes		= rr->size;
	parameters.rma.cookie 		= (void*)ptr_clientdata;//TODO 
	parameters.rma.done_fn 		= rpc_cb_get_completion;
	parameters.rdma.local.mr 	= ptr_clientdata->local_memregion;
	parameters.rdma.local.offset 	= 0;
	parameters.rdma.remote.mr	= ptr_clientdata->remote_memregion;
	parameters.rdma.remote.offset	= 0;
	//parameters.put.rdone_fn		= cb_done;

	//printf("before PAMI_Rget\n");
	err = PAMI_Rget(rpc_s->contexts[0], &parameters);
	if(err != PAMI_SUCCESS)
		printf("fail at pami_rget\n");

	rpc_server_inc_reply(rpc_s);

	return 0;
}

int rpc_read_config(size_t *rank_pami, const char *fname)
{
	//printf("get into rpc_read_config\n");
        FILE *f;
        int err;

       	f = fopen(fname,"rt");
       	if(!f){
               	uloga("%s(): failed.\n", __func__);
                return -1;
	}

	char version[16];

        err = fscanf(f, "PAMIRANK=%llu\n%s\n",rank_pami, version);

	if(strcmp(version, VERSION) != 0)
		printf("Warning: DataSpaces server(s) and client(s) have mis-matched version\n");

        fclose(f);
        if(err==2)
                return 0;

        uloga("'%s()': failed\n", __func__);
        return -EIO;
}

int rpc_write_config(struct rpc_server *rpc_s, const char *fname)
{
	//printf("get int rpc_write_config\n");
        FILE *f;
        int err;

	char tmp_name[20];
	sprintf(tmp_name, "%s.tmp", fname);
	//printf("%s\n", tmp_name);
	
        f = fopen(tmp_name, "wt");
        if (!f)
                goto err_out;

        err = fprintf(f, "PAMIRANK=%u\n%s\n",
                        rpc_s->ptlmap.rank_pami, VERSION);
        if (err < 0)
                goto err_out_close;
        fclose(f);

	err = rename(tmp_name, fname);
	if(err < 0){
		uloga("%s(): failed to rename the config file!\n", __func__);
	}
        return 0;
 err_out_close:
        fclose(f);
 err_out:
//        uloga("'%s()': failed with %d.\n", __func__, err);
        return -1;
}

int rpc_process_event(struct rpc_server *rpc_s)
{
	int err;
	//err = PAMI_Context_trylock_advancev(&(rpc_s->contexts[0]), 1, 1000);	
	err = PAMI_Context_advance(rpc_s->contexts[0], 1);
	if(err != PAMI_SUCCESS)
		printf("fail-5\n");

	struct rpc_request *rr;
	while(!list_empty(&rpc_s->rpc_list)){
		rr = list_entry(rpc_s->rpc_list.next, struct rpc_request, req_entry);
		list_del(&rr->req_entry);
		rpc_decode(rpc_s, rr);
		if(rr){
			(*rr->msg->cb)(rpc_s, rr->msg);
			free(rr);
		}
	}
	return 0;
}

void rpc_server_free(struct rpc_server *rpc_s)
{
	//printf("rank%d num msg posted %d, num msg freed %d\n", rpc_s->ptlmap.rank_pami, rpc_s->num_rep_posted, rpc_s->num_rep_freed);

	while(rpc_s->num_rep_posted != rpc_s->num_rep_freed)
		rpc_process_event(rpc_s);
	//MPI_Barrier(MPI_COMM_WORLD);

	struct rpc_request *rr = NULL, *t;
	list_for_each_entry_safe(rr, t, &rpc_s->out_rpc_list, struct rpc_request, req_entry){
		if(rr){
			list_del(&rr->req_entry);
			PAMI_Memregion_destroy(rpc_s->contexts[0], &(rr->msg->msg_rpc->mem_region));
                        (*rr->msg->cb)(rpc_s,rr->msg);
                        if(rr)
                                free(rr);
                        rpc_server_dec_reply(rpc_s);
		}
	}

	pami_result_t result = PAMI_ERROR;

	result = PAMI_Context_destroyv(rpc_s->contexts, rpc_s->num_contexts);
	if(result != PAMI_SUCCESS){
		printf("PAMI_Context_destroyv fails\n");
	}
	free(rpc_s->contexts);

	PAMI_Client_destroy(&rpc_s->client);
	
	if(rpc_s)
		free(rpc_s);
	//printf("rank%d finished test. \n", rpc_s->ptlmap.rank_pami);
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
	peer->cached_remote_memregion = &cmd->mem_region;
}
