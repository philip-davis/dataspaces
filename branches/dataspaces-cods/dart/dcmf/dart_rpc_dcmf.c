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

#include "dart_rpc_dcmf.h"
#include "debug.h"

struct client_data_rpc_send{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	struct node_id	*peer;
	DCMF_Request_t *dcmf_req;
};

struct client_data_rpc_receive{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	DCMF_Request_t *dcmf_req;
};

struct client_data_sys_send{
	struct rpc_server *rpc_s;
	struct node_id	*peer;
	DCMF_Request_t *dcmf_req;
};

struct client_data_sys_receive{
	struct rpc_server *rpc_s;
	struct sys_request *sys_req;
	DCMF_Request_t *dcmf_req;	
};

struct client_data_rpc_get{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	DCMF_Memregion_t *remote_memregion;
	DCMF_Memregion_t *local_memregion;
	struct node_id	*peer;
	DCMF_Request_t *dcmf_req;
};

struct client_data_rpc_put{
	struct rpc_server *rpc_s;
	struct rpc_request *rr;
	DCMF_Memregion_t *remote_memregion;
	DCMF_Memregion_t *local_memregion;
	struct node_id *peer;
	DCMF_Request_t *dcmf_req;
};

static struct {
	enum cmd_type   rpc_cmd;
	rpc_service     rpc_func;
} rpc_commands[64];

static int num_service = 0;

#define SYS_WAIT_COMPLETION(x)					\
	while (!(x)) {						\
		err = sys_process_event(rpc_s);			\
		if (err < 0)					\
			goto err_out;				\
	}


static int log2_ceil(int n)
{
	unsigned int i;
	int k = -1;

	i = ~(~0U >> 1);
	while (i && !(i & n))
		i = i >> 1;
	if (i != n)
		i = i << 1;

	while (i) {
		k++;
		i = i >> 1;
	}

	return k;
}

/*
Function used to decode received RPC msg, and call corresponding msg handler function
*/
static void rpc_decode(struct rpc_server* rpc_s, struct rpc_request *rr)
{
	struct rpc_cmd *cmd;
	int err,i;

	//Check the validity of the pointers
	if(!rpc_s || !rr)
		return;

	cmd = (struct rpc_cmd*)rr->data;
	
	//Get the credits information for update
	struct hdr_sys ret_hs;
	ret_hs.sys_cmd = sys_msg_ret;
	ret_hs.sys_msg = cmd->num_msg;
	ret_hs.sys_id = cmd->id;
	sys_update_credits(rpc_s, &ret_hs, 1);//Important
	
	for(i=0; i<num_service; i++){
		if(cmd->cmd == rpc_commands[i].rpc_cmd){
			err = rpc_commands[i].rpc_func(rpc_s, cmd);
			return;
		}
	}
	
	uloga("%s(): Network command unknown %d.\n", __func__,cmd->cmd);
}

/*
  Routine  to update  the credits;  routine  called in  response to  a
  remote return credits.
*/
static inline int sys_update_credits(struct rpc_server *rpc_s, struct hdr_sys *hs, int num_return)
{
	struct node_id *peer;
	
	if(rpc_s->peer_tab && hs->sys_cmd == sys_msg_ret && rpc_s->ptlmap.id != hs->sys_id){
		peer = &rpc_s->peer_tab[hs->sys_id];
		peer->num_msg_at_peer += hs->sys_msg;
		peer->num_msg_ret += num_return;
			
		if(peer->f_need_msg && peer->num_msg_ret){
			/*Send back some credits*/
			sys_credits_return(rpc_s, hs);
			peer->f_need_msg = 0;
		}
		
		if(peer->f_req_msg && peer->num_msg_at_peer){
			peer->f_req_msg = 0;
			return peer_process_send_list(rpc_s, peer);
		}
	}
	
	return 0;
}

/*
  Routine  to return  credits  to  a remote  node;  routine called  in
  response to a credits request.
*/
static int sys_credits_return(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
//#ifdef DEBUG
//	uloga("%s(): #%u, from peer hs->sys_id=%d\n",
//		__func__, DCMF_Messager_rank(), hs->sys_id);
//#endif

	struct node_id * to;
	struct hdr_sys ret_hs;
	int err;
	
	to = &rpc_s->peer_tab[hs->sys_id];
	if(to->num_msg_ret == 0){
		/*No credits to return right now*/
		to->f_need_msg = 1;
		return 0;
	}
	
	memset(&ret_hs, 0, sizeof(struct hdr_sys));
	ret_hs.sys_cmd = sys_msg_ret;
	ret_hs.sys_msg = to->num_msg_ret;
	ret_hs.sys_id = rpc_s->ptlmap.id;
	to->num_msg_ret = 0;
	
	err = sys_send(rpc_s, to, &ret_hs);
	if(err == 0)
		return 0;
	
	//recover...
	to->num_msg_ret = ret_hs.sys_msg;
	uloga("%s(): failed\n", __func__);
	return err;
}

#define myid(rpc_s)		(rpc_s->ptlmap.id)
#define rank2id(rpc_s, rank)	((rank) + (rpc_s->app_minid))
#define id2rank(rpc_s, id)	((id) - (rpc_s->app_minid))
#define myrank(rpc_s)		id2rank(rpc_s, myid(rpc_s))

/*
  A remote peer entered the barrier.
*/
static int sys_bar_arrive(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
	/* NOTE: use the rank to  index the 'bat_tab' array. This is a
	   safe way  as the table is  allocated at init  time, and can
	   accomodate differences due to start-up times. */
	//uloga("%s(): #%u, get sys_bar_enter msg\n", __func__, rpc_s->ptlmap.rank_dcmf);
	rpc_s->bar_tab[hs->sys_id] = hs->sys_pad1;

	return 0;
}

static int sys_bar_send(struct rpc_server *rpc_s, int peerid)
{
	struct node_id *peer = &rpc_s->peer_tab[peerid];
	struct hdr_sys hs;
	int err;

	memset(&hs, 0, sizeof(hs));
	hs.sys_cmd = sys_bar_enter;
	hs.sys_pad1 = rpc_s->bar_num;
	hs.sys_id = myrank(rpc_s);

	err = sys_send(rpc_s, peer, &hs);

	return err;
}

/*
  This is the  entry point for processing system  messages.  It can be
  called  from rpc_process_event() to  handle. system events. System messages
  are not subject to flow control.
*/
static int sys_dispatch_event(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
	int err = 0;

	switch (hs->sys_cmd) {
	case sys_none:
		break;

	case sys_msg_ret:
		err = sys_update_credits(rpc_s, hs, 0);
		break;

	case sys_msg_req:
		err = sys_credits_return(rpc_s, hs);
		break;

	case sys_bar_enter:
		err = sys_bar_arrive(rpc_s, hs);
		break;
	}

	return err;
}

static int sys_process_event(struct rpc_server *rpc_s)
{
	DCMF_Messager_advance();
	
	struct sys_request *sys_req;
	while(!list_empty(&rpc_s->sys_list)){
		sys_req = list_entry(rpc_s->sys_list.next, struct sys_request, req_entry);

		list_del(&sys_req->req_entry);//Delete it from list here

		sys_dispatch_event(rpc_s, sys_req->hs);
		
		if(sys_req){
			free(sys_req->hs);
			free(sys_req);
		}
	}
	
	return 0;
}

static void sys_cb_recv_done(void * clientdata, DCMF_Error_t * err_dcmf)
{
	struct client_data_sys_receive * ptr = (struct client_data_sys_receive *)clientdata;
	if(!ptr)
		return;
	
	struct rpc_server *rpc_s = ptr->rpc_s;
	struct sys_request *sys_req = ptr->sys_req;
	
	//Check the validity of the pointers
	if(!rpc_s || !sys_req)
		return;
	
	//Add the new message at the tail of incoming sys list
	list_add_tail(&sys_req->req_entry, &rpc_s->sys_list);
	
	//Free memory
	if(ptr->dcmf_req)
		free(ptr->dcmf_req);
	
	if(ptr)
		free(ptr);	
}

static void rpc_cb_recv_done(void * clientdata, DCMF_Error_t * err_dcmf)
{
	struct client_data_rpc_receive * ptr = (struct client_data_rpc_receive *)clientdata;
	if(!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	DCMF_Request_t *dcmf_req = ptr->dcmf_req;

	//Check the validity of the pointers
	if(!rpc_s || !rr)
		return;
	
	//Add the new message at the tail of incoming rpc list
	list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
	
	//Free memory
	if(dcmf_req)
		free(dcmf_req);
	
	if(ptr)
		free(ptr);
}

/*
Callback function(handler) to invoke when (short) SYS message is received
*/
static void sys_cb_recv_short(void *clientdata, const DCQuad *msginfo, unsigned count,
			size_t peer, const char *src, size_t bytes)
{
	//uloga("%s(): #%u, bytes_size=%u\n", __func__, DCMF_Messager_rank(), bytes);
	struct rpc_server *rpc_s = (struct rpc_server*)clientdata;
	struct hdr_sys *hs = (struct hdr_sys *)calloc(1, sizeof(struct hdr_sys));
	struct sys_request *sys_req = (struct sys_request *)calloc(1, sizeof(struct sys_request));
	sys_req->hs = hs;

	memcpy(sys_req->hs, src, bytes);
	
	//Add the new message at the tail of incoming sys list
	list_add_tail(&sys_req->req_entry, &rpc_s->sys_list);
}

/*
Callback function(handler) to invoke when SYS message is received
*/
static DCMF_Request_t* sys_cb_recv(void *clientdata, const DCQuad *msginfo, unsigned count,
			size_t peer, size_t sndlen, size_t *rcvlen, char** rcvbuf, DCMF_Callback_t *cb_done)
{
	struct rpc_server *rpc_s = (struct rpc_server*)clientdata;
	struct hdr_sys *hs = (struct hdr_sys *)calloc(1, sizeof(struct hdr_sys));
	struct sys_request *sys_req = (struct sys_request *)calloc(1, sizeof(struct sys_request));
	DCMF_Request_t *dcmf_req = (DCMF_Request_t *)calloc(1, sizeof(DCMF_Request_t));
	if(!sys_req || !hs || !rpc_s)
		goto err_out;
	
	sys_req->hs = hs;
	
	*rcvlen = sndlen>0?sndlen:1;
	*rcvbuf = (char*)sys_req->hs;
	struct client_data_sys_receive *ptr_clientdata = (struct client_data_sys_receive*)calloc(1,sizeof(struct client_data_sys_receive));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->sys_req = sys_req;
	ptr_clientdata->dcmf_req = dcmf_req;
	
	cb_done->function = sys_cb_recv_done;
	cb_done->clientdata = (void*)ptr_clientdata;
	
	return dcmf_req;
err_out:
	if(hs)
		free(hs);
	if(sys_req)
		free(sys_req);
	uloga("'%s()': failed with memory allocation\n", __func__);
	return dcmf_req;	
}

/*
Callback function(handler) to invoke when (short) RPC message is received
*/
static void rpc_cb_recv_short(void *clientdata, const DCQuad *msginfo, unsigned count,
			size_t peer, const char *src, size_t bytes)
{
#ifdef DEBUG
	uloga("'%s()': #%u, bytes_size=%u\n", __func__, DCMF_Messager_rank(), bytes);
#endif
}

/*
Callback function(handler) to invoke when RPC message is received
*/
static DCMF_Request_t* rpc_cb_recv(void *clientdata, const DCQuad *msginfo, unsigned count,
			size_t peer, size_t sndlen, size_t *rcvlen, char** rcvbuf, DCMF_Callback_t *cb_done)
{
	struct rpc_server *rpc_s = (struct rpc_server*)clientdata;
	struct msg_buf *msg = msg_buf_alloc(rpc_s, NULL, 1);
	struct rpc_request *rr = (struct rpc_request *)calloc(1, sizeof(struct rpc_request));
	DCMF_Request_t *dcmf_req = (DCMF_Request_t *)calloc(1, sizeof(DCMF_Request_t));
	if(!rr || !msg || !rpc_s)
		goto err_out;

	rr->msg = msg;
	rr->iodir = io_receive;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);

	*rcvlen = sndlen>0?sndlen:1;
	*rcvbuf = rr->data;
	struct client_data_rpc_receive *ptr_clientdata = (struct client_data_rpc_receive*)calloc(1,sizeof(struct client_data_rpc_receive));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->dcmf_req = dcmf_req;

	cb_done->function = rpc_cb_recv_done;
	cb_done->clientdata = (void*)ptr_clientdata;

	return dcmf_req;
err_out:
	if(msg)
		free(msg);
	if(rr)
		free(rr);
	uloga("'%s()': failed with memory allocation\n", __func__);
	return dcmf_req;
}

/*
 Local callback functions(handler) to invoke when DCMF_Get() is complete
*/
static void rpc_cb_get_completion(void *clientdata, DCMF_Error_t *err_dcmf)
{
	struct client_data_rpc_get * ptr =
		(struct client_data_rpc_get *)clientdata;
	if (!ptr) {
		uloga("%s(): ptr to clientdata is NULL.\n", __func__);
		goto err_out;
	}

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	DCMF_Memregion_t *remote_memregion = ptr->remote_memregion;
	DCMF_Memregion_t *local_memregion = ptr->local_memregion;
	struct node_id *peer = ptr->peer;
	
	if (!rpc_s || !rr || !peer ) {
		uloga("%s(): NULL value for rpc_s or rr or peer.\n", __func__);
		return;
	}
	
	rr->msg->refcont--;

	if (rr->msg->refcont == 0){
		/*Notify remote peer with RPC msg rpc_get_finish*/
		struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 1);
		int err;
		if (msg){
			msg->msg_rpc->cmd = rpc_get_finish;
			memcpy(&msg->msg_rpc->mem_region,
				remote_memregion,
				sizeof(DCMF_Memregion_t));
			err = rpc_send(rpc_s, peer, msg);
			if (err<0) {
				free(msg);
				uloga("%s(): #%u, rpc_send() of RPC msg "
					"rpg_get_finish failed\n",
					__func__, rpc_s->ptlmap.rank_dcmf);
			}
		}
	
		/* After a memory region is destroyed it is again legal to 
		write into or deallocate the memory region */
		DCMF_Memregion_destroy(local_memregion);
		(*rr->msg->cb)(rpc_s, rr->msg);
		if(rr)
			free(rr);
		rpc_server_dec_reply(rpc_s);
	}
	
	//Free other memory resource
	if(remote_memregion)
		free(remote_memregion);
	if(local_memregion)
		free(local_memregion);
	if(ptr->dcmf_req)
		free(ptr->dcmf_req);
	if(ptr)
		free(ptr);
err_out:
	return;
}

/*
 Local callback functions to invoke when DCMF_Put() is complete
*/
static void rpc_cb_put_completion(void *clientdata, DCMF_Error_t *err_dcmf)
{
	struct client_data_rpc_put * ptr = (struct client_data_rpc_put *)clientdata;
	if(!ptr)
		return;
		
	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	DCMF_Memregion_t *remote_memregion = ptr->remote_memregion;
	DCMF_Memregion_t *local_memregion = ptr->local_memregion;
	struct node_id *peer = ptr->peer;
	
	if(!rpc_s || !rr || !peer )
		return;
	
	rr->msg->refcont--;

	if(rr->msg->refcont == 0){
		/* Notify remote peer with RPC msg rpc_put_finish*/
		struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 1);
		int err;
		if(msg){
			msg->msg_rpc->cmd = rpc_put_finish;
			memcpy(&msg->msg_rpc->mem_region,
				remote_memregion,
				sizeof(DCMF_Memregion_t));
			err = rpc_send(rpc_s, peer, msg);
			if(err<0){
				free(msg);
				uloga("%s(): #%u, rpc_send() of RPC msg "
					"rpg_get_finish failed\n",
					__func__, rpc_s->ptlmap.rank_dcmf);
			}
		}
	
		/* After a memory region is destroyed it is again legal to 
		write into or deallocate the memory region*/
		DCMF_Memregion_destroy(local_memregion);
		(*rr->msg->cb)(rpc_s, rr->msg);
		if(rr)
			free(rr);
		rpc_server_dec_reply(rpc_s);
	}
	
	//Free other memory resource
	if(remote_memregion)
		free(remote_memregion);
	if(local_memregion)
		free(local_memregion);
	if(ptr->dcmf_req)
		free(ptr->dcmf_req);
	if(ptr)
		free(ptr);
}

/*
Callback function(handler) to invoke when DCMF_Send() for SYS msg is complete
*/
static void sys_cb_req_completion(void * clientdata, DCMF_Error_t *err_dcmf)
{
	struct client_data_sys_send *ptr = (struct client_data_sys_send *)clientdata;
	if(!ptr)
		return;
	
	struct rpc_server *rpc_s = ptr->rpc_s;
	struct node_id *peer = ptr->peer;
	
	if(!rpc_s || !peer)
		return;
	
	//Free the memory resource
	if(ptr->dcmf_req)
		free(ptr->dcmf_req);
	if(ptr)
		free(ptr);
}

/*
  Local callback function(handler) to invoke when DCMF_Send() for RPC msg is complete
*/
static void rpc_cb_req_completion(void * clientdata, DCMF_Error_t * err_dcmf)
{
	struct client_data_rpc_send * ptr = (struct client_data_rpc_send *)clientdata;
	if (!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	struct node_id *peer = ptr->peer;

	if (!rpc_s || !rr || !peer)
		return;

	rr->msg->refcont--;

	if (rr->msg->refcont == 0){
		//free the msg_buf
		(*rr->msg->cb)(rpc_s,rr->msg);
		//free the rpc_request
		if (rr)
			free(rr);
		rpc_server_dec_reply(rpc_s);
	}
	
	//Free the memory resource
	if (ptr->dcmf_req)
		free(ptr->dcmf_req);

	if (ptr)
		free(ptr);
}

/*
  Local callback function(handler) to invoke when DCMF_Send() for RPC msg(has data) is complete
*/
static void rpc_cb_req_hasdata_completion(void * clientdata, DCMF_Error_t * err_dcmf)
{
	struct client_data_rpc_send * ptr = (struct client_data_rpc_send *)clientdata;
	if (!ptr)
		return;

	struct rpc_server *rpc_s = ptr->rpc_s;
	struct rpc_request *rr = ptr->rr;
	struct node_id *peer = ptr->peer;
	
	if (!rpc_s || !rr || !peer)
		return;
	
	// Put the rpc_request into the peer's outgoing buffer for data objects
	list_add_tail(&rr->req_entry, &rpc_s->out_rpc_list);
	
	// Free the memory resource
	if (ptr->dcmf_req)
		free(ptr->dcmf_req);

	if (ptr)
		free(ptr);
}

/*
  Routine to send/post a RPC message to remote peer.
*/
static int rpc_post_request(struct rpc_server *rpc_s, struct node_id *peer,
			struct rpc_request *rr, const int msg_has_data)
{
	DCQuad msginfo;
	DCMF_Request_t *request = (DCMF_Request_t*)calloc(1, sizeof(DCMF_Request_t));
	struct client_data_rpc_send *ptr_clientdata = (struct client_data_rpc_send*)calloc(1,sizeof(struct client_data_rpc_send));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->peer = peer;
	ptr_clientdata->dcmf_req = request;	

	// Is that safe to use calloc() and ptr_clientdata, do remember to free the memory
	DCMF_Callback_t cb_send_done;
	cb_send_done.clientdata = (void*)ptr_clientdata;
	if (msg_has_data) {
		cb_send_done.function = rpc_cb_req_hasdata_completion;
	} else {
		cb_send_done.function = rpc_cb_req_completion;
	}
	
	// Send rpc message to remote peer node
	DCMF_Result ret = DCMF_Send(&rpc_s->rpc_send_protocol_dcmf,
				request,
				cb_send_done,
				DCMF_MATCH_CONSISTENCY,
				peer->ptlmap.rank_dcmf,
				rr->size,
				rr->data,
				&msginfo,
				1);
	if (ret != DCMF_SUCCESS)
		goto err_out;
	
	// After posting the rpc request message
	rr->msg->refcont++;
	rpc_server_inc_reply(rpc_s);
	
	return 0;
err_out:
	if (ptr_clientdata)
		free(ptr_clientdata); //release memory allocated
	uloga("%s(): failed with %d.\n", __func__, ret);
	return -EIO;
}

/*
Create DCMF memory region for asynchronous data transfer
*/
static int rpc_prepare_buffers(struct rpc_server *rpc_s, const struct node_id *peer,
				struct rpc_request *rr, enum io_dir dir)
{
	struct msg_buf *msg = rr->msg;
	size_t bytes_out;
	DCMF_Result ret = DCMF_Memregion_create(
		&msg->msg_rpc->mem_region, 
		&bytes_out,
		msg->size,
		msg->msg_data,
		0);
	msg->msg_rpc->mem_size = msg->size;//Important
	
	if (ret != DCMF_SUCCESS)
	{
		uloga("%s(): DCMF_Memregion_create() failed with %d\n",
			__func__, ret);
		goto err_out;
	}

#ifdef DEBUG
	if (bytes_out != msg->size) {
		uloga("%s(): bytes_out=%u, msg->size=%u\n",
			__func__, bytes_out, msg->size);
	}
#endif

	return 0;
err_out:
	return -1;
}

/*
Generic routine to send a system message
*/
static int sys_send(struct rpc_server *rpc_s, struct node_id *peer, struct hdr_sys *hs)
{
	DCQuad msginfo;
	DCMF_Request_t *request = (DCMF_Request_t *)calloc(1,sizeof(DCMF_Request_t));
	struct client_data_sys_send *ptr_clientdata = (struct client_data_sys_send*)calloc(1,sizeof(struct client_data_sys_send));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->peer = peer;
	ptr_clientdata->dcmf_req = request;

	//Is that safe to use calloc() and ptr_clientdata, do remember to free the memory
	DCMF_Callback_t cb_send_done = {sys_cb_req_completion, (void*)ptr_clientdata};
	
	//Send rpc message to remote peer node
	DCMF_Result ret = DCMF_Send(&rpc_s->sys_send_protocol_dcmf,
				request,
				cb_send_done,
				DCMF_MATCH_CONSISTENCY,
				peer->ptlmap.rank_dcmf,
				sizeof(struct hdr_sys),
				(char*)hs,
				&msginfo,
				1);
	if(ret != DCMF_SUCCESS)
		goto err_out;
	
	return 0;
err_out:
	if(ptr_clientdata)
		free(ptr_clientdata); //release memory allocated
	uloga("'%s()': failed with %d.\n", __func__, ret);
	return -1;
}

/*
Routine to send a request for credits to a remote node;
routine is called by local process
*/
static int sys_credits_request(struct rpc_server *rpc_s, struct node_id *to)
{
	struct hdr_sys req_hs;
	int err;
	
	memset(&req_hs, 0, sizeof(req_hs));
	req_hs.sys_cmd = sys_msg_req;
	req_hs.sys_id = rpc_s->ptlmap.id;
	
	err = sys_send(rpc_s, to, &req_hs);
	if(err == 0)
		return 0;
	
	return err;
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer)
{
	struct rpc_request *rr;
	int err;
	
	while(!list_empty(&peer->req_list)){
		/*
		if(peer->num_msg_at_peer == 0){
			//request for credits
			if(!peer->f_req_msg){
				err = sys_credits_request(rpc_s, peer);
				if(err<0)
					goto err_out;
				peer->f_req_msg = 1;		
			}
		
			//err = sys_process_event(rpc_s);
			//if(err < 0)
			//	goto err_out;
		
			break;
		}
		*/
		rr = list_entry(peer->req_list.next, struct rpc_request, req_entry);
		
		int has_data = 0; //To distinguish if the RPC message has associated data
		if(rr->msg->msg_data){
			has_data = 1;
			//Prepare memory region for remote node to access
			err = rpc_prepare_buffers(rpc_s, peer, rr, rr->iodir);
			if(err<0)
				goto err_out;
		}
		
		//Retrun the credits we have for remote peer
		int num_msg_ret = 0;
		num_msg_ret = rr->msg->msg_rpc->num_msg = peer->num_msg_ret;
		rr->msg->msg_rpc->id = rpc_s->ptlmap.id;
		
		list_del(&rr->req_entry);//This operation must be put here! Before rpc_post_request
		err = rpc_post_request(rpc_s, peer, rr, has_data);
		if(err<0)
			goto err_out;
		
		//Message was sent, consuming one credit
		peer->num_msg_at_peer--;
		peer->num_msg_ret -= num_msg_ret;
	}
	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int test_equal_memregion(DCMF_Memregion_t * left, DCMF_Memregion_t * right)
{
	int ret = 1;
	int i;	
	for(i=0; i<DCMF_MEMREGION_NQUADS; i++){
		if((*left)[i].w0 != (*right)[i].w0){
			ret = 0;
			break;
		}
		if((*left)[i].w1 != (*right)[i].w1){
			ret = 0;
			break;
		}
		if((*left)[i].w2 != (*right)[i].w2){
			ret = 0;
			break;
		}
		if((*left)[i].w3 != (*right)[i].w3){
			ret = 0;
			break;
		}
	}

	return ret;
}

/*
  Handler function for RPC msg rpc_get_finish from remote peer.
*/
static int rpc_service_get_finish(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct rpc_request *rr = NULL;
	int flag = 0;

	// Search for the memory region
	list_for_each_entry(rr, &rpc_s->out_rpc_list, struct rpc_request, req_entry){
		struct rpc_cmd	*msg_rpc = rr->msg->msg_rpc;
		if (msg_rpc && test_equal_memregion(
				 &msg_rpc->mem_region, &cmd->mem_region)) {
			flag = 1;
			break;
		}
	}
	
	if (flag && rr) {
		list_del(&rr->req_entry);
		rr->msg->refcont--;

		if (rr->msg->refcont == 0){
			DCMF_Memregion_destroy(&(rr->msg->msg_rpc->mem_region));
			// Invoke the callback function associated with the rr.
			(*rr->msg->cb)(rpc_s,rr->msg);
			if (rr)
				free(rr);
			rpc_server_dec_reply(rpc_s);
		}
	}
	
	return 0;
}

/*
  Handler function for RPC msg rpc_put_finish from remote node.
*/
static int rpc_service_put_finish(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct rpc_request *rr = NULL;
	int flag = 0;

	//Search for the memory region
	list_for_each_entry(rr, &rpc_s->out_rpc_list, struct rpc_request, req_entry) {
		struct rpc_cmd	*msg_rpc = rr->msg->msg_rpc;
		if (msg_rpc && test_equal_memregion(
				 &msg_rpc->mem_region, &cmd->mem_region)) {
			flag = 1;
			break;
		}
	}
	
	if (flag && rr) {
		list_del(&rr->req_entry);
		rr->msg->refcont--;

		if (rr->msg->refcont == 0) {
			DCMF_Memregion_destroy(&(rr->msg->msg_rpc->mem_region));
			// Invoke the callback function associated with the rr.
			(*rr->msg->cb)(rpc_s,rr->msg);
			if (rr)
				free(rr);
			rpc_server_dec_reply(rpc_s);
		}
	}
	
	return 0;
}

int rpc_read_config(size_t *rank_dcmf, const char *fname)
{
	FILE *f;
	int err;
	
	f = fopen(fname,"rt");
	if(!f){
		uloga("%s(): failed.\n", __func__);
		return -1;
	}
	
	err = fscanf(f, "DCMFRANK=%u\n",rank_dcmf);
	fclose(f);
	if(err==1)
		return 0;
	
	uloga("'%s()': failed\n", __func__);
	return -EIO;
}

int rpc_write_config(struct rpc_server *rpc_s, const char *fname)
{
	FILE *f;
	int err;

	f = fopen(fname, "wt");
	if (!f)
		goto err_out;

	err = fprintf(f, "DCMFRANK=%u\n", 
					rpc_s->ptlmap.rank_dcmf);
	if (err < 0)
		goto err_out_close;
	fclose(f);

	return 0;
 err_out_close:
	fclose(f);
 err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return -1;
}

int rpc_process_event(struct rpc_server *rpc_s)
{
	DCMF_Messager_advance();
	
	struct sys_request *sys_req;
	while(!list_empty(&rpc_s->sys_list)){
		sys_req = list_entry(rpc_s->sys_list.next, struct sys_request, req_entry);

		list_del(&sys_req->req_entry);//Delete it from list here

		sys_dispatch_event(rpc_s, sys_req->hs);
		
		if(sys_req){
			free(sys_req->hs);
			free(sys_req);
		}
	}
	
	struct rpc_request *rr;
	while(!list_empty(&rpc_s->rpc_list)){
		//Get the first rpc msg in the list
		rr = list_entry(rpc_s->rpc_list.next, struct rpc_request, req_entry);

		list_del(&rr->req_entry);
		rpc_decode(rpc_s, rr);

		//Free the memory of rr
		if(rr){
			(*rr->msg->cb)(rpc_s,rr->msg);
			free(rr);
		}
	}
	
	return 0;
}

struct rpc_server *rpc_server_init(int num_buff, int num_rpc_per_buff, 
				void *dart_ref, enum rpc_component cmp_type)
{
	struct rpc_server *rpc_s = 0;
	struct rpc_request *rr;
	struct msg_buf *msg;
	size_t size, size_b;
	int i,err;
	
	size = sizeof(struct rpc_server);
	rpc_s = calloc(1,size);
	if(!rpc_s)
		return NULL;
	
	rpc_s->dart_ref = dart_ref;
	rpc_s->num_buff = num_buff;
	rpc_s->num_rpc_per_buff = num_rpc_per_buff;
	rpc_s->max_num_msg = num_buff;
	rpc_s->cmp_type = cmp_type;
	rpc_s->ptlmap.id = 0;

	//Init the list for incoming RPC msgs
	INIT_LIST_HEAD(&rpc_s->rpc_list);
	//Init the list for outgoing RPC msgs which have data
	INIT_LIST_HEAD(&rpc_s->out_rpc_list);
	
	//Init the list for incoming SYS msgs
	INIT_LIST_HEAD(&rpc_s->sys_list);

	DCMF_Messager_initialize();
	rpc_s->ptlmap.rank_dcmf = DCMF_Messager_rank();
       
	DCMF_Result ret;
	//Register DCMF send configuration(for RPC messages)
	rpc_s->rpc_send_config_dcmf.cb_recv_short = rpc_cb_recv_short;
	rpc_s->rpc_send_config_dcmf.cb_recv_short_clientdata = (void*) rpc_s;
	rpc_s->rpc_send_config_dcmf.cb_recv = rpc_cb_recv;
	rpc_s->rpc_send_config_dcmf.cb_recv_clientdata = (void*) rpc_s;
	rpc_s->rpc_send_config_dcmf.protocol = DCMF_DEFAULT_SEND_PROTOCOL;
	rpc_s->rpc_send_config_dcmf.network = DCMF_DEFAULT_NETWORK;
	
	ret = DCMF_Send_register(&(rpc_s->rpc_send_protocol_dcmf), &(rpc_s->rpc_send_config_dcmf));
	if(ret != DCMF_SUCCESS)
	{
		uloga("'%s()': DCMF_Send_register() failed with %d\n", __func__, ret);
		goto err_out_free;
	}
	
	//Register DCMF send configuration(for SYS messages)
	rpc_s->sys_send_config_dcmf.cb_recv_short = sys_cb_recv_short;
	rpc_s->sys_send_config_dcmf.cb_recv_short_clientdata = (void*) rpc_s;
	rpc_s->sys_send_config_dcmf.cb_recv = sys_cb_recv;
	rpc_s->sys_send_config_dcmf.cb_recv_clientdata = (void*) rpc_s;
	rpc_s->sys_send_config_dcmf.protocol = DCMF_DEFAULT_SEND_PROTOCOL;
	rpc_s->sys_send_config_dcmf.network = DCMF_DEFAULT_NETWORK;
	
	ret = DCMF_Send_register(&(rpc_s->sys_send_protocol_dcmf), &(rpc_s->sys_send_config_dcmf));
	if(ret != DCMF_SUCCESS)
	{
		uloga("'%s()': DCMF_Send_register() failed with %d\n", __func__, ret);
		goto err_out_free;
	}
	
	//Register DCMF get protocols
	rpc_s->rpc_get_config_dcmf.protocol = DCMF_DEFAULT_GET_PROTOCOL;
	rpc_s->rpc_get_config_dcmf.network = DCMF_DEFAULT_NETWORK;
	
	ret = DCMF_Get_register(&rpc_s->rpc_get_protocol_dcmf, &rpc_s->rpc_get_config_dcmf);
	if(ret != DCMF_SUCCESS){
		uloga("%s(): DCMF_Get_register() failed with %d\n", __func__, ret);
		goto err_out_free;
	}
	
	//Register DCMF put protocols
	rpc_s->rpc_put_config_dcmf.protocol = DCMF_DEFAULT_PUT_PROTOCOL;
	rpc_s->rpc_put_config_dcmf.network = DCMF_DEFAULT_NETWORK;
	
	ret = DCMF_Put_register(&rpc_s->rpc_put_protocol_dcmf, &rpc_s->rpc_put_config_dcmf);
	if(ret != DCMF_SUCCESS){
		uloga("%s(): DCMF_Put_register() failed with %d\n", __func__, ret);
		goto err_out_free;
	}
	
	//Add RPC service routines for corresponding messages
	rpc_add_service(rpc_get_finish, rpc_service_get_finish);
	rpc_add_service(rpc_put_finish, rpc_service_put_finish);
        
	rpc_s->bar_tab = malloc(sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);
	if(!rpc_s->bar_tab)
		goto err_out_free;
	memset(rpc_s->bar_tab, 0, sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);
	
	return rpc_s;
err_out_free:
	if(rpc_s)
		free(rpc_s);
        if (err > 0)
                uloga("'%s()': failed with %d\n", __func__, err);
        return 0;
}

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

/*
  System barrier implementation.
*/
int rpc_barrier(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	int round, np;
	int next, prev;
        int err;

	np = log2_ceil(rpc_s->app_num_peers);
	round = -1;

	rpc_s->bar_num = (rpc_s->bar_num + 1) & 0xFF;

	//uloga("'%s()': #%u, barrier number %d.\n", 
	//	__func__, rpc_s->ptlmap.rank_dcmf, rpc_s->bar_num);

	while (round < np-1) {

		round = round + 1;

		next = (myrank(rpc_s) + (1 << round)) % rpc_s->app_num_peers;
		prev = (rpc_s->app_num_peers + 
			myrank(rpc_s) - (1 << round)) % rpc_s->app_num_peers;

		err = sys_bar_send(rpc_s, rank2id(rpc_s, next));
		if (err < 0)
			goto err_out;

		SYS_WAIT_COMPLETION(rpc_s->bar_tab[prev] == rpc_s->bar_num || 
			rpc_s->bar_tab[prev] == ((rpc_s->bar_num+1) & 0xFF));
	}

	return 0;
 err_out:
        ERROR_TRACE();
}

/* 
   Generic interface to send an rpc message to a remote node; peer is
   subject to flow control.
*/
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;
	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;
	
	rr->msg = msg;
	rr->iodir = io_send;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
	
	list_add(&rr->req_entry, &peer->req_list);
	
	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;
	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;
	
	rr->msg = msg;
	rr->iodir = io_receive;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
	
	list_add(&rr->req_entry, &peer->req_list);
	
	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}


struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, 
        const struct node_id *peer, int num_rpcs)
{
	struct msg_buf *msg;
	size_t size;
	
	size = sizeof(struct msg_buf)+sizeof(struct rpc_cmd)*num_rpcs+7;
	msg = calloc(1,size);
	if(!msg){
		uloga("%s(): failed\n", __func__);
		return NULL;
	}
	
	msg->peer = peer;
	msg->cb = default_completion_callback;
	if(num_rpcs > 0){
		msg->msg_rpc = (struct rpc_cmd*)(msg+1);
		ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
		if(peer){
			msg->msg_rpc->dstnid = peer->ptlmap.rank_dcmf;
			msg->msg_rpc->dstpid = 0;
			msg->msg_rpc->srcnid = rpc_s->ptlmap.rank_dcmf;
			msg->msg_rpc->srcpid = 0;
		}
	}
	
	return msg;
}

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)
{
	rpc_commands[num_service].rpc_cmd = rpc_cmd;
	rpc_commands[num_service].rpc_func = rpc_func;
	num_service++;
}

void rpc_server_free(struct rpc_server *rpc_s)
{
	//free the out_rpc_list
	struct rpc_request *rr = NULL, *t;
	list_for_each_entry_safe(rr, t, &rpc_s->out_rpc_list, struct rpc_request, req_entry){
		if(rr){
			list_del(&rr->req_entry);//remove from the list before free memory of rr!!

			rr->msg->refcont--;
			/*After a memory region is destroyed it is again legal to 
			write into or deallocate the memory used by the memory region object*/
			DCMF_Memregion_destroy(&(rr->msg->msg_rpc->mem_region));
			//free the msg_buf
			(*rr->msg->cb)(rpc_s,rr->msg);
			//free the rpc_request
			if(rr)
				free(rr);
			rpc_server_dec_reply(rpc_s);
		}
	}

	// TODO(fan): Why DCMF finalize function would stuck in some cases?
	//DCMF_Messager_finalize();
	
	if(rpc_s->bar_tab)
		free(rpc_s->bar_tab);
	
	if(rpc_s)
		free(rpc_s);

#ifdef DEBUG
	uloga("'%s()': OK, bye.\n",__func__);
#endif
}

int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;
	DCMF_Result ret;
	struct client_data_rpc_get *ptr_clientdata = NULL;
	
	rr = calloc(1, sizeof(struct rpc_request));
	if (!rr)
		goto err_out;
	
	rr->msg = msg;
	rr->data = msg->msg_data;
	rr->size = msg->size;
	
	// Create local memory region for receiving data
	DCMF_Memregion_t *local_memregion =
		(DCMF_Memregion_t*)calloc(1, sizeof(DCMF_Memregion_t));
	size_t bytes_out;
	ret = DCMF_Memregion_create(
				local_memregion,
				&bytes_out,
				rr->size,
				rr->data,
				0);
	
	if (ret != DCMF_SUCCESS)
	{
		uloga("%s(): DCMF_Memregion_create() failed with %d\n",
			__func__, ret);
		goto err_out;
	}

	DCMF_Request_t *request =
		(DCMF_Request_t*)calloc(1,sizeof(DCMF_Request_t));

	// Get remote data
	ptr_clientdata = (struct client_data_rpc_get*)calloc(1,sizeof(struct client_data_rpc_get));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->remote_memregion =
		(DCMF_Memregion_t*)calloc(1, sizeof(DCMF_Memregion_t));
	memcpy(ptr_clientdata->remote_memregion, 
	       peer->cached_remote_memregion, sizeof(DCMF_Memregion_t));
	ptr_clientdata->local_memregion = local_memregion;
	ptr_clientdata->peer = peer;
	ptr_clientdata->dcmf_req = request;
	
	DCMF_Callback_t cb_get_done =
		{rpc_cb_get_completion, (void*)ptr_clientdata};
	ret = DCMF_Get(&rpc_s->rpc_get_protocol_dcmf,
			request,
			cb_get_done,
			DCMF_MATCH_CONSISTENCY,
			peer->ptlmap.rank_dcmf,
			rr->size,
			ptr_clientdata->remote_memregion,
			ptr_clientdata->local_memregion,
			0,
			0);
	if(ret != DCMF_SUCCESS){
		uloga("%s(): DCMF_Get() failed with %d\n", __func__, ret);
		goto err_out;
	}
	
	//After posting the rpc request message
	rr->msg->refcont++;
	rpc_server_inc_reply(rpc_s);
	
	return 0;
err_out:
	if(rr)
		free(rr);
	if(ptr_clientdata)
		free(ptr_clientdata); //release memory allocated
	uloga("%s(): failed with %d\n", __func__, ret);
	return -1;
}


int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {
	uloga("%s(): Not supported in DCMF version!\n", __func__);
	return -1;
}

int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;
	DCMF_Result ret;
	struct client_data_rpc_put *ptr_clientdata = NULL;
	
	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;
	
	rr->msg = msg;
	rr->data = msg->msg_data;
	rr->size = msg->size;
	
	//Create local memory region for sending data
	DCMF_Memregion_t *local_memregion = (DCMF_Memregion_t*)calloc(1, sizeof(DCMF_Memregion_t));
	size_t bytes_out;
	ret = DCMF_Memregion_create(local_memregion, &bytes_out,
				rr->size, rr->data, 0);
	
	if(ret != DCMF_SUCCESS)
	{
		uloga("%s(): DCMF_Memregion_create() failed with %d\n",__func__, ret);
		goto err_out;
	}

	DCMF_Request_t *request = (DCMF_Request_t*)calloc(1,sizeof(DCMF_Request_t));
	//Put data
	ptr_clientdata = (struct client_data_rpc_put*)calloc(1,sizeof(struct client_data_rpc_put));
	ptr_clientdata->rpc_s = rpc_s;
	ptr_clientdata->rr = rr;
	ptr_clientdata->remote_memregion = (DCMF_Memregion_t*)calloc(1, sizeof(DCMF_Memregion_t));
	memcpy(ptr_clientdata->remote_memregion, 
	       peer->cached_remote_memregion, sizeof(DCMF_Memregion_t));
	ptr_clientdata->local_memregion = local_memregion;
	ptr_clientdata->peer = peer;
	ptr_clientdata->dcmf_req = request;
	
	DCMF_Callback_t cb_ack = {rpc_cb_put_completion, (void*)ptr_clientdata};
	DCMF_Callback_t cb_put_done = {NULL, (void*)NULL};
	ret = DCMF_Put(&rpc_s->rpc_put_protocol_dcmf,
			request,
			cb_put_done,
			DCMF_MATCH_CONSISTENCY,
			peer->ptlmap.rank_dcmf,
			rr->size,
			ptr_clientdata->local_memregion,
			ptr_clientdata->remote_memregion,
			0,
			0,
			cb_ack);
	if(ret != DCMF_SUCCESS){
		uloga("%s(): DCMF_Put() failed with %d\n", __func__, ret);
		goto err_out;
	}
	
	//After posting the rpc request message
	rr->msg->refcont++;
	rpc_server_inc_reply(rpc_s);
	
	return 0;
err_out:
	if(rr)
		free(rr);
	if(ptr_clientdata)
		free(ptr_clientdata); //release memory allocated
	ERROR_TRACE();
}

void rpc_report_md_usage(struct rpc_server *rpc_s)
{
	uloga("%s(): Not supported.\n", __func__);
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg,
			struct rpc_cmd *cmd)
{
	peer->cached_remote_memregion = &cmd->mem_region;
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg,
                        struct rpc_cmd *cmd) {
    return;
}
