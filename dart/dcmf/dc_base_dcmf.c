/*
 * Base implementation of DART client.
 *
 * Ciprian Docan (2010) TASSL Rutgers University
 *
 *  The redistribution of the source code is subject to the terms of version 
 *  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "debug.h"
#include "dc_base_dcmf.h"

/*using consistent hashing to get Data Space server id*/
static inline struct node_id * dc_which_peer(struct dart_client *dc)
{
	int peer_id;
	struct node_id *peer;
	
	peer_id = dc->self->ptlmap.id % dc->num_sp;
	peer = dc_get_peer(dc, peer_id);

	return peer;
}

static int register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct ptlid_map *pptlmap;
	int i, id_min;

	peer = dc->peer_tab;
	pptlmap = msg->msg_data;
	id_min = dc->cp_min_rank;

	for (i = 0; i < dc->peer_size; i++) {
		peer->ptlmap = *pptlmap;
		peer++;
		pptlmap++;
	}	

	rpc_s->app_minid = dc->cp_min_rank;
	rpc_s->app_num_peers = dc->cp_in_job;//important

	free(msg->msg_data);
	free(msg);
	dc->f_reg = 1;
	
	uloga("%s(): #%u(dart_id=%d), job has %d peers, starting at %d.\n",
		__func__, rpc_s->ptlmap.rank_dcmf, rpc_s->ptlmap.id, 
		dc->cp_in_job, dc->cp_min_rank);
	
	return 0;
}

/*
  RPC routine to complete the registration process with a master server
  peer.
*/
static int dcrpc_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	size_t size_peertab;
	int i, err = -ENOMEM;

	dc->num_sp = hreg->num_sp;
	dc->num_cp = hreg->num_cp;
	dc->peer_size = dc->num_sp + dc->num_cp;
	dc->cp_min_rank = hreg->id_min;
	
	/* Allocate peer resources (storage). */
	size_peertab = sizeof(*peer) * dc->peer_size;
	dc->peer_tab = malloc(size_peertab);
	if (!dc->peer_tab)
			goto err_out;
	memset(dc->peer_tab, 0, size_peertab);
	dc->cn_peers = dc->peer_tab + dc->num_sp;

	peer = dc->peer_tab;
	for (i = 0; i < dc->peer_size; i++) {
			INIT_LIST_HEAD(&peer->req_list);
			peer->num_msg_at_peer = rpc_s->max_num_msg;
			peer->num_msg_ret = 0;
			peer++;
	}

	peer = dc->peer_tab;
	/* To register and get an ID I sent a message to the servers
	   group master, adjust its credits here.  */
	peer->num_msg_at_peer--;

	rpc_server_set_peer_ref(rpc_s, dc->peer_tab, dc->peer_size); 
	rpc_server_set_rpc_per_buff(rpc_s, dc->num_sp + dc->cp_in_job);

	/* Setup self reference. */
	dc->self = dc_get_peer(dc, hreg->pm_cp.id);
	dc->self->ptlmap = hreg->pm_cp;
	rpc_s->ptlmap = hreg->pm_cp;

	/* Setup server reference, note that cmd->id can be different
	   than 0 when running with multiple servers. */
	peer = dc_get_peer(dc, cmd->id);
	peer->ptlmap = hreg->pm_sp;
	peer->num_msg_ret++;
	if (peer == dc->peer_tab)
			dc->peer_tab->num_msg_at_peer++;
	
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	
	size_peertab = sizeof(struct ptlid_map) * dc->peer_size;
	msg->msg_data = malloc(size_peertab);
	if(!msg->msg_data){
		free(msg);
		goto err_out;
	}
	msg->size = size_peertab;
	msg->cb = register_completion;
	
	//for debug
	/*
	uloga("%s(): #%u(dart_id=%d), get cn_register reply from server node #%u(dart_id=%d)\n",
		__func__, rpc_s->ptlmap.rank_dcmf, hreg->pm_cp.id, hreg->pm_sp.rank_dcmf, cmd->id);
	*/        

	err = rpc_receive_direct(rpc_s, peer, msg, &cmd->mem_region);
	if(err<0)		
		goto err_out_free;
	
	return 0;
err_out_free:
        free(msg->msg_data);
        free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;        
}

static int dc_register_at_master(struct dart_client *dc, int appid)
{
	struct msg_buf *msg;
	struct hdr_register *hr;
	struct node_id peer;
	int err;

	memset(&peer, 0, sizeof(struct node_id));
	err = rpc_read_config(&(peer.ptlmap.rank_dcmf));
	if (err < 0) 
		goto err_out;

	INIT_LIST_HEAD(&peer.req_list);
	peer.num_msg_at_peer = dc->rpc_s->max_num_msg;
	peer.num_msg_ret = 0;

	msg = msg_buf_alloc(dc->rpc_s, &peer, 1);
	if (!msg) 
		goto err_out;
	msg->msg_rpc->cmd = cn_register;

	hr = (struct hdr_register *) msg->msg_rpc->pad;
	hr->pm_sp = peer.ptlmap;
	hr->pm_cp.rank_dcmf = dc->rpc_s->ptlmap.rank_dcmf;
	hr->pm_cp.id = -1;
	hr->pm_cp.appid = appid;
	hr->num_cp = dc->cp_in_job;

	err = rpc_send(dc->rpc_s, &peer, msg);
	if (err < 0)
		goto err_out_free;

	/* 
	   If we got here, rpc_send() was successfull, and msg will be
	   freed in the async path by rpc_cb_req_completion().
	*/
	do {
		rpc_process_event(dc->rpc_s);
	}
	while (!dc->f_reg);

	//for debug
	//uloga("%s(): #%u successfull.\n", __func__, dc->rpc_s->ptlmap.rank_dcmf);

	return 0;
 err_out_free:
        free(msg);
 err_out:
        uloga("'%s()': failed.\n", __func__);
        return -1;
	
}

static int dc_unregister(struct dart_client *dc)
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;

	sp_index = dc->self->ptlmap.id % dc->num_sp;
	peer = dc_get_peer(dc, sp_index);
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	
	msg->msg_rpc->cmd = cn_unregister;
	msg->msg_rpc->id = dc->self->ptlmap.id;

	hreg = (struct hdr_register *) msg->msg_rpc->pad;
	hreg->num_sp = dc->num_sp;
	hreg->pm_cp = dc->self->ptlmap;
	hreg->pm_sp = peer->ptlmap;
	//for debug
	uloga("%s(): #%u(dart_id=%d) send cn_unregister to #%u(dart_id=%d), hreg->num_sp=%d\n",
		__func__,dc->self->ptlmap.rank_dcmf,dc->self->ptlmap.id,
		peer->ptlmap.rank_dcmf,peer->ptlmap.id,hreg->num_sp);

	err = rpc_send(dc->rpc_s, peer, msg);
	if(err < 0)
		goto err_out_free;

	/* this loop is for clean-up? */
	int i = 0;
	while (i++ < dc->rpc_s->max_num_msg) 
		rpc_process_event(dc->rpc_s);

	return 0;	
 err_out_free:
        free(msg);
 err_out:
        uloga("'%s()': failed.\n", __func__);
        return err;
}

/*
  Public API starts here.
*/
struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref)
{
	struct dart_client *dc;
	size_t size;
	int err;
	
	dc = calloc(1, sizeof(*dc));
	if(!dc)
		return NULL;
	dc->dart_ref = dart_ref;
	dc->cp_in_job = num_peers;

	//Register rpc msg handler functions
	rpc_add_service(cn_register, dcrpc_register);
	
	dc->rpc_s = rpc_server_init(10, num_peers, dc, DART_CLIENT);
	if(!dc->rpc_s){
		free(dc);
		return NULL;
	}
	
	err = dc_register_at_master(dc, appid);
	if(err<0){
		rpc_server_free(dc->rpc_s);
		free(dc);
		return NULL;
	}
	
	return dc;
}

void dc_free(struct dart_client *dc)
{
        uloga("'%s()': #%u num_posted = %d.\n", __func__,dc->self->ptlmap.rank_dcmf, dc->num_posted);
        while (dc->num_posted)
                rpc_process_event(dc->rpc_s);

        dc_unregister(dc);

        rpc_server_free(dc->rpc_s);
        if (dc->peer_tab)
                free(dc->peer_tab);
        free(dc);	
}

int dc_process(struct dart_client *dc)
{
	return rpc_process_event(dc->rpc_s);
}

//For testing DART DCMF performance
static int data_send_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	dc->num_posted--;
	//for debug
	//uloga("%s(): #%u num_posted = %d.\n", __func__, rpc_s->ptlmap.rank_dcmf, dc->num_posted);

	if(msg && msg->msg_data){
		free(msg->msg_data);
		free(msg);
	}

	return 0;
}

static int data_read_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	dc->num_posted--;
	dc->read_complete = 1;	
	
	if(msg && msg->msg_data){
		int offset = msg->size - 1;
		*((char*)(msg->msg_data)+offset ) = 0;
		uloga("%s(): #%u, read data '%s'\n", __func__, rpc_s->ptlmap.rank_dcmf, msg->msg_data);

		free(msg->msg_data);
		free(msg);
	}

	return 0;
}

int dc_send_test(struct dart_client *dc, size_t size)
{
	//struct node_id *peer = dc_get_peer(dc, 0);
	struct node_id *peer = dc_which_peer(dc);
	struct msg_buf *msg;
	int err;
	
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	
	msg->msg_rpc->cmd = cn_data;
	msg->msg_rpc->id = dc->self->ptlmap.id;
	msg->size = size;
	msg->msg_data = calloc(1,msg->size);
	if(!msg->msg_data)
		goto err_out_free;

	msg->cb = data_send_completion;

	err = rpc_send(dc->rpc_s, peer, msg);
	if(err < 0){
		if(msg->msg_data)
			free(msg->msg_data);
		goto err_out_free;
	}
	dc->num_posted++;

	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("%s(): #%u, failed\n", __func__, dc->self->ptlmap.rank_dcmf);
 	return -1;	
}

int dc_read_test(struct dart_client *dc, size_t size)
{
	struct node_id *peer = dc_get_peer(dc, 0);
	struct msg_buf	*msg;
	int err;

	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if(!msg)
		goto err_out;

	msg->msg_rpc->cmd = cn_read;
	msg->msg_rpc->id = dc->self->ptlmap.id;
	msg->size = size;
	msg->msg_data = calloc(1, msg->size);
	if(!msg->msg_data)
		goto err_out_free;

	msg->cb = data_read_completion;

	err = rpc_receive(dc->rpc_s, peer, msg);
	if(err < 0){
		if(msg->msg_data)
			free(msg->msg_data);
		goto err_out_free;
	}
	dc->num_posted++;	
	dc->read_complete = 0;

	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("%s(): #%u, failed\n", __func__, dc->self->ptlmap.rank_dcmf);
 	return -1;	
}
