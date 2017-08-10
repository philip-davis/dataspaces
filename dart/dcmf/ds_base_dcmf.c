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
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (
INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Fan Zhang (2011) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/stat.h>

#include "debug.h"
#include "ds_base_dcmf.h"
#include "list.h"

//For testing DART DCMF performance
static unsigned long long _transfer_start_time = 0;
static unsigned long long _transfer_end_time = 0;
static double _clockMHz = 850;

extern void rpc_server_dec_reply(struct rpc_server*);

static int dsrpc_announce_cp(struct rpc_server *, struct rpc_cmd *);

static struct app_info *app_alloc()
{
	struct app_info *app = 0;

	app = malloc(sizeof(*app));
	if (!app)
		return 0;
	memset(app, 0, sizeof(*app));

	return app;
}

static struct app_info *app_find(struct dart_server *ds, int appid)
{
	struct app_info *app;

	list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
		if (app->app_id == appid)
			return app;
	}

	return NULL;
}

/*
  Propagate compute peer information to other server peers in the space.
*/
static int ds_announce_cp(struct dart_server *ds, struct app_info *app)
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer, *cpeer;
	struct ptlid_map *pptlmap;
	int i, k, err;

	cpeer = app->app_peer_tab;

	for(i = 1; i< ds->num_sp; i++){
		peer = ds_get_peer(ds, i);
		
		err = -ENOMEM;
		msg = msg_buf_alloc(ds->rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->cb = default_completion_with_data_callback;
		msg->size = sizeof(*pptlmap) * app->app_num_peers;

		pptlmap = msg->msg_data = calloc(1, msg->size);
		if(!msg->msg_data)
			goto err_out_free;

		for(k=0; k<app->app_num_peers; k++){
			*pptlmap++ = (cpeer++)->ptlmap;
		}

		cpeer = app->app_peer_tab;

		msg->msg_rpc->cmd = sp_announce_cp;
		msg->msg_rpc->id = ds->self->ptlmap.id;
		
		hreg = (struct hdr_register *) msg->msg_rpc->pad;
		hreg->pm_cp = cpeer->ptlmap;
		hreg->num_cp = app->app_num_peers;

		err = rpc_send(ds->rpc_s, peer, msg);
		if(err<0){
			free(msg->msg_data);
			goto err_out_free;
		}
	}
	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;	
}

/*
  Complete compute peer registration: send back the registration info.
*/
static int ds_register_cp(struct dart_server *ds, struct app_info *app)
{
	struct hdr_register *hreg;
	struct msg_buf *msg;
	struct node_id *peer;
	struct ptlid_map *pptlmap;
	int err, i, k, id_min;
        
	/* ds->self->id is used as an offset in the cn_peers tab, and
	   each service peer will complete a compute peer request wich
	   is a multiple of ds->num_sp, starting from its id/rank. */
	i = ds->self->ptlmap.id;

	while(i < ds->num_cp){
		peer = ds->cn_peers + i;
        	
		err = -ENOMEM;
        	
		//Construct reply msg for remote computer node
		msg = msg_buf_alloc(ds->rpc_s, peer, 1);
		if(!msg)
			break;
        	
		msg->msg_rpc->cmd = cn_register;
		msg->msg_rpc->id = ds->self->ptlmap.id;
        	
		msg->cb = default_completion_with_data_callback;
		msg->size = sizeof(*pptlmap) * ds->peer_size;
        	
		pptlmap = msg->msg_data = malloc(msg->size);
		if(!msg->msg_data){
			free(msg);
			break;
		}
        	
		//Copy the data
		for(k=0; k<ds->peer_size; k++)
			*pptlmap++ = ds->peer_tab[k].ptlmap;

		hreg = (struct hdr_register *) msg->msg_rpc->pad;
		hreg->pm_cp = peer->ptlmap;
		hreg->pm_sp = ds->self->ptlmap;
		hreg->roff = 0;
		hreg->maxsize = 4 * 10124 * 1024;
		hreg->num_sp = ds->size_sp;
		hreg->num_cp = ds->size_cp;

		list_for_each_entry(app, &ds->app_list, struct app_info, app_entry){
			if(app->app_id == peer->ptlmap.appid) { //find which app this dc belongs to
				hreg->id_min = app->app_peer_tab[0].ptlmap.id;
			}
		}		 			
       	
		err = rpc_send(ds->rpc_s, peer, msg);

		if(err < 0){
			free(msg->msg_data);
			free(msg);
			break;
		}
                
		//Step forward with a stride as ds->num_sp
		i = i + ds->num_sp;
	}
        
	if(i < ds->num_cp)
		goto err_out;
	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/* 
  RPC routine to serve a compute node registration request.
*/
static int dsrpc_cn_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct node_id *cn_peer;
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct app_info *app;
	int err = -ENOMEM;
        
	app = app_find(ds, hreg->pm_cp.appid);
	if(!app){
		//Create new app obj
		app = app_alloc();
		if(!app)
			goto err_out;
		
		app->app_id = hreg->pm_cp.appid;
		app->app_num_peers = hreg->num_cp;
		app->app_peer_tab = ds->cn_peers + ds->num_cp;
			
		/* Reserve # of peers for the new application. */
		ds->num_cp += hreg->num_cp;
		  
		list_add(&app->app_entry, &ds->app_list);
	}

	cn_peer = app->app_peer_tab + app->app_cnt_peers;
	cn_peer->ptlmap = hreg->pm_cp;//Important copy
	cn_peer->ptlmap.id = cn_peer - ds->peer_tab; //global dart id?

	cn_peer->num_msg_ret++;
	ds->self->num_msg_ret--;

	/* One more node for this application has joined. */
	app->app_cnt_peers++;

	/* Wait for all of the peers to join in. */
	if (app->app_cnt_peers != app->app_num_peers)
		return 0;
#ifdef DEBUG
	uloga("%s(): all %d compute peers for app %d have joined.\n", 
		__func__, app->app_num_peers, app->app_id);
#endif
	
	err = ds_announce_cp(ds, app);
	if(err < 0)
		goto err_out;

	if (ds->num_cp == ds->size_cp){ //all apps have joined
#ifdef DEBUG
   		uloga("All apps has registered in the master server!\n");         
#endif
		list_for_each_entry(app, &ds->app_list, struct app_info, app_entry){
			if(app->app_cnt_peers != app->app_num_peers)
				return 0;
		}

		err = ds_register_cp(ds, app);
		if(err < 0)
			goto err_out;
	}

	return 0;
err_out:
	uloga("'%s()' failed with err %d\n", __func__, err);
	return err;	
}


/*
  RPC routine to unregister a compute peer.
*/
static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	static int num_unreg = 0;

	if(num_unreg == ds->num_cp)
		return 0;

	if((++num_unreg) == ds->num_cp){
		ds->f_stop = 1;
	}

	/* 
	   After  the first  compute peer  'unregister'  request, stop
	   accepting new requests and terminate pending ones.
	*/	
	ds->f_nacc = 1;
	
	/* Forwarding the unregister rpc message in a ring manner*/
	hreg->num_sp--; //the rest number of forwarding

	if(hreg->num_sp){
		peer = ds_get_peer(ds, (ds->self->ptlmap.id+1)%ds->num_sp);
		msg = msg_buf_alloc(rpc_s, peer, 1);
		if(!msg)
			goto err_out;
		
		memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
		msg->msg_rpc->id = ds->self->ptlmap.id;
	
		err = rpc_send(rpc_s, peer, msg);
		if(err < 0){
			free(msg);
			goto err_out;
		}
	}
	
	return 0;
err_out:
	ERROR_TRACE();
}

static int cn_data_transfer_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);

	//Record END_TIME of cn_data transfer
	/*
	_transfer_end_time = DCMF_Timebase();
	unsigned long long cycles = _transfer_end_time - _transfer_start_time;
	double usec = cycles/_clockMHz;
	uloga("%s():cycles,%10lld,usec,%8.4f\n",
		__func__, cycles, usec);	
	*/
	if(msg && msg->msg_data){
		free(msg->msg_data);
		free(msg);
	}

	return 0;
}

static int cn_read_transfer_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);

	if(msg && msg->msg_data){
		free(msg->msg_data);
		free(msg);
	}

	return 0;
}

/*
 RPC routine to service cn_data from compute peer.
*/
static int dsrpc_cn_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct msg_buf *msg;
	int err;

	peer = ds_get_peer(ds, cmd->id);
	
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = cmd->mem_size;
	msg->msg_data = malloc(msg->size);
	if(!msg->msg_data){
		goto err_out_free;
	}
	msg->cb = cn_data_transfer_completion;
	
	//Record START_TIME of cn_data transfer
	//_transfer_start_time = DCMF_Timebase();
	
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	if(err<0){
		free(msg->msg_data);
		goto err_out_free;
	}	

	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("%s(): #%u, failed\n",__func__, ds->self->ptlmap.rank_dcmf);
	return -1;
}

static int dsrpc_cn_read(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct msg_buf *msg;
	int err;

	peer = ds_get_peer(ds, cmd->id);
	
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	msg->size = cmd->mem_size;
	msg->msg_data = calloc(1, msg->size);
	if(!msg->msg_data){
		goto err_out_free;
	}
	msg->cb = cn_read_transfer_completion;

	int len = 'Z' - 'A' + 1;
	for(int i=0; i < msg->size; i++){
		*((char*)msg->msg_data + i) = 'A' + (i%len);
	}

#ifdef DEBUG
	uloga("%s(): #%u(dart_id=%d), get cn_read(msg->size=%d) from compute node #%u(dart_id=%d)\n",
		__func__,rpc_s->ptlmap.rank_dcmf,msg->size,rpc_s->ptlmap.id,peer->ptlmap.rank_dcmf,peer->ptlmap.id); 
#endif	
	
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_send_direct(rpc_s, peer, msg);
	if(err<0){
		free(msg->msg_data);
		goto err_out_free;
	}	

	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("%s(): #%u, failed\n",__func__, ds->self->ptlmap.rank_dcmf);
	return -1;
}


/*
 RPC routine to service registration request from server peer.
 This is invoked only by the master node.
*/
static int dsrpc_sp_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	
	peer = ds_get_peer(ds, ds->num_sp);
	peer->ptlmap = hreg->pm_cp;
	peer->ptlmap.id = ds->num_sp; //Assign application level id(DART id)
	peer->num_msg_ret++;
	/*Keep return messages consistent*/
	ds->self->num_msg_ret--;
	
	//Incremental
	ds->num_sp = ds->num_sp + 1;

	//Wait for all nodes to join in before sending back registration info
	if(ds->num_sp < ds->size_sp)
		return 0;

#ifdef DEBUG	
	uloga("'%s()': all space peers joined.\n", __func__);
#endif
	
	//Send back registration info to space peers(excluding itself)
	int i, k, err;
	struct ptlid_map *pptlmap;
	for(i=1; i<ds->size_sp; i++){
		err = -ENOMEM;
		peer = ds_get_peer(ds, i);
		
		msg = msg_buf_alloc(rpc_s, peer, 1);
		if(!msg)
			goto err_out;
		
		//Send back data space servers basic info
		msg->cb = default_completion_with_data_callback;
		msg->size = sizeof(struct ptlid_map)*ds->num_sp;
		pptlmap = msg->msg_data = calloc(1,msg->size);
		if(!msg->msg_data)
			goto err_out_free;
		for(k=0; k<ds->num_sp; k++){
			*pptlmap++ = ds->peer_tab[k].ptlmap;
		}
		
		msg->msg_rpc->cmd = sp_reg_reply;
		msg->msg_rpc->id = ds->self->ptlmap.id;
		
		hreg = (struct hdr_register *)msg->msg_rpc->pad;
		hreg->pm_cp = peer->ptlmap;
		hreg->pm_sp = ds->self->ptlmap;
		hreg->num_sp = ds->num_sp;
	
		err = rpc_send(rpc_s, peer, msg);
		if(err < 0){
			if(msg->msg_data)
				free(msg->msg_data);
			goto err_out_free;
		}
	}

	// Rename file "conf.srv" -> "conf"
	err = rename("conf.srv", "conf");
	if (err < 0) {
		uloga("%s(): failed to rename the config file!\n", __func__);
		goto err_out;
	} 
	
	return 0;
err_out_free:
	free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int sp_ack_register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct ptlid_map *pptlmap = msg->msg_data;
	struct node_id *peer;
	int i;
	
	//Copy the data space server nodes info
	peer = ds->peer_tab;
	for(i = 0; i<ds->size_sp; i++){
		peer->ptlmap = *pptlmap++;
		peer++;
	}
	
	ds->f_reg = 1;
	
	//Free the memory
	free(msg->msg_data);
	free(msg);
	
	return 0;
}

/*
 RPC routine to complete server peer registration process.
 This is invoked by non-master peers.
*/
static int dsrpc_sp_ack_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	int err = -ENOMEM;
	
	ds->self = ds_get_peer(ds, hreg->pm_cp.id);//Locate myself
	ds->num_sp = hreg->num_sp;
	
	rpc_s->ptlmap = hreg->pm_cp;
	
	peer = ds_get_peer(ds, cmd->id);
	peer->ptlmap = hreg->pm_sp; //Update the ptlmap info for remote node
	
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_out;
	
	msg->size = cmd->mem_size;
	msg->msg_data = malloc(msg->size);
	if(!msg->msg_data)
		goto err_out;
	msg->cb = sp_ack_register_completion;
	
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
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

static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app = msg->private;
	struct ptlid_map *pptlmap = msg->msg_data;
	struct node_id *peer;
	int i;

	peer = app->app_peer_tab;
	//Copy the received data
	for (i = 0; i < app->app_num_peers; i++) {
		(peer++)->ptlmap = *pptlmap++;
	}
	free(msg->msg_data);
	free(msg);

	if(ds->num_cp == ds->size_cp)
		ds_register_cp(ds, app);       

	return 0;
}

/*
  RPC routine to service compute peer joins.
*/
static int dsrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	struct app_info *app;
	int err = -ENOMEM;
	
	app = app_alloc();//new app obj
	if (!app)
		goto err_out;

	app->app_id = hreg->pm_cp.appid;
	app->app_num_peers = hreg->num_cp;
	app->app_cnt_peers = hreg->num_cp;
	app->app_peer_tab = ds->peer_tab + hreg->pm_cp.id;

	ds->num_cp += hreg->num_cp;
	list_add(&app->app_entry, &ds->app_list);//add app obj into list
	
	peer = ds_get_peer(ds, cmd->id);
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg) 
		goto err_out;
	
	msg->size = cmd->mem_size;
	msg->msg_data = malloc(msg->size); 
	if (!msg->msg_data) {
		free(msg);
		goto err_out;
	}
	msg->cb = announce_cp_completion;
	msg->private = app;
        
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	if(err<0){
		free(msg->msg_data);
		goto err_out_free;
	}
	
	return 0;
err_out_free:
	if(msg)
		free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int ds_register_at_master(struct dart_server *ds)
{
	struct node_id *peer = ds_get_peer(ds,0);
	struct msg_buf *msg;
	struct hdr_register *hreg;
	int err;
	
	err = rpc_read_config(&peer->ptlmap.rank_dcmf, "conf.srv");
	if (err<0)
		goto err_out;
	
	err = -ENOMEM;
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;
	
	msg->msg_rpc->cmd = sp_reg_request;
	
	hreg = (struct hdr_register *)msg->msg_rpc->pad;
	hreg->pm_cp = ds->rpc_s->ptlmap;
	hreg->pm_sp = peer->ptlmap;
	
	err = rpc_send(ds->rpc_s, peer, msg);
	if (err<0)
		goto err_out_free;

	while (!ds->f_reg){
		err = rpc_process_event(ds->rpc_s);
		if (err<0)
			goto err_out;
	}

	return 0;
err_out_free:
	if (msg)
		free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);	
}

static int file_lock(int fd, int op)
{
	int err;
	struct flock fl = {
			.l_type = (op != 0)? F_WRLCK : F_UNLCK, 
			.l_whence = SEEK_SET,
			.l_start = 0,
			.l_len = 0,
			.l_pid = getpid()
	};


	err = fcntl(fd, F_SETLKW, &fl);
	if (err == 0)
			return 0;

	ulog_err("'%s()' failed", __func__);
	return err;
}

static int ds_boot(struct dart_server *ds)
{
	struct stat st_buff;
	char lck_file[] = "srv.lck";
	char conf_file[] = "conf.srv";
	int fd, err;

	memset(&st_buff, 0, sizeof(st_buff));

	err = fd = open(lck_file, O_WRONLY | O_CREAT, 0644);
	if (err < 0) 
		goto err;

	/* File locking does not work on the login nodes :-( */
	err = file_lock(fd, 1);
	if (err < 0)
		goto err_fd;

	err = stat(conf_file, &st_buff);
	if (err < 0 && errno != ENOENT)
		goto err_flock;

	if (st_buff.st_size == 0) {
		/* Config file is empty, should run as master. */
		ds->self = ds->peer_tab;
		ds->self->ptlmap = ds->rpc_s->ptlmap;

		err = rpc_write_config(ds->rpc_s, conf_file);

		if (err < 0)
			goto err_flock;
		file_lock(fd, 0);
	}
	else {
		/* Run as slave. */
		file_lock(fd, 0);
		err = ds_register_at_master(ds);
		if (err < 0)
			goto err_flock;
	}

	close(fd);
	remove(lck_file);
    
	return 0;
err_flock:
	uloga("%s(): failed with err_flock.\n");
	file_lock(fd, 0);
err_fd:
	uloga("%s(): failed with err_fd.\n");
	close(fd);
	remove(lck_file);
err:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

//Public APIs
struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm)
{
	struct dart_server *ds = NULL;
	struct node_id *peer;
	size_t size;
	int i,err;

	size = sizeof(struct dart_server) + 
			(num_sp + num_cp) * sizeof(struct node_id);
	ds = calloc(1, size);
	if (!ds)
		goto err_out;
	ds->dart_ref = dart_ref;
	ds->peer_tab = (struct node_id*)(ds+1);
	ds->cn_peers = ds->peer_tab + num_sp;
	ds->peer_size = num_sp + num_cp;
	ds->size_cp = num_cp;
	ds->size_sp = num_sp;
	ds->num_sp = 1;//used for generating server peer id
	INIT_LIST_HEAD(&ds->app_list);
        
	ds->rpc_s = rpc_server_init(10, ds->peer_size, ds, DART_SERVER);

	if (!ds->rpc_s)
		goto err_free_dsrv;
	rpc_server_set_peer_ref(ds->rpc_s, ds->peer_tab, ds->peer_size);
	rpc_server_set_rpc_per_buff(ds->rpc_s, ds->peer_size);
	ds->rpc_s->app_num_peers = num_sp;//important
        
	peer = ds->peer_tab;
	for(i=0; i<ds->peer_size;i++){
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->num_msg_ret = 0;

		peer++;
	}

	//Add RPC service routines for corresponding messages
	rpc_add_service(cn_register, dsrpc_cn_register);
	rpc_add_service(cn_unregister, dsrpc_cn_unregister);
	rpc_add_service(sp_reg_request, dsrpc_sp_register);
	rpc_add_service(sp_reg_reply, dsrpc_sp_ack_register);
	rpc_add_service(sp_announce_cp, dsrpc_announce_cp);
	//For testing DART DCMF performance
	//rpc_add_service(cn_data, dsrpc_cn_data);       
	//rpc_add_service(cn_read, dsrpc_cn_read);
 
	err = ds_boot(ds);
	if(err<0)
		goto err_free_dsrv;
        
#ifdef DEBUG
	uloga("'%s()': rank=%u, init ok.\n", __func__, DCMF_Messager_rank());
#endif        

	return ds;
err_free_dsrv:
	if(ds)
		free(ds);
err_out:
	uloga("'%s()': failed with %d.\n",__func__,err);
	return NULL;
}

void ds_free(struct dart_server *ds)
{
	struct app_info *app, *t;

	rpc_server_free(ds->rpc_s);

	list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry) {
		list_del(&app->app_entry);
		free(app);
	}
	free(ds);
#ifdef DEBUG
	uloga("%s(): OK\n", __func__);
#endif
}

int ds_process(struct dart_server *ds){
	return rpc_process_event(ds->rpc_s);
}
