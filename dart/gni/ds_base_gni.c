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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "ds_base_gni.h"
#include <netdb.h>
#include "sys/socket.h"
#include "netinet/in.h"
#include "arpa/inet.h"
#include "sys/ioctl.h"
#include <sys/types.h>
#include <ifaddrs.h>
#include <pthread.h>

#define PEER_ID(peer)	peer->ptlmap.id
#define CONNMAX 1000

static int connectfd[CONNMAX];

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

	return 0;
}

// Master server gets slave servers announcement, gather their attr for app. 
static int dsrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct msg_buf *msg;
	struct node_id *peer;
        struct app_info *app;
	int err = -ENOMEM;
	int i, j;
	int info_size;

	struct gni_smsg_attr_info *tmp_attr_info;
	struct gni_smsg_attr_info *cur_attr_info = (struct gni_smsg_attr_info *)(cmd->pad);

	app = app_find(ds, cur_attr_info->appid);
	tmp_attr_info = rpc_s->attr_info_start;


	while(tmp_attr_info){
		if(tmp_attr_info->appid == cur_attr_info->appid){
			break;
		}
		else
			tmp_attr_info = tmp_attr_info->next;	
	}

	if(!tmp_attr_info)
		printf("'%s()': attr_info NULL.\n", __func__);

	tmp_attr_info->remote_smsg_attr[cmd->id] = cur_attr_info->local_smsg_attr;
	tmp_attr_info->tmp_num++;

	if(tmp_attr_info->tmp_num == ds->num_sp-1)
		app->f_reg = 1;

	if(app->f_reg == 1){
		//send all attr info to master client of such app.
	  tmp_attr_info->remote_smsg_attr[0] = tmp_attr_info->local_smsg_attr;
	  
		peer = app->app_peer_tab;
		msg = msg_buf_alloc(rpc_s, peer, 1);
		if (!msg) 
			goto err_out;


		msg->msg_rpc->cmd = cn_register;
		msg->msg_rpc->id = ds->rpc_s->ptlmap.id;

		msg->size = info_size = ds->num_sp * sizeof(gni_smsg_attr_t);

		msg->msg_data = (gni_smsg_attr_t *)malloc(info_size);
		memcpy(msg->msg_data, tmp_attr_info->remote_smsg_attr, info_size);
		//msg->msg_data = tmp_attr_info->remote_smsg_attr;//ToDo
                msg->cb = default_completion_with_data_callback;

		err = rpc_send(rpc_s, peer, msg);
		if (err != 0){
			free(msg);
			goto err_out;
		}		

	}

        return 0;

 err_out:
	printf("(%s): failed. (%d)\n", __func__, err);
	return err;
}


// Slave server respond to master server with its attr for app.
static int ds_register_cp(struct dart_server *ds, struct app_info *app)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;
	int i, j;

	struct gni_smsg_attr_info *cur_attr_info;
	struct gni_smsg_attr_info *attr_info;
	gni_smsg_attr_t	*smsg_attr;
	int info_size;	

	peer = ds->peer_tab;
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg) 
		goto err_out;

	msg->msg_rpc->cmd = sp_announce_cp;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;

	cur_attr_info = (struct gni_smsg_attr_info *)(msg->msg_rpc->pad);
	cur_attr_info->appid = app->app_id;

	attr_info = ds->rpc_s->attr_info_start;

	while(attr_info->appid != app->app_id){
		attr_info = attr_info->next;
	}

	cur_attr_info->local_smsg_attr = attr_info->local_smsg_attr;

	err = rpc_send(ds->rpc_s, peer, msg);
	if (err != 0){
		free(msg);
		goto err_out;
	}

	return 0;

 err_out:
	printf("(%s): failed. (%d)\n", __func__, err);
	return err;

}

// slave server gets app info and finish local reg for this app
static int sp_register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct app_info *app;
	struct node_id *peer;
	int err = -ENOMEM;
	int i, j;

	struct ptlid_map *dcreg;
	struct smsg_attr_reg *sar;
	struct gni_smsg_attr_info *cur_attr_info;
	gni_smsg_attr_t	*smsg_attr;
	
	gni_return_t status;
	void *recv_buffer;

	int info_size = msg->size;
	int num_cp = info_size/(sizeof(struct ptlid_map) + sizeof(gni_smsg_attr_t));	

	recv_buffer = msg->msg_data;
	dcreg = (struct ptlid_map *)recv_buffer;
	smsg_attr = (gni_smsg_attr_t *)(dcreg+num_cp);

	cur_attr_info = ds->rpc_s->attr_info_start;
	while(1){
		if(!cur_attr_info->next){
			cur_attr_info->next = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info));
			memset(cur_attr_info->next, 0, sizeof(struct gni_smsg_attr_info));
			cur_attr_info = cur_attr_info->next;
			cur_attr_info->next = NULL;
			cur_attr_info->appid = dcreg->appid;
			break;
		}
		else
			cur_attr_info = cur_attr_info->next;	
	}

	err = rpc_smsg_init(ds->rpc_s, cur_attr_info, num_cp);
	if (err != 0){
		printf("Rank %d: failed for rpc_smsg_init. (%d)\n", ds->rpc_s->ptlmap.id, err);
		goto err_out;
	}


	peer = &ds->peer_tab[ds->size_sp-1];
	while(1){
		if(!peer->next){
			peer->next = (struct node_id *)malloc(num_cp * sizeof(struct node_id));
			memset(peer->next, 0, num_cp * sizeof(struct node_id));
			peer = peer->next;

			break;
		}
		else
			peer = (struct node_id *)(peer->next + peer->next->peer_num - 1);		
	}	


	//set up new peer_tab block for this app
	for(j=0;j<num_cp;j++){
		peer->ptlmap.nid = dcreg->nid;
		peer->ptlmap.pid = dcreg->pid;
		peer->ptlmap.appid = dcreg->appid;
		peer->ptlmap.id = dcreg->id;		
		peer->peer_rank = j;
		peer->peer_num = num_cp;

		peer->f_reg = 1;

		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;

		peer->num_msg_recv = 0;
		peer->num_msg_ret = 0;	  

		peer->sys_msg_recv = 0;
		peer->sys_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->sys_msg_ret = 0;	 
		peer->next = NULL;


		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &peer->ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
		printf("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
		goto err_out;
		}

		peer->remote_smsg_attr = *smsg_attr;	

		err = rpc_smsg_config(ds->rpc_s, cur_attr_info, peer);
		if (err != 0){
			printf("Rank %d: failed for config SMSG for peer %d. (%d)\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}	

		dcreg++;
		peer++;
		smsg_attr++;

	}


	dcreg = (struct ptlid_map *)recv_buffer;
	app = app_find(ds, dcreg->appid);
	if (!app) {
		app = app_alloc();
		if (!app) 
			goto err_out;

		ds->num_cp += num_cp;

		app->app_id = dcreg->appid;
		app->app_num_peers = app->app_cnt_peers = num_cp;

		app->app_peer_tab = ds_get_peer(ds, dcreg->id);
		list_add(&app->app_entry, &ds->app_list);

		app->f_reg = 1;
	}


        err = ds_register_cp(ds, app);
	if (err != 0){
		printf("Rank %d: failed for ds_register_cp for application %d. (%d)\n", ds->rpc_s->ptlmap.id, dcreg->appid, err);
		goto err_out;
	}

		
	free(msg->msg_data);
	free(msg);

        return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;

 err_out:
	printf("(%s): failed. (%d)\n", __func__, err);
	return err;
}


static int dsrpc_cn_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)(cmd->pad);	
	struct hdr_register *hr;
	struct msg_buf *msg;
	struct node_id *peer;
        struct app_info *app;
	int err = -ENOMEM;
	int i, j;

	struct ptlid_map *dcreg;
	struct smsg_attr_reg *sar;
	struct gni_smsg_attr_info *cur_attr_info, *tmp_attr_info;
	gni_smsg_attr_t	*smsg_attr;
	
	void *recv_buffer;
	void *send_buffer;
	int info_size;

	// if ds is master server, bcast node_id info and attr info of this app to client servers
	if(rpc_s->ptlmap.id == 0) {
		if(ds->num_sp == 1) {
		    tmp_attr_info = rpc_s->attr_info_start;

		    while(tmp_attr_info) {
		        if(tmp_attr_info->appid == hreg->pm_cp.appid) {
		            break;
		        } else {
			        tmp_attr_info = tmp_attr_info->next;	
		        }
            }

		    if(!tmp_attr_info) {
		        uloga("'%s()': attr_info NULL.\n", __func__);
            }

		    msg = msg_buf_alloc(rpc_s, peer, 1);
		    if (!msg) {
			    goto err_out;
            }

		    msg->msg_rpc->cmd = cn_register;
		    msg->msg_rpc->id = ds->rpc_s->ptlmap.id;

		    msg->size = ds->num_sp * sizeof(gni_smsg_attr_t);
		    msg->msg_data = (gni_smsg_attr_t *)malloc(msg->size);
		    smsg_attr = msg->msg_data;
		    *smsg_attr = tmp_attr_info->local_smsg_attr;

		    msg->cb = default_completion_with_data_callback;

		    peer = ds_get_peer(ds, hreg->pm_cp.id);

		    err = rpc_send(rpc_s, peer, msg);
		    if(err != 0){
			    free(msg);
			    goto err_out;
		    }		
		} else {
		    info_size = hreg->num_cp * (sizeof(struct ptlid_map) + sizeof(gni_smsg_attr_t));
		    send_buffer = malloc(info_size);

		    dcreg = (struct ptlid_map *)send_buffer;

		    peer = ds_get_peer(ds, hreg->pm_cp.id);

		    for(j=0;j<hreg->num_cp;j++){
			dcreg->nid = peer->ptlmap.nid;
			dcreg->pid = peer->ptlmap.pid;
			dcreg->appid = peer->ptlmap.appid;
			dcreg->id = peer->ptlmap.id;

			dcreg++;
			peer++;
		}		

		smsg_attr = (gni_smsg_attr_t *)dcreg;
		peer = ds_get_peer(ds, hreg->pm_cp.id);

		for(j = 0; j < hreg->num_cp; j++) {
			*smsg_attr = peer->remote_smsg_attr;

			smsg_attr++;
			peer++;
		}

		msg = msg_buf_alloc(ds->rpc_s, peer, 1);
		if(!msg) {
		    goto err_out;
        }
		msg->msg_rpc->cmd = cn_register;
		msg->msg_data = send_buffer;
	 	msg->size = info_size;
	    msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
        msg->cb = default_completion_with_data_callback;

		hr = (struct hdr_register *) msg->msg_rpc->pad;
		hr->pm_sp = peer->ptlmap;
		hr->pm_cp = hreg->pm_cp;
		hr->num_sp = hreg->num_sp;
		hr->num_cp = hreg->num_cp;
		hr->id_min = hreg->id_min;

		peer = ds->peer_tab;
		for(i = 0; i < ds->num_sp; i++) {
	        if(i==0){
			    peer++;
				continue;
			}

		    err = rpc_send(rpc_s, peer, msg);
			if(err != 0) {
			    free(msg);
				goto err_out;
			}	
			peer++;	
	    }
    }

	app = app_find(ds, hreg->pm_cp.appid);
	if (!app) {
        app = app_alloc();
		if (!app) 
		    goto err_out;

		    ds->num_cp += hreg->num_cp;

		    app->app_id = hreg->pm_cp.appid;
		    app->app_num_peers = app->app_cnt_peers = hreg->num_cp;

			app->app_peer_tab = ds_get_peer(ds, hreg->pm_cp.id);
		    list_add(&app->app_entry, &ds->app_list);
		}

	} else {
		info_size = hreg->num_cp * (sizeof(struct ptlid_map) + sizeof(gni_smsg_attr_t));
		recv_buffer = malloc(info_size);
		memset(recv_buffer, 0, info_size);

		peer = ds_get_peer(ds, 0);

		msg = msg_buf_alloc(ds->rpc_s, peer, 0);
		if (!msg) { 
		    goto err_out;
        }
		
        msg->msg_data = recv_buffer;
		msg->size = info_size;
        msg->cb = sp_register_completion;

		rpc_mem_info_cache(peer, msg, cmd); 
		err = rpc_receive_direct(rpc_s, peer, msg);
		rpc_mem_info_reset(peer, msg, cmd);

	}

    return 0;

err_out:
	ulogaf("(%s): failed. (%d)\n", __func__, err);
	return err;

}

static int ds_free_app(struct dart_server *ds, struct app_info *app)
{
  struct gni_smsg_attr_info *tmp_attr_info, *cur_attr_info;
	struct node_id *peer, *cur_peer;
	int err = -ENOMEM;
	int i;

	peer = &ds->peer_tab[ds->size_sp-1];
	while(1){
		if(peer->next){
		  if(peer->next->ptlmap.appid == app->app_id){
				break;
		  }
			else
				peer = (struct node_id *)(peer->next + peer->next->peer_num - 1);	
		}
	}

	cur_peer = (struct node_id *)(app->app_peer_tab + app->app_peer_tab->peer_num - 1);
	if(cur_peer->next)
		peer->next = cur_peer->next;
	else
		peer->next = NULL;
	
	//rpc_peer_check(ds->rpc_s);

	peer = app->app_peer_tab;
	for(i=0; i<app->app_peer_tab->peer_num; i++, peer++){

		err = rpc_peer_cleanup(ds->rpc_s, peer);
		if (err != 0)
			goto err_out;
	}


	  cur_attr_info = ds->rpc_s->attr_info_start;
	  tmp_attr_info = ds->rpc_s->attr_info_start;
	  while(cur_attr_info){
	    if(cur_attr_info->appid == app->app_id)
	      break;
	    else{
	      tmp_attr_info = cur_attr_info;
	      cur_attr_info = cur_attr_info->next;
	    }
	  }

	  if(!cur_attr_info){
	    printf("Rank %d: in %s, cur_attr_info is NULL.\n", ds->rpc_s->ptlmap.id, __func__);
	    err = -1;
	    goto err_out;
	  }

	    tmp_attr_info->next = cur_attr_info->next;


	  err = rpc_attr_cleanup(ds->rpc_s, cur_attr_info);
	  if (err != 0) 
	        goto err_out;
	  free(cur_attr_info);


	if(app->app_peer_tab)
	  free(app->app_peer_tab);

	list_del(&app->app_entry);
	free(app);

	return 0;

 err_out:
	printf("(%s): failed. (%d)\n", __func__, err);
	return err;
}


/*
  RPC routine to unregister a compute peer. 
Master Server: 
	Accept unregister from master client, then bcast to all slave servers. Then send back acknowledge info to master client.
Master and Client Server:
	unlink and delete app info, unlink and delete peer_tab info for this app.
*/
static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)(cmd->pad);
	struct hdr_register *hr;
	struct msg_buf *msg, *msg_l, *msg_r;
	struct node_id *peer, *cur_peer;
        struct app_info *app;
	int err = -ENOMEM;
	int i;
	int left, right;

	app = app_find(ds, hreg->pm_cp.appid);

	if(ds->rpc_s->ptlmap.id == 0){

		peer = ds_get_peer(ds, hreg->pm_cp.id);
		peer->f_unreg = 1;

		msg = msg_buf_alloc(rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->id = ds->self->ptlmap.id;
		msg->msg_rpc->cmd = cn_unregister;

		hr = (struct hdr_register *)(msg->msg_rpc->pad);


		hr->pm_cp.appid = hreg->pm_cp.appid;
		hr->pm_cp.id = hreg->pm_cp.id;
		hr->pm_cp.pid = hreg->pm_cp.pid;
		hr->pm_cp.nid = hreg->pm_cp.nid;

		hr->num_sp = ds->num_sp;


		err = rpc_send(rpc_s, peer, msg);
		if (err < 0) 
		{
			free(msg);
			goto err_out;
		}

	}


	left = (rpc_s->ptlmap.id)*2+1;
	right = left+1;
	if(right < ds->num_sp){
	        peer = ds_get_peer(ds, right);

		msg_r = msg_buf_alloc(rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg_r->msg_rpc->id = ds->self->ptlmap.id;
		msg_r->msg_rpc->cmd = cn_unregister;

		hr = (struct hdr_register *)(msg_r->msg_rpc->pad);

		hr->pm_cp.appid = hreg->pm_cp.appid;
		hr->pm_cp.id = hreg->pm_cp.id;
		hr->pm_cp.pid = hreg->pm_cp.pid;
		hr->pm_cp.nid = hreg->pm_cp.nid;
		hr->num_sp = ds->num_sp;

		err = rpc_send(rpc_s, peer, msg_r);
		if (err < 0) 
		{
			free(msg);
			goto err_out;
		}
	}

	if(left < ds->num_sp){
		peer = ds_get_peer(ds, left);

		msg_l = msg_buf_alloc(rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg_l->msg_rpc->id = ds->self->ptlmap.id;
		msg_l->msg_rpc->cmd = cn_unregister;

		hr = (struct hdr_register *)(msg_l->msg_rpc->pad);

		hr->pm_cp.appid = hreg->pm_cp.appid;
		hr->pm_cp.id = hreg->pm_cp.id;
		hr->pm_cp.pid = hreg->pm_cp.pid;
		hr->pm_cp.nid = hreg->pm_cp.nid;
		hr->num_sp = ds->num_sp;

		peer = ds_get_peer(ds, left);

		err = rpc_send(rpc_s, peer, msg_l);
		if (err < 0) 
		{
			free(msg);
			goto err_out;
		}

	}


	//Temp: ToDo DSaaS: not sure ...	
	/*peer = app->app_peer_tab;
	for(i=0;i<app->app_num_peers; i++, peer++){
	  while(peer->num_req)
	  {
	    err = rpc_process_event(rpc_s);
	    if (err!=0)
	      printf("'%s()': encountered an error %d, skipping.\n", __func__, err);
	  }
	  }*/

	for(i=0;i<100;i++)
	  {
	    err = rpc_process_event(rpc_s);
	    if (err!=0)
	      printf("'%s()': encountered an error %d, skipping.\n", __func__, err);
	  }

	err = ds_free_app(ds, app);
	if (err != 0) 
		goto err_out;

	
	//	rpc_peer_check(ds->rpc_s);

	return 0;
 err_out:
	printf("(%s): failed. (%d)\n", __func__, err);
	return err;

}

#if 1
static char* ip_search(void)
{

    /* 1. get my GNI IP address */
    struct ifaddrs *ifaddr, *ifa;
    int s;
    char host[NI_MAXHOST], *ibip = NULL;

    if (getifaddrs(&ifaddr) == -1) {
        return NULL;
    }

    for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next)
    {
        if(ifa->ifa_addr == NULL)
            continue;

        if (ifa->ifa_addr->sa_family == AF_INET)
        {

            s = getnameinfo(ifa->ifa_addr, sizeof(struct sockaddr_in),
                    host, NI_MAXHOST, NULL, 0, NI_NUMERICHOST);
            // fprintf(stderr, "%s: %s\n", ifa->ifa_name, host);
            if (s != 0)
            {
                perror("getnameinfo");
            }
            else{
                if (!strncmp(ifa->ifa_name,"ipogif", 6)) {
                    ibip = host;
                    break;
                }
            }
        }
    }

    return ibip;
}

#else 
static char *ip_search(void)
{
	int sfd, intr;
	struct ifreq buf[16];
	struct ifconf ifc;
	sfd = socket (AF_INET, SOCK_DGRAM, 0); 
	if (sfd < 0){
	  printf("CANNOT FIND HOST IP\n");
		return "cannot find ip";
	}
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = (caddr_t)buf;
	if (ioctl(sfd, SIOCGIFCONF, (char *)&ifc)){
	  printf("CANNOT FIND HOST IP\n");
		return "cannot find ip";
	}
	intr = ifc.ifc_len / sizeof(struct ifreq);
	while (intr-- > 0 && ioctl(sfd, SIOCGIFADDR, (char *)&buf[intr]));
	close(sfd);
	return inet_ntoa(((struct sockaddr_in*)(&buf[intr].ifr_addr))-> sin_addr);
}
#endif

int listen_sock_init(int option, const char *ip, int port, int *SocketPri)
{
	struct sockaddr_in server_addr;
	memset(&server_addr, 0, sizeof server_addr);
	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(port);
	server_addr.sin_addr.s_addr = inet_addr(ip);
	
	
	/* 
	Create the SocketPri use IPv4, Stream socket, IPPROTO
	It is the primary socket that keeps listening during the entire runtime.
	*/
	*SocketPri = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	 
	if(-1 == *SocketPri)
	{
		perror("can not create socket");
		exit(EXIT_FAILURE);
	}

	/* Set options for socket, avoiding "address in use" problem */
	int enable = 1;
	setsockopt(*SocketPri, SOL_SOCKET, SO_REUSEADDR,  &enable, sizeof(int));
	struct linger li;
	li.l_onoff = 1;
	li.l_linger = 0;
	setsockopt(*SocketPri, SOL_SOCKET, SO_LINGER, (const char *)&li, sizeof (li));

	/* Bind the socket with an address*/
	if(-1 == bind(*SocketPri,(struct sockaddr *)&server_addr, sizeof server_addr))
	{
		perror("error bind failed");
		close(*SocketPri);
		exit(EXIT_FAILURE);
	}
	 
	/* prepares the socket for incoming connections */
	if(-1 == listen(*SocketPri, 65535))
	{
		perror("error listen failed");
		close(*SocketPri);
		exit(EXIT_FAILURE);
	}

	return 0;
}

// Get the auto-assigned port number
int port_search(int socket)
{
  int port;
  int err;
  socklen_t len = sizeof(struct sockaddr);

  struct sockaddr_in address;
  memset(&address, 0, sizeof(struct sockaddr_in));
  err = getsockname(socket, (struct sockaddr *)&address, &len);
  if(err < 0)
    return err;
  port = ntohs(address.sin_port);
  return port;
}

static int ds_master_init(struct dart_server *ds)
{
	int i, j, err, p, count = 0;
	int cur_cp = 0; 
	char *localip;
	gni_return_t status;
	struct node_id *peer;
	struct gni_smsg_attr_info *cur_attr_info;

	int k, connect_num = 0;

	int tmp_size=0;
	int info_size;
	void *recv_buffer;
	void *send_buffer;
	struct ptlid_map *dcreg;
	struct smsg_attr_reg *sar;


	// EpCreate+Epbind+smsg_init(rpc+sys)

	for(i=0;i<ds->num_sp; i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;

		peer = &ds->peer_tab[i];
		peer->peer_rank = i;
		peer->peer_num = ds->num_sp;

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &peer->ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		
		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

	cur_attr_info = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info));
	memset(cur_attr_info, 0, sizeof(struct gni_smsg_attr_info));	
	ds->rpc_s->attr_info_start = cur_attr_info;
	cur_attr_info->next = NULL;

	err = rpc_smsg_init(ds->rpc_s, cur_attr_info, ds->num_sp);
	if (err != 0){
        printf("Rank %d: failed for rpc_smsg_init. (%d)\n", ds->rpc_s->ptlmap.id, err);
		goto err_out;
	}


	// allgather APP smsg_attr[rpc+sys]
    gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(ds->size_sp * sizeof(gni_smsg_attr_t));

    allgather(&cur_attr_info->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), ds->comm);

    if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

	for(j=0;j<ds->size_sp;j++){
		if(j == ds->rpc_s->ptlmap.id)
			continue;

		ds->peer_tab[j].remote_smsg_attr = remote_smsg_rpc_array[j];

	}

	free(remote_smsg_rpc_array);


	// smsg_config
	for(i=0;i<ds->num_sp;i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;

		peer = &ds->peer_tab[i];

		err = rpc_smsg_config(ds->rpc_s, ds->rpc_s->attr_info_start, peer);
		if (err != 0){
		  printf("Rank 0: failed for config RPC SMSG for peer %i is %d. (%d)\n", peer->ptlmap.id, i, err);
			goto err_out;
		}
	}

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

// Thread for master server to create listening port, waiting for connection and taking care of application join-in.

/*
1. open listening 
2. get connection from an app

3. recv num of client from this app, recv ptlmap of all client from this app
4. set up EP, smsg init for app

5. send back ptlmap info to master client of this app

6. recv attr from master client of this app
7. smsg config

8. send master server attr to master client of this app

*/

static int ds_master_listen(struct dart_server *ds)
{
	int i, j, err, p, count = 0;// count is number of total cp (all registered apps)
	int cur_cp = 0; //DSaaS: number of cp in one particular app.
	char *localip;
	gni_return_t status;
	struct node_id *peer, *cur_cn_peer;
	struct gni_smsg_attr_info *cur_attr_info;
	//gni_smsg_attr_t *smsg_attr;

	int k, connect_num = 0;

	int tmp_size=0;
	int info_size, appid;
	void *recv_buffer;
	void *send_buffer;
	struct ptlid_map *dcreg, *dcreg_tmp;
	struct smsg_attr_reg *sar;

	// DYNAMIC for app and server.

	// get socket address, open listening port

	localip = ip_search();
	ds->rpc_s->address.address.sin_addr.s_addr = inet_addr(localip);
	ds->rpc_s->address.address.sin_port = htons(0);
	ds->rpc_s->address.address.sin_family = AF_INET;	

	// DSaaS need add a thread and make it alive all the time
	err = listen_sock_init(0, localip, ds->rpc_s->address.address.sin_port, &ds->rpc_s->address.sockfd);
	if(err != 0 || ds->rpc_s->address.sockfd < 0)
		goto err_out;

	err = port_search(ds->rpc_s->address.sockfd);
	if(err<0)
	  goto err_out;

	ds->rpc_s->address.address.sin_port = htons(err);

	//printf("ip=%s port=%d\n", inet_ntoa(ds->rpc_s->address.address.sin_addr), ntohs(ds->rpc_s->address.address.sin_port));
	//printf("nid=%d pid=%d\n", ds->rpc_s->ptlmap.nid, ds->rpc_s->ptlmap.pid);

	//use socket address
	err = rpc_write_socket(ds->rpc_s);
	if (err != 0)
		goto err_out;

	//1. recv APP msg_size + real_msg[all peer ptlmap info]
	for(k=0;k<CONNMAX;k++)
		connectfd[k]=-1;

	k = 0;

	while(ds->thread_alive){

		//1. connect to app
		connectfd[k] = accept(ds->rpc_s->address.sockfd, NULL, NULL);
		if(0 > connectfd[k]){
			perror("error accept failed");
			continue;
		}

		connect_num++;
		ds->app_num++;


		//2. recv app ptlmap info
		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
			if (-1 == err) {
				perror("error receive failed");
				goto err_out;
			}
			else if (0 == err) {
				printf("%s(): recv return 0?\n",__func__);
			}

			tmp_size += err;

			if(sizeof(int)<=tmp_size)
				break;
		}

		recv_buffer = malloc(info_size);
		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+recv_buffer, info_size-tmp_size, 0);
			if (-1 == err) {
				perror("error receive failed");
				goto err_out;
			}
			else if (0 == err) {
				printf("%s(): recv return 0?\n",__func__);
			}

			tmp_size += err;

			if(info_size<=tmp_size)
				break;
		}

		cur_cp = info_size/sizeof(struct ptlid_map);
		dcreg = (struct ptlid_map *)recv_buffer;

		//3. init new peer_tab block for this app
		peer = ds->peer_tab + ds->size_sp - 1;

		while(1){
		  if(!(peer->next)){
				peer->next = (struct node_id *)malloc(cur_cp * sizeof(struct node_id));
				peer = peer->next;
				break;
			}
			else
				peer = (struct node_id *)(peer->next + peer->next->peer_num - 1);		
		}

		cur_cn_peer = peer;


		//4. set up new peer_tab block for this app
                for(j=0;j<cur_cp;j++){
			appid = dcreg->appid;

			peer->ptlmap.nid = dcreg->nid;
			peer->ptlmap.pid = dcreg->pid;
			peer->ptlmap.appid = dcreg->appid;
			peer->ptlmap.id = dcreg->id+count+ds->num_sp;		
			peer->peer_rank = j;
			peer->peer_num = cur_cp;

			//modify recv_buffer since it will be send back with now id
			dcreg->id = dcreg->id+count+ds->num_sp;

			peer->f_reg = 1;

			INIT_LIST_HEAD(&peer->req_list);
			peer->num_msg_at_peer = ds->rpc_s->max_num_msg;

			peer->num_msg_recv = 0;
			peer->num_msg_ret = 0;	  

			peer->sys_msg_recv = 0;
			peer->sys_msg_at_peer = ds->rpc_s->max_num_msg;
			peer->sys_msg_ret = 0;	 
			peer->next = NULL;

			status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &peer->ep_hndl);
			if (status != GNI_RC_SUCCESS)
			{
				printf("Fail: GNI_EpCreate returned error. %d.\n", status);
				goto err_free;
			}

			status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
			if (status != GNI_RC_SUCCESS)
			{
				printf("Fail: GNI_EpBind returned error. %d.\n", status);
				goto err_free;
			}

			status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
			if (status != GNI_RC_SUCCESS)
			{
				printf("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
				goto err_out;
			}

			status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
			if (status != GNI_RC_SUCCESS)
			{
				printf("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
				goto err_out;
			}

			dcreg++;
			peer++;

        }


		cur_attr_info = ds->rpc_s->attr_info_start;
		while(cur_attr_info->next)
		  cur_attr_info = cur_attr_info->next;

		cur_attr_info->next = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info)); //// DSaaS
		memset(cur_attr_info->next, 0, sizeof(struct gni_smsg_attr_info));
		cur_attr_info = cur_attr_info->next;
		cur_attr_info->appid = appid;
		cur_attr_info->remote_smsg_attr = (gni_smsg_attr_t *)malloc(ds->num_sp*sizeof(gni_smsg_attr_t));
		memset(cur_attr_info->remote_smsg_attr, 0, ds->num_sp * sizeof(gni_smsg_attr_t));
		cur_attr_info->next = NULL;

		err = rpc_smsg_init(ds->rpc_s, cur_attr_info, cur_cp);
		if (err != 0){
			printf("Rank %d: failed for rpc_smsg_init. (%d)\n", ds->rpc_s->ptlmap.id, err);
			goto err_out;
		}

		// send ALL msg_size + real_msg[all peer ptlmap info]
		info_size = (ds->num_sp + cur_cp) * sizeof(struct ptlid_map);
		send_buffer = malloc(info_size);
		dcreg = (struct ptlid_map *)send_buffer;
		dcreg_tmp = (struct ptlid_map *)recv_buffer;

		for(j=0;j<ds->num_sp + cur_cp;j++){
			if(j<ds->num_sp){
				dcreg->nid = ds->peer_tab[j].ptlmap.nid;
				dcreg->pid = ds->peer_tab[j].ptlmap.pid;
				dcreg->appid = ds->peer_tab[j].ptlmap.appid;
				dcreg->id = ds->peer_tab[j].ptlmap.id;
			}
			else{
				dcreg->nid = dcreg_tmp->nid;
				dcreg->pid = dcreg_tmp->pid;
				dcreg->appid = dcreg_tmp->appid;
				dcreg->id = dcreg_tmp->id;
				dcreg_tmp++;
			}
			dcreg++;
		}

		free(recv_buffer);

		tmp_size = 0;
		while(1){
			err = send(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
			if (-1 == err) {
				perror("error send failed");
				goto err_out;
		    }

		    tmp_size += err;

		    if(sizeof(int)<=tmp_size)
			    break;
	    }   

	    tmp_size = 0;
	    while(1){
		    err = send(connectfd[k], tmp_size+send_buffer, info_size-tmp_size, 0);
		    if (-1 == err) {
			    perror("error send failed");
			    goto err_out;
		    }

		    tmp_size += err;

			if(info_size<=tmp_size)
				break;
		}	

		free(send_buffer);	


		// 5. recv APP msg_size + smsg_attr[rpc]
		gni_smsg_attr_t	*smsg_attr;

		info_size = 0;
		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
			if (-1 == err) {
				perror("error receive failed\n");
				goto err_out;
			}
			else if (0 == err) {
				printf("%s(): recv return 0?\n",__func__);
			}

			tmp_size += err;

			if(sizeof(int)<=tmp_size)
				break;
		}

		recv_buffer = malloc(info_size);
		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+recv_buffer, info_size-tmp_size, 0);
			if (-1 == err) {
				perror("error receive failed\n");
				goto err_out;
			}
			else if (0 == err) {
				printf("%s(): recv return 0?\n",__func__);
			}

			tmp_size += err;

			if(info_size<=tmp_size)
				break;
		}


		peer = cur_cn_peer;//DSaaS	
		smsg_attr = (gni_smsg_attr_t *)recv_buffer;


		for(j=0;j<info_size/sizeof(gni_smsg_attr_t);j++){
			peer->remote_smsg_attr = *smsg_attr;
			peer++;
			smsg_attr = smsg_attr+1;
		}

		free(recv_buffer);


		// smsg_config
		peer = cur_cn_peer;
		for(i=0;i<cur_cp;i++){
			err = rpc_smsg_config(ds->rpc_s, cur_attr_info, peer);////DSaaS
			if (err != 0){
			  printf("Rank 0: failed for config RPC SMSG for peer %d i is %d. (%d)\n", peer->ptlmap.id, i, err);
				goto err_out;
			}
			peer++;
		}

		// send master server smsg_attr[rpc]
		info_size = sizeof(gni_smsg_attr_t);//DSaaS
		send_buffer = malloc(info_size);
		smsg_attr = (gni_smsg_attr_t *)send_buffer;
		*smsg_attr = cur_attr_info->local_smsg_attr;

		tmp_size = 0;
		while(1){
			err = send(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
			if (-1 == err) {
				perror("error send attr size failed");
				goto err_out;
			}

			tmp_size += err;

			if(sizeof(int)<=tmp_size)
				break;
		}

		tmp_size = 0;
		while(1){
			err = send(connectfd[k], tmp_size+send_buffer, info_size-tmp_size, 0);
			if (-1 == err) {
				perror("error send attr failed");
				goto err_out;
			}

			tmp_size += err;

			if(info_size<=tmp_size)
				break;
		}	

		free(send_buffer);

		//rpc_peer_check(ds->rpc_s);

		//close(connectfd[k]);
		// Done with this new app. Continue on waiting for the next ...
		k++;
		count = count+cur_cp;
		cur_cp = 0;

	}


	// 8. free connection, close socket
	/*	for(k=0; k<connect_num;k++){
			close(connectfd[k]);
		}
	*/

		pthread_exit(0);

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	pthread_exit(0);
	return err;
}


static int ds_boot_master(struct dart_server *ds)
{
	struct node_id *peer;
        struct stat  st_buff;
	const char fil_conf[] = "conf";
        int i, fd, err = -ENOMEM;

	err = ds_master_init(ds);
	if (err != 0){
		printf("Rank 0: failed for ds_master_init. (%d)\n", err);
		goto err_out;
	}

	//printf("PASS ds_master_init.\n");//debug

	return 0;

 err_out:
        printf("'%s()': failed with %d.\n", __func__, err);
        return err;		

}

/*
1. bcast slave servers
2. EpCreate+Epbind+smsg_init(rpc+sys)

3. allgather APP smsg_attr[rpc+sys]
4. bcast slave servers
5. smsg_config

*/
static int ds_boot_slave(struct dart_server *ds)
{
        int i,j;
        int err = -ENOMEM;
	struct node_id *peer;
	struct ptlid_map * ptlmap;
	struct peer_attr_reg *peer_attr = (struct peer_attr_reg *)malloc(ds->peer_size * sizeof(struct peer_attr_reg));
	void *recv_buffer;
	gni_smsg_attr_t *smsg_attr;
	gni_return_t status;
	struct gni_smsg_attr_info *cur_attr_info;


	// EpCreate+Epbind+smsg_init(rpc+sys)
	for(i=0;i<ds->num_sp; i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;

		peer = &ds->peer_tab[i];
		peer->peer_rank = i;
		peer->peer_num = ds->num_sp;


		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &peer->ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		
		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			printf("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}	

	cur_attr_info = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info));
	memset(cur_attr_info, 0, sizeof(struct gni_smsg_attr_info));	
	ds->rpc_s->attr_info_start = cur_attr_info;
	cur_attr_info->next = NULL;

	err = rpc_smsg_init(ds->rpc_s, cur_attr_info, ds->num_sp);
	if (err != 0){
        printf("Rank %d: failed for rpc_smsg_init. (%d)\n", ds->rpc_s->ptlmap.id, err);
		goto err_out;
	}
	// allgather server smsg_attr[rpc]
        gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(ds->size_sp * sizeof(gni_smsg_attr_t));

	allgather(&cur_attr_info->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), ds->comm);
    if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

	for(j = 0; j < ds->size_sp; j++) {
	    if(j == ds->rpc_s->ptlmap.id) {
	        continue;
        }
		
        ds->peer_tab[j].remote_smsg_attr = remote_smsg_rpc_array[j];
	}

	free(remote_smsg_rpc_array);


	// server smsg_config
	for(i=0;i<ds->size_sp;i++){////DSaaS
		if(i == ds->rpc_s->ptlmap.id)
			continue;
		
		peer = &ds->peer_tab[i];

		err = rpc_smsg_config(ds->rpc_s, ds->rpc_s->attr_info_start, peer);
		if (err != 0){
			printf("Rank %d: failed for config SMSG for peer %d. (%d)\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}

	}


	return 0;

 err_free:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;

 err_out:
        printf("'%s()': failed with %d.\n", __func__, err);
        return err;	
}

static int ds_boot(struct dart_server *ds)
{
	int i, err = -ENOMEM;
	struct app_info *app;
	struct app_info *current_app;
	int rc=0;

	if(ds->rpc_s->ptlmap.id == 0){
		err = ds_boot_master(ds);
		if(err!=0)
			goto err_out;
	}
	else{
		err = ds_boot_slave(ds);
		if(err!=0)
			goto err_out;
	}

	//fill in the self

	for(i=0; i<ds->num_sp; i++)
	{
		if (ds->peer_tab[i].ptlmap.nid == ds->rpc_s->ptlmap.nid && ds->peer_tab[i].ptlmap.pid == ds->rpc_s->ptlmap.pid) 
		{
			ds->self = &ds->peer_tab[i];
			break;
		}
	}

	//mark it to registered
	ds->f_reg = 1;

    if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }
	
    if(ds->rpc_s->ptlmap.id == 0){
	  ds->thread_alive = 1;
	  rc = pthread_create(&(ds->comm_thread), NULL, ds_master_listen, (void *)ds);////DSaaS
	  if(rc) {
		printf("ERROR; return code from pthread_create() is %d\n", rc);
		exit(-1);
	  }



	}

	return 0;
 err_out:
	printf("'%s()': failed.\n", __func__);
	return -1;
}

/*
  Public API starts here.
*/

/* 
   Allocate and initialize dart server; the server initializes rpc
   server. 
*/
struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm)
{
	struct dart_server *ds = 0;
	struct node_id *peer;
	struct node_id *peer_list;
	size_t size;
	int i, err;
	int node_size;

	size = sizeof(struct dart_server) + num_sp * sizeof(struct node_id);
	ds = calloc(1, size);
	if (!ds)
		goto err_out;
	ds->dart_ref = dart_ref;
	ds->peer_tab = (struct node_id *) (ds+1);
	ds->peer_size = num_sp;
	ds->size_sp = num_sp;
	ds->num_sp = num_sp;
	INIT_LIST_HEAD(&ds->app_list);

    if(comm) {
        ds->comm = malloc(sizeof(*ds->comm));
        MPI_Comm_dup(*(MPI_Comm *)comm, ds->comm);
    } else {
        ds->comm = NULL;
    }

	ds->rpc_s = rpc_server_init(30, num_sp, ds, DART_SERVER, 0, ds->comm);
	if (!ds->rpc_s)
		goto err_free_dsrv;

	ds->rpc_s->app_num_peers = num_sp;
	ds->rpc_s->app_minid = 0;

	for (i=0;i<ds->size_sp;i++)
	{
		ds->peer_tab[i].ptlmap.nid = ds->rpc_s->peer_tab[i].ptlmap.nid;
		ds->peer_tab[i].ptlmap.pid = ds->rpc_s->peer_tab[i].ptlmap.pid;
		ds->peer_tab[i].ptlmap.id = ds->rpc_s->peer_tab[i].ptlmap.id;
		ds->peer_tab[i].ptlmap.appid = 0;
		ds->peer_tab[i].peer_rank = ds->rpc_s->peer_tab[i].ptlmap.id;
		ds->peer_tab[i].peer_num = ds->num_sp;
	}
	free(ds->rpc_s->peer_tab);
	ds->rpc_s->peer_tab = ds->peer_tab;

	peer = ds->peer_tab;
	for (i = 0; i < ds->size_sp; i++) 
	{
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;

		peer->num_msg_recv = 0;
		peer->num_msg_ret = 0;	  

		peer->sys_msg_recv = 0;
		peer->sys_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->sys_msg_ret = 0;
		peer->next = NULL;	 

		peer++;
	}

	rpc_add_service(cn_register, dsrpc_cn_register);
        rpc_add_service(cn_unregister, dsrpc_cn_unregister);
        rpc_add_service(sp_announce_cp, dsrpc_announce_cp);

	err = ds_boot(ds);
	if (err < 0)
		goto err_free_dsrv;

	if (num_sp == 1)
	{
		// If there is a single server, mark it as registered, but it should also be master!
		ds->f_reg = 1;
	}

   if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    } 

	uloga("'%s(%d)': init ok.\n", __func__, ds->self->ptlmap.id);

	return ds;

 err_free_dsrv:
	printf("'%s()': failed with %d.\n", __func__, err);
	free(ds);
	return NULL;
 err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return NULL;
}

int print_ds(struct dart_server *ds)
{
  int i;

  printf("ds is:\n ds->rpc_s: ptlmap[nid(%d),pid(%d),id(%d),appid(%d)];\n num_buf(%d);num_rpc_per_buff(%d);max_num_msg(%d);com_type(%d);num_peers(%d);bar_num(%d);app_minid(%d);app_num_peers(%d);num_md_posted(%d);num_md_unlinked(%d);\n ds->peer_size(%d), ds->num_cp(%d), ds->num_sp(%d), ds->size_cp(%d), ds->size_sp(%d), f_reg(%d), f_stop(%d), f_nacc(%d);\n ds->self:[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", ds->rpc_s->ptlmap.nid, ds->rpc_s->ptlmap.pid, ds->rpc_s->ptlmap.id, ds->rpc_s->ptlmap.appid,ds->rpc_s->num_buf, ds->rpc_s->num_rpc_per_buff, ds->rpc_s->max_num_msg, ds->rpc_s->cmp_type, ds->rpc_s->num_peers, ds->rpc_s->bar_num, ds->rpc_s->app_minid, ds->rpc_s->app_num_peers, ds->rpc_s->num_md_posted, ds->rpc_s->num_md_unlinked, ds->peer_size,ds->num_cp, ds->num_sp, ds->size_cp, ds->size_sp, ds->f_reg, ds->f_stop, ds->f_nacc, ds->self->ptlmap.nid, ds->self->ptlmap.pid, ds->self->ptlmap.id, ds->self->ptlmap.appid, ds->self->num_req, ds->self->f_req_msg, ds->self->f_need_msg, ds->self->num_msg_at_peer, ds->self->num_msg_ret);

for(i=0; i<ds->num_sp;i++)
  {
  printf("ds->peer_tab(%d):[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", i, ds->peer_tab[i].ptlmap.nid, ds->peer_tab[i].ptlmap.pid, ds->peer_tab[i].ptlmap.id, ds->peer_tab[i].ptlmap.appid, ds->peer_tab[i].num_req, ds->peer_tab[i].f_req_msg, ds->peer_tab[i].f_need_msg, ds->peer_tab[i].num_msg_at_peer, ds->peer_tab[i].num_msg_ret);
  }

 return 0;

}


void ds_free(struct dart_server *ds)//not done
{
	struct app_info *app, *t;
	int err;

	int track = ds->self->ptlmap.id;//debug

	err = rpc_server_free(ds->rpc_s, ds->comm);//not done
	if(err!=0)
		printf("(%s): rpc server free failed.\n", __func__);
	//printf("Rank(%d): step2.1.\n",track);//debug
	list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry) 
	{
		list_del(&app->app_entry);
		free(app);
        }
	//printf("Rank(%d): step2.2.\n",track);//debug
    if(ds->comm) {
        MPI_Comm_free(ds->comm);
        free(ds->comm);
    }
	free(ds);
}

int ds_process(struct dart_server *ds)
{
    if (ds->f_reg) {
        rpc_process_msg_resend(ds->rpc_s, ds->peer_tab, ds->num_sp);
    }
	return rpc_process_event(ds->rpc_s);
}

