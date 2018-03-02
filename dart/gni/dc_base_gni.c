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

// Modified in August 2013 for scalability. Marked by #SCA.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <mpi.h>

#include "dc_base_gni.h"

static int connectfd;

static struct node_id* create_app_peer_tab(const struct ptlid_map* buffer, struct dart_client *dc)
{
    // Format of 'buffer':
    // | peers info of dataspaces servers | peers info of current application |
    struct node_id* peer_tab = malloc(sizeof(struct node_id)*dc->num_cp);
    if (!peer_tab) goto err_out;
    memset(peer_tab, 0, sizeof(struct node_id)*dc->num_cp);

    gni_return_t status;
    struct node_id* peer = peer_tab;
    int i = dc->num_sp; // Starting index for peers info of current application
    for(; i < dc->peer_size; i++, peer++) {
        INIT_LIST_HEAD(&peer->req_list);
        peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
        peer->num_msg_recv = 0;
        peer->num_msg_ret = 0;
        peer->sys_msg_recv = 0;
        peer->sys_msg_at_peer = dc->rpc_s->max_num_msg;
        peer->sys_msg_ret = 0;

        peer->ptlmap.nid = buffer[i].nid;
        peer->ptlmap.pid = buffer[i].pid;
        peer->ptlmap.appid = buffer[i].appid;
        peer->ptlmap.id = buffer[i].id;
        peer->next = NULL;
        peer->peer_rank = peer->ptlmap.id;
        peer->peer_num = dc->num_cp;

        if (peer->ptlmap.id == dc->rpc_s->ptlmap.id) continue;
 
        status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->src_cq_hndl, &peer->ep_hndl);
        if (status != GNI_RC_SUCCESS)
        {
            uloga("%s(): ERROR! GNI_EpCreate returned %d.\n", __func__, status);
            goto err_out;
        }
        status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
        if (status != GNI_RC_SUCCESS)
        {
            uloga("%s(): ERROR! GNI_EpBind returned %d.\n", __func__, status);
            goto err_out;
        }
    /*
        status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
        if (status != GNI_RC_SUCCESS)
        {
            uloga("%s(): ERROR! Rank %d GNI_EpCreate SYS returned %d.\n", __func__, dc->rpc_s->ptlmap.id, status);
            goto err_out;
        }
        status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
        if (status != GNI_RC_SUCCESS)
        {
            uloga("%s(): ERROR! Rank %d GNI_EpBind SYS returned %d.\n", __func__, dc->rpc_s->ptlmap.id, status);
            goto err_out;
        }
    */
    }

    return peer_tab;
 err_out:
    if (peer_tab) free(peer_tab);
    uloga("%s(): failed.\n", __func__);
    return NULL;
}

static void peer_tab_list_push_back(struct node_id* head_peer_tab, struct node_id* new_peer_tab)
{
    struct node_id *peer = head_peer_tab+head_peer_tab->peer_num-1; // Move to the last peer of head_peer_tab
    while (1) {
        if (!peer->next) {
            peer->next = new_peer_tab;
            break;
        } else {
            peer = peer->next + peer->next->peer_num - 1;
        }
    } 
}

static int barrier_broadcast(struct dart_client *dc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int i, err;

	for (i = 1; i < dc->cp_in_job; i++) 
	{
		peer = dc_get_peer(dc, dc->self->ptlmap.id+i);

		err = -ENOMEM;
		msg = msg_buf_alloc(dc->rpc_s, peer, 1);
		if (!msg) 
			break;

		msg->msg_rpc->cmd = cp_barrier;
		msg->msg_rpc->id = dc->self->ptlmap.id;

		err = rpc_send(dc->rpc_s, peer, msg);
		if (err < 0) 
		{
			free(msg);
			break;
		}
	}

	dc->cp_barrier_req = 0;
	dc->f_bar = 1;

	if (i == dc->cp_in_job)
		return 0;
	return err;
}

/* 
   Implement a simple 'start' barrier across the nodes in an app job.
*/
static int dcrpc_barrier(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	int err;

	if (dc->self->ptlmap.id == dc->cp_min_rank) 
	{
		/* I am the master peer in this job. */
		dc->cp_barrier_req++;
		if (dc->cp_barrier_req < dc->cp_in_job)
			return 0;
		printf("'%s()': I am master rank %d, and all peers joined the barrier.\n", __func__, dc->self->ptlmap.id);

		err = barrier_broadcast(dc);
		if (err < 0)
		goto err_out;
	}
	else 
	{
		/* Non master peer in this job. */
		dc->f_bar = 1;
	}

	return 0;
 err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
  RPC routine  to wait for  server confirmation that it  processed our
  unregister message and all our other messages.
*/
static int dcrpc_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_client *dc = dc_ref_from_rpc(rpc_s);

	dc->f_reg = 0;

	return 0;
}

static int dc_unregister(struct dart_client *dc)
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;

	sp_index = 0;
	peer = dc_get_peer(dc, sp_index);
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg) 
		goto err_out;

	msg->msg_rpc->cmd = cn_unregister;
	msg->msg_rpc->id = dc->self->ptlmap.id;

	hreg = (struct hdr_register *)(msg->msg_rpc->pad);

	hreg->pm_cp.appid = dc->self->ptlmap.appid;
	hreg->pm_cp.id = dc->self->ptlmap.id;
	hreg->pm_cp.pid = dc->self->ptlmap.pid;
	hreg->pm_cp.nid = dc->self->ptlmap.nid;

	hreg->pm_sp.appid = peer->ptlmap.appid;
	hreg->pm_sp.id = peer->ptlmap.id;
	hreg->pm_sp.pid = peer->ptlmap.pid;
	hreg->pm_sp.nid = peer->ptlmap.nid;

	hreg->num_sp = dc->num_sp;
	hreg->num_cp = 1;

	err = rpc_send(dc->rpc_s, peer, msg);
	if (err < 0) 
		goto err_out_free;

	while (dc->f_reg) 
	{
		err = rpc_process_event(dc->rpc_s);
		if (err < 0)
			goto err_out;
	}

        return 0;
 err_out_free:
	free(msg);
 err_out:
	printf("'%s()': failed.\n", __func__);
	return err;
}

static int dc_connect_init(struct dart_client *dc, struct sockaddr_in *dest)
{
    struct sockaddr_in dest_addr;
    dest_addr = *dest;
    dest_addr.sin_family = AF_INET;

    int Res, i=0;
    int SocketFD = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
     
    Res = inet_pton(AF_INET, inet_ntoa(dest_addr.sin_addr), &dest_addr.sin_addr);

    if (-1 == SocketFD)
    {
      printf("Rank %d: Cannot create socket in %s.\n", dc->rpc_s->ptlmap.id, __func__);
        exit(EXIT_FAILURE);
    }
     
    if (0 > Res)
    {
      printf("Rank %d: first parameter is not a valid address family, error %d in %s.\n", dc->rpc_s->ptlmap.id, Res, __func__);
        close(SocketFD);
        exit(EXIT_FAILURE);
    }
    else if (0 == Res)
    {
      printf("Rank %d: char string (second parameter does not contain valid ipaddress in %s.\n", dc->rpc_s->ptlmap.id, __func__);
        close(SocketFD);
        exit(EXIT_FAILURE);
    }

    while(i<1000){
      if (-1 == connect(SocketFD, (struct sockaddr *)&dest_addr, sizeof dest_addr))
          printf("Rank %d: connect failed in %s.\n", dc->rpc_s->ptlmap.id, __func__);
      else
          break;
          i++;
    }
    if(i==1000){
          close(SocketFD);
          exit(EXIT_FAILURE);
    }

    return SocketFD;
}


static int dc_register_at_master(struct dart_client *dc, int appid)
{
        struct msg_buf *msg;
        struct hdr_register *hr;
        struct node_id *peer;
        int err;
        gni_return_t status;

        peer = dc->peer_tab;

	INIT_LIST_HEAD(&peer->req_list);
	peer->num_msg_at_peer = dc->rpc_s->max_num_msg;
	peer->num_msg_recv = 0;
	peer->num_msg_ret = 0;	  
	peer->sys_msg_recv = 0;
	peer->sys_msg_at_peer = dc->rpc_s->max_num_msg;
	peer->sys_msg_ret = 0;	


	err = -ENOMEM;
        msg = msg_buf_alloc(dc->rpc_s, peer, 1);
        if (!msg) 
                goto err_out;
        msg->msg_rpc->cmd = cn_register;

        hr = (struct hdr_register *) msg->msg_rpc->pad;
        hr->pm_sp = peer->ptlmap;
        hr->pm_cp = dc->rpc_s->ptlmap;
	hr->num_sp = dc->num_sp;
	hr->num_cp = dc->num_cp;
	hr->id_min = dc->rpc_s->ptlmap.id;

        err = rpc_send(dc->rpc_s, peer, msg);
        if (err < 0)
                goto err_free;

        do {
                err = rpc_process_event(dc->rpc_s);
                if (err < 0)
                        goto err_out;
        }
        while (!dc->f_reg);

        return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;

}

/*
1. collect all app peers ptlmap info, then send APP msg_size + real_msg[all peer ptlmap info] to master server
2. recv ALL msg_size + real_msg[all peer ptlmap info], set up peer_tab, bcast slave clients
3. EpCreate+Epbind+smsg_init(rpc) 

4. allgather APP smsg_attr[rpc]
5. send APP msg_size + smsg_attr[rpc] to master server
6. recv APP msg_size + smsg_attr[rpc] of master server 
7. call dc_register, send cn_register and attr to master server
8. recv attr of all servers, then broadcast to slave clients
9. smsg_config
10. Done with registration.
*/

static int dc_master_init(struct dart_client *dc)
{

	int i, j, k, err;
	struct ptlid_map *dcreg;
	struct node_id *peer;

	void *send_buffer, *recv_buffer;
	int info_size, tmp_size;

	int check = 0;
	int sp = 0;

	gni_return_t status;
	struct sockaddr_in address;
	gni_smsg_attr_t * smsg_attr;

	err = rpc_read_socket(&address);
	if (err != 0)
		goto err_out;

	connectfd = dc_connect_init(dc, &address);
	if(connectfd < 0)
		goto err_out;	

	// 1. collect all app peers ptlmap info, then send master_server APP msg_size + real_msg[all peer ptlmap info]	
	info_size = dc->num_cp * sizeof(struct ptlid_map);
	send_buffer = malloc(info_size);

	dcreg = (struct ptlid_map *)send_buffer;
	for(i=0;i<dc->num_cp;i++){
		dcreg->nid = dc->rpc_s->peer_tab[i].ptlmap.nid;
		dcreg->pid = dc->rpc_s->peer_tab[i].ptlmap.pid;
		dcreg->appid = dc->rpc_s->peer_tab[i].ptlmap.appid;
		dcreg->id = dc->rpc_s->peer_tab[i].ptlmap.id;
		dcreg++;
	}

	tmp_size = 0;
	while(1){
		err = send(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
		if (-1 == err) {
			perror("error send failed\n");
			goto err_out;
		}

		tmp_size += err;

		if(sizeof(int)<=tmp_size)
			break;
	}

	tmp_size = 0;
	while(1){
		err = send(connectfd, tmp_size+send_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			perror("error send failed\n");
			goto err_out;
		}

		tmp_size += err;

		if(info_size<=tmp_size)
			break;
	}		
	free(send_buffer);

	// 2. recv ALL msg_size + real_msg[all peer ptlmap info] from master_server, bcast to slave clients	
	info_size = 0;
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
		if (-1 == err) {
			perror("error receive failed\n");
			goto err_out;
		}
		else if (0 == err) {
			uloga("%s(): recv return 0?\n",__func__);
		}

		tmp_size += err;

		if(sizeof(int)<=tmp_size)
			break;
	}

	recv_buffer = malloc(info_size);
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+recv_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			perror("error receive failed");
			goto err_out;
		}
		else if (0 == err) {
			uloga("%s(): recv return 0?\n",__func__);
		}

		tmp_size += err;

		if(info_size<=tmp_size) {
			break;
		}
	}

	dc->peer_size = info_size/sizeof(struct ptlid_map); //DSaaS: peer_size = num_cp + num_sp (num_cp is number of peers in the same app)
	dc->rpc_s->num_rpc_per_buff = info_size/sizeof(struct ptlid_map) - dc->num_cp; //DSaaS
	dc->num_sp = dc->rpc_s->num_rpc_per_buff;	

	dc->peer_tab = (struct node_id *)malloc(dc->rpc_s->num_rpc_per_buff * sizeof(struct node_id)); //// DSaaS
	memset(dc->peer_tab, 0, dc->rpc_s->num_rpc_per_buff * sizeof(struct node_id));

	free(dc->rpc_s->peer_tab);
	dc->rpc_s->peer_tab = dc->peer_tab;

	dcreg = (struct ptlid_map *)recv_buffer;
	peer = dc->peer_tab;
	check = 0;
	for (i = 0; i < dc->peer_size; i++) 
	{
		if(i<dc->rpc_s->num_rpc_per_buff){
		        INIT_LIST_HEAD(&peer->req_list);
		        peer->num_msg_at_peer = dc->rpc_s->max_num_msg;

	                peer->num_msg_recv = 0;
		        peer->num_msg_ret = 0;	  

		        peer->sys_msg_recv = 0;
		        peer->sys_msg_at_peer = dc->rpc_s->max_num_msg;
		        peer->sys_msg_ret = 0;	

			peer->ptlmap.nid = dcreg->nid;
			peer->ptlmap.pid = dcreg->pid;
			peer->ptlmap.appid = dcreg->appid;
			peer->ptlmap.id = dcreg->id;
			peer->next = NULL;
			peer->peer_rank = peer->ptlmap.id;
			peer->peer_num = dc->rpc_s->num_rpc_per_buff;

		}

		else{
		  if((dc->rpc_s->ptlmap.appid == dcreg->appid) && (i == dc->rpc_s->num_rpc_per_buff))
			dc->rpc_s->app_minid = dc->cp_min_rank = dcreg->id;

		  if((dc->rpc_s->ptlmap.pid == dcreg->pid) && (dc->rpc_s->ptlmap.nid == dcreg->nid)){
			dc->rpc_s->ptlmap.id = dcreg->id;
			break;
		  }
		}

		dcreg++;
		peer++;
	}

    if (dc->comm) {
        // MPI_Bcast to all slave clients
        err = MPI_Bcast(&dc->peer_size, 1, MPI_INT, 0, *dc->comm);
        assert(err == MPI_SUCCESS);
        err = MPI_Bcast(recv_buffer, dc->peer_size * sizeof(struct ptlid_map), MPI_BYTE, 0, *dc->comm);
        if (err != MPI_SUCCESS) {
        uloga("Rank %d: failed for broadcast Address information to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);
                goto err_out;
        }
    } else {
        // PMI_Bcast to all slave clients.
        PMI_Bcast(&dc->peer_size, sizeof(int));

        err = PMI_Bcast(recv_buffer, dc->peer_size * sizeof(struct ptlid_map));
        if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast Address information to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);            
        goto err_out;
        }
    }

	// Create peer_tab for all peers of current application
	struct node_id* app_peer_tab = create_app_peer_tab(recv_buffer, dc);
	if (app_peer_tab) {
		// Add peer_tab to the end of the linked list chained into dc->peer_tab
		peer_tab_list_push_back(dc->peer_tab, app_peer_tab);
	}
	free(recv_buffer);

	// 3. EpCreate+Epbind+smsg_init(rpc+sys) 
	peer = dc->peer_tab;
	for(i=0;i<dc->rpc_s->num_rpc_per_buff; i++, peer++){
		status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->src_cq_hndl, &peer->ep_hndl);	
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

	k = 0;
	sp = dc->rpc_s->num_rpc_per_buff;

	dc->rpc_s->attr_info_start = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info)); //// DSaaS
	memset(dc->rpc_s->attr_info_start, 0, sizeof(struct gni_smsg_attr_info));

    err = rpc_smsg_init(dc->rpc_s, dc->rpc_s->attr_info_start, sp);
    if (err != 0){
        uloga("Rank %d: failed for rpc_smsg_init %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
        goto err_out;
    }

	// 4. allgather APP smsg_attr[rpc+sys]
    gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(dc->num_cp * sizeof(gni_smsg_attr_t));

    allgather(&dc->rpc_s->attr_info_start->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), dc->comm);
    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
            assert(err == PMI_SUCCESS);
    }

	// 5. send APP msg_size + smsg_attr[rpc+sys]
	info_size = dc->num_cp * sizeof(gni_smsg_attr_t);
	send_buffer = malloc(info_size);

	smsg_attr = send_buffer;
	for(i=0,j=0;j<dc->num_cp;j++,i++){
		smsg_attr[i] = remote_smsg_rpc_array[j];
	}	

	free(remote_smsg_rpc_array);

	tmp_size = 0;
	while(1){
		err = send(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
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
		err = send(connectfd, tmp_size+send_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			perror("error send attr failed");
			goto err_out;
		}

		tmp_size += err;

		if(info_size<=tmp_size)
			break;
	}		
	free(send_buffer);

	// 6. recv APP msg_size + smsg_attr[rpc+sys], from master server.
	info_size = 0;
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
		if (-1 == err) {
			perror("error receive failed\n");
			goto err_out;
		}
		else if (0 == err) {
			uloga("%s(): recv return 0?\n",__func__);
		}

		tmp_size += err;

		if(sizeof(int)<=tmp_size)
			break;
	}

	recv_buffer = malloc(info_size);
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+recv_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			perror("error receive failed\n");
			goto err_out;
		}
		else if (0 == err) {
			uloga("%s(): recv return 0?\n",__func__);
		}

		tmp_size += err;

		if(info_size<=tmp_size)
			break;
	}

	peer = dc->peer_tab;
	smsg_attr = (gni_smsg_attr_t *)recv_buffer;

	peer->remote_smsg_attr = *smsg_attr;
	free(recv_buffer);

	err = rpc_smsg_config(dc->rpc_s, dc->rpc_s->attr_info_start, peer);
	if (err != 0) {
		  uloga("Rank %d: failed for config SMSG for peer %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
		  goto err_out;
	}

	err = dc_register_at_master(dc, dc->rpc_s->ptlmap.appid);
	if (err != 0){
		uloga("Rank %d: failed for dc_register_at_master for peer %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
		goto err_out;
	}

	// 8. free connection, close socket
	close(connectfd);

	return(0);

err_free:
	uloga("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct node_id *peer;
        gni_smsg_attr_t *remote_attr;
        int i, err = -1;

        remote_attr = (gni_smsg_attr_t *)msg->msg_data;
        peer = dc->peer_tab;

	// config all slave servers
	for(i=0;i<dc->num_sp;i++, peer++, remote_attr++){
	  if(i==0)
	    continue;

		peer->remote_smsg_attr = *remote_attr;

		err = rpc_smsg_config(rpc_s, rpc_s->attr_info_start, peer);
		if (err != 0){
			uloga("Rank %d: failed for config SMSG for peer %d. (%d)\n", rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}
	}

	// bcast to slave clients
    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);

        err = MPI_Bcast(msg->msg_data, msg->size, MPI_BYTE, 0, *dc->comm);
        if(err != MPI_SUCCESS) {
            printf("Rank %d: failed for broadcast smsg attributes information to slave clients. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
        
        err = MPI_Barrier(*dc->comm);
        assert(err == PMI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);

        err = PMI_Bcast(msg->msg_data, msg->size);
	    if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast smsg attributes information to slave clients. (%d)\n", dc->rpc_s->ptlmap.id, err);			
		    goto err_out;
	    }
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

    free(msg->msg_data);
    free(msg);

    dc->f_reg = 1;

    return 0;

err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
static int dcrpc_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
	struct node_id *peer;
	struct msg_buf *msg;
    int i, num, err = -ENOMEM;

	peer = &dc->peer_tab[0];

	msg = msg_buf_alloc(dc->rpc_s, peer, 0);
	msg->size = dc->num_sp * sizeof(gni_smsg_attr_t);
	msg->msg_data = (gni_smsg_attr_t *)malloc(msg->size);
	memset(msg->msg_data, 0, msg->size);
	msg->cb = register_completion;

	rpc_mem_info_cache(peer, msg, cmd); 
	err = rpc_receive_direct(rpc_s, peer, msg);
    if (err != 0){
		free(msg);
        goto err_out;
	}
	rpc_mem_info_reset(peer, msg, cmd);

	return 0;

err_out:
        printf("'%s()': failed with %d.\n", __func__, err);
        return err;
}
*/
static int dc_boot_master(struct dart_client *dc, int appid)
{
    int i, err = -ENOMEM;
	struct node_id *peer;

	err = dc_master_init(dc);
	if (err != 0){
		uloga("Rank 0: master client failed to init. (%d)\n", err);
		goto err_out;
	}
	return 0;

err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int dc_boot_slave(struct dart_client *dc, int appid)
{
  int i, err = -ENOMEM;
  int sp = 0;
	struct node_id *peer;
	struct ptlid_map *dcreg;

	void *recv_buffer;
	void *send_buffer;	
	gni_return_t status;

	gni_smsg_attr_t *smsg_attr;

    if (dc->comm) {
        // MPI_Bcast to all slave clients
        err = MPI_Bcast(&dc->peer_size, 1, MPI_INT, 0, *dc->comm);
        if (err != MPI_SUCCESS) {
            uloga("Rank %d: failed for broadcast peer_size to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
    } else {
        // PMI_Bcast to all slave clients.
        err = PMI_Bcast(&dc->peer_size, sizeof(int));
        if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast peer_size to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
    }

	dc->rpc_s->num_rpc_per_buff = dc->peer_size - dc->num_cp;
	dc->num_sp = dc->peer_size - dc->num_cp;

	recv_buffer = malloc(dc->peer_size * sizeof(struct ptlid_map));
	memset(recv_buffer,0,dc->peer_size * sizeof(struct ptlid_map));

    if (dc->comm) {
        // MPI_Bcast to all slave clients
        err = MPI_Bcast(recv_buffer, dc->peer_size * sizeof(struct ptlid_map), MPI_BYTE, 0, *dc->comm);
        if (err != MPI_SUCCESS) {
            uloga("Rank %d: failed for broadcast Address information to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
    } else {
        // PMI_Bcast to all slave clients.
        err = PMI_Bcast(recv_buffer, dc->peer_size * sizeof(struct ptlid_map));
        if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast Address information to slave servers. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
    }

	dc->peer_tab = (struct node_id *)malloc(dc->rpc_s->num_rpc_per_buff * sizeof(struct node_id));
	memset(dc->peer_tab, 0, dc->rpc_s->num_rpc_per_buff * sizeof(struct node_id));

	free(dc->rpc_s->peer_tab);
	dc->rpc_s->peer_tab = dc->peer_tab;

	dcreg = (struct ptlid_map *)recv_buffer;
	peer = dc->peer_tab;
	
	for (i = 0; i < dc->peer_size; i++) 
	{
		if(i<dc->rpc_s->num_rpc_per_buff){
		        INIT_LIST_HEAD(&peer->req_list);
		        peer->num_msg_at_peer = dc->rpc_s->max_num_msg;

		        peer->num_msg_recv = 0;
		        peer->num_msg_ret = 0;

		        peer->sys_msg_recv = 0;
		        peer->sys_msg_at_peer = dc->rpc_s->max_num_msg;
		        peer->sys_msg_ret = 0;


			peer->ptlmap.nid = dcreg->nid;
			peer->ptlmap.pid = dcreg->pid;
			peer->ptlmap.appid = dcreg->appid;
			peer->ptlmap.id = dcreg->id;
			peer->next = NULL;
			peer->peer_rank = peer->ptlmap.id;
			peer->peer_num = dc->rpc_s->num_rpc_per_buff;

			peer++;

		}
		else{

		  if((dc->rpc_s->ptlmap.appid == dcreg->appid) && (i == dc->rpc_s->num_rpc_per_buff))
			dc->rpc_s->app_minid = dc->cp_min_rank = dcreg->id;

		  if((dc->rpc_s->ptlmap.pid == dcreg->pid) && (dc->rpc_s->ptlmap.nid == dcreg->nid)){
		    dc->rpc_s->ptlmap.id = dcreg->id;
			break;
		  }

		}

		dcreg++;
	}

	// Create peer_tab for all peers of current application
	struct node_id* app_peer_tab = create_app_peer_tab(recv_buffer, dc);
    	if (app_peer_tab) {
        	// Add peer_tab to the end of the linked list chained into dc->peer_tab  
        	peer_tab_list_push_back(dc->peer_tab, app_peer_tab);
	}
	free(recv_buffer);

	for(i=0;i<dc->peer_size; i++){
		if(i == dc->rpc_s->ptlmap.id)
			continue;

		peer = &dc->peer_tab[i];

		status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->src_cq_hndl, &dc->peer_tab[i].ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(dc->peer_tab[i].ep_hndl, dc->peer_tab[i].ptlmap.nid, dc->peer_tab[i].ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpCreate(dc->rpc_s->nic_hndl, dc->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

	// #SCA start

	sp = dc->rpc_s->num_rpc_per_buff;

	dc->rpc_s->attr_info_start = (struct gni_smsg_attr_info *)malloc(sizeof(struct gni_smsg_attr_info)); //// DSaaS
	memset(dc->rpc_s->attr_info_start, 0, sizeof(struct gni_smsg_attr_info));

	err = rpc_smsg_init(dc->rpc_s, dc->rpc_s->attr_info_start, sp);
	if (err != 0){
		uloga("Rank %d: failed for rpc_smsg_init %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
		goto err_out;
	}

	// #SCA end


	// 4. allgather APP smsg_attr[rpc+sys]
        gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(dc->num_cp * sizeof(gni_smsg_attr_t));

    allgather(&dc->rpc_s->attr_info_start->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), dc->comm);
    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
            assert(err == PMI_SUCCESS); 
    }
    
    free(remote_smsg_rpc_array);

	// recv APP smsg_attr[rpc] of servers from master client
    recv_buffer = (gni_smsg_attr_t *)malloc(dc->num_sp * sizeof(gni_smsg_attr_t));
	memset(recv_buffer, 0, dc->num_sp * sizeof(gni_smsg_attr_t));

    if(dc->comm) {
        //MPI_Barrier to all slave clients
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
        err = MPI_Bcast(recv_buffer, dc->num_sp * sizeof(gni_smsg_attr_t), MPI_BYTE, 0, *dc->comm);

        if (err != MPI_SUCCESS){
            uloga("Rank %d: failed for broadcast smsg attributes information to slave client. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        // PMI_Bcast to all slave clients.
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
        err = PMI_Bcast(recv_buffer, dc->num_sp * sizeof(gni_smsg_attr_t));
        if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast smsg attributes information to slave client. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }

        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

	smsg_attr = (gni_smsg_attr_t *)recv_buffer;
	for(i = 0; i < dc->num_sp; i++){
	    dc->peer_tab[i].remote_smsg_attr = *smsg_attr;
	    smsg_attr++;
	}


	for(i=0;i<dc->num_sp;i++){
		peer = &dc->peer_tab[i];

		err = rpc_smsg_config(dc->rpc_s, dc->rpc_s->attr_info_start, peer);
		if (err != 0){
			uloga("Rank %d: failed for config RPC SMSG for peer %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}
	}

	free(recv_buffer);

    free(recv_buffer);

    dc->f_reg  = 1;
    return 0;

 err_free:
	uloga("'%s()': failed with %d.\n", __func__, status);
	return status;

err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int dc_boot(struct dart_client *dc, int appid)
{
        int i, counter, sp, err = -ENOMEM;
        int flag = 0;
        struct node_id *peer;

        i = sp = 0;
        counter = 0;

        if(dc->rpc_s->ptlmap.id == 0){
            err = dc_boot_master(dc, appid);
            if (err != 0)
                goto err_out;
        }
        else{
            err = dc_boot_slave(dc, appid);
		if (err != 0)
			goto err_out;
	}

	//dc->self value
	dc->self = (struct node_id *)malloc(sizeof(struct node_id));
	memset(dc->self, 0, sizeof(struct node_id));
	dc->self->ptlmap.nid = dc->rpc_s->ptlmap.nid;
	dc->self->ptlmap.pid = dc->rpc_s->ptlmap.pid;	
	dc->self->ptlmap.id = dc->rpc_s->ptlmap.id;
	dc->self->ptlmap.appid = dc->rpc_s->ptlmap.appid;
	dc->self->peer_rank = dc->rpc_s->ptlmap.id - dc->cp_min_rank;
	dc->self->peer_num = dc->num_cp;

	dc->peer_size = dc->rpc_s->num_rpc_per_buff + dc->num_cp;
	dc->rpc_s->app_minid = dc->cp_min_rank;

	rpc_server_set_peer_ref(dc->rpc_s, dc->peer_tab, dc->rpc_s->num_rpc_per_buff);
	rpc_server_set_rpc_per_buff(dc->rpc_s, dc->rpc_s->num_rpc_per_buff);

	dc->f_reg = 1;
	return 0;

 err_out:
	uloga("'%s()': failed.\n", __func__);
	return err;
}
/*
  Public API starts here.
*/

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm)
{
	struct dart_client *dc;
	struct node_id *peer_list, *peer;
	int err, i, j;
	int node_size;
	gni_return_t status;

	dc = calloc(1, sizeof(*dc));
	if (!dc) {
		return NULL;
	}
	dc->dart_ref = dart_ref;
	dc->cp_in_job = num_peers;
	dc->num_cp = num_peers;
    if(comm) {
        dc->comm = malloc(sizeof(*dc->comm));
        MPI_Comm_dup(*(MPI_Comm *)comm, dc->comm);
    } else {
        dc->comm = NULL;
    }

	dc->rpc_s = rpc_server_init(30, num_peers, dc, DART_CLIENT, appid, dc->comm);
	if (!dc->rpc_s) {
		free(dc);
		return NULL;
	}	
	dc->rpc_s->app_minid = appid;
	dc->rpc_s->app_num_peers = num_peers;

	rpc_add_service(cn_register, dcrpc_register);
	rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);

	err = dc_boot(dc, appid);
	if (err < 0) {
		rpc_server_free(dc->rpc_s, dc->boot);
		goto err_free;
	}

	peer = dc->peer_tab;
	for(i = 0; i < dc->peer_size - dc->num_cp; i++)
	{
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = dc->rpc_s->max_num_msg;

		peer->num_msg_recv = 0;
		peer->num_msg_ret = 0;	  

		peer->sys_msg_recv = 0;
		peer->sys_msg_at_peer = dc->rpc_s->max_num_msg;
		peer->sys_msg_ret = 0;	

		peer++;
	}

    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();    
        assert(err == PMI_SUCCESS);
    }

#ifdef DEBUG
	printf("'%s(%d:%d:%d:%d)': init ok.\n", __func__, dc->self->ptlmap.id, dc->self->ptlmap.appid, dc->self->ptlmap.pid, dc->self->ptlmap.nid);//partial debug
#endif

    return dc;

 err_free:
	uloga("'%s()': failed with %d.\n", __func__, err);
	free(dc);
	return NULL;
}

int print_dc(struct dart_client *dc)
{
  int i;

  printf("dc is:\n dc->rpc_s: ptlmap[nid(%d),pid(%d),id(%d),appid(%d)];\n num_buf(%d);num_rpc_per_buff(%d);max_num_msg(%d);com_type(%d);num_peers(%d);bar_num(%d);app_minid(%d);app_num_peers(%d);num_md_posted(%d);num_md_unlinked(%d);\n dc->peer_size(%d), dc->num_cp(%d), dc->num_sp(%d), dc->cp_in_job(%d), dc->cp_min_rank(%d), f_reg(%d), f_bar(%d), dc->cp_barrier_req(%d), dc->num_posted(%d);\n dc->self:[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", dc->rpc_s->ptlmap.nid, dc->rpc_s->ptlmap.pid, dc->rpc_s->ptlmap.id, dc->rpc_s->ptlmap.appid,dc->rpc_s->num_buf, dc->rpc_s->num_rpc_per_buff, dc->rpc_s->max_num_msg, dc->rpc_s->cmp_type, dc->rpc_s->num_peers, dc->rpc_s->bar_num, dc->rpc_s->app_minid, dc->rpc_s->app_num_peers, dc->rpc_s->num_md_posted, dc->rpc_s->num_md_unlinked, dc->peer_size,dc->num_cp, dc->num_sp, dc->cp_in_job, dc->cp_min_rank, dc->f_reg, dc->f_bar, dc->cp_barrier_req, dc->num_posted, dc->self->ptlmap.nid, dc->self->ptlmap.pid, dc->self->ptlmap.id, dc->self->ptlmap.appid, dc->self->num_req, dc->self->f_req_msg, dc->self->f_need_msg, dc->self->num_msg_at_peer, dc->self->num_msg_ret);

  for(i=0; i<dc->peer_size;i++)
    {
      printf("dc->peer_tab(%d):[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", i, dc->peer_tab[i].ptlmap.nid, dc->peer_tab[i].ptlmap.pid, dc->peer_tab[i].ptlmap.id, dc->peer_tab[i].ptlmap.appid, dc->peer_tab[i].num_req, dc->peer_tab[i].f_req_msg, dc->peer_tab[i].f_need_msg, dc->peer_tab[i].num_msg_at_peer, dc->peer_tab[i].num_msg_ret);
    }

  return 0;

}

void dc_free(struct dart_client *dc)
{
	int err;
	int track;
	int f_unregister = 1;

   
	while(dc->rpc_s->rr_num != 0){
	  err = rpc_process_event(dc->rpc_s);
            if (err < 0){
	      printf("'%s()': failed with %d.\n", __func__, err);
	      //return -1;
	    }
	}

    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();    
        assert(err == PMI_SUCCESS);
    }	
		
	if(dc->cp_min_rank == dc->rpc_s->ptlmap.id){
	    err = dc_unregister(dc);
	    if(err!=0) {
		printf("Rank %d: dc_unregister failed with err %d.\n", dc->self->ptlmap.id, err);
	    }
	    while(dc->rpc_s->rr_num != 0){
		err = rpc_process_event(dc->rpc_s);
		if (err < 0){
		    printf("'%s()': failed with %d.\n", __func__, err);
	      	}
	    }
	} 

    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();    
        assert(err == PMI_SUCCESS);
    }

	track = dc->self->ptlmap.id;
    
    err = rpc_server_free(dc->rpc_s, dc->comm);
	if(err!=0) {
	    uloga("(%s): failed. (%d)\n",__func__, err);
    }

    if(dc->comm) {
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();    
        assert(err == PMI_SUCCESS);
    }

	free(dc);

}

int dc_process(struct dart_client *dc)//done
{
  return rpc_process_event(dc->rpc_s);
}
