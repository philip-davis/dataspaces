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
		uloga("'%s()': I am master rank %d, and all peers joined the barrier.\n", __func__, dc->self->ptlmap.id);

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
	uloga("'%s()': failed with %d.\n", __func__, err);
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

//? working
static int dc_unregister(struct dart_client *dc)
{
	struct msg_buf *msg;
	struct hdr_register *hreg;
	struct node_id *peer;
	int sp_index, err = -ENOMEM;

	sp_index = dc->self->ptlmap.id % dc->num_sp;
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
	uloga("'%s()': failed.\n", __func__);
	return err;
}

static int dc_connect_init(struct sockaddr_in *dest)
{
	struct sockaddr_in dest_addr;
	dest_addr = *dest;
	dest_addr.sin_family = AF_INET;
;
	int Res;
	int SocketFD = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	 
	Res = inet_pton(AF_INET, inet_ntoa(dest_addr.sin_addr), &dest_addr.sin_addr);

	if (-1 == SocketFD)
	{
		perror("cannot create socket");
		exit(EXIT_FAILURE);
	}
	 
	if (0 > Res)
	{
		perror("error: first parameter is not a valid address family");
		close(SocketFD);
		exit(EXIT_FAILURE);
	}
	else if (0 == Res)
	{
		perror("char string (second parameter does not contain valid ipaddress");
		close(SocketFD);
		exit(EXIT_FAILURE);
	}

	if (-1 == connect(SocketFD, (struct sockaddr *)&dest_addr, sizeof dest_addr))
	{
		perror("connect failed");
		close(SocketFD);
		exit(EXIT_FAILURE);
	}

	return SocketFD;
}


/*
1. send APP msg_size + real_msg[all peer ptlmap info]
2. recv ALL msg_size + real_msg[all peer ptlmap info], bcast slave clients
3. EpCreate+Epbind+smsg_init(rpc+sys) 

4. allgather APP smsg_attr[rpc+sys]
5. send APP msg_size + smsg_attr[rpc+sys]
6. recv APP msg_size + smsg_attr[rpc+sys], bcast slave clients
7. smsg_config
*/


static int dc_master_init(struct dart_client *dc) //working
{
  int i, j, k, err;
	struct ptlid_map *dcreg;
	struct node_id *peer;

	void *send_buffer, *recv_buffer;
	int info_size, tmp_size;

	int check=0;
	int sp=0;

        gni_return_t status;
	struct sockaddr_in address;
	gni_smsg_attr_t * smsg_attr;

	err = rpc_read_socket(&address);
	if (err != 0)
		goto err_out;


	connectfd = dc_connect_init(&address);
	if(connectfd < 0)
		goto err_out;	
	
	// 1. send APP msg_size + real_msg[all peer ptlmap info]
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
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
		}

		tmp_size += err;

		if(sizeof(int)<=tmp_size)
			break;
	}

	tmp_size = 0;
	while(1){
		err = send(connectfd, tmp_size+send_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
		}

		tmp_size += err;

		if(info_size<=tmp_size)
			break;
	}		
	free(send_buffer);
	
	// 2. recv ALL msg_size + real_msg[all peer ptlmap info], bcast slave clients
	info_size = 0;
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
		if (-1 == err) {
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
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
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
		}
		else if (0 == err) {
			uloga("%s(): recv return 0?\n",__func__);
		}

	tmp_size += err;

	if(info_size<=tmp_size)
		break;
	}
	
	dc->peer_size = dc->rpc_s->num_rpc_per_buff = info_size/sizeof(struct ptlid_map);
	dc->peer_tab = (struct node_id *)malloc(dc->peer_size * sizeof(struct node_id));
	memset(dc->peer_tab, 0, dc->peer_size * sizeof(struct node_id));

	dcreg = (struct ptlid_map *)recv_buffer;
	peer = dc->peer_tab;
	check = 0;
	for (i = 0; i < dc->peer_size; i++) 
	{
		peer->ptlmap.nid = dcreg->nid;
		peer->ptlmap.pid = dcreg->pid;
		peer->ptlmap.appid = dcreg->appid;
		peer->ptlmap.id = dcreg->id;

		if(dc->rpc_s->ptlmap.pid == dcreg->pid && dc->rpc_s->ptlmap.nid == dcreg->nid)
		  dc->rpc_s->ptlmap.id = dcreg->id;

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
	free(recv_buffer);

	// 3. EpCreate+Epbind+smsg_init(rpc+sys) 
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

		status = GNI_EpBind(dc->peer_tab[i].ep_hndl, dc->peer_tab[i].ptlmap.nid, dc->peer_tab[i].ptlmap.pid);
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
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

	k=0;
	while(k < dc->peer_size)
	{
		peer = &dc->peer_tab[k];
		if(peer->ptlmap.appid == 0)
			sp++;
		k++;
	}

	err = rpc_smsg_init(dc->rpc_s, sp);
	if (err != 0){
		uloga("Rank %d: failed for rpc_smsg_init %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
		goto err_out;
	}

	// 4. allgather APP smsg_attr[rpc+sys]
        gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(dc->num_cp * sizeof(gni_smsg_attr_t));
	allgather(&dc->rpc_s->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), dc->comm);
	if(dc->comm) {
	    err = MPI_Barrier(*dc->comm);
	    assert(err == MPI_SUCCESS);
	} else {
	    err = PMI_Barrier();
            assert(err == PMI_SUCCESS);
	}
	// 5. send APP msg_size + smsg_attr[rpc+sys]
	//	info_size = dc->num_cp * sizeof(gni_smsg_attr_t) * 2;// SCA SYS
	info_size = dc->num_cp * sizeof(gni_smsg_attr_t);// SCA SYS
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
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
		}

		tmp_size += err;

		if(sizeof(int)<=tmp_size)
			break;
	}

	tmp_size = 0;
	while(1){
		err = send(connectfd, tmp_size+send_buffer, info_size-tmp_size, 0);
		if (-1 == err) {
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
		}

		tmp_size += err;

		if(info_size<=tmp_size)
			break;
	}		
	free(send_buffer);

	// 6. recv APP msg_size + smsg_attr[rpc+sys], bcast slave clients
	info_size = 0;
	tmp_size = 0;
	while(1){
		err = recv(connectfd, tmp_size+&info_size, sizeof(int)-tmp_size, 0);
		if (-1 == err) {
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
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
			if(errno == EINTR) {
				continue;
			} else {
				perror("error receive failed\n");
				goto err_out;
			}
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

	for(j=0;j<info_size/sizeof(gni_smsg_attr_t);j++){
		dc->peer_tab[j].remote_smsg_attr = *smsg_attr;
		peer++;
		smsg_attr++;
	}

	// Slave server broadcast
	if(dc->comm) {
	    err = MPI_Barrier(*dc->comm);
            assert(err == MPI_SUCCESS);
            err = MPI_Bcast(recv_buffer, dc->peer_size * sizeof(gni_smsg_attr_t), MPI_BYTE, 0, *dc->comm);

            if (err != MPI_SUCCESS){
                uloga("Rank %d: failed for broadcast smsg attributes information to slave client. (%d)\n", dc->rpc_s->ptlmap.id, err);
                goto err_out;
            }
            err = MPI_Barrier(*dc->comm);
            assert(err == MPI_SUCCESS);
        } else {
	    err = PMI_Barrier();
            assert(err == PMI_SUCCESS);
            err = PMI_Bcast(recv_buffer, dc->peer_size * sizeof(gni_smsg_attr_t));
            if (err != PMI_SUCCESS){
                uloga("Rank %d: failed for broadcast smsg attributes information to slave client. (%d)\n", dc->rpc_s->ptlmap.id, err);
                goto err_out;
            }

            err = PMI_Barrier();
            assert(err == PMI_SUCCESS);
	}

	free(recv_buffer);

	// 7. smsg_config
	for(i=0;i<sp;i++){
		if(i == dc->rpc_s->ptlmap.id)
			continue;

		peer = &dc->peer_tab[i];

		err = rpc_smsg_config(dc->rpc_s, peer);
		if (err != 0){
			uloga("Rank %d: failed for config RPC SMSG for peer %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}
	}
	// 8. free connection, close socket
	//close(connectfd);
	
	dc->f_reg  = 1;
	return 0;

err_free:
	uloga("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
static int dcrpc_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
	struct peer_attr_reg local_server_attr, *peer_attr;
	struct peer_attr_reg *remote_server_attr = (struct peer_attr_reg *)malloc(dc->num_cp * sizeof(struct peer_attr_reg));
	struct node_id *peer;
	struct msg_buf *msg;
        int i, num, err = -ENOMEM;

	local_server_attr.ptlmap = dc->rpc_s->ptlmap;
	local_server_attr.remote_smsg_attr= dc->rpc_s->local_smsg_attr;
	//local_server_attr.sys_remote_smsg_attr = dc->rpc_s->sys_local_smsg_attr;//SCA SYS	

	err = PMI_Allgather(&local_server_attr, remote_server_attr, dc->num_cp * sizeof(struct peer_attr_reg));
	if (err != PMI_SUCCESS){
		uloga("Rank 0: failed for gather information to slave servers. (%d)\n", err);			
		goto err_out;
	}

	peer = &dc->peer_tab[0];
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
	msg->msg_data = remote_server_attr;
	msg->size = dc->num_cp * sizeof(struct peer_attr_reg);

	rpc_mem_info_cache(peer, msg, cmd); //Modified by Tong Jin for decoupling DS and DART

        err = rpc_send_direct(rpc_s, peer, msg);
        if (err != 0){
		free(msg);
                goto err_out;
	}

	dc->peer_tab[0].f_reg = 1;

	return 0;

err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
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
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int dc_boot_slave(struct dart_client *dc, int appid)
{
  int i, k, err = -ENOMEM;
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

	dc->rpc_s->num_rpc_per_buff = dc->peer_size;

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

	dc->peer_tab = (struct node_id *)malloc(dc->peer_size * sizeof(struct node_id));
	memset(dc->peer_tab, 0, dc->peer_size * sizeof(struct node_id));

	dcreg = (struct ptlid_map *)recv_buffer;
	peer = dc->peer_tab;
	
	for (i = 0; i < dc->peer_size; i++) 
	{
		peer->ptlmap.nid = dcreg->nid;
		peer->ptlmap.pid = dcreg->pid;
		peer->ptlmap.appid = dcreg->appid;
		peer->ptlmap.id = dcreg->id;

		if(dc->rpc_s->ptlmap.pid == dcreg->pid && dc->rpc_s->ptlmap.nid == dcreg->nid)
		  dc->rpc_s->ptlmap.id = dcreg->id;

		dcreg++;
		peer++;
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

		status = GNI_EpBind(dc->peer_tab[i].ep_hndl, dc->peer_tab[i].ptlmap.nid, dc->peer_tab[i].ptlmap.pid);
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
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", dc->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

    for(k = 0; k < dc->peer_size; k++) {
        peer = &dc->peer_tab[k];
        if(peer->ptlmap.appid == 0)
            sp++;
    }
	err = rpc_smsg_init(dc->rpc_s, sp);
	if (err != 0){
	    uloga("Rank %d: failed for rpc_smsg_init %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
	    goto err_out;
	}
        gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(dc->num_cp * sizeof(gni_smsg_attr_t));
	allgather(&dc->rpc_s->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), dc->comm);
	if(dc->comm) {
	    err = MPI_Barrier(*dc->comm);
	    assert(err == MPI_SUCCESS);
	} else {
	    err = PMI_Barrier();
            assert(err == PMI_SUCCESS);	
 	}
	free(remote_smsg_rpc_array);
	
	// PMI_Bcast to all slave clients.
	recv_buffer = malloc(dc->peer_size * sizeof(gni_smsg_attr_t));//SCA SYS
	memset(recv_buffer,0,dc->peer_size * sizeof(gni_smsg_attr_t));//SCA SYS

	if(dc->comm) {
	    //MPI_Barrier to all slave clients
        err = MPI_Barrier(*dc->comm);
        assert(err == MPI_SUCCESS);
        err = MPI_Bcast(recv_buffer, dc->peer_size * sizeof(gni_smsg_attr_t), MPI_BYTE, 0, *dc->comm);

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
        err = PMI_Bcast(recv_buffer, dc->peer_size * sizeof(gni_smsg_attr_t));
        if (err != PMI_SUCCESS){
            uloga("Rank %d: failed for broadcast smsg attributes information to slave client. (%d)\n", dc->rpc_s->ptlmap.id, err);
            goto err_out;
        }

        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

	smsg_attr = (gni_smsg_attr_t *)recv_buffer;
	for(i=0;i<dc->peer_size;i++){
	  dc->peer_tab[i].remote_smsg_attr = *smsg_attr;
	  smsg_attr++;
	}
	

	for(i=0;i<sp;i++){
		if(i == dc->rpc_s->ptlmap.id)
			continue;

		peer = &dc->peer_tab[i];

		err = rpc_smsg_config(dc->rpc_s, peer);
        if (err != 0){
           uloga("Rank %d: failed for config RPC SMSG for peer %d. (%d)\n", dc->rpc_s->ptlmap.id, peer->ptlmap.id, err);
            goto err_out;
        }
    }

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

	while(counter < dc->rpc_s->num_rpc_per_buff)
	{
		peer = &dc->peer_tab[i];
		if(peer->ptlmap.appid == 0)
			sp++;
		if(peer->ptlmap.appid == appid)
		{
			counter++;
			if(counter == 1)
				dc->rpc_s->app_minid = dc->cp_min_rank = peer->ptlmap.id;
		}

		//dc->self value
		if (dc->peer_tab[i].ptlmap.nid == dc->rpc_s->ptlmap.nid && dc->peer_tab[i].ptlmap.pid == dc->rpc_s->ptlmap.pid) 
		{
			dc->self = &dc->peer_tab[i];
			break;
		}

		i++;
	}


	free(dc->rpc_s->peer_tab);
	
	dc->num_sp = sp;
	dc->cn_peers = dc->peer_tab + dc->cp_min_rank;
	dc->peer_size = dc->rpc_s->num_rpc_per_buff;
	dc->rpc_s->app_minid = dc->cp_min_rank;

	rpc_server_set_peer_ref(dc->rpc_s, dc->peer_tab, dc->peer_size);
	rpc_server_set_rpc_per_buff(dc->rpc_s, dc->peer_size);

	dc->f_reg = 1;
	return 0;

 err_out:
	uloga("'%s()': failed.\n", __func__);
	return -1;
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
	if (!dc)
		return NULL;

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

        //rpc_add_service(cn_register, dcrpc_register);//not used in GNI version
        rpc_add_service(cp_barrier, dcrpc_barrier);
	rpc_add_service(cn_unregister, dcrpc_unregister);
	//rpc_add_service(sp_announce_cp, dcrpc_announce_cp);// dont need in GNI

        err = dc_boot(dc, appid);
        if (err < 0) {
	  	rpc_server_free(dc->rpc_s, dc->comm);
                free(dc);
                return NULL;
        }

	peer = dc->peer_tab;
	for (i = 0; i < dc->peer_size; i++) 
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
	uloga("'%s(%d:%d:%d:%d)': init ok.\n", __func__, dc->self->ptlmap.id, dc->self->ptlmap.appid, dc->self->ptlmap.pid, dc->self->ptlmap.nid);//partial debug
#endif
        return dc;

 err_free:
	uloga("'%s()': failed.\n", __func__);
	free(dc);
	return NULL;
}

int print_dc(struct dart_client *dc)
{
  int i;

  uloga("dc is:\n dc->rpc_s: ptlmap[nid(%d),pid(%d),id(%d),appid(%d)];\n num_buf(%d);num_rpc_per_buff(%d);max_num_msg(%d);com_type(%d);num_peers(%d);bar_num(%d);app_minid(%d);app_num_peers(%d);num_md_posted(%d);num_md_unlinked(%d);\n dc->peer_size(%d), dc->num_cp(%d), dc->num_sp(%d), dc->cp_in_job(%d), dc->cp_min_rank(%d), f_reg(%d), f_bar(%d), dc->cp_barrier_req(%d), dc->num_posted(%d);\n dc->self:[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", dc->rpc_s->ptlmap.nid, dc->rpc_s->ptlmap.pid, dc->rpc_s->ptlmap.id, dc->rpc_s->ptlmap.appid,dc->rpc_s->num_buf, dc->rpc_s->num_rpc_per_buff, dc->rpc_s->max_num_msg, dc->rpc_s->cmp_type, dc->rpc_s->num_peers, dc->rpc_s->bar_num, dc->rpc_s->app_minid, dc->rpc_s->app_num_peers, dc->rpc_s->num_md_posted, dc->rpc_s->num_md_unlinked, dc->peer_size,dc->num_cp, dc->num_sp, dc->cp_in_job, dc->cp_min_rank, dc->f_reg, dc->f_bar, dc->cp_barrier_req, dc->num_posted, dc->self->ptlmap.nid, dc->self->ptlmap.pid, dc->self->ptlmap.id, dc->self->ptlmap.appid, dc->self->num_req, dc->self->f_req_msg, dc->self->f_need_msg, dc->self->num_msg_at_peer, dc->self->num_msg_ret);

  for(i=0; i<dc->peer_size;i++)
    {
      uloga("dc->peer_tab(%d):[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", i, dc->peer_tab[i].ptlmap.nid, dc->peer_tab[i].ptlmap.pid, dc->peer_tab[i].ptlmap.id, dc->peer_tab[i].ptlmap.appid, dc->peer_tab[i].num_req, dc->peer_tab[i].f_req_msg, dc->peer_tab[i].f_need_msg, dc->peer_tab[i].num_msg_at_peer, dc->peer_tab[i].num_msg_ret);
    }

  return 0;

}

void dc_free(struct dart_client *dc)
{
	int err;
	int track;
	/*
	while (dc->rpc_s->rr_num != 0)
		rpc_process_event(dc->rpc_s);
	*/
	//uloga("Rank %d: step0 done.\n", dc->self->ptlmap.id);//debug
	err = dc_unregister(dc);
	if(err!=0)
	    uloga("Rank %d: dc_unregister failed with err %d.\n", dc->self->ptlmap.id, err);

	//uloga("Rank(%d): step1 done.\n", dc->self->ptlmap.id);//debug
	track = dc->self->ptlmap.id;
	err = rpc_server_free(dc->rpc_s, dc->comm);
	if(err!=0)
	  uloga("(%s): failed. (%d)\n",__func__, err);

	//uloga("Rank(%d): step2 done.\n", track);//debug
	if (dc->peer_tab)
		free(dc->peer_tab);
	//free(dc);
	if(dc->comm) {
		MPI_Comm_free(dc->comm);
		free(dc->comm);
	}
}

int dc_process(struct dart_client *dc)//done
{
  return rpc_process_event(dc->rpc_s);
}
