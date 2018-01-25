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

/*
  RPC routine to unregister a compute peer.
*/
static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
	struct hdr_register *hreg = (struct hdr_register *)(cmd->pad);
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;
	int i;
	static int num_unreg = 0;

	if(ds->f_stop != 1) {
	    num_unreg = num_unreg + hreg->num_cp;

	    if (num_unreg == ds->num_cp) {
	        ds->f_stop = 1;
         }
	    // All compute peers  have unregistered. I should send one RPC but not respond to any.
	
	    ds->f_nacc = 1;

	    if (hreg->num_cp && cmd->id >= ds->num_sp) {
		    hreg->num_cp = 0;
		    peer = ds_get_peer(ds, cmd->id);

		    if(peer->f_unreg != 1) {
		        msg = msg_buf_alloc(rpc_s, peer, 1);
		        if (!msg)
			        goto err_out;

		        msg->msg_rpc->id = ds->self->ptlmap.id;
		        msg->msg_rpc->cmd = cn_unregister;
		        peer->f_unreg = 1;

		        err = rpc_send(rpc_s, peer, msg);
		        if (err < 0) {
			        free(msg);
			        goto err_out;
		        }
		    }
		    ds->num_charge--;
	    }

	    if ( ds->num_charge == 0 ) {
	        for(i=0;i<ds->num_sp;i++) {
	            if(ds->self->ptlmap.id == i)
	                continue;
                
	            hreg->num_cp = ds->num_charge_cp;
		        peer = ds_get_peer(ds, i);

		        if(peer->f_unreg != 1)
		        {
		            msg = msg_buf_alloc(rpc_s, peer, 1);
		            if (!msg)
			            goto err_out;
		      
		            memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
		            msg->msg_rpc->id = ds->self->ptlmap.id;
		            peer->f_unreg = 1;

		            err = rpc_send(rpc_s, peer, msg);
                    if (err < 0) {
			            free(msg);
			            goto err_out;
		            }
		        }   
	        }
	    }
    }

	return 0;
 err_out:
	uloga("(%s): failed. (%d)\n", __func__, err);
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
	  uloga("CANNOT FIND HOST IP\n");
		return "cannot find ip";
	}
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = (caddr_t)buf;
	if (ioctl(sfd, SIOCGIFCONF, (char *)&ifc)){
	  uloga("CANNOT FIND HOST IP\n");
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
	if(-1 == listen(*SocketPri, 10))
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

/*
1. recv APP msg_size + real_msg[all peer ptlmap info]
2. send ALL msg_size + real_msg[all peer ptlmap info], bcast slave servers
3. EpCreate+Epbind+smsg_init(rpc+sys)

4. allgather APP smsg_attr[rpc+sys]
5. recv APP msg_size + smsg_attr[rpc+sys]
6. send ALL msg_size + smsg_attr[rpc+sys], bcast slave servers
7. smsg_config

8. free connection, close socket
*/

static int ds_master_init(struct dart_server *ds)//testing
{
	int i, j, err, p, count = 0;
	char *localip;
	gni_return_t status;
	struct node_id *peer;

	int k, connect_num = 0;

	int tmp_size=0;
	int info_size;
	void *recv_buffer;
	void *send_buffer;
	struct ptlid_map *dcreg;
	struct smsg_attr_reg *sar;

// 1. get socket address, open listening port

	localip = ip_search();
	ds->rpc_s->address.address.sin_addr.s_addr = inet_addr(localip);
	ds->rpc_s->address.address.sin_port = htons(0);
	ds->rpc_s->address.address.sin_family = AF_INET;	

	err = listen_sock_init(0, localip, ds->rpc_s->address.address.sin_port, &ds->rpc_s->address.sockfd);
	if(err != 0 || ds->rpc_s->address.sockfd < 0)
		goto err_out;

	err = port_search(ds->rpc_s->address.sockfd);
	if(err<0)
	  goto err_out;

	ds->rpc_s->address.address.sin_port = htons(err);

	uloga("ip=%s port=%d\n", inet_ntoa(ds->rpc_s->address.address.sin_addr), ntohs(ds->rpc_s->address.address.sin_port));
	//uloga("nid=%d pid=%d\n", ds->rpc_s->ptlmap.nid, ds->rpc_s->ptlmap.pid);

	//use socket address
	err = rpc_write_socket(ds->rpc_s);
	if (err != 0)
		goto err_out;

//1. recv APP msg_size + real_msg[all peer ptlmap info]
	for(k=0;k<CONNMAX;k++)
		connectfd[k]=-1;

	k = 0;
	while(count != ds->size_cp){
		connectfd[k] = accept(ds->rpc_s->address.sockfd, NULL, NULL);
		if(0 > connectfd[k]) {
			perror("error accept failed");
			continue;
		}

		connect_num++;
		ds->app_num++;

		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
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
			err = recv(connectfd[k], tmp_size+recv_buffer, info_size-tmp_size, 0);
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

		dcreg = (struct ptlid_map *)recv_buffer;
		peer = &ds->peer_tab[ds->size_sp+count];

        for(j=0;j<info_size/sizeof(struct ptlid_map);j++){
			ds->peer_tab[ds->size_sp+count+j].ptlmap.nid = dcreg->nid;
			ds->peer_tab[ds->size_sp+count+j].ptlmap.pid = dcreg->pid;
			ds->peer_tab[ds->size_sp+count+j].ptlmap.appid = dcreg->appid;
			ds->peer_tab[ds->size_sp+count+j].ptlmap.id = ds->size_sp+count+j;
			dcreg++;
        }
		count = count+info_size/sizeof(struct ptlid_map);	
		free(recv_buffer);	
		k++;
	}



//2. send ALL msg_size + real_msg[all peer ptlmap info], bcast slave servers


	info_size = ds->peer_size * sizeof(struct ptlid_map);
	send_buffer = malloc(info_size);
	dcreg = (struct ptlid_map *)send_buffer;
	for(j=0;j<ds->peer_size;j++){
		dcreg->nid = ds->peer_tab[j].ptlmap.nid;
		dcreg->pid = ds->peer_tab[j].ptlmap.pid;
		dcreg->appid = ds->peer_tab[j].ptlmap.appid;
		dcreg->id = ds->peer_tab[j].ptlmap.id;
        dcreg++;
	}

	for(k=0;k<connect_num;k++){
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
	}

	if(ds->comm) {
	    // MPI_Bcast to all slave servers.
	    err = MPI_Barrier(*ds->comm);
	    assert(err == MPI_SUCCESS);
	    err = MPI_Bcast(send_buffer, ds->peer_size * sizeof(struct ptlid_map), MPI_BYTE, 0, *ds->comm);
	    if(err != MPI_SUCCESS) {
		    uloga("Rank 0: failed for broadcast Address information to slave servers. (%d)\n", err);
            goto err_out;
        }
	    err = MPI_Barrier(*ds->comm);
	    assert(err == MPI_SUCCESS);
	} else {
	    // PMI_Bcast to all slave servers.
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);	

	    err = PMI_Bcast(send_buffer, ds->peer_size * sizeof(struct ptlid_map));
	    if (err != PMI_SUCCESS){
		    uloga("Rank 0: failed for broadcast Address information to slave servers. (%d)\n", err);			
		    goto err_out;
	    }

        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);	
	}

	free(send_buffer);


// 3. EpCreate+Epbind+smsg_init(rpc+sys)

	for(i=0;i<ds->peer_size; i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;


		peer = &ds->peer_tab[i];

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &ds->peer_tab[i].ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(ds->peer_tab[i].ep_hndl, ds->peer_tab[i].ptlmap.nid, ds->peer_tab[i].ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}

		err = rpc_smsg_init(ds->rpc_s, ds->peer_size);
		if (err != 0){
			uloga("Rank 0: failed for rpc_smsg_init %d. (%d)\n", peer->ptlmap.id, err);
			goto err_out;
		}

		//debug
		//rpc_smsg_check(ds->rpc_s);

		/*
		err = sys_smsg_init(ds->rpc_s, ds->peer_size);
		if (err != 0){
			uloga("Rank 0: failed for sys_smsg_init. (%d)\n", err);
			goto err_out;
		}
		*///SCA SYS
		//debug
		//sys_smsg_check(ds->rpc_s);

	
	//4. allgather APP smsg_attr[rpc+sys]
    gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(ds->size_sp * sizeof(gni_smsg_attr_t));

	allgather(&ds->rpc_s->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), ds->comm);

	if(ds->comm) {
	    err = MPI_Barrier(*ds->comm);
	    assert(err == MPI_SUCCESS);
	} else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
	}

	for(j=0;j<ds->size_sp;j++){
		ds->peer_tab[j].remote_smsg_attr = remote_smsg_rpc_array[j];
		//		ds->peer_tab[j].sys_remote_smsg_attr = remote_smsg_sys_array[j];//SCA SYS
	}

	free(remote_smsg_rpc_array);
	//	free(remote_smsg_sys_array);//SCA SYS


	// 5. recv APP msg_size + smsg_attr[rpc+sys]
	count = 0;
	gni_smsg_attr_t	*smsg_attr;
	for(k=0;k<connect_num;k++){
		info_size = 0;
		tmp_size = 0;
		while(1){
			err = recv(connectfd[k], tmp_size+&info_size, sizeof(int)-tmp_size, 0);
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
			err = recv(connectfd[k], tmp_size+recv_buffer, info_size-tmp_size, 0);
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

		peer = &ds->peer_tab[ds->size_sp+count];
		smsg_attr = (gni_smsg_attr_t *)recv_buffer;
		/*
                for(j=0;j<info_size/sizeof(gni_smsg_attr_t)/2;j++){
			ds->peer_tab[ds->size_sp+count+j].remote_smsg_attr = *smsg_attr;
			ds->peer_tab[ds->size_sp+count+j].sys_remote_smsg_attr = *(smsg_attr+1);
			peer++;
			smsg_attr = smsg_attr+2;
                }
		count = count+info_size/sizeof(gni_smsg_attr_t)/2;	
		*///SCA SYS

                for(j=0;j<info_size/sizeof(gni_smsg_attr_t);j++){
			ds->peer_tab[ds->size_sp+count+j].remote_smsg_attr = *smsg_attr;
			//ds->peer_tab[ds->size_sp+count+j].sys_remote_smsg_attr = *(smsg_attr+1);
			peer++;
			smsg_attr = smsg_attr+1;
                }
		count = count+info_size/sizeof(gni_smsg_attr_t);	

	}
	free(recv_buffer);

	// 6. send ALL msg_size + smsg_attr[rpc+sys], bcast slave servers
	//	info_size = ds->peer_size * sizeof(gni_smsg_attr_t) * 2;//SCA SYS
	info_size = ds->peer_size * sizeof(gni_smsg_attr_t);//SCA SYS
	send_buffer = malloc(info_size);
	smsg_attr = (gni_smsg_attr_t *)send_buffer;
	for(j=0;j<ds->peer_size;j++){
		*smsg_attr = ds->peer_tab[j].remote_smsg_attr;
		smsg_attr++;
		//		*smsg_attr = ds->peer_tab[j].sys_remote_smsg_attr;//SCA SYS
		//smsg_attr++;//SCA SYS

		//peer_smsg_check(ds->rpc_s, &ds->peer_tab[j], &ds->peer_tab[j].remote_smsg_attr);
		//peer_smsg_check(ds->rpc_s, &ds->peer_tab[j], &ds->peer_tab[j].sys_remote_smsg_attr);

	}

	for(k=0;k<connect_num;k++){
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
	}

	if(ds->comm) {
	    // MPI_Bcast to all slave servers.
	    err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
	    err = MPI_Bcast(send_buffer, ds->peer_size * sizeof(gni_smsg_attr_t), MPI_BYTE, 0, *ds->comm);
	    if (err != MPI_SUCCESS){
            uloga("Rank 0: failed for broadcast smsg attributes information to slave servers. (%d)\n", err);
            goto err_out;
        }
	} else {
	    // PMI_Bcast to all slave servers.
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);

	    err = PMI_Bcast(send_buffer, ds->peer_size * sizeof(gni_smsg_attr_t));//SCA SYS
	    if (err != PMI_SUCCESS){
		    uloga("Rank 0: failed for broadcast smsg attributes information to slave servers. (%d)\n", err);	
		    goto err_out;
	    }
	}

	smsg_attr = (gni_smsg_attr_t *)send_buffer;

	if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
    } else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
    }

	free(send_buffer);


	// 7. smsg_config
	for(i=0;i<ds->peer_size;i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;

		peer = &ds->peer_tab[i];

		err = rpc_smsg_config(ds->rpc_s, peer);
		if (err != 0){
		  uloga("Rank 0: failed for config RPC SMSG for peer %d i is %d. (%d)\n", peer->ptlmap.id, i, err);
			goto err_out;
		}
		/*
		err = sys_smsg_config(ds->rpc_s, peer);
		if (err != 0){
			uloga("Rank 0: failed for config SYS SMSG for peer %d. (%d)\n", peer->ptlmap.id, err);
			goto err_out;
			}*///SCA SYS
	}

	// 8. free connection, close socket
	/*	for(k=0; k<connect_num;k++){
			close(connectfd[k]);
		}*/

	return 0;

err_free:
	uloga("'%s()': failed with %d.\n", __func__, status);
	return status;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
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
		uloga("Rank 0: failed for ds_master_init. (%d)\n", err);
		goto err_out;
	}

	//uloga("PASS ds_master_init.\n");//debug

	return 0;

 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
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
  int i,j, err = -ENOMEM;
	struct node_id *peer;
	struct ptlid_map * ptlmap;
	struct peer_attr_reg *peer_attr = (struct peer_attr_reg *)malloc(ds->peer_size * sizeof(struct peer_attr_reg));
	void *recv_buffer;
	gni_smsg_attr_t *smsg_attr;
	gni_return_t status;
	// 1. bcast slave servers
	recv_buffer = malloc(ds->peer_size * sizeof(struct ptlid_map));
	memset(recv_buffer,0,ds->peer_size * sizeof(struct ptlid_map));

	if(ds->comm) {
	    err = MPI_Barrier(*ds->comm);
	    assert(err == MPI_SUCCESS);
	    err = MPI_Bcast(recv_buffer, ds->peer_size * sizeof(struct ptlid_map), MPI_BYTE, 0, *ds->comm);
	    if (err != MPI_SUCCESS){
            uloga("Rank %d: failed for broadcast ptlmap information from master server. (%d)\n", ds->rpc_s->ptlmap.id, err);
            goto err_out;
        }
	    err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
	} else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);	
	    err = PMI_Bcast(recv_buffer, ds->peer_size * sizeof(struct ptlid_map));
	    if (err != PMI_SUCCESS){
	        uloga("Rank %d: failed for broadcast ptlmap information from master server. (%d)\n", ds->rpc_s->ptlmap.id, err);			
		    goto err_out;
	    }   
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);	
	}
	ptlmap = (struct ptlid_map *)recv_buffer;
	for(j=0;j<ds->peer_size;j++){
		ds->peer_tab[j].ptlmap.nid = ptlmap->nid;
		ds->peer_tab[j].ptlmap.pid = ptlmap->pid;
		ds->peer_tab[j].ptlmap.appid = ptlmap->appid;
		ds->peer_tab[j].ptlmap.id = ptlmap->id;
		ptlmap++;

		//		uloga("Rank %d: peer[nid %d, pid %d, id %d, appid %d].\n", ds->rpc_s->ptlmap.id, ds->peer_tab[j].ptlmap.nid, ds->peer_tab[j].ptlmap.pid, ds->peer_tab[j].ptlmap.id, ds->peer_tab[j].ptlmap.appid);

	}

	free(recv_buffer);

	// 2. EpCreate+Epbind+smsg_init(rpc+sys)
	for(i=0;i<ds->peer_size; i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;

		peer = &ds->peer_tab[i];

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->src_cq_hndl, &ds->peer_tab[i].ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpCreate returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpBind(ds->peer_tab[i].ep_hndl, ds->peer_tab[i].ptlmap.nid, ds->peer_tab[i].ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Fail: GNI_EpBind returned error. %d.\n", status);
			goto err_free;
		}

		status = GNI_EpCreate(ds->rpc_s->nic_hndl, ds->rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}
		status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.pid);
		if (status != GNI_RC_SUCCESS)
		{
			uloga("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", ds->rpc_s->ptlmap.id, status);
			goto err_out;
		}

	}	

		err = rpc_smsg_init(ds->rpc_s, ds->peer_size);
		if (err != 0){
			uloga("Rank 0: failed for rpc_smsg_init %d. (%d)\n", peer->ptlmap.id, err);
			goto err_out;
		}

	// 3. allgather APP smsg_attr[rpc+sys]
    gni_smsg_attr_t *remote_smsg_rpc_array = (gni_smsg_attr_t *)malloc(ds->size_sp * sizeof(gni_smsg_attr_t));

	allgather(&ds->rpc_s->local_smsg_attr, remote_smsg_rpc_array, sizeof(gni_smsg_attr_t), ds->comm);
    if(ds->comm) {
        err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
	} else {
	    err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
	}

	// 4. bcast slave servers

      	recv_buffer = malloc(ds->peer_size * sizeof(gni_smsg_attr_t));// SCA SYS
	memset(recv_buffer,0,ds->peer_size * sizeof(gni_smsg_attr_t));// SCA SYS

	if(ds->comm) {
	    err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
	    err = MPI_Bcast(recv_buffer, ds->peer_size * sizeof(gni_smsg_attr_t), MPI_BYTE, 0, *ds->comm);
	    if (err != MPI_SUCCESS){
            uloga("Rank %d: failed for broadcast smsg attributes information to slave servers. (%d)\n", ds->rpc_s->ptlmap.id, err);
            goto err_out;
        }
	    err = MPI_Barrier(*ds->comm);
        assert(err == MPI_SUCCESS);
	} else {
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);

	    err = PMI_Bcast(recv_buffer, ds->peer_size * sizeof(gni_smsg_attr_t));//SCA SYS
	    if (err != PMI_SUCCESS){
		    uloga("Rank %d: failed for broadcast smsg attributes information to slave servers. (%d)\n", ds->rpc_s->ptlmap.id, err);			
		    goto err_out;
	    }  
	
        err = PMI_Barrier();
        assert(err == PMI_SUCCESS);
	}
	smsg_attr = (gni_smsg_attr_t *)recv_buffer;
	for(j=0;j<ds->peer_size;j++){
		ds->peer_tab[j].remote_smsg_attr = *smsg_attr;
		smsg_attr++;
		//		ds->peer_tab[j].sys_remote_smsg_attr = *smsg_attr;//SCA SYS
		//smsg_attr++;//SCA SYS

		//peer_smsg_check(ds->rpc_s, &ds->peer_tab[j], &ds->peer_tab[j].remote_smsg_attr);
		//peer_smsg_check(ds->rpc_s, &ds->peer_tab[j], &ds->peer_tab[j].sys_remote_smsg_attr);

	}

	// 5. smsg_config
	for(i=0;i<ds->peer_size;i++){
		if(i == ds->rpc_s->ptlmap.id)
			continue;
		
		peer = &ds->peer_tab[i];

		err = rpc_smsg_config(ds->rpc_s, peer);
		if (err != 0){
			uloga("Rank %d: failed for config SMSG for peer %d. (%d)\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
		}
		/*
		err = sys_smsg_config(ds->rpc_s, peer);
		if (err != 0){
			uloga("Rank %d: failed for config SYS SMSG for peer %d. (%d)\n", ds->rpc_s->ptlmap.id, peer->ptlmap.id, err);
			goto err_out;
			}*///SCA SYS
	}

	free(recv_buffer);
	return 0;

 err_free:
	uloga("'%s()': failed with %d.\n", __func__, status);
	return status;

 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;	
}

static int ds_boot(struct dart_server *ds)
{
	int i, err = -ENOMEM;
	struct app_info *app;
	struct app_info *current_app;


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

	//fill in the app_list
	for (i=ds->num_sp; i < ds->peer_size; i++)
	{
		current_app = app_find(ds, ds->peer_tab[i].ptlmap.appid);
		if( current_app == NULL )
		{
			app = app_alloc();
			if (!app)
				goto err_out;
			app->app_id = ds->peer_tab[i].ptlmap.appid;
			app->app_num_peers = 1;
			app->app_cnt_peers = 1;
			app->app_peer_tab = ds->peer_tab + ds->peer_tab[i].ptlmap.id;
			list_add(&app->app_entry, &ds->app_list);
		}
		else{
			current_app->app_num_peers++;
			current_app->app_cnt_peers++;		
		}
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

	return 0;
 err_out:
	uloga("'%s()': failed.\n", __func__);
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

	size = sizeof(struct dart_server) + (num_sp + num_cp) * sizeof(struct node_id);
	ds = calloc(1, size);
	if (!ds)
		goto err_out;
	ds->dart_ref = dart_ref;
	ds->peer_tab = (struct node_id *) (ds+1);
	ds->cn_peers = ds->peer_tab + num_sp;
	ds->peer_size = num_sp + num_cp;
	ds->size_cp = num_cp;
	ds->size_sp = num_sp;
	ds->num_sp = num_sp;
	ds->num_cp = num_cp; //total number of peers from all applications
	INIT_LIST_HEAD(&ds->app_list);

	if(comm) {
		ds->comm = malloc(sizeof(*ds->comm));
		MPI_Comm_dup(*(MPI_Comm *)comm, ds->comm);
	} else {
		ds->comm = NULL;
	}

	ds->rpc_s = rpc_server_init(30, ds->peer_size, ds, DART_SERVER, 0, comm);
	if (!ds->rpc_s)
		goto err_free_dsrv;

	for (i=0;i<ds->size_sp;i++)
	{
		ds->peer_tab[i] = ds->rpc_s->peer_tab[i];
	}
	free(ds->rpc_s->peer_tab);
	ds->rpc_s->peer_tab = ds->peer_tab;

	peer = ds->peer_tab;
	for (i = 0; i < ds->peer_size; i++) 
	{
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;

		peer->num_msg_recv = 0;
		peer->num_msg_ret = 0;	  

		peer->sys_msg_recv = 0;
		peer->sys_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->sys_msg_ret = 0;	 

		peer++;
	}

	rpc_server_set_peer_ref(ds->rpc_s, ds->peer_tab, ds->peer_size);
	rpc_server_set_rpc_per_buff(ds->rpc_s, ds->peer_size);
	ds->rpc_s->app_num_peers = num_sp;
	ds->rpc_s->app_minid = 0;

	//rpc_add_service(cn_register, dsrpc_cn_register);////
        rpc_add_service(cn_unregister, dsrpc_cn_unregister);
	//rpc_add_service(sp_reg_request, dsrpc_sp_register);// dont need in GNI
	//rpc_add_service(sp_reg_reply, dsrpc_sp_ack_register);// dont need in GNI

	err = ds_boot(ds);
	if (err < 0)
		goto err_free_dsrv;

	if (num_sp == 1)
	{
		// If there is a single server, mark it as registered, but it should also be master!
		ds->f_reg = 1;
	}

	for(i=ds->num_sp;i<num_cp+num_sp;i++)
	{
	  if((i % ds->num_sp) == ds->self->ptlmap.id)
	    ds->num_charge++;
	}
	ds->num_charge_cp = ds->num_charge;

	if(ds->comm) {
		err = MPI_Barrier(*ds->comm);
		assert(err == MPI_SUCCESS);
	} else {
		err = PMI_Barrier();	
		assert(err == PMI_SUCCESS);
	}

	uloga("'%s(%d)': init ok.\n", __func__, ds->self->ptlmap.id);
	//print_ds(ds);

	return ds;

 err_free_dsrv:
	uloga("'%s()': failed with %d.\n", __func__, err);
	free(ds);
	return NULL;
 err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return NULL;
}

int print_ds(struct dart_server *ds)
{
  int i;

  uloga("ds is:\n ds->rpc_s: ptlmap[nid(%d),pid(%d),id(%d),appid(%d)];\n num_buf(%d);num_rpc_per_buff(%d);max_num_msg(%d);com_type(%d);num_peers(%d);bar_num(%d);app_minid(%d);app_num_peers(%d);num_md_posted(%d);num_md_unlinked(%d);\n ds->peer_size(%d), ds->num_cp(%d), ds->num_sp(%d), ds->size_cp(%d), ds->size_sp(%d), f_reg(%d), f_stop(%d), f_nacc(%d);\n ds->self:[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", ds->rpc_s->ptlmap.nid, ds->rpc_s->ptlmap.pid, ds->rpc_s->ptlmap.id, ds->rpc_s->ptlmap.appid,ds->rpc_s->num_buf, ds->rpc_s->num_rpc_per_buff, ds->rpc_s->max_num_msg, ds->rpc_s->cmp_type, ds->rpc_s->num_peers, ds->rpc_s->bar_num, ds->rpc_s->app_minid, ds->rpc_s->app_num_peers, ds->rpc_s->num_md_posted, ds->rpc_s->num_md_unlinked, ds->peer_size,ds->num_cp, ds->num_sp, ds->size_cp, ds->size_sp, ds->f_reg, ds->f_stop, ds->f_nacc, ds->self->ptlmap.nid, ds->self->ptlmap.pid, ds->self->ptlmap.id, ds->self->ptlmap.appid, ds->self->num_req, ds->self->f_req_msg, ds->self->f_need_msg, ds->self->num_msg_at_peer, ds->self->num_msg_ret);

for(i=0; i<ds->peer_size;i++)
  {
  uloga("ds->peer_tab(%d):[nid(%d),pid(%d),id(%d),appid(%d), num_req(%d), f_reg_msg(%d), f_need_msg(%d), num_msg_at_peer(%d), num_msg_ret(%d)];\n", i, ds->peer_tab[i].ptlmap.nid, ds->peer_tab[i].ptlmap.pid, ds->peer_tab[i].ptlmap.id, ds->peer_tab[i].ptlmap.appid, ds->peer_tab[i].num_req, ds->peer_tab[i].f_req_msg, ds->peer_tab[i].f_need_msg, ds->peer_tab[i].num_msg_at_peer, ds->peer_tab[i].num_msg_ret);
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
		uloga("(%s): rpc server free failed.\n", __func__);
	//uloga("Rank(%d): step2.1.\n",track);//debug
	list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry) 
	{
		list_del(&app->app_entry);
		free(app);
        }
	//uloga("Rank(%d): step2.2.\n",track);//debug
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

