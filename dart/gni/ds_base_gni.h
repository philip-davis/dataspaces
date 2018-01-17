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

#ifndef __DS_BASE_GNI_H__
#define __DS_BASE_GNI_H__

#include <mpi.h>

#include "dart_rpc_gni.h"
#include <mpi.h>

#define ds_barrier(ds)  rpc_barrier(ds->rpc_s)


/*
  Structure to represent an application that uses the Spaces.
*/
struct app_info {
	struct list_head	app_entry;
	char			*app_name;      /* Application name */
	int			app_id;         /* Application identifier */
	int			app_num_peers;  /* Total number of peers in app */
	struct node_id		*app_peer_tab;  /* Reference to app nodes info */
	int			app_cnt_peers;  /* Peers so far */
	int     f_reg; //DSaaS: if this app has been registered. 0: not yet; 1: already.
};

struct dart_server {
	struct rpc_server	*rpc_s;

	/* List (array) of peer nodes; this should be of fixed size. */
	int			peer_size;
	struct node_id		*peer_tab;
	struct node_id		*cn_peers;

	/* Number of compute node peers; number of server peers. */
	int			num_cp, num_sp;
	int			size_cp, size_sp;
        int                     num_charge;
        int                     num_charge_cp;
	int			app_num;

	struct list_head	app_list;       /* List of applications */

	/* Reference for self instance in 'peer_tab'. */
	struct node_id		*self;

	/* 'f_reg' records if registration is complete. */
	int			f_reg:1;
	int			f_stop:1;

	/* Flag to accept new requests or drom exsiting ones. */
	int			f_nacc:1;

	/* Reference to the front end module used. */
	void			*dart_ref;
    
    /* MPI Communicator (can be null) */
    MPI_Comm        *comm;

	pthread_t		comm_thread;
	int			thread_alive;

};

struct dart_server* ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm);
void ds_free(struct dart_server *ds);
int ds_process(struct dart_server *ds);

int print_ds(struct dart_server *ds);

static inline struct dart_server * ds_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}

static inline int ds_get_rank(struct dart_server *ds)
{
	return ds->self->ptlmap.id;
}

static inline struct node_id * ds_get_peer(struct dart_server *ds, int peer_id)
{
	int count=0;
	struct node_id *cur_peer;

	cur_peer = ds->peer_tab;

	while(cur_peer){
		//count = count + cur_peer->peer_num;

	  if(peer_id < (cur_peer->ptlmap.id + cur_peer->peer_num ) && peer_id > (cur_peer->ptlmap.id - 1))
			return cur_peer + peer_id - cur_peer->ptlmap.id;
		else
			cur_peer = (struct node_id *)(cur_peer + cur_peer->peer_num - 1)->next;	

	}

	printf("Rank %d: %s: cannot find peer %d in peer_tab error -1.\n", ds->rpc_s->ptlmap.id, __func__, peer_id);
	return NULL;
}

static inline int ds_stop(struct dart_server *ds)
{
	return ds->f_stop;
}

#endif
