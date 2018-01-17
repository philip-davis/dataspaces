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
#ifndef __DC_BASE_GNI_H__
#define __DC_BASE_GNI_H__

#include "dart_rpc_gni.h"
#include <mpi.h>

#define dc_barrier(dc)	rpc_barrier(dc->rpc_s)

struct dart_client {
	struct rpc_server	*rpc_s;

	int			peer_size;
	struct node_id		*peer_tab;

	int			num_sp, num_cp;

	struct node_id		*self;

	/* Number of compute peers in the app job. */
	int			cp_in_job;
	/* Rank of the master peer in the app job. */
	int			cp_min_rank;

	int			cp_barrier_req;

        /* Registration flag; 0 - unregistred, 1 - registered. */
        int			f_reg:1;
        int			f_bar:1;

        void			*dart_ref;
        MPI_Comm        *comm;

        int			num_posted;
};

// normally, n represents destination id .
static inline struct node_id * dc_get_peer(struct dart_client *dc, int peer_id)
{

    int count=0;
    struct node_id *cur_peer;

    cur_peer = dc->peer_tab;

    while(cur_peer){
        //count = count + cur_peer->peer_num;

        if(peer_id < (cur_peer->ptlmap.id + cur_peer->peer_num ) && peer_id > (cur_peer->ptlmap.id - 1))
            return cur_peer + peer_id - cur_peer->ptlmap.id;
        else
            cur_peer = (struct node_id *)(cur_peer + cur_peer->peer_num - 1)->next;

    }

    printf("%s: cannot find peer in peer_tab error -1.\n", __func__);
    return NULL;

}

static inline struct dart_client *dc_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm);
void dc_free(struct dart_client *dc);
int dc_process(struct dart_client *dc);
int print_dc(struct dart_client *dc);

#endif
