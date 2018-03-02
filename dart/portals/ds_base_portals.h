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
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*/
#ifndef __DS_BASE_PORTALS_H_
#define __DS_BASE_PORTALS_H_

#include "dart_rpc_portals.h"

/*
  Structure to represent an application that uses the Spaces.
*/
struct app_info {
        struct list_head        app_entry;

        char                    *app_name;      /* Application name */
        int                     app_id;         /* Application identifier */

        int                     app_num_peers;  /* Total number of peers in app */
        struct node_id          *app_peer_tab;  /* Reference to app nodes info */

        int                     app_cnt_peers;  /* Peers so far */
};

struct dart_server {
        struct rpc_server       *rpc_s;

        /* List (array) of peer nodes; this should be of fixed size. */
        int                     peer_size;
        struct node_id          *peer_tab;
        struct node_id          *cn_peers;

        /* Number of compute node peers; number of server peers. */
        int                     num_cp, num_sp;
        int                     size_cp, size_sp;

        struct list_head        app_list;       /* List of applications */

        /* Reference for self instance in 'peer_tab'. */
        struct node_id          *self;

	/* 'f_reg' records if registration is complete. */
        int                     f_reg:1;
        int                     f_stop:1;

        /* Flag to accept new requests or drom exsiting ones. */
        int                     f_nacc:1;

        /* Reference to the front end module used. */
        void                    *dart_ref;
};

struct dart_server* ds_alloc(int, int, void *, void *);
void ds_free(struct dart_server *);
int ds_process(struct dart_server *);

#define ds_barrier(ds)  rpc_barrier(ds->rpc_s)

static inline struct dart_server * ds_ref_from_rpc(struct rpc_server *rpc_s)
{
        return rpc_s->dart_ref;
}


/* TODO: rename to getid() */
static inline int ds_get_rank(struct dart_server *ds)
{
        return ds->self->ptlmap.id;
}

static inline struct node_id * ds_get_peer(struct dart_server *ds, int n)
{
        return (ds->peer_tab + n);
}

static inline int ds_stop(struct dart_server *ds)
{
        return ds->f_stop;
}

/*
int dart_server_start(struct dart_server *);
void dart_server_stop(struct dart_server *);
void dart_server_wait(struct dart_server *);
*/

#endif /* __DS_BASE_H_ */
