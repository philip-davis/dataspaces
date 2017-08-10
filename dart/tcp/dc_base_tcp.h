#ifndef __DC_BASE_TCP_H_
#define __DC_BASE_TCP_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "dart_rpc_tcp.h"

#include<mpi.h>

struct dart_client{
        struct rpc_server       *rpc_s;

        int             peer_size;
        struct node_id  *peer_tab;
        struct node_id  *cn_peers;

        int     num_sp,num_cp;

        struct node_id  *self;

        /*Number of compute peers in the app job*/
        int     cp_in_job;
        /*Rank of the master peer in the app job*/
        int     cp_min_rank;

        /*??*/
        int     cp_barrier_req;

        int     f_reg:1; //Registration flag
        int     f_bar:1;

        void    *dart_ref;
        int     num_posted;

	MPI_Comm    *comm;	//MPI communicator for barrier
};

#define dc_barrier(dc) rpc_barrier(dc->rpc_s, dc->comm)

static inline struct node_id * dc_get_peer(struct dart_client *dc, int n)
{
        return dc->peer_tab + n;
}

static inline struct dart_client *dc_ref_from_rpc(struct rpc_server *rpc_s)
{
        return rpc_s->dart_ref;
}

struct dart_client* dc_alloc(int,int,void*,void*);
void dc_free(struct dart_client*);
int dc_process(struct dart_client*);

#ifdef __cplusplus
}
#endif

#endif

