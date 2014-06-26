#ifndef __DC_BASE_PAMI_H_
#define __DC_BASE_PAMI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "dart_rpc_pami.h"

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

        //For testing DART PAMI performance
        int     read_complete;

	void* 	comm;	//MPI communicator for barrier
};

//#define dc_barrier(dc) rpc_barrier(dc->rpc_s)
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


//int dc_read_test(struct rpc_server *rpc_s, size_t size, int target);
int dc_read_test(struct dart_client *dc, size_t size);


#ifdef __cplusplus
}
#endif

#endif

