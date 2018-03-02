#ifndef __DS_BASE_PAMI_H_
#define __DS_BASE_PAMI_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "dart_rpc_pami.h"

struct app_info;

/*
 *   Structure to represent an application that uses the Spaces.
 *   */
struct app_info {
        struct list_head        app_entry;

        char                    *app_name;      /* Application name */
        int                     app_id;         /* Application identifier */

        int                     app_num_peers;  /* Total number of peers in app */
        struct node_id          *app_peer_tab;  /* Reference to app nodes info */

        int                     app_cnt_peers;  /* Peers so far */
};

struct dart_server{
        struct rpc_server *rpc_s;

        int             peer_size;
        struct  node_id *peer_tab;
        struct  node_id *cn_peers;

        int     num_cp,num_sp;
        int     size_cp,size_sp;
	int 	num_charge;
	int 	num_charge_cp;

        struct list_head        app_list;

        struct node_id  *self;

        int     f_reg;
        int     f_stop;

        int     f_nacc;
        void    *dart_ref;
};

struct dart_server* ds_alloc(int, int, void*, void*);
void ds_free(struct dart_server*);
int ds_process(struct dart_server*);

#define ds_barrier(ds) rpc_barrier(ds->rpc_s);

static inline struct dart_server* ds_ref_from_rpc(struct rpc_server *rpc_s)
{
        return (struct dart_server*)rpc_s->dart_ref;
}

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

#ifdef __cplusplus
}
#endif

#endif

//int dsrpc_sp_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd);
//int dsrpc_cn_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd);
int dsrpc_cn_read(struct rpc_server *rpc_s, struct rpc_cmd *cmd);
