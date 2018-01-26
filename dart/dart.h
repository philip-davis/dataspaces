#ifndef __DART_H_
#define __DART_H_

#include "config.h"

#if HAVE_MPI
#include<mpi.h>
#endif /* HAVE_MPI */

#if USE_DART2

#include "dart_rpc.h"

//TODO: move to ds_gspace
enum lock_type {
    lk_read_get,
    lk_read_release,
    lk_write_get,
    lk_write_release,
    lk_grant
};

//TODO: move to d2_gspace
/*
  Header  structure  used  to  send  timing messages.  Note:  size  of
  time_tab[] should fit into the pad field of a 'struct rpc_cmd'.
*/
struct hdr_timing {
        /* Timer offsets into the table to be determined by the
           applications. */
        int             time_num;
        double          time_tab[20];
};

#define LOCK_NAME_SIZE 64
/* Header for the locking service. */
struct lockhdr {
	int type;
	int rc;
	int id;
	int lock_num;
	char name[LOCK_NAME_SIZE];	/* lock name */
} __attribute__ ((__packed__));

#define dart_server dart_agent
#define dart_client dart_agent

struct dart_agent {
    struct rpc_server *rpc_s;

    int peer_size;

    struct node_id *self;

    int f_reg;
    int f_stop;

    int f_nacc;
    void *dart_ref;

#if HAVE_MPI
    MPI_Comm *comm;
#endif /* HAVE_MPI */

};


//Backwards-compatible dart client interface
int dc_barrier(struct dart_client *dc);
struct dart_agent *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm);
void dc_free(struct dart_agent *da);
int dc_process(struct dart_agent *dc);

//Backwards-compatible dart server interface
int ds_barrier(struct dart_server *ds);
struct dart_agent *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm);
void ds_free(struct dart_agent* ds);
int ds_process(struct dart_agent* ds);

//====//
int da_barrier(struct dart_agent *da);
//====//


#else /* USE_DART2 */
#if HAVE_UGNI

#include "gni/dart_rpc_gni.h"
#include "gni/dc_base_gni.h"
#include "gni/ds_base_gni.h"
#include "gni/dart_rdma_gni.h"

#elif HAVE_PORTALS

#include "portals/dart_rpc_portals.h"
#include "portals/dc_base_portals.h"
#include "portals/ds_base_portals.h"

#elif HAVE_INFINIBAND

#include "ib/dart_rpc_ib.h"
#include "ib/dc_base_ib.h"
#include "ib/ds_base_ib.h"
#include "ib/dart_rdma_ib.h"

#elif HAVE_DCMF

#include "dcmf/dart_rpc_dcmf.h"
#include "dcmf/dc_base_dcmf.h"
#include "dcmf/ds_base_dcmf.h"
#include "dcmf/dart_rdma_dcmf.h"

#elif HAVE_PAMI

#include "pami/dart_rpc_pami.h"
#include "pami/dc_base_pami.h"
#include "pami/ds_base_pami.h"
#include "pami/dart_rdma_pami.h"

#elif HAVE_TCP_SOCKET

#include "tcp/dart_rpc_tcp.h"
#include "tcp/dc_base_tcp.h"
#include "tcp/ds_base_tcp.h"

#endif

#endif

#endif
