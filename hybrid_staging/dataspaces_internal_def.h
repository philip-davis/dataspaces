#ifndef __DS_INTERNAL_DEF_H__
#define __DS_INTERNAL_DEF_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "dataspaces_api.h"
#include "dart_rpc_gni.h"

/* structure to keep rdma memory address and descriptor of a data object */
struct rdma_data_descriptor {
        int                     dart_id;
        struct mdh_addr_t       mdh_addr;
        struct data_descriptor  desc;
};

struct data_desc {
        struct list_head desc_entry;
        struct rdma_data_descriptor desc;
};

/*TODO: only added for testing purpose ...*/
//int ds_rank();
//void ds_do_barrier();

#ifdef __cplusplus
}
#endif

#endif
