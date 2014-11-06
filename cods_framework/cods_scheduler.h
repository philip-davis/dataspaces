#ifndef __CODS_SCHEDULER_H__
#define __CODS_SCHEDULER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "debug.h"
#include "timer.h"
#include "list.h"
#include "dart.h"
#include "dataspaces.h"
#include "dc_gspace.h"

#include "cods_internal.h"

enum bucket_status {
    bk_none = 0, // not ready for task execution 
    bk_idle,
    bk_busy,
};

struct cods_framework_state {
    unsigned char f_done;
    unsigned char f_notify_executor_to_exit;
};

struct runnable_task {
    struct list_head entry;
    struct task_entry *task_ref;
    // int num_input_vars;
    // enum cods_location_type location_type;
    int bk_pool_id; // pool id for the allocated buckets
    int bk_allocation_size; // number of buckets required for task execution
    int *bk_idx_tab; // array of idx to the buckets in bk_tab 
};

struct task_entry {
    struct list_head entry;
    uint32_t tid;
    int appid; // id of workflow component application/operation/executable 
    enum cods_task_status status;
    int size_hint;
    struct cods_var vars[MAX_NUM_VARS];
    int num_vars;
    int submitter_dart_id; // peer who submits the task execution
    // programmer-provided placement hint information
    unsigned char location_hint;
};

struct bucket {
    int dart_id; // unique id
    int pool_id; // indicate which resource pool the bucket is from
    int origin_mpi_rank;
    unsigned char partition_type;
    enum bucket_status status;
    struct node_topology_info topo_info;
};

struct bucket_pool {
    struct list_head entry;
    int pool_id;
    int num_bucket;
    int bk_tab_size;
    struct bucket *bk_tab;
    int f_bk_reg_done;
};

struct node_entry {
    struct list_head entry;
    uint32_t nid;
    int bk_tab_offset;
    int num_idle_bk;
    uint64_t available_data_size;
};

struct cods_scheduler {
    struct dcg_space* dcg;
    struct cods_framework_state framework_state;     
    struct list_head bk_pool_list;
    struct list_head rtask_list;
    struct list_head pending_msg_list;
    struct list_head task_list;
};

int cods_scheduler_parse_args(int argc, char** argv);
void cods_scheduler_usage();

int cods_scheduler_init(int num_peers, int appid);
int cods_scheduler_run();
int cods_scheduler_finalize();

#ifdef __cplusplus
}
#endif

#endif
