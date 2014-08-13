#ifndef __CODS_SCHEDULER_H__
#define __CODS_SCHEDULER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "debug.h"
#include "timer.h"
#include "list.h"
#include "dart.h"
#include "ds_gspace.h"
#include "dimes_server.h"

#include "cods_def.h"

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
    int bk_allocation_size; // number of buckets required for task execution
    int *bk_idx_tab; // array of idx to the buckets in bk_tab 
};

struct task_entry {
    struct list_head entry;
    uint32_t tid;
    int appid; // id of workflow component application/operation/executable 
    enum cods_task_status status;
    enum cods_placement_hint placement_hint;
    int size_hint;
    struct cods_var vars[MAX_NUM_VARS];
    int num_vars;
    int submitter_dart_id; // peer who submits the task execution
};

#define MAX_NUM_BK_PER_NODE 32
struct compute_node {
    struct node_topology_info topo_info;
    int num_bucket;
    int bk_idx_tab[MAX_NUM_BK_PER_NODE];
};

struct bucket {
    int dart_id; // unique id
    int pool_id; // indicate which resource pool the bucket is from
    int origin_mpi_rank;
    enum bucket_status status;
    struct executor_topology_info topo_info;
};

struct bucket_pool {
    struct list_head entry;
    int pool_id;
    int num_bucket;
    int bk_tab_size;
    struct bucket *bk_tab;
    int num_node;
    struct compute_node *node_tab;
    int f_bk_reg_done;
    int f_node_tab_done;
};

struct pending_msg {
    struct list_head entry;
    struct rpc_cmd cmd;
};

struct cods_scheduler {
    struct dimes_server *dimes_s; 
    struct cods_framework_state framework_state;     
    struct list_head bk_pool_list;
    struct list_head rtask_list;
    struct list_head pending_msg_list;
    struct list_head task_list;
};

int cods_scheduler_parse_args(int argc, char** argv);
void cods_scheduler_usage();

int cods_scheduler_init();
int cods_scheduler_run();
int cods_scheduler_finish();

#ifdef __cplusplus
}
#endif

#endif
