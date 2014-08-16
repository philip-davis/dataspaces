#ifndef __CODS_DEF_H__
#define __CODS_DEF_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "debug.h"
#include "list.h"
#include "bbox.h"
#include "ss_data.h"

#ifdef HAVE_UGNI
#include <pmi.h>
#include <rca_lib.h>
#endif

#define EPSI_WORKFLOW_ID 1
#define S3D_WORKFLOW_ID 2
#define DAG_WORKFLOW_ID 3
#define DNS_LES_WORKFLOW_ID 4
#define STAGING_IO_WORKFLOW_ID 5
#define S3D_ANALYSIS_WORKFLOW_ID 6

#define NAME_MAXLEN 128
#define MAX_VAR_NAME_LEN 32
#define MAX_NUM_TASKS 512 
#define MAX_NUM_VARS 48 
#define PATH_MAXLEN 128 

// default value for (1) task executor partition type;
// (2) task placement location hint;
#define DEFAULT_PART_TYPE 0

enum core_type {
    hs_manager_core = 1,
    hs_worker_core
};

enum worker_type {
    hs_simulation_worker = 1,
    hs_staging_worker
};

// TODO: change to lowercase, reasonable naming for the enum types below
enum cods_var_type {
	var_type_depend = 0,
	var_type_put,
	var_type_get,
};

enum cods_var_status {
	var_not_available = 0,
	var_available,
};

enum cods_task_status {
	task_not_ready = 0,
    task_ready,
    task_pending,
    task_running,
    task_finish,
};

enum cods_bucket_status {
    bucket_idle = 0,
    bucket_busy
};

enum cods_update_var_op {
	OP_PUT = 0,
	OP_GET,
};

enum cods_pe_type {
    cods_manager = 0,
    cods_executor,
    cods_submitter,
};

#ifdef HAVE_UGNI
struct node_topology_info {
    uint32_t nid;
    rca_mesh_coord_t mesh_coord; 
}; 
#endif

static char* var_type_name[] =
{
	"depend", "put", "get"
};

struct block_distribution {
    int ndim;
    struct coord sizes;    
};

struct cods_var {
	char name[MAX_VAR_NAME_LEN];
	enum cods_var_type type;
    enum cods_var_status status;
    int version;
    size_t elem_size;
    struct global_dimension gdim;
    struct block_distribution dist_hint;    
};

struct cods_task {
	uint32_t tid;
    int appid; // id of workflow component application/operation/executable
	int rank;
	int nproc;
	int num_vars;
    int *bk_mpi_rank_tab;
	struct cods_var *vars;
};

struct task_descriptor {
    uint32_t tid;
    char conf_file[PATH_MAXLEN];
    unsigned char location_hint;
};

struct executor_descriptor {
    int dart_id;
    int bk_idx;
    unsigned char partition_type;
    struct node_topology_info topo_info;
};

struct compute_node {
    struct node_topology_info topo_info;
    int node_num_executor;
    struct executor_descriptor* node_executor_tab;
};

struct executor_pool_info {
    int pool_id;
    uint32_t num_executor;
    struct executor_descriptor* executor_tab;
    int num_node;
    struct compute_node* node_tab;
};

/*
 *  Message headers 
 */
struct hdr_register_resource {
    int pool_id;
    int num_bucket;
    int mpi_rank;
    struct node_topology_info topo_info;
} __attribute__((__packed__));

struct hdr_update_var {
    int op; // TODO: what is this needed for?
    char name[MAX_VAR_NAME_LEN];
    int version;
    int elem_size;
    struct global_dimension gdim;
} __attribute__((__packed__));

struct hdr_exec_task {
    uint32_t tid;
    int appid;
    int rank_hint;
    int nproc_hint;
    int num_vars;
} __attribute__((__packed__));

struct hdr_finish_task {
    int pool_id;
    uint32_t tid;
} __attribute__((__packed__));

struct hdr_submit_task {
    struct task_descriptor task_desc;
} __attribute__((__packed__));

struct hdr_submitted_task_done {
    uint32_t tid;
    double task_execution_time;
} __attribute__((__packed__));

struct hdr_get_executor_pool_info {
    int pool_id;
} __attribute__((__packed__));

struct hdr_executor_pool_info {
    int pool_id;
    uint32_t num_executor;
} __attribute__((__packed__));

struct hdr_build_partition {
    int pool_id;
    uint32_t num_executor;
} __attribute__((__packed__));

#ifdef __cplusplus
}
#endif

#endif
