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

enum core_type {
    hs_manager_core = 1,
    hs_worker_core
};

enum worker_type {
    hs_simulation_worker = 1,
    hs_staging_worker
};

enum location_type {
    hs_insitu = 1,
    hs_intransit
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

enum cods_location_type {
	loc_insitu = 0,
	loc_intransit
};

enum cods_placement_hint {
	hint_insitu = 0,
	hint_intransit,
	hint_none
};

#ifdef HAVE_UGNI
struct executor_topology_info {
    uint32_t nid;
    rca_mesh_coord_t mesh_coord; 
} __attribute__((__packed__));

struct node_topology_info {
    uint32_t nid;
    rca_mesh_coord_t mesh_coord;
};
#endif

/*
 *  Message headers 
 */
struct hdr_register_resource {
    int pool_id;
    int num_bucket;
    int mpi_rank;
    struct executor_topology_info topo_info;
} __attribute__((__packed__));

struct hdr_update_var {
	int op; // TODO: what is this needed for?
	char name[MAX_VAR_NAME_LEN];
	int version;
	int elem_size;
    struct global_dimension gdim;
} __attribute__((__packed__));

struct hdr_exec_task {
    uint32_t wid;
    uint32_t tid;
    int appid;
	int rank_hint;
	int nproc_hint;
	int num_vars;
} __attribute__((__packed__));

struct hdr_finish_task {
    int pool_id;
    uint32_t wid;
    uint32_t tid;
} __attribute__((__packed__));

struct hdr_submit_task {
    uint32_t wid;
    uint32_t tid;
    char conf_file[NAME_MAXLEN];
} __attribute__((__packed__));

struct hdr_submitted_task_done {
    uint32_t wid;
    uint32_t tid;
    double task_execution_time;
} __attribute__((__packed__));
   
struct hdr_build_staging {
    int pool_id;
    char staging_conf_file[NAME_MAXLEN];
} __attribute__((__packed__));

struct hdr_finish_workflow {
    uint32_t wid;
} __attribute__((__packed__));

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
    uint32_t wid;
	uint32_t tid;
    int appid; // id of workflow component application/operation/executable
	int rank;
	int nproc;
	int num_vars;
    int *bk_mpi_rank_tab;
	struct cods_var *vars;
};

struct task_entry {
    struct list_head entry;
    uint32_t wid; // TODO: why we need both wid and tid?
    uint32_t tid;
    int appid; // id of workflow component application/operation/executable 
    enum cods_task_status status;
    enum cods_placement_hint placement_hint;
    int size_hint;
    struct cods_var vars[MAX_NUM_VARS];
    int num_vars;
    int submitter_dart_id; // peer who submits the task execution
};

struct workflow_state {
    unsigned char f_done;
};

struct cods_workflow {
    struct list_head entry;
    uint32_t wid;
    struct workflow_state state;
    struct list_head task_list;
	int num_tasks;
};

int parse_task_conf_file(struct task_entry *task, const char *fname);
int evaluate_dataflow_by_available_var(struct cods_workflow *wf, const struct cods_var *var_desc);
int get_ready_tasks(struct cods_workflow *wf, struct task_entry **tasks, int *num_ready_tasks);
void update_task_status(struct task_entry *task, enum cods_task_status status);
int is_task_finish(struct task_entry *task);
int is_task_ready(struct task_entry *task);

void print_workflow(struct cods_workflow *wf);
#ifdef __cplusplus
}
#endif

#endif
