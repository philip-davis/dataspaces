#ifndef __HSTAGING_DEF_H__
#define __HSTAGING_DEF_H__

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

#define NAME_MAXLEN 128
#define MAX_VAR_NAME_LEN 32
#define MAX_NUM_TASKS 512 
#define MAX_NUM_VARS 48 

// TODO: change to lowercase, reasonable naming for the enum types below
enum hstaging_var_type {
	var_type_depend = 0,
	var_type_put,
	var_type_get,
};

enum hstaging_var_status {
	var_not_available = 0,
	var_available,
};

enum hstaging_task_status {
	task_not_ready = 0,
    task_ready,
    task_pending,
    task_running,
    task_finish,
};

enum hstaging_bucket_status {
    bucket_idle = 0,
    bucket_busy
};

enum hstaging_update_var_op {
	OP_PUT = 0,
	OP_GET,
};

enum hstaging_pe_type {
    hs_manager = 0,
    hs_executor,
    hs_submitter,
};

enum hstaging_location_type {
	loc_insitu = 0,
	loc_intransit
};

enum hstaging_placement_hint {
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

struct hstaging_var {
	char name[MAX_VAR_NAME_LEN];
	enum hstaging_var_type type;
    enum hstaging_var_status status;
    int version;
    size_t elem_size;
    struct global_dimension gdim;
    struct block_distribution distribution_hint;    
};

struct task_descriptor {
    uint32_t wid;
	uint32_t tid;
    int appid; // id of workflow component application/operation/executable
	int rank;
	int nproc;
	int num_vars;
    int *bk_mpi_rank_tab;
	struct hstaging_var *vars;
};

struct hstaging_task {
    struct list_head entry;
    uint32_t wid; // TODO: why we need both wid and tid?
    uint32_t tid;
    int appid; // id of workflow component application/operation/executable 
    enum hstaging_task_status status;
    enum hstaging_placement_hint placement_hint;
    int size_hint;
    struct hstaging_var vars[MAX_NUM_VARS];
    int num_vars;
    int submitter_dart_id; // peer who submits the task execution
};

struct workflow_state {
    unsigned char f_done;
};

struct hstaging_workflow {
    struct list_head entry;
    uint32_t wid;
    struct workflow_state state;
    struct list_head task_list;
	int num_tasks;
};

int parse_task_conf_file(struct hstaging_task *task, const char *fname);
int evaluate_dataflow_by_available_var(struct hstaging_workflow *wf, const struct hstaging_var *var_desc);
int get_ready_tasks(struct hstaging_workflow *wf, struct hstaging_task **tasks, int *num_ready_tasks);
void update_task_status(struct hstaging_task *task, enum hstaging_task_status status);
int is_task_finish(struct hstaging_task *task);
int is_task_ready(struct hstaging_task *task);

void print_workflow(struct hstaging_workflow *wf);

#ifdef __cplusplus
}
#endif

#endif
