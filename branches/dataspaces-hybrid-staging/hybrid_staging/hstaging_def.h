#ifndef __HSTAGING_DEF_H__
#define __HSTAGING_DEF_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"
#include "bbox.h"

#define NAME_MAXLEN 128
#define MAX_VAR_NAME_LEN 32
#define MAX_NUM_TASKS 512 
#define MAX_NUM_VARS 48 

//#define BK_GROUP_BASIC_SIZE 4 

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

/*
 *  Message headers 
 */
struct hdr_register_resource {
    int pool_id;
    int num_bucket;
    int mpi_rank;
} __attribute__((__packed__));

struct hdr_update_var {
	int op;
	char var_name[MAX_VAR_NAME_LEN];
	int step;
	int size;
	struct bbox bb;
} __attribute__((__packed__));

struct hdr_exec_task {
	int tid;
	int step;
	int rank_hint;
	int nproc_hint;
	int num_input_vars;
} __attribute__((__packed__));

struct hdr_finish_task {
    int pool_id;
    int tid;
    int step;
} __attribute__((__packed__));

struct hdr_exec_dag {
    char dag_conf_file[MAX_VAR_NAME_LEN];
} __attribute__((__packed__));

struct hdr_finish_dag {
    double dag_execution_time;
} __attribute__((__packed__));
   

static char* var_type_name[] =
{
	"depend", "put", "get"
};

struct var_descriptor {
	char var_name[MAX_VAR_NAME_LEN];
	int step; // time step
	struct bbox bb; // global domain
	size_t size; // size of data element
};

struct task_descriptor {
	int tid;
	int step;
	int rank;
	int nproc;
	int num_input_vars;
    int *bk_mpi_rank_tab;
	struct var_descriptor *input_vars;
};

struct var_instance {
	struct list_head entry;
	struct hstaging_var *var;
	enum hstaging_var_status status;
	size_t size;
	struct bbox bb;
};

struct task_instance {
	struct list_head entry;
	int tid;
	int step;
	struct list_head input_vars_list;
	enum hstaging_task_status status;
    enum hstaging_placement_hint placement_hint; 
    int size_hint;
};

struct hstaging_var {
	char name[NAME_MAXLEN];
	enum hstaging_var_type type;
};

struct hstaging_task {
	int tid;
	enum hstaging_placement_hint placement_hint;
    int size_hint; // number of processor cores
	struct hstaging_var vars[MAX_NUM_VARS];	
	int num_vars;
	struct list_head instances_list;
    int num_finished_ti;
};

struct hstaging_workflow {
	struct hstaging_task tasks[MAX_NUM_TASKS];
	int num_tasks;
    int submitter_dart_id; // peer who submits the dag execution
};

struct hstaging_workflow* read_workflow_conf_file(const char *fname);
int free_workflow(struct hstaging_workflow *wf);

int evaluate_dataflow_by_available_var(struct hstaging_workflow *wf,
    struct var_descriptor *var_desc);
int get_ready_tasks(struct hstaging_workflow *wf, struct task_instance **tasks,
    int *n);
int clear_finished_tasks(struct hstaging_workflow *wf);
void update_task_instance_status(struct task_instance *ti,
    enum hstaging_task_status status);

int is_workflow_finished(struct hstaging_workflow *wf);

int read_emulated_vars_sequence(struct hstaging_workflow *wf, const char *fname);
void print_workflow(struct hstaging_workflow *wf);


#ifdef __cplusplus
}
#endif

#endif
