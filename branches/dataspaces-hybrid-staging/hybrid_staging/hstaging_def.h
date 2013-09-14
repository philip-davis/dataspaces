#ifndef __HSTAGING_DEF_H__
#define __HSTAGING_DEF_H__

#ifdef __cplusplus
extern "C" {
#endif

//#include "dart.h"
#include "list.h"
#include "bbox.h"

#define NAME_MAXLEN 128
#define MAX_NUM_TASKS 512 
#define MAX_NUM_VARS 48 

// TODO: change to lowercase, reasonable naming for the enum types below
enum hstaging_var_type {
	DEPEND = 0,
	PUT,
	GET,
};

enum hstaging_var_status {
	NOT_AVAILABLE = 0,
	AVAILABLE,
};

enum hstaging_task_status {
	NOT_READY = 0,
	READY,
	PENDING,
	RUN,
	FINISH,
};

enum hstaging_update_var_op {
	OP_PUT = 0,
	OP_GET,
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

struct hdr_update_var {
	int op;
	char var_name[32];
	int step;
	int size;
	struct bbox bb;
} __attribute__((__packed__));

struct hdr_request_task {
	int mpi_rank;
	int location_type;
} __attribute__((__packed__));

struct hdr_exec_task {
	int tid;
	int step;
	int rank;
	int nproc;
	int num_input_vars;
} __attribute__((__packed__));

static char* var_type_name[] =
{
	"depend", "put", "get"
};

////
struct var_descriptor {
	char var_name[32];
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
	struct var_descriptor *input_vars;
};

////

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
};

////

struct hstaging_var {
	char name[NAME_MAXLEN];
	enum hstaging_var_type type;
};

struct hstaging_task {
	int tid;
	enum hstaging_placement_hint placement_hint;
	// char name[NAME_MAXLEN];
	struct hstaging_var vars[MAX_NUM_VARS];	
	int num_vars;
	struct list_head instances_list;
};

struct hstaging_workflow {
	struct hstaging_task tasks[MAX_NUM_TASKS];
	int num_tasks;
};

////
struct hstaging_workflow* read_workflow_conf_file(const char *fname);
int free_workflow(struct hstaging_workflow *wf);

int evaluate_dataflow_by_available_var(struct hstaging_workflow *wf, struct var_descriptor *var_desc);
int get_ready_tasks(struct hstaging_workflow *wf, struct task_instance **tasks, int *n);
void update_task_instance_status(struct task_instance *ti, enum hstaging_task_status status);

int read_emulated_vars_sequence(struct hstaging_workflow *wf, const char *fname);
void print_workflow(struct hstaging_workflow *wf);


#ifdef __cplusplus
}
#endif

#endif
