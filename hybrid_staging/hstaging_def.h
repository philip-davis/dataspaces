#ifndef __HSTAGING_DEF_H__
#define __HSTAGING_DEF_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "dart.h"
#include "list.h"
#include "bbox.h"

/* structure to keep rdma memory address and descriptor of a data object */
struct rdma_data_descriptor {
	int	dart_id;
	int num_obj_to_receive;
#ifdef HAVE_UGNI
	struct mdh_addr_t	mdh_addr;
#endif
#ifdef HAVE_DCMF
	DCMF_Memregion_t	mem_region;
#endif
	struct data_descriptor  desc;
};

struct data_desc {
	struct list_head desc_entry;
	struct rdma_data_descriptor desc;
};

enum hstaging_update_var_op {
	OP_PUT = 0,
	OP_GET,
};

struct hdr_update_var {
	int op;
	char var_name[32];
	int step;
	int size;
	struct bbox bb;
} __attribute__((__packed__));

struct hdr_exec_task {
	int tid;
	int step;
	int rank;
	int nproc;
	int num_input_vars;
} __attribute__((__packed__));

#define NAME_MAXLEN 128
#define MAX_NUM_TASKS 512 
#define MAX_NUM_VARS 48 

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

static char* var_type_name[] =
{
	"DEPEND", "PUT", "GET"
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
	char name[NAME_MAXLEN];
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
