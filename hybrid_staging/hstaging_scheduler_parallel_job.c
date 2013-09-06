#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "arpa/inet.h"
#include <getopt.h>
#include <stdio.h>
#include "unistd.h"

#include "debug.h"
#include "list.h"
#include "dart.h"
#include "ds_gspace.h"
#include "dimes_server.h"

#include "hybrid_staging_api.h"
#include "hstaging_def.h"
#include "mpi.h"


const int BK_TAB_SIZE = 1024;
const int BK_PER_JOB = 4;

static int num_sp;
static int num_cp;
static char *conf;

static struct dimes_server *dimes_s = NULL;
static struct dart_server *ds = NULL;
static struct ds_gspace *dsg = NULL;

static struct hstaging_workflow *wf = NULL;

/*
 TODO: this file will be merged into ds_gspace.c ??
*/
// Header for messages that request bucket allocation from master srv
/*
struct hdr_req_allocation {
	struct job_id jid;
} __attribute__((__packed__));
*/

// Header for messages that reply bucket allocation requests
/*
struct hdr_req_allocation_reply {
	int flag;
	struct job_id jid;
	int num_bk;
} __attribute__((__packed__));

struct allocation_request {
	struct list_head req_entry;
	int	dart_id; 
};
*/

struct job_id {
	int tid;
	int step;
};

struct job {
	struct list_head entry;
	struct task_instance *ti;
	int num_input_vars;
	struct job_id id;
	int num_finish; // number of buckets that finish the execution
	int	num_bk; // number of buckets required for job execution
	int *bk_tab;
};

struct bucket {
	struct	list_head entry;
	int	dart_id;
	struct job_id current_jid;
};

struct workflow_state {
	unsigned int f_done;
	unsigned int f_send_exit_msg;
};

static struct list_head jobq;
static struct list_head idle_bk_list;
static struct list_head run_bk_list;
static struct workflow_state state;

static inline int equal_jid(struct job_id *l, struct job_id *r)
{
	return (l->tid == r->tid) && (l->step == r->step);
}

static inline int is_master()
{
	return ( 0 == ds->rpc_s->ptlmap.id );
}

/* operations on the jobq, idle_bk_list, run_bk_list */
static inline void jobq_init()
{
	INIT_LIST_HEAD(&jobq);
}

static inline void idle_bk_list_init()
{
	INIT_LIST_HEAD(&idle_bk_list);
}

static inline void run_bk_list_init()
{
	INIT_LIST_HEAD(&run_bk_list);
}

static int idle_bk_list_count()
{
	int cnt = 0;
	struct bucket *bk;
	list_for_each_entry(bk, &idle_bk_list, struct bucket, entry) {
		cnt++;
	}

	return cnt;
}

static struct bucket * idle_bk_list_remove_head()
{
	struct bucket *bk,*t;
	list_for_each_entry_safe(bk,t,&idle_bk_list,struct bucket,entry) {
		list_del(&bk->entry);
		return bk;
	}

	return NULL;
}

static inline void idle_bk_list_add(struct bucket *bk)
{
	list_add_tail(&bk->entry, &idle_bk_list);
}

static int idle_bk_list_free()
{
	uloga("%s(): idle_bk_list_count returns %d\n", __func__,
			idle_bk_list_count());

	struct bucket *bk, *t;
	list_for_each_entry_safe(bk,t,&idle_bk_list,struct bucket,entry) {
		list_del(&bk->entry);
		free(bk);
	}

	return 0;
}

static int run_bk_list_count()
{
	int cnt = 0;
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, entry) {
		cnt++;
	}

	return cnt;
}

static struct bucket* run_bk_list_lookup(int dart_id)
{
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, entry) {
		if ( dart_id == bk->dart_id )
			return bk;
	}

	return NULL;	
}

static void run_bk_list_remove(int dart_id)
{
	struct bucket *bk, *tmp;
	list_for_each_entry_safe(bk, tmp, &run_bk_list, struct bucket, entry) {
		if ( dart_id ==  bk->dart_id ) {
			list_del(&bk->entry);
			free(bk);
			return;
		}
	}
}

static inline void run_bk_list_add(struct bucket *bk)
{
	list_add_tail(&bk->entry, &run_bk_list);
}

static int run_bk_list_free()
{
	uloga("%s(): run_bk_list_count returns %d\n", __func__,
			run_bk_list_count());

	struct bucket *bk, *t;
	list_for_each_entry_safe(bk,t,&run_bk_list,struct bucket,entry) {
		list_del(&bk->entry);
		free(bk);
	}

	return 0;
}

static int jobq_count()
{
	int cnt = 0;
	struct job *j;
	list_for_each_entry(j, &jobq, struct job, entry) {
		cnt++;
	}

	return cnt;
}

static struct job * jobq_lookup(struct job_id *jid)
{
	struct job *j;
	list_for_each_entry(j,&jobq,struct job,entry) {
		if ( equal_jid(jid, &j->id) )
			return j;
	}

	return NULL;
}

static struct job * jobq_create_job(struct task_instance *ti)
{
	struct job *j = malloc(sizeof(*j));
	j->id.tid = ti->tid;
	j->id.step = ti->step;
	j->ti = ti;
	j->ti->status = PENDING; 
	j->num_finish = 0;
	j->num_bk = BK_PER_JOB;
	j->bk_tab = NULL;
	list_add_tail(&j->entry, &jobq);	

	int cnt = 0;
	struct var_instance *vi;
	list_for_each_entry(vi, &ti->input_vars_list, struct var_instance, entry) {
		cnt++;
	}
	j->num_input_vars = cnt;
	
	return j;
}

static inline int job_is_pending(struct job *j) {
	return j->ti->status == PENDING;
}

static inline int job_is_running(struct job *j) {
	return j->ti->status == RUN;
}

static inline int job_is_finish(struct job *j) {
	return j->ti->status == FINISH;
}

// TODO: very tricky implementation
static void job_update_status(struct job *j, int dart_id)
{
	if (j->ti->status == RUN) {
		j->num_finish++;
		if (j->num_finish == j->num_bk)
			j->ti->status = FINISH;
	}
}

static int job_notify_bk_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	if (msg->size > 0) {
		free(msg->msg_data);
	}
	free(msg);	

	return 0;
}

static int job_notify_bk(struct job *j, struct bucket *bk, int rank, int nproc)
{
	int err = -ENOMEM;
	struct msg_buf *msg;
	struct node_id *peer;
	peer = ds_get_peer(ds, bk->dart_id);
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	if (j->num_input_vars > 0) {
		msg->msg_data = malloc(j->num_input_vars*sizeof(struct var_descriptor));
		msg->size = j->num_input_vars*sizeof(struct var_descriptor);
		msg->cb = job_notify_bk_completion;

		// Copy the input var information
		struct var_descriptor *vars = msg->msg_data;
		struct var_instance *vi;
		int i = 0;
		list_for_each_entry(vi, &j->ti->input_vars_list, struct var_instance,
				entry) {
			strcpy(vars[i].var_name, vi->var->name);
			vars[i].step = j->ti->step;
			vars[i].bb = vi->bb;
			vars[i].size = vi->size;
			i++;
		}	
	}

	msg->msg_rpc->cmd = hs_exec_task_msg;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	struct hdr_exec_task *hdr =
		(struct hdr_exec_task *)msg->msg_rpc->pad;
	hdr->tid = j->ti->tid;
	hdr->step = j->ti->step;
	hdr->rank = rank; 
	hdr->nproc = nproc;
	hdr->num_input_vars = j->num_input_vars;

	err = rpc_send(ds->rpc_s, peer, msg);
	if (err < 0) {
		free(msg->msg_data);
		free(msg);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static void job_allocate_bk(struct job *j)
{
	if (j->ti->status != PENDING) {
		return;
	}

	if (idle_bk_list_count() < j->num_bk) {
		// Not enough free buckets available
		return;
	}

	j->bk_tab = (int*)malloc(sizeof(int)*j->num_bk);
	if (!j->bk_tab) {
		uloga("%s(): malloc failed.\n", __func__);
		return;
	}

	// Select the first num_bk buckets from the idle_bk_list
	int i;
	struct bucket *bk;
	for (i = 0; i < j->num_bk; i++) {
		bk = idle_bk_list_remove_head();
		bk->current_jid = j->id;
		j->bk_tab[i] = bk->dart_id;
		// Update run_bk_list
		// TODO: why has to use two functions?
		run_bk_list_remove(bk->dart_id);
		run_bk_list_add(bk);

		if (job_notify_bk(j, bk, i, j->num_bk) < 0) {
			return;
		}
	}

	// Update job state
	j->ti->status = RUN;

	uloga("%s(): assign job (%d,%d) to %d buckets\n",
		__func__, j->id.tid, j->id.step, j->num_bk);

	return;	
}

static int jobq_free()
{
	uloga("%s(): jobq_count returns %d\n", __func__, jobq_count());

	struct job *j, *t;
	list_for_each_entry_safe(j,t,&jobq,struct job,entry) {
		list_del(&j->entry);
		if (j->bk_tab)
			free(j->bk_tab);
		free(j);
	}
}

/************/
static int timestamp_ = 0;
static void print_rr_count()
{
	int num_job_in_queue = 0;
	int num_idle_bucket = 0;
	int num_busy_bucket = 0;

	num_job_in_queue = jobq_count();
	num_idle_bucket = idle_bk_list_count();
	num_busy_bucket = run_bk_list_count();

	timestamp_++;
	fprintf(stderr, "EVAL: %d num_job_in_queue= %d num_idle_bucket= %d num_busy_bucket= %d\n",
		timestamp_, num_job_in_queue, num_idle_bucket, num_busy_bucket);
}


/***********/
static int process_workflow_state()
{
	struct bucket *bk;
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (state.f_send_exit_msg) return 0;

	if (state.f_done && 0 == run_bk_list_count()) {
		list_for_each_entry(bk, &idle_bk_list, struct bucket, entry) {
			peer = ds_get_peer(ds, bk->dart_id);
			msg = msg_buf_alloc(ds->rpc_s, peer, 1);
			if (!msg)
				goto err_out;

			msg->msg_rpc->cmd = staging_exit;
			msg->msg_rpc->id = ds->rpc_s->ptlmap.id;

			err = rpc_send(ds->rpc_s, peer, msg);
			if (err < 0) {
				free(msg);
				goto err_out;
			}
		}

		state.f_send_exit_msg = 1;
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

/*
*/
static int process_jobq()
{
	struct job *j, *t;
	int err = -ENOMEM;

	// 1. Add ready jobs (if any) in the queue
	struct task_instance *tasks[MAX_NUM_TASKS];
	int num_tasks;
	get_ready_tasks(wf, tasks, &num_tasks);
	if (num_tasks > 0) {
		int i;
		for (i = 0; i < num_tasks; i++) {
			jobq_create_job(tasks[i]);
		}
	}

	// 2. process ready jobs (if any) in the queue
	list_for_each_entry_safe(j, t, &jobq, struct job, entry) {
		if (job_is_pending(j)) {
			job_allocate_bk(j);
		}
	}	

	// 3. process finish jobs (if any) in the queue
	list_for_each_entry_safe(j, t, &jobq, struct job, entry) {
		if (job_is_finish(j)) {
			list_del(&j->entry);

			int i;
			for (i = 0; i < j->num_bk; i++) {
				// Free the bucket
				run_bk_list_remove(j->bk_tab[i]);		
				struct bucket *bk = malloc(sizeof(*bk));
				bk->dart_id = j->bk_tab[i];
				idle_bk_list_add(bk);
			}

			free(j->bk_tab);
			free(j);
		}
	}

	return 0;
}

static int callback_hs_req_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct bucket *bk;
	int dart_id;
	int err = -ENOMEM;

	dart_id = cmd->id;
	bk = run_bk_list_lookup(dart_id);
	if (bk) {
		struct job* j = jobq_lookup(&bk->current_jid);
		job_update_status(j, dart_id);
	} else {
		bk = (struct bucket*) malloc(sizeof(*bk));
		bk->dart_id = dart_id;
		idle_bk_list_add(bk);
		uloga("%s(): should call idle_bk_list_add for #%d once\n",
			__func__, dart_id);
	}

	if (is_master()) {
		print_rr_count();
	}

	return 0;
}

static int callback_insitu_unreg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	state.f_done = 1;
	return 0;
}

static int callback_hs_update_var(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_update_var *hdr;
	hdr = (struct hdr_update_var*)cmd->pad;

	uloga("%s(): update variable '%s' step %d "
		"dims %d bbox {(%d,%d,%d),(%d,%d,%d)}\n", 
			__func__, hdr->var_name, hdr->step, hdr->bb.num_dims,
			hdr->bb.lb.c[0], hdr->bb.lb.c[1], hdr->bb.lb.c[2],
			hdr->bb.ub.c[0], hdr->bb.ub.c[1], hdr->bb.ub.c[2]);
	if (wf != NULL) {
		struct var_descriptor var_desc;
		strcpy(var_desc.var_name, hdr->var_name);
		var_desc.step = hdr->step;
		var_desc.bb = hdr->bb;
		var_desc.size = hdr->size; 
		evaluate_dataflow_by_available_var(wf, &var_desc);
	}

	return 0;
}

int hstaging_scheduler_parallel_init()
{
	rpc_add_service(insitu_unreg, callback_insitu_unreg);
	rpc_add_service(hs_update_var_msg, callback_hs_update_var); 
	rpc_add_service(hs_req_task_msg, callback_hs_req_task);

	dimes_s = dimes_server_alloc(num_sp, num_cp, conf);
	if (!dimes_s) {
		return -1;
	}
	
	dsg = dimes_s->dsg;
	ds = dimes_s->dsg->ds;

	// Init
	jobq_init();
	idle_bk_list_init();
	run_bk_list_init();

	state.f_done = 0;
	state.f_send_exit_msg = 0;

	if (is_master()) {
		wf = read_workflow_conf_file("workflow.conf");
		if (wf == NULL) {
			uloga("%s(): failed to read workflow config file\n", __func__);
		} else {
			print_workflow(wf);
		}
	}

	return 0;
}

int hstaging_scheduler_parallel_run()
{
	int err;

	while (!dimes_server_complete(dimes_s)) {
		err = dimes_server_process(dimes_s);
		if (err < 0) {
			/* If there is an error on the execution path,
			   I should stop the server. */
			dimes_server_free(dimes_s);

			/* TODO:  implement an  exit method  to signal
			   other servers to stop. */
			printf("Server exits due to error %d.\n", err);

			return err;
		}

		process_jobq();
		if (is_master()) {
			process_workflow_state();
		}
	}

	idle_bk_list_free();
	run_bk_list_free();
	jobq_free();

	return 0;
}

int hstaging_scheduler_parallel_finish(void)
{
	if (is_master() && wf) {
		free(wf);
	}

	dimes_server_barrier(dimes_s);
	dimes_server_free(dimes_s);

	return 0;
}


void hstaging_scheduler_parallel_usage(void)
{
	printf("Usage: server OPTIONS\n"
			"OPTIONS: \n"
			"--server, -s    Number of server instance/staging nodes\n"
			"--cnodes, -c    Number of compute nodes\n"
			"--conf, -f      Define configuration file\n");
}

int hstaging_scheduler_parallel_parse_args(int argc, char *argv[])
{
	const char opt_short[] = "s:c:f:";
	const struct option opt_long[] = {
			{"server",      1,      NULL,   's'},
			{"cnodes",      1,      NULL,   'c'},
			{"conf",        1,      NULL,   'f'},
			{NULL,          0,      NULL,   0}
	};

	int opt;

	while ((opt = getopt_long(argc, argv, opt_short, opt_long, NULL)) != -1) {
		switch (opt) {
		case 's':
			num_sp = (optarg) ? atoi(optarg) : -1;
			break;

		case 'c':
			num_cp = (optarg) ? atoi(optarg) : -1;
			break;

		case 'f':
			conf = (optarg) ? optarg : NULL;
			break;

		default:
			printf("Unknown argument \n");
		}
	}


	if (num_sp <= 0)
		num_sp = 1;
	if (num_cp == 0)
		num_cp = 0;
	if (!conf)
		conf = "dataspaces.conf";
	return 0;
}
