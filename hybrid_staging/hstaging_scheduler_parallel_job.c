#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "arpa/inet.h"
#include <getopt.h>
#include <stdio.h>
#include "unistd.h"
#include <math.h>

#include "debug.h"
#include "list.h"
#include "dart.h"
#include "ds_gspace.h"
#include "dimes_server.h"

#include "hstaging_api.h"
#include "hstaging_def.h"
#include "mpi.h"

static int num_sp;
static int num_cp;
static char *conf;

static struct dimes_server *dimes_s = NULL;
static struct dart_server *ds = NULL;
static struct ds_gspace *dsg = NULL;

static struct hstaging_workflow *wf = NULL;

struct job_id {
	int tid;
	int step;
};

struct job {
	struct list_head entry;
	struct task_instance *ti;
	int num_input_vars;
	struct job_id jid;
	int	num_required_bk; // number of buckets required for job execution
    enum hstaging_location_type location_type;
    int bk_group_color;
};

enum bucket_group_status {
    bk_group_none = 0, // group not exist
    bk_group_idle,
    bk_group_busy,
    bk_group_invalidated
};

struct mpi_rank_range {
    int l, r;
};

struct bucket {
    int dart_id;
    int mpi_rank;
};

struct bucket_group {
    enum bucket_group_status status;
    int color;
    int size;
    struct mpi_rank_range rank_range;
    struct job_id current_jid;
};

struct staging_resource {
    struct list_head entry;
    enum hstaging_location_type location_type;
    int num_bucket;
    int group_tab_size;
    struct bucket_group *group_tab;
    int bk_tab_size;
    struct bucket *bk_tab;
};

struct workflow_state {
	unsigned int f_done;
	unsigned int f_send_exit_msg;
};

static struct list_head rs_list;
static struct list_head jobq;
static struct workflow_state state;

static inline int equal_jid(struct job_id *l, struct job_id *r)
{
	return (l->tid == r->tid) && (l->step == r->step);
}

static inline int is_master()
{
	return ( 0 == ds->rpc_s->ptlmap.id );
}

static inline void rs_init()
{
    INIT_LIST_HEAD(&rs_list);
}

static struct staging_resource* rs_lookup(enum hstaging_location_type loctype)
{
    struct staging_resource *rs;
    list_for_each_entry(rs, &rs_list, struct staging_resource, entry) {
        if (rs->location_type == loctype)
            return rs;
    }

    return NULL;    
}

static struct staging_resource* rs_create_new(enum hstaging_location_type loctype, int num_bucket)
{
    struct staging_resource *rs;
    rs = malloc(sizeof(*rs));
    rs->location_type = loctype;
    rs->group_tab_size = 0;
    rs->group_tab = NULL;
    rs->num_bucket = 0;
    rs->bk_tab_size = num_bucket;
    rs->bk_tab = malloc(sizeof(struct bucket) * num_bucket);

    // Add to the list
    list_add_tail(&rs->entry, &rs_list);

    return rs; 
}

static int rs_add_bucket(struct staging_resource *rs, int mpi_rank, int dart_id)
{
    if (mpi_rank < 0 || mpi_rank >= rs->bk_tab_size) {
        uloga("%s(): mpi_rank out of range\n", __func__);
        return -1;
    }

    struct bucket *bk = &(rs->bk_tab[mpi_rank]);
    bk->dart_id = dart_id;
    bk->mpi_rank = mpi_rank;
    rs->num_bucket++;

    return 0;
}

static struct bucket* rs_get_bucket(struct staging_resource *rs, int mpi_rank)
{
    if (mpi_rank < 0 || mpi_rank >= rs->bk_tab_size) {
        uloga("%s(): mpi_rank out of range\n", __func__);
        return NULL;
    }

    return &(rs->bk_tab[mpi_rank]);
}

void recursive_add_bucket_group(struct staging_resource *r, int color, int current_group_size, struct mpi_rank_range *rank_range)
{
    // Add
    r->group_tab[color].status = bk_group_idle;
    r->group_tab[color].color = color;
    r->group_tab[color].size = current_group_size;
    r->group_tab[color].rank_range.l = rank_range->l;
    r->group_tab[color].rank_range.r = rank_range->r;
    
    int left_child_size, right_child_size;
    struct mpi_rank_range left_child_rank_range, right_child_rank_range;

    if (current_group_size <= BK_GROUP_BASIC_SIZE) {
        return; // Done!
    }

    left_child_size = current_group_size / 2;
    if ((left_child_size % BK_GROUP_BASIC_SIZE) != 0) {
        int t = left_child_size / BK_GROUP_BASIC_SIZE;
        left_child_size = (t+1) * BK_GROUP_BASIC_SIZE;
    }
    right_child_size = current_group_size - left_child_size;

    left_child_rank_range.l = rank_range->l;
    left_child_rank_range.r = rank_range->l + left_child_size - 1;
    right_child_rank_range.l = rank_range->l + left_child_size;
    right_child_rank_range.r = rank_range->r; 

    // Add left child
    recursive_add_bucket_group(r, 2*color, left_child_size, &left_child_rank_range);

    // Add right child
    recursive_add_bucket_group(r, 2*color+1, right_child_size, &right_child_rank_range);
}

void rs_build_bk_group(struct staging_resource *r)
{
    // Calculate max number of levels/groups can be built for the given staging
    // resource
    double n1 = r->num_bucket;
    double n2 = BK_GROUP_BASIC_SIZE;
    int max_levels = ceil(log2(n1)+1-log2(n2)); 
    int max_groups = pow(2, max_levels) - 1;

    // set tab size as (max_groups + 1), so we can index group using color value
    r->group_tab_size = max_groups+1;
    if (r->group_tab_size <= 1) {
        uloga("%s(): error max_levels %d max_groups %d\n", __func__,
            max_levels, max_groups);
    }
    r->group_tab = (struct bucket_group*)
            malloc(sizeof(struct bucket_group) * r->group_tab_size);
    if (r->group_tab == NULL) {
        uloga("%s(): error malloc failed\n", __func__);
        return;
    }

    int i;
    for (i = 0; i < r->group_tab_size; i++) {
        r->group_tab[i].status = bk_group_none;
    }

    int color = 1;
    int current_group_size = r->num_bucket;
    struct mpi_rank_range rank_range;
    rank_range.l = 0;
    rank_range.r = r->num_bucket-1;
    recursive_add_bucket_group(r, color, current_group_size, &rank_range); 

    for (i = 0; i < r->group_tab_size; i++) {
        if (r->group_tab[i].status != bk_group_none) {
            uloga("%s(): group: tab index %d, color %d, size %d "
                " rank range (%d, %d)\n",
                __func__, i, r->group_tab[i].color, r->group_tab[i].size,
                r->group_tab[i].rank_range.l, r->group_tab[i].rank_range.r);
        }
    }
}

static void rs_free()
{
    struct staging_resource *rs, *t;
    list_for_each_entry_safe(rs, t, &rs_list, struct staging_resource, entry)
    {
        list_del(&rs->entry);
        if (rs->group_tab) free(rs->group_tab);
        if (rs->bk_tab) free(rs->bk_tab);
        free(rs);
    }
}

static void invalidate_ancestor_groups(struct staging_resource *r, int color)
{
    int parent_color = color / 2;
    if (parent_color >= 1) {
        r->group_tab[parent_color].status = bk_group_invalidated;
    }

    return;
}

static void invalidate_descendant_groups(struct staging_resource *r, int color)
{
    int i;
    int colors[2];
    colors[0] = 2*color;
    colors[1] = 2*color + 1;
    
    for ( i = 0; i < 2; i++) {
        if (colors[i] <= r->group_tab_size &&
            r->group_tab[colors[i]].status != bk_group_none) {
            r->group_tab[colors[i]].status = bk_group_invalidated;
            invalidate_descendant_groups(r, colors[i]);
        }
    }

    return;
}

static struct bucket_group* request_bucket_group(struct staging_resource *r, int num_required_bk)
{
    int i;
    for (i = 0; i < r->group_tab_size; i++) {
        struct bucket_group *g = &(r->group_tab[i]);
        if (g->status == bk_group_idle && g->size == num_required_bk) {
            // Mark the bucket group as busy
            g->status = bk_group_busy;
            // Invalid all its ancestors and descendants
            invalidate_ancestor_groups(r, g->color);
            invalidate_descendant_groups(r, g->color);

            return g;
        }
    } 

    return NULL;    
}


void validate_ancestor_groups(struct staging_resource *r, int color)
{
    int parent_color = color / 2;
    if (parent_color < 1) return;

    // Check if both children of my parent is idle
    int left = parent_color * 2;
    int right = parent_color * 2 + 1;
    if (r->group_tab[left].status == bk_group_idle &&
        r->group_tab[right].status == bk_group_idle) {
        r->group_tab[parent_color].status = bk_group_idle;
        validate_ancestor_groups(r, parent_color);
    }

    return;
}

void validate_descendant_groups(struct staging_resource *r, int color)
{
    int i;
    int colors[2];
    colors[0] = 2*color;
    colors[1] = 2*color + 1;

    for ( i = 0; i < 2; i++) {
        if (colors[i] <= r->group_tab_size &&
            r->group_tab[colors[i]].status != bk_group_none) {
            r->group_tab[colors[i]].status = bk_group_idle;
            validate_descendant_groups(r, colors[i]);
        }
    }

    return; 
}

int free_bucket_group(struct staging_resource *r, int color)
{
    if (color < r->group_tab_size &&
        r->group_tab[color].status == bk_group_busy)
    {
        // Mark the bucket group as idle
        r->group_tab[color].status = bk_group_idle;
        validate_ancestor_groups(r, color);
        validate_descendant_groups(r, color);
    }
}

/* operations on the jobq */
static inline void jobq_init()
{
	INIT_LIST_HEAD(&jobq);
}

static int idle_bk_count()
{
	int cnt = 0;

	return cnt;
}

static int busy_bk_count()
{
	int cnt = 0;

	return cnt;
}

static struct bucket* busy_bk_lookup(int dart_id)
{
	struct bucket *bk;
	return NULL;	
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
		if ( equal_jid(jid, &j->jid) )
			return j;
	}

	return NULL;
}

static struct job * jobq_create_job(struct task_instance *ti)
{
	struct job *j = malloc(sizeof(*j));
	j->jid.tid = ti->tid;
	j->jid.step = ti->step;
	j->ti = ti;
    unsigned int num_basic_bk_group = j->ti->size_hint / BK_GROUP_BASIC_SIZE;
    if (num_basic_bk_group == 0) num_basic_bk_group = 1;
    j->num_required_bk = num_basic_bk_group * BK_GROUP_BASIC_SIZE; 
    j->num_input_vars = 0;
	struct var_instance *vi;
	list_for_each_entry(vi, &ti->input_vars_list, struct var_instance, entry) {
		j->num_input_vars++;
	}

    update_task_instance_status(ti, PENDING);
	list_add_tail(&j->entry, &jobq);	
	
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

static void job_done(struct job *j)
{
	if (j->ti->status == RUN) {
        update_task_instance_status(j->ti, FINISH);
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

static int job_notify_bk(struct job *j, struct bucket *bk, int rank_hint, int nproc_hint)
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
    hdr->color = j->bk_group_color;
	hdr->rank_hint = rank_hint; 
	hdr->nproc_hint = nproc_hint;
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

static int job_allocate_bk(struct job *j)
{
	if (j->ti->status != PENDING) {
		return 0;
	}

    // Try to determine placement location
    enum hstaging_location_type preferred_loc;
    switch (j->ti->placement_hint) {
    case hint_insitu:
        preferred_loc = loc_insitu;
        break;
    case hint_intransit:
        preferred_loc = loc_intransit;
        break;
    case hint_none:
        preferred_loc = loc_insitu;
        break;
    default:
        preferred_loc = loc_insitu;
        uloga("%s(): unknown placement hint information\n", __func__);
        break;
    }

    // Lookup staging resource based on placement location
    struct staging_resource *rs = rs_lookup(preferred_loc);
    if (rs == NULL) {
        return -1;
    }

    // Request for bucket group
    struct bucket_group *bk_group;
    bk_group = request_bucket_group(rs, j->num_required_bk);
    if (bk_group == NULL) {
        // No matched bucket group resource
        uloga("%s(): failed to request bucket for job tid= %d step= %d "
            "num_required_bk=%d\n", __func__, j->jid.tid, j->jid.step,
            j->num_required_bk);
        return -1;
    }

    j->location_type = rs->location_type;
    j->bk_group_color = bk_group->color;
    bk_group->current_jid = j->jid;

    int i;
	for (i = bk_group->rank_range.l; i <= bk_group->rank_range.r; i++) {
        struct bucket *bk = rs_get_bucket(rs, i);
        int rank_hint = i - bk_group->rank_range.l;
        int nproc_hint = bk_group->size;
		if (job_notify_bk(j, bk, rank_hint, nproc_hint) < 0) {
			return -1;
		}
	}

	// Update job state
    update_task_instance_status(j->ti, RUN);

    uloga("%s(): assign job (%d,%d) to bk_group: size %d color %d\n",
        __func__, j->jid.tid, j->jid.step, bk_group->size, bk_group->color);

	return 0;	
}

static int jobq_free()
{
	struct job *j, *t;
	list_for_each_entry_safe(j,t,&jobq,struct job,entry) {
		list_del(&j->entry);
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
	num_idle_bucket = idle_bk_count();
	num_busy_bucket = busy_bk_count();

	timestamp_++;
	fprintf(stderr, "EVAL: %d num_job_in_queue= %d num_idle_bucket= %d num_busy_bucket= %d\n",
		timestamp_, num_job_in_queue, num_idle_bucket, num_busy_bucket);
}


/***********/
static int process_workflow_state()
{
	int err = -ENOMEM;

	if (state.f_send_exit_msg) return 0;

	if (state.f_done && 0 == busy_bk_count()) {
        struct msg_buf *msg;
        struct node_id *peer;
        struct staging_resource *rs;
        struct bucket *bk;
        list_for_each_entry(rs, &rs_list, struct staging_resource, entry)
        {
            int mpi_rank;
            for (mpi_rank = 0; mpi_rank < rs->bk_tab_size; mpi_rank++) {
                bk = rs_get_bucket(rs, mpi_rank);
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
			free(j);
		}
	}

    // 4. clear finished task instances
    clear_finished_tasks(wf);

	return 0;
}

static int callback_hs_task_done(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_task_done *hdr = (struct hdr_task_done*)cmd->pad;
    struct staging_resource *rs = rs_lookup(hdr->location_type);
    if (rs == NULL) {
        uloga("%s(): should not happen rs == NULL\n", __func__);
        return 0;
    }

    uloga("%s(): get job done msg for task tid= %d step= %d color= %d "
        "location_type= %d\n",
        __func__, hdr->tid, hdr->step, hdr->color, hdr->location_type);

    int color = hdr->color;
    struct bucket_group *bk_group = NULL;
    if (color < rs->group_tab_size) {
        bk_group = &(rs->group_tab[color]);
    }

    if (bk_group && bk_group->status == bk_group_busy) {
        // Mark job as done
        struct job *j = jobq_lookup(&(bk_group->current_jid));
        if (j) {
            job_done(j);
            list_del(&j->entry);
            free(j);
        }

        // Free bk group resource
        free_bucket_group(rs, color);
    }

    return 0;
}

static int callback_hs_reg_resource(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_register_resource *hdr = (struct hdr_register_resource*)cmd->pad;
    enum hstaging_location_type loctype = hdr->location_type;
    int num_bucket = hdr->num_bucket;
    int mpi_rank = hdr->mpi_rank;
    int dart_id = cmd->id;

    uloga("%s(): get msg from peer #%d mpi_rank %d num_bucket %d loctype %d\n",
            __func__, dart_id, mpi_rank, num_bucket, loctype);

    struct staging_resource *rs = rs_lookup(loctype);
    if (rs == NULL) {
        rs = rs_create_new(loctype, num_bucket);
        if (rs == NULL) {
            uloga("%s(): error rs == NULL\n", __func__);
            return -1;
        }
    }

    rs_add_bucket(rs, mpi_rank, dart_id);
    // Check if all peers of the staging resource have registered
    if (rs->num_bucket == rs->bk_tab_size) {
        rs_build_bk_group(rs);
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
    rpc_add_service(hs_reg_resource_msg, callback_hs_reg_resource);
    rpc_add_service(hs_task_done_msg, callback_hs_task_done);

	dimes_s = dimes_server_alloc(num_sp, num_cp, conf);
	if (!dimes_s) {
		return -1;
	}
	
	dsg = dimes_s->dsg;
	ds = dimes_s->dsg->ds;

	// Init
	jobq_init();
    rs_init();

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

		if (is_master()) {
            process_jobq();
			process_workflow_state();
		}
	}

    rs_free();
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
