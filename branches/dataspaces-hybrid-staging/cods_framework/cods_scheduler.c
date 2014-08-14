#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "arpa/inet.h"
#include <getopt.h>
#include <stdio.h>
#include "unistd.h"
#include <math.h>

#include "cods_scheduler.h"
#include "cods_def.h"
#include "mpi.h"

static struct cods_scheduler *sched = NULL; 
#define DART_DSG sched->dimes_s->dsg
#define DART_DS sched->dimes_s->dsg->ds
#define DART_RPC_S sched->dimes_s->dsg->ds->rpc_s
#define DART_ID sched->dimes_s->dsg->ds->rpc_s->ptlmap.id

static int num_sp;
static int num_cp;
static char *conf;
static struct timer tm; 
static double tm_st;

/* Forward declaration */
static int evaluate_task_by_available_var(struct task_entry *task, const struct cods_var *var_desc);
static int get_ready_tasks(struct list_head *list, struct task_entry **tasks, int *n /*num_ready_tasks*/);
static int is_task_ready(struct task_entry *task);
static int is_task_finish(struct task_entry *task);
static void update_task_status(struct task_entry *task, enum cods_task_status status);
static int parse_task_conf_file(struct task_entry *task, const char *fname);    
static void print_task_info(struct task_entry *task);

/**
 Tasks
**/

static struct task_entry* task_list_lookup_task(struct list_head *list, uint32_t tid)
{
    struct task_entry *task;
    list_for_each_entry(task, list, struct task_entry, entry) {
        if (task->tid == tid)
            return task;
    } 

    return NULL;
}

static void task_list_add_task(struct list_head *list, struct task_entry *task)
{
    if (!task) return;
    list_add_tail(&task->entry, list);
}

static struct task_entry* create_new_task(uint32_t tid, const char* conf_file, 
    int submitter_dart_id)
{
    struct task_entry *task = (struct task_entry*)malloc(sizeof(*task));
    if (!task) {
        goto err_out;
    }
    task->tid = tid;
    task->status = task_not_ready;   
    task->appid = 0;
    task->placement_hint = hint_none;
    task->size_hint = 0;
    task->num_vars = 0;
    task->submitter_dart_id = submitter_dart_id;
    if (parse_task_conf_file(task, conf_file) < 0) {
        goto err_out_free;
    }

    return task;
 err_out_free:
    free(task);
 err_out:
    uloga("ERROR %s: failed to create new task tid= %d conf_file %s\n",
        __func__, tid, conf_file);
    return NULL;
}

static inline void task_list_init(struct list_head *list)
{
    INIT_LIST_HEAD(list);
}

static void task_list_evaluate_dataflow(struct list_head *list, const struct cods_var *var_desc)
{  
    // Update variable and task status
    struct task_entry *task = NULL;
    list_for_each_entry(task, list, struct task_entry, entry) {
        evaluate_task_by_available_var(task, var_desc);
    }
}

static void task_list_free(struct list_head *list)
{
    struct task_entry *task, *temp;
    list_for_each_entry_safe(task, temp, list, struct task_entry, entry) {
        list_del(&task->entry);
        free(task);
    }
}

/**
    Workflow executors
**/
static inline void bk_pool_list_init(struct list_head* list)
{
    INIT_LIST_HEAD(list);
}

static struct bucket_pool* bk_pool_list_lookup(struct list_head* list, int pool_id)
{
    struct bucket_pool *bp;
    list_for_each_entry(bp, list, struct bucket_pool, entry) {
        if (bp->pool_id == pool_id)
            return bp;
    }

    return NULL;    
}

static struct bucket_pool* bk_pool_list_create_new(struct list_head* list, int pool_id, int num_bucket)
{
    struct bucket_pool *bp;
    bp = malloc(sizeof(*bp));
    bp->pool_id = pool_id;
    bp->num_bucket = 0;
    bp->bk_tab_size = num_bucket;
    bp->bk_tab = malloc(sizeof(struct bucket) * num_bucket);
    bp->f_bk_reg_done = 0;

    list_add_tail(&bp->entry, list);
    return bp; 
}

static int bk_pool_add_bucket(struct bucket_pool *bp, int origin_mpi_rank, int dart_id,
                    int pool_id, struct node_topology_info *topo_info)
{
    if (origin_mpi_rank < 0 || origin_mpi_rank >= bp->bk_tab_size) {
        uloga("ERROR %s(): origin_mpi_rank out of range\n", __func__);
        return -1;
    }

    struct bucket *bk = &(bp->bk_tab[origin_mpi_rank]);
    bk->dart_id = dart_id;
    bk->pool_id = pool_id;
    bk->origin_mpi_rank = origin_mpi_rank;
    bk->status = bk_none;
    bk->partition_type = DEFAULT_PART_TYPE;
    if (topo_info) bk->topo_info = *topo_info; 

    bp->num_bucket++;
    return 0;
}

static void bk_pool_free_bk_allocation(struct bucket_pool *bp, int *bk_idx_tab,
                                int allocation_size)
{
    if (allocation_size > bp->bk_tab_size) {
        uloga("ERROR %s(): allocation_size is larger than bk_tab_size\n", __func__);
        return;
    }

    int i;
    for (i = 0; i < allocation_size; i++) {
        bp->bk_tab[bk_idx_tab[i]].status = bk_idle;
    }
}


static int* bk_pool_request_bk_allocation(struct bucket_pool *bp, int allocation_size)
{
    if (allocation_size > bp->bk_tab_size) {
        uloga("ERROR %s(): allocation_size is larger than bk_tab_size\n", __func__);
        return NULL;
    }

    int *bk_idx_tab = (int*)malloc(allocation_size*sizeof(int));
    if (!bk_idx_tab) return NULL;

    int i, j;
    for (i = 0, j = 0; i < bp->bk_tab_size; i++) {
        if (bp->bk_tab[i].status == bk_idle) {
            bk_idx_tab[j++] = i;
        }
        if (j == allocation_size) break;
    }

    if (j < allocation_size) {
        // no sufficient idle buckets
        free(bk_idx_tab);
        return NULL;  
    }

    for (i = 0; i < allocation_size; i++) {
        // update status of the bucket
        bp->bk_tab[bk_idx_tab[i]].status = bk_busy;
    }
    return bk_idx_tab;
}

static struct bucket* bk_pool_get_bucket(struct bucket_pool *bp, int origin_mpi_rank)
{
    if (origin_mpi_rank < 0 || origin_mpi_rank >= bp->bk_tab_size) {
        uloga("ERROR %s(): origin_mpi_rank out of range\n", __func__);
        return NULL;
    }

    return &(bp->bk_tab[origin_mpi_rank]);
}

static void bk_pool_set_bucket_idle(struct bucket_pool *bp)
{
    int i;
    for (i = 0; i < bp->bk_tab_size; i++) {
        bp->bk_tab[i].status = bk_idle;
    }

    bp->f_bk_reg_done = 1;
    uloga("%s(): bucket resource pool %d is ready, num_bucket %d\n",
        __func__, bp->pool_id, bp->bk_tab_size);
}

int compare_bucket_nid(const void *p1, const void *p2)
{
    return ((const struct bucket*)p1)->topo_info.nid -
            ((const struct bucket*)p2)->topo_info.nid;
}

static void bk_pool_sort_bucket(struct bucket_pool *bp)
{
    // sort bk_tab by nid
    qsort(bp->bk_tab, bp->bk_tab_size, sizeof(struct bucket),
        compare_bucket_nid);
}

static void print_bk_pool(struct bucket_pool *bp)
{
    int i;
    for (i = 0; i < bp->bk_tab_size; i++) {
        uloga("bucket pool_id= %d dart_id= %d nid= %u partition_type= %u\n",
            bp->bk_tab[i].pool_id, bp->bk_tab[i].dart_id,
            bp->bk_tab[i].topo_info.nid, bp->bk_tab[i].partition_type);
    }
}

static void bk_pool_list_free(struct list_head* list)
{
    struct bucket_pool *bp, *t;
    list_for_each_entry_safe(bp, t, list, struct bucket_pool, entry)
    {
        list_del(&bp->entry);
        if (bp->bk_tab) free(bp->bk_tab);
        free(bp);
    }
}

/**
    Pending messages
**/
static void pending_msg_list_init(struct list_head *list)
{
    INIT_LIST_HEAD(list);
}

static void pending_msg_list_free(struct list_head *list)
{
    int msg_cnt = 0;
    struct pending_msg *p, *t;
    list_for_each_entry_safe(p, t, list, struct pending_msg, entry)
    {
        list_del(&p->entry);
        free(p);
        msg_cnt++;
    }
    uloga("%s(): msg_cnt= %d\n", __func__, msg_cnt);
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

/**
    Runnable tasks management    
**/
static inline void rtask_list_init(struct list_head* list)
{
	INIT_LIST_HEAD(list);
}

static int rtask_list_count(struct list_head* list)
{
	int cnt = 0;
	struct runnable_task *rtask;
	list_for_each_entry(rtask, list, struct runnable_task, entry) {
		cnt++;
	}

	return cnt;
}

static struct runnable_task *rtask_list_lookup(struct list_head* list, uint32_t tid)
{
    struct runnable_task *rtask;
	list_for_each_entry(rtask,list,struct runnable_task, entry) {
        if (rtask->task_ref->tid == tid)
			return rtask;
	}

	return NULL;
}

static struct runnable_task *rtask_list_add_new(struct list_head* list, struct task_entry *t)
{
    struct runnable_task *rtask = malloc(sizeof(*rtask));
    rtask->task_ref = t;
    rtask->bk_allocation_size = t->size_hint;
    rtask->bk_idx_tab = NULL;

    update_task_status(t, task_pending);
    list_add_tail(&rtask->entry, list);
    return rtask;
}

static inline int rtask_is_pending(struct runnable_task *rtask) {
    return rtask->task_ref->status == task_pending;
}

static inline int rtask_is_running(struct runnable_task *rtask) {
    return rtask->task_ref->status == task_running;
}

static inline int rtask_is_finish(struct runnable_task *rtask) {
    return rtask->task_ref->status == task_finish;
}

static inline void rtask_set_finish(struct runnable_task *rtask) {
    if (rtask->task_ref->status == task_running) {
        update_task_status(rtask->task_ref, task_finish);
    }
}

static void free_rtask(struct runnable_task *rtask) {
    list_del(&rtask->entry);
    if (rtask->bk_idx_tab) free(rtask->bk_idx_tab);
    free(rtask);
}

/**

**/
static int notify_bk_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	if (msg->size > 0) {
		free(msg->msg_data);
	}
	free(msg);	

	return 0;
}

static int notify_bk(struct runnable_task *rtask, struct bucket_pool *bp, 
    struct bucket *bk, int rank_hint, int nproc_hint)
{
	int err = -ENOMEM;
	struct msg_buf *msg;
	struct node_id *peer;
	peer = ds_get_peer(DART_DS, bk->dart_id);
	msg = msg_buf_alloc(DART_RPC_S, peer, 1);
	if (!msg)
		goto err_out;
    
    struct task_entry *t = rtask->task_ref;
    size_t mpi_rank_tab_size = rtask->bk_allocation_size*sizeof(int);
    size_t var_tab_size = t->num_vars*sizeof(struct cods_var);
    msg->size = mpi_rank_tab_size + var_tab_size;
    msg->msg_data = malloc(msg->size); 
    msg->cb = notify_bk_completion;

    int i;
    // copy mpi ranks of the bk allocation
    int *mpi_rank_tab = (int*)msg->msg_data;
    for (i = 0; i < rtask->bk_allocation_size; i++) {
        mpi_rank_tab[i] = bp->bk_tab[rtask->bk_idx_tab[i]].origin_mpi_rank; 
    }

    // copy variable information
    struct cods_var *vars = (struct cods_var*)(msg->msg_data+mpi_rank_tab_size);
    for (i = 0; i < t->num_vars; i++) {
        vars[i] = t->vars[i];
    } 

	msg->msg_rpc->cmd = cods_exec_task_msg;
	msg->msg_rpc->id = DART_ID;
	struct hdr_exec_task *hdr =
		(struct hdr_exec_task *)msg->msg_rpc->pad;
	hdr->tid = t->tid;
    hdr->appid = t->appid;
	hdr->rank_hint = rank_hint; 
	hdr->nproc_hint = nproc_hint;
	hdr->num_vars = t->num_vars;

	err = rpc_send(DART_RPC_S, peer, msg);
	if (err < 0) {
		free(msg->msg_data);
		free(msg);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static int bk_pool_list_allocate_bk(struct list_head *list, struct runnable_task *rtask)
{
	if (rtask->task_ref->status != task_pending) {
		return 0;
	}

    // Try to determine placement location
/*
    enum cods_location_type preferred_loc;
    switch (j->ti->placement_hint) {
    case hint_insitu:
        preferred_loc = loc_insitu;
        break;
    case hint_intransit:
        preferred_loc = loc_intransit;
        break;
    case hint_none:
        preferred_loc = loc_intransit;
        break;
    default:
        preferred_loc = loc_intransit;
        uloga("%s(): unknown placement hint information\n", __func__);
        break;
    }
*/

    // try to allocate buckets in a simple first-fit manner
    struct bucket_pool *bp;
    list_for_each_entry(bp, list, struct bucket_pool, entry) {
        rtask->bk_idx_tab = bk_pool_request_bk_allocation(bp, rtask->bk_allocation_size);
        if (rtask->bk_idx_tab) break;
    }    
    
    if (!rtask->bk_idx_tab) {
        return 0;
    }

    // notify the allocated buckets
    int i;
    for (i = 0; i < rtask->bk_allocation_size; i++) {
        struct bucket *bk = bk_pool_get_bucket(bp, rtask->bk_idx_tab[i]);
        int rank_hint = i;
        int nproc_hint = rtask->bk_allocation_size;
		if (notify_bk(rtask, bp, bk, rank_hint, nproc_hint) < 0) {
			return -1;
		}
	}

	// uppdate task state
    update_task_status(rtask->task_ref, task_running);

    uloga("%s(): assign task tid= %u to buckets timestamp %lf\n",
        __func__, rtask->task_ref->tid, timer_read(&tm)-tm_st);
	return 0;	
}

static void rtask_list_free(struct list_head* list)
{
	struct runnable_task *rtask, *temp;
	list_for_each_entry_safe(rtask, temp, list, struct runnable_task, entry) {
		list_del(&rtask->entry);
		free(rtask);
	}
}

/************/
static int timestamp_ = 0;
/*
static void print_rr_count()
{
	int num_runnable_task = 0;
	int num_idle_bucket = 0;
	int num_busy_bucket = 0;

	num_runnable_task = rtask_list_count();
	num_idle_bucket = idle_bk_count();
	num_busy_bucket = busy_bk_count();

	timestamp_++;
	fprintf(stderr, "EVAL: %d num_runnable_task= %d num_idle_bucket= %d num_busy_bucket= %d\n",
		timestamp_, num_runnable_task, num_idle_bucket, num_busy_bucket);
}
*/

static int notify_task_submitter(struct task_entry *task)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;
    int dart_id = task->submitter_dart_id;

    peer = ds_get_peer(DART_DS, dart_id);
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_submitted_task_done_msg;
    msg->msg_rpc->id = DART_ID;
    struct hdr_submitted_task_done *hdr= (struct hdr_submitted_task_done*)msg->msg_rpc->pad;
    hdr->tid = task->tid;
    hdr->task_execution_time = 0;

    err = rpc_send(DART_RPC_S, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int process_finished_task(struct cods_scheduler *scheduler)
{
    struct runnable_task *rtask, *temp;
    list_for_each_entry_safe(rtask, temp, &scheduler->rtask_list, struct runnable_task, entry) {
        if (rtask_is_finish(rtask)) {
            uloga("WARNING %s: shuold not remove runnable task here...\n", __func__);
            // remove runnable task from the list
            list_del(&rtask->entry); 
            free(rtask);
        }
    }

    struct task_entry *task, *t;
    list_for_each_entry_safe(task, t, &scheduler->task_list, struct task_entry, entry) {
        if (is_task_finish(task)) {
            notify_task_submitter(task);
            // remove from the task list 
            list_del(&task->entry);
            free(task);
        }
    }

    return 0;
}

static void reset_framework_state(struct cods_framework_state *state)
{
    state->f_done = 0;
    state->f_notify_executor_to_exit = 0;
}

static int process_framework_state(struct cods_scheduler *scheduler)
{
    int err = -ENOMEM;
    if (scheduler->framework_state.f_notify_executor_to_exit) return 0;
    if (scheduler->framework_state.f_done && 0 == busy_bk_count()) {
        struct msg_buf *msg;
        struct node_id *peer;
        struct bucket_pool *bp;
        struct bucket *bk;
        list_for_each_entry(bp, &scheduler->bk_pool_list, struct bucket_pool, entry)
        {
            int mpi_rank;
            for (mpi_rank = 0; mpi_rank < bp->bk_tab_size; mpi_rank++) {
                bk = bk_pool_get_bucket(bp, mpi_rank);
                peer = ds_get_peer(DART_DS, bk->dart_id);
                msg = msg_buf_alloc(DART_RPC_S, peer, 1);
                if (!msg)
                    goto err_out;

                msg->msg_rpc->cmd = cods_stop_executor_msg;
                msg->msg_rpc->id = DART_ID;

                err = rpc_send(DART_RPC_S, peer, msg);
                if (err < 0) {
                    free(msg);
                    goto err_out;
                }
            }
        }

        scheduler->framework_state.f_notify_executor_to_exit = 1;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int send_executor_pool_info_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    if (msg->size > 0) {
        free(msg->msg_data);
    }
    free(msg);
    return 0;
}

static int process_cods_get_executor_pool_info(struct pending_msg *p)
{
    int err;
    struct hdr_get_executor_pool_info *hdr = (struct hdr_get_executor_pool_info*)p->cmd.pad;
    int pool_id = hdr->pool_id;
    struct bucket_pool *bp = bk_pool_list_lookup(&sched->bk_pool_list, pool_id);
    if (!bp) return -1;
    if (!bp->f_bk_reg_done) return -1;
    
    // reply to task submitter
    struct msg_buf *msg;
    struct node_id *peer;
    peer = ds_get_peer(DART_DS, p->cmd.id);
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg) goto err_out;

    msg->size = sizeof(struct executor_descriptor)*bp->num_bucket;
    msg->msg_data = malloc(msg->size);
    msg->cb = send_executor_pool_info_completion;
    struct executor_descriptor *tab = msg->msg_data;
    int i;
    for (i = 0; i < bp->num_bucket; i++) {
        tab[i].dart_id = bp->bk_tab[i].dart_id; 
        tab[i].bk_idx = i;
        tab[i].topo_info = bp->bk_tab[i].topo_info;
        tab[i].partition_type = bp->bk_tab[i].partition_type;
    } 

    msg->msg_rpc->cmd = cods_executor_pool_info_msg;
    msg->msg_rpc->id = DART_ID;
    struct hdr_executor_pool_info *reply_hdr = msg->msg_rpc->pad;
    reply_hdr->pool_id = bp->pool_id;
    reply_hdr->num_executor = bp->num_bucket;
    
    err = rpc_send(DART_RPC_S, peer, msg);
    if (err < 0) {
        free(msg->msg_data);
        free(msg);
        goto err_out;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int process_pending_msg(struct cods_scheduler *scheduler)
{
    // process one msg at a time
    struct pending_msg *p, *t;
    int err;
    list_for_each_entry_safe(p, t, &scheduler->pending_msg_list, struct pending_msg, entry)
    {
        switch (p->cmd.cmd) {
        case cods_get_executor_pool_info_msg:
            err = process_cods_get_executor_pool_info(p);
            if (err == 0) {
                list_del(&p->entry);
                free(p);
            }
            break;
        default:
            uloga("%s(): unknown message type\n", __func__);
            break;
        }
        break;
    }

    return 0;
}

/*
*/
static int process_runnable_task(struct cods_scheduler *scheduler)
{
	struct runnable_task *rtask, *temp;
	int err = -ENOMEM;

	// 1. Add ready tasks (if any) 
    struct task_entry *tasks[MAX_NUM_TASKS];
    int num_tasks;
    get_ready_tasks(&scheduler->task_list, tasks, &num_tasks);
    if (num_tasks > 0) {
        int i;
        for (i = 0; i < num_tasks; i++) {
            rtask_list_add_new(&scheduler->rtask_list, tasks[i]);
        }
    }

	// 2. process runnable tasks (if any) 
	list_for_each_entry_safe(rtask, temp, &scheduler->rtask_list, struct runnable_task, entry) {
		if (rtask_is_pending(rtask)) {
			bk_pool_list_allocate_bk(&scheduler->bk_pool_list, rtask);
		}
	}	

	return 0;
}

static int callback_cods_finish_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_finish_task *hdr = (struct hdr_finish_task*)cmd->pad;
    struct bucket_pool *bp = bk_pool_list_lookup(&sched->bk_pool_list, hdr->pool_id);
    if (bp == NULL) {
        uloga("ERROR %s(): should not happen bp == NULL\n", __func__);
        return 0;
    }

    struct runnable_task *rtask = rtask_list_lookup(&sched->rtask_list, hdr->tid);
    if (rtask) {
        uloga("%s(): finish task tid= %u timestamp %lf\n",
            __func__, hdr->tid, timer_read(&tm)-tm_st);

        rtask_set_finish(rtask);
        bk_pool_free_bk_allocation(bp, rtask->bk_idx_tab, rtask->bk_allocation_size);
        free_rtask(rtask); 
    } else {
        uloga("ERROR %s: failed to find task tid= %u\n",
            __func__, hdr->tid);
    } 

    return 0;
}

static int callback_cods_reg_resource(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_register_resource *hdr = (struct hdr_register_resource*)cmd->pad;
    int pool_id = hdr->pool_id;
    int num_bucket = hdr->num_bucket;
    int mpi_rank = hdr->mpi_rank;
    int dart_id = cmd->id; // TODO: do we need to store dart id explicitly in hdr?

#ifdef HAVE_UGNI
/*
    uloga("%s(): get msg from peer #%d mpi_rank %d num_bucket %d pool_id %d nid %u "
            "mesh_coord (%u,%u,%u)\n",
            __func__, dart_id, mpi_rank, num_bucket, pool_id,
            hdr->topo_info.nid, hdr->topo_info.mesh_coord.mesh_x,
            hdr->topo_info.mesh_coord.mesh_y, hdr->topo_info.mesh_coord.mesh_z);
*/
#endif

    struct bucket_pool *bp = bk_pool_list_lookup(&sched->bk_pool_list, pool_id);
    if (bp == NULL) {
        bp = bk_pool_list_create_new(&sched->bk_pool_list, pool_id, num_bucket);
        if (bp == NULL) {
            uloga("ERROR %s(): bk_pool_list_create_new() failed\n", __func__);
            return -1;
        }
    }

    bk_pool_add_bucket(bp, mpi_rank, dart_id, pool_id, &hdr->topo_info);
    // Check if all peers of the resource pool have registered
    if (bp->num_bucket == bp->bk_tab_size) {
        bk_pool_set_bucket_idle(bp);
        bk_pool_sort_bucket(bp);
    }

    return 0;
}

// TODO: make the workflow evaluation asynchronous...
static int callback_cods_update_var(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_update_var *hdr = (struct hdr_update_var*)cmd->pad;

/*
	uloga("%s(): update variable '%s' version %d "
		"dims %d bbox {(%d,%d,%d),(%d,%d,%d)}\n", 
			__func__, hdr->name, hdr->version, hdr->bb.num_dims,
			hdr->bb.lb.c[0], hdr->bb.lb.c[1], hdr->bb.lb.c[2],
			hdr->bb.ub.c[0], hdr->bb.ub.c[1], hdr->bb.ub.c[2]);
*/

    struct cods_var var_desc;
    strcpy(var_desc.name, hdr->name);
    var_desc.version = hdr->version;
    var_desc.elem_size = hdr->elem_size;
    var_desc.gdim = hdr->gdim;
    task_list_evaluate_dataflow(&sched->task_list, &var_desc);

	return 0;
}

static int callback_cods_submit_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_submit_task *hdr = (struct hdr_submit_task*)cmd->pad;
    uloga("%s(): config file %s\n", __func__, hdr->conf_file);

    struct task_entry *task = task_list_lookup_task(&sched->task_list, hdr->tid);
    if (task) {
        uloga("ERROR %s: task tid= %d already exist\n", __func__, hdr->tid);
        return 0;
    }

    task = create_new_task(hdr->tid, hdr->conf_file, cmd->id);
    if (!task) {
        return 0;
    }

    task_list_add_task(&sched->task_list, task);
    print_task_info(task);
    return 0;
}


static int callback_add_pending_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct pending_msg *p = malloc(sizeof(*p));
    memcpy(&p->cmd, cmd, sizeof(struct rpc_cmd));        
    list_add_tail(&p->entry, &sched->pending_msg_list);
    return 0;
}

static int callback_cods_stop_framework(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    sched->framework_state.f_done = 1;    
    return 0;
}

static int fetch_executor_partition_info_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct rpc_cmd *cmd = (struct rpc_cmd*)msg->private;
    int submitter_dart_id = cmd->id;
    struct hdr_build_partition *hdr = (struct hdr_build_partition*)cmd->pad;

    // copy partition information to bucket pool
    struct bucket_pool *bp = bk_pool_list_lookup(&sched->bk_pool_list, hdr->pool_id);
    if (bp) {
        int i;
        struct executor_descriptor *tab = msg->msg_data;
        for (i = 0; i < hdr->num_executor; i++) {
            bp->bk_tab[tab[i].bk_idx].partition_type = tab[i].partition_type;
        }
        print_bk_pool(bp);
    }

    // reply message
    int err;
    struct msg_buf *reply_msg;
    struct node_id *peer;
    peer = ds_get_peer(DART_DS, submitter_dart_id);
    reply_msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    reply_msg->msg_rpc->cmd = cods_build_partition_done_msg;
    reply_msg->msg_rpc->id = DART_ID;

    err = rpc_send(DART_RPC_S, peer, reply_msg);
    if (err < 0) {
        free(reply_msg);
    }

    // free cmd 
    free(cmd);

    // free received data
    if (msg->msg_data) free(msg->msg_data);
    free(msg);
    return 0;
}

static int callback_cods_build_partition(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct rpc_cmd *cmd_copy = malloc(sizeof(*cmd));
    memcpy(cmd_copy, cmd, sizeof(*cmd));

    int submitter_dart_id = cmd->id;
    struct hdr_build_partition *hdr = (struct hdr_build_partition*)cmd->pad;
    struct node_id *peer = ds_get_peer(DART_DS, submitter_dart_id);
    struct msg_buf *msg;
    int err = -ENOMEM;

    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg) goto err_out;

    msg->size = sizeof(struct executor_descriptor)*hdr->num_executor;
    msg->msg_data = malloc(msg->size);
    msg->cb = fetch_executor_partition_info_completion;
    msg->private = cmd_copy;

    rpc_mem_info_cache(peer, msg, cmd);
    err = rpc_receive_direct(DART_RPC_S, peer, msg);
    rpc_mem_info_reset(peer, msg, cmd);
    if (err == 0)
        return 0;

    free(msg);
err_out:
    ERROR_TRACE();
}

int cods_scheduler_init()
{
    sched = malloc(sizeof(*sched));
    if (!sched) return -1;

	rpc_add_service(cods_update_var_msg, callback_cods_update_var); 
    rpc_add_service(cods_reg_resource_msg, callback_cods_reg_resource);
    rpc_add_service(cods_finish_task_msg, callback_cods_finish_task);
    rpc_add_service(cods_submit_task_msg, callback_cods_submit_task);
    rpc_add_service(cods_get_executor_pool_info_msg, callback_add_pending_msg);
    rpc_add_service(cods_build_partition_msg, callback_cods_build_partition);
    rpc_add_service(cods_stop_framework_msg, callback_cods_stop_framework);

	sched->dimes_s = dimes_server_alloc(num_sp, num_cp, conf);
	if (!sched->dimes_s) {
        free(sched);
        return -1;
	}
	
	// Init
    pending_msg_list_init(&sched->pending_msg_list);
    bk_pool_list_init(&sched->bk_pool_list);
    task_list_init(&sched->task_list);
	rtask_list_init(&sched->rtask_list);
    reset_framework_state(&sched->framework_state);

    timer_init(&tm, 1);
    timer_start(&tm);
    tm_st = timer_read(&tm);

	return 0;
}

int cods_scheduler_run()
{
	int err;

	while (!dimes_server_complete(sched->dimes_s)) {
		err = dimes_server_process(sched->dimes_s);
		if (err < 0) {
			/* If there is an error on the execution path,
			   I should stop the server. */
			dimes_server_free(sched->dimes_s);

			/* TODO:  implement an  exit method  to signal
			   other servers to stop. */
			printf("Server exits due to error %d.\n", err);

			return err;
		}

        process_pending_msg(sched);
        process_runnable_task(sched);
        process_finished_task(sched);
        process_framework_state(sched);
	}

    pending_msg_list_free(&sched->pending_msg_list);
    bk_pool_list_free(&sched->bk_pool_list);
	rtask_list_free(&sched->rtask_list);
    task_list_free(&sched->task_list);

	return 0;
}

int cods_scheduler_finish()
{
    if (!sched) return -1;
	dimes_server_barrier(sched->dimes_s);
	dimes_server_free(sched->dimes_s);
    free(sched); 

	return 0;
}


void cods_scheduler_usage()
{
	printf("Usage: server OPTIONS\n"
			"OPTIONS: \n"
			"--server, -s    Number of server instance/staging nodes\n"
			"--cnodes, -c    Number of compute nodes\n"
			"--conf, -f      Define configuration file\n");
}

int cods_scheduler_parse_args(int argc, char *argv[])
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

static void print_str_decimal(const char *str)
{
    int j = 0;
    while (j < strlen(str)) {
        printf("%d ", str[j++]);
    }   
    printf("\n");
}

// TODO: use more generic approach to convert strings to type id
static int str_to_var_type(const char *str, enum cods_var_type *type)
{
    if (0 == strcmp(str, "depend")) {
        *type = var_type_depend;
        return 0;
    }
 
    if (0 == strcmp(str, "put")) {
        *type = var_type_put;
        return 0;
    }

    if (0 == strcmp(str, "get")) {
        *type = var_type_get;
        return 0;
    }

    fprintf(stderr, "Unknown variable type string '%s'\n", str);    
    return -1;
}

static int str_to_placement_hint(const char *str, enum cods_placement_hint *hint)
{
    if (0 == strcmp(str, "insitu")) {
        *hint = hint_insitu;
        return 0;
    }

    if (0 == strcmp(str, "intransit")) {
        *hint = hint_intransit;
        return 0;
    }

    if (0 == strcmp(str, "none")) {
        *hint = hint_none;
        return 0;
    }

    fprintf(stderr, "Unknown placement hint string '%s'\n", str);
    return -1;
}

static void update_task_status(struct task_entry *task, enum cods_task_status status)
{
    task->status = status;
}

static int is_task_finish(struct task_entry *task)
{
    return task->status == task_finish;
}

static int is_task_ready(struct task_entry *task)
{
    if (task->status == task_ready) return 1;
    else if (task->status == task_not_ready) {
        int i;
        for (i = 0; i < task->num_vars; i++) {
            if (task->vars[i].type == var_type_depend &&
                task->vars[i].status == var_not_available) {
                return 0;
            }
        }

        update_task_status(task, task_ready);
        return 1;
    }

    return 0;
}

static int get_ready_tasks(struct list_head *list,
    struct task_entry **tasks, int *n /*num_ready_tasks*/)
{
    *n = 0;
    struct task_entry *task = NULL;
    list_for_each_entry(task, list, struct task_entry, entry) {
        if (is_task_ready(task)) {
            tasks[*n] = task;
            *n = *n + 1;
        }
    }

    return 0;
}

static struct cods_var *lookup_var(struct task_entry *task, const char *var_name)
{
    int i;
    for (i = 0; i < task->num_vars; i++) {
        if (0 == strcmp(var_name, task->vars[i].name))
            return &task->vars[i];
    }

    return NULL;
}

static int evaluate_task_by_available_var(struct task_entry *task, const struct cods_var *var_desc)
{
    if (!task) return 0;
    struct cods_var *var = lookup_var(task, var_desc->name);
    if (var) {
        var->status = var_available;
        var->elem_size = var_desc->elem_size;
        var->gdim = var_desc->gdim;
    } 

    return 0;
}

static struct cods_var* task_add_var(struct task_entry *task, const char *name)
{
    if (task == NULL) {
        fprintf(stderr, "%s(): task == NULL\n", __func__);
        return NULL;
    }

    if (task->num_vars >= MAX_NUM_VARS) {
        fprintf(stderr, "%s(): exceeds MAX_NUM_VARS\n", __func__);
        return NULL;
    }

    struct cods_var *var = &task->vars[task->num_vars];
    strcpy(var->name, name);
    var->version = -1;
    var->elem_size = 0;
    memset(&var->gdim, 0, sizeof(struct global_dimension));
    memset(&var->dist_hint, 0, sizeof(struct block_distribution));
    var->gdim.ndim = 0;
    var->dist_hint.ndim = 0;   
 
    task->num_vars++;
    return var;
}

static int read_task_var_type(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_var_type = 3;
    // Get var type
    enum cods_var_type type;
    if (str_to_var_type(fields[index_to_var_type], &type) < 0) {
        return -1;
    }

    // Get the variables
    int vars_start_at = 4;
    int i = vars_start_at, j = 0;
    while (i < num_fields) {
        struct cods_var *var = lookup_var(task, fields[i]);
        if (!var) {
            // add new var
            var = task_add_var(task, fields[i]);
            if (!var) return -1;
        }
        // set var type
        var->type = type;
        i++;
    }

    return 0;
}

static int read_task_var_dimension(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_var_dim = 3;
    int ndim = atoi(fields[index_to_var_dim]);
    if (ndim < 0 || ndim > BBOX_MAX_NDIM) {
        uloga("ERROR %s: wrong value for ndim %s\n", __func__, fields[index_to_var_dim]);
        return -1;
    }

    struct global_dimension gdim;
    gdim.ndim = ndim;
    int dims_start_at = 4;
    int i = dims_start_at, j = 0;
    while ((i < num_fields) && (j < gdim.ndim)) {
        gdim.sizes.c[j++] = atoll(fields[i++]);
    }
    
    if (j != gdim.ndim) {
        uloga("ERROR %s: var dimension incomplete ndim is %d but only read %d values\n",
            __func__, gdim.ndim, j);    
        return -1;
    }

    size_t elem_size = atoi(fields[i++]);
    while (i < num_fields) {
        struct cods_var *var = lookup_var(task, fields[i]);
        if (!var) {
            // add new var
            var = task_add_var(task, fields[i]);
            if (!var) return -1;
        }
        var->elem_size = elem_size;
        memcpy(&var->gdim, &gdim, sizeof(struct global_dimension));
        i++;
    }

    return 0;
}

static int read_task_var_distribution(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_var_dist_type = 3;
    if (0 != strcmp("block", fields[index_to_var_dist_type])) {
        uloga("ERROR %s: we only support 'block' distribution\n", __func__);
        return -1;
    }

    int index_to_var_dist_dim = 4;
    int ndim = atoi(fields[index_to_var_dist_dim]);
    if (ndim < 0 || ndim > BBOX_MAX_NDIM) {
        uloga("ERROR %s: wrong value for ndim %s\n", __func__, fields[index_to_var_dist_dim]);
        return -1;
    }

    struct block_distribution dist;
    dist.ndim = ndim;
    int dims_start_at = 5;
    int i = dims_start_at, j = 0; 
    while ((i < num_fields) && (j < dist.ndim)) {
        dist.sizes.c[j++] = atoll(fields[i++]);
    }

    if (j != dist.ndim) {
        uloga("ERROR %s: var distribution incomplete ndim is %d but only read %d values\n",
            __func__, dist.ndim, j);
        return -1;
    }

    while (i < num_fields) {
        struct cods_var *var = lookup_var(task, fields[i]);
        if (!var) {
            // add new var
            var = task_add_var(task, fields[i]);
            if (!var) return -1;
        }
        memcpy(&var->dist_hint, &dist, sizeof(struct block_distribution));
        i++;
    }

    return 0;
}

static int read_task_placement_hint(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_hint = 3;
    // Get placement hint
    if (str_to_placement_hint(fields[index_to_hint], &task->placement_hint) < 0 ) {
        return -1;
    }

    return 0;
}

static int read_task_size_hint(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_hint = 3;
    int size_hint = atoi(fields[index_to_hint]);
    if (size_hint < 0) {
        size_hint = 0;
    }

    task->size_hint = size_hint;
    return 0;
}

static int read_workflow_task(struct task_entry *task, char *fields[], int num_fields)
{
    int index_to_appid = 1;
    int index_to_desc = 2;
    int required_fields = 4;

    if (num_fields < required_fields) {
        fprintf(stderr, "Can NOT be valid task information\n");
        return -1;
    }

    // read application id for the task
    int appid = atoi(fields[index_to_appid]);
    task->appid = appid;

    char *t_desc = fields[index_to_desc];
    if (0 == strcmp(t_desc, "def_var_type")) {
        if (read_task_var_type(task, fields, num_fields) < 0) {
            return -1;
        }
    } else if (0 == strcmp(t_desc, "placement_hint")) {
        if (read_task_placement_hint(task, fields, num_fields) < 0) {
            return -1;  
        }
    } else if (0 == strcmp(t_desc, "def_size_hint")) {
        if (read_task_size_hint(task, fields, num_fields) < 0 ) {
            return -1;
        } 
    } else if (0 == strcmp(t_desc, "def_var_dimension")) {
        if (read_task_var_dimension(task, fields, num_fields) < 0) {
            return -1;
        }
    } else if (0 == strcmp(t_desc, "def_var_distribution")) {
        if (read_task_var_distribution(task, fields, num_fields) < 0) {
            return -1;
        }
    }

    return 0;
}

static int parse_task_conf_file(struct task_entry *task, const char *fname)
{
    const size_t MAX_LINE = 4096;
    const char *DELIM = " \t\n\r"; //space, tab, line feed, carriage return
    const int MAX_FIELDS = 50;

    int err = -1;
    if (!task) return err;

    FILE *file = fopen(fname, "r");
    if (!file) {
        fprintf(stderr, "%s(): unable to open file %s\n", __func__, fname);
        return err;  
    }

    char line[MAX_LINE];
    int i = 1;
    while (fgets(line, MAX_LINE, file) != NULL) {
        // Trim the line
        trim(line, DELIM);
        i++;

        // Blank line
        if (strlen(line) == 0)
            continue;

        // Comment line
        if (line[0] == '#')
            continue;

        //printf("line: %s\n", line);

        // Split line into fields
        int n = 0;
        char *fields[MAX_FIELDS];
        char *tok;
        tok = strtok(line, DELIM);
        while (tok != NULL) {
            if (n < MAX_FIELDS) {
                fields[n] = (char *) malloc(strlen(tok)+1);
                strcpy(fields[n], tok);         
                tok = strtok(NULL, DELIM);
                n++;
            } else {
                fprintf(stderr, "Exceeds the max number of fields %d\n",
                    MAX_FIELDS);
                break;
            }
        }

        // Read task information
        if (0 == strcmp("task", fields[0])) {
            read_workflow_task(task, fields, n);
        }

        // Free the fields array
        int j = 0;
        while (j < n) {
            free(fields[j]);
            j++;
        }   
    }

    fclose(file);
    return 0;   
}

static void print_task_info(struct task_entry *task)
{
    if (!task) return;
    uint64_t gdim[BBOX_MAX_NDIM], dist[BBOX_MAX_NDIM];
    char gdim_str[256], dist_str[256];
    int i, j;

    printf("task tid= %u appid= %d size_hint= %d submitter_dart_id= %d\n",
        task->tid, task->appid, task->size_hint, task->submitter_dart_id);
    for (i = 0; i < task->num_vars; i++) {
        for (j = 0; j < task->vars[i].gdim.ndim; j++) {
            gdim[j] = task->vars[i].gdim.sizes.c[j];
        }
        for (j = 0; j < task->vars[i].dist_hint.ndim; j++) {
            dist[j] = task->vars[i].dist_hint.sizes.c[j];
        }
        int64s_to_str(task->vars[i].gdim.ndim, gdim, gdim_str);
        int64s_to_str(task->vars[i].dist_hint.ndim, dist, dist_str);

        printf("task tid= %u appid= %d var= '%s': type= '%s' elem_size= %u gdim= (%s) dist_hint= (%s)\n", 
            task->tid, task->appid, task->vars[i].name, var_type_name[task->vars[i].type], task->vars[i].elem_size, gdim_str, dist_str);
    }
}

/*
int read_emulated_vars_sequence(struct workflow_entry *wf, const char *fname)
{
    int err = -1;
    const size_t MAX_LINE = 4096;
    const char *DELIM = " \t\n\r";
    const int MAX_FIELDS = 50;

    FILE *file = fopen(fname, "r");
    if (!file) {
        fprintf(stderr, "%s(): unable to open file %s\n", __func__, fname);
        free(wf);
        return -1;  
    }

    char line[MAX_LINE];
    int i = 1;
    while (fgets(line, MAX_LINE, file) != NULL) {
        // Trim the line
        trim(line, DELIM);
        i++;

        // Blank line
        if (strlen(line) == 0)
            continue;

        // Comment line
        if (line[0] == '#')
            continue;

        // Split line into fields
        int n = 0;
        char *fields[MAX_FIELDS];
        char *tok;
        tok = strtok(line, DELIM);
        while (tok != NULL) {
            if (n < MAX_FIELDS) {
                fields[n] = (char *) malloc(strlen(tok)+1);
                strcpy(fields[n], tok);         
                tok = strtok(NULL, DELIM);
                n++;
            } else {
                fprintf(stderr, "Exceeds the max number of fields %d\n",
                    MAX_FIELDS);
                break;
            }
        }

        // Update the tasks
        int step = atoi(fields[1]);
        struct var_descriptor var_desc;
        strcpy(var_desc.var_name, fields[0]);
        var_desc.step = step;
        evaluate_dataflow_by_available_var(wf, &var_desc);

        // Get READY tasks
        printf("Available var %s step= %d\n", fields[0], step);
        struct task_instance *ready_tasks[MAX_NUM_TASKS];
        int num_ready_tasks = 0;
        get_ready_tasks(wf, ready_tasks, &num_ready_tasks);
        if (num_ready_tasks > 0) {
            int j;
            for (j = 0; j < num_ready_tasks; j++) {
                printf("Execute tid= %d step= %d\n",
                    ready_tasks[j]->tid, ready_tasks[j]->step);
                update_task_instance_status(ready_tasks[j], task_finish);
            }
        }
    }

    fclose(file);

    return 0;
}
*/
