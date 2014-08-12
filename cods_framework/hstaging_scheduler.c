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
#include "timer.h"
#include "list.h"
#include "dart.h"
#include "ds_gspace.h"
#include "dimes_server.h"

#include "hstaging_scheduler.h"
#include "hstaging_api.h"
#include "hstaging_def.h"
#include "mpi.h"

static int num_sp;
static int num_cp;
static char *conf;

static struct dimes_server *dimes_s = NULL;
static struct dart_server *ds = NULL;
static struct ds_gspace *dsg = NULL;

static struct timer tm; 
static double tm_st;
//static double dag_tm_st, dag_tm_end;

struct hstaging_framework_state {
    unsigned char f_done;
    unsigned char f_notify_executor_to_exit;
};
static struct hstaging_framework_state framework_state;

struct runnable_task {
	struct list_head entry;
	struct hstaging_task *task_ref;
	// int num_input_vars;
    // enum hstaging_location_type location_type;
    int bk_allocation_size; // number of buckets required for task execution
    int *bk_idx_tab; // array of idx to the buckets in bk_tab 
};

enum bucket_status {
    bk_none = 0, // not ready for task execution 
    bk_idle,
    bk_busy,
};

struct mpi_rank_range {
    int l, r;
};

struct bucket {
    int dart_id; // unique id
    int pool_id; // indicate which resource pool the bucket is from
    int origin_mpi_rank;
    enum bucket_status status;
    struct executor_topology_info topo_info;
};

#define MAX_NUM_BK_PER_NODE 32
struct compute_node {
    struct node_topology_info topo_info; 
    int num_bucket;
    int bk_idx_tab[MAX_NUM_BK_PER_NODE];
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

static struct list_head bk_pool_list;
static struct list_head rtask_list; // runnable task list
static struct list_head pending_msg_list;
static struct list_head workflow_list;

/**
 Workflow & Tasks
**/

static void free_workflow(struct hstaging_workflow *wf)
{
    if (!wf) return;
    if (wf->num_tasks > 0) {
        uloga("WARNING %s: wf->num_tasks= %d\n", __func__, wf->num_tasks);
    }

    struct hstaging_task *task, *temp;
    list_for_each_entry_safe(task, temp, &wf->task_list, struct hstaging_task, entry) {
        list_del(&task->entry);
        free(task);
    }

    list_del(&wf->entry);
    free(wf);
}

static struct hstaging_task* workflow_lookup_task(struct hstaging_workflow *wf, uint32_t tid)
{
    struct hstaging_task *task;
    list_for_each_entry(task, &wf->task_list, struct hstaging_task, entry) {
        if (task->tid == tid)
            return task;
    } 

    return NULL;
}

static void workflow_add_task(struct hstaging_workflow *wf, struct hstaging_task *task)
{
    if (!task) return;
    list_add_tail(&task->entry, &wf->task_list);
    wf->num_tasks++;
}

static void workflow_clear_finished_tasks(struct hstaging_workflow *wf)
{
    struct hstaging_task *task, *temp;
    list_for_each_entry_safe(task, temp, &wf->task_list, struct hstaging_task, entry) {
        if (is_task_finish(task)) {
            list_del(&task->entry);
            free(task);
            wf->num_tasks--;
        }
    }
}

static struct hstaging_task* create_new_task(uint32_t wid, uint32_t tid, const char* conf_file)
{
    struct hstaging_task *task = (struct hstaging_task*)malloc(sizeof(*task));
    if (!task) {
        goto err_out;
    }
    task->wid = wid;
    task->tid = tid;
    task->status = task_not_ready;   
    task->appid = 0;
    task->placement_hint = hint_none;
    task->size_hint = 0;
    task->num_vars = 0;
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

static inline void workflow_list_init()
{
    INIT_LIST_HEAD(&workflow_list);
}

static struct hstaging_workflow* workflow_list_lookup(uint32_t wid) {
    struct hstaging_workflow *wf;
    list_for_each_entry(wf, &workflow_list, struct hstaging_workflow, entry) {
        if (wf->wid == wid)
            return wf;
    }

    return NULL;
}

static struct hstaging_workflow* workflow_list_create_new(uint32_t wid) {
    struct hstaging_workflow *wf;
    wf = (struct hstaging_workflow*)malloc(sizeof(*wf));
    wf->wid = wid;
    wf->state.f_done = 0;
    INIT_LIST_HEAD(&wf->task_list);
    wf->num_tasks = 0;

    list_add_tail(&wf->entry, &workflow_list);
    return wf;
}

static void workflow_list_clear_finished_tasks()
{
    struct hstaging_workflow *wf;
    list_for_each_entry(wf, &workflow_list, struct hstaging_workflow, entry) {
        workflow_clear_finished_tasks(wf);
    }
}

static void workflow_list_evaluate_dataflow(const struct hstaging_var *var_desc)
{   
    struct hstaging_workflow *wf;
    list_for_each_entry(wf, &workflow_list, struct hstaging_workflow, entry) {
       evaluate_dataflow_by_available_var(wf, var_desc);
    }
}

static void workflow_list_free()
{
    struct hstaging_workflow *wf, *temp;
    list_for_each_entry_safe(wf, temp, &workflow_list, struct hstaging_workflow, entry) {
        free_workflow(wf);
    }
}

/**
    Workflow executors
**/
static inline void bk_pool_init()
{
    INIT_LIST_HEAD(&bk_pool_list);
}

static struct bucket_pool* bk_pool_lookup(int pool_id)
{
    struct bucket_pool *bp;
    list_for_each_entry(bp, &bk_pool_list, struct bucket_pool, entry) {
        if (bp->pool_id == pool_id)
            return bp;
    }

    return NULL;    
}

static struct bucket_pool* bk_pool_create_new(int pool_id, int num_bucket)
{
    struct bucket_pool *bp;
    bp = malloc(sizeof(*bp));
    bp->pool_id = pool_id;
    bp->num_bucket = 0;
    bp->bk_tab_size = num_bucket;
    bp->bk_tab = malloc(sizeof(struct bucket) * num_bucket);
    bp->node_tab = NULL;
    bp->f_bk_reg_done = 0;
    bp->f_node_tab_done = 0;

    list_add_tail(&bp->entry, &bk_pool_list);
    return bp; 
}

static int bk_pool_add_bucket(struct bucket_pool *bp, int origin_mpi_rank, int dart_id,
                    int pool_id, struct executor_topology_info *topo_info)
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

int compare_bk_ptr(const void *p1, const void *p2)
{
    return (*(const struct bucket**)p1)->topo_info.nid -
           (*(const struct bucket**)p2)->topo_info.nid;
}

static void bk_pool_build_node_tab(struct bucket_pool *bp)
{
    struct bucket ** bk_ptr_tab = (struct bucket **)malloc(sizeof(struct bucket*)
                                    *bp->bk_tab_size);
    int i, j, k;
    // copy pointers
    for (i = 0; i < bp->bk_tab_size; i++) {
        bk_ptr_tab[i] = &bp->bk_tab[i];
    }

    // sort bk_ptr_tab by bk_ptr_tab[i]->topo_info.nid
    qsort(bk_ptr_tab, bp->bk_tab_size, sizeof(struct bucket *), compare_bk_ptr);

    bp->num_node = 0;
    uint32_t cur_nid = ~0; // assume node id can not be ~0.... 
    for (i = 0; i < bp->bk_tab_size; i++) {
        if (cur_nid != bk_ptr_tab[i]->topo_info.nid) {
            cur_nid = bk_ptr_tab[i]->topo_info.nid;
            bp->num_node++;
        }
    }    
        
    bp->node_tab = malloc(sizeof(struct compute_node)*bp->num_node);
    cur_nid = ~0;
    for (i = 0, j = -1, k = 0; i < bp->bk_tab_size; i++) {
        if (cur_nid != bk_ptr_tab[i]->topo_info.nid) {
            j++; 
            k = 0;
            cur_nid = bk_ptr_tab[i]->topo_info.nid;
            bp->node_tab[j].bk_idx_tab[k++] = bk_ptr_tab[i]->origin_mpi_rank;
            bp->node_tab[j].num_bucket = 1;
        } else {
            bp->node_tab[j].bk_idx_tab[k++] = bk_ptr_tab[i]->origin_mpi_rank;
            bp->node_tab[j].num_bucket += 1;
        }
    } 

    bp->f_node_tab_done = 1;

    // print it out
    uloga("bp->num_node= %d\n", bp->num_node);
    for (i = 0; i < bp->num_node; i++) {
        uloga("compute node id %u num_bucket %d:\n", bp->node_tab[i].topo_info.nid,
                bp->node_tab[i].num_bucket);
        for (k = 0; k < bp->node_tab[i].num_bucket; k++) {
            struct bucket *p = &(bp->bk_tab[bp->node_tab[i].bk_idx_tab[k]]);
            uloga("bucket dart_id %d pool_id %d origin_mpi_rank %d check nid %u\n",
                p->dart_id, p->pool_id, p->origin_mpi_rank, p->topo_info.nid);
        }
    }

    return;
}

static void bk_pool_free()
{
    struct bucket_pool *bp, *t;
    list_for_each_entry_safe(bp, t, &bk_pool_list, struct bucket_pool, entry)
    {
        list_del(&bp->entry);
        if (bp->bk_tab) free(bp->bk_tab);
        if (bp->node_tab) free(bp->node_tab);
        free(bp);
    }
}

/**
    Pending messages
**/
static void pending_msg_list_init()
{
    INIT_LIST_HEAD(&pending_msg_list);
}

static void pending_msg_list_free()
{
    int msg_cnt = 0;
    struct pending_msg *p, *t;
    list_for_each_entry_safe(p, t, &pending_msg_list, struct pending_msg, entry)
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
static inline void rtask_list_init()
{
	INIT_LIST_HEAD(&rtask_list);
}

static int rtask_list_count()
{
	int cnt = 0;
	struct runnable_task *rtask;
	list_for_each_entry(rtask, &rtask_list, struct runnable_task, entry) {
		cnt++;
	}

	return cnt;
}

static struct runnable_task *rtask_list_lookup(uint32_t wid, uint32_t tid)
{
    struct runnable_task *rtask;
	list_for_each_entry(rtask,&rtask_list,struct runnable_task, entry) {
		if (rtask->task_ref->wid == wid &&
            rtask->task_ref->tid == tid)
			return rtask;
	}

	return NULL;
}

static struct runnable_task *rtask_list_add_new(struct hstaging_task *t)
{
    struct runnable_task *rtask = malloc(sizeof(*rtask));
    rtask->task_ref = t;
    rtask->bk_allocation_size = t->size_hint;
    rtask->bk_idx_tab = NULL;

    update_task_status(t, task_pending);
    list_add_tail(&rtask->entry, &rtask_list);
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
	peer = ds_get_peer(ds, bk->dart_id);
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;
    
    struct hstaging_task *t = rtask->task_ref;
    size_t mpi_rank_tab_size = rtask->bk_allocation_size*sizeof(int);
    size_t var_tab_size = t->num_vars*sizeof(struct hstaging_var);
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
    struct hstaging_var *vars = (struct hstaging_var*)(msg->msg_data+mpi_rank_tab_size);
    for (i = 0; i < t->num_vars; i++) {
        vars[i] = t->vars[i];
    } 

	msg->msg_rpc->cmd = hs_exec_task_msg;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	struct hdr_exec_task *hdr =
		(struct hdr_exec_task *)msg->msg_rpc->pad;
    hdr->wid = t->wid;
	hdr->tid = t->tid;
    hdr->appid = t->appid;
	hdr->rank_hint = rank_hint; 
	hdr->nproc_hint = nproc_hint;
	hdr->num_vars = t->num_vars;

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

static int runnable_task_allocate_bk(struct runnable_task *rtask)
{
	if (rtask->task_ref->status != task_pending) {
		return 0;
	}

    // Try to determine placement location
/*
    enum hstaging_location_type preferred_loc;
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
    list_for_each_entry(bp, &bk_pool_list, struct bucket_pool, entry) {
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

    uloga("%s(): assign task (%d,%d) to buckets timestamp %lf\n",
        __func__, rtask->task_ref->wid, rtask->task_ref->tid, timer_read(&tm)-tm_st);
	return 0;	
}

static void rtask_list_free()
{
	struct runnable_task *rtask, *temp;
	list_for_each_entry_safe(rtask, temp, &rtask_list, struct runnable_task, entry) {
		list_del(&rtask->entry);
		free(rtask);
	}
}

/************/
static int timestamp_ = 0;
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

static int notify_task_submitter(struct hstaging_task *task)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;
    int dart_id = task->submitter_dart_id;

    peer = ds_get_peer(ds, dart_id);
    msg = msg_buf_alloc(ds->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_submitted_task_done_msg;
    msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
    struct hdr_submitted_task_done *hdr= (struct hdr_submitted_task_done*)msg->msg_rpc->pad;
    hdr->wid = task->wid;
    hdr->tid = task->tid;
    hdr->task_execution_time = 0;

    err = rpc_send(ds->rpc_s, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int process_finished_task()
{
    struct runnable_task *rtask, *temp;
    list_for_each_entry_safe(rtask, temp, &rtask_list, struct runnable_task, entry) {
        if (rtask_is_finish(rtask)) {
            uloga("WARNING %s: shuold not remove runnable task here...\n", __func__);
            // remove runnable task from the list
            list_del(&rtask->entry); 
            free(rtask);
        }
    }

    struct hstaging_workflow *wf;
    list_for_each_entry(wf, &workflow_list, struct hstaging_workflow, entry) {
        struct hstaging_task *task, *t;
        list_for_each_entry_safe(task, t, &wf->task_list, struct hstaging_task, entry) {
            if (is_task_finish(task)) {
                notify_task_submitter(task);
                // remove task from the workflow's task list 
                list_del(&task->entry);
                free(task);
                wf->num_tasks--;
            }
        }
    }    

    return 0;
}

static int process_workflow_state()
{
    struct hstaging_workflow *wf, *temp;
    list_for_each_entry_safe(wf, temp, &workflow_list, struct hstaging_workflow, entry) {
        if (wf->state.f_done) {
            uloga("%s: to free workflow wid= %u\n", __func__, wf->wid);
            // TODO: 
            free_workflow(wf);
        }
    }

    return 0;
}

static int process_framework_state()
{
    int err = -ENOMEM;
    if (framework_state.f_notify_executor_to_exit) return 0;
    if (framework_state.f_done && 0 == busy_bk_count()) {
        struct msg_buf *msg;
        struct node_id *peer;
        struct bucket_pool *bp;
        struct bucket *bk;
        list_for_each_entry(bp, &bk_pool_list, struct bucket_pool, entry)
        {
            int mpi_rank;
            for (mpi_rank = 0; mpi_rank < bp->bk_tab_size; mpi_rank++) {
                bk = bk_pool_get_bucket(bp, mpi_rank);
                peer = ds_get_peer(ds, bk->dart_id);
                msg = msg_buf_alloc(ds->rpc_s, peer, 1);
                if (!msg)
                    goto err_out;

                msg->msg_rpc->cmd = hs_stop_executor_msg;
                msg->msg_rpc->id = ds->rpc_s->ptlmap.id;

                err = rpc_send(ds->rpc_s, peer, msg);
                if (err < 0) {
                    free(msg);
                    goto err_out;
                }
            }
        }

        framework_state.f_notify_executor_to_exit = 1;
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

static int process_hs_build_staging(struct pending_msg *p)
{
    int err;
    int submitter_dart_id = p->cmd.id;
    struct hdr_build_staging *hdr = (struct hdr_build_staging*)p->cmd.pad;
    int pool_id = hdr->pool_id;
  
    struct bucket_pool *bp = bk_pool_lookup(pool_id);
    if (!bp) { 
        return -1;
    }
    if (!bp->f_bk_reg_done || !bp->f_node_tab_done) {
        return -1;
    }

    uloga("%s(): build multi-level staging for bucket pool %d based on config file '%s'\n",
        __func__, pool_id, hdr->staging_conf_file);

    // TODO: build multi-level staging 

    // reply to the submitter of the dag
    struct msg_buf *msg;
    struct node_id *peer;

    peer = ds_get_peer(ds, submitter_dart_id);
    msg = msg_buf_alloc(ds->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_build_staging_done_msg;
    msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
    err = rpc_send(ds->rpc_s, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
 err_out:
    err = -1;
    ERROR_TRACE();
}

static int process_pending_msg()
{
    // process one msg at a time
    struct pending_msg *p, *t;
    int err;
    list_for_each_entry_safe(p, t, &pending_msg_list, struct pending_msg, entry)
    {
        switch (p->cmd.cmd) {
        case hs_build_staging_msg:
            err = process_hs_build_staging(p);
            if (0 == err) {
                // remove msg from list
                list_del(&p->entry);
                free(p);
            }
            break;
        default:
            uloga("%s(): unknown message type\n", __func__);
            break;
        }
    }

    return 0;
}

/*
*/
static int process_runnable_task()
{
	struct runnable_task *rtask, *temp;
	int err = -ENOMEM;

	// 1. Add ready tasks (if any) 
    struct hstaging_workflow *wf;
    list_for_each_entry(wf, &workflow_list, struct hstaging_workflow, entry) {
        struct hstaging_task *tasks[MAX_NUM_TASKS];
        int num_tasks;
        get_ready_tasks(wf, tasks, &num_tasks);
        if (num_tasks > 0) {
            int i;
            for (i = 0; i < num_tasks; i++) {
                rtask_list_add_new(tasks[i]);
            }
        }
    }

	// 2. process runnable tasks (if any) 
	list_for_each_entry_safe(rtask, temp, &rtask_list, struct runnable_task, entry) {
		if (rtask_is_pending(rtask)) {
			runnable_task_allocate_bk(rtask);
		}
	}	

	return 0;
}

static int callback_hs_finish_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_finish_task *hdr = (struct hdr_finish_task*)cmd->pad;
    struct bucket_pool *bp = bk_pool_lookup(hdr->pool_id);
    if (bp == NULL) {
        uloga("ERROR %s(): should not happen bp == NULL\n", __func__);
        return 0;
    }

    struct runnable_task *rtask = rtask_list_lookup(hdr->wid, hdr->tid);
    if (rtask) {
        uloga("%s(): finish task (%u,%u) timestamp %lf\n",
            __func__, hdr->wid, hdr->tid, timer_read(&tm)-tm_st);

        rtask_set_finish(rtask);
        bk_pool_free_bk_allocation(bp, rtask->bk_idx_tab, rtask->bk_allocation_size);
        free_rtask(rtask); 
    } else {
        uloga("ERROR %s: failed to find task (%u,%u)\n",
            __func__, hdr->wid, hdr->tid);
    } 

    return 0;
}

static int callback_hs_reg_resource(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_register_resource *hdr = (struct hdr_register_resource*)cmd->pad;
    int pool_id = hdr->pool_id;
    int num_bucket = hdr->num_bucket;
    int mpi_rank = hdr->mpi_rank;
    int dart_id = cmd->id; // TODO: do we need to store dart id explicitly in hdr?

#ifdef HAVE_UGNI
    uloga("%s(): get msg from peer #%d mpi_rank %d num_bucket %d pool_id %d nid %u "
            "mesh_coord (%u,%u,%u)\n",
            __func__, dart_id, mpi_rank, num_bucket, pool_id,
            hdr->topo_info.nid, hdr->topo_info.mesh_coord.mesh_x,
            hdr->topo_info.mesh_coord.mesh_y, hdr->topo_info.mesh_coord.mesh_z);
#endif

    struct bucket_pool *bp = bk_pool_lookup(pool_id);
    if (bp == NULL) {
        bp = bk_pool_create_new(pool_id, num_bucket);
        if (bp == NULL) {
            uloga("ERROR %s(): bk_pool_create_new() failed\n", __func__);
            return -1;
        }
    }

    bk_pool_add_bucket(bp, mpi_rank, dart_id, pool_id, &hdr->topo_info);
    // Check if all peers of the resource pool have registered
    if (bp->num_bucket == bp->bk_tab_size) {
        bk_pool_set_bucket_idle(bp);
        bk_pool_build_node_tab(bp);
    }

    return 0;
}

static int callback_hs_finish_workflow(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_finish_workflow *hdr = (struct hdr_finish_workflow*)cmd->pad;
    struct hstaging_workflow *wf = workflow_list_lookup(hdr->wid);    
    if (wf) {
        wf->state.f_done = 1;
    } else {
        uloga("ERROR %s: failed to lookup workflow with wid= %u\n", __func__,
            hdr->wid);
    }

	return 0;
}

// TODO: make the workflow evaluation asynchronous...
static int callback_hs_update_var(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_update_var *hdr = (struct hdr_update_var*)cmd->pad;

/*
	uloga("%s(): update variable '%s' version %d "
		"dims %d bbox {(%d,%d,%d),(%d,%d,%d)}\n", 
			__func__, hdr->name, hdr->version, hdr->bb.num_dims,
			hdr->bb.lb.c[0], hdr->bb.lb.c[1], hdr->bb.lb.c[2],
			hdr->bb.ub.c[0], hdr->bb.ub.c[1], hdr->bb.ub.c[2]);
*/

    struct hstaging_var var_desc;
    strcpy(var_desc.name, hdr->name);
    var_desc.version = hdr->version;
    var_desc.elem_size = hdr->elem_size;
    var_desc.gdim = hdr->gdim;
    workflow_list_evaluate_dataflow(&var_desc);

	return 0;
}

static int callback_hs_submit_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_submit_task *hdr = (struct hdr_submit_task*)cmd->pad;
    uloga("%s(): config file %s\n", __func__, hdr->conf_file);

    struct hstaging_workflow *wf = workflow_list_lookup(hdr->wid);
    if (!wf) {
        wf = workflow_list_create_new(hdr->wid);
    } else {
        if (workflow_lookup_task(wf, hdr->tid)) {
            uloga("ERROR %s: task tid= %d already exist\n", __func__, hdr->tid);
            return 0;
        }
    }

    struct hstaging_task *task = create_new_task(hdr->wid, hdr->tid, hdr->conf_file);
    if (!task) {
        return 0;
    }

    task->submitter_dart_id = cmd->id;
    workflow_add_task(wf, task);

    print_workflow(wf);
    return 0;
}

static int callback_hs_build_staging(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    uloga("%s(): get request from peer #%d\n", __func__, cmd->id);

    // copy message to the application-level message list 
    struct pending_msg *p = malloc(sizeof(*p));
    memcpy(&p->cmd, cmd, sizeof(struct rpc_cmd));        
    list_add_tail(&p->entry, &pending_msg_list);

    return 0;
}

static int callback_hs_stop_framework(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    framework_state.f_done = 1;    
    return 0;
}

int hstaging_scheduler_init()
{
	rpc_add_service(hs_finish_workflow_msg, callback_hs_finish_workflow);
	rpc_add_service(hs_update_var_msg, callback_hs_update_var); 
    rpc_add_service(hs_reg_resource_msg, callback_hs_reg_resource);
    rpc_add_service(hs_finish_task_msg, callback_hs_finish_task);
    rpc_add_service(hs_submit_task_msg, callback_hs_submit_task);
    rpc_add_service(hs_build_staging_msg, callback_hs_build_staging);
    rpc_add_service(hs_stop_framework_msg, callback_hs_stop_framework);

	dimes_s = dimes_server_alloc(num_sp, num_cp, conf);
	if (!dimes_s) {
		return -1;
	}
	
	dsg = dimes_s->dsg;
	ds = dimes_s->dsg->ds;

	// Init
    pending_msg_list_init();
    bk_pool_init();
    workflow_list_init();
	rtask_list_init();
    framework_state.f_done = 0;
    framework_state.f_notify_executor_to_exit = 0;

    timer_init(&tm, 1);
    timer_start(&tm);
    tm_st = timer_read(&tm);

	return 0;
}

int hstaging_scheduler_run()
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

        process_pending_msg();
        process_runnable_task();
        process_finished_task();
        process_workflow_state();
        process_framework_state();
	}

    pending_msg_list_free();
    bk_pool_free();
	rtask_list_free();
    workflow_list_free();

	return 0;
}

int hstaging_scheduler_finish()
{
	dimes_server_barrier(dimes_s);
	dimes_server_free(dimes_s);

	return 0;
}


void hstaging_scheduler_usage()
{
	printf("Usage: server OPTIONS\n"
			"OPTIONS: \n"
			"--server, -s    Number of server instance/staging nodes\n"
			"--cnodes, -c    Number of compute nodes\n"
			"--conf, -f      Define configuration file\n");
}

int hstaging_scheduler_parse_args(int argc, char *argv[])
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
