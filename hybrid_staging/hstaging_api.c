#include "hstaging_api.h"
#include "hstaging_def.h"

#include <stdio.h>
#include <stdlib.h>
#include "debug.h"
#include "dart.h"
#include "dataspaces.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"


static struct dart_client *dc;
static struct dcg_space *dcg;
static struct timer timer;
//static enum storage_type st = column_major;
static enum storage_type st = row_major;
// TODO: 'num_dims' is hardcoded to 2.
static int num_dims = 2;

#define MY_DART_ID dc->rpc_s->ptlmap.id 

/**
    Platform (system) dependent functions
**/
#ifdef HAVE_UGNI
static int process_event(struct dcg_space *dcg)
{
    int err;
    err = rpc_process_event_with_timeout(dcg->dc->rpc_s, 1);
    if (err < 0)
        goto err_out;

    return 0;
err_out:
    ERROR_TRACE();
}
#endif

#ifdef HAVE_DCMF
static int process_event(struct dcg_space *dcg)
{
    int err;
    err = rpc_process_event(dcg->dc->rpc_s);
    if (err < 0)
        goto err_out;

    return 0;
err_out:
    ERROR_TRACE();
}
#endif

#ifdef HAVE_UGNI 
int get_topology_information(struct executor_topology_info *topo_info)
{
  int rc;
  int pmi_rank, size;

  PMI_BOOL initialized;
  rc = PMI_Initialized(&initialized);
  if (rc!=PMI_SUCCESS)
    PMI_Abort(rc,"PMI_Initialized failed");

  if (initialized!=PMI_TRUE)
  {
    int spawned;
    rc = PMI_Init(&spawned);
    if (rc!=PMI_SUCCESS)
      PMI_Abort(rc,"PMI_Init failed");
  }

  rc = PMI_Get_rank(&pmi_rank);
  if (rc!=PMI_SUCCESS)
    PMI_Abort(rc,"PMI_Get_rank failed");

  rc = PMI_Get_size(&size);
  if (rc!=PMI_SUCCESS)
    PMI_Abort(rc,"PMI_Get_size failed");

  int nid;
  rc = PMI_Get_nid(pmi_rank, &topo_info->nid);
  if (rc!=PMI_SUCCESS)
    PMI_Abort(rc,"PMI_Get_nid failed");

  rca_get_meshcoord( (uint16_t) topo_info->nid, &topo_info->mesh_coord);
  return 0;
}
#endif

/**
    Messaging
**/
struct client_rpc_send_state {
    int f_done;
};

static int client_rpc_send_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct client_rpc_send_state *p = (struct client_rpc_send_state *)msg->private;
    p->f_done = 1;
    return 0;
}

static int client_rpc_send(struct node_id *peer, struct msg_buf *msg, struct client_rpc_send_state *state)
{
    int err;
    state->f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = state;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out;
    }

    // Wait/block for the message delivery 
    while (!state->f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }

    return 0;
 err_out:
    uloga("ERROR %s: err= %d\n", __func__, err);
    return err;
}

/**
    Data structures: bucket (executor), task
**/
struct parallel_job {
	struct list_head entry;
	int type;
	int tstep;
};

struct bucket_info {
	/* keep basic info of current task */
	struct task_descriptor *current_task;

	/* flags */
	int f_get_task;
	int f_stop_executor; //flag indicates that current executor can exit 
	int f_init_dspaces;
    int f_build_staging_done;

	/* track data get time */
	double tm_st, tm_end;

	enum hstaging_pe_type pe_type;
    // enum hstaging_location_type location_type;
    int pool_id, mpi_rank, num_bucket;
    struct executor_topology_info topo_info;
};
static struct bucket_info bk_info;

struct task_info {
    struct list_head entry;
    uint32_t wid;
    uint32_t tid;
    char conf_file[NAME_MAXLEN];
    unsigned char f_task_execution_done;
};
static struct list_head submitted_task_list;

void submitted_task_list_init()
{
    INIT_LIST_HEAD(&submitted_task_list);
}

struct task_info* create_submitted_task(uint32_t wid, uint32_t tid, const char *conf_file)
{
    struct task_info *t = malloc(sizeof(*t));
    if (!t) {
        uloga("ERROR %s: malloc() failed\n", __func__);
        return NULL;
    }
    
    t->wid = wid;
    t->tid = tid;
    strcpy(t->conf_file, conf_file);
    t->f_task_execution_done = 0;

    list_add_tail(&t->entry, &submitted_task_list);
    return t; 
}

struct task_info* lookup_submitted_task(uint32_t wid, uint32_t tid)
{
    struct task_info *t;
    list_for_each_entry(t, &submitted_task_list, struct task_info, entry) {
        if (t->wid == wid && t->tid == tid) {
            return t;
        }
    }

    return NULL;
}

void free_submitted_task(struct task_info *t)
{
    if (!t) return;
    list_del(&t->entry);
    free(t);
}

inline int is_submitted_task_done(struct task_info* t) {
    return t->f_task_execution_done;
}

static int fetch_task_info_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    size_t mpi_rank_tab_size = bk_info.current_task->nproc*sizeof(int);
    size_t var_tab_size = bk_info.current_task->num_vars*sizeof(struct hstaging_var);

    // copy origin mpi ranks for allocated bk
    bk_info.current_task->bk_mpi_rank_tab = malloc(mpi_rank_tab_size);
    memcpy(bk_info.current_task->bk_mpi_rank_tab, msg->msg_data, mpi_rank_tab_size);

    // copy vars
    bk_info.current_task->vars = malloc(var_tab_size);
    memcpy(bk_info.current_task->vars, msg->msg_data+mpi_rank_tab_size, var_tab_size);

	bk_info.f_get_task = 1;
    free(msg->msg_data);
	free(msg);
	return 0;
}

static int fetch_task_info(struct rpc_cmd *cmd)
{
	struct node_id *peer = dc_get_peer(dc, cmd->id);
	struct msg_buf *msg;
	int err = -ENOMEM;

	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

    msg->size = bk_info.current_task->num_vars*sizeof(struct hstaging_var)
                + bk_info.current_task->nproc*sizeof(int);
    msg->msg_data = malloc(msg->size);
	msg->cb = fetch_task_info_completion;

	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(dc->rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	if (err == 0)
		return 0;

	free(msg);
err_out:
	ERROR_TRACE();
}

static int callback_hs_exec_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_exec_task *hdr = (struct hdr_exec_task *)cmd->pad;
    bk_info.current_task->wid = hdr->wid;
	bk_info.current_task->tid = hdr->tid;
    bk_info.current_task->appid = hdr->appid;
	bk_info.current_task->rank = hdr->rank_hint;
	bk_info.current_task->nproc = hdr->nproc_hint;
    bk_info.current_task->num_vars = hdr->num_vars;
    bk_info.current_task->vars = NULL;

    if (fetch_task_info(cmd) < 0) {
        uloga("ERROR %s(): failed to fetch info for task (%d,%d)\n",
            __func__, hdr->wid, hdr->tid);
        bk_info.current_task = NULL;
        return -1;
    }
    
	return 0;
}

static int callback_hs_submitted_task_done(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_submitted_task_done *hdr = (struct hdr_submitted_task_done *)cmd->pad;
    uloga("%s(): task wid= %u tid= %u execution time %.6f\n", __func__, 
        hdr->wid, hdr->tid, hdr->task_execution_time);

    struct task_info *t = lookup_submitted_task(hdr->wid, hdr->tid);
    if (!t) {
        uloga("ERROR %s: failed to find submitted task wid= %u tid= %u\n",
            __func__, hdr->wid, hdr->tid);
        return 0;
    }
    
    t->f_task_execution_done = 1;
    return 0;
}

static int callback_hs_stop_executor(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	uloga("%s(): rank= %d get msg from %d\n",
		__func__, rpc_s->ptlmap.id, cmd->id);

	bk_info.f_stop_executor = 1;
	return 0;
}

static int callback_hs_build_staging_done(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    bk_info.f_build_staging_done = 1;    
    return 0;
}

/*
*
  Public APIs
*
*/

// TODO: do we need to separate the init function for submitter/executor?
/* Initialize the dataspaces library. */
int hstaging_init(int num_peers, int appid, enum hstaging_pe_type pe_type)
{
	int err = -ENOMEM;

	/* Init the bk_info */
	bk_info.pe_type = pe_type;
	bk_info.f_stop_executor = 0;
	bk_info.f_init_dspaces = 0;

	if (dcg) {
		return 0;
	}

	// TODO: does it matter to place rpc_add_service after dspaces_init()?
	rpc_add_service(hs_stop_executor_msg, callback_hs_stop_executor);
	rpc_add_service(hs_exec_task_msg, callback_hs_exec_task);
    rpc_add_service(hs_submitted_task_done_msg, callback_hs_submitted_task_done);
    rpc_add_service(hs_build_staging_done_msg, callback_hs_build_staging_done);

	err = dspaces_init(num_peers, appid);
	if (err < 0) {
		uloga("%s(): failed to initialize.\n", __func__);
		return err;	
	}

	dcg = dcg_get_ref();
	if (!dcg) {
		uloga("%s(): dcg == NULL should not happen.\n", __func__);
		return -1;
	}

	dc = dcg->dc;
	if (!dc) {
		uloga("%s(): dc == NULL should not happen\n", __func__);
		return -1;
	}

	timer_init(&timer, 1);
	timer_start(&timer);

    // Init
    submitted_task_list_init();    
	
	bk_info.f_init_dspaces = 1;
	return 0;
}

/* Finalize the dataspaces library. */
int hstaging_finalize()
{
	int err;
	if (!bk_info.f_init_dspaces) {
		uloga("'%s()': library was not properly initialized!\n", __func__);
		return -1;
	}

    // Free all previously allocated RDMA buffers
    dimes_put_sync_all();

	dspaces_finalize();
	dcg = 0;
	return 0;
err_out:
	dspaces_finalize();
	dcg = 0;
	return -1;
}

int hstaging_put_var(const char *var_name, unsigned int ver, int size,
    int xl, int yl, int zl,
    int xu, int yu, int zu,
    void *data, MPI_Comm *comm)
{
	int err = -ENOMEM;

	if (!bk_info.f_init_dspaces) {
		uloga("%s(): library not init!\n", __func__);
		goto err_out;
	}

	dspaces_lock_on_write(var_name, comm);
	err = dimes_put(var_name, ver, size, xl, yl, zl, xu, yu, zu, data);
	if (err < 0) {
		goto err_out;
	}
	dspaces_unlock_on_write(var_name, comm);

	return 0;
err_out:
	ERROR_TRACE();
}

int hstaging_put_sync_all()
{
	return dimes_put_sync_all();
}

int hstaging_get_var(const char *var_name, unsigned int ver, int size,
    int xl, int yl, int zl,
    int xu, int yu, int zu,
    void *data, MPI_Comm *comm)
{
	int err = -ENOMEM;

	if (!bk_info.f_init_dspaces) {
		uloga("%s(): library not init!\n", __func__);
		goto err_out;
	}

    if (comm) {
        dspaces_lock_on_read(var_name, comm);
    }
	err = dimes_get(var_name, ver, size, xl, yl, zl, xu, yu, zu, data);
	if (err < 0) {
		goto err_out;
	}
    if (comm) {
        dspaces_unlock_on_read(var_name, comm);
    }
	return 0;
err_out:
	ERROR_TRACE();
}

int hstaging_update_var(struct hstaging_var *var, enum hstaging_update_var_op op)
{
    struct client_rpc_send_state send_state;
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_update_var *hdr;
	int peer_id, err = -ENOMEM;

	peer = dc_get_peer(dc, 0); //master srv has dart id as 0
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = hs_update_var_msg;
	msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
	hdr = (struct hdr_update_var*) msg->msg_rpc->pad;
	strcpy(hdr->name, var->name);
	hdr->version = var->version;
	hdr->elem_size = var->elem_size;
	hdr->bb = var->bb;
	hdr->op = op;

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

	return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
	ERROR_TRACE();
}

// TODO: the logic of the function is not clear
int hstaging_request_task(struct task_descriptor *t)
{
	int err;

	bk_info.f_get_task = 0;
	bk_info.current_task = t;

	while (!bk_info.f_get_task && !bk_info.f_stop_executor) {
		err = process_event(dcg);
		if (err < 0) {
			return err;
		}
	}	

	if (bk_info.f_stop_executor) {
		return -1;
	}	

	bk_info.current_task = NULL;
	return 0;
}

int hstaging_register_executor(int pool_id, int num_bucket, int mpi_rank)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_register_resource *hdr;
    int err = -ENOMEM;   

    bk_info.pool_id = pool_id;
    bk_info.num_bucket = num_bucket;
    bk_info.mpi_rank = mpi_rank;
    get_topology_information(&bk_info.topo_info);

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_reg_resource_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    hdr = (struct hdr_register_resource *)msg->msg_rpc->pad;
    hdr->pool_id = bk_info.pool_id;
    hdr->num_bucket = bk_info.num_bucket;
    hdr->mpi_rank = bk_info.mpi_rank;
    hdr->topo_info = bk_info.topo_info;

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();     
}

int hstaging_set_task_finished(struct task_descriptor *t)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_finish_task *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_finish_task_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    hdr= (struct hdr_finish_task *)msg->msg_rpc->pad;
    hdr->pool_id = bk_info.pool_id;
    hdr->wid = t->wid;
    hdr->tid = t->tid;

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int hstaging_submit_task_nb(uint32_t wid, uint32_t tid, const char* conf_file)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_submit_task_msg;
    msg->msg_rpc->id = MY_DART_ID;
    struct hdr_submit_task *hdr= (struct hdr_submit_task*)msg->msg_rpc->pad;
    hdr->wid = wid;
    hdr->tid = tid;
    strcpy(hdr->conf_file, conf_file);

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    struct task_info *t = create_submitted_task(wid, tid, conf_file);    
    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int hstaging_wait_submitted_task(uint32_t wid, uint32_t tid)
{
    int err = -ENOMEM;
    struct task_info *t = lookup_submitted_task(wid, tid);
    if (!t) {
        uloga("ERROR %s: failed to find submitted task wid= %u tid= %u\n",
            __func__, wid, tid);
        return err;
    }

    // Wait/block for the task execution to complete
    while (!is_submitted_task_done(t)) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }

    free_submitted_task(t);
    return 0;
 err_out:
    ERROR_TRACE();
}

int hstaging_submit_task(uint32_t wid, uint32_t tid, const char* conf_file)
{
    int err;
    err = hstaging_submit_task_nb(wid, tid, conf_file);
    if (err < 0) {
        return err;
    }

    err = hstaging_wait_submitted_task(wid, tid);
    if (err < 0) {
        return err;
    }

    return 0;
}

int hstaging_build_staging(int pool_id, const char *conf_file)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_build_staging *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_build_staging_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    hdr= (struct hdr_build_staging *)msg->msg_rpc->pad;
    hdr->pool_id = pool_id;
    strcpy(hdr->staging_conf_file, conf_file);

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait for the reply from master
    while (!bk_info.f_build_staging_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }

    return 0;
err_out_free:
    if (msg) free(msg);
err_out:
    ERROR_TRACE();
}

int hstaging_set_workflow_finished(uint32_t wid)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_finish_workflow *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_finish_workflow_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    hdr = (struct hdr_finish_workflow*) msg->msg_rpc->pad;
    hdr->wid = wid;

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int hstaging_stop_framework()
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    //struct hdr_stop_framework *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_stop_framework_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

    err = client_rpc_send(peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

