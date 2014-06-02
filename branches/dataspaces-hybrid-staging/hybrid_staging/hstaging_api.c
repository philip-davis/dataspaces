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

struct dag_info {
    char dag_conf_file[MAX_VAR_NAME_LEN];
    int f_dag_execution_done; //flag indicates that all tasks of the dag is done
};
static struct dag_info current_dag;

struct client_rpc_send_state {
    int f_done;
};

static int client_rpc_send_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct client_rpc_send_state *p = (struct client_rpc_send_state *)msg->private;
    p->f_done = 1;
    return 0;
}

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

static int fetch_task_info_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    size_t mpi_rank_tab_size = bk_info.current_task->nproc*sizeof(int);
    size_t input_var_tab_size =
                bk_info.current_task->num_input_vars*sizeof(struct var_descriptor);

    // copy origin mpi ranks for allocated bk
    bk_info.current_task->bk_mpi_rank_tab = malloc(mpi_rank_tab_size);
    memcpy(bk_info.current_task->bk_mpi_rank_tab, msg->msg_data, mpi_rank_tab_size);

    // copy input vars
    bk_info.current_task->input_vars = malloc(input_var_tab_size);
    memcpy(bk_info.current_task->input_vars, msg->msg_data+mpi_rank_tab_size,
            input_var_tab_size);

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

    msg->size = bk_info.current_task->num_input_vars*sizeof(struct var_descriptor)
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
	bk_info.current_task->tid = hdr->tid;
	bk_info.current_task->step = hdr->step;
	bk_info.current_task->rank = hdr->rank_hint;
	bk_info.current_task->nproc = hdr->nproc_hint;
    bk_info.current_task->num_input_vars = hdr->num_input_vars;
    bk_info.current_task->input_vars = NULL;

    if (fetch_task_info(cmd) < 0) {
        uloga("ERROR %s(): failed to fetch info for task (%d,%d)\n",
            __func__, hdr->tid, hdr->step);
        bk_info.current_task = NULL;
        return -1;
    }
    
	return 0;
}

static int callback_hs_finish_dag(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_finish_dag *hdr = (struct hdr_finish_dag*)cmd->pad;
    uloga("%s(): dag execution time %.6f\n", __func__, hdr->dag_execution_time);
    current_dag.f_dag_execution_done = 1;

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
    rpc_add_service(hs_finish_dag_msg, callback_hs_finish_dag);
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

int hstaging_update_var(struct var_descriptor *var_desc, enum hstaging_update_var_op op)
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
	strcpy(hdr->var_name, var_desc->var_name);
	hdr->step = var_desc->step;
	hdr->size = var_desc->size;
	hdr->bb = var_desc->bb;
	hdr->op = op;

    send_state.f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = &send_state;

	err = rpc_send(dc->rpc_s, peer, msg);
	if (err < 0) {
		goto err_out_free;
	}

    // Wait/block for the message delivery 
    while (!send_state.f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
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

    send_state.f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = &send_state;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait/block for the message delivery 
    while (!send_state.f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();     
}

int hstaging_set_task_done(struct task_descriptor *t)
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
    hdr->tid = t->tid;
    hdr->step = t->step;

    send_state.f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = &send_state;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait/block for the message delivery 
    while (!send_state.f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int hstaging_execute_dag(const char* conf_file)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_exec_dag_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    struct hdr_exec_dag *hdr= (struct hdr_exec_dag*)msg->msg_rpc->pad;
    strcpy(hdr->dag_conf_file, conf_file);

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out_free;
    }

    strcpy(current_dag.dag_conf_file, conf_file);
    current_dag.f_dag_execution_done = 0;

    // Wait/block for the dag execution to complete
    while (!current_dag.f_dag_execution_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
    }    

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();        
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

    send_state.f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = &send_state;

    bk_info.f_build_staging_done = 0;    
    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait/block for the message delivery 
    while (!send_state.f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
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

int hstaging_set_workflow_finished()
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_finish_workflow_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

    send_state.f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = &send_state;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait/block for the message delivery 
    while (!send_state.f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}
