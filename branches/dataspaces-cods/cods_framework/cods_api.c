#include "cods_api.h"
#include "cods_internal.h"

#include <stdio.h>
#include <stdlib.h>
#include "debug.h"
#include "dart.h"
#include "dataspaces.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"


static struct dcg_space *dcg;
static struct timer timer;

#define DART_DC dcg->dc
#define DART_RPC_S dcg->dc->rpc_s
#define DART_ID dcg->dc->rpc_s->ptlmap.id 

static int get_server_id()
{
    return DART_ID % dcg->dc->num_sp;
}

/**
    Platform (system) dependent functions
**/
#ifdef HAVE_UGNI 
int get_topology_information(struct node_topology_info *topo_info)
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
    Data structures: bucket (executor), task
**/
struct parallel_job {
	struct list_head entry;
	int type;
	int tstep;
};

struct bucket_info {
	/* keep basic info of current task */
	struct cods_task *current_task;

	/* flags */
	int f_get_task;
	int f_stop_executor; //flag indicates that current executor can exit 
	int f_init_dspaces;

	/* track data get time */
	double tm_st, tm_end;

	enum cods_pe_type pe_type;
    int dart_id, pool_id, mpi_rank, num_bucket;
    struct node_topology_info topo_info;
};
static struct bucket_info bk_info;

struct task_info {
    struct list_head entry;
    uint32_t tid;
    char conf_file[NAME_MAXLEN];
    unsigned char f_task_execution_done;
};
static struct list_head submitted_task_list;

struct cods_submitter {
    unsigned char f_get_executor_pool_info;
    unsigned char f_build_partition;
    struct executor_pool_info *pool_info;
};
static struct cods_submitter submitter; 

static struct list_head pending_msg_list;

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
}

void submitted_task_list_init()
{
    INIT_LIST_HEAD(&submitted_task_list);
}

struct task_info* create_submitted_task(struct task_descriptor *task_desc)
{
    struct task_info *t = malloc(sizeof(*t));
    if (!t) {
        uloga("ERROR %s: malloc() failed\n", __func__);
        return NULL;
    }
    
    t->tid = task_desc->tid;
    strcpy(t->conf_file, task_desc->conf_file);
    t->f_task_execution_done = 0;

    list_add_tail(&t->entry, &submitted_task_list);
    return t; 
}

struct task_info* lookup_submitted_task(uint32_t tid)
{
    struct task_info *t;
    list_for_each_entry(t, &submitted_task_list, struct task_info, entry) {
        if (t->tid == tid) {
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

static int fetch_task_info(struct cods_task *task, const char *meta_data_name)
{
    size_t mpi_rank_tab_size = bk_info.current_task->nproc*sizeof(int);
    size_t var_tab_size = bk_info.current_task->num_vars*sizeof(struct cods_var);
    size_t size = mpi_rank_tab_size + var_tab_size;

    void *data = malloc(size);
    int err = read_meta_data(meta_data_name, size, data);
    if (err < 0) {
        free(data);
        return -1;
    }   
 
    // copy origin mpi ranks for allocated bk
    task->bk_mpi_rank_tab = malloc(mpi_rank_tab_size);
    memcpy(task->bk_mpi_rank_tab, data, mpi_rank_tab_size);

    // copy vars
    task->vars = malloc(var_tab_size);
    memcpy(task->vars, data+mpi_rank_tab_size, var_tab_size);

    free(data);
    return 0;
}

static int callback_cods_exec_task(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_exec_task *hdr = (struct hdr_exec_task *)cmd->pad;
	bk_info.current_task->tid = hdr->tid;
    bk_info.current_task->appid = hdr->appid;
	bk_info.current_task->rank = hdr->rank_hint;
	bk_info.current_task->nproc = hdr->nproc_hint;
    bk_info.current_task->num_vars = hdr->num_vars;
    bk_info.current_task->bk_mpi_rank_tab = NULL;
    bk_info.current_task->vars = NULL;

    if (fetch_task_info(bk_info.current_task, hdr->meta_data_name) < 0) {
        uloga("ERROR %s(): failed to fetch info for task tid= %u\n",
            __func__, hdr->tid);
        bk_info.current_task = NULL;
        return -1;
    }

    bk_info.f_get_task = 1;    
	return 0;
}

static int process_cods_executor_pool_info(struct pending_msg *p)
{
    struct hdr_executor_pool_info *hdr = p->cmd.pad;
    submitter.pool_info = malloc(sizeof(*submitter.pool_info));
    submitter.pool_info->pool_id = hdr->pool_id;
    submitter.pool_info->num_executor = hdr->num_executor;

    // Read executor pool information from dataspaces.
    size_t size = sizeof(struct executor_descriptor)*hdr->num_executor;
    submitter.pool_info->executor_tab = malloc(size);
    int err = read_meta_data(hdr->meta_data_name, size, submitter.pool_info->executor_tab);
    if (err < 0) {
        free(submitter.pool_info->executor_tab);
        free(submitter.pool_info);
        goto err_out;
    }

    // Set the flag.
    submitter.f_get_executor_pool_info = 1;
    return 0;
 err_out:
    return -1;
}

static int callback_cods_submitted_task_done(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct hdr_submitted_task_done *hdr = (struct hdr_submitted_task_done *)cmd->pad;
    //uloga("%s(): task tid= %u execution time %.6f\n", __func__, 
    //    hdr->tid, hdr->task_execution_time);

    struct task_info *t = lookup_submitted_task(hdr->tid);
    if (!t) {
        uloga("ERROR %s: failed to find submitted task tid= %u\n",
            __func__, hdr->tid);
        return 0;
    }
    
    t->f_task_execution_done = 1;
    return 0;
}

static int callback_cods_stop_executor(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	uloga("%s(): #%d get msg from #%d\n",
		__func__, DART_ID, cmd->id);

	bk_info.f_stop_executor = 1;
	return 0;
}

static int callback_cods_build_partition_done(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    submitter.f_build_partition = 1;
    return 0;
}

static int callback_add_pending_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
    struct pending_msg *p = malloc(sizeof(*p));
    memcpy(&p->cmd, cmd, sizeof(struct rpc_cmd));
    list_add_tail(&p->entry, &pending_msg_list);
    return 0;
}

static int process_pending_msg()
{
    struct pending_msg *p, *t;
    list_for_each_entry_safe(p, t, &pending_msg_list, struct pending_msg, entry)
    {
        int err = 0;
        switch(p->cmd.cmd) {
        case cods_executor_pool_info_msg:
            err = process_cods_executor_pool_info(p);
            break;
        default:
            uloga("%s(): unknonw message type\n", __func__);
            break;
        }

        if (err == 0) {
            list_del(&p->entry);
            free(p);
            break; // process one msg at a time.
        }
    }
    return 0;    
}

/*
*
  Public APIs
*
*/

// TODO: do we need to separate the init function for submitter/executor?
/* Initialize the dataspaces library. */
int cods_init(int num_peers, int appid, enum cods_pe_type pe_type)
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
	rpc_add_service(cods_stop_executor_msg, callback_cods_stop_executor);
	rpc_add_service(cods_exec_task_msg, callback_cods_exec_task);
    rpc_add_service(cods_submitted_task_done_msg, callback_cods_submitted_task_done);
    rpc_add_service(cods_executor_pool_info_msg, callback_add_pending_msg);
    rpc_add_service(cods_build_partition_done_msg, callback_cods_build_partition_done);

	err = dspaces_init(num_peers, appid, NULL, NULL);
	if (err < 0) {
		uloga("%s(): failed to initialize.\n", __func__);
		return err;	
	}

	dcg = dcg_get_ref();
	if (!dcg) {
		uloga("%s(): dcg == NULL should not happen.\n", __func__);
		return -1;
	}

	timer_init(&timer, 1);
	timer_start(&timer);

    // Init
    submitted_task_list_init();    
    pending_msg_list_init(&pending_msg_list);
	
	bk_info.f_init_dspaces = 1;
	return 0;
}

/* Finalize the dataspaces library. */
int cods_finalize()
{
	int err;
	if (!bk_info.f_init_dspaces) {
		uloga("'%s()': library was not properly initialized!\n", __func__);
		return -1;
	}

    pending_msg_list_free(&pending_msg_list);

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

int cods_update_var(struct cods_var *var, enum cods_update_var_op op)
{
    struct client_rpc_send_state send_state;
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_update_var *hdr;
	int peer_id, err = -ENOMEM;

	peer = dc_get_peer(DART_DC, get_server_id());
	msg = msg_buf_alloc(DART_RPC_S, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = cods_update_var_msg;
	msg->msg_rpc->id = DART_ID;
	hdr = (struct hdr_update_var*) msg->msg_rpc->pad;
	strcpy(hdr->name, var->name);
	hdr->version = var->version;
	hdr->elem_size = var->elem_size;
	hdr->gdim = var->gdim;
	hdr->op = op;

    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

	return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
	ERROR_TRACE();
}

// TODO: the logic of this function is not clear
int cods_request_task(struct cods_task *t)
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

int cods_register_executor(int pool_id, int num_bucket, MPI_Comm comm)
{
    int err;
    int mpi_rank;
    MPI_Comm_rank(comm, &mpi_rank);
 
    bk_info.dart_id = DART_ID;
    bk_info.pool_id = pool_id;
    bk_info.num_bucket = num_bucket;
    bk_info.mpi_rank = mpi_rank;
    get_topology_information(&bk_info.topo_info);

    struct executor_register_info info;
    info.pool_id = bk_info.pool_id;
    info.dart_id = bk_info.dart_id;
    info.mpi_rank = bk_info.mpi_rank;
    info.topo_info = bk_info.topo_info;

    // Gather registration info of all task executors
    struct executor_register_info *info_tab = NULL;
    size_t info_tab_size = sizeof(*info_tab)*num_bucket;
    int root_rank = 0;
    if (mpi_rank == root_rank) {
        info_tab = malloc(info_tab_size);
    }
    int count = sizeof(struct executor_register_info);
    err = MPI_Gather(&info, count, MPI_BYTE, info_tab, count, MPI_BYTE,
                root_rank, comm);
    if (err != MPI_SUCCESS) {
        uloga("%s(): MPI_Gather() error %d.\n", __func__, err);
        goto err_out_free;
    } 

    // Root rank write gathered data to dataspaces
    if (info_tab) {
        int i;
        uloga("%s(): info_tab_size= %u\n", __func__, info_tab_size);
        // debug print
        //for (i = 0; i < num_bucket; i++) {
        //    uloga("%s(): info_tab[%d] dart_id= %d pool_id= %d node_id= %u\n",
        //        __func__, i, info_tab[i].dart_id, info_tab[i].pool_id,
        //        info_tab[i].topo_info.nid);
        //}

        // write to dataspaces
        char meta_data_name[NAME_MAXLEN];
        sprintf(meta_data_name, "reg_executor_pool_%d", pool_id);
        err = write_meta_data(meta_data_name, info_tab_size, info_tab);
        if (err < 0) {
            goto err_out_free;
        }
        free(info_tab); 

        // send message to workflow manager
        struct client_rpc_send_state send_state;
        struct msg_buf *msg;
        struct node_id *peer;
        struct hdr_register_resource *hdr;    
        peer = dc_get_peer(DART_DC, get_server_id());
        msg = msg_buf_alloc(DART_RPC_S, peer, 1);
        if (!msg)
            goto err_out;
        
        msg->msg_rpc->cmd = cods_reg_resource_msg;
        msg->msg_rpc->id = DART_ID;
        hdr = (struct hdr_register_resource*)msg->msg_rpc->pad;
        hdr->pool_id = pool_id;
        hdr->num_bucket = num_bucket;
        strcpy(hdr->meta_data_name, meta_data_name);
       
        err = client_rpc_send(dcg, peer, msg, &send_state);
        if (err < 0) {
            free(msg);
            goto err_out;
        }
    }

    return 0;
 err_out_free:
    if (info_tab) free(info_tab);
 err_out:
    return -1;
    uloga("%s(): ERROR!\n", __func__);
}

int cods_set_task_finished(struct cods_task *t)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_finish_task *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(DART_DC, get_server_id()); 
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_finish_task_msg;
    msg->msg_rpc->id = DART_ID;
    hdr= (struct hdr_finish_task *)msg->msg_rpc->pad;
    hdr->pool_id = bk_info.pool_id;
    hdr->tid = t->tid;

    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int cods_exec_task(struct task_descriptor *task_desc)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(DART_DC, get_server_id()); 
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_submit_task_msg;
    msg->msg_rpc->id = DART_ID;
    struct hdr_submit_task *hdr= (struct hdr_submit_task*)msg->msg_rpc->pad;
    memcpy(&hdr->task_desc, task_desc, sizeof(struct hdr_submit_task));
    hdr->src_dart_id = DART_ID;

    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    struct task_info *t = create_submitted_task(task_desc);
    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

int cods_wait_task_completion(uint32_t tid)
{
    int err = -ENOMEM;
    struct task_info *t = lookup_submitted_task(tid);
    if (!t) {
        uloga("ERROR %s: failed to find submitted task tid= %u\n",
            __func__,  tid);
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

int cods_get_task_status(uint32_t tid)
{
    int err = -ENOMEM;
    struct task_info *t = lookup_submitted_task(tid);
    if (!t) {
        uloga("ERROR %s: failed to find submitted task tid= %u\n",
            __func__, tid);
        return err;
    }

    int check_loop_cnt = 2;
    while (check_loop_cnt-- > 0) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }

    if (is_submitted_task_done(t)) {
        free_submitted_task(t);
        return 1;
    }
    return 0;
 err_out:
    ERROR_TRACE();
}

int cods_stop_framework()
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(DART_DC, get_server_id()); 
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_stop_framework_msg;
    msg->msg_rpc->id = DART_ID;

    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    return 0;
 err_out_free:
    if (msg) free(msg);
 err_out:
    ERROR_TRACE();
}

struct executor_pool_info* cods_get_executor_pool_info(int pool_id)
{
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_get_executor_pool_info *hdr;
    int err = -ENOMEM;

    peer = dc_get_peer(DART_DC, get_server_id()); 
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_get_executor_pool_info_msg;
    msg->msg_rpc->id = DART_ID;
    hdr = msg->msg_rpc->pad;
    hdr->pool_id = pool_id;
    hdr->src_dart_id = DART_ID;

    submitter.f_get_executor_pool_info = 0;
    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    while (!submitter.f_get_executor_pool_info) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out_free;
        }
        if (process_pending_msg()) goto err_out_free;
    }

    struct executor_pool_info* pool_info = submitter.pool_info;
    // reset
    submitter.f_get_executor_pool_info = 0;
    submitter.pool_info = NULL;

    // Note: node_executor_tab has been sorted by nid (at workflow manager).
    // Following code builds the compute node table of pool_info.
    int i, j;
    pool_info->num_node = 0;
    uint32_t cur_nid = ~0; // assume node id can never be ~0
    for (i = 0; i < pool_info->num_executor; i++) {
        if (cur_nid != pool_info->executor_tab[i].topo_info.nid) {
            cur_nid = pool_info->executor_tab[i].topo_info.nid;
            pool_info->num_node++;
        }
    }

    pool_info->node_tab = malloc(sizeof(struct compute_node)*pool_info->num_node);
    i = j = 0;
    while (i < pool_info->num_executor) {
        cur_nid = pool_info->executor_tab[i].topo_info.nid;
        pool_info->node_tab[j].topo_info = pool_info->executor_tab[i].topo_info;
        pool_info->node_tab[j].node_num_executor = 0;
        pool_info->node_tab[j].node_executor_tab = &(pool_info->executor_tab[i]);
        while (pool_info->executor_tab[i].topo_info.nid == cur_nid &&
                i < pool_info->num_executor) {
            i++;
            pool_info->node_tab[j].node_num_executor++;
        }
        j++;
    }

    return pool_info;
 err_out_free:
    if (msg) free(msg);
 err_out:
    uloga("%s(): failed with %d\n", __func__, err);
    return NULL;        
}

int cods_build_partition(struct executor_pool_info *pool_info)
{
    int err = -ENOMEM;
    submitter.f_build_partition = 0;

    // Write new partition information to dataspaces.
    char meta_data_name[NAME_MAXLEN];
    sprintf(meta_data_name, "executor_pool_%d_partition", pool_info->pool_id);
    err = write_meta_data(meta_data_name,
                    sizeof(struct executor_descriptor)*pool_info->num_executor,
                    pool_info->executor_tab);
    if (err < 0) {
        goto err_out;
    }

    // Send message to workflow manager.
    struct client_rpc_send_state send_state;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_build_partition *hdr;

    peer = dc_get_peer(DART_DC, get_server_id()); 
    msg = msg_buf_alloc(DART_RPC_S, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = cods_build_partition_msg;
    msg->msg_rpc->id = DART_ID;
    hdr = (struct hdr_build_partition*)msg->msg_rpc->pad;
    hdr->src_dart_id = DART_ID;
    hdr->pool_id = pool_info->pool_id;
    hdr->num_executor = pool_info->num_executor;
    strcpy(hdr->meta_data_name, meta_data_name);

    err = client_rpc_send(dcg, peer, msg, &send_state);
    if (err < 0) {
        goto err_out_free;
    }

    // Wait for the reply from master
    while (!submitter.f_build_partition) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }
    // reset
    submitter.f_build_partition = 0;
    uloga("%s(): done.\n", __func__);
    return 0;
err_out_free:
    if (msg) free(msg);
err_out:
    ERROR_TRACE();
}

void init_task_descriptor(struct task_descriptor *task_desc, int task_id, const char* conf_file)
{
    task_desc->location_hint = DEFAULT_PART_TYPE;
    task_desc->tid = task_id;
    strcpy(task_desc->conf_file, conf_file); 
    memset(&task_desc->data_hint, 0, sizeof(struct bbox));
    task_desc->data_hint.num_dims = 0;
}
