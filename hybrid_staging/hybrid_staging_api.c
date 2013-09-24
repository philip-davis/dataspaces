#include "hybrid_staging_api.h"
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
	int f_done; //flag indicates that all insitu work is done
	int f_init_dspaces;

	/* track data get time */
	double tm_st, tm_end;

	enum worker_type workertype;
    enum hstaging_location_type location_type;
    int mpi_rank, num_bucket;
};
static struct bucket_info bk_info;

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

static int fetch_input_vars_info_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	bk_info.f_get_task = 1;
	free(msg);
	return 0;
}

static int fetch_input_vars_info(struct rpc_cmd *cmd)
{
	struct node_id *peer = dc_get_peer(dc, cmd->id);
	struct msg_buf *msg;
	int err = -ENOMEM;

	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	size_t size = bk_info.current_task->num_input_vars * 
					sizeof(struct var_descriptor);
	bk_info.current_task->input_vars = (struct var_descriptor *)malloc(size);
	msg->msg_data = bk_info.current_task->input_vars;
	msg->size = size;
	msg->cb = fetch_input_vars_info_completion;

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
    bk_info.current_task->color = hdr->color;
	bk_info.current_task->rank = hdr->rank_hint;
	bk_info.current_task->nproc = hdr->nproc_hint;

	if (hdr->num_input_vars > 0) {
		bk_info.current_task->num_input_vars = hdr->num_input_vars;
		if (fetch_input_vars_info(cmd) < 0) {
			bk_info.current_task = NULL;
			return -1;
		}
	} else {
		bk_info.current_task->num_input_vars = 0;
		bk_info.current_task->input_vars = NULL;
		bk_info.f_get_task = 1;
	}

	return 0;
}

/*
  RPC callback function to process msg
*/
static int callback_staging_exit_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	uloga("%s(): rank= %d get msg from %d\n",
		__func__, rpc_s->ptlmap.id, cmd->id);

	bk_info.f_done = 1;
	return 0;
}

/*
*
  Public APIs
*
*/

/* Initialize the dataspaces library. */
int hstaging_init(int num_peers, int appid, enum worker_type workertype)
{
	int err = -ENOMEM;

	/* Init the bk_info */
	bk_info.workertype = workertype;
	bk_info.f_done = 0;
	bk_info.f_init_dspaces = 0;

	if (dcg) {
		return 0;
	}

	// TODO: does it matter to place rpc_add_service after dspaces_init()?
	rpc_add_service(staging_exit, callback_staging_exit_msg);
	rpc_add_service(hs_exec_task_msg, callback_hs_exec_task);

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
		return;
	}

	if ( bk_info.workertype == hs_simulation_worker ) {
		// check if all pending RDMA operations completed
		dimes_put_sync_all();

		// TODO: make it more scalable...
		// err = dcg_barrier(dcg);
		int dart_rank = dcg_get_rank(dcg);
		if ( 0 == dart_rank ) {
			struct msg_buf *msg;
			struct node_id *peer;
			peer = dc_get_peer(dc, 0); // Send unreg msg to master srv

			msg = msg_buf_alloc(dc->rpc_s, peer, 1);
			if (!msg)
				goto err_out;

			msg->msg_rpc->cmd = insitu_unreg;
			msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

			uloga("%s(): %d send unreg msg\n",
				__func__, dc->rpc_s->ptlmap.id);
		
			err = rpc_send(dc->rpc_s, peer, msg);
			if (err < 0) {
				free(msg);
				goto err_out;
			}
		}			
	}
	
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

	//dspaces_lock_on_read(var_name, comm);
	err = dimes_get(var_name, ver, size, xl, yl, zl, xu, yu, zu, data);
	if (err < 0) {
		goto err_out;
	}
	//dspaces_unlock_on_read(var_name, comm);

	return 0;
err_out:
	ERROR_TRACE();
}

int hstaging_update_var(struct var_descriptor *var_desc, enum hstaging_update_var_op op)
{
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_update_var *hdr;
	int peer_id, err = -ENOMEM;

	peer_id = 0; // all update info is sent to master srv
	peer = dc_get_peer(dc, peer_id);

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

	err = rpc_send(dc->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

int hstaging_request_task(struct task_descriptor *t)
{
	int err;

	bk_info.f_get_task = 0;
	bk_info.current_task = t;

	while (!bk_info.f_get_task && !bk_info.f_done) {
		err = process_event(dcg);
		if (err < 0) {
			return err;
		}
	}	

	if (bk_info.f_done) {
		return -1;
	}	

	bk_info.current_task = NULL;
	return 0;
}

int hstaging_register_bucket_resource(enum hstaging_location_type loc_type, int num_bucket, int mpi_rank)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;   

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_reg_resource_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    struct hdr_register_resource *hdr = (struct hdr_register_resource *)
            msg->msg_rpc->pad;
    hdr->location_type = loc_type;
    hdr->num_bucket = num_bucket;
    hdr->mpi_rank = mpi_rank;

    bk_info.location_type = loc_type;
    bk_info.num_bucket = num_bucket;
    bk_info.mpi_rank = mpi_rank;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
err_out:
    ERROR_TRACE();     
}

int hstaging_set_task_done(struct task_descriptor *t)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(dc, 0); //master srv has dart id as 0
    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (!msg)
        goto err_out;

    msg->msg_rpc->cmd = hs_task_done_msg;
    msg->msg_rpc->id = dc->rpc_s->ptlmap.id;
    struct hdr_task_done *hdr= (struct hdr_task_done *)msg->msg_rpc->pad;
    hdr->tid = t->tid;
    hdr->step = t->step;
    hdr->color = t->color;
    hdr->location_type = bk_info.location_type;

    err = rpc_send(dc->rpc_s, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    return 0;
err_out:
    ERROR_TRACE();
}
