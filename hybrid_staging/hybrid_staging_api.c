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

const int NUM_TRYS = 50;
const int RDMA_GET_CREDITS = 225;

struct parallel_job {
	struct list_head entry;
	int type;
	int tstep;
};

static struct {
	int f_get_task;
	/* keep basic info of current task */
	struct task_descriptor *current_task;

	/* flags */
	int f_done; //flag indicates that all insitu work is done
	int f_init_dspaces;

	/* track data get time */
	double tm_st, tm_end;

	enum worker_type workertype;
} g_info;

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
	g_info.f_get_task = 1;
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

	size_t size = g_info.current_task->num_input_vars * 
					sizeof(struct var_descriptor);
	g_info.current_task->input_vars = (struct var_descriptor *)malloc(size);
	msg->msg_data = g_info.current_task->input_vars;
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
	g_info.current_task->tid = hdr->tid;
	g_info.current_task->step = hdr->step;
	g_info.current_task->rank = hdr->rank;
	g_info.current_task->nproc = hdr->nproc;

	if (hdr->num_input_vars > 0) {
		g_info.current_task->num_input_vars = hdr->num_input_vars;
		if (fetch_input_vars_info(cmd) < 0) {
			g_info.current_task = NULL;
			return -1;
		}
	} else {
		g_info.current_task->num_input_vars = 0;
		g_info.current_task->input_vars = NULL;
		g_info.f_get_task = 1;
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

	g_info.f_done = 1;
	return 0;
}


static int request_task() {
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	peer = dc_get_peer(dc, 0); //master srv has dart id as 0
	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = hs_req_task_msg;
	msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

	err = rpc_send(dc->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}

	return 0;	
err_out:
	ERROR_TRACE();
}

/*
*
  Public APIs
*
*/

/* Initialize the dataspaces library. */
int hstaging_init(int num_peers, enum worker_type type, int appid)
{
	int err = -ENOMEM;

	/* Init the g_info */
	g_info.workertype = type;
	g_info.f_done = 0;
	g_info.f_init_dspaces = 0;

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
	
	g_info.f_init_dspaces = 1;
	return 0;
}

/* Finalize the dataspaces library. */
int hstaging_finalize()
{
	int err;

	if (!g_info.f_init_dspaces) {
		uloga("'%s()': library was not properly initialized!\n", __func__);
		return;
	}

	if ( g_info.workertype == hs_simulation_worker ) {
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

	if (!g_info.f_init_dspaces) {
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

	if (!g_info.f_init_dspaces) {
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

	g_info.f_get_task = 0;
	g_info.current_task = t;

	// Send hs_req_task_msg to master srv
	err = request_task();
	if (err < 0)
		return -1;

	while (!g_info.f_get_task && !g_info.f_done) {
		err = process_event(dcg);
		if (err < 0) {
			return err;
		}
	}	

	if (g_info.f_done) {
		return -1;
	}	

	g_info.current_task = NULL;
	return 0;
}
