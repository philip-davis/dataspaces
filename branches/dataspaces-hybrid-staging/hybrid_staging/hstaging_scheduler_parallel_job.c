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

#include "hybrid_staging_internal_def.h"
#include "mpi.h"


const int BK_TAB_SIZE = 1024;
const int BK_PER_JOB = 4;

static int num_sp;
static int num_cp;
static char *conf;
static struct dart_server *ds;
static struct ds_gspace *dsg;

enum job_state {
	pending = 1,
	running,
	finish
};

struct job_id {
	int type;
	int tstep;
};

/*
 TODO: this file will be merged into ds_gspace.c ??
*/
// Header for messages that request bucket allocation from master srv
struct hdr_req_allocation {
	struct job_id jid;
} __attribute__((__packed__));

// Header for messages that reply bucket allocation requests
struct hdr_req_allocation_reply {
	int flag;
	struct job_id jid;
	int num_bk;
} __attribute__((__packed__));

struct allocation_request {
	struct list_head req_entry;
	int	dart_id; /* which slave srv */
};

struct job {
	struct list_head job_entry;
	struct job_id id;
	enum job_state state;
	int num_finish; // number of buckets that finish the execution
	int	num_bk; // number of buckets required for job execution
	int *bk_tab;
	struct list_head req_list;
	struct list_head data_desc_list; 
};

struct bucket {
	struct	list_head	bucket_entry;
	int	dart_id;
	struct	job_id	current_jid;
};

struct workflow_state {
	unsigned int f_done;
	unsigned int f_send_exit_msg;
};

static struct list_head jobq;
static struct list_head idle_bk_list;
static struct list_head run_bk_list;
static struct workflow_state state;

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

static struct job_id extract_job_id(struct data_descriptor *desc)
{
	struct job_id temp;
	temp.type = desc->type;
	temp.tstep = desc->tstep;

	return temp;
}

static inline int equal_job_id(struct job_id *a, struct job_id *b)
{
	return (a->type == b->type) && (a->tstep == b->tstep);
}

static int idle_bk_list_count()
{
	int cnt = 0;
	struct bucket *bk;
	list_for_each_entry(bk, &idle_bk_list, struct bucket, bucket_entry) {
		cnt++;
	}

	return cnt;
}

static struct bucket * idle_bk_list_remove_head()
{
	struct bucket *bk,*t;
	list_for_each_entry_safe(bk,t,&idle_bk_list,struct bucket,bucket_entry) {
		list_del(&bk->bucket_entry);
		return bk;
	}

	return NULL;
}

static inline void idle_bk_list_add(struct bucket *bk)
{
	list_add_tail(&bk->bucket_entry, &idle_bk_list);
}

static int idle_bk_list_free()
{
	uloga("%s(): idle_bk_list_count returns %d\n", __func__,
			idle_bk_list_count());

	struct bucket *bk, *t;
	list_for_each_entry_safe(bk,t,&idle_bk_list,struct bucket,bucket_entry) {
		list_del(&bk->bucket_entry);
		free(bk);
	}

	return 0;
}

static int run_bk_list_count()
{
	int cnt = 0;
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, bucket_entry) {
		cnt++;
	}

	return cnt;
}

static struct bucket* run_bk_list_lookup(int dart_id)
{
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, bucket_entry) {
		if ( dart_id == bk->dart_id )
			return bk;
	}

	return NULL;	
}

static void run_bk_list_remove(int dart_id)
{
	struct bucket *bk, *tmp;
	list_for_each_entry_safe(bk, tmp, &run_bk_list, struct bucket,
			bucket_entry) {
		if ( dart_id ==  bk->dart_id ) {
			list_del(&bk->bucket_entry);
			free(bk);
			return;
		}
	}
}

static inline void run_bk_list_add(struct bucket *bk)
{
	list_add_tail(&bk->bucket_entry, &run_bk_list);
}

static int run_bk_list_free()
{
	uloga("%s(): run_bk_list_count returns %d\n", __func__,
			run_bk_list_count());

	struct bucket *bk, *t;
	list_for_each_entry_safe(bk,t,&run_bk_list,struct bucket,bucket_entry) {
		list_del(&bk->bucket_entry);
		free(bk);
	}

	return 0;
}

static int jobq_count()
{
	int cnt = 0;
	struct job *j;
	list_for_each_entry(j, &jobq, struct job, job_entry) {
		cnt++;
	}

	return cnt;
}

static struct job * jobq_lookup(struct job_id *jid)
{
	struct job *j;
	list_for_each_entry(j,&jobq,struct job,job_entry) {
		if ( equal_job_id(jid, &j->id) )
			return j;
	}

	return NULL;
}

static struct job * jobq_create_job(struct job_id *jid)
{
	struct job *j = malloc(sizeof(*j));
	INIT_LIST_HEAD(&j->req_list);
	INIT_LIST_HEAD(&j->data_desc_list);
	j->id = *jid;
	j->state = pending; 
	j->num_finish = 0;
	j->num_bk = BK_PER_JOB;
	j->bk_tab = NULL;
	list_add_tail(&j->job_entry, &jobq);	
	
	return j;
}

static inline int job_is_pending(struct job *j) {
	return j->state == pending;
}

static inline int job_is_running(struct job *j) {
	return j->state == running;
}

static inline int job_is_finish(struct job *j) {
	return j->state == finish;
}

// TODO: very tricky implementation
static void job_update_state(struct job *j, int dart_id)
{
	if (j->state == pending) {
		uloga("%s(): j->state == pending should not happen\n", __func__);
	}

	if (j->state == running) {
		j->num_finish++;
		if (j->num_finish == j->num_bk)
			j->state = finish;
	}
}

static void job_allocate_bk(struct job *j)
{
	if (j->state != pending) {
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
	for (i = 0; i < j->num_bk;) {
		bk = idle_bk_list_remove_head();
		bk->current_jid = j->id;
		j->bk_tab[i++] = bk->dart_id;
		// Update run_bk_list
		// TODO: why has to use two functions?
		run_bk_list_remove(bk->dart_id);
		run_bk_list_add(bk);
	}

	// Update job state
	j->state = running;

	uloga("%s(): assign job (%d, %d) to %d buckets\n",
		__func__, j->id.type, j->id.tstep, j->num_bk);

	return;	
}

static void job_cache_data_desc(struct job *j, struct rdma_data_descriptor *rdma_desc)
{
	struct data_desc *d = malloc(sizeof(*d));
	d->desc = *rdma_desc;
	list_add_tail(&d->desc_entry, &j->data_desc_list);
}

static void job_remove_cached_data_desc(struct job *j)
{
	struct data_desc *d, *t;
	list_for_each_entry_safe(d,t,&j->data_desc_list,struct data_desc,
		desc_entry)
	{
		list_del(&d->desc_entry);
		free(d);
	}
}

static void job_cache_allocation_req(struct job *j, int dart_id)
{
	struct allocation_request *req = malloc(sizeof(*req));
	req->dart_id = dart_id;
	list_add_tail(&req->req_entry, &j->req_list);
}

static void job_remove_cached_allocation_req(struct job *j)
{
	struct allocation_request *req, *t;
	list_for_each_entry_safe(req,t,&j->req_list,struct allocation_request,
		req_entry)
	{
		list_del(&req->req_entry);
		free(req);
	}
}

/*
static inline int jobq_empty()
{
	return list_empty(&jobq);
}

static struct job * jobq_remove_head()
{
	struct job *j, *t;
	list_for_each_entry_safe(j,t,&jobq,struct job,job_entry) {
		list_del(&j->job_entry);
		return j;
	}
	
	return NULL;
}
*/

static int jobq_free()
{
	uloga("%s(): jobq_count returns %d\n", __func__, jobq_count());

	struct job *j, *t;
	list_for_each_entry_safe(j,t,&jobq,struct job,job_entry) {
		list_del(&j->job_entry);
		if (j->bk_tab)
			free(j->bk_tab);
		job_remove_cached_data_desc(j);
		job_remove_cached_allocation_req(j);	
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

static int reply_allocation_request_hit_completion(struct rpc_server *rpc_s,
	struct msg_buf *msg)
{
	free(msg->msg_data);
	free(msg);

	return 0;	
}

static int reply_allocation_request_hit(struct job *j, int dart_id)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int *bk_tab, i, err = -ENOMEM;

	bk_tab = malloc(sizeof(int) * BK_TAB_SIZE);
	if (!bk_tab)
		goto err_out;

	peer = ds_get_peer(ds, dart_id);
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	for (i = 0; i < j->num_bk; i++) {
		bk_tab[i] = j->bk_tab[i];	
	}	

	msg->msg_data = (void*) bk_tab;
	msg->size = sizeof(int) * BK_TAB_SIZE;
	msg->cb = reply_allocation_request_hit_completion;

	msg->msg_rpc->cmd = rr_req_allocation_reply;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	struct hdr_req_allocation_reply *hdr =
		(struct hdr_req_allocation_reply *)msg->msg_rpc->pad;
	hdr->flag = 1;
	hdr->jid = j->id;
	hdr->num_bk = j->num_bk;
	
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

static int send_allocation_request(struct job_id *jid)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;
	
	peer = ds_get_peer(ds, 0); //master srv has dart id as 0
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;
	
	msg->msg_rpc->cmd = rr_req_allocation;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	struct hdr_req_allocation *hdr = 
		(struct hdr_req_allocation*)msg->msg_rpc->pad;
	hdr->jid = *jid;

	err = rpc_send(ds->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static int map_data_to_bk(struct job *j, struct rdma_data_descriptor *rdma_desc)
{
	// index value is in the range of 0 ~ j->num_bk-1
	int index = rdma_desc->desc.rank % j->num_bk;
	int dart_id = j->bk_tab[index];

	// Update the num_obj_to_receive field for peer
	int num_obj = rdma_desc->desc.num_obj;
	int quotient = num_obj / j->num_bk;
	int remainder = num_obj % j->num_bk;
	if (remainder == 0) {
		rdma_desc->num_obj_to_receive = quotient;
	} else {
		if ( index < remainder )
			rdma_desc->num_obj_to_receive = quotient + 1;
		else
			rdma_desc->num_obj_to_receive = quotient;
	}

	return dart_id;
}

static int send_rdma_data_desc(struct job *j, struct rdma_data_descriptor *rdma_desc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	int dart_id = map_data_to_bk(j, rdma_desc);
	peer = ds_get_peer(ds, dart_id);

	// for debug
	//uloga("%s(): #%d send obj desc for job (%d,%d) to #%d\n",
	//	__func__, ds->rpc_s->ptlmap.id, rdma_desc->desc.type,
	//	rdma_desc->desc.tstep, dart_id);

	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = rr_data_desc;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	memcpy(msg->msg_rpc->pad, rdma_desc, sizeof(struct rdma_data_descriptor));

	err = rpc_send(ds->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}


static int process_workflow_state()
{
	struct bucket *bk;
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (state.f_send_exit_msg) return 0;

	if (state.f_done && 0 == run_bk_list_count()) {
		list_for_each_entry(bk, &idle_bk_list, struct bucket, bucket_entry) {
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

	// 1. process pending jobs (if any) in the queue
	list_for_each_entry_safe(j, t, &jobq, struct job, job_entry) {
		if (job_is_pending(j)) {
			job_allocate_bk(j);
		}
	}	

	// 2. process running jobs (if any) in the queue
	list_for_each_entry_safe(j, t, &jobq, struct job, job_entry) {
		if (job_is_running(j)) {
			// handle data_desc_list
			struct data_desc *desc, *desc_tmp;
			list_for_each_entry_safe(desc,desc_tmp,&j->data_desc_list,
					struct data_desc,desc_entry) {
				list_del(&desc->desc_entry);
				err = send_rdma_data_desc(j, &desc->desc);	
				free(desc);

				if ( err < 0 ) {
					uloga("%s(): send_rdma_data_desc error\n", __func__);
				}

				break; // proceed to process next running job
			}

			// handle req_list
			struct allocation_request *req, *req_tmp;
			list_for_each_entry_safe(req,req_tmp,&j->req_list,
					struct allocation_request,req_entry) {
				list_del(&req->req_entry);
				err = reply_allocation_request_hit(j, req->dart_id);
				free(req);

				if (err < 0) {
					uloga("%s(): reply_allocation_request_hit error\n", __func__);
				}
			}
		}
	}

	// 3. process finish jobs (if any) in the queue
	list_for_each_entry_safe(j, t, &jobq, struct job, job_entry) {
		if (job_is_finish(j)) {
			list_del(&j->job_entry);

			if (!list_empty(&j->data_desc_list) ||
				!list_empty(&j->req_list)) {
				uloga("%s(): job not empty yet? should not happen\n", __func__);
			}

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

/*
  RPC callback function for job request from bucket
*/
// TODO: callback_intran_req_job -> callback_bk_req_job, the bucket can resides at either in-situ or in-transit...
static int callback_intran_req_job(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct bucket *bk;
	struct job *j;
	int dart_id;
	int err = -ENOMEM;

	dart_id = cmd->id;
	bk = run_bk_list_lookup(dart_id);
	if (bk) {
		j = jobq_lookup(&bk->current_jid);
		job_update_state(j, dart_id);
	} else {
		bk = (struct bucket*) malloc(sizeof(*bk));
		bk->dart_id = dart_id;
		idle_bk_list_add(bk);	
		uloga("%s(): should call idle_bk_list_add for #%d once\n",
			__func__, dart_id);
	}

	print_rr_count();

	return 0;
}

static int fetch_allocation_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	int *bk_tab = (int*) msg->msg_data;
	struct job* j = (struct job*) msg->private;
	int i;
	j->bk_tab = (int*)malloc(j->num_bk*sizeof(int));
	for (i = 0; i < j->num_bk; i++)
		j->bk_tab[i] = bk_tab[i];

	// Update the job state
	j->state = running;

	free(msg->msg_data);
	free(msg);

	return 0;
}

static int fetch_allocation(struct job *j, struct rpc_cmd *cmd)
{
	struct node_id *peer = ds_get_peer(ds, 0);
	struct msg_buf *msg;
	int err = -ENOMEM;

	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->private = j;
	msg->msg_data = malloc(sizeof(int) * BK_TAB_SIZE);
	msg->size = sizeof(int) * BK_TAB_SIZE;
	msg->cb = fetch_allocation_completion;

	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(ds->rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	if (err == 0)
		return 0;

	free(msg->private);
	free(msg);
 err_out:
	ERROR_TRACE();
}

/*
  RPC callback function for reply msg from master server
*/
static int callback_rr_req_allocation_reply(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_req_allocation_reply *hdr = (struct hdr_req_allocation_reply *)cmd->pad;
	struct bucket *bk;
	struct job *j;
	int err;

	if (!hdr->flag) {
		uloga("%s(): hdr->flag=%d should not happen...\n", hdr->flag, __func__);
		return -1;
	}

	j = jobq_lookup(&hdr->jid);
	if (!j) {
		uloga("%s(): jobq_lookup failed should not happen...\n", __func__);
		return -1;
	}

	j->num_bk = hdr->num_bk;
	err = fetch_allocation(j, cmd);
	if (err < 0)
		return -1;

	return 0;
}


/*
  RPC callback function for bk allocation request from slave servers
*/
static int callback_rr_req_allocation(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_req_allocation *hdr =
		(struct hdr_req_allocation*) cmd->pad;
	int	dart_id = cmd->id;
	struct job_id jid = hdr->jid;
	struct job *j;

	struct bucket_request *bk_req;
	int err;

	j = jobq_lookup(&jid);
	if (j == NULL) {
		j = jobq_create_job(&jid);
		job_allocate_bk(j);
	}

	if (job_is_running(j)) {
		err = reply_allocation_request_hit(j, dart_id);
	} else {
		job_cache_allocation_req(j, dart_id);
		err = 0;
	}

	return err;
}

/*
 RPC callback function for incoming in-situ data descriptors msg (slave server side)
*/
static int process_insitu_data_desc_slave(struct rpc_server *rpc_s, struct rdma_data_descriptor *rdma_desc)
{
	int err = -ENOMEM;
	struct bucket *bk;
	struct job_id jid = extract_job_id(&rdma_desc->desc);
	struct job *j;

	j = jobq_lookup(&jid);
	if (j == NULL) {
		j = jobq_create_job(&jid);
		send_allocation_request(&jid);
	}

	if (job_is_running(j)) {
		err = send_rdma_data_desc(j, rdma_desc);
		return err;
	} else {
		job_cache_data_desc(j, rdma_desc);
		return 0;
	}
}

/*
 RPC callback function for incoming in-situ data descriptors msg (master server side)
*/
static int process_insitu_data_desc_master(struct rpc_server *rpc_s, struct rdma_data_descriptor *rdma_desc)
{
	int err = -ENOMEM;
	struct bucket *bk;
	struct job_id jid = extract_job_id(&rdma_desc->desc);
	struct job *j;

	j = jobq_lookup(&jid);
	if (j == NULL) {
		j = jobq_create_job(&jid);
		job_allocate_bk(j);
	}

	if (job_is_running(j)) {
		err = send_rdma_data_desc(j, rdma_desc);
		return err;
	} else {
		job_cache_data_desc(j, rdma_desc);
		return 0;
	}
}

static int callback_insitu_data_desc(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct data_descriptor *data_desc = (struct data_descriptor *)cmd->pad;
	
	struct rdma_data_descriptor rdma_desc;
	rdma_desc.desc  = *data_desc;
#ifdef HAVE_UGNI
	rdma_desc.mdh_addr = cmd->mdh_addr;
#endif
#ifdef HAVE_DCMF
	memcpy(&rdma_desc.mem_region, &cmd->mem_region, sizeof(DCMF_Memregion_t));
#endif
	rdma_desc.dart_id = cmd->id; //TODO: important!!

	// for debug
	// uloga("%s(): #%d get obj desc for job (%d,%d) from #%d\n",
	// 	__func__, rpc_s->ptlmap.id, data_desc->type, data_desc->tstep, cmd->id);

	if (is_master())
		process_insitu_data_desc_master(rpc_s, &rdma_desc);
	else process_insitu_data_desc_slave(rpc_s, &rdma_desc);

	return 0;
}

static int callback_insitu_unreg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	state.f_done = 1;
	return 0;
}

int hstaging_scheduler_parallel_init()
{
	dsg = dsg_alloc(num_sp, num_cp, conf);
	if (!dsg) {
		return -1;
	}

	ds = dsg->ds;

	rpc_add_service(insitu_data_desc, callback_insitu_data_desc);
	rpc_add_service(intran_req_job, callback_intran_req_job);
	rpc_add_service(rr_req_allocation, callback_rr_req_allocation);
	rpc_add_service(rr_req_allocation_reply,
				callback_rr_req_allocation_reply);
	rpc_add_service(insitu_unreg, callback_insitu_unreg);

	// Init
	jobq_init();
	idle_bk_list_init();
	run_bk_list_init();

	state.f_done = 0;
	state.f_send_exit_msg = 0;

	return 0;
}

int hstaging_scheduler_parallel_run()
{
	int err;

	while (!dsg_complete(dsg)) {
		err = dsg_process(dsg);
		if (err < 0) {
			/* If there is an error on the execution path,
			   I should stop the server. */

			dsg_free(dsg);

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
	dsg_barrier(dsg);
	dsg_free(dsg);

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
