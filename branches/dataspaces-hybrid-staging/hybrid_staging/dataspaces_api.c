#include "dataspaces_api.h"
#include "dataspaces_internal_def.h"

#include <stdio.h>
#include <stdlib.h>
#include "debug.h"
#include "dart.h"
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

const int NUM_TRYS = 20;
const int RDMA_GET_CREDITS = 225;

struct intran_job {
	struct list_head job_entry;
	int type;
	int tstep;
};

static struct {
	struct	list_head * data_list;
	int	has_job;
	int	num_obj_recv;
	/* keep basic info of current running job */
	int type, tstep, num_obj;

	/* info for performing adaptive RDMA get */
	struct  list_head	desc_list;
	int	num_get_credits;
	size_t	bytes_recv;
	
	/* track data get time */
	double tm_st, tm_end;

	int cmp_type; //the component type of current process
	int f_done; //flag indicates that all insitu work is done
	
	/* track the number of function calls to process forwarded data desc from one job */
	int num_cb_call;
	struct	list_head	intran_job_list;
} g_info;

#ifdef HAVE_UGNI
static int process_event(struct dcg_space *dcg)
{
	int err;
	err = rpc_process_event_with_timeout(dcg->dc->rpc_s, 2);
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

static int get_app_num_peers(int appid)
{
	int count = 0;
	struct node_id *peer;
	int i;
	for (i=0; i < dc->peer_size; i++) {
		peer = dc_get_peer(dc, i);
		if ( peer->ptlmap.appid == appid )
			count++;
	}

	return count;
}

/*
 Get the dart id of the master peer in the specified app
*/
static int get_app_min_dartid(int appid)
{
	struct node_id *peer;
	int i, min_dartid;

	for (i=0; i < dc->peer_size; i++) {
		peer  = dc_get_peer(dc, i);
		if ( peer->ptlmap.appid == appid ) {
			return peer->ptlmap.id;
		}
	}

	return -1;
}


/*
  Completion callback function of RDMA get
*/
static int get_insitu_data_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct data_item *item = msg->private;

	//put data into data items list
	list_add_tail(&item->item_entry, g_info.data_list);
	
	//count the number of received insitu data objects 
	g_info.num_obj_recv++;

	/* */
	g_info.num_get_credits++;
	g_info.bytes_recv += item->desc.size;

	free(msg);
	return 0;
}

static int get_insitu_data(struct rdma_data_descriptor *rdma_desc)
{
	int err = -ENOMEM;
	struct msg_buf *msg;
	struct node_id *peer;
	struct data_item *item;

	item = malloc(sizeof(*item));
	if (!item) {
		uloga("%s(): failed to alloc memory.\n", __func__);
		goto err_out;
	}

	item->desc = rdma_desc->desc;
	item->buf = malloc(rdma_desc->desc.size);
	if (!item->buf) {
		free(item);
		goto err_out;
	}

	peer = dc_get_peer(dc, rdma_desc->dart_id);
	msg = msg_buf_alloc(dc->rpc_s, peer, 0);
	if (!msg) {
		free(item->buf);
		free(item);
		goto err_out;
	}

	msg->msg_data = item->buf;
	msg->size = rdma_desc->desc.size;
	msg->private = item;
	msg->cb = get_insitu_data_completion;

#ifdef HAVE_UGNI
	peer->mdh_addr = rdma_desc->mdh_addr;
	err = rpc_receive_direct(dc->rpc_s, peer, msg);
#endif
#ifdef HAVE_DCMF
	peer->cached_remote_memregion = &rdma_desc->mem_region;
	err = rpc_receive_direct(dc->rpc_s, peer, msg);
#endif
	if (err < 0) {
		free(item->buf);
		free(item);
		free(msg);
		goto err_out;
	}
	
	return 0;
err_out:
	ERROR_TRACE();
}

/*
  RPC callback function to process rr_data_desc msg from servers.  
*/
static int callback_rr_data_desc_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd) 
{
	int err = -ENOMEM;
	int myrank = dcg_get_rank(dcg);
	struct rdma_data_descriptor *rdma_desc =
			(struct rdma_data_descriptor *)cmd->pad;

	/* count the number of functin calls (for current job) */
	g_info.num_cb_call++;

	if ( 0 == g_info.has_job ) {
		struct intran_job *job;
		list_for_each_entry(job, &g_info.intran_job_list, struct intran_job,
				job_entry) {
			if (job->type == rdma_desc->desc.type &&
			    job->tstep == rdma_desc->desc.tstep ) {
				uloga("%s(): get duplicate intran job (%d,%d)\n",
					__func__, job->type, job->tstep);
				return 0;
			}
		}

		/*get a new job ... */
		g_info.num_obj = rdma_desc->desc.num_obj;
		g_info.type = rdma_desc->desc.type;
		g_info.tstep = rdma_desc->desc.tstep;
		g_info.has_job = 1;

		g_info.tm_st = timer_read(&timer);

		job = (struct intran_job*)malloc(sizeof(*job));
		job->type = g_info.type;
		job->tstep = g_info.tstep;
		list_add_tail(&job->job_entry, &g_info.intran_job_list);

		uloga("%s(): Bucket %d get job (%d,%d) from %d\n",
			__func__, myrank, g_info.type, g_info.tstep, cmd->id);
	} else {
		/*check if i'm getting obj for the same job*/
		if ( g_info.type != rdma_desc->desc.type ||
			g_info.tstep != rdma_desc->desc.tstep ) {
			uloga("%s(): error! Bucket %d should get obj desc for job (%d,%d), "
				"but get for job (%d,%d) from %d\n", 
				__func__, myrank, g_info.type, g_info.tstep,
				rdma_desc->desc.type, rdma_desc->desc.tstep,
				cmd->id);
			return 0;
		}

		if ( g_info.num_cb_call > g_info.num_obj) {
			uloga("%s(): Bucket %d  g_info.num_cb_call= %d "
				"rank= %d type= %d tstep= %d from %d\n",
				__func__, myrank, g_info.num_cb_call,
				rdma_desc->desc.rank, g_info.type, g_info.tstep, cmd->id);
		} 
	}
	
	//RDMA get data
	if ( g_info.num_get_credits > 0 ) {
		err = get_insitu_data(rdma_desc);
		if (err < 0)
			goto err_out;
		g_info.num_get_credits--;
	}
	else {
		uloga("No sufficient rdma get credits for (%d,%d)!\n",
			rdma_desc->desc.type, rdma_desc->desc.tstep);
		//No sufficient rdma get credits ...
		struct data_desc *desc = malloc(sizeof(*desc));
		desc->desc = *rdma_desc;
		list_add_tail(&desc->desc_entry, &g_info.desc_list);		
	}

	return 0; 
err_out:
	ERROR_TRACE(); 
}


/*
  RPC callback function to process insitu_unreg msg
*/
static int callback_insitu_unreg_msg(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	uloga("%s(): rank= %d get msg from %d\n",
		__func__, rpc_s->ptlmap.id, cmd->id);

	g_info.f_done = 1;
	
	return 0;
}

/*
 Send intran_req_job msg to hashed server.
*/
static int request_job() {
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	peer = dc_get_peer(dc, 0); //master srv has dart id as 0

	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = intran_req_job;
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

static int is_ready() 
{
	int err;

	/* perform one RDMA get when get credits is > 0 */
	if (g_info.num_get_credits > 0 ) {
		struct data_desc *desc, *tmp;
		list_for_each_entry_safe(desc, tmp, &g_info.desc_list, struct data_desc, desc_entry) {
			err = get_insitu_data(&desc->desc);
			if (0 == err) {
				list_del(&desc->desc_entry);
				free(desc);
				g_info.num_get_credits--;
			}

			break;
		}
	}
		
	if (g_info.has_job && (g_info.num_obj == g_info.num_obj_recv)) {
		g_info.tm_end = timer_read(&timer);
		return 1;
	}

	return 0;
}


/*
  Callback function after the completion of RDMA GET by remote bucket.
*/
static int obj_put_direct_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct obj_data *od = (struct obj_data *)msg->private;
	obj_data_free(od);
	free(msg);

	dcg->num_pending--;

	return 0;
}

/*
  Send data descriptor to hashed remote RR server. 
*/
static int obj_put_direct(struct obj_data *od, struct data_descriptor *desc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int peer_id, err = -ENOMEM;

	peer_id =  (dc->self->ptlmap.id + desc->tstep) % dc->num_sp;
	peer = dc_get_peer(dc, peer_id);

	msg = msg_buf_alloc(dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_data = od->data;
	msg->size = obj_data_size(&od->obj_desc);
	msg->cb = obj_put_direct_completion;
	msg->private = od;

	msg->msg_rpc->cmd = insitu_data_desc;
	msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

	memcpy(msg->msg_rpc->pad, desc, sizeof(struct data_descriptor));

	/* try to regain credits to send msg */
	while (peer->num_msg_at_peer <= 0) {
		err = process_event(dcg);
		if ( err < 0 )
			uloga("%s(): process_event() err\n", __func__);
	}

	/*
	uloga("%s(): Rank %d, msg->size=%u, job:type=%d tstep=%d num_obj=%d\n",
		__func__, dc->self->ptlmap.id, msg->size, desc->type, desc->tstep, desc->num_obj);
	*/

	err = rpc_send(dc->rpc_s, peer, msg);
	//err = rpc_send_v1(dc->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}

	dcg->num_pending++;

	return 0;
 err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
*
  Public APIs
*
*/

/* Initialize the dataspaces library. */
int ds_init(int num_peers, enum component_type type)
{
	int err = -ENOMEM;
	int appid = type;

	/* Init the g_info */
	g_info.cmp_type = type;
	g_info.f_done = 0;
	INIT_LIST_HEAD(&g_info.intran_job_list);

	if (dcg) {
		return 0;
	}

	if (type == IN_TRANSIT) {
		rpc_add_service(rr_data_desc, callback_rr_data_desc_msg); //add callback func for msg
		rpc_add_service(insitu_unreg, callback_insitu_unreg_msg);
	}

	dcg = dcg_alloc(num_peers, appid);	
	if (!dcg) {
		uloga("'%s()': failed to initialize.\n", __func__);
		return err;
	}

/* TODO: do we need to keep this? */
/*
        err = dcg_ss_info(dcg, &num_dims);
        if (err < 0) {
                uloga("'%s()': failed to obtain space info, err %d.\n",
                        __func__, err);
                return err;
        }
*/
	dc = dcg->dc;
	if (!dc) {
		uloga("%s(): failed to obtain dart client...err\n", __func__);
		return -1;
	}

	timer_init(&timer, 1);
	timer_start(&timer);

	return 0;
}

/* Finalize the dataspaces library. */
int ds_finalize()
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			__func__);
		return;
	}

	/* free the intran_job_list */
	struct intran_job *job, *t;
	list_for_each_entry_safe(job,t,&g_info.intran_job_list, struct intran_job, job_entry) {
		list_del(&job->job_entry);
		free(job);
	}

	if (g_info.cmp_type == IN_SITU ) {
		/* TODO: the calculation below may not work when have more than 2 applications */
		int num_peers_intransit = get_app_num_peers(IN_TRANSIT);
		int min_dartid = get_app_min_dartid(IN_TRANSIT);	
		int myrank = dcg_get_rank(dcg);
		/* check if all pending RDMA operations completed */
		while ( dcg->num_pending != 0 ) {
			err = process_event(dcg);
			if ( err < 0 ) {
				uloga("%s(): error= %d of process_event()\n",__func__, err);
				goto err_out;
			}
		}

		ds_do_barrier();

		/*
		  In-situ processes send insitu_unreg msgs to mapped in-transit processes
		*/
		if ( myrank < num_peers_intransit ) {
			struct msg_buf *msg;
			struct node_id *peer;
			peer = dc_get_peer(dc, min_dartid + myrank);

			msg = msg_buf_alloc(dc->rpc_s, peer, 1);
			if (!msg)
				goto err_out;

			msg->msg_rpc->cmd = insitu_unreg;
			msg->msg_rpc->id = dc->rpc_s->ptlmap.id;

			uloga("%s(): %d send unreg msg to %d\n",
				__func__, dc->rpc_s->ptlmap.id, min_dartid + myrank);
		
			err = rpc_send(dc->rpc_s, peer, msg);
			if (err < 0) {
				free(msg);
				goto err_out;
			}			
		}
		
	}
	
	dcg_free(dcg);
	dcg = 0;
	return 0;
err_out:
	dcg_free(dcg);
	dcg = 0;
	return -1;
}


/* Put/write memory block. */
int ds_put_obj_data(void * data, struct data_descriptor * desc)
{
	struct obj_data *od;
	int i, err, rank = dcg_get_rank(dcg); //TODO: or use desc->rank??

	/* prepare dataspaces level object descriptor */
	struct obj_descriptor odsc = {
		.version = desc->tstep, .owner = dc->rpc_s->ptlmap.id,
		.st = st,
		.size = desc->size,
		.bb = {.num_dims = num_dims,
			.lb.c = {rank, 0, 0},
			.ub.c = {rank, 0, 0}}};
	sprintf(odsc.name, "block-%d-%d-%d", 
			desc->type, desc->tstep, rank);

	/* copy and create dataspaces obj data */
	//od = obj_data_alloc_with_data(&odsc, data);
	od = obj_data_alloc_no_data(&odsc, data);
	if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n",
				__func__);
		return -ENOMEM;
	}

	/* register RDMA mem buffer, send rpc msg to hashed server */
	err = obj_put_direct(od, desc);
	if (err < 0) {
		obj_data_free(od);

		uloga("'%s()': failed with %d, can not put data object.\n",
				__func__, err);
		return err;
	}

	/* try to release registered RDMA mem buffers */
	i = 0;
	while ( i++ < NUM_TRYS ) {
		err  = process_event(dcg);
		if (err < 0)
			uloga("%s(): process_event() err\n",__func__);
	}

	return 0; 
}

/*
Request to get job from RR & Get/read memory blocks & Aggregate data and store them into a data_list. Output: the application type and head pointer of data_list.
*/
int ds_request_job(enum op_type *type, struct list_head *head)
{
	int err;

	/* init */
	g_info.data_list = head;
	g_info.num_obj_recv = 0;
	g_info.has_job = 0;
	g_info.type = 0;
	g_info.tstep = 0;
	g_info.num_obj = 0;	
	INIT_LIST_HEAD(&g_info.desc_list);
	g_info.num_get_credits = RDMA_GET_CREDITS;
	g_info.bytes_recv = 0;
	g_info.num_cb_call = 0;
	
	/* send intran_req_job_msg to RR master server */
	err = request_job();
	if ( err < 0 )
		return -1;

	while ( !g_info.f_done && !is_ready() ) {
		err = process_event(dcg);
		if ( err < 0 ) {
			ds_free_data_list(head);
			return err;
		}		
	}

	if (g_info.f_done)
		return -1;

	fprintf(stderr, "EVAL: type= %d tstep= %d kB_recv=%u"
		" get_data_time= %lf\n",
		 g_info.type, g_info.tstep, g_info.bytes_recv/1024,
		 g_info.tm_end-g_info.tm_st );

	g_info.data_list = NULL;

	//return operation type and data items list
	*type = g_info.type;
	return 0;	
}

int ds_free_data_list(struct list_head *head)
{
	//free data objs chained by this list
	struct data_item *item, *tmp;
	list_for_each_entry_safe(item,tmp,head,struct data_item,item_entry) {
		list_del(&item->item_entry);
		if (item->buf)
			free(item->buf);
		free(item);
	}

	return 0;
}

void ds_do_barrier()
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			__func__);
		return;
	}

	err = dcg_barrier(dcg);

	return;
}

int ds_rank()
{
	if (dcg)
		return dcg_get_rank(dcg);
	else return -1;
}
