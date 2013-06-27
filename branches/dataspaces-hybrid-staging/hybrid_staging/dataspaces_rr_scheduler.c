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

#include "dataspaces_internal_def.h"
#include "mpi.h"

static int num_sp;
static int num_cp;
static char *conf;
static struct dart_server *ds;
static struct ds_gspace *dsg;

struct job_id {
	int type;
	int tstep;
};

/*
 TODO: this file will be merged into ds_gspace.c ??
*/
struct hdr_req_bk_reply {
	int	flag;
	struct	job_id	jid;
	int	bk_dart_id;
};

struct bucket_request {
	struct list_head req_entry;
	int	dart_id; /*which slave srv*/
};

struct job {
	struct list_head job_entry;
	struct job_id id;
	struct list_head req_list;
	struct list_head data_desc_list; 
};

struct bucket {
	struct list_head	bucket_entry;
	int			dart_id; /*which peer*/
	struct	job_id		current_jid;
};

static struct list_head jobq;
static struct list_head idle_bk_list;
static struct list_head run_bk_list;
static struct list_head pending_jobq;

static inline int is_master()
{
	return ( 0 == ds->rpc_s->ptlmap.id );
}

/* operations on the jobq, idle_bk_list, run_bk_list */
static inline void jobq_init()
{
	INIT_LIST_HEAD(&jobq);
	INIT_LIST_HEAD(&pending_jobq);
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

static int run_bk_list_count()
{
	int cnt = 0;
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, bucket_entry) {
		cnt++;
	}

	return cnt;
}

static struct bucket * run_bk_list_lookup(struct job_id *jid)
{
	struct bucket *bk;
	list_for_each_entry(bk, &run_bk_list, struct bucket, bucket_entry) {
		if ( equal_job_id(jid, &bk->current_jid) )
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

static struct job * jobq_add_job(struct job_id *jid)
{
	struct job *j = malloc(sizeof(*j));
	INIT_LIST_HEAD(&j->req_list);
	INIT_LIST_HEAD(&j->data_desc_list);
	j->id = *jid;
	list_add_tail(&j->job_entry, &jobq);	
	
	return j;
}

static inline void jobq_cache_data_desc(struct job *j, struct rdma_data_descriptor *rdma_desc)
{
	struct data_desc *d = malloc(sizeof(*d));
	d->desc = *rdma_desc;
	list_add_tail(&d->desc_entry, &j->data_desc_list);
}

static inline void jobq_cache_bk_req(struct job *j, struct bucket_request *bk_req)
{
	list_add_tail(&bk_req->req_entry, &j->req_list);
} 

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

static  int idle_bk_list_count()
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


/************/
static int timestamp_ = 0;
void print_rr_count()
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

static int reply_bucket_request_hit(int dart_id, struct bucket *bk)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	peer = ds_get_peer(ds, dart_id);
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = rr_req_bk_reply;
	msg->msg_rpc->id = ds->rpc_s->ptlmap.id;
	struct hdr_req_bk_reply *hdr = (struct hdr_req_bk_reply *)msg->msg_rpc->pad;
	hdr->flag = 1;
	hdr->jid = bk->current_jid;
	hdr->bk_dart_id = bk->dart_id;

	err =rpc_send(ds->rpc_s, peer, msg);
        if (err < 0) {
                free(msg);
                goto err_out;
        }

        return 0;
err_out:
        ERROR_TRACE();

}

static int send_bucket_request(struct rdma_data_descriptor *rdma_desc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;
	
	peer = ds_get_peer(ds, 0); //master srv has dart id as 0
	msg = msg_buf_alloc(ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;
	
	msg->msg_rpc->cmd = rr_req_bk;
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

static int send_rdma_data_desc(struct bucket *bk, struct rdma_data_descriptor *rdma_desc)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	/*
	uloga("%s(): #%d send obj desc for job (%d,%d) to #%d\n",
		__func__, ds->rpc_s->ptlmap.id, rdma_desc->desc.type,
		rdma_desc->desc.tstep, bk->dart_id);
	*/

	peer = ds_get_peer(ds, bk->dart_id);
	
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

/*
*/
static int send_pending_jobq_desc()
{
	struct job *j, *t;
	struct bucket *bk;
	struct data_desc *desc, *tmp;
	struct node_id *peer;
	int err;

	list_for_each_entry_safe(j, t, &pending_jobq, struct job, job_entry) {
		bk = run_bk_list_lookup(&j->id);
		if (bk) {
			list_for_each_entry_safe(desc,tmp,&j->data_desc_list,
					struct data_desc,desc_entry) {
				peer = ds_get_peer(ds, bk->dart_id);
				if (peer->num_msg_at_peer > 0) {
					list_del(&desc->desc_entry);	
					err = send_rdma_data_desc(bk, &desc->desc);
					free(desc);

					if ( err < 0 ) {
						printf("%s(): error\n", __func__);
					}
				} 
				
				break;
			}

			if (list_empty(&j->data_desc_list) && list_empty(&j->req_list)) {
				list_del(&j->job_entry);
				free(j);
			}
		}
	}

	return 0;
}


/*
  RPC callback function for job request from in-transit bucket
*/
static int callback_intran_req_job(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	int err = -ENOMEM;
	struct bucket *bk = malloc(sizeof(struct bucket));	
	struct job *job;

	bk->dart_id = cmd->id;

	/* check if job queue is empty */
	if ( jobq_empty() ) {
		/* yes.*/
		/* remove if previously exisit in run_bk_list */
		run_bk_list_remove(bk->dart_id);

		/* update idle_bk_list */
		idle_bk_list_add(bk);

		/*
		uloga("%s(): get intran_req_job msg from bucket= %d, jobq empty\n",
			__func__, bk->dart_id);
		*/
	} 
	else {
		/* no. */
		/*get job from job queue*/
		job = jobq_remove_head();

		/*update run_bk_list*/
		bk->current_jid = job->id;
		run_bk_list_remove(bk->dart_id);
		run_bk_list_add(bk);

   	        uloga("%s(): assign job (%d, %d) to bucket %d\n",
                        __func__, job->id.type, job->id.tstep, bk->dart_id);

		/*process req_list of the job */
		struct bucket_request *bk_req,*tmp1;
		list_for_each_entry_safe(bk_req,tmp1,&job->req_list,
				struct bucket_request,req_entry) {
			list_del(&bk_req->req_entry);
			err = reply_bucket_request_hit(bk_req->dart_id, bk);
			free(bk_req);
		}

		/* add the job to the pending job queue */
		list_add_tail(&job->job_entry, &pending_jobq);	
	}

	return 0;
}

/*
  RPC callback function for reply msg from master server
*/
static int callback_rr_req_bk_reply(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_req_bk_reply *hdr = (struct hdr_req_bk_reply *)cmd->pad;
	struct bucket *bk;
	struct job *job;
	int err;

	if (hdr->flag) {
		//Hit
		bk = malloc(sizeof(struct bucket));
		bk->dart_id = hdr->bk_dart_id;
		bk->current_jid = hdr->jid;
		
		/*update run_bk_list*/
		run_bk_list_remove(bk->dart_id);
		run_bk_list_add(bk);

		// search job queue for previously cached job
		job = jobq_lookup(&bk->current_jid);

		if (!job) {
			uloga("%s() error failed to find job...\n", __func__);
			return -1;
		}

		// remove the job from job queue and add to pending queue
		list_del(&job->job_entry);
		list_add_tail(&job->job_entry, &pending_jobq);
	} else {
		uloga("%s(): not possible...\n", __func__);
	}
	
	return 0;
}


/*
  RPC callback function for bucket request from slave servers
*/
static int callback_rr_req_bk(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct rdma_data_descriptor *rdma_desc = (struct rdma_data_descriptor *)cmd->pad;
	int	dart_id = cmd->id;
	struct job_id jid = extract_job_id(&rdma_desc->desc);
	struct job *job;
	struct bucket_request *bk_req;
	int err;

	// jid in job queue ? 
	job = jobq_lookup(&jid);
	if (job) {
		/* Yes, then add bucket request to req_list of job  */
		bk_req = malloc(sizeof(struct bucket_request));
		bk_req->dart_id = dart_id;
		jobq_cache_bk_req(job, bk_req);

		/* Reply miss msg to slave server */
		//err = reply_bucket_request_miss();
	} 
	else {
		/* working bk in run_bk_list (for job id)? */
		struct bucket *bk = run_bk_list_lookup(&jid);
		if ( bk ) {
			err = reply_bucket_request_hit(dart_id, bk);	
			return err;
		}

		bk = idle_bk_list_remove_head();
		if ( !bk ) {
			/* create new job queue entry */
			job = jobq_add_job(&jid);
			
			/* add bucket request to req_list of job */
			bk_req = malloc(sizeof(struct bucket_request));
			bk_req->dart_id = dart_id;
			jobq_cache_bk_req(job, bk_req);
	
			/* Reply miss msg to slave server */
			//err = reply_bucket_request_miss();
		}
		else {
			/* assign job id to the selected idle bucket */
			bk->current_jid = jid;
			run_bk_list_remove(bk->dart_id);
			run_bk_list_add(bk);

               		uloga("%s(): assign job (%d, %d) to bucket %d\n",
                        	__func__, jid.type, jid.tstep, bk->dart_id);

			/* Reply hit msg to slave server */
			err = reply_bucket_request_hit(dart_id, bk);
		}

		// Added for SC evaluation
		print_rr_count();
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

	/* any working bk in the run_bk_list for this job ID? */
	bk = run_bk_list_lookup(&jid);
	
	if (bk) {
		/* try to regain credits to send msg */
		struct node_id *peer = ds_get_peer(ds, bk->dart_id);
		while ( peer->num_msg_at_peer <= 0 ) {
			err = dsg_process(dsg);
			if (err < 0)
				uloga("%s(): dsg_process err\n", __func__);
		}

		err = send_rdma_data_desc(bk, rdma_desc);
		return err;
	}

	/* if job already in the job_queue, if yes insert data descriptor */
	struct job *job = jobq_lookup(&jid);
	if (!job) {
		//No. create new job in job queue
		job = jobq_add_job(&jid);
	
		//send bucket request
		err = send_bucket_request(rdma_desc);
	} 

	//insert data descriptor into job queue
	jobq_cache_data_desc(job, rdma_desc);

	return err;
}

/*
 RPC callback function for incoming in-situ data descriptors msg (master server side)
*/
static int process_insitu_data_desc_master(struct rpc_server *rpc_s, struct rdma_data_descriptor *rdma_desc)
{
	int err = -ENOMEM;
	struct bucket *bk;
	struct job_id jid = extract_job_id(&rdma_desc->desc);

	/* any working bk in run_bk_list for this job ID? */
	bk = run_bk_list_lookup(&jid);

	if (bk) {
		/* try to regain credits to send msg */
		struct node_id *peer = ds_get_peer(ds, bk->dart_id);
		while ( peer->num_msg_at_peer <= 0 ) {
			err = dsg_process(dsg);
			if (err < 0)
				uloga("%s(): dsg_process err\n", __func__);
		}

		/* send data desc to that bucket */
		err = send_rdma_data_desc(bk, rdma_desc);
		return err;
	}

	/* pull bucket from idle_bk_list*/
	bk = idle_bk_list_remove_head();
	if (bk) {
		uloga("%s(): assign job (%d, %d) to bucket %d\n",
			__func__, jid.type, jid.tstep, bk->dart_id);
		/* get one ! */
		bk->current_jid = jid;	
		run_bk_list_remove(bk->dart_id);
		run_bk_list_add(bk);

		/* try to regain credits to send msg */
		struct node_id *peer = ds_get_peer(ds, bk->dart_id);
		while ( peer->num_msg_at_peer <= 0 ) {
			err = dsg_process(dsg);
			if (err < 0)
				uloga("%s(): dsg_process err\n", __func__);
		}

		err = send_rdma_data_desc(bk, rdma_desc);

		//Added for SC evaluation
		print_rr_count();		
	} 
	else {
		/* no idle ... */
		struct job *job = jobq_lookup(&jid);
		if (!job) {
			/* create new job queue entry */
			job = jobq_add_job(&jid);

			//Added for SC evaluation
			print_rr_count();
		}

		/* cache received data descriptor */
		jobq_cache_data_desc(job, rdma_desc);		
	}

	return err;
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

	/*
	uloga("%s(): #%d get obj desc for job (%d,%d) from #%d\n",
		__func__, rpc_s->ptlmap.id, data_desc->type, data_desc->tstep, cmd->id );
	*/

	if (is_master())
		process_insitu_data_desc_master(rpc_s, &rdma_desc);
	else process_insitu_data_desc_slave(rpc_s, &rdma_desc);

	return 0;
}


static int rr_init()
{
	dsg = dsg_alloc(num_sp, num_cp, conf);
	if (!dsg) {
		return -1;
	}

	ds = dsg->ds;

	rpc_add_service(intran_req_job, callback_intran_req_job);
	rpc_add_service(rr_req_bk, callback_rr_req_bk);
	rpc_add_service(insitu_data_desc, callback_insitu_data_desc);
	rpc_add_service(rr_req_bk_reply, callback_rr_req_bk_reply);

	/*init the data structure */
	jobq_init();
	idle_bk_list_init();
	run_bk_list_init();

	return 0;
}

static int rr_run()
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

		send_pending_jobq_desc();
	}

	return 0;
}

static int rr_finish(void)
{
	dsg_barrier(dsg);
	dsg_free(dsg);

	return 0;
}


static void usage(void)
{
	printf("Usage: server OPTIONS\n"
			"OPTIONS: \n"
			"--server, -s    Number of server instance/staging nodes\n"
			"--cnodes, -c    Number of compute nodes\n"
			"--conf, -f      Define configuration file\n");
}

static int parse_args(int argc, char *argv[])
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

/* Public APIs */
int dspaces_rr_parse_args(int argc, char** argv)
{
	return parse_args(argc, argv);
}

void dspaces_rr_usage()
{
	usage();
}

int dspaces_rr_init()
{
	return rr_init();
}

int dspaces_rr_run()
{
	return rr_run();
}

int dspaces_rr_finish()
{
	return rr_finish();
}
