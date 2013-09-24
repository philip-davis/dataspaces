/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Fan Zhang (2012)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#ifdef DS_HAVE_DIMES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "dimes_interface.h"
#include "dimes_client.h"
#include "dimes_data.h"

/* Name mangling for C functions to adapt Fortran compiler */
#define FC_FUNC(name,NAME) name ## _

static struct dimes_client *dimes_c = NULL;
//static enum storage_type st = row_major;
static enum storage_type st = column_major;
static int num_dims = 2;

static void lib_exit_d(void)
{
	dcg_free(dimes_c->dcg);
	exit(EXIT_FAILURE);
}

#define ERROR_TRACE_AND_EXIT_D()					\
do {								\
	uloga("'%s()': failed with %d.\n", __func__, err);	\
	lib_exit_d();						\
} while (0)							

#define DIMES_WAIT_COMPLETION(x)				\
	do {							\
		err = dc_process(dimes_c->dcg->dc);		\
		if (err < 0)					\
			goto err_out;				\
	} while (!(x))

#define DIMES_CID	dimes_c->dcg->dc->self->ptlmap.id
#define DC dimes_c->dcg->dc
#define RPC_S dimes_c->dcg->dc->rpc_s 
#define NUM_SP dimes_c->dcg->dc->num_sp

static struct {
	int next;
	int opid[4095];
} sync_op_d;

static int syncop_next_d(void)
{
	static int num_op_d = sizeof(sync_op_d.opid) / sizeof(sync_op_d.opid[0]);
	int n;

	n = sync_op_d.next;
	// NOTE: this is tricky and error prone; implement a better sync opertation
	if (sync_op_d.opid[n] != 1) {
		uloga("'%s()': error sync operation overflows.\n", __func__);
	}
	sync_op_d.opid[n] = 0;
	sync_op_d.next = (sync_op_d.next + 1) % num_op_d;

	return n;
}

static int * syncop_ref_d(int opid)
{
	return &sync_op_d.opid[opid];
}

enum dimes_put_status {
	DIMES_PUT_OK = 0,
	DIMES_PUT_PENDING = 1,
};

struct dimes_memory_obj {
	struct list_head entry;
	int sid;
	size_t bytes_read;
	void *arg1;
	void *arg2;
};

// List of dimes_memory_obj  
static struct list_head mem_obj_list;
static inline int mem_obj_list_add(struct list_head *l, struct dimes_memory_obj *p) {
	list_add(&p->entry, l);
	return 0;
}

static inline int mem_obj_list_del(struct list_head *l, int sid) {
	struct dimes_memory_obj *p, *tmp;
	list_for_each_entry_safe(p, tmp, l, struct dimes_memory_obj, entry) {
		if (p->sid == sid) {
			list_del(&p->entry);
			free(p);
			return 0;
		}
	}

	return -1;
}

static inline struct dimes_memory_obj* mem_obj_list_find(struct list_head *l, int sid) {
	struct dimes_memory_obj *p;
	list_for_each_entry(p, l, struct dimes_memory_obj, entry) {
		if (p->sid == sid)
			return p;
	}

	return NULL;
} 

static inline void inc_rdma_pending()
{
	dimes_c->dcg->num_pending++;
}

static inline void dec_rdma_pending()
{
	dimes_c->dcg->num_pending--;
}

/**************************************************
  Data structures & functions for DIMES transaction
***************************************************/
struct query_dht_d {
	int                     qh_size, qh_num_peer;
	int                     qh_num_req_posted;
	int                     qh_num_req_received;
	int                     *qh_peerid_tab;
};

/* 
   A query is a multi step transaction that serves an 'obj_get'
   request. This structure keeps query info to assemble the result.
*/
struct query_tran_entry_d {
	struct list_head        q_entry;

	int                     q_id;
	struct obj_descriptor   q_obj;
	void                    *data_ref;

	/* Object data information. */
	int                     size_od, num_od, num_parts_rec;
	struct list_head        od_list;

	struct query_dht_d        *qh;

	/* Allocate/setup data for the objects in the 'od_list' that
		 are retrieved from the space */
	unsigned int	f_alloc_data:1,
					f_dht_peer_recv:1,
					f_locate_data_complete:1,
					f_complete:1,
					f_err:1;
};

/*
  Generate a unique query id.
*/
static int qt_gen_qid_d(void)
{
	static int qid_d = 0;
	return qid_d++;
}

static struct query_dht_d *qh_alloc_d(int qh_num)
{
	struct query_dht_d *qh = 0;

	qh = malloc(sizeof(*qh) + sizeof(int)*(qh_num+1) + 7);
	if (!qh) {
		errno = ENOMEM;
		return qh;
	}
	memset(qh, 0, sizeof(*qh));

	qh->qh_peerid_tab = (int *) (qh + 1);
	ALIGN_ADDR_QUAD_BYTES(qh->qh_peerid_tab);
	qh->qh_size = qh_num + 1;

	return qh;
}

static struct query_tran_entry_d*
qte_alloc_d(struct obj_data *od, int alloc_data)
{
	struct query_tran_entry_d *qte;

	qte = malloc(sizeof(*qte));
	if (!qte) {
		errno = ENOMEM;
		return NULL;
	}
	memset(qte, 0, sizeof(*qte));

	INIT_LIST_HEAD(&qte->od_list);
	qte->q_id = qt_gen_qid_d();
	qte->q_obj = od->obj_desc;
	qte->data_ref = od->data;
	qte->f_alloc_data = !!(alloc_data);
	qte->qh = qh_alloc_d(NUM_SP);
	if (!qte->qh) {
		free(qte);
		errno = ENOMEM;
		return NULL;
	}

	return qte;
}

static void qte_free_d(struct query_tran_entry_d *qte)
{
	free(qte->qh);
	free(qte);
}

static void qt_init_d(struct query_tran_d *qt)
{
	qt->num_ent = 0;
	INIT_LIST_HEAD(&qt->q_list);
}

static struct query_tran_entry_d * qt_find_d(struct query_tran_d *qt, int q_id)
{
	struct query_tran_entry_d *qte;

	list_for_each_entry(qte, &qt->q_list, struct query_tran_entry_d, q_entry) {
		if (qte->q_id == q_id)
			return qte;
	}

	return NULL;
}

static void qt_add_d(struct query_tran_d *qt, struct query_tran_entry_d *qte)
{
	list_add(&qte->q_entry, &qt->q_list);
	qt->num_ent++;
}

static void qt_remove_d(struct query_tran_d *qt, struct query_tran_entry_d *qte)
{
	list_del(&qte->q_entry);
	qt->num_ent--;
}

static struct obj_descriptor *
qt_find_obj_d(struct query_tran_entry_d *qte, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct obj_data *od;

	list_for_each_entry(od_w, &qte->od_list,
		struct obj_data_wrapper, obj_entry) {
	od = od_w->od;
		if (obj_desc_equals(&od->obj_desc, odsc))
			return &od->obj_desc;
	}

	return NULL;
}

static void qt_remove_obj_d(struct query_tran_entry_d *qte, 
				struct obj_data_wrapper *od_w)
{
	list_del(&od_w->obj_entry);
	qte->num_od--;
	qte->size_od--;

	free(od_w->od);
		free(od_w);
}

/*
  Unlink and release memory resources for obj_data objects in the 'od_list'.
*/
static void qt_free_obj_data_d(struct query_tran_entry_d *qte, int unlink)
{
	struct obj_data_wrapper *od_w, *t;
	struct obj_data *od;

	list_for_each_entry_safe(od_w, t, &qte->od_list,
			struct obj_data_wrapper, obj_entry) {
		/* TODO: free the object data withought iov. */
		od = od_w->od;
		if (od->data)
			free(od->data);

		if (unlink)
			qt_remove_obj_d(qte, od_w);
	}
}

/*
  Allocate obj data storage for a given transaction, i.e., allocate
  space for all object pieces.
*/
static int qt_alloc_obj_data_d(struct query_tran_entry_d *qte)
{
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	int n = 0;

	list_for_each_entry(od_w, &qte->od_list,
			struct obj_data_wrapper, obj_entry) {
		od = od_w->od;
		// TODO(fan):
		// It's important to also assign pointer value to od->_data,
		// otherwise obj_data_free() can NOT free the buffer properly.
		od->_data = od->data = malloc(obj_data_size(&od->obj_desc));
		if (!od->data)
			break;
		n++;
	}

	if (n != qte->num_od) {
		qt_free_obj_data_d(qte, 0);
		uloga("%s(): Failed to allocate memory.\n", __func__);
		return -ENOMEM;
	}

	return 0;
}

/*
  Add an object descriptor to a query transaction entry.
*/
static int qt_add_obj_d(struct query_tran_entry_d *qte, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	int err = -ENOMEM;

	/* I will allocate and add data later; so give a NULL now. */
	od = obj_data_alloc_no_data(odsc, NULL);
	if (!od)
		return err;

	/* Preserve the storage type of the query object. */
	od->obj_desc.st = qte->q_obj.st;

	od_w = (struct obj_data_wrapper*)malloc(sizeof(*od_w));
	od_w->od = od;
	list_add(&od_w->obj_entry, &qte->od_list);
	qte->num_od++;

	return 0;
}

static int qt_add_obj_with_cmd_d(struct query_tran_entry_d *qte,
                struct obj_descriptor *odsc, struct rpc_cmd *cmd)
{
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	int err = -ENOMEM;

	od = obj_data_alloc_no_data(odsc, NULL);
	if (!od)
		return err;

	/* Preserve the storage type of the query object */
	od->obj_desc.st = qte->q_obj.st;

	od_w = (struct obj_data_wrapper*)malloc(sizeof(*od_w));
	od_w->od = od;
	od_w->cmd = *cmd; // Important!	

	list_add(&od_w->obj_entry, &qte->od_list);
	qte->num_od++;

	return 0;
}

static char *fstrncpy(char *cstr, const char *fstr, size_t len, size_t maxlen)
{
	if (!maxlen)
		return 0;

	while (len > 0 && fstr[len-1] == ' ')
		len--;

	if (len > maxlen-1)
		len = maxlen-1;

	strncpy(cstr, fstr, len);
	cstr[len] = '\0';

	return cstr;
}

static int obj_assemble(struct query_tran_entry_d *qte, struct obj_data *od)
{
	int err;
	struct obj_data *from, *t;
	struct obj_data_wrapper *od_w;
	struct list_head tmp_list;
	INIT_LIST_HEAD(&tmp_list);

	list_for_each_entry(od_w, &qte->od_list, struct obj_data_wrapper, obj_entry) {
		from = od_w->od;
		list_add_tail(&from->obj_entry, &tmp_list);
	}

	err = ssd_copy_list(od, &tmp_list);

	list_for_each_entry_safe(from, t, &tmp_list, struct obj_data, obj_entry) {
		list_del(&from->obj_entry);
	}
	
	if (err == 0)
		return 0;

	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int locate_data_completion_client(struct rpc_server *rpc_s,
					 struct msg_buf *msg)
{
	struct hdr_dimes_get *oh = msg->private;
	struct rpc_cmd *cmd_tab = msg->msg_data;
	int i, err = -ENOENT;

	struct query_tran_entry_d *qte = qt_find_d(&dimes_c->qt, oh->qid);
	if (!qte) {
		uloga("%s(): can not find transaction qid= %d\n",
			__func__, oh->qid);
		goto err_out_free;
	}

	// Add received rpc_cmd information.
	qte->qh->qh_num_req_received++;
	qte->size_od += oh->num_de;
	for (i = 0; i < oh->num_de; i++) {
		struct hdr_dimes_put *hdr =
			(struct hdr_dimes_put*)cmd_tab[i].pad;
		struct obj_descriptor odsc = hdr->odsc;

		// Calculate the intersection of the bbox (specified by the 
		// receiver process) and the bbox (returned by the server).
		bbox_intersect(&qte->q_obj.bb, &hdr->odsc.bb, &odsc.bb);	
		if (!qt_find_obj_d(qte, &odsc)) {
#ifdef DEBUG    
			uloga("%s(): #%d get cmd from server "
			      "with hdr->has_rdma_data=%d, odsc->name=%s, odsc->owner=%d, "
			      "data_size=%u, qte->size_od=%d\n",
				__func__, DIMES_CID, hdr->has_rdma_data, odsc.name,
				odsc.owner, obj_data_size(&odsc), qte->size_od);
#endif
                        err = qt_add_obj_with_cmd_d(qte, &odsc, &cmd_tab[i]);
                        if (err < 0)
                                goto err_out_free;
		}
		else {
/*
#ifdef DEBUG
			uloga("%s(): duplicate obj descriptor detected.\n",
				__func__);
#endif
*/
			qte->size_od--;
		}
	}

	free(oh);
	free(cmd_tab);
	free(msg);

	if (qte->qh->qh_num_req_received == qte->qh->qh_num_req_posted) {
		qte->f_locate_data_complete = 1;
	}

	return 0;
err_out_free:
	free(oh);
	free(cmd_tab);
	free(msg);
	ERROR_TRACE();
}

static int dcgrpc_dimes_locate_data(struct rpc_server *rpc_s,
				    struct rpc_cmd *cmd)
{
	struct hdr_dimes_get *oht, 
			 *oh = (struct hdr_dimes_get *) cmd->pad;
	struct node_id *peer = dc_get_peer(DC, cmd->id);
	struct rpc_cmd *cmd_tab;
	struct msg_buf *msg;
	int err = -ENOMEM;

#ifdef DEBUG
	uloga("%s(): #%d oh->qid=%d, oh->rc=%d, oh->num_de=%d\n", __func__,
		DIMES_CID, oh->qid, oh->rc, oh->num_de);
#endif

	if (oh->rc == -1) {
		// No need to fetch the cmd table.
		struct query_tran_entry_d *qte =
				qt_find_d(&dimes_c->qt, oh->qid);
		if (!qte) {
			uloga("%s(): can not find transaction ID= %d\n",
					__func__, oh->qid);
			goto err_out;
		}

		qte->qh->qh_num_req_received++;
		if (qte->qh->qh_num_req_received == qte->qh->qh_num_req_posted) 
		{
			qte->f_locate_data_complete = 1;
		}
		return 0;
	}

	cmd_tab = malloc(sizeof(struct rpc_cmd) * oh->num_de);
	if (!cmd_tab)
		goto err_out;

	oht = malloc(sizeof(*oh));
	if (!oht) {
		free(cmd_tab);
		goto err_out;
	}
	memcpy(oht, oh, sizeof(*oh));

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg) {
		free(cmd_tab);
		goto err_out;
	}

	msg->size = sizeof(struct rpc_cmd) * oh->num_de;
	msg->msg_data = cmd_tab;
	msg->cb = locate_data_completion_client;
	msg->private = oht;

	if (msg->size <= 0) {
		free(cmd_tab);
		free(msg);
		goto err_out;
	}	

	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	if (err == 0)
		return 0;

	free(cmd_tab);
	free(msg);
err_out:
	ERROR_TRACE();
}

static int location_peers_table_clear(struct query_tran_entry_d *qte)
{
	qte->qh->qh_num_peer = 0;
	qte->qh->qh_peerid_tab[0] = -1;
}

static int location_peers_table_insert(struct query_tran_entry_d *qte,
				 struct obj_descriptor *podsc)
{
	int i;
	for (i = 0; i < qte->qh->qh_num_peer; i++) {
		if (qte->qh->qh_peerid_tab[i] == podsc->owner ||
		    podsc->owner < 0) {
#ifdef DEBUG
			uloga("%s(): duplicate or wrong peer id %d\n",
			__func__, podsc->owner);
#endif
			return 0;
		}
	}

	// Insert new peer id
	qte->qh->qh_peerid_tab[qte->qh->qh_num_peer++] = podsc->owner;
	qte->qh->qh_peerid_tab[qte->qh->qh_num_peer] = -1;
	return 0;
}

static int dcgrpc_dimes_ss_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_ss_info *hsi = (struct hdr_ss_info *) cmd->pad;

	dimes_c->dcg->ss_info.num_dims = hsi->num_dims;
	dimes_c->dcg->ss_info.num_space_srv = hsi->num_space_srv;
	dimes_c->domain.num_dims = hsi->num_dims;
	dimes_c->domain.lb.c[0] = 0;
	dimes_c->domain.lb.c[1] = 0;
	dimes_c->domain.lb.c[2] = 0;
	dimes_c->domain.ub.c[0] = hsi->val_dims[0]-1;
	dimes_c->domain.ub.c[1] = hsi->val_dims[1]-1;
	dimes_c->domain.ub.c[2] = hsi->val_dims[2]-1;
	dimes_c->f_ss_info = 1;

#ifdef DEBUG
	uloga("%s(): num_dims=%d, num_space_srv=%d, "
		"domain={(%d,%d,%d),(%d,%d,%d)}\n",
		__func__, hsi->num_dims, hsi->num_space_srv,
		0, 0, 0, hsi->val_dims[0]-1, hsi->val_dims[1]-1,
		hsi->val_dims[2]-1);
#endif
	return 0;
}

static int dimes_ss_info(int *num_dims)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (dimes_c->f_ss_info) {
		*num_dims = dimes_c->dcg->ss_info.num_dims;
		return 0;
	}

	peer = dc_get_peer(DC, DIMES_CID % NUM_SP);
	msg = msg_buf_alloc(RPC_S, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = dimes_ss_info_msg;
	msg->msg_rpc->id = DIMES_CID;
	
	err = rpc_send(RPC_S, peer, msg);
	if (err < 0)
		goto err_out;

	DIMES_WAIT_COMPLETION(dimes_c->f_ss_info == 1);
	
	*num_dims = dimes_c->dcg->ss_info.num_dims;

	dimes_c->ssd = ssd_alloc(&dimes_c->domain,
				dimes_c->dcg->ss_info.num_space_srv, 1);
	if (!dimes_c->ssd) {
		uloga("%s(): ssd_alloc failed!\n",__func__);
		err = -1;
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

/*
  Free resources after object retrieved by remote peers
*/
static int dimes_obj_put_test_completion(struct rpc_server *rpc_s,
				    struct msg_buf *msg)
{
	struct obj_data *od = msg->private;

	(*msg->sync_op_id) = 1;

	obj_data_free(od);
	free(msg);

	dec_rdma_pending();

	return 0;
}

static int dimes_obj_put_test(struct obj_data *od)
{
	struct dht_entry *de_tab[dimes_c->dcg->ss_info.num_space_srv];
	struct hdr_dimes_put *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int i, num_de;
	int err = -ENOMEM;
	int sid = syncop_next_d();
	
	num_de = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, de_tab);
	if (num_de <= 0) {
		uloga("%s(): NOT Good!! ssd_hash return num_de=%d\n",
				__func__, num_de);
		goto err_out;
	}

	// Update the corresponding DHT nodes.
	for (i = 0; i < num_de; i++) {
		// TODO(fan): There is assumption here that the space servers
		// rank range from 0~(num_space_srv-1).
		peer = dc_get_peer(DC, de_tab[i]->rank);
		msg = msg_buf_alloc(RPC_S, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_put_test_msg;
		msg->msg_rpc->id = DIMES_CID;
		hdr = (struct hdr_dimes_put *)msg->msg_rpc->pad;
		memcpy(&hdr->odsc, &od->obj_desc, sizeof(struct obj_descriptor));

		// Only send the RDMA buffer handle to the minrank DHT node...
		// TODO(fan): This is tricky. Need a better solution.
		if (i == 0) {
			msg->msg_data = od->data;
			msg->size = obj_data_size(&od->obj_desc);
			msg->cb = dimes_obj_put_test_completion;
			msg->private = od;
			msg->sync_op_id = syncop_ref_d(sid);
			hdr->has_rdma_data = 1;

			inc_rdma_pending(); // Tricky!!!
		} else {
			hdr->has_rdma_data = 0;
		}

		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	// TODO(fan): We really need DART level APIs to explicitly
	// allocate/deallocate RDMA send/recv buffers.

	struct dimes_memory_obj *mem_obj = (struct dimes_memory_obj*)malloc(sizeof(*mem_obj));
	mem_obj->sid = sid;
	mem_obj->bytes_read = 0;
	mem_obj->arg1 = mem_obj->arg2 = NULL;
	mem_obj_list_add(&mem_obj_list, mem_obj);

	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int obj_data_get_direct_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct query_tran_entry_d *qte = msg->private;

	if (++qte->num_parts_rec == qte->size_od) {
			qte->f_complete = 1;
	#ifdef DEBUG
		uloga("%s(): #%d, qte->num_parts_rec = %d\n",
				__func__, DIMES_CID, qte->num_parts_rec);
	#endif
	}

	free(msg);
	return 0;
}

static int dimes_obj_data_get_test(struct query_tran_entry_d *qte)
{
	struct msg_buf *msg;
	struct node_id *peer;
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	int err;

	err = qt_alloc_obj_data_d(qte);
	if (err < 0)
		goto err_out;

	qte->f_complete = 0;

	list_for_each_entry(od_w, &qte->od_list,
			struct obj_data_wrapper, obj_entry) {
		err = -ENOMEM;

		od = od_w->od;
		peer = dc_get_peer(DC, od->obj_desc.owner);
		msg = msg_buf_alloc(RPC_S, peer, 0);
		if (!msg) {
			free(od->data);
			od->data = NULL;
			goto err_out;
		}

		msg->size = obj_data_size(&od->obj_desc);
		msg->msg_data = od->data;
		msg->private = qte;
		msg->cb = obj_data_get_direct_completion;

		if (msg->size <=0) {
			uloga("%s(): msg->size = %d.\n",
				__func__, msg->size);
			free(msg);
			free(od->data);
			od->data = NULL;
			goto err_out;
		}

		rpc_mem_info_cache(peer, msg, &od_w->cmd);
		err = rpc_receive_direct(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			free(od->data);
			od->data = NULL;
			goto err_out;
		}
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static int dimes_locate_data_test(struct query_tran_entry_d *qte)
{
	struct hdr_dimes_get *oh;
	struct node_id *peer;
	struct msg_buf *msg;
	int *peer_id, err;

	qte->f_locate_data_complete = 0;
	qte->qh->qh_num_req_posted =
	qte->qh->qh_num_req_received = 0;

	peer_id = qte->qh->qh_peerid_tab;
	while (*peer_id != -1) {
		peer = dc_get_peer(DC, *peer_id);
		err = -ENOMEM;
		msg = msg_buf_alloc(RPC_S, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_locate_data_test_msg;
		msg->msg_rpc->id = DIMES_CID;

		oh = (struct hdr_dimes_get *) msg->msg_rpc->pad;
		oh->qid = qte->q_id;
		oh->odsc = qte->q_obj;
		oh->rank = DIMES_CID;

		qte->qh->qh_num_req_posted++;
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			qte->qh->qh_num_req_posted--;
			goto err_out;
		}
		peer_id++;
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

static int dimes_obj_get_test(struct obj_data *od)
{
	struct dht_entry *de_tab[dimes_c->dcg->ss_info.num_space_srv];
	struct query_tran_entry_d *qte;
	int err = -ENOMEM;
	int num_de, i;
	
	qte = qte_alloc_d(od, 1);
	if (!qte)
		goto err_out;

	qt_add_d(&dimes_c->qt, qte);
	
	/* get dht peers */
	num_de = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, de_tab);
	if (num_de <= 0) {
		uloga("%s(): ssd_hash return %d\n",__func__,num_de);
		goto err_qt_free;
	}

	for ( i=0; i<num_de && i<qte->qh->qh_size; i++ )
			qte->qh->qh_peerid_tab[i] = de_tab[i]->rank;
	qte->qh->qh_peerid_tab[i] = -1;
	qte->qh->qh_num_peer = num_de;
	qte->f_dht_peer_recv = 1;

	// Locate the RDMA buffers
	err = dimes_locate_data_test(qte);
	if ( err < 0 ) {
		if (err == -EAGAIN)
			goto out_no_data;
		else goto err_qt_free;
	}
	DIMES_WAIT_COMPLETION(qte->f_locate_data_complete == 1);

	// Fetch the data buffers
	err = dimes_obj_data_get_test(qte);
	if ( err < 0 ) {
		qt_free_obj_data_d(qte, 1);
		goto err_data_free;
	}

	while (!qte->f_complete) {
		err = dc_process(DC);
		if (err < 0) {
			uloga("%s(): error.\n",__func__);
			break;
		}
	}

	if (!qte->f_complete) {
		err = -ENODATA;
		goto out_no_data;
	}

	err = obj_assemble(qte, od);
out_no_data:
	qt_free_obj_data_d(qte, 1);
	qt_remove_d(&dimes_c->qt, qte);
	free(qte);
	return err;
err_data_free:
	qt_free_obj_data_d(qte, 1);
err_qt_free:
	qt_remove_d(&dimes_c->qt, qte);
	free(qte);
err_out:
	ERROR_TRACE();    	
}

static int dimes_memory_obj_status(struct dimes_memory_obj *mem_obj)
{
	int sid = mem_obj->sid;
	int *sync_op_ref = syncop_ref_d(sid);
	int err;
	
	if (sync_op_ref[0] == 1) {
		err = DIMES_PUT_OK;
	} else {
		err = DIMES_PUT_PENDING;
	}

	return err;
}

static int dimes_obj_put(struct obj_data *od)
{
	struct dht_entry *de_tab[dimes_c->dcg->ss_info.num_space_srv];
	struct hdr_dimes_put *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int i, num_de;
	int err = -ENOMEM;
	int sid = syncop_next_d();

	// Register the RDMA memory buffer
	void *data = od->data;
	size_t bytes = obj_data_size(&od->obj_desc);
	struct dart_rdma_mem_handle *rdma_hndl =
		(struct dart_rdma_mem_handle*)malloc(sizeof(*rdma_hndl));
	if (rdma_hndl == NULL) {
		uloga("%s(): malloc() failed\n", __func__);
		goto err_out;
	} 

	err = dart_rdma_register_mem(rdma_hndl, data, bytes);
	if (err < 0) {
		free(rdma_hndl);
		goto err_out;
	}

	// Update the DHT nodes
	num_de = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, de_tab);
	if (num_de <= 0) {
		uloga("%s(): NOT Good!! ssd_hash return num_de=%d\n",
				__func__, num_de);
		goto err_out;
	}
	
	for (i = 0; i < num_de; i++) {
		// TODO(fan): There is assumption here that the space servers
		// rank range from 0~(num_space_srv-1).
		peer = dc_get_peer(DC, de_tab[i]->rank);
		msg = msg_buf_alloc(RPC_S, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_put_msg;
		msg->msg_rpc->id = DIMES_CID;
		dart_rdma_set_memregion_to_cmd(rdma_hndl, msg->msg_rpc);	

		hdr = (struct hdr_dimes_put *)msg->msg_rpc->pad;
		hdr->odsc = od->obj_desc;
		hdr->sync_id = sid;
		hdr->has_rdma_data = 1;
	
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	inc_rdma_pending();
	struct dimes_memory_obj *mem_obj = (struct dimes_memory_obj*)malloc(sizeof(*mem_obj));
	mem_obj->sid = sid;
	mem_obj->bytes_read = 0;
	mem_obj->arg1 = rdma_hndl;
	mem_obj->arg2 = od;
	mem_obj_list_add(&mem_obj_list, mem_obj);

	return 0;
err_out:
	ERROR_TRACE();			
}

static int dimes_locate_data(struct query_tran_entry_d *qte)
{
	struct hdr_dimes_get *oh;
	struct node_id *peer;
	struct msg_buf *msg;
	int *peer_id, err;

	qte->f_locate_data_complete = 0;
	qte->qh->qh_num_req_posted =
	qte->qh->qh_num_req_received = 0;

	peer_id = qte->qh->qh_peerid_tab;
	while (*peer_id != -1) {
		peer = dc_get_peer(DC, *peer_id);
		err = -ENOMEM;
		msg = msg_buf_alloc(RPC_S, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_locate_data_msg;
		msg->msg_rpc->id = DIMES_CID;

		oh = (struct hdr_dimes_get *) msg->msg_rpc->pad;
		oh->qid = qte->q_id;
		oh->odsc = qte->q_obj;
		oh->rank = DIMES_CID;

		qte->qh->qh_num_req_posted++;
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			qte->qh->qh_num_req_posted--;
			goto err_out;
		}
		peer_id++;
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

// TODO(fan): to modify the structure of dart_rdma_read_tran
struct temp_read_tran {
	int tran_id;
	struct dart_rdma_mem_handle src;
	struct dart_rdma_mem_handle dst;
};

struct matrix_view_d {
	int lb[3];
	int ub[3];
};

struct matrix_d {
	int dimx, dimy, dimz;
	size_t size_elem;
	enum storage_type mat_storage;
	struct matrix_view_d mat_view;
};

static void matrix_init_d(struct matrix_d *mat, enum storage_type st,
                        struct bbox *bb_glb, struct bbox *bb_loc, size_t se)
{
	int i;

	mat->dimx = bbox_dist(bb_glb, bb_x);
	mat->dimy = bbox_dist(bb_glb, bb_y);
	mat->dimz = bbox_dist(bb_glb, bb_z);

	mat->mat_storage = st;

	for (i = bb_x; i <= bb_z; i++) {
		mat->mat_view.lb[i] = bb_loc->lb.c[i] - bb_glb->lb.c[i];
		mat->mat_view.ub[i] = bb_loc->ub.c[i] - bb_glb->lb.c[i];
	}

	mat->size_elem = se;
}

static int matrix_rdma_copy(struct matrix_d *a, struct matrix_d *b, int tran_id)
{
	int ai, aj, ak, bi, bj, bk;
	int n;
	int err = -ENOMEM;
	size_t src_offset = 0;
	size_t dst_offset = 0;
	size_t bytes = 0;

	if (a->mat_storage == row_major && b->mat_storage == row_major) {
		n = a->mat_view.ub[bb_x] - a->mat_view.lb[bb_x] + 1;
		ak = a->mat_view.lb[bb_x];
		bk = b->mat_view.lb[bb_x];
		for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
			 ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
			for (aj = a->mat_view.lb[bb_y], bj = b->mat_view.lb[bb_y];
				 aj <= a->mat_view.ub[bb_y]; aj++, bj++) {
				bytes = a->size_elem * n;
				src_offset = a->size_elem *
					(bk + b->dimx * (bj + b->dimy * bi));
				dst_offset = a->size_elem *
					(ak + a->dimx * (aj + a->dimy * ai));
				/*
				#ifdef DEBUG
				uloga("%s(): tran_id=%d, bk=%d, bj=%d, bi=%d, "
				  "src_offset=%u, ak=%d, aj=%d, ai=%d, "
				  "dst_offset=%u, bytes=%u\n",
					__func__, tran_id, bk, bj, bi, src_offset,
					ak, aj, ai, dst_offset, bytes);
				#endif
				*/

				err = dart_rdma_schedule_read(tran_id, src_offset, dst_offset, bytes);
				if (err < 0)
					goto err_out;
			}
		}
	} else if (a->mat_storage == column_major && b->mat_storage == column_major) {
		n = a->mat_view.ub[bb_y] - a->mat_view.lb[bb_y] + 1;
		ak = a->mat_view.lb[bb_y];
		bk = b->mat_view.lb[bb_y];
		for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
			 ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
			for (aj = a->mat_view.lb[bb_x], bj = b->mat_view.lb[bb_x];
				 aj <= a->mat_view.ub[bb_x]; aj++, bj++) {
				bytes = a->size_elem * n;
				src_offset = a->size_elem *
						(bk + b->dimy * (bj + b->dimx * bi));
				dst_offset = a->size_elem *
						(ak + a->dimy * (aj + a->dimx * ai));

				err = dart_rdma_schedule_read(
						tran_id, src_offset, dst_offset, bytes);
				if (err < 0)
					goto err_out;
			}
		}
	}
	
	return 0;
err_out:
	ERROR_TRACE();
}

static int schedule_rdma_reads(int tran_id,
        struct obj_descriptor *src_odsc, struct obj_descriptor *dst_odsc)
{
	int err = -ENOMEM;
	size_t src_offset = 0;
	size_t dst_offset = 0;
	size_t bytes = 0;

/*
#ifdef DEBUG
	char *str;
	asprintf(&str, "#%d get obj %s ver %d from ",
		DIMES_CID, dst_odsc->name, dst_odsc->version);
	str = str_append(str, bbox_sprint(&dst_odsc->bb));
	uloga("%s(): tran_id=%d, %s\n", __func__, tran_id, str);
	free(str);
#endif
*/

	if (obj_desc_equals_no_owner(src_odsc, dst_odsc)) {
#ifdef DEBUG
		uloga("%s(): #%d performs only one rdma read for tran_id=%d\n",
			__func__, DIMES_CID, tran_id);
#endif
		src_offset = 0;
		dst_offset = 0;
		bytes = obj_data_size(dst_odsc);
		err = dart_rdma_schedule_read(
					tran_id,
					src_offset,
					dst_offset,
					bytes);	
		if (err < 0) {
			uloga("%s(): failed with dart_rdma_schedule_read\n",
				__func__);
			goto err_out;
		}
	} else {
		struct matrix_d to, from;
		struct bbox bbcom;
		bbox_intersect(&dst_odsc->bb, &src_odsc->bb, &bbcom);
		matrix_init_d(&from, src_odsc->st, &src_odsc->bb, &bbcom,
				src_odsc->size);
		matrix_init_d(&to, dst_odsc->st, &dst_odsc->bb, &bbcom,
				dst_odsc->size);
		err = matrix_rdma_copy(&to, &from, tran_id);
		if (err < 0) {
			uloga("%s(): failed with matrix_rdma_copy\n", __func__);
		}	
	}	

	return 0;
err_out:
	ERROR_TRACE();
} 

static int send_ack_v1(struct query_tran_entry_d *qte)
{
	int err;
	struct msg_buf *msg;
	struct node_id *peer;
	struct obj_data_wrapper *od_w;
	struct obj_data *od; 
	struct hdr_dimes_get_ack_v1 *oh;
	struct hdr_dimes_put *hdr;

	list_for_each_entry(od_w, &qte->od_list,
						struct obj_data_wrapper, obj_entry) {
		od = od_w->od;
		hdr = (struct hdr_dimes_put*)od_w->cmd.pad;
		// Ack. directly to the owner of retrieved data object
		peer = dc_get_peer(DC, hdr->odsc.owner);
		msg = msg_buf_alloc(RPC_S, peer, 1);
		msg->msg_rpc->cmd = dimes_obj_get_ack_v1_msg;
		msg->msg_rpc->id = DIMES_CID;

		oh = (struct hdr_dimes_get_ack_v1*)msg->msg_rpc->pad;
		oh->qid = qte->q_id;
		oh->sync_id = hdr->sync_id;
		oh->odsc = od->obj_desc;
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	return 0;	
 err_out:
	ERROR_TRACE();
}

static int send_ack_v2(struct query_tran_entry_d *qte)
{
	int err;
	struct msg_buf *msg;
	struct node_id *peer;
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	struct hdr_dimes_get_ack_v2 *oh;
	struct hdr_dimes_put *hdr;

	list_for_each_entry(od_w, &qte->od_list,
			struct obj_data_wrapper, obj_entry) {
		od = od_w->od;
		hdr = (struct hdr_dimes_put*)od_w->cmd.pad;
		// Ack. indirectly through server
		peer = dc_get_peer(DC, hdr->odsc.owner % NUM_SP);
		msg = msg_buf_alloc(RPC_S, peer, 1);
		msg->msg_rpc->cmd = dimes_obj_get_ack_v2_msg;
		msg->msg_rpc->id = DIMES_CID;
		
		oh = (struct hdr_dimes_get_ack_v2 *)msg->msg_rpc->pad;
		oh->qid = qte->q_id;
		oh->sync_id = hdr->sync_id;
		oh->odsc = hdr->odsc;
		oh->bytes_read = obj_data_size(&od->obj_desc);
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

static int dimes_obj_data_get(struct query_tran_entry_d *qte)
{
	struct node_id *peer;
	struct obj_data_wrapper *od_w;
	struct obj_data *od;
	struct hdr_dimes_put *hdr;
	int i = 0, err;

	err = qt_alloc_obj_data_d(qte);
	if (err < 0)
		goto err_out;

	qte->f_complete = 0;

	// Allocate the array for read transactions
	struct dart_rdma_read_tran **trans_tab = NULL;
	trans_tab = (struct dart_rdma_read_tran **)
			malloc(sizeof(struct dart_rdma_read_tran *) * qte->size_od);
	if (trans_tab == NULL) {
		uloga("%s(): failed with malloc()\n", __func__);
		err = -1;
		goto err_out;
	}
	memset(trans_tab, 0, sizeof(struct dart_rdma_read_tran *)*qte->size_od);

	list_for_each_entry(od_w, &qte->od_list,
			    struct obj_data_wrapper, obj_entry) {
		od = od_w->od;
		hdr = (struct hdr_dimes_put*)od_w->cmd.pad;
		peer = dc_get_peer(DC, od->obj_desc.owner);

		err = dart_rdma_create_read_tran(peer, &trans_tab[i]);	

		if (trans_tab[i]->remote_peer->ptlmap.id == DIMES_CID) {
			// Data on local peer
			struct dimes_memory_obj *mem_obj = mem_obj_list_find(&mem_obj_list,
					hdr->sync_id);
			if (mem_obj == NULL) {
				uloga("%s(): failed to find dimes memory object sid=%d\n",
					__func__, hdr->sync_id);
				goto err_out_free;
			}
			struct dart_rdma_mem_handle *rdma_hndl = 
					(struct dart_rdma_mem_handle*) mem_obj->arg1; 			
			trans_tab[i]->src.base_addr = rdma_hndl->base_addr;
			trans_tab[i]->src.size = rdma_hndl->size;
			trans_tab[i]->dst.base_addr = od->data; 
			trans_tab[i]->dst.size = obj_data_size(&od->obj_desc);
		} else {
			// Data on remote peer
			dart_rdma_get_memregion_from_cmd(&trans_tab[i]->src,
							 &od_w->cmd);
			err = dart_rdma_register_mem(&trans_tab[i]->dst,
							 od->data,
							 obj_data_size(&od->obj_desc));
			if (err < 0) {
				uloga("%s(): failed with dart_rdma_register_mem\n",
					__func__);
				goto err_out_free;
			}
		}

		// Add read operations into the transaction
		err = schedule_rdma_reads(trans_tab[i]->tran_id,
					  &hdr->odsc,
					  &od->obj_desc);
		if (err < 0) {
			uloga("%s(): failed with schedule_rdma_reads()\n",
				__func__);
			goto err_out_free;
		}

		// Perform RDMA read transaction
		err = dart_rdma_perform_reads(trans_tab[i]->tran_id);
		if (err < 0) {
			uloga("%s(): failed with dart_rdma_perform_reads\n",
				__func__);
			goto err_out_free;
		}

		err = dart_rdma_check_reads(trans_tab[i]->tran_id);
		if (err < 0) {
			uloga("%s(): failed with dart_rdma_check_reads on tran %d\n",
				__func__, trans_tab[i]->tran_id);
			goto err_out_free;
		}
	
		if (trans_tab[i]->remote_peer->ptlmap.id != DIMES_CID) {	
			err = dart_rdma_deregister_mem(&trans_tab[i]->dst);
			if (err < 0) {
				uloga("%s(): failed dart_rdma_deregister_mem on tran %d\n",
					__func__, trans_tab[i]->tran_id);
				goto err_out_free;
			}
		}

		dart_rdma_delete_read_tran(trans_tab[i]->tran_id);

		i++;
	}

	if (i != qte->size_od) {
		uloga("%s(): ERROR i=%d qte->size_od=%d\n",
			__func__, i, qte->size_od);
	}

	// Send back ack messages to all dst. peers
	//err = send_ack_v1(qte);
	err = send_ack_v2(qte);
	if (err < 0) {
		goto err_out_free;
	}

	qte->f_complete = 1;

	if (trans_tab) {
		free(trans_tab);
	}

	return 0;
err_out_free:
	free(trans_tab);
err_out:
	ERROR_TRACE();
}

static int dimes_obj_get(struct obj_data *od)
{
	struct dht_entry *de_tab[dimes_c->dcg->ss_info.num_space_srv];
	struct query_tran_entry_d *qte;
	int err = -ENOMEM;
	int num_de, i;

	qte = qte_alloc_d(od, 1);
	if (!qte)
		goto err_out;

	qt_add_d(&dimes_c->qt, qte);

	/* get dht peers */
	num_de = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, de_tab);
	if (num_de <= 0) {
		uloga("%s(): ssd_hash return %d\n",__func__,num_de);
		goto err_qt_free;
	}

	for ( i=0; i<num_de && i<qte->qh->qh_size; i++ )
		qte->qh->qh_peerid_tab[i] = de_tab[i]->rank;
	qte->qh->qh_peerid_tab[i] = -1;
	qte->qh->qh_num_peer = num_de;
	qte->f_dht_peer_recv = 1;
#ifdef DEBUG
	uloga("%s(): #%d get dht peers complete!\n", __func__, DIMES_CID);
#endif

	// Locate the RDMA buffers
	err = dimes_locate_data(qte);
	if ( err < 0 ) {
		if (err == -EAGAIN)
			goto out_no_data;
		else goto err_qt_free;
	}
	DIMES_WAIT_COMPLETION(qte->f_locate_data_complete == 1);
#ifdef DEBUG
	uloga("%s(): #%d locate data complete!\n", __func__, DIMES_CID);
#endif

	// Fetch the data
	err = dimes_obj_data_get(qte);
	if (err < 0) {
		qt_free_obj_data_d(qte, 1);
		goto err_data_free;
	}

	if (!qte->f_complete) {
		err = -ENODATA;
		goto out_no_data;
	}
#ifdef DEBUG
	uloga("%s(): #%d fetch data complete!\n", __func__, DIMES_CID);
#endif

	err = obj_assemble(qte, od);
out_no_data:
	qt_free_obj_data_d(qte, 1);
	qt_remove_d(&dimes_c->qt, qte);
	free(qte);
	return err;
err_data_free:
	qt_free_obj_data_d(qte, 1);
err_qt_free:
	qt_remove_d(&dimes_c->qt, qte);
	free(qte);
err_out:
	ERROR_TRACE();
}

static int dcgrpc_dimes_obj_get_ack_v1(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct dart_rdma_mem_handle *rdma_hndl = NULL;
	struct obj_data *od = NULL;
	struct hdr_dimes_get_ack_v1 *oh = (struct hdr_dimes_get_ack_v1 *)cmd->pad;
	int sid = oh->sync_id;
	int err = -ENOMEM;

	struct dimes_memory_obj *mem_obj = mem_obj_list_find(&mem_obj_list, sid);
	if (mem_obj == NULL) {
		uloga("%s(): failed to find dimes memory object sid=%d\n", __func__, sid);
		err = -1;
		goto err_out;
	}	

	rdma_hndl = (struct dart_rdma_mem_handle *) mem_obj->arg1;
	od = (struct obj_data *) mem_obj->arg2;
	mem_obj->bytes_read += obj_data_size(&oh->odsc);
	if (mem_obj->bytes_read == obj_data_size(&od->obj_desc)) {
		// Deregister RDMA memory region
		err = dart_rdma_deregister_mem(rdma_hndl);
		if (err < 0) {
			uloga("%s(): failed with dart_rdma_deregister_mem\n", __func__);
			goto err_out;
		}
			
		// Free memory
		obj_data_free(od);

		// Set the flag
		int *ref = syncop_ref_d(mem_obj->sid);
		*ref = 1;

		dec_rdma_pending();
	}

#ifdef DEBUG
	uloga("%s(): #%d get ack from #%d for sync_id=%d\n",
		__func__, DIMES_CID, cmd->id, mem_obj->sid);
#endif

	return 0;
err_out:
	ERROR_TRACE();
}

static int dcgrpc_dimes_obj_get_ack_v2(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_dimes_get_ack_v2 *oh = (struct hdr_dimes_get_ack_v2 *)cmd->pad;
	int sid = oh->sync_id;
	int err = -ENOMEM;

	struct dimes_memory_obj *mem_obj = mem_obj_list_find(&mem_obj_list, sid);
	if (mem_obj == NULL) {
		uloga("%s(): failed to find dimes memory object sid=%d\n", __func__, sid);
		err = -1;
		goto err_out;
	}	

	struct dart_rdma_mem_handle *rdma_hndl = (struct dart_rdma_mem_handle *) mem_obj->arg1;
	struct obj_data *od = (struct obj_data *) mem_obj->arg2;
	if (oh->bytes_read != obj_data_size(&od->obj_desc)) {
		uloga("%s(): should not happen...\n", __func__);
	}

	// Deregister RDMA memory region
	err = dart_rdma_deregister_mem(rdma_hndl);
	if (err < 0) {
		uloga("%s(): failed with dart_rdma_deregister_mem\n", __func__);
		goto err_out;
	}
			
	// Free memory
	obj_data_free(od);

	// Set the flag
	int *ref = syncop_ref_d(mem_obj->sid);
	*ref = 1;

	dec_rdma_pending();

#ifdef DEBUG
	uloga("%s(): #%d get ack from #%d for sync_id=%d\n",
		__func__, DIMES_CID, cmd->id, mem_obj->sid);
#endif

	return 0;
 err_out:
	ERROR_TRACE();	
}

static int dimes_put_sync_all_with_timeout(float timeout_sec)
{
	int err;
	if (!dimes_c) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		return -EINVAL;
	}

	int stay_in_poll_loop = 1;
	struct timer tm;
	double t1, t2;
	timer_init(&tm, 1);
	timer_start(&tm);
	t1 = timer_read(&tm);

	while (stay_in_poll_loop) {	
		// Check the size of mem_obj_list
		if (list_empty(&mem_obj_list)) {
			err = 0;
			break;
		}

		struct dimes_memory_obj *p, *tmp;
		list_for_each_entry_safe(p, tmp, &mem_obj_list,
					 struct dimes_memory_obj, entry) {
			// TODO: fix this part
#ifdef HAVE_DCMF
			err = rpc_process_event(RPC_S);
#else
			err = rpc_process_event_with_timeout(RPC_S, 1);
#endif
			if (err < 0) {
				return -1;
			}

			switch (dimes_memory_obj_status(p)) {
			case DIMES_PUT_OK:
				list_del(&p->entry);
				free(p);
				break;
			case DIMES_PUT_PENDING:
				// Continue to block and check...
				break;
			default:
				uloga("%s(): err= %d shouldn't happen!\n", __func__, err);
				break;
			}
		}

		if (timeout_sec < 0) {
			stay_in_poll_loop = 1;
		} else if (timeout_sec == 0) {
			// TODO: or i should return immediately before the loop?
			stay_in_poll_loop = 0;
		} else if (timeout_sec > 0) {
			t2 = timer_read(&tm);
			if ((t2-t1) >= timeout_sec) {
				stay_in_poll_loop = 0;
			}
		}
	}

	return 0;
}

/*
  DIMES client public APIs.
*/
struct dimes_client* dimes_client_alloc(void * ptr)
{
	int err = -ENOMEM;

	if (dimes_c) {
		// Library already initialized.
		uloga("%s(): dimes already initialized.");
		return dimes_c;
	}

	dimes_c = calloc(1, sizeof(*dimes_c));

	dimes_c->dcg = (struct dcg_space*)ptr;
	if (!dimes_c->dcg) {
		uloga("'%s()': failed to initialize.\n", __func__);
		return NULL; 
	}

	int i;
	for (i = 0; i < sizeof(sync_op_d.opid)/sizeof(sync_op_d.opid[0]); i++)
		sync_op_d.opid[i] = 1;

	qt_init_d(&dimes_c->qt);

	// Add rpc servie routines
	rpc_add_service(dimes_ss_info_msg, dcgrpc_dimes_ss_info);
	rpc_add_service(dimes_locate_data_test_msg, dcgrpc_dimes_locate_data);
	rpc_add_service(dimes_locate_data_msg, dcgrpc_dimes_locate_data);
	rpc_add_service(dimes_obj_get_ack_v1_msg, dcgrpc_dimes_obj_get_ack_v1); 
	rpc_add_service(dimes_obj_get_ack_v2_msg, dcgrpc_dimes_obj_get_ack_v2); 

	err = dimes_ss_info(&num_dims);
	if (err < 0) {
		uloga("'%s()': failed to obtain space info, err %d.\n",
				__func__, err);
		return NULL;
	}

	INIT_LIST_HEAD(&mem_obj_list);

	dart_rdma_init(RPC_S);

#ifdef DEBUG
	uloga("%s(): OK.\n", __func__);
#endif
	return dimes_c;
}

void dimes_client_free(void) {
	if (!dimes_c) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		return;
	}

	free(dimes_c);
	dimes_c = NULL;

	dart_rdma_finalize();
}

void common_dimes_set_storage_type(int fst)
{
	if (fst == 0)
		st = row_major;
	else if (fst == 1)
		st = column_major;
}

int common_dimes_get(const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data)
{
	struct obj_descriptor odsc = {
			.version = ver, .owner = -1,
			.st = st,
			.size = size,
			.bb = {.num_dims = num_dims,
				   .lb.c = {xl, yl, zl},
				   .ub.c = {xu, yu, zu}}};
	struct obj_data *od;
	int err = -ENOMEM;

	if (!dimes_c->dcg) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		err =  -EINVAL;
		goto err_out;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_no_data(&odsc, data);
	if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n",
				__func__);
		err = -ENOMEM;
		goto err_out;
	}
	
	err = dimes_obj_get(od);
	//err = dimes_obj_get_test(od);
 
	obj_data_free(od);
	if (err < 0 && err != -EAGAIN)
		uloga("'%s()': failed with %d, can not get data object.\n",
				__func__, err);

	return err;
err_out:
	ERROR_TRACE();
}

int common_dimes_put(const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data)
{
	struct obj_descriptor odsc = {
			.version = ver, .owner = -1,
			.st = st,
			.size = size,
			.bb = {.num_dims = num_dims,
				   .lb.c = {xl, yl, zl},
				   .ub.c = {xu, yu, zu}}};
	struct obj_data *od;
	int err = -ENOMEM;

	if (!dimes_c->dcg) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		err = -EINVAL;
		goto err_out;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_with_data(&odsc, data);
	if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n",
				__func__);
		err = -ENOMEM;
		goto err_out;
	}

	err = dimes_obj_put(od);
	//err = dimes_obj_put_test(od);
	if (err < 0) {
		obj_data_free(od);

		uloga("'%s()': failed with %d, can not put data object.\n",
				__func__, err);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

int common_dimes_put_sync_all(void)
{
/*
	int err;
	if (!dimes_c) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		return -EINVAL;
	}

	do {
		// Check the size of mem_obj_list
		if (list_empty(&mem_obj_list)) {
			err = 0;
			break;
		}

		struct dimes_memory_obj *p, *tmp;
		list_for_each_entry_safe(p, tmp, &mem_obj_list,
					 struct dimes_memory_obj, entry) {
			// TODO: fix this part
#ifdef HAVE_DCMF
			err = rpc_process_event(RPC_S);
#else
			err = rpc_process_event_with_timeout(RPC_S, 1);
#endif
			if (err < 0) {
				return -1;
			}

			switch (dimes_memory_obj_status(p)) {
			case DIMES_PUT_OK:
				list_del(&p->entry);
				free(p);
				break;
			case DIMES_PUT_PENDING:
				// Continue to block and check...
				break;
			default:
				uloga("%s(): err= %d shouldn't happen!\n", __func__, err);
				break;
			}
		}
	} while (1);

	return err;
*/
	return dimes_put_sync_all_with_timeout(-1.0);
}

int common_dimes_put_sync_all_with_timeout(float timeout_sec)
{
	return dimes_put_sync_all_with_timeout(timeout_sec);
}

#endif // end of #ifdef DS_HAVE_DIMES
