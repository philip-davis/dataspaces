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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "dimes_interface.h"
#include "dimes_data.h"

#include "debug.h"
#include "dart.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"

/* Name mangling for C functions to adapt Fortran compiler */
#define FC_FUNC(name,NAME) name ## _

struct query_cache_d {
        struct list_head        q_list;
        int                     num_ent;
};

struct query_tran_d {
        struct list_head        q_list;
        int                     num_ent;
};

struct obj_data_wrapper {
	struct list_head obj_entry;
	struct obj_data *od;
	struct rpc_cmd cmd;
};

struct dimes_client {
	struct dcg_space *dcg;
	struct sspace *ssd; //only used for hashing
	struct bbox domain;
	struct query_cache_d	qc;
	struct query_tran_d	qt;
	int	f_ss_info; // TODO(fanc): rename it?
};

static struct dimes_client *dimes_c = NULL;
static struct timer timer;
static int sync_op_id;
static int cq_id = -1;
static enum storage_type st = row_major;
//static enum storage_type st = column_major;
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

inline static void inc_rdma_pending()
{
	dimes_c->dcg->num_pending++;
}

inline static void dec_rdma_pending()
{
	dimes_c->dcg->num_pending--;
}

static void versions_reset_d(void)
{
        memset(dimes_c->dcg->versions, 0, sizeof(dimes_c->dcg->versions));
        dimes_c->dcg->num_vers = 0;
}

/**************************************************
  Data structures & functions for DIMES transaction
***************************************************/
struct query_cache_entry_d {
        struct list_head        q_entry;

        /* Initial query copy. */
        struct obj_descriptor   q_obj;

        /* Decomposition of initial query. */
        int                     num_odsc;
        struct obj_descriptor   *odsc_tab;
};

struct query_dht_d {
        int                     qh_size, qh_num_peer;
        int                     qh_num_req_posted;
        int                     qh_num_rep_received;
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
        unsigned int            f_alloc_data:1,
                                f_peer_received:1,
                                f_odsc_recv:1,
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

static struct query_tran_entry_d * qte_alloc_d(struct obj_data *od, int alloc_data)
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

        qte->qh = qh_alloc_d(dimes_c->dcg->dc->num_sp);
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

        list_for_each_entry(od_w, &qte->od_list, struct obj_data_wrapper, obj_entry) {
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

        list_for_each_entry_safe(od_w, t, &qte->od_list, struct obj_data_wrapper, obj_entry) {
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

        list_for_each_entry(od_w, &qte->od_list, struct obj_data_wrapper, obj_entry) {
		od = od_w->od;
                od->data = malloc(obj_data_size(&od->obj_desc));
                if (!od->data)
                        break;
                n++;
        }

        if (n != qte->num_od) {
                qt_free_obj_data_d(qte, 0);
		uloga("%s(): Failed to alloc memory.\n", __func__);
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
	od_w->cmd = *cmd;	

        list_add(&od_w->obj_entry, &qte->od_list);
        qte->num_od++;

        return 0;
}

static int qte_set_odsc_from_cache_d(struct query_tran_entry_d *qte,
        const struct query_cache_entry_d *qce)
{
        int err, i;

        for (i = 0; i < qce->num_odsc; i++) {
                err = qt_add_obj_d(qte, qce->odsc_tab+i);
                if (err < 0)
                        break;
        }

        if (i != qce->num_odsc) {
                qt_free_obj_data_d(qte, 1);
                return err;
        }
        qte->size_od = qce->num_odsc;

        return 0;
}

static struct query_cache_entry_d *qce_alloc_d(int num_obj_desc)
{
        struct query_cache_entry_d *qce = 0;

        qce = malloc(sizeof(*qce) + num_obj_desc * sizeof(struct obj_descriptor));
        if (!qce)
                return NULL;

        qce->num_odsc = num_obj_desc;
        qce->odsc_tab = (struct obj_descriptor *) (qce + 1);

        return qce;
}

static void qce_free_d(struct query_cache_entry_d *qce)
{
        free(qce);
}

static void qce_set_obj_desc_d(struct query_cache_entry_d *qce,
        struct obj_descriptor *q_obj, const struct list_head *obj_list)
{
	struct obj_data_wrapper *od_w;
        int i = 0;

        qce->q_obj = *q_obj;
        list_for_each_entry(od_w, obj_list, struct obj_data_wrapper, obj_entry) {
                qce->odsc_tab[i++] = od_w->od->obj_desc;
        }
}

static struct query_cache_d * qc_alloc_d(void) // __attribute__((unused))
{
        struct query_cache_d *qc;

        qc = malloc(sizeof(*qc));
        if (!qc)
                return NULL;

        memset(qc, 0, sizeof(*qc));
        INIT_LIST_HEAD(&qc->q_list);

        return qc;
}

static void qc_init_d(struct query_cache_d *qc)
{
        INIT_LIST_HEAD(&qc->q_list);
        qc->num_ent = 0;
}

static void qc_add_entry_d(struct query_cache_d *qc, struct query_cache_entry_d *qce)
{
        list_add(&qce->q_entry, &qc->q_list);
        qc->num_ent++;
}

static void qc_del_entry_d(struct query_cache_d *qc, struct query_cache_entry_d *qce)
{
        list_del(&qce->q_entry);
        qc->num_ent--;
}

static const struct query_cache_entry_d *
qc_find_d(struct query_cache_d *qc, struct obj_descriptor *odsc)
{
        struct query_cache_entry_d *qce = 0;

        list_for_each_entry(qce, &qc->q_list, struct query_cache_entry_d, q_entry) {
                if (obj_desc_equals_no_owner(&qce->q_obj, odsc))
                        return qce;
        }

        return NULL;
}

static void qc_free_d(struct query_cache_d *qc)
{
        struct query_cache_entry_d *qce, *tqce;

        list_for_each_entry_safe(qce, tqce, &qc->q_list, struct query_cache_entry_d,  q_entry) {
                free(qce);
        }
        // free(qc);
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

static int get_desc_completion_client(struct rpc_server *rpc_s, struct msg_buf *msg
)
{
        struct hdr_dimes_obj_get *oh = msg->private;
        struct rpc_cmd *cmd_tab = msg->msg_data;
        struct obj_descriptor *odsc;
        struct query_tran_entry_d *qte;
        int i, err = -ENOENT;

        qte = qt_find_d(&dimes_c->qt, oh->qid);
        if (!qte) {
                uloga("%s(): can not find transaction ID= %d\n",
		      __func__, oh->qid);
                goto err_out_free;
        }

        /*add received rpc_cmd info*/
        qte->qh->qh_num_rep_received++;
        qte->size_od += oh->u.o.num_de;
        for (i = 0; i < oh->u.o.num_de; i++) {
		struct hdr_dimes_put *hdr =
			(struct hdr_dimes_put*)cmd_tab[i].pad;
		odsc = &hdr->odsc;
#ifdef DEBUG    
        uloga("%s(): get cmd from server #%d, "
              "with hdr->has_rdma_data=%d, "
              "odsc->name=%s, odsc->owner=%d, data_size=%u.\n",
                __func__, cmd_tab[i].id, hdr->has_rdma_data,
                odsc->name, odsc->owner, obj_data_size(odsc));
#endif
                if (!qt_find_obj_d(qte, odsc)) {
                        err = qt_add_obj_with_cmd_d(qte, odsc, &cmd_tab[i]);
                        if (err < 0)
                                goto err_out_free;
                }
                else {
                        uloga("%s(): duplicate obj descriptor detected.\n",
				__func__);
                        qte->size_od--;
                }
        }

        free(oh);
        free(cmd_tab);
        free(msg);

        if (qte->qh->qh_num_rep_received == qte->qh->qh_num_peer) {
                qte->f_odsc_recv = 1;
	}

        return 0;
err_out_free:
        free(oh);
        free(cmd_tab);
        free(msg);
        ERROR_TRACE();
}

static int dcgrpc_dimes_get_desc(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_dimes_obj_get *oht, 
				 *oh = (struct hdr_dimes_obj_get *) cmd->pad;
        struct node_id *peer = dc_get_peer(dimes_c->dcg->dc, cmd->id);
        struct rpc_cmd *cmd_tab;
        struct msg_buf *msg;
        int err = -ENOMEM;

#ifdef DEBUG
	uloga("%s(): oh->qid=%d, oh->rc=%d, oh->u.o.num_de=%d\n", __func__,
		oh->qid, oh->rc, oh->u.o.num_de);
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

                qte->qh->qh_num_rep_received++;
                if (qte->qh->qh_num_rep_received == qte->qh->qh_num_peer) {
                        qte->f_odsc_recv = 1;
                }
                return 0;
        }

        cmd_tab = malloc(sizeof(struct rpc_cmd) * oh->u.o.num_de);
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

        msg->size = sizeof(struct rpc_cmd) * oh->u.o.num_de;
        msg->msg_data = cmd_tab;
        msg->cb = get_desc_completion_client;
        msg->private = oht; //TODO: need this??

	if (msg->size <= 0) {
		ulog("%s(): msg->size = %d.\n", __func__, msg->size);
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

	peer = dc_get_peer(dimes_c->dcg->dc, DIMES_CID % dimes_c->dcg->dc->num_sp);
	msg = msg_buf_alloc(dimes_c->dcg->dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = dimes_ss_info_msg;
	msg->msg_rpc->id = DIMES_CID;
	
	err = rpc_send(dimes_c->dcg->dc->rpc_s, peer, msg);
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
static int dimes_obj_put_completion(struct rpc_server *rpc_s,
				    struct msg_buf *msg)
{
        struct obj_data *od = msg->private;

        (*msg->sync_op_id) = 1;

        obj_data_free(od);
        free(msg);

        dec_rdma_pending();

#ifdef DEBUG
	uloga("%s(): #%d.\n", __func__, DIMES_CID);
#endif
        return 0;
}

static int dimes_obj_put(struct obj_data *od)
{
	struct dht_entry *de_tab[dimes_c->dcg->ss_info.num_space_srv];
	struct hdr_dimes_put *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int sync_id;
	int i, num_de;
	int err = -ENOMEM;

	sync_id = syncop_next_d();
	
	num_de = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, de_tab);
	if (num_de <= 0) {
                uloga("%s(): NOT Good!! ssd_hash return num_de=%d\n",
                        __func__, num_de);
                goto err_out;
	}

        // Update the corresponding DHT nodes.
        for (i = 0; i < num_de; i++) {
                peer = dc_get_peer(dimes_c->dcg->dc, de_tab[i]->rank);
                msg = msg_buf_alloc(dimes_c->dcg->dc->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_rpc->cmd = dimes_put_msg;
                msg->msg_rpc->id = DIMES_CID;
                hdr = (struct hdr_dimes_put *)msg->msg_rpc->pad;
                memcpy(&hdr->odsc, &od->obj_desc, sizeof(od->obj_desc));

                // Only send the RDMA buffer handle to the minrank DHT node...
                // TODO(fan): This is tricky. Need a better solution.
                if (i == 0) {
                        msg->msg_data = od->data;
                        msg->size = obj_data_size(&od->obj_desc);
                        msg->cb = dimes_obj_put_completion;
                        msg->private = od;
                        msg->sync_op_id = syncop_ref_d(sync_id);
                        hdr->has_rdma_data = 1;

                        inc_rdma_pending(); // Tricky!!!
                } else {
                        hdr->has_rdma_data = 0;
                }

                err = rpc_send(dimes_c->dcg->dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        goto err_out;
                }
        }

        // TODO(fan): We really need DART level APIs to explicitly
        // allocate/deallocate RDMA send/recv buffers.

	return sync_id;
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

static int dimes_obj_data_get(struct query_tran_entry_d *qte)
{
        struct msg_buf *msg;
        struct node_id *peer;
	struct obj_data_wrapper *od_w;
        struct obj_data *od;
        int err;

        err = qt_alloc_obj_data_d(qte);
        if (err < 0)
                goto err_out;

        list_for_each_entry(od_w, &qte->od_list,
			    struct obj_data_wrapper, obj_entry) {
		err = -ENOMEM;

		od = od_w->od;
                peer = dc_get_peer(dimes_c->dcg->dc, od->obj_desc.owner);
                msg = msg_buf_alloc(dimes_c->dcg->dc->rpc_s, peer, 0);
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
                err = rpc_receive_direct(dimes_c->dcg->dc->rpc_s, peer, msg);
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

static int dimes_get_obj_descriptors(struct query_tran_entry_d *qte)
{
        struct hdr_dimes_obj_get *oh;
        struct node_id *peer;
        struct msg_buf *msg;
        int *peer_id, err;

        qte->f_odsc_recv = 0;

        peer_id = qte->qh->qh_peerid_tab;
        while (*peer_id != -1) {
                peer = dc_get_peer(dimes_c->dcg->dc, *peer_id);
                err = -ENOMEM;
                msg = msg_buf_alloc(dimes_c->dcg->dc->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_rpc->cmd = dimes_get_desc_msg;
                msg->msg_rpc->id = DIMES_CID;

                oh = (struct hdr_dimes_obj_get *) msg->msg_rpc->pad;
                oh->qid = qte->q_id;
                oh->u.o.odsc = qte->q_obj;
                oh->rank = DIMES_CID;

                qte->qh->qh_num_req_posted++;
                err = rpc_send(dimes_c->dcg->dc->rpc_s, peer, msg);
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
	
	versions_reset_d();

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
        qte->f_peer_received = 1;

        /* get data obj descriptors */
        err = dimes_get_obj_descriptors(qte);
        if ( err < 0 ) {
                if (err == -EAGAIN)
                        goto out_no_data;
                else goto err_qt_free;
        }
        DIMES_WAIT_COMPLETION(qte->f_odsc_recv == 1);

        if (qte->f_err != 0) {
                err = -EAGAIN;
                goto out_no_data;
        }

        err = dimes_obj_data_get(qte);
        if ( err < 0 ) {
                qt_free_obj_data_d(qte, 1);
                goto err_data_free;
        }

        while ( !qte->f_complete) {
                err = dc_process(dimes_c->dcg->dc);

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

static int dimes_obj_sync(int sync_op_id)
{
        int *sync_op_ref = syncop_ref_d(sync_op_id);
        int err;

        while (sync_op_ref[0] != 1) {
                err = dc_process(dimes_c->dcg->dc);
                if (err < 0) {
                        goto err_out;
                }
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  C interface for DIMES.
*/
/*
int dimes_init(int num_peers, int appid)
{
	struct dimes_client *dimes_c_l;
	int err = -ENOMEM;

        if (dimes_c) {
                // Library already initialized.
                return 0;
        }
	
	dimes_c = dimes_c_l = calloc(1, sizeof(*dimes_c_l));

        dimes_c->dcg = dcg_alloc(num_peers, appid);
        if (!dimes_c->dcg) {
                uloga("'%s()': failed to initialize.\n", __func__);
                return err;
        }

	int i;
        for (i = 0; i < sizeof(sync_op_d.opid)/sizeof(sync_op_d.opid[0]); i++)
                sync_op_d.opid[i] = 1;

	qt_init_d(&dimes_c->qt);

	//add rpc servie routines
	rpc_add_service(dimes_ss_info_msg, dcgrpc_ss_info_dimes);
	rpc_add_service(dimes_get_desc_msg, dcgrpc_get_desc);

        err = dimes_ss_info(&num_dims);
        if (err < 0) {
                uloga("'%s()': failed to obtain space info, err %d.\n",
                        __func__, err);
                return err;
        }

	qc_init_d(&dimes_c->qc);

        return 0;
}
*/

int dimes_alloc(void * ptr)
{
        int err = -ENOMEM;

        if (dimes_c) {
                // Library already initialized.
		uloga("%s(): dimes already initialized.");
                return 0;
        }

        dimes_c = calloc(1, sizeof(*dimes_c));

        dimes_c->dcg = (struct dcg_space*)ptr;
        if (!dimes_c->dcg) {
                uloga("'%s()': failed to initialize.\n", __func__);
                return err;
        }

        int i;
        for (i = 0; i < sizeof(sync_op_d.opid)/sizeof(sync_op_d.opid[0]); i++)
                sync_op_d.opid[i] = 1;

        qt_init_d(&dimes_c->qt);

        //add rpc servie routines
        rpc_add_service(dimes_ss_info_msg, dcgrpc_dimes_ss_info);
        rpc_add_service(dimes_get_desc_msg, dcgrpc_dimes_get_desc);

        err = dimes_ss_info(&num_dims);
        if (err < 0) {
                uloga("'%s()': failed to obtain space info, err %d.\n",
                        __func__, err);
                return err;
        }

        qc_init_d(&dimes_c->qc);

	uloga("%s(): OK.\n", __func__);
        return 0;
}

void dimes_set_storage_type(int fst)
{
        if (fst == 0)
                st = row_major;
        else if (fst == 1)
                st = column_major;
}

/*
int dimes_rank(void)
{
        if (dimes_c)
                return dcg_get_rank(dimes_c->dcg);
        else return -1;
}

int dimes_peers(void)
{
        if (dimes_c)
                return dcg_get_num_peers(dimes_c->dcg);
        else return -1;
}

void dimes_barrier(void)
{
	int err;
	
        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        err = dcg_barrier(dimes_c->dcg);
        if (err < 0)
                ERROR_TRACE_AND_EXIT_D();
}

void dimes_lock_on_read(const char *lock_name)
{
        int err;

        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        err = dcg_lock_on_read(lock_name);
        if (err < 0)
                ERROR_TRACE_AND_EXIT_D();
}

void dimes_unlock_on_read(const char *lock_name)
{
        int err;

        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        err = dcg_unlock_on_read(lock_name);
        if (err < 0)
                ERROR_TRACE_AND_EXIT_D();
}

void dimes_lock_on_write(const char *lock_name)
{
        int err;

        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        err = dcg_lock_on_write(lock_name);
        if (err < 0)
                ERROR_TRACE_AND_EXIT_D();
}

void dimes_unlock_on_write(const char *lock_name)
{
        int err;

        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        err = dcg_unlock_on_write(lock_name);
        if (err < 0)
                ERROR_TRACE_AND_EXIT_D();
}
*/

int dimes_get(const char *var_name,
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
                return -EINVAL;
        }

        strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
        odsc.name[sizeof(odsc.name)-1] = '\0';

        od = obj_data_alloc_no_data(&odsc, data);
        if (!od) {
                uloga("'%s()': failed, can not allocate data object.\n",
                        __func__);
                return -ENOMEM;
        }

        err = dimes_obj_get(od);
        obj_data_free(od);
        if (err < 0 && err != -EAGAIN)
                uloga("'%s()': failed with %d, can not get data object.\n",
                        __func__, err);

        return err;
}

int dimes_put(const char *var_name,
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
                return -EINVAL;
        }

        strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
        odsc.name[sizeof(odsc.name)-1] = '\0';

        od = obj_data_alloc_with_data(&odsc, data);
        if (!od) {
                uloga("'%s()': failed, can not allocate data object.\n",
                        __func__);
                return -ENOMEM;
        }

        err = dimes_obj_put(od);
        if (err < 0) {
                obj_data_free(od);

                uloga("'%s()': failed with %d, can not put data object.\n",
                        __func__, err);
                return err;
        }
        sync_op_id = err;

        return 0;
}

int dimes_put_sync(void)
{
        int err;

        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return -EINVAL;
        }

        err = dimes_obj_sync(sync_op_id);
        if (err < 0)
                uloga("'%s()': failed with %d, can not complete put_sync.\n",
                        __func__, err);

        return err;
}

/*
void dimes_finalize(void)
{
        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        dcg_free(dimes_c->dcg);
	qc_free_d(&dimes_c->qc);
	free(dimes_c);
	dimes_c = NULL;
}
*/

void dimes_free(void) {
        if (!dimes_c) {
                uloga("'%s()': library was not properly initialized!\n",
                         __func__);
                return;
        }

        qc_free_d(&dimes_c->qc);
        free(dimes_c);
        dimes_c = NULL;
}

/*
  Fortran interface to DIMES
*/
/*
void FC_FUNC(dimes_init, DIMES_INIT)(int *num_peers, int *appid, int *err)
{
        *err = dimes_init(*num_peers, *appid);
}
*/

void FC_FUNC(dimes_set_storage_type, DIMES_SET_STORAGE_TYPE) (int *fst)
{
        dimes_set_storage_type(*fst);
}

/*
void FC_FUNC(dimes_rank, DIMES_RANK)(int *rank)
{
        *rank = dimes_rank();
}

void FC_FUNC(dimes_peers, DIMES_PEERS)(int *peers)
{
        *peers = dimes_peers();
}

void FC_FUNC(dimes_barrier, DIMES_BARRIER)(void)
{
        dimes_barrier();
}

void FC_FUNC(dimes_lock_on_read, DIMES_LOCK_ON_READ)(const char *lock_name, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        dimes_lock_on_read(c_lock_name);
}

void FC_FUNC(dimes_unlock_on_read, DIMES_UNLOCK_ON_READ)(const char *lock_name, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        dimes_unlock_on_read(c_lock_name);
}

void FC_FUNC(dimes_lock_on_write, DIMES_LOCK_ON_WRITE)(const char *lock_name, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        dimes_lock_on_write(c_lock_name);
}

void FC_FUNC(dimes_unlock_on_write, DIMES_UNLOCK_ON_WRITE)(const char *lock_name, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        dimes_unlock_on_write(c_lock_name);
}
*/

void FC_FUNC(dimes_get, DIMES_GET) (const char *var_name,
        unsigned int *ver, int *size,
        int *xl, int *yl, int *zl,
        int *xu, int *yu, int *zu,
        void *data, int *err, int len)
{
        char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
                uloga("'%s()': failed, can not copy Fortran var of len %d.\n",
                        __func__, len);
                *err = -ENOMEM;
        }

        *err = dimes_get(vname, *ver, *size, 
				*xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dimes_put, DIMES_PUT) (const char *var_name,
        unsigned int *ver, int *size,
        int *xl, int *yl, int *zl,
        int *xu, int *yu, int *zu,
        void *data, int *err, int len)
{
        char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
                uloga("'%s': failed, can not copy Fortran var of len %d.\n",
                        __func__, len);
                *err = -ENOMEM;
        }

        *err =  dimes_put(vname, *ver, *size,
				 *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dimes_put_sync, DIMES_PUT_SYNC)(int *err)
{
        *err = dimes_put_sync();
}

/*
void FC_FUNC(dimes_finalize, DIMES_FINALIZE) (void)
{
        dimes_finalize();
}
*/
