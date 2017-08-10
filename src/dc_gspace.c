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
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "debug.h"
#include "dart.h"
#include "dc_gspace.h"
#include "ss_data.h"

#define DC_WAIT_COMPLETION(x)                                   \
        do {                                                    \
                err = dc_process(dcg->dc);                      \
                if (err < 0)                                    \
                        goto err_out;                           \
        } while (!(x))

#define DCG_ID          dcg->dc->self->ptlmap.id

#ifdef TIMING_PERF
#include "timer.h"
static char log_header[256] = "";
static struct timer tm_perf;
#endif

static int mpi_rank = 0;
static int flag_set_mpi_rank = 0;

/* Record the number of timing msgs received */
static int demo_num_timing_recv;

/* Record the sum of timing */ 
static double demo_sum_timing; 

typedef unsigned char		_u8;

struct query_cache_entry {
        struct list_head        q_entry;

        /* Initial query copy. */
        struct obj_descriptor   q_obj;

        /* Decomposition of initial query. */
        int                     num_odsc;
        struct obj_descriptor   *odsc_tab;
};

struct query_dht {
        int                     qh_size, qh_num_peer;
        int                     qh_num_req_posted;
        int                     qh_num_rep_received;
        int                     *qh_peerid_tab;
};

/* 
   A query is a multi step transaction that serves an 'obj_get'
   request. This structure keeps query info to assemble the result.
*/
struct query_tran_entry {
        struct list_head        q_entry;

        int                     q_id;

        struct obj_descriptor   q_obj;
        void                    *data_ref;

        /* Object data information. */
        int                     size_od, num_od, num_parts_rec;
        struct list_head        od_list;

        struct query_dht        *qh;

        struct global_dimension gdim;
        /* Allocate/setup data for the objects in the 'od_list' that
           are retrieved from the space */
        unsigned int		f_alloc_data:1,
                    f_peer_received:1,
                    f_odsc_recv:1,
                    f_complete:1,
                    f_err:1;
};

/*
  Lock ... service.
*/
struct dcg_lock {
	struct list_head	lock_entry;

        int                     req;
        int                     ack;
        int                     lock_num;
	char			name[LOCK_NAME_SIZE];
};

/* 
   Some operations  may require synchronizing API;  use this structure
   as a temporary hack to implement synchronization. 
*/
static struct {
        int next;
        int opid[4095];
} sync_op;

static struct dcg_space *dcg;

static inline void dcg_inc_pending(void)
{
        dcg->num_pending++;
}

static inline void dcg_dec_pending(void)
{
        dcg->num_pending--;
}

static inline struct dcg_space * dcg_ref_from_rpc(struct rpc_server *rpc_s)
{
        struct dart_client *dc = dc_ref_from_rpc(rpc_s);

        return dc->dart_ref;
}

/*
  Generate a unique query id.
*/
static int qt_gen_qid(void)
{
        static int qid = 0;
        return qid++;
}

static struct query_dht *qh_alloc(int qh_num)
{
        struct query_dht *qh = 0;

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

static struct query_tran_entry * qte_alloc(struct obj_data *od, int alloc_data)
{
	struct query_tran_entry *qte;

	qte = malloc(sizeof(*qte));
	if (!qte) {
		errno = ENOMEM;
		return NULL;
	}
	memset(qte, 0, sizeof(*qte));

	INIT_LIST_HEAD(&qte->od_list);
	qte->q_id = qt_gen_qid();
	qte->q_obj = od->obj_desc;
	qte->data_ref = od->data;
	qte->f_alloc_data = !!(alloc_data);
    memcpy(&qte->gdim, &od->gdim, sizeof(struct global_dimension));

	qte->qh = qh_alloc(dcg->dc->num_sp);
	if (!qte->qh) {
		free(qte);
		errno = ENOMEM;
		return NULL;
	}

	return qte;
}

static void qte_free(struct query_tran_entry *qte)
{
        free(qte->qh);
        free(qte);
}

static void qt_init(struct query_tran *qt)
{
        qt->num_ent = 0;
        INIT_LIST_HEAD(&qt->q_list);
}

static struct query_tran_entry * qt_find(struct query_tran *qt, int q_id)
{
        struct query_tran_entry *qte;

        list_for_each_entry(qte, &qt->q_list, struct query_tran_entry, q_entry) {
                if (qte->q_id == q_id)
                        return qte;
        }

        return NULL;
}

static void qt_add(struct query_tran *qt, struct query_tran_entry *qte)
{
        list_add(&qte->q_entry, &qt->q_list);
        qt->num_ent++;
}

static void qt_remove(struct query_tran *qt, struct query_tran_entry *qte)
{
        list_del(&qte->q_entry);
        qt->num_ent--;
}

static struct obj_descriptor * 
qt_find_obj(struct query_tran_entry *qte, struct obj_descriptor *odsc)
{
        struct obj_data *od;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                if (obj_desc_equals(&od->obj_desc, odsc))
                        return &od->obj_desc;
        }

        return NULL;
}

static void qt_remove_obj(struct query_tran_entry *qte, struct obj_data *od)
{
        list_del(&od->obj_entry);
        qte->num_od--;
        qte->size_od--;

        free(od);
}

/*
  Unlink and release memory resources for obj_data objects in the 'od_list'.
*/
static void qt_free_obj_data(struct query_tran_entry *qte, int unlink)
{
        struct obj_data *od, *t;

        list_for_each_entry_safe(od, t, &qte->od_list, struct obj_data, obj_entry) {
		/* TODO: free the object data withought iov. */
                if (od->data)
                        free(od->data);

                if (unlink)
			qt_remove_obj(qte, od);
        }
}

/*
  Allocate obj data storage for a given transaction, i.e., allocate
  space for all object pieces.
*/
static int qt_alloc_obj_data(struct query_tran_entry *qte)
{
        struct obj_data *od;
        int n = 0;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                od->data = malloc(obj_data_size(&od->obj_desc));
                if (!od->data)
                        break;
                n++;
        }

        if (n != qte->num_od) {
                qt_free_obj_data(qte, 0);
                return -ENOMEM;
        }
        return 0;
}

static int qt_alloc_obj_data_with_size(struct query_tran_entry *qte, size_t size)
{
        struct obj_data *od;
        int n = 0;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                od->data = malloc(size);
                if (!od->data)
                        break;
                n++;
        }

        if (n != qte->num_od) {
                qt_free_obj_data(qte, 0);
                return -ENOMEM;
        }
        return 0;
}

/*
  Add an object descriptor to a query transaction entry.
*/
static int qt_add_obj(struct query_tran_entry *qte, struct obj_descriptor *odsc)
{
        struct obj_data *od;
        int err = -ENOMEM;

        /* I will allocate and add data later; so give a NULL now. */
        od = obj_data_alloc_no_data(odsc, NULL);
        if (!od)
                return err;

        /* Preserve the storage type of the query object. */
        od->obj_desc.st = qte->q_obj.st;

        list_add(&od->obj_entry, &qte->od_list);
        qte->num_od++;

        return 0;
}

static int qt_add_objv(struct query_tran_entry *qte, struct obj_descriptor *odsc)
{
	struct obj_data qod, *od;
	int err = -ENOMEM;

	if (qte->f_alloc_data)
		od = obj_data_allocv(odsc);
	else	
		/* No need to allocate/setup data for rexec transactions. */
		od = obj_data_alloc_no_data(odsc, NULL);
	if (!od)
		return err;

	od->obj_desc.st = qte->q_obj.st;

	memset(&qod, 0, sizeof(qod));
	qod.obj_desc = qte->q_obj;
	qod.data = qte->data_ref;

	/* Setup pointers into the storage of the query object. */
	if (qte->f_alloc_data)
		ssd_copyv(od, &qod);

	list_add(&od->obj_entry, &qte->od_list);
	qte->num_od++;

	return 0;
}


/*
  Fill a new transaction entry for a data object.
static void qt_set(struct query_tran_entry *qte, struct obj_data *od)
{
        qte->q_id = qt_gen_qid();
        qte->q_obj = od->obj_desc;
        qte->data_ref = od->data;
}
*/

static int qte_set_odsc_from_cache(struct query_tran_entry *qte, 
        const struct query_cache_entry *qce)
{
        int err, i;

        for (i = 0; i < qce->num_odsc; i++) {
                err = qt_add_obj(qte, qce->odsc_tab+i);
                if (err < 0)
                        break;
        }

        if (i != qce->num_odsc) {
                qt_free_obj_data(qte, 1);
                return err;
        }
        qte->size_od = qce->num_odsc;

        return 0;
}

static struct query_cache_entry *qce_alloc(int num_obj_desc)
{
        struct query_cache_entry *qce = 0;

        qce = malloc(sizeof(*qce) + num_obj_desc * sizeof(struct obj_descriptor));
        if (!qce)
                return NULL;

        qce->num_odsc = num_obj_desc;
        qce->odsc_tab = (struct obj_descriptor *) (qce + 1);

        return qce;
}

static void qce_free(struct query_cache_entry *qce)
{
        free(qce);
}

static void qce_set_obj_desc(struct query_cache_entry *qce, 
        struct obj_descriptor *q_obj, const struct list_head *obj_list)
{
        struct obj_data *od;
        int i = 0;

        qce->q_obj = *q_obj;
        list_for_each_entry(od, obj_list, struct obj_data, obj_entry) {
                qce->odsc_tab[i++] = od->obj_desc;
        }
}

static struct query_cache * qc_alloc(void) // __attribute__((unused))
{
        struct query_cache *qc;

        qc = malloc(sizeof(*qc));
        if (!qc)
                return NULL;

        memset(qc, 0, sizeof(*qc));
        INIT_LIST_HEAD(&qc->q_list);

        return qc;
}

static void qc_init(struct query_cache *qc)
{
        INIT_LIST_HEAD(&qc->q_list);
        qc->num_ent = 0;
}

static void qc_add_entry(struct query_cache *qc, struct query_cache_entry *qce)
{
        list_add(&qce->q_entry, &qc->q_list);
        qc->num_ent++;
}

static void qc_del_entry(struct query_cache *qc, struct query_cache_entry *qce)
{
        list_del(&qce->q_entry);
        qc->num_ent--;
}

static const struct query_cache_entry *
qc_find(struct query_cache *qc, struct obj_descriptor *odsc)
{
        struct query_cache_entry *qce = 0;

        list_for_each_entry(qce, &qc->q_list, struct query_cache_entry, q_entry) {
                if (obj_desc_equals_no_owner(&qce->q_obj, odsc))
                        return qce;
        }

        return NULL;
}

static void qc_free(struct query_cache *qc)
{
        struct query_cache_entry *qce, *tqce;

        list_for_each_entry_safe(qce, tqce, &qc->q_list, struct query_cache_entry, q_entry) {
                free(qce);
        }
        // free(qc);
}


/*
  NOTE: routine seems to work, however it requires some more testing.

  Initial code: http://www.devmaster.net/codespotlight/show.php?id=25
*/
int instructionCount(const _u8 *func, int *size, int *off)
{ 
	const _u8 *pfn = func;
	int twoByte, operandSize, FPU;
	int found_off = 0;

	while (*func != 0xC3 && *func != 0xC9) { 
		// Skip prefixes F0h, F2h, F3h, 66h, 67h, D8h-DFh, 2Eh, 36h, 3Eh, 26h, 64h and 65h
		operandSize = 4; 
		FPU = 0; 

		while (*func == 0xF0 || 
		       *func == 0xF2 || 
		       *func == 0xF3 ||
		       *func == 0x48 ||
		       (*func & 0xFC) == 0x64 || 
		       (*func & 0xF8) == 0xD8 ||
		       (*func & 0x7E) == 0x64) { 

			if (*func == 0x66) { 
				operandSize = 2; 
			}
			else if ((*func & 0xF8) == 0xD8) {
				FPU = *func++;
				break;
			}

			func++;
		}

		// Skip two-byte opcode byte 
		twoByte = 0; 
		if (*func == 0x0F) { 
			twoByte = 1; 
			func++; 
		} 

		// Skip opcode byte 
		_u8 opcode = *func++; 
		if (!found_off && opcode == 0xC7) {
			found_off = 1;
			*off = func - pfn - 2;
		}

		// Skip mod R/M byte 
		_u8 modRM = 0xFF; 
		if (FPU) { 
			if ((opcode & 0xC0) != 0xC0) { 
				modRM = opcode; 
			}
		} 
		else if (!twoByte) { 
			if ((opcode & 0xC4) == 0x00 || 
			    ((opcode & 0xF4) == 0x60 && ((opcode & 0x0A) == 0x02 || (opcode & 0x09) == 0x09)) || 
			    (opcode & 0xF0) == 0x80 || 
			    ((opcode & 0xF8) == 0xC0 && (opcode & 0x0E) != 0x02) || 
			    (opcode & 0xFC) == 0xD0 || 
			    (opcode & 0xF6) == 0xF6)  { 
				modRM = *func++; 
			} 
		} 
		else { 
			if (((opcode & 0xF0) == 0x00 && (opcode & 0x0F) >= 0x04 && (opcode & 0x0D) != 0x0D) || 
			    (opcode & 0xF0) == 0x30 || 
			    opcode == 0x77 || 
			    (opcode & 0xF0) == 0x80 || 
			    ((opcode & 0xF0) == 0xA0 && (opcode & 0x07) <= 0x02) || 
			    (opcode & 0xF8) == 0xC8) { 
				// No mod R/M byte 
			} 
			else { 
				modRM = *func++; 
			} 
		} 
		// Skip SIB
		if ((modRM & 0x07) == 0x04 &&
		    (modRM & 0xC0) != 0xC0) {
			func += 1;   // SIB
		}

		// Skip displacement
		if ((modRM & 0xC5) == 0x05) func += 4;   // Dword displacement, no base 
		if ((modRM & 0xC0) == 0x40) func += 1;   // Byte displacement 
		if ((modRM & 0xC0) == 0x80) func += 4;   // Dword displacement 

		// Skip immediate 
		if (FPU) { 
			// Can't have immediate operand 
		} 
		else if(!twoByte) { 
			if ((opcode & 0xC7) == 0x04 || 
			    (opcode & 0xFE) == 0x6A ||   // PUSH/POP/IMUL 
			    (opcode & 0xF0) == 0x70 ||   // Jcc 
			    opcode == 0x80 || 
			    opcode == 0x83 || 
			    (opcode & 0xFD) == 0xA0 ||   // MOV 
			    opcode == 0xA8 ||            // TEST 
			    (opcode & 0xF8) == 0xB0 ||   // MOV
			    (opcode & 0xFE) == 0xC0 ||   // RCL 
			    opcode == 0xC6 ||            // MOV 
			    opcode == 0xCD ||            // INT 
			    (opcode & 0xFE) == 0xD4 ||   // AAD/AAM 
			    (opcode & 0xF8) == 0xE0 ||   // LOOP/JCXZ 
			    opcode == 0xEB || 
			    (opcode == 0xF6 && (modRM & 0x30) == 0x00)) {  // TEST
				func += 1; 
			} 
			else if((opcode & 0xF7) == 0xC2) { 
				func += 2;   // RET 
			} 
			else if ((opcode & 0xFC) == 0x80 || 
				 (opcode & 0xC7) == 0x05 || 
				 (opcode & 0xF8) == 0xB8 ||
				 (opcode & 0xFE) == 0xE8 ||      // CALL/Jcc 
				 (opcode & 0xFE) == 0x68 || 
				 (opcode & 0xFC) == 0xA0 || 
				 (opcode & 0xEE) == 0xA8 || 
				 opcode == 0xC7 || 
				 (opcode == 0xF7 && (modRM & 0x30) == 0x00)) { 
				func += operandSize; 
			} 
		} 
		else { 
			if (opcode == 0xBA ||            // BT 
			    opcode == 0x0F ||            // 3DNow! 
			    (opcode & 0xFC) == 0x70 ||   // PSLLW 
			    (opcode & 0xF7) == 0xA4 ||   // SHLD 
			    opcode == 0xC2 || 
			    opcode == 0xC4 || 
			    opcode == 0xC5 || 
			    opcode == 0xC6) { 
				func += 1; 
			} 
			else if((opcode & 0xF0) == 0x80) {
				func += operandSize;   // Jcc -i
			}
		}

		// count++;
	}

	// return count;
	*size = func - pfn + 2;
	return 0;
}

static int syncop_next(void)
{
        static int num_op = sizeof(sync_op.opid) / sizeof(sync_op.opid[0]);
        int n;

        n = sync_op.next;
        // NOTE: this is tricky and error prone; implement a better sync opertation
        if (sync_op.opid[n] != 1) {
                uloga("'%s()': error sync operation overflows.\n", __func__);
        }
        sync_op.opid[n] = 0;
        sync_op.next = (sync_op.next + 1) % num_op;

        return n;
}

static int * syncop_ref(int opid)
{
        return &sync_op.opid[opid];
}

static inline struct node_id * dcg_which_peer(void)
{
        int peer_id;
        struct node_id *peer;

        peer_id = dcg->dc->self->ptlmap.id % dcg->dc->num_sp;
        peer = dc_get_peer(dcg->dc, peer_id);

        return peer;
}

#ifdef DS_HAVE_ACTIVESPACE
/*
  RPC routine to receive results from code executed in the space.
*/
static int dcgrpc_code_reply(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_bin_result *hr = (typeof(hr)) cmd->pad;
	struct query_tran_entry *qte;
	int err = -ENOENT;

	qte = qt_find(&dcg->qt, hr->qid);
	if (!qte) 
		goto err_out;

	/* Copy  the   code  execution  result  back   on  the  result
	   reference.  There  can be  more  than  one partial  result,
	   depending on the object distribution in the space. */
	memcpy((char *) qte->data_ref + qte->num_parts_rec * qte->q_obj.size, 
		hr->pad, qte->q_obj.size);

	if (++qte->num_parts_rec == qte->size_od)
		qte->f_complete = 1;

	return 0;
 err_out:
	ERROR_TRACE();
}
#endif // end of #ifdef DS_HAVE_ACTIVESPACE


/*
  RPC routine to collect timing info from non-master app nodes.
*/
static int dcgrpc_collect_timing(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_timing *ht = (struct hdr_timing *) cmd->pad;

	demo_num_timing_recv++;

	if (ht->time_num == 1)
		demo_sum_timing += ht->time_tab[0];

	return 0;
}

// TODO: in the server free
static struct dcg_lock *lock_get(const char *lock_name, int should_alloc)
{
	struct dcg_lock *lock;

	list_for_each_entry(lock, &dcg->locks_list, struct dcg_lock, lock_entry) {
		if (!strncmp(lock->name, lock_name, sizeof(lock->name)-1))
			return lock;
	}

	if (!should_alloc)
		return NULL;

	lock = malloc(sizeof(*lock));
	if (!lock) 
		return NULL;
	memset(lock, 0, sizeof(*lock));

	strncpy(lock->name, lock_name, sizeof(lock->name));
	lock->name[sizeof(lock->name)-1] = '\0';

	list_add(&lock->lock_entry, &dcg->locks_list);
	return lock;
}

static void lock_free(void)
{
	struct dcg_lock *lock, *tlock;

	list_for_each_entry_safe(lock, tlock, &dcg->locks_list, struct dcg_lock, lock_entry) {
		list_del(&lock->lock_entry);
		free(lock);
	}
}

/*
  Routine to send lock/unlock requests to the master service peer.
*/
static int dcg_lock_request(struct dcg_lock *lock, enum lock_type type)
{
        struct node_id *peer;
        struct msg_buf *msg;
	struct lockhdr *lh;
        int err = -ENOMEM;

        peer = dc_get_peer(dcg->dc, 0);

        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = cp_lock;
        msg->msg_rpc->id = DCG_ID;      

	lh = (struct lockhdr *) msg->msg_rpc->pad;
	strcpy(lh->name, lock->name);
	lh->type = type;
        lh->lock_num = lock->lock_num;

        err = rpc_send(dcg->dc->rpc_s, peer, msg);
        if (err < 0) {
                free(msg);
                goto err_out;
        }

	lock->ack = 0;
	lock->req = 1;

        return 0;
 err_out:
	ERROR_TRACE();
}

/* 
   RPC routine to implement a locking service. This receives only lock
   acks.
*/
static int dcgrpc_lock_service(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct lockhdr *lh = (struct lockhdr *) cmd->pad;
	struct dcg_lock *lock;

	lock = lock_get(lh->name, 0);
	if (!lock) {
		int err = -ENOENT;
		ERROR_TRACE();
	}

        lock->lock_num = lh->lock_num;
        lock->ack = 1;
        lock->req = 0;

        return 0;
}

/*
  Test if we did receive all the parts for a distributed object.
*/
static int cq_received_all_parts(struct query_tran_entry *qte)
{
        struct obj_data *od;
        unsigned long g_vol, l_vol;

        g_vol = bbox_volume(&qte->q_obj.bb);
        l_vol = 0UL;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                /* We want all parts of an object to be at the same
                   version. */
                if (qte->q_obj.version != od->obj_desc.version)
                        break;

                l_vol += bbox_volume(&od->obj_desc.bb);
        }

        return (l_vol == g_vol);
}

/* Forward definition. */
static int dcg_obj_data_get(struct query_tran_entry *);

/*
  RPC routine to receive notifications  when the region of interest is
  updated.
*/
static int dcgrpc_obj_cq_update(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct obj_descriptor *odsc;
        struct query_tran_entry *qte;
        int err = -ENOENT;

        /* NOTE: if data is not  consumed between two updates, i.e., a
           call  to dcg_obj_cq_update(), there  are problems  ... most
           likely  app  will  block   or  sckip  one  version  of  the
           object. */
        qte = qt_find(&dcg->qt, oh->qid);
        if (!qte)
                goto err_out;

        /* Early discard; we are receiving an update for an old part
           of an object. */
        if (oh->u.o.odsc.version < qte->q_obj.version) {
                // uloga("'%s()': discard descriptor version %d, from %d, for %d.\n",
                //        __func__, oh->odsc.version, peer->id, oh->odsc.owner);
                uloga("'%s()': this should not happen: CASE I\n", __func__);
                return 0;
        }

        odsc = qt_find_obj(qte, &oh->u.o.odsc);
        // if (!qt_find_obj(qte, &oh->odsc)) {
        if (!odsc) {
                if ((err = qt_add_objv(qte, &oh->u.o.odsc)) < 0)
                        goto err_out;

                /* NOTE: we take the  latest object part arrived to be
                   the latest version, should ensure all the parts are
                   at the same version. */

                /* We assume  this is the latest obj  version; this is
                   to be  used in  obj_data_get() routine, and  has to
                   match the version on the server. */
                qte->q_obj.version = oh->u.o.odsc.version;

                qte->size_od++;
                // uloga("'%s()': accepted descriptor version %d, from %d for %d.\n",
                //        __func__, oh->odsc.version, peer->id, oh->odsc.owner);
        }
        else {
                uloga("'%s()': this should not happen: CASE II.\n", __func__);
                if (odsc->version <= oh->u.o.odsc.version) {
                        *odsc = oh->u.o.odsc;
                        /* Update the version on the query as well. */
                        qte->q_obj.version = odsc->version;

                        // uloga("'%s()': updated descriptor version %d, from %d"
                        //      " for %d.\n", 
                        //      __func__, oh->odsc.version, peer->id, oh->odsc.owner);
                }
                else {
                        // uloga("'%s()': duplicate descriptor from %d, original from %d.\n",
                        // __func__, peer->id, oh->odsc.owner);
                        // uloga("'%s()': new version is %d, local version is %d.\n",
                        //      __func__, oh->odsc.version, odsc->version);
                        return 0;
                }
        }

        if (cq_received_all_parts(qte)) {
                qte->num_parts_rec = 0;
                if ((err = dcg_obj_data_get(qte)) < 0)
                        goto err_out;
        }

        return 0;
 err_out:
        ERROR_TRACE();
}

static int get_dht_peers_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct query_tran_entry *qte = msg->private;
        int i = 0;

        while (i < qte->qh->qh_size && qte->qh->qh_peerid_tab[i] != -1)
                i++;
        qte->qh->qh_num_peer = i;
        qte->f_peer_received = 1;

        free(msg);

        return 0;
}

/* 
   Util function to retrieve the DHT peer ids for an object descriptor
   from the server peer. The object descriptor should be embedded in a
   query transaction entry structure.
*/
static int get_dht_peers(struct query_tran_entry *qte)
{
        struct hdr_obj_get *oh;
        struct msg_buf *msg;
        struct node_id *peer;
        int err = -ENOMEM;

        peer = dcg_which_peer();
        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
        if (!msg) 
                goto err_out;

        msg->msg_rpc->cmd = ss_obj_get_dht_peers;
        msg->msg_rpc->id = DCG_ID;
        msg->msg_data = qte->qh->qh_peerid_tab;
        msg->size = sizeof(int) * qte->qh->qh_size;
        msg->cb = get_dht_peers_completion;
        msg->private = qte;

        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->qid = qte->q_id;
        oh->u.o.odsc = qte->q_obj;
        oh->rank = DCG_ID;
        memcpy(&oh->gdim, &qte->gdim, sizeof(struct global_dimension));

        err = rpc_receive(dcg->dc->rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(msg);
 err_out:
        ERROR_TRACE();
}

/*
  Util function to retrieve the object descriptors from DHT peers for
  the object parts that intersect with the object descriptor in the
  query.
*/
static int get_obj_descriptors(struct query_tran_entry *qte)
{
        struct hdr_obj_get *oh;
        struct node_id *peer;
        struct msg_buf *msg;
        int *peer_id, err;

        qte->f_odsc_recv = 0;
        peer_id = qte->qh->qh_peerid_tab;
        while (*peer_id != -1) {
                peer = dc_get_peer(dcg->dc, *peer_id);

                err = -ENOMEM;
                msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_rpc->cmd = ss_obj_get_desc;
                msg->msg_rpc->id = DCG_ID;

                oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
                oh->qid = qte->q_id;
                oh->u.o.odsc = qte->q_obj;
                oh->rank = DCG_ID; 
                memcpy(&oh->gdim, &qte->gdim,
                    sizeof(struct global_dimension)); 

                qte->qh->qh_num_req_posted++;
                err = rpc_send(dcg->dc->rpc_s, peer, msg);
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

#if 0
/*
  DHT peers completion and initiate requests for object descriptors.
*/
static int obj_get_dht_peers_completion(void *dcg_ref, struct msg_buf *msg) // __attribute__((unused))
{
        struct query_tran_entry *qte = msg->private;
        struct node_id *peer;
        struct hdr_obj_get *oh;
        int i, err;

        free(msg);

        for (i = 0; i < qte->dht_peer_num; i++) {
                err = -ENOMEM;
                peer = dc_get_peer(dcg->dc, qte->dht_peer_id_tab[i]);

                msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
                if (!msg)
                        break;

                msg->msg_rpc->cmd = ss_obj_get_desc;
                msg->msg_rpc->id = DCG_ID; // dcg->dc->self->id;

                oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
                oh->qid = qte->q_id;
                oh->odsc = qte->q_obj;
                oh->rank = DCG_ID; // dcg->dc->self->id;

                qte->dht_peer_req++;
                err = rpc_send(dcg->dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        qte->dht_peer_req--;
                        break;
                }
        }

        if (i != qte->dht_peer_num)
                goto err_out;

        return 0;
 err_out:
        uloga("'%s()' failed with %d.\n", __func__, err);
        return err;
}

/*
  Rpc step 1 of an 'obj_get()' transaction.  Get the DHT peer ids that
  have object descriptors for our query object.
*/
static int dcgrpc_obj_get_dht_peers(struct rpc_server *rpc_s, struct rpc_cmd *cmd) // __attribute__((unused))
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer = dc_get_peer(dcg->dc, cmd->id);
        struct query_tran_entry *qte;
        struct msg_buf *msg;
        int err = -ENOENT;

        qte = qt_find(&dcg->qt, oh->qid);
        if (!qte)
                goto err_out;

        qte->dht_peer_num = oh->num_de;
        if (qte->dht_peer_num > 64) {
                uloga("'%s()': PANIC enlarge dht_peer_id_tab.\n", __func__);
                goto err_out;
        }

        err = -ENOMEM;
        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->size = sizeof(int) * qte->dht_peer_num;
        msg->msg_data = qte->dht_peer_id_tab;
        msg->private = qte;
        msg->cb = obj_get_dht_peers_completion;
        //peer->mb = cmd->mbits;

	rpc_mem_info_cache(peer, cmd);

        err = rpc_receive_direct(rpc_s, peer, msg);
        peer->mb = MB_RPC_MSG;
        if (err < 0) {
                free(msg);
                goto err_out;
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}
#endif /* 0 */

static int obj_data_get_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct query_tran_entry *qte = msg->private;
        /*
        qte->num_reply++;
        if (qte->num_req == qte->num_od && qte->num_reply == qte->num_req)
                qte->f_complete = 1;
        */

        if (++qte->num_parts_rec == qte->size_od) {
                qte->f_complete = 1;
        }

        free(msg);
        return 0;
}

/*
  Fetch a data object from the distributed storage. We call this
  routine when we have all object descriptors for all parts.
*/
static int dcg_obj_data_get(struct query_tran_entry *qte)
{
        struct msg_buf *msg;
        struct node_id *peer;
        struct hdr_obj_get *oh;
        struct obj_data *od;
        int err;

        err = qt_alloc_obj_data(qte);
        if (err < 0)
                goto err_out;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                peer = dc_get_peer(dcg->dc, od->obj_desc.owner);

                err = -ENOMEM;
                msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
                if (!msg) {
                        free(od->data);
                        od->data = NULL;
                        goto err_out;
                }

                msg->msg_data = od->data;
                msg->size = obj_data_size(&od->obj_desc);
                msg->cb = obj_data_get_completion;
                msg->private = qte;

                msg->msg_rpc->cmd = ss_obj_get;
                msg->msg_rpc->id = DCG_ID;

                oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
                oh->qid = qte->q_id;
                oh->u.o.odsc = od->obj_desc;
                oh->u.o.odsc.version = qte->q_obj.version;
                memcpy(&oh->gdim, &qte->gdim,
                    sizeof(struct global_dimension));

                err = rpc_receive(dcg->dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        free(od->data);
                        od->data = NULL;
                        goto err_out;
                }
                // TODO: uncomment next line ?!
                // qte->num_req++;
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Initiate a custom filter retrieve operation.
  TODO: add a parameter for cusom functions ... 
*/
static int obj_filter_init(struct query_tran_entry *qte)
{
        struct node_id *peer;
        struct msg_buf *msg;
        struct hdr_obj_filter *hf;
        struct obj_data *od;
        int err;

        // TODO: data size if fixed to double; make it dynamic.
        err = qt_alloc_obj_data_with_size(qte, sizeof(double));
        if (err < 0)
                goto err_out;

        list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
                peer = dc_get_peer(dcg->dc, od->obj_desc.owner);

                err = -ENOMEM;
                msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_data = od->data;
                msg->size = sizeof(double);
                msg->cb = obj_data_get_completion;
                msg->private = qte;

                msg->msg_rpc->cmd = ss_obj_filter;
                msg->msg_rpc->id = DCG_ID; // dcg->dc->self->id;

                hf = (struct hdr_obj_filter *) msg->msg_rpc->pad;
                hf->qid = qte->q_id;
                hf->rc = 0;
                hf->odsc = od->obj_desc;
                hf->odsc.version = qte->q_obj.version;

                err = rpc_receive(dcg->dc->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        goto err_out;
                }
        }

        return 0;
 err_out:
        ERROR_TRACE();
}

static int obj_filter_reduce(struct query_tran_entry *qte, struct obj_data *od)
{
        struct obj_data *odt;
        double *pa, *pb;
        int first = 1;

        pb = od->data;
        list_for_each_entry(odt, &qte->od_list, struct obj_data, obj_entry) {
                pa = odt->data;
                if (first) {
                        first = 0;
                        *pb = *pa;
                }
                else if (*pa < *pb)
                        *pb = *pa;
        }

        return 0;
}

static int obj_get_desc_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct hdr_obj_get *oh = msg->private;
        struct obj_descriptor *od_tab = msg->msg_data;
        struct query_tran_entry *qte;
        int i, err = -ENOENT;

        qte = qt_find(&dcg->qt, oh->qid);
        if (!qte) {
		uloga("can not find transaction ID = %d.\n", oh->qid);
                goto err_out_free;
	}

        qte->qh->qh_num_rep_received++;
        qte->size_od += oh->u.o.num_de;

        for (i = 0; i < oh->u.o.num_de; i++) {
                if (!qt_find_obj(qte, od_tab+i)) {
                        err = qt_add_obj(qte, od_tab+i);
			// err = qt_add_objv(qte, od_tab+i);
                        if (err < 0)
                                goto err_out_free;
                }
                else {
			/* DEBUG:
                        uloga("'%s()': duplicate obj descriptor detected, "
                              "keep only one copy.\n", __func__);
			*/
                        qte->size_od--;
                }
        }

        free(oh);
        free(od_tab);
        free(msg);

        if (qte->qh->qh_num_rep_received == qte->qh->qh_num_peer) {
                /* Object descriptor receive completed. */
                qte->f_odsc_recv = 1;

		/* NOTE: disabled the cache for object descriptors.
                if (qte->f_err == 0) {
                        struct query_cache_entry *qce;
                        qce = qce_alloc(qte->num_od);
                        if (qce) {
                                qce_set_obj_desc(qce, &qte->q_obj, &qte->od_list);
                                qc_add_entry(&dcg->qc, qce);
                        }
                }
		*/
        }

        return 0;
 err_out_free:
        free(oh);
        free(od_tab);
        free(msg);

	ERROR_TRACE();
}

static void versions_reset(void)
{
	memset(dcg->versions, 0, sizeof(dcg->versions));
	dcg->num_vers = 0;
}

static void versions_add(int n, int versions[])
{
	int i, j;

	for (i = 0; i < n; i++) {
		for (j = 0; j < dcg->num_vers; j++)
			if (dcg->versions[j] == versions[i])
				break;
		if (j == dcg->num_vers) 
			dcg->versions[dcg->num_vers++] = versions[i];
	}
}

/*
  RPC routine to receive object descriptors from DHT nodes.
*/
static int dcgrpc_obj_get_desc(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oht, *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer = dc_get_peer(dcg->dc, cmd->id);
        struct obj_descriptor *od_tab;
        struct msg_buf *msg;
        int err = -ENOMEM;

        /* Test for errors ... */
        if (oh->rc < 0) {
		/* TODO: copy versions available if any !!! */
                struct query_tran_entry *qte = qt_find(&dcg->qt, oh->qid);
                if (!qte) {
                        err = -ENOENT;
                        goto err_out;
                }
                qte->qh->qh_num_rep_received++; 
                if (qte->qh->qh_num_rep_received == qte->qh->qh_num_peer)
                        qte->f_odsc_recv = 1;
                qte->f_err = 1;

		versions_add(oh->u.v.num_vers, oh->u.v.versions);

                return 0;
        }
	//printf("sizeof obj_descriptor is %d.\n", sizeof(struct obj_descriptor));//debug
        od_tab = malloc(sizeof(*od_tab) * oh->u.o.num_de);
        if (!od_tab)
                goto err_out;

        oht = malloc(sizeof(*oh));
        if (!oht) {
                free(od_tab);
                goto err_out;
        }
        memcpy(oht, oh, sizeof(*oh));

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg) {
                free(od_tab);
                free(oht);
                goto err_out;
        }

        msg->size = sizeof(*od_tab) * oh->u.o.num_de;
        msg->msg_data = od_tab;
        msg->cb = obj_get_desc_completion;
        msg->private = oht;

        rpc_mem_info_cache(peer, msg, cmd); 
        err = rpc_receive_direct(rpc_s, peer, msg);
        rpc_mem_info_reset(peer, msg, cmd);	

        if (err == 0)
                return 0;

        free(od_tab);
        free(oht);
        free(msg);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Assemble the object 'od' from pieces in 'qte->od_list'.
*/
static int dcg_obj_assemble(struct query_tran_entry *qte, struct obj_data *od)
{
        int err;

        err = ssd_copy_list(od, &qte->od_list);
        if (err == 0)
                return 0;

        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Free resources after 'dcg_obj_put()' inserts an object in the space.
*/
static int obj_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct obj_data *od = msg->private;

        (*msg->sync_op_id) = 1;

        obj_data_free(od);
        free(msg);

        dcg_dec_pending();
        return 0;
}

/*
  Routine to receive space info.
*/
static int dcgrpc_ss_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_ss_info *hsi = (struct hdr_ss_info *) cmd->pad;

	dcg->ss_info.num_dims = hsi->num_dims;
	dcg->ss_info.num_space_srv = hsi->num_space_srv;
    dcg->ss_domain.num_dims = hsi->num_dims;
    dcg->default_gdim.ndim = hsi->num_dims;
    dcg->hash_version = hsi->hash_version;
    dcg->max_versions = hsi->max_versions;
	int i;
	for(i = 0; i < hsi->num_dims; i++){
		dcg->ss_domain.lb.c[i] = 0;
		dcg->ss_domain.ub.c[i] = hsi->dims.c[i]-1;
        dcg->default_gdim.sizes.c[i] = hsi->dims.c[i];
	}
	dcg->f_ss_info = 1;

	return 0;
}

/*
  Routine to log the timing information. NOTE: output goes to 'stdout' !
*/
static void time_log(int id, double time_tab[], int n)
{
        int i;

        fprintf(stderr, "%d,", id);
        for (i = 0; i < n; i++)
                fprintf(stderr, "%.6lf,", time_tab[i]);
        fprintf(stderr, "\n");
        // fflush(stdout);
}

/* 
   Routine to log remote timing information. 
*/
static int dcgrpc_time_log(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_timing *ht = (struct hdr_timing *) cmd->pad;

        time_log(cmd->id, ht->time_tab, ht->time_num);
        return 0;
}

/*
  Public API starts here.
*/

struct dcg_space *dcg_alloc(int num_nodes, int appid, void* comm)
{
        struct dcg_space *dcg_l;
        int i, err = -ENOMEM;

        if (dcg)
                return dcg;

        dcg_l = calloc(1, sizeof(*dcg_l));
        if (!dcg_l)
                goto err_out;


        for (i = 0; i < sizeof(sync_op.opid)/sizeof(sync_op.opid[0]); i++)
                sync_op.opid[i] = 1;

        dcg_l->num_pending = 0;
        qt_init(&dcg_l->qt);
        // rpc_add_service(ss_obj_get_dht_peers, dcgrpc_obj_get_dht_peers);
        rpc_add_service(ss_obj_get_desc, dcgrpc_obj_get_desc);
        rpc_add_service(ss_obj_cq_notify, dcgrpc_obj_cq_update);
        rpc_add_service(cp_lock, dcgrpc_lock_service);
        rpc_add_service(cn_timing, dcgrpc_time_log);
        rpc_add_service(ss_info, dcgrpc_ss_info);
#ifdef DS_HAVE_ACTIVESPACE
        rpc_add_service(ss_code_reply, dcgrpc_code_reply);
#endif
        /* Added for ccgrid demo. */
        rpc_add_service(CN_TIMING_AVG, dcgrpc_collect_timing);	
	
        dcg_l->dc = dc_alloc(num_nodes, appid, dcg_l, comm);
        if (!dcg_l->dc) {
                free(dcg_l);
                goto err_out;
        }

        INIT_LIST_HEAD(&dcg_l->locks_list);
        init_gdim_list(&dcg_l->gdim_list);    
        qc_init(&dcg_l->qc);
        dcg_l->hash_version = ssd_hash_version_v1; // set default hash version

#ifdef TIMING_PERF
        timer_init(&tm_perf, 1);
        timer_start(&tm_perf);
#endif
        dcg = dcg_l;
        return dcg_l;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return NULL;
}

int dcg_barrier(struct dcg_space *dcg)
{
        return dc_barrier(dcg->dc);
}

void dcg_free(struct dcg_space *dcg)
{
#ifdef DEBUG
        uloga("'%s()': num pending = %d.\n", __func__, dcg->num_pending);
#endif

	while (dcg->num_pending) {
	      dc_process(dcg->dc);
	}

    dc_free(dcg->dc);
    qc_free(&dcg->qc);
	lock_free();

    free_gdim_list(&dcg->gdim_list);
    free(dcg);
}

/*
*/
int dcg_obj_put(struct obj_data *od)
{
        struct msg_buf *msg;
        struct node_id *peer;
        struct hdr_obj_put *hdr; 
        int sync_op_id;
        int err = -ENOMEM;

        if (flag_set_mpi_rank) {
            int peer_id = mpi_rank % dcg->dc->num_sp;
            peer = dc_get_peer(dcg->dc, peer_id);
        } else {
            peer = dcg_which_peer();
        }

        sync_op_id = syncop_next();

        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_data = od->data;
        msg->size = obj_data_size(&od->obj_desc);
        msg->cb = obj_put_completion;
        msg->private = od;

        msg->sync_op_id = syncop_ref(sync_op_id);

        msg->msg_rpc->cmd = ss_obj_put;
        msg->msg_rpc->id = DCG_ID; // dcg->dc->self->id;

        hdr = msg->msg_rpc->pad;
        hdr->odsc = od->obj_desc;
        memcpy(&hdr->gdim, &od->gdim, sizeof(struct global_dimension));

        err = rpc_send(dcg->dc->rpc_s, peer, msg);
        if (err < 0) {
                free(msg);
                goto err_out;
        }

        dcg_inc_pending();

        return sync_op_id;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/* 
   Register a region for continuous queries and return the transaction
   id; it will be used for transaction completion checks.
*/
int dcg_obj_cq_register(struct obj_data *od)
{
        struct query_tran_entry *qte;
        struct msg_buf *msg;
        struct node_id *peer;
        struct hdr_obj_get *oh;
        int err = -ENOMEM;

        qte = qte_alloc(od, 1);
        if (!qte)
                goto err_out;

        peer = dcg_which_peer();

        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        qt_add(&dcg->qt, qte);

        msg->msg_rpc->cmd = ss_obj_cq_register;
        msg->msg_rpc->id = DCG_ID; // dcg->dc->self->id;

        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->rank = DCG_ID; // dcg->dc->self->id;
        oh->qid = qte->q_id;
        oh->u.o.odsc = od->obj_desc;

        err = rpc_send(dcg->dc->rpc_s, peer, msg);
        if (err == 0)
                return qte->q_id;

        qt_remove(&dcg->qt, qte);
 err_out:
        ERROR_TRACE();
}

int dcg_obj_cq_update(int cq_id)
{
        struct query_tran_entry *qte;
	// struct obj_data od;
        int err = 0;

        qte = qt_find(&dcg->qt, cq_id);
        if (!qte) {
		uloga("'%s()': looking for qte id %d.\n", __func__, cq_id);
		err = -ENOENT;
		goto err_out;
	}

        while (!qte->f_complete) {
                err = dc_process(dcg->dc);
                if (err < 0)
                        goto err_out;
        }
        qte->f_complete = 0;

	/*
        // TODO: do I need more fields here ?!
        od.data = qte->data_ref;
        od.obj_desc = qte->q_obj;

        err = dcg_obj_assemble(qte, &od);
	*/
        qt_free_obj_data(qte, 1);

        if (!list_empty(&qte->od_list)) {
                uloga("'%s()': ALERT something went wrong !!!\n", __func__);
        }

        // TODO: need some clean on qte values here.
        // qte->num_od = 0;
        // qte->size_od = 0;
        qte->num_parts_rec = 0;

	// TODO: where do I free the qte ?!

        if (err == 0)
                return 0;
 err_out:
        ERROR_TRACE();
}

int dcg_obj_sync(int sync_op_id)
{
        int *sync_op_ref = syncop_ref(sync_op_id);
        int err;

        while (sync_op_ref[0] != 1) {
                err = dc_process(dcg->dc);
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
*/
int dcg_obj_get(struct obj_data *od)
{
        struct query_tran_entry *qte;
        const struct query_cache_entry *qce;
        int err = -ENOMEM;
#ifdef TIMING_PERF
        double tm_st, tm_end;
        tm_st = timer_read(&tm_perf);
#endif

        qte = qte_alloc(od, 1);
        if (!qte)
                goto err_out;

        qt_add(&dcg->qt, qte);

        versions_reset();

        // TODO:  I have  intentionately  disabled the  cache here.  I
        // should reconsider.
        // qce = qc_find(&dcg->qc, &od->obj_desc);
        qce = 0;
        if (qce) {
                err = qte_set_odsc_from_cache(qte, qce);
                if (err < 0) {
                        free(qte);
                        goto err_out;
                }
        }
        else {
                err = get_dht_peers(qte);
                if (err < 0)
                        goto err_qt_free;
                DC_WAIT_COMPLETION(qte->f_peer_received == 1);

                err = get_obj_descriptors(qte);
                if (err < 0) {
                    if (err == -EAGAIN)
                        goto out_no_data;
                    else	goto err_qt_free;
                }
                DC_WAIT_COMPLETION(qte->f_odsc_recv == 1);
        }

        if (qte->f_err != 0) {
                err = -EAGAIN;
                goto out_no_data;
        }

#ifdef TIMING_PERF
        tm_end = timer_read(&tm_perf);
        uloga("TIMING_PERF locate_data ts %d peer %d time %lf %s\n",
            od->obj_desc.version, dcg_get_rank(dcg), tm_end-tm_st, log_header);
        tm_st = tm_end;
#endif

        err = dcg_obj_data_get(qte);
        if (err < 0) {
                // FIXME: should I jump to err_qt_free ?
                qt_free_obj_data(qte, 1);
                goto err_data_free; // err_out;
        }

        /* The request send succeeds, we can post the transaction to
           the list. */


        /* Wait for transaction to complete. */
        while (! qte->f_complete) {
                err = dc_process(dcg->dc);
                if (err < 0) {
                        uloga("'%s()': error %d.\n", __func__, err);
                        break;
                }
        }

        if (!qte->f_complete) {
                // !qte->num_req || qte->num_reply != qte->num_od) {
                /* Object is not complete, not all parts
                   successfull. */
                // qt_free_obj_data(qte, 1);
                err = -ENODATA;
                goto out_no_data;
        }

        err = dcg_obj_assemble(qte, od);
#ifdef TIMING_PERF
        tm_end = timer_read(&tm_perf);
        uloga("TIMING_PERF fetch_data ts %d peer %d time %lf %s\n",
            od->obj_desc.version, dcg_get_rank(dcg), tm_end-tm_st, log_header);
#endif 
 out_no_data:
        qt_free_obj_data(qte, 1);
        qt_remove(&dcg->qt, qte);
        free(qte);

        return err;
 err_data_free:
        qt_free_obj_data(qte, 1);
 err_qt_free:
        qt_remove(&dcg->qt, qte);
        free(qte);
 err_out:
        ERROR_TRACE();
}

int dcg_get_versions(int **p_version)
{
	static int versions[sizeof(dcg->versions)/sizeof(int)];

	memset(versions, 0, sizeof(versions));
	memcpy(versions, dcg->versions, dcg->num_vers);

	*p_version = versions;

	return dcg->num_vers;
}

/*
  Routine to implement the "custom" filters, e.g., min, max, avg, sum.
*/
int dcg_obj_filter(struct obj_data *od)
{
        struct query_tran_entry *qte;
        const struct query_cache_entry *qce;
        int err = -ENOMEM;

        qte = qte_alloc(od, 1);
        if (!qte)
                goto err_out;
        // DELETE: qt_set(qte, od);
        qt_add(&dcg->qt, qte);

        qce = qc_find(&dcg->qc, &od->obj_desc);
        if (qce) {
                err = qte_set_odsc_from_cache(qte, qce);
                if (err < 0) {
                        free(qte);
                        goto err_out;
                }
        }
        else {
                err = get_dht_peers(qte);
                if (err < 0)
                        goto err_out;
                DC_WAIT_COMPLETION(qte->f_peer_received == 1);

                err = get_obj_descriptors(qte);
                if (err < 0)
                        goto err_out;
                DC_WAIT_COMPLETION(qte->f_odsc_recv == 1);
        }

        err = obj_filter_init(qte);
        if (err < 0)
                goto err_out;
        DC_WAIT_COMPLETION(qte->f_complete == 1);

        err = obj_filter_reduce(qte, od);

        qt_free_obj_data(qte, 1);
        qt_remove(&dcg->qt, qte);
        free(qte);

        return err;
 err_out:
        ERROR_TRACE();
}

int dcg_ss_info(struct dcg_space *dcg, int *num_dims)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (dcg->f_ss_info) {
		*num_dims = dcg->ss_info.num_dims;
		return 0;
	}

	peer = dcg_which_peer();
	msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = ss_info;
	msg->msg_rpc->id = DCG_ID;

	err = rpc_send(dcg->dc->rpc_s, peer, msg);
	if (err < 0)
		goto err_out;

	DC_WAIT_COMPLETION(dcg->f_ss_info == 1);

	*num_dims = dcg->ss_info.num_dims;
	return 0;
 err_out:
	ERROR_TRACE();
}

int dcghlp_get_id(struct dcg_space *dcg)
{
        return DCG_ID; // dcg->dc->self->id;
}

int dcg_get_rank(struct dcg_space *dcg)
{
        // return dcg->dc->self->id - dcg->dc->cp_min_rank;
        return DCG_ID - dcg->dc->cp_min_rank;
}

int dcg_get_num_peers(struct dcg_space *dcg)
{
        return dcg->dc->cp_in_job;
}

int dcg_get_num_servers(struct dcg_space *dcg)
{  
   return dcg->dc->num_sp;
}

int dcg_get_num_space_peers(struct dcg_space *dcg)
{
	return dcg->dc->num_sp;
}

/* 
   Routine     to    log    the     timer    values.      Format    is
   "node_id,timer1[,timer2,...]"   the  sequence  and the  values  are
   arranged by the application and have the purpose of post-processing
   and plotting.
*/
int dcg_time_log(double time_tab[], int n)
{
        struct node_id *peer;
        struct msg_buf *msg;
        struct hdr_timing *ht;
        int i, err;

        // if (dcg->dc->self->id == dcg->dc->cp_min_rank) {
        if (DCG_ID == dcg->dc->cp_min_rank) {
                time_log(DCG_ID, time_tab, n);
                return 0;
        }

        err = -ENOMEM;
        peer = dc_get_peer(dcg->dc, dcg->dc->cp_min_rank);
        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = cn_timing;
        msg->msg_rpc->id = DCG_ID; // dcg->dc->self->id;

        ht = (struct hdr_timing *) msg->msg_rpc->pad;
        for (i = 0; i < n; i++)
                ht->time_tab[i] = time_tab[i];
        ht->time_num = n;

        err = rpc_send(dcg->dc->rpc_s, peer, msg);
        if (err == 0)
                return 0;
 err_out:
        ERROR_TRACE();
}

int dcg_remove(const char *var_name, unsigned int ver)
{
	int err = -ENOMEM;
	int i;
	for(i=0; i <dcg->dc->num_sp;i++){
	        struct node_id *peer;
	        struct msg_buf *msg;
	        //using struct lockhdr to transport var_name and version to server side
	        struct lockhdr *lh;

        	peer = dc_get_peer(dcg->dc, i);

	        msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
	        if (!msg)
        	        goto err_out;

	        msg->msg_rpc->cmd = cp_remove;
	        msg->msg_rpc->id = DCG_ID;

        	lh = (struct lockhdr *) msg->msg_rpc->pad;
	        strcpy(lh->name, var_name);
        	//using lock_num for version number
	        lh->lock_num = ver;

	        err = rpc_send(dcg->dc->rpc_s, peer, msg);
	        if (err < 0) {
        	        free(msg);
                	goto err_out;
	        }
	}

        return 0;
 err_out:
        ERROR_TRACE();
}




int dcg_lock_on_read(const char *lock_name, void *comm)
{
	struct dcg_lock *lock;
	int err = -ENOMEM;
	int myid, app_minid;

	lock = lock_get(lock_name, 1);
	if (!lock) 
		goto err_out;

	if (comm == NULL) {
		myid = DCG_ID;
		app_minid = dcg->dc->cp_min_rank;
	} else {
		MPI_Comm_rank(*(MPI_Comm *)comm, &myid);
		app_minid = 0;
	}

	if (myid == app_minid) {
		/* I am the master peer for this app job. */
		err = dcg_lock_request(lock, lk_read_get);
		if (err < 0)
			goto err_out;

		while (lock->ack == 0) {
			err = dc_process(dcg->dc);
			if (err < 0)
				goto err_out;
		}
	}


	if(comm == NULL){
		err = dc_barrier(dcg->dc);
		if (err == 0)
			return 0;
	}
	else{
		err = MPI_Barrier(*(MPI_Comm *)comm);
		if(err == MPI_SUCCESS)
			return 0;
	}

 err_out:
	ERROR_TRACE();
}

int dcg_unlock_on_read(const char *lock_name, void *comm)
{
	struct dcg_lock *lock;
	int err = -ENOMEM;
	int myid, app_minid;

	lock = lock_get(lock_name, 1);
	if (!lock)
		goto err_out;

	if(comm == NULL){
		err = dc_barrier(dcg->dc);
		if (err < 0)
			goto err_out;
	}
	else{
		err = MPI_Barrier(*(MPI_Comm *)comm);
		if(err != MPI_SUCCESS)
			goto err_out;
	}

	if (comm == NULL) {
		myid = DCG_ID;
		app_minid = dcg->dc->cp_min_rank;
	} else {
		MPI_Comm_rank(*(MPI_Comm *)comm, &myid);
		app_minid = 0;
	}

	if (myid == app_minid) {
		err = dcg_lock_request(lock, lk_read_release);
		if (err < 0)
			goto err_out;
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

int dcg_lock_on_write(const char *lock_name, void *comm)
{
	struct dcg_lock *lock;
	int err = -ENOMEM;
	int myid, app_minid;

	lock = lock_get(lock_name, 1);
	if (!lock)
		goto err_out;

	if (comm == NULL) {
		myid = DCG_ID;
		app_minid = dcg->dc->cp_min_rank;
	} else {
		MPI_Comm_rank(*(MPI_Comm *)comm, &myid);
		app_minid = 0;
	}

	if (myid == app_minid) {
		/* I am the master peer for this app job. */
		err = dcg_lock_request(lock, lk_write_get);
		if (err < 0)
			goto err_out;

		while (lock->ack == 0) {
			err = dc_process(dcg->dc);
			if (err < 0)
				goto err_out;
		}
	}

	if(comm == NULL){
		err = dc_barrier(dcg->dc);
		if (err == 0)
			return 0;
	}
	else{
		err = MPI_Barrier(*(MPI_Comm *)comm);
		if(err == MPI_SUCCESS)
			return 0;
	}

 err_out:
	ERROR_TRACE();
}

int dcg_unlock_on_write(const char *lock_name, void *comm)
{
	struct dcg_lock *lock;
	int err = -ENOMEM;
	int myid, app_minid;

	lock = lock_get(lock_name, 1);
	if (!lock)
		goto err_out;

	if(comm == NULL){
		err = dc_barrier(dcg->dc);
		if (err < 0)
			goto err_out;
	}
	else{
		err = MPI_Barrier(*(MPI_Comm *)comm);
		if(err != MPI_SUCCESS)
			goto err_out;
	}

	if (comm == NULL) {
		myid = DCG_ID;
		app_minid = dcg->dc->cp_min_rank;
	} else {
		MPI_Comm_rank(*(MPI_Comm *)comm, &myid);
		app_minid = 0;
	}

	if (myid == app_minid) {
		err = dcg_lock_request(lock, lk_write_release);
		if (err < 0)
			goto err_out;
	}

	//debug
	//printf("rank %d: barrier done.\n", dcg->dc->self->ptlmap.id);
	return 0;
 err_out:
	ERROR_TRACE();
}

#ifdef DS_HAVE_ACTIVESPACE
// TODO: move this to the non public API area !
static int 
dcg_code_rexec_at_peers(const void *fn_addr, int size, int off, 
			struct query_tran_entry *qte)
{
	struct node_id *peer;
	struct msg_buf *msg;
	struct hdr_bin_code *hc;
	struct obj_data *od;
	int err;

	list_for_each_entry(od, &qte->od_list, struct obj_data, obj_entry) {
		peer = dc_get_peer(dcg->dc, od->obj_desc.owner);

		err = -ENOMEM;
		msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = ss_code_put;
		msg->msg_rpc->id = DCG_ID;
		// NOTE: const fn_addr gets aliased here, you will get
		// a  warning from the  compiler; the  address however
		// will not be changed through the assigned pointer.
		msg->msg_data = fn_addr;
		msg->size = size;

		hc = (struct hdr_bin_code *) msg->msg_rpc->pad;
		hc->offset = off;
		hc->size = size;
		hc->odsc = od->obj_desc;
		hc->qid = qte->q_id;

		err = rpc_send(dcg->dc->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	return 0;
 err_out:
	ERROR_TRACE();
}


// TODO: make this synchronous, i.e., wait for the retrieval of the result !
int dcg_code_send(const void *addr, /* int off, int size, */ struct obj_data *od)
{
	struct query_tran_entry *qte;
	int n, err = -ENOMEM;
	int size, off;

	instructionCount(addr, &size, &off);
	// printf("Size: %d, offset: %d;\n", size, off);
	//	my_size, my_off, size, off);

	/* Do not allocate data for objects, e.g., 2nd param is 0. */
	qte = qte_alloc(od, 0);
	if (!qte)
		goto err_out;

	qt_add(&dcg->qt, qte);

	err = get_dht_peers(qte);
	if (err < 0)
		goto err_qte_free;
	DC_WAIT_COMPLETION(qte->f_peer_received == 1);

	err = get_obj_descriptors(qte);
	if (err < 0)
		goto err_qte_free;
	DC_WAIT_COMPLETION(qte->f_odsc_recv == 1);

	err = dcg_code_rexec_at_peers(addr, size, off, qte);
	if (err < 0)
		goto err_qte_free;

	// TODO: error path is not right here, should change
	// DC_WAIT_COMPLETION to break the loop instead of jumping to
	// err_out;
	DC_WAIT_COMPLETION(qte->f_complete == 1);

	n = qte->size_od;
	qt_free_obj_data(qte, 1);

	qt_remove(&dcg->qt, qte);
	qte_free(qte);

	return n;

 err_qte_free:
	qt_remove(&dcg->qt, qte);
	qte_free(qte);
 err_out:
	ERROR_TRACE();
}

/*
  Execute a function that does not return a result in the space.
*/
int dcg_rexe_voidfunc_exec(const void *addr)
{
	int size, off;

	instructionCount(addr, &size, &off);
	// TODO : continue from here ... 
	return 0;
}
#endif // end of #ifdef DS_HAVE_ACTIVESPACE

/*
  Collect timing information to master  node of app. And calculate the
  sum.
*/
int dcg_collect_timing(double time, double *sum_ptr)
{
	struct node_id *peer;
	struct msg_buf *msg;
	struct hdr_timing *ht;
	int i, err;
	
	if (dcg->dc->cp_min_rank == DCG_ID) {
		DC_WAIT_COMPLETION(
			demo_num_timing_recv == dcg->dc->cp_in_job-1);

		demo_sum_timing += time;

		if (sum_ptr)
			*sum_ptr = demo_sum_timing;	

		demo_num_timing_recv = 0;
		demo_sum_timing = 0;		

		return 0;
	}
	
	peer = dc_get_peer(dcg->dc, dcg->dc->cp_min_rank);
	msg = msg_buf_alloc(dcg->dc->rpc_s, peer, 1);
	if (!msg) {
		err = -ENOMEM;
		goto err_out;
	}

	msg->msg_rpc->cmd = CN_TIMING_AVG;
	msg->msg_rpc->id = DCG_ID;	

	ht = (struct hdr_timing *) msg->msg_rpc->pad;
	ht->time_tab[0] = time;
	ht->time_num = 1;

	err = rpc_send(dcg->dc->rpc_s, peer, msg);
	if (err == 0)
		return 0;
err_out:
	ERROR_TRACE();
}

int dcg_get_num_space_srv(struct dcg_space *dcg)
{
	return dcg->ss_info.num_space_srv;
}

void dcg_set_mpi_rank_hint(int rank)
{
    mpi_rank = rank;
    flag_set_mpi_rank = 1;
}

void dcg_unset_mpi_rank_hint()
{
    mpi_rank = 0;
    flag_set_mpi_rank = 0;
}

#ifdef TIMING_PERF
int common_dspaces_set_log_header(const char *str)
{
    strcpy(log_header, str);
    return 0;
}
#endif
