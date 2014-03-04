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

#include <sys/mman.h>

#include "debug.h"
#include "dart.h"
#include "ds_gspace.h"
#include "ss_data.h"
#ifdef DS_HAVE_ACTIVESPACE
#include "rexec.h"
#endif
#include "timer.h"
#include "strutil.h"

#ifdef DS_HAVE_FASTBIT
#include "fastspace.h"
#endif

#define DSG_ID                  dsg->ds->self->ptlmap.id

struct cont_query {
        int                     cq_id;
        int                     cq_rank;
        struct obj_descriptor   cq_odsc;

        struct list_head        cq_entry;
};

/* 
   Requests  that the  server can  not  handle right  away, they  will
   usually be queued in waiting lists.
*/
struct req_pending {
        struct list_head        req_entry;
        struct rpc_cmd          cmd;
};

enum lock_service {
        lock_unknown = 0,
        lock_generic,
        lock_custom
};

enum lock_state {
        unlocked = 0,
        locked
};

enum lock_action {
        la_none = 0,
        la_wait,
        la_grant,
        la_notify
};

struct dsg_lock {
	struct list_head	lk_entry;

	char			lk_name[LOCK_NAME_SIZE];

        int                     rd_notify_cnt; 
        int                     wr_notify_cnt;

        int                     rd_max, wr_max;

        int                     rd_epoch, wr_epoch;

        int                     rd_cnt;
        int                     wr_cnt;

        enum lock_state         rd_lock_state;
        enum lock_state         wr_lock_state;

        /* Waiting list for RCP lock requests. */
        struct list_head        wait_list;

        void                    (*init)(struct dsg_lock *, int);
        enum lock_action        (*process_request)(struct dsg_lock *, struct lockhdr *, int);
        int                     (*process_wait_list)(struct dsg_lock *);
        int                     (*service)(struct dsg_lock *, struct rpc_server *, struct rpc_cmd *);
};

static struct ds_gspace *dsg;
static struct timer timer;

/* Server configuration parameters */
static struct {
        int ndims;
        int dimx, dimy, dimz;
        int max_versions;
        int max_readers;
	int lock_type;		/* 1 - generic, 2 - custom */
} ds_conf;

static struct {
        const char      *opt;
        int             *pval;
} options[] = {
        {"ndim",                &ds_conf.ndims},
        {"dimx",                &ds_conf.dimx},
        {"dimy",                &ds_conf.dimy},
        {"dimz",                &ds_conf.dimz},
        {"max_versions",        &ds_conf.max_versions},
        {"max_readers",         &ds_conf.max_readers},
	{"lock_type",		&ds_conf.lock_type}
};

#ifdef DS_HAVE_FASTBIT
struct metaData mtdata;         /* meta_data to store attribute name, and type */

static struct query_result_list{
        struct list_head        qr_entry;
        void *                  qret;
        int                     size;
};
#endif

static void eat_spaces(char *line)
{
        char *t = line;

        while (t && *t) {
                if (*t != ' ' && *t != '\t' && *t != '\n')
                        *line++ = *t;
                t++;
        }
        if (line)
                *line = '\0';
}

static int parse_line(int lineno, char *line)
{
        char *t;
        int i, n;

        /* Comment line ? */
        if (line[0] == '#')
                return 0;

        t = strstr(line, "=");
        if (!t) {
                eat_spaces(line);
                if (strlen(line) == 0)
                        return 0;
                else    return -EINVAL;
        }

        t[0] = '\0';
        eat_spaces(line);
        t++;
        eat_spaces(t);

        n = sizeof(options) / sizeof(options[0]);
        for (i = 0; i < n; i++) {
                if (strcmp(line, options[i].opt) == 0) {
                        *options[i].pval = atoi(t);
                        break;
                }
        }

        if (i == n) {
                printf("Unknown option '%s' at line %d.\n", line, lineno);
        }
        return 0;
}

static int parse_conf(char *fname)
{
        FILE *fin;
        char buff[1024];
        int lineno = 1, err;

        fin = fopen(fname, "rt");
        if (!fin)
                return -errno;

        while (fgets(buff, sizeof(buff), fin) != NULL) {
                err = parse_line(lineno++, buff);
                if (err < 0) {
                        fclose(fin);
                        return err;
                }
        }

        fclose(fin);
        return 0;
}

static inline struct ds_gspace * dsg_ref_from_rpc(struct rpc_server *rpc_s)
{
        struct dart_server * ds = ds_ref_from_rpc(rpc_s);

        return ds->dart_ref;
}

#ifdef DS_HAVE_ACTIVESPACE
static int bin_code_local_bind(void *pbuf, int offset)
{
	PLT_TAB;
	unsigned char *pptr = pbuf;
	unsigned int *pui, i = 0;

	/*
	400410:       55                      push   %rbp
	400411:       48 89 e5                mov    %rsp,%rbp
	400414:       48 81 ec 80 00 00 00    sub    $0x80,%rsp
	40041b:       48 89 7d b8             mov    %rdi,0xffffffffffffffb8(%rbp)
	*/

	/*
	40041f:       48 c7 45 c0 c0 1f 41    movq   $0x411fc0,0xffffffffffffffc0(%rbp)
	400426:       00
	400427:       48 c7 45 c8 70 fc 40    movq   $0x40fc70,0xffffffffffffffc8(%rbp)
	40042e:       00
	40042f:       48 c7 45 d0 f0 3f 41    movq   $0x413ff0,0xffffffffffffffd0(%rbp)
	400436:       00
	400437:       48 c7 45 d8 c0 75 40    movq   $0x4075c0,0xffffffffffffffd8(%rbp)
	40043e:       00
	*/

	// pptr = pptr + 0x11; // 0xf;
	pptr = pptr + offset;

	for (i = 0; i < sizeof(plt)/sizeof(plt[0]); i++) {
		if (((pptr[2] >> 4) & 0xf) > 7) {
			pui = (unsigned int *) (pptr + 7);
			pptr = pptr + 11;
		}
		else {
			pui = (unsigned int *) (pptr + 4);
			pptr = pptr + 8;
		}

		*pui = (unsigned int) ((unsigned long) plt[i] & 0xFFFFFFFFUL);
		// pptr = pptr + 0x8;
	}

	return 0;
}

static int 
bin_code_local_exec(
	bin_code_fn_t ptr_fn, 
	struct obj_data *od, 
	struct obj_descriptor *req_desc, 
	struct rexec_args *rargs)
{
	struct bbox bb;
	int err;

	rargs->ptr_data_in = od->data;
	// NOTE:  I  can account  for  different data  representations
	// here, knowing that the code to execute is written in C ! (row major)
	rargs->ni = bbox_dist(&od->obj_desc.bb, bb_y);
	rargs->nj = bbox_dist(&od->obj_desc.bb, bb_x);
	rargs->nk = bbox_dist(&od->obj_desc.bb, bb_z);

	bbox_intersect(&od->obj_desc.bb, &req_desc->bb, &bb);
	bbox_to_origin(&bb, &od->obj_desc.bb);

	rargs->im = bb.lb.c[bb_y];
	rargs->jm = bb.lb.c[bb_x];
	rargs->km = bb.lb.c[bb_z];

	rargs->iM = bb.ub.c[bb_y];
	rargs->jM = bb.ub.c[bb_x];
	rargs->kM = bb.ub.c[bb_z];

	err = ptr_fn(rargs);
#ifdef DEBUG
	uloga("'%s()': execution succedeed, rc = %d.\n", __func__, err);
#endif
	return err;
}

static int bin_code_return_result(struct rexec_args *rargs, struct node_id *peer, int qid)
{
	struct msg_buf *msg;
	struct hdr_bin_result *hr;
	int err = -ENOMEM;

	msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = ss_code_reply;
	msg->msg_rpc->id = DSG_ID;

	hr = (typeof(hr)) msg->msg_rpc->pad;
	hr->qid = qid;
	// TODO: consider the case in which rargs->size > hr->pad
	memcpy(hr->pad, rargs->ptr_data_out, rargs->size_res);

	free(rargs->ptr_data_out);

	err = rpc_send(dsg->ds->rpc_s, peer, msg);
	if (err == 0)
		return 0;

	free(msg);
 err_out:
	ERROR_TRACE();
}

#define ALIGN_ADDR_AT_BYTES(addr, bytes)			\
do {								\
        unsigned long _a = (unsigned long) (addr);		\
        _a = (_a + bytes-1) & ~(bytes-1);			\
        (addr) = (void *) _a;					\
} while (0)

static int bin_code_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	struct hdr_bin_code *hc = (typeof(hc)) msg->msg_rpc->pad;
	struct rexec_args rargs;
	struct node_id *peer;
	int err;

	/* Looks like a stack overflow ... to me, let's see. */
	static int r_enter = 0;
	static int r_exit = 0;

	// void *ptr_res;

	r_enter++;
#ifdef DEBUG
	uloga("'%s()': got the code, should execute it.\n", __func__);
#endif
	bin_code_local_bind(msg->msg_data, hc->offset);

	/* NOTE: On Cray the heap is already marked as executable !
	err = mprotect(msg->msg_data, 1024, PROT_EXEC | PROT_READ | PROT_WRITE);
	if (err < 0) {
		ulog_err("'%s()': failed.\n", __func__);
	}
	*/

	struct obj_data *from_obj;

        from_obj = ls_find(dsg->ssd->storage, &hc->odsc);
	// TODO: what if you can not find it ?!

	memset(&rargs, 0, sizeof(rargs));

	err = bin_code_local_exec((bin_code_fn_t) msg->msg_data, 
			from_obj, &hc->odsc, &rargs);

	peer = ds_get_peer(dsg->ds, msg->peer->ptlmap.id);
	// TODO:  write the error  path here  ... msg->peer  is const;
	// work around the warning!!!
	err = bin_code_return_result(&rargs, peer, hc->qid);



	/*
	err = mprotect(msg->msg_data, 1024, PROT_READ | PROT_WRITE);
	if (err < 0) {
		uloga("'%s()': failed mprotect().\n", __func__);
	}
	*/

	free(msg->private);
	free(msg);

	r_exit++;

	return 0;
}

static int dsgrpc_bin_code_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
	struct hdr_bin_code *hc = (struct hdr_bin_code *) cmd->pad;
	struct msg_buf *msg;
	int err = -ENOMEM;

	msg = msg_buf_alloc(rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	/* Copy  the  command request;  this  is  only  needed in  the
	   completion routine. */
	memcpy(msg->msg_rpc, cmd, sizeof(*cmd));

	msg->private = 
	msg->msg_data = malloc(4096 + hc->size);
	ALIGN_ADDR_AT_BYTES(msg->msg_data, 4096);
	msg->size = hc->size;
	msg->cb = bin_code_put_completion;

	rpc_mem_info_cache(peer, msg, cmd); 
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	if(err == 0)
		return 0;

	free(msg->private);
	free(msg);
 err_out:
	ERROR_TRACE();
}
#endif // end of #ifdef DS_HAVE_ACTIVESPACE

/*
  Generic lock service.
*/
void lock_init(struct dsg_lock *dl, int max_readers)
{
        dl->rd_lock_state = 
        dl->wr_lock_state = unlocked;

        dl->rd_cnt = 
        dl->wr_cnt = 0;
}

/*
  Custom lock service.
*/
void sem_init(struct dsg_lock *dl, int max_readers)
{
        dl->wr_max = 1;
        dl->rd_max = max_readers;

        dl->wr_cnt = 1;
        dl->rd_cnt = 0;

        dl->wr_notify_cnt =
        dl->rd_notify_cnt = 0;

        dl->rd_epoch = 
        dl->wr_epoch = 0;
}

static int dsg_lock_put_on_wait(struct dsg_lock *dl, struct rpc_cmd *cmd)
{
        struct req_pending *rr;
        int err = -ENOMEM;

        rr = malloc(sizeof(*rr));
        if (!rr)
                goto err_out;

        memcpy(&rr->cmd, cmd, sizeof(*cmd));
        list_add_tail(&rr->req_entry, &dl->wait_list);

        return 0;
 err_out:
        ERROR_TRACE();
}

static int dsg_lock_grant(struct node_id *peer, struct lockhdr *lhr)
{
        struct msg_buf *msg;
        struct lockhdr *lh;
        int err = -ENOMEM;

        msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = cp_lock;
        msg->msg_rpc->id = DSG_ID;

        lh = (struct lockhdr *) msg->msg_rpc->pad;
	strcpy(lh->name, lhr->name);
        lh->type = lk_grant;
        lh->rc = 0;
        lh->id = lhr->id;
        lh->lock_num = lhr->lock_num + 1;

        err = rpc_send(dsg->ds->rpc_s, peer, msg);
        if (err == 0)
                return 0;
        free(msg);
 err_out:
        ERROR_TRACE();
}

static enum lock_action 
lock_process_request(struct dsg_lock *dl, struct lockhdr *lh, int may_grant)
{
        enum lock_action f_lock = la_none;

        switch (lh->type) {
        case lk_read_get:
                if (dl->wr_lock_state == unlocked && may_grant) {
                        dl->rd_lock_state = locked;
                        dl->rd_cnt++;
                        f_lock = la_grant;
                }
		else	f_lock = la_wait;
                break;

        case lk_read_release:
                dl->rd_cnt--;
                if (dl->rd_cnt == 0) {
                        dl->rd_lock_state = unlocked;
                        f_lock = la_notify;
                }
                break;

        case lk_write_get:
                if (dl->rd_lock_state == unlocked && dl->wr_cnt == 0) { 
                        dl->wr_lock_state = locked;
                        dl->wr_cnt++;
                        f_lock = la_grant;
                }
		else	f_lock = la_wait;
                break;

        case lk_write_release:
                dl->wr_cnt--;
                if (dl->wr_cnt == 0) {
                        dl->wr_lock_state = unlocked;
                        f_lock = la_notify;
                }
        }

        return f_lock;
}

static int lock_process_wait_list(struct dsg_lock *dl)
{
        struct req_pending *rr; // , *tmp;
        struct lockhdr *lh;
        struct node_id *peer;
        int err;

        while (! list_empty(&dl->wait_list)) {
                rr = list_entry(dl->wait_list.next, 
                                struct req_pending, req_entry);

                lh = (struct lockhdr *) rr->cmd.pad;
                switch (lock_process_request(dl, lh, 1)) {
                case la_none:
                        /* Nothing to do. Yeah ... this should not happen! */
                case la_wait:
                        return 0;

                case la_grant:
                        peer = ds_get_peer(dsg->ds, rr->cmd.id);
                        err = dsg_lock_grant(peer, lh);
                        if (err < 0)
                                goto err_out;
                        break;

                case la_notify:
                        break;
                }

                list_del(&rr->req_entry);
                free(rr);
        }

        return 0;
 err_out:
        ERROR_TRACE();
}

static enum lock_action 
sem_process_request(struct dsg_lock *dl, struct lockhdr *lh, int may_grant)
{
        enum lock_action f_lock = la_none;

        switch (lh->type) {
        case lk_read_get:
                f_lock = la_wait;
                if (dl->rd_cnt > 0 && lh->lock_num == dl->rd_epoch) {
                        dl->rd_cnt--;
                        f_lock = la_grant;
                }
                break;

        case lk_read_release:
                dl->rd_notify_cnt++;
                f_lock = la_none;
                if (dl->rd_notify_cnt == dl->rd_max) {
                        dl->rd_notify_cnt = 0;
                        dl->wr_cnt = dl->wr_max;
                        f_lock = la_notify;

                        /* All  read locks have  been released,  so we
                           can step to the next round. */
                        dl->rd_epoch++;
                }
                break;

        case lk_write_get:
                f_lock = la_wait;
                if (dl->wr_cnt > 0 && lh->lock_num == dl->wr_epoch) {
                        dl->wr_cnt--;
                        f_lock = la_grant;
                }
                break;

        case lk_write_release:
                dl->wr_notify_cnt++;
                f_lock = la_none;
                if (dl->wr_notify_cnt == dl->wr_max) {
                        dl->wr_notify_cnt = 0;
                        dl->rd_cnt = dl->rd_max;
                        f_lock = la_notify;

                        /* All write  locks have been  released, so we
                           can step to the next round. */
                        dl->wr_epoch++;
                }
                break;
        }

        return f_lock;
}

static int sem_process_wait_list(struct dsg_lock *dl)
{
        struct req_pending *rr, *tmp;
        struct lockhdr *lh;
        struct node_id *peer;
        int err;

        list_for_each_entry_safe(rr, tmp, &dl->wait_list, struct req_pending, req_entry) {
                lh = (struct lockhdr *) rr->cmd.pad;

                switch (sem_process_request(dl, lh, 1)) {
                case la_grant:
                        peer = ds_get_peer(dsg->ds, rr->cmd.id);
                        err = dsg_lock_grant(peer, lh);
                        if (err < 0)
                                goto err_out;

                        list_del(&rr->req_entry);
                        free(rr);

                        /* I can  only grant one lock now;  it is safe
                           to break here. */
                        // return 0;
                        break;

                case la_notify:
                        uloga("'%s()': error unexpected notify rquest "
                              "from wait list.\n", __func__);

                case la_wait:
                case la_none:
                        break;
                }
        }

        return 0;
 err_out:
        ERROR_TRACE();
}


static int 
lock_service(struct dsg_lock *dl, struct rpc_server *rpc, struct rpc_cmd *cmd)
{
        struct lockhdr *lh = (struct lockhdr *) cmd->pad;
        struct node_id *peer;
        int err = 0;

        switch (lock_process_request(dl, lh, list_empty(&dl->wait_list))) {

        case la_none:
		break;

        case la_wait:
                err = dsg_lock_put_on_wait(dl, cmd);
                break;

        case la_notify:
                err = lock_process_wait_list(dl);
                break;

        case la_grant:
                peer = ds_get_peer(dsg->ds, cmd->id);
                err = dsg_lock_grant(peer, lh);
                break;
        }

        if (err == 0)
                return 0;
// err_out:
        ERROR_TRACE();
}

static int 
sem_service(struct dsg_lock *dl, struct rpc_server *rpc, struct rpc_cmd *cmd)
{
        struct lockhdr *lh = (struct lockhdr *) cmd->pad;
        struct node_id *peer;
        int err = 0;

        switch (sem_process_request(dl, lh, 0)) {

        case la_wait:
                err = dsg_lock_put_on_wait(dl, cmd);
                break;

        case la_notify:
                err = sem_process_wait_list(dl);
                break;

        case la_grant:
                peer = ds_get_peer(dsg->ds, cmd->id);
                err = dsg_lock_grant(peer, lh);
                break;

        case la_none:
                break;
        }

        if (err == 0)
                return 0;
        ERROR_TRACE();
}

static struct dsg_lock * dsg_lock_alloc(const char *lock_name,
	enum lock_service lock_type, int max_readers)
{
	struct dsg_lock *dl;

	dl = malloc(sizeof(*dl));
	if (!dl) {
		errno = ENOMEM;
		return dl;
	}
	memset(dl, 0, sizeof(*dl));

	strncpy(dl->lk_name, lock_name, sizeof(dl->lk_name));
	INIT_LIST_HEAD(&dl->wait_list);

        switch (lock_type) {
        case lock_generic:
                dl->init = &lock_init;
                dl->process_request = &lock_process_request;
                dl->process_wait_list = &lock_process_wait_list;
                dl->service = &lock_service;
#ifdef DEBUG
		uloga("'%s()': generic lock %s created.\n", 
			__func__, lock_name);
#endif
                break;

        case lock_custom:
                dl->init = &sem_init;
                dl->process_request = &sem_process_request;
                dl->process_wait_list = &sem_process_wait_list;
                dl->service = &sem_service;
#ifdef DEBUG
		uloga("'%s()': custom lock %s created.\n", 
			__func__, lock_name);
#endif
                break;

	default:
		// TODO: ERROR here, this should not happen. 
		break;
        }

        dl->init(dl, max_readers);

	list_add(&dl->lk_entry, &dsg->locks_list);

	return dl;
}

static struct dsg_lock *dsg_lock_find_by_name(const char *lock_name)
{
	struct dsg_lock *dl;

	list_for_each_entry(dl, &dsg->locks_list, struct dsg_lock, lk_entry) {
		if (strcmp(dl->lk_name, lock_name) == 0)
			return dl;
	}

	return 0;
}

/*
  Rpc routine to service lock requests.
*/
static int dsgrpc_lock_service(struct rpc_server *rpc, struct rpc_cmd *cmd)
{
	struct lockhdr *lh = (struct lockhdr *) cmd->pad;
	struct dsg_lock *dl;
        int err = -ENOMEM;

	// uloga("'%s()': lock %s.\n", __func__, lh->name);
	dl = dsg_lock_find_by_name(lh->name);

	if (!dl) {
		dl = dsg_lock_alloc(lh->name, 
			(ds_conf.lock_type == 1) ? lock_generic : lock_custom, 
			ds_conf.max_readers);
		/*
		struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);

		uloga("'%s()': lock '%s' created on server %d at request "
			"from compute peer %d.\n", 
			__func__, lh->name, DSG_ID, peer->ptlmap.id);
		*/

		if (!dl)
			goto err_out;
	}

	err = dl->service(dl, rpc, cmd);
	if (err == 0)
		return 0;

 err_out:
        ERROR_TRACE();
}

static struct cont_query *cq_alloc(struct hdr_obj_get *oh)
{
        struct cont_query *cq;

        cq = malloc(sizeof(*cq));
        if (!cq)
                return NULL;

        cq->cq_id = oh->qid;
        cq->cq_rank = oh->rank;
        cq->cq_odsc = oh->u.o.odsc;

        return cq;
}

static void cq_add_to_list(struct cont_query *cq)
{
        // TODO: add policy, e.g., add only new entries ?!
        list_add(&cq->cq_entry, &dsg->cq_list);
        dsg->cq_num++;
}

static void cq_rem_from_list(struct cont_query *cq)
{
        list_del(&cq->cq_entry);
        dsg->cq_num--;
}

static struct cont_query *cq_find_in_list(struct hdr_obj_get *oh)
{
        struct cont_query *cq;

        list_for_each_entry(cq, &dsg->cq_list, struct cont_query, cq_entry) {
                if (obj_desc_by_name_intersect(&cq->cq_odsc, &oh->u.o.odsc))
                        return cq;
        }

        return NULL;
}

static int cq_find_all_in_list(struct hdr_obj_get *oh, struct cont_query *cq_tab[])
{
        struct cont_query *cq;
        int cq_num = 0;

        list_for_each_entry(cq, &dsg->cq_list, struct cont_query, cq_entry) {
                if (obj_desc_by_name_intersect(&cq->cq_odsc, &oh->u.o.odsc))
                        cq_tab[cq_num++] = cq;
        }

        return cq_num;
}

static int cq_notify_on_match(struct cont_query *cq, struct obj_descriptor *odsc)
{
        struct node_id *peer;
        struct hdr_obj_get *oh;
        struct msg_buf *msg;
        struct bbox bbcom;
        int err = -ENOMEM;

        bbox_intersect(&cq->cq_odsc.bb, &odsc->bb, &bbcom);
        peer = ds_get_peer(dsg->ds, cq->cq_rank);

        msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = ss_obj_cq_notify;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        memcpy(oh->u.o.odsc.name, odsc->name, sizeof(odsc->name));
        oh->u.o.odsc.bb = bbcom;
        oh->u.o.odsc.owner = odsc->owner;
        oh->u.o.odsc.version = odsc->version;
        oh->u.o.odsc.size = odsc->size;
        oh->qid = cq->cq_id;

        // DEBUG: mark the latest version that we sent to the peer.
        cq->cq_odsc.version = odsc->version;

        err = rpc_send(dsg->ds->rpc_s, peer, msg);
        if (err == 0)
                return 0;
 err_out:
        ERROR_TRACE();
}

/*
  Check  in the  CQ list  if any  entry overlaps  with the  new object
  descriptor, and notify  the proper compute peer as  we have new data
  of interest for it.
*/
static int cq_check_match(struct obj_descriptor *odsc)
{
        struct cont_query *cq;
        int err;

        list_for_each_entry(cq, &dsg->cq_list, struct cont_query, cq_entry) {
                if (obj_desc_by_name_intersect(&cq->cq_odsc, odsc)) {
                        err = cq_notify_on_match(cq, odsc);
                        if (err < 0)
                                goto err_out;
                }
        }

        return 0;
 err_out:
        ERROR_TRACE();
}

static char * obj_desc_sprint(const struct obj_descriptor *odsc)
{
	char *str;
	int nb;

        nb = asprintf(&str, "obj_descriptor = {\n"
                "\t.name = %s,\n"
                "\t.owner = %d,\n"
                "\t.version = %d\n"
                "\t.bb = ", odsc->name, odsc->owner, odsc->version);
	str = str_append_const(str_append(str, bbox_sprint(&odsc->bb)), "}\n");

	return str;
}

/*
  Rpc routine to update (add or insert) an object descriptor in the
  dht table.
*/
static int dsgrpc_obj_update(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct dht_entry *de = dsg->ssd->ent_self;
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        int err;

#ifdef DEBUG
 {
	 char *str;

	 asprintf(&str, "S%2d: update obj_desc '%s' ver %d from S%2d for  ",
                DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
	 str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

	 uloga("'%s()': %s\n", __func__, str);
	 free(str);
 }
#endif
        oh->u.o.odsc.owner = cmd->id;
        err = dht_add_entry(de, &oh->u.o.odsc);
        if (err < 0)
                goto err_out;

        if (DSG_ID == oh->rank) {
                err = cq_check_match(&oh->u.o.odsc);
                if (err == 0)
                        return 0;
        }
        return 0;
 err_out:
        ERROR_TRACE();
}

/* 
   Update the DHT metadata with the new obj_descriptor information.
*/
static int obj_put_update_dht(struct ds_gspace *dsg, struct obj_descriptor *odsc)
{
	struct dht_entry *dht_tab[dsg->ssd->dht->num_entries];
	/* TODO: create a separate header structure for object
	   updates; for now just abuse the hdr_obj_get. */
	struct hdr_obj_get *oh;
	struct msg_buf *msg;
	struct node_id *peer;
	int num_de, i, min_rank, err;

	/* Compute object distribution to nodes in the space. */
	num_de = ssd_hash(dsg->ssd, &odsc->bb, dht_tab);
	if (num_de == 0) {
		uloga("'%s()': this should not happen, num_de == 0 ?!\n",
			__func__);
	}

	min_rank = dht_tab[0]->rank;
	/* Update object descriptors on the corresponding nodes. */
	for (i = 0; i < num_de; i++) {
		peer = ds_get_peer(dsg->ds, dht_tab[i]->rank);
		if (peer == dsg->ds->self) {
			// TODO: check if owner is set properly here.
			/*
			uloga("Obj desc version %d, for myself ... owner is %d\n",
				od->obj_desc.version, od->obj_desc.owner);
			*/
#ifdef DEBUG
			char *str;

			asprintf(&str, "S%2d: got obj_desc '%s' ver %d for ", 
				 DSG_ID, odsc->name, odsc->version);
			str = str_append(str, bbox_sprint(&odsc->bb));

			uloga("'%s()': %s\n", __func__, str);
			free(str);
#endif
			dht_add_entry(dsg->ssd->ent_self, odsc);
			if (peer->ptlmap.id == min_rank) {
				err = cq_check_match(odsc);
				if (err < 0)
					goto err_out;
			}
			continue;
		}
#ifdef DEBUG
		{
			char *str;

			asprintf(&str, "S%2d: fwd obj_desc '%s' to S%2d ver %d for ",
				 DSG_ID, odsc->name, peer->ptlmap.id, odsc->version);
			str = str_append(str, bbox_sprint(&odsc->bb));

			uloga("'%s()': %s\n", __func__, str);
			free(str);
		}
#endif
		err = -ENOMEM;
		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = ss_obj_update;
		msg->msg_rpc->id = DSG_ID;

		oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
		oh->u.o.odsc = *odsc;
		oh->rank = min_rank;

		err = rpc_send(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

/*
*/
static int obj_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct obj_data *od = msg->private;
	// int err = -ENOMEM;

        ssd_add_obj(dsg->ssd, od);

#ifdef DS_HAVE_FASTBIT
        int err = 0;
        struct timeval  tv1, tv2;

        gettimeofday(&tv1, NULL);
        err = build_obj_idx(od, &mtdata);
        gettimeofday(&tv2, NULL);

        if(err){
                printf("build_obj_idx failed\n");
                return err;
        }
        uloga("rank=%d Build index Total time = %f seconds\n",rpc_s->ptlmap.id,
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));
#endif

        free(msg);
#ifdef DEBUG
	uloga("'%s()': finished receiving  %s, version %d.\n",
		__func__, od->obj_desc.name, od->obj_desc.version);
#endif
	// obj_put_update_dht(...);

        return 0;
	/*
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
	*/
}

/*
*/
static int dsgrpc_obj_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct obj_descriptor *odsc = (struct obj_descriptor *) cmd->pad;
        struct obj_data *od;
        struct node_id *peer;
        struct msg_buf *msg;
        int err;

	odsc->owner = DSG_ID;

	err = -ENOMEM;
        peer = ds_get_peer(dsg->ds, cmd->id);
        od = obj_data_alloc(odsc);
        if (!od)
                goto err_out;

        od->obj_desc.owner = DSG_ID; // dsg->ds->self->id;

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg)
                goto err_free_data;

        msg->msg_data = od->data;
        msg->size = obj_data_size(&od->obj_desc);
        msg->private = od;
        msg->cb = obj_put_completion;

#ifdef DEBUG
	uloga("'%s()': start receiving %s, version %d.\n", 
		__func__, odsc->name, odsc->version);
#endif
	rpc_mem_info_cache(peer, msg, cmd); 
        err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);

        if (err < 0)
                goto err_free_msg;

	/* NOTE: This  early update, has  to be protected  by external
	   locks in the client code. */

	err = obj_put_update_dht(dsg, odsc);
	if (err == 0)
	        return 0;

 err_free_msg:
        free(msg);
 err_free_data:
        free(od);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int obj_info_reply_descriptor(
        struct node_id *q_peer,
        const struct obj_descriptor *q_odsc) // __attribute__((__unused__))
{
        struct msg_buf *msg;
        struct hdr_obj_get *oh;
        const struct obj_descriptor *loc_odsc;
        int err = -ENOMEM;

        msg = msg_buf_alloc(dsg->ds->rpc_s, q_peer, 1);
        if (!msg)
                return err;
        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;

        msg->msg_rpc->cmd = ss_obj_info;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        err = 0;
        loc_odsc = dht_find_entry(dsg->ssd->ent_self, q_odsc);
        if (!loc_odsc) {
                err = -ENOENT;
                // TODO: could send the odsc with the latest verssion
                // we have.  loc_odsc = dht_find_match();
        }
        else {
                oh->u.o.odsc = *loc_odsc;
                bbox_intersect(&oh->u.o.odsc.bb, &q_odsc->bb, &oh->u.o.odsc.bb);
        }
        oh->rc = err;

        err = rpc_send(dsg->ds->rpc_s, q_peer, msg);
        if (err == 0)
                return 0;

        free(msg);
        return err;
}

static int obj_query_reply_num_de(struct node_id *peer, int num_de)
{
        struct msg_buf *msg;
        struct hdr_obj_get *oh;
        int err = -ENOMEM;

        msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
        if (!msg)
                return err;

        msg->msg_rpc->cmd = ss_obj_query;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->u.o.num_de = num_de;

        err = rpc_send(dsg->ds->rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(msg);
        return err;
}

/*
*/
static int obj_query_forward_obj_info(struct dht_entry *de_tab[], int num_de, 
                                      struct hdr_obj_get *oh)
{
	//        struct dht_entry *de;
        struct msg_buf *msg;
        struct node_id *peer;
        int i, err, to_self = 0;

        for (i = 0; i < num_de; i++) {
                peer = ds_get_peer(dsg->ds, de_tab[i]->rank);
                if (peer == dsg->ds->self) {
                        to_self++;
                        continue;
                }

                err = -ENOMEM;
                msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
                if (!msg)
                        break;

                msg->msg_rpc->cmd = ss_obj_info;
                msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

                memcpy(msg->msg_rpc->pad, oh, sizeof(*oh));

                err = rpc_send(dsg->ds->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        break;
                }
        }

        if (i == num_de)
                return to_self;

        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int obj_send_dht_peers_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);

        return 0;
}

/*
  RPC routine to return the peer ids corresponding to DHT entries that
  have object descriptors for the data object being queried.
*/
static int dsgrpc_obj_send_dht_peers(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer;
        struct dht_entry *de_tab[dsg->ssd->dht->num_entries];
        struct msg_buf *msg;
        int *peer_id_tab, peer_num, i;
        int err = -ENOMEM;

        double tm_start, tm_end;
        static int num = 1;

        peer = ds_get_peer(dsg->ds, cmd->id);

        tm_start = timer_read(&timer);
        peer_num = ssd_hash(dsg->ssd, &oh->u.o.odsc.bb, de_tab);
        tm_end = timer_read(&timer);
#ifdef DEBUG
        printf("SRV %d %d %lf\n", DSG_ID, num++, tm_end-tm_start);
#endif
        peer_id_tab = malloc(sizeof(int) * (peer_num+1));
        if (!peer_id_tab)
                goto err_out;
        for (i = 0; i < peer_num; i++)
                peer_id_tab[i] = de_tab[i]->rank;
        /* The -1 here  is a marker for the end of the array. */
        peer_id_tab[peer_num] = -1;

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(peer_id_tab);
                goto err_out;
        }

        msg->msg_data = peer_id_tab;
        msg->size = sizeof(int) * (peer_num+1);
        msg->cb = obj_send_dht_peers_completion;

	rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_send_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
        if (err == 0)
                return 0;

        free(peer_id_tab);
        free(msg);
 err_out:
        ERROR_TRACE();
}


/*
  RPC routine to return the peer ids for the DHT entries that have
  descriptors for data objects.
*/
static int __dsgrpc_obj_send_dht_peers(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
        struct dht_entry *de_tab[dsg->ssd->dht->num_entries];
        struct msg_buf *msg;
        int *peer_id_tab, peer_num, i;
        int err = -ENOMEM;

        double tm_start, tm_end;
        static int num = 1;

        tm_start = timer_read(&timer);
        peer_num = ssd_hash(dsg->ssd, &oh->u.o.odsc.bb, de_tab);
        tm_end = timer_read(&timer);
#ifdef DEBUG
        printf("SRV %d %d %lf\n", DSG_ID, num++, tm_end-tm_start);
#endif
        peer_id_tab = malloc(sizeof(int) * peer_num);
        if (!peer_id_tab)
                goto err_out;

        for (i = 0; i < peer_num; i++)
                peer_id_tab[i] = de_tab[i]->rank;

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(peer_id_tab);
                goto err_out;
        }

        msg->size = sizeof(int) * peer_num;
        msg->msg_data = peer_id_tab;
        msg->cb = obj_send_dht_peers_completion;

        msg->msg_rpc->cmd = ss_obj_get_dht_peers;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        //TODO: do I need any other fields from the query transaction ?
        i = oh->qid;
        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->u.o.num_de = peer_num;
        oh->qid = i;

        err = rpc_send(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(peer_id_tab);
        free(msg);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Rpc routine to  locate the servers that may  have object descriptors
  that overlap the descriptor in the query.
*/
static int dsgrpc_obj_query(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
// __attribute__((__unused__))
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct dht_entry *de_tab[dsg->ssd->dht->num_entries];
	//        struct obj_descriptor odsc1;
	//        struct obj_descriptor const *odsc;
        struct node_id *peer;
        int num_de, err = -ENOMEM;

        num_de = ssd_hash(dsg->ssd, &oh->u.o.odsc.bb, de_tab);
        peer = ds_get_peer(dsg->ds, cmd->id);

        err = obj_query_reply_num_de(peer, num_de);
        if (err < 0)
                goto err_out;

        err = obj_query_forward_obj_info(de_tab, num_de, oh);
        if (err < 0)
                goto err_out;

        if (err > 0) {
                uloga("'%s()': should send obj info myself.\n", __func__);

                err = obj_info_reply_descriptor(peer, &oh->u.o.odsc);
                if (err < 0)
                        goto err_out;
                /*
                odsc = dht_find_entry(dsg->ssd->ent_self, &oh->odsc);
                err = -ENOENT;
                if (!odsc)
                        // goto err_out;

                odsc1 = *odsc;
                bbox_intersect(&odsc1.bb, &oh->odsc.bb, &odsc1.bb);
                peer = ds_get_peer(dsg->ds, oh->rank);
                err = obj_info_reply_descriptor(peer, &odsc1, err);
                if (err < 0)
                        goto err_out;
                */
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int dsgrpc_obj_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
// __attribute__((__unused__))
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        // const struct obj_descriptor *odsc;
        // struct obj_descriptor odsc1;
        struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
        int err = -ENOENT;

        peer = ds_get_peer(dsg->ds, oh->rank);
        err = obj_info_reply_descriptor(peer, &oh->u.o.odsc);
        if (err < 0)
                goto err_out;
        /*
        odsc = dht_find_entry(dsg->ssd->ent_self, &oh->odsc);
        if (!odsc)
                goto err_out;

        odsc1 = *odsc;
        bbox_intersect(&oh->odsc.bb, &odsc1.bb, &odsc1.bb);

        peer = ds_get_peer(dsg->ds, oh->rank);
        err = obj_info_reply_descriptor(&odsc1, peer);
        if (err < 0)
                goto err_out;
        */
        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int obj_desc_req_add_pending(struct rpc_cmd *cmd)
{
        struct req_pending *rp;
        int err = -ENOMEM;

        rp = malloc(sizeof(*rp));
        if (!rp)
                goto err_out;

        rp->cmd = *cmd;
        list_add(&rp->req_entry, &dsg->obj_desc_req_list);

        return 0;
 err_out:
        ERROR_TRACE();
}

/*
  Send an  object not  found erorr message  to the  requesting compute
  peer, and a list of object versions available in the space.
*/
static int obj_desc_not_found(struct node_id *peer, int qid, int num_vers, int *versions)
{
        struct msg_buf *msg;
        struct hdr_obj_get *oh;
        int err = -ENOMEM;

        msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;
        msg->msg_rpc->cmd = ss_obj_get_desc;

        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->rc = -ENOENT;
        oh->qid = qid;
	oh->u.v.num_vers = num_vers;
	memcpy(oh->u.v.versions, versions, num_vers * sizeof(int));

        err = rpc_send(dsg->ds->rpc_s, peer, msg);
        if (err == 0)
                return 0;
 err_out:
        ERROR_TRACE();
}

static int obj_get_desc_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg->msg_data);
        free(msg);
        return 0;
}

/*
  RPC  routine to  send the  object  descriptors that  match the  data
  object being queried.
*/
static int dsgrpc_obj_get_desc(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
        struct obj_descriptor odsc, *odsc_tab;
        const struct obj_descriptor *podsc[dsg->ssd->ent_self->odsc_num];
	int obj_versions[dsg->ssd->ent_self->odsc_size];
        int num_odsc, i;
        struct msg_buf *msg;
        int err = -ENOENT;

        num_odsc = dht_find_entry_all(dsg->ssd->ent_self, &oh->u.o.odsc, podsc);
        // TODO: mark dht_find_entry as unused. 
        // podsc = dht_find_entry(dsg->ssd->ent_self, &oh->odsc);
        if (!num_odsc) {
#ifdef DEBUG
		char *str = 0;

                asprintf(&str, "S%2d: obj_desc not found for ", DSG_ID); 
		str = str_append(str, obj_desc_sprint(&oh->u.o.odsc));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
#endif
		i = dht_find_versions(dsg->ssd->ent_self, &oh->u.o.odsc, obj_versions);
                err = obj_desc_not_found(peer, oh->qid, i, obj_versions);
                if (err < 0)
                        goto err_out;

                return 0;
                /* TODO: a pending list on the descriptors may not be
                   such a good idea. -yeah .. why ?
                err = obj_desc_req_add_pending(cmd);
                if (err < 0)
                        goto err_out;
                return 0;
                */
        }

        err = -ENOMEM;
        odsc_tab = malloc(sizeof(*odsc_tab) * num_odsc);
        if (!odsc_tab)
                goto err_out;

	for (i = 0; i < num_odsc; i++) {
		odsc = *podsc[i];
		/* Preserve storage type at the destination. */
		odsc.st = oh->u.o.odsc.st;
		bbox_intersect(&oh->u.o.odsc.bb, &odsc.bb, &odsc.bb);
		odsc_tab[i] = odsc;
        }

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(odsc_tab);
                goto err_out;
        }

        msg->size = sizeof(*odsc_tab) * num_odsc;
        msg->msg_data = odsc_tab;
        msg->cb = obj_get_desc_completion;

        msg->msg_rpc->cmd = ss_obj_get_desc;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        i = oh->qid;
        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        oh->u.o.num_de = num_odsc;
        oh->qid = i;

        err = rpc_send(rpc_s, peer, msg);
        if (err == 0)
                return 0;

        free(odsc_tab);
        free(msg);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int obj_cq_local_register(struct hdr_obj_get *oh)
{
        struct cont_query *cq;
        int err = -ENOMEM;

        cq = cq_alloc(oh);
        if (!cq)
                goto err_out;

        cq_add_to_list(cq);

        return 0;
 err_out:
        ERROR_TRACE();
}

static int obj_cq_forward_register(struct hdr_obj_get *oh)
{
        struct msg_buf *msg;
        struct dht_entry *de_tab[dsg->ssd->dht->num_entries];
        struct node_id *peer;
        int num_de, i, err;

        oh->rc = 1;
        num_de = ssd_hash(dsg->ssd, &oh->u.o.odsc.bb, de_tab);

        for (i = 0; i < num_de; i++) {
                peer = ds_get_peer(dsg->ds, de_tab[i]->rank);
                if (peer == dsg->ds->self) {
                        err = obj_cq_local_register(oh);
                        if (err < 0)
                                goto err_out;
                        continue;
                }

                err = -ENOMEM;
                msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->msg_rpc->cmd = ss_obj_cq_register;
                msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

                memcpy(msg->msg_rpc->pad, oh, sizeof(*oh));

                err = rpc_send(dsg->ds->rpc_s, peer, msg);
                if (err < 0) {
                        free(msg);
                        goto err_out;
                }
        }

        return 0;
 err_out:
        ERROR_TRACE();
}

/*
  RPC routine  to register a  CQ (continuous query);  possible callers
  are (1) compute  peer to directly register a CQ,  or (2) server peer
  to forward a CQ registration to proper DHT entries.
*/
static int dsgrpc_obj_cq_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        int err;

        if (oh->rc == 0) {
                err = obj_cq_forward_register(oh);
        }
        else {
                err = obj_cq_local_register(oh);
        }

        if (err == 0)
                return 0;
// err_out:
        ERROR_TRACE();
}

static int obj_get_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct obj_data *od = msg->private;

        free(msg);
        obj_data_free(od);

        return 0;
}

/*
  Rpc routine  to respond to  an 'ss_obj_get' request; we  assume that
  the requesting peer knows we have the data.
*/
static int dsgrpc_obj_get(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        struct obj_data *od, *from_obj;
	int fast_v;
        int err = -ENOENT; 

        peer = ds_get_peer(dsg->ds, cmd->id);

#ifdef DEBUG
 {
	 char *str;
	 
	 asprintf(&str, "S%2d: request for obj_desc '%s' ver %d from C%2d for  ",
		  DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
	 str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

 	 uloga("'%s()': %s\n", __func__, str);
	 free(str);
 }
#endif

        // CRITICAL: use version here !!!
        from_obj = ls_find(dsg->ssd->storage, &oh->u.o.odsc);
        // from_obj = ls_find_no_version(&dsg->ssd->storage, &oh->odsc);
        if (!from_obj) {
		char *str;
                str = obj_desc_sprint(&oh->u.o.odsc);
		uloga("'%s()': %s\n", __func__, str);
		free(str);

                goto err_out;
        }
        //TODO:  if required  object is  not  found, I  should send  a
        //proper error message back, and the remote node should handle
        //the error.

        /*
          Problem: How can I send  a transfer status together with the
          data ? (1)  piggyback it with the data,  (2) send a separate
          RPC in response to a transfer.
        */

	fast_v = 0; // ERROR: iovec operation fails after Cray Portals
		    // Update (oh->odsc.st == from_obj->obj_desc.st);

        err = -ENOMEM;
	// CRITICAL:     experimental    stuff,     assumption    data
	// representation is the same on both ends.
        // od = obj_data_alloc(&oh->odsc);
	od = (fast_v)? obj_data_allocv(&oh->u.o.odsc) : obj_data_alloc(&oh->u.o.odsc);
        if (!od)
                goto err_out;

        // ssd_copy(od, from_obj);
	(fast_v)? ssd_copyv(od, from_obj) : ssd_copy(od, from_obj);
        od->obj_ref = from_obj;

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg) {
                obj_data_free(od);
                goto err_out;
        }

        msg->msg_data = od->data;
	msg->size = (fast_v)? obj_data_sizev(&od->obj_desc) / sizeof(iovec_t) : obj_data_size(&od->obj_desc);
        msg->cb = obj_get_completion;
        msg->private = od;


	rpc_mem_info_cache(peer, msg, cmd); 
        // err = rpc_send_direct(rpc_s, peer, msg);
	err = (fast_v)? rpc_send_directv(rpc_s, peer, msg) : rpc_send_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
        if (err == 0)
                return 0;

        obj_data_free(od);
        free(msg);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/*
  Routine to execute "custom" application filters.
*/
static int dsgrpc_obj_filter(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_filter *hf = (struct hdr_obj_filter *) cmd->pad;
        struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
        struct msg_buf *msg;
        struct obj_data *from;
	double *dval;
        int err = -ENOENT;

        // BUG: when using version numbers here.
        from = ls_find(dsg->ssd->storage, &hf->odsc);
        if (!from) {
		char *str;
                str = obj_desc_sprint(&hf->odsc);
		uloga("'%s()': %s\n", __func__, str);
		free(str);

                goto err_out;
        }

        err = -ENOMEM;
        dval = malloc(sizeof(*dval));
        if (!dval)
                goto err_out;

        ssd_filter(from, &hf->odsc, dval);

        // TODO: process the filter ... and return the result
        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg)
                goto err_out;

        msg->msg_data = dval;
        msg->size = sizeof(*dval);
        msg->cb = default_completion_with_data_callback;

	rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_send_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
        if (err < 0)
                goto err_out;

        return 0;
 err_out:
        ERROR_TRACE();
}

/*
  Routine to return the space info, e.g., number of dimenstions.
*/
static int dsgrpc_ss_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
	struct hdr_ss_info *hsi;
	struct msg_buf *msg;
	int err = -ENOMEM;

	msg = msg_buf_alloc(rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_rpc->cmd = ss_info;
	msg->msg_rpc->id = DSG_ID;

	hsi = (struct hdr_ss_info *) msg->msg_rpc->pad;
	hsi->num_dims = ds_conf.ndims;
	hsi->val_dims[bb_x] = ds_conf.dimx;
	hsi->val_dims[bb_y] = ds_conf.dimy;
	hsi->val_dims[bb_z] = ds_conf.dimz;
	hsi->num_space_srv = dsg->ds->size_sp;

	err = rpc_send(rpc_s, peer, msg);
	if (err == 0)
		return 0;
 err_out:
	ERROR_TRACE();
}

#ifdef DS_HAVE_FASTBIT
static int vq_result_send_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	free(msg->private);
	free(msg);
	return 0;
}

static int fb_query_return_result(struct obj_descriptor *odsc, struct node_id *peer, int qid, void *result, int *size_ret)
{
	//msg->msg_rpc->cmd = ss_vq_reply;
	//rpc_send();
	struct hdr_vq_result *hvr;
	struct msg_buf *msg;
	int err = -ENOMEM;
	
	void *query_ret;

	msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
	if(!msg)
		goto err_out;
	
	int size = 0;
	if(size_ret != NULL){
		size = *size_ret;	//TODO size of result

		msg->private = query_ret = malloc(size + 7);
	//query_ret = realloc(result, size + 7);	
	if(!query_ret){
		printf("failed to malloc!!!!\n");
		goto err_free_msg;
	}
	ALIGN_ADDR_QUAD_BYTES(query_ret);
	memcpy(query_ret, result, size);
	//printf("query_ret=%d\n", *(int*)query_ret);
	//result = query_ret;
	//ALIGN_ADDR_QUAD_BYTES(result);
	free(result);

	msg->msg_rpc->cmd = ss_vq_reply;
	msg->msg_rpc->id = DSG_ID;
	//msg->msg_data = result; //TODO
	msg->msg_data = query_ret; //TODO
	msg->size = size;
	msg->cb = vq_result_send_completion;

	hvr = (struct hdr_vq_result *)msg->msg_rpc->pad;
	hvr->qid = qid;
	hvr->size = size;
	hvr->odsc = *odsc;
	hvr->flag = 0;
	}else{
		printf("num of reply = %d\n", *(int*)result);
		msg->private = query_ret = malloc(sizeof(int) + 7);
		size = 4;
			
	msg->msg_rpc->cmd = ss_vq_reply;
	msg->msg_rpc->id = DSG_ID;
	msg->cb = vq_result_send_completion;

	hvr = (struct hdr_vq_result *)msg->msg_rpc->pad;
	hvr->qid = qid;
	hvr->odsc = *odsc;
	hvr->flag = 1;
	hvr->size = *(int*)result;
	}
	err = rpc_send(dsg->ds->rpc_s, peer, msg);
	if(err == 0)
		return 0;
err_free_msg:
	free(msg->private);
	free(result);
	free(msg);
err_out:
	ERROR_TRACE();
}

static int local_value_query(struct hdr_value_query *hq, void* qcond, int* size_qret, struct node_id *peer)
{
	//parse query
	int strsize = 100; //TODO
	void *qret;
        char *select, *from, *where;
        select = malloc(sizeof(char)*strsize);
        from = malloc(sizeof(char*)*strsize);
        where = malloc(sizeof(char*)*strsize);
	if(!select || !from || !where)
		goto err_free_query;

        memset(select, '\0', strsize);
        memset(from, '\0', strsize);
        memset(where, '\0', strsize);
        parse_query_str(qcond, select, from, where);

        printf("------select=%s, from=%s, where=%s\n", select, from, where);
        
        struct obj_data *od;
        struct list_head *list;
	struct obj_descriptor *odsc = &hq->odsc;
	struct ss_storage *ls = dsg->ssd->storage;
	int hq_qid = hq->qid;
        int index = odsc->version % ls->size_hash;
        list = &ls->obj_hash[index];


	//create a result list to store all the result after query
	struct list_head qr_list;
	INIT_LIST_HEAD(&qr_list);
	int count_qr_list = 0;

	int ret_size = 0;
	struct timeval  tv1, tv2;
      	gettimeofday(&tv1, NULL);

        list_for_each_entry(od, list, struct obj_data, obj_entry){  //====Sith====//
        //list_for_each_entry(od, list, obj_entry){   //====Hopper====//
        //      printf("---find od name = %s\n", od->obj_desc.name);
       		if(strcmp(odsc->name, od->obj_desc.name) == 0 && odsc->version == od->obj_desc.version){
			qret= malloc(obj_data_size(&od->obj_desc));
			local_fb_query(od, select, from, where, size_qret, qret);	
			//printf("After fb_query, size_qret=%d\n", *size_qret);

			if(*size_qret > 0){
				struct query_result_list *qr = malloc(sizeof(*qr));
				memset(qr, 0, sizeof(*qr));
				qr->size = *size_qret;
				qr->qret = qret;
				list_add(&qr->qr_entry, &qr_list);
				count_qr_list++;	
			}
		}
				//INIT_LIST_HEAD(&qr->qr_entry);
	}
	
	gettimeofday(&tv2, NULL);
    	uloga("In memory query part Total time = %f seconds\n",
            (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
            (double) (tv2.tv_sec - tv1.tv_sec));
	//printf("Query result=%s\n", (char *)qret);

	//qret_num_reply(&hq->odsc, peer, hq->qid, count_qr_list);
	fb_query_return_result(&hq->odsc, peer, hq_qid, (void*)&count_qr_list, NULL);

	struct query_result_list *qr;
	list_for_each_entry(qr, &qr_list, struct query_result_list, qr_entry){ //====Sith====//
	//list_for_each_entry(qr, &qr_list, qr_entry){  //====Hopper====//
		//printf("qr->size=%d\n", qr->size);
		fb_query_return_result(&hq->odsc, peer, hq_qid, qr->qret, &qr->size);
		ret_size += qr->size;
	}
	//printf("client%d, ret_count=%d\n", peer->ptlmap.id, ret_count);
err_free_query:
	free(select);
	free(from);
	free(where);
	return 0;
}

static int query_receive_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	//struct hdr_value_query *hq = (struct hdr_value_query*)msg->msg_rpc->pad;
	struct hdr_value_query *hq = msg->private;
	//struct query_data *qdata = msg->private;
	//struct hdr_value_query *hq = &qdata->hq;
	void* qcond;
	struct node_id *peer;
	//int num_hits;
	void* qret;
	int size_qret = 0;
	int err;

	//qcond = msg->msg_data; //odsc = &hq->odsc;
	qcond = malloc(hq->size + 1);
	memcpy(qcond, msg->msg_data, hq->size);
	*((char*)qcond+hq->size) = '\0';
	peer = ds_get_peer(dsg->ds, msg->peer->ptlmap.id);
	printf("\nReceived query condition = %s\n", qcond);
	
	local_value_query(hq, qcond, &size_qret, peer);
	
	free(msg->msg_data);
	free(qcond);
	free(msg);	
	return 0;
}

static int dsgrpc_value_query(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//ds_get_peer();
	//msg->cb = query_receive_completion;
	//rpc_receive_direct();
	//struct query_data *qdata;

	//printf("get into ds_value_query+++++++++++++++++\n");

	//struct hdr_value_query *hq = (struct hdr_value_query*)cmd->pad;
	struct hdr_value_query *hq = malloc(sizeof(*hq));
	memcpy(hq, (struct hdr_value_query*)cmd->pad, sizeof(*hq));
	struct node_id *peer;
	struct msg_buf *msg;
	//void *qcond;
	int err = -ENOENT;

	//printf("\nreceived hq_odsc_name=%s\n", hq->odsc.name);
/*	struct list_head *list;
	struct ss_storage *ls;
	ls = dsg->ssd->storage;
	list = &ls->obj_hash[0];
	printf("-----------dsgrpc_value_query obj_name=%s\n",((struct obj_data*)list->next)->obj_desc.name);
*/
	peer = ds_get_peer(dsg->ds, cmd->id);

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if(!msg)
		goto err_free_data;

	//qdata = malloc(sizeof(struct query_data));
	//memset(qdata, 0, sizeof(struct query_data));
	//qdata->data = malloc(4096);
	//qdata->_data = qdata->data = malloc(hq->size + 7);
	//ALIGN_ADDR_QUAD_BYTES(qdata->data);
	//qdata->hq = *hq;

	msg->msg_data = malloc(1028);
	//printf("address = 0x%x\n", msg->msg_data);
	//ALIGN_ADDR_QUAD_BYTES(msg->msg_data);

	//msg->msg_data = qdata->data;
	//msg->size = hq->size;
	msg->size = 1024;
	msg->cb = query_receive_completion;
	//msg->private = &hq->odsc;
	
	msg->private = hq;
	//msg->private = qdata;

	//peer->mdh_addr = cmd->mdh_addr; // Modified by Tong Jin for GEMINI
	rpc_mem_info_cache(peer, msg, cmd); 
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	/*peer->mb = cmd->mbits;
	err = rpc_receive_direct(rpc_s, peer, msg);
	peer->mb = MB_RPC_MSG;*/
	if(err < 0)
		goto err_free_msg;
	return 0;
err_free_msg:
	free(msg);
err_free_data:
	//free(qcond);
	return err;
}

#endif

/*
  Public API starts here.
*/

struct ds_gspace *dsg_alloc(int num_sp, int num_cp, char *conf_name)
{
        struct ds_gspace *dsg_l;
        int err = -ENOMEM;

	/* Alloc routine should be called only once. */
        if (dsg)
                return dsg;

        /* Default values */
        ds_conf.max_versions = 1;
        ds_conf.max_readers = 1;
	ds_conf.lock_type = 1;

        err = parse_conf(conf_name);
        if (err < 0) {
                ds_conf.ndims = 3;
                ds_conf.dimx = 4096;
                ds_conf.dimy = 4096;
                ds_conf.dimz = 4096;

		uloga("'%s()' error loading config file, starting "
			"with defaults.\n", __func__);
        }
        else
                uloga("'%s()' config file loaded.\n", __func__);

#ifdef DS_HAVE_FASTBIT
        char *meta_name = "metadata.txt";
        err = parse_meta(meta_name, &mtdata);
        if(err){
                printf("parse_meta failed with %d\n", err);
                goto err_out;
        }
#endif

	if (ds_conf.ndims == 2)
		ds_conf.dimz = 1;

        struct bbox domain = {.num_dims = ds_conf.ndims, 
                .lb.c = {0, 0, 0}, 
                .ub.c = {ds_conf.dimx-1, ds_conf.dimy-1, ds_conf.dimz-1}};
        double tm_start, tm_end;

        timer_init(&timer, 1);
        timer_start(&timer);

	/*
	dsg_lock_init((ds_conf.lock_type == 1) ? lock_generic : lock_custom, 
			ds_conf.max_readers);
	*/
        dsg = dsg_l = malloc(sizeof(*dsg_l));
        if (!dsg_l)
                goto err_out;

        rpc_add_service(ss_obj_get_dht_peers, dsgrpc_obj_send_dht_peers);
        rpc_add_service(ss_obj_get_desc, dsgrpc_obj_get_desc);
        rpc_add_service(ss_obj_get, dsgrpc_obj_get);
        rpc_add_service(ss_obj_put, dsgrpc_obj_put);
        rpc_add_service(ss_obj_update, dsgrpc_obj_update);
        rpc_add_service(ss_obj_filter, dsgrpc_obj_filter);
        rpc_add_service(ss_obj_cq_register, dsgrpc_obj_cq_register);
        rpc_add_service(cp_lock, dsgrpc_lock_service);
	rpc_add_service(ss_info, dsgrpc_ss_info);
#ifdef DS_HAVE_ACTIVESPACE
	rpc_add_service(ss_code_put, dsgrpc_bin_code_put);
#endif

#ifdef DS_HAVE_FASTBIT
        rpc_add_service(ss_value_query, dsgrpc_value_query);
#endif

        INIT_LIST_HEAD(&dsg_l->cq_list);
        INIT_LIST_HEAD(&dsg_l->obj_desc_req_list);
        INIT_LIST_HEAD(&dsg_l->obj_data_req_list);
	INIT_LIST_HEAD(&dsg_l->locks_list);

        dsg_l->ds = ds_alloc(num_sp, num_cp, dsg_l);
        if (!dsg_l->ds)
                goto err_free;

        tm_start = timer_read(&timer);
        dsg_l->ssd = ssd_alloc(&domain, num_sp, ds_conf.max_versions);
        if (!dsg_l->ssd)
                /* Yes, I don't free resources here, but we should
                   fail anyway. */
                goto err_out;
        tm_end = timer_read(&timer);

#ifdef DEBUG
        printf("SRV %d %d %lf\n", DSG_ID, -1, tm_end-tm_start);
#endif

        err = ssd_init(dsg_l->ssd, ds_get_rank(dsg_l->ds));
        if (err < 0)
                goto err_out;

        return dsg_l;
 err_free:
        free(dsg_l);
 err_out:
        uloga("'%s()': failed with %d.", __func__, err);
        dsg = 0;
        return NULL;
}

/* Helper routine for external calls, e.g., rexec.  */
struct dht_entry *dsg_dht_get_self_entry(void)
{
	return dsg->ssd->ent_self;
}

/* Helper routine for external calls, e.g., rexec. */
void dsg_dht_print_descriptors(const struct obj_descriptor *odsc_tab[], int n)
{
	char *str = 0;
	int i;

	for (i = 0; i < n; i++) {
		str = str_append(str, obj_desc_sprint(odsc_tab[i]));
	}

	uloga("'%s()': %d descriptors - %s.\n", __func__, n, str);
	free(str);
}

/* Helper routine for external calls, e.g., rexec. */
struct ss_storage *dsg_dht_get_storage(void)
{
	return dsg->ssd->storage;
}

void dsg_free(struct ds_gspace *dsg)
{
        ds_free(dsg->ds);
	ssd_free(dsg->ssd);
        free(dsg);
}


int dsg_process(struct ds_gspace *dsg)
{
	//        return rpc_process_event(dsg->ds->rpc_s);
	int err;
	err = rpc_process_event(dsg->ds->rpc_s);
	if (err < 0)
		rpc_report_md_usage(dsg->ds->rpc_s);

	return err;
}

int dsg_complete(struct ds_gspace *dsg)
{
        return ds_stop(dsg->ds);
}

int dsg_barrier(struct ds_gspace *dsg)
{
        return ds_barrier(dsg->ds);
}

/*
  Helper function  to enable a local  process to add an  object to the
  space.  The  reference to "od"  should be on  the heap, and  will be
  managed by the server.
*/
int dsghlp_obj_put(struct ds_gspace *dsg, struct obj_data *od)
{
        struct msg_buf *msg;
        int err = -ENOMEM;
        double tm_start, tm_end;

        tm_start = timer_read(&timer);
        od->obj_desc.owner = DSG_ID; // dsg->ds->self->id;
        msg = msg_buf_alloc(dsg->ds->rpc_s, dsg->ds->self, 0);
        if (!msg)
                goto err_out;

        msg->private = od;
        err = obj_put_completion(dsg->ds->rpc_s, msg);
		tm_end = timer_read(&timer);
#ifdef DEBUG
        printf("SRV %d %d %lf.\n", DSG_ID, -2, (tm_end-tm_start));
#endif
        if (err == 0)
                return 0;

 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

int dsghlp_get_rank(struct ds_gspace *dsg)
{
        return DSG_ID;
}

/*
  Helper function  to find out when  all the service  peers have joind
  in.
*/
int dsghlp_all_sp_joined(struct ds_gspace *dsg)
{
        return (dsg->ds->num_sp == dsg->ds->size_sp);
}
