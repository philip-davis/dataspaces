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
#include "util.h"

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

/* Server configuration parameters */
static struct {
        int ndim;
        struct coord dims;
        int max_versions;
        int max_readers;
        int lock_type;		/* 1 - generic, 2 - custom */
        int hash_version;   /* 1 - ssd_hash_version_v1, 2 - ssd_hash_version_v2 */
} ds_conf;

static struct {
        const char      *opt;
        int             *pval;
} options[] = {
        {"ndim",                &ds_conf.ndim},
        {"dims",                &ds_conf.dims},
        {"max_versions",        &ds_conf.max_versions},
        {"max_readers",         &ds_conf.max_readers},
        {"lock_type",           &ds_conf.lock_type},
        {"hash_version",        &ds_conf.hash_version}, 
};

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

        n = sizeof(options) / sizeof(options[0]);

        for (i = 0; i < n; i++) {
                if (strcmp(line, options[1].opt) == 0){ /**< when "dims" */
                    //get coordinates
                    int idx = 0;
                    char* crd;
                    crd = strtok(t, ",");
                    while(crd != NULL){
                        ((struct coord*)options[1].pval)->c[idx] = atoll(crd);
                        crd = strtok(NULL, ",");
                        idx++;
                    }
                    if(idx != *(int*)options[0].pval){
                        uloga("index=%d, ndims=%d\n",idx, *(int*)options[0].pval);
                        uloga("The number of coordination should the same with number of dimension!\n");
                        return -EINVAL;
                    }
                    break;
                }
                if (strcmp(line, options[i].opt) == 0) {
                        eat_spaces(line);
                        *(int*)options[i].pval = atoi(t);
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

static int init_sspace(struct bbox *default_domain, struct ds_gspace *dsg_l)
{
    int err = -ENOMEM;
    dsg_l->ssd = ssd_alloc(default_domain, dsg_l->ds->size_sp,
                            ds_conf.max_versions, ds_conf.hash_version);
    if (!dsg_l->ssd)
        goto err_out;

    err = ssd_init(dsg_l->ssd, ds_get_rank(dsg_l->ds));
    if (err < 0)
        goto err_out;

    dsg_l->default_gdim.ndim = ds_conf.ndim;
    int i;
    for (i = 0; i < ds_conf.ndim; i++) {
        dsg_l->default_gdim.sizes.c[i] = ds_conf.dims.c[i];
    }

    INIT_LIST_HEAD(&dsg_l->sspace_list);
    return 0;
 err_out:
    uloga("%s(): ERROR failed\n", __func__);
    return err;
}

static int free_sspace(struct ds_gspace *dsg_l)
{
    ssd_free(dsg_l->ssd);
    struct sspace_list_entry *ssd_entry, *temp;
    list_for_each_entry_safe(ssd_entry, temp, &dsg_l->sspace_list,
            struct sspace_list_entry, entry)
    {
        ssd_free(ssd_entry->ssd);
        list_del(&ssd_entry->entry);
        free(ssd_entry);
    }

    return 0;
}

static struct sspace* lookup_sspace(struct ds_gspace *dsg_l, const char* var_name, const struct global_dimension* gd)
{
    struct global_dimension gdim;
    memcpy(&gdim, gd, sizeof(struct global_dimension));

    // Return the default shared space created based on
    // global data domain specified in dataspaces.conf 
    if (global_dimension_equal(&gdim, &dsg_l->default_gdim )) {
        return dsg_l->ssd;
    }

    // Otherwise, search for shared space based on the
    // global data domain specified by application in put()/get().
    struct sspace_list_entry *ssd_entry = NULL;
    list_for_each_entry(ssd_entry, &dsg_l->sspace_list,
        struct sspace_list_entry, entry)
    {
        // compare global dimension
        if (gdim.ndim != ssd_entry->gdim.ndim)
            continue;

        if (global_dimension_equal(&gdim, &ssd_entry->gdim))
            return ssd_entry->ssd;
    }

    // If not found, add new shared space
    int i, err;
    struct bbox domain;
    memset(&domain, 0, sizeof(struct bbox));
    domain.num_dims = gdim.ndim;
    for (i = 0; i < gdim.ndim; i++) {
        domain.lb.c[i] = 0;
        domain.ub.c[i] = gdim.sizes.c[i] - 1;
    } 

    ssd_entry = malloc(sizeof(struct sspace_list_entry));
    memcpy(&ssd_entry->gdim, &gdim, sizeof(struct global_dimension));
    ssd_entry->ssd = ssd_alloc(&domain, dsg_l->ds->size_sp, 
                            ds_conf.max_versions, ds_conf.hash_version);     
    if (!ssd_entry->ssd) {
        uloga("%s(): ssd_alloc failed\n", __func__);
        return dsg_l->ssd;
    }

    err = ssd_init(ssd_entry->ssd, ds_get_rank(dsg_l->ds));
    if (err < 0) {
        uloga("%s(): ssd_init failed\n", __func__); 
        return dsg_l->ssd;
    }

#ifdef DEBUG
/*
    uloga("%s(): add new shared space ndim= %d global dimension= %llu %llu %llu\n",
        __func__, gdim.ndim, gdim.sizes.c[0], gdim.sizes.c[1], gdim.sizes.c[2]);
*/
#endif

    list_add(&ssd_entry->entry, &dsg_l->sspace_list);
    return ssd_entry->ssd;
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

    from_obj = ls_find(dsg->ls, &hc->odsc);
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

/*routine to remove data object*/

static int dsgrpc_remove_service(struct rpc_server *rpc, struct rpc_cmd *cmd)
{
        struct lockhdr *lh = (struct lockhdr *) cmd->pad;
        struct dsg_lock *dl;
        int err = -ENOMEM;

//        uloga("'%s()': Remove %s version %d.\n", __func__, lh->name, lh->lock_num  );


	if (!dsg->ls) return 0;

	struct obj_data *od, *t;
	struct list_head *list;
	int i;

	for (i = 0; i < dsg->ls->size_hash; i++) {
        	list = &(dsg->ls)->obj_hash[i];
	        list_for_each_entry_safe(od, t, list, struct obj_data, obj_entry ) {
			if (od->obj_desc.version == lh->lock_num && !strcmp(od->obj_desc.name,lh->name) ) {
				ls_remove(dsg->ls, od);
				obj_data_free(od);
			}
		}
	}

        
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
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct sspace* ssd = lookup_sspace(dsg, oh->u.o.odsc.name, &oh->gdim); 
        struct dht_entry *de = ssd->ent_self;
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
static int obj_put_update_dht(struct ds_gspace *dsg, struct obj_data *od)
{
    struct obj_descriptor *odsc = &od->obj_desc;
    struct sspace* ssd = lookup_sspace(dsg, odsc->name, &od->gdim);
	struct dht_entry *dht_tab[ssd->dht->num_entries];
	/* TODO: create a separate header structure for object
	   updates; for now just abuse the hdr_obj_get. */
	struct hdr_obj_get *oh;
	struct msg_buf *msg;
	struct node_id *peer;
	int num_de, i, min_rank, err;

	/* Compute object distribution to nodes in the space. */
	num_de = ssd_hash(ssd, &odsc->bb, dht_tab);
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
			dht_add_entry(ssd->ent_self, odsc);
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
        memcpy(&oh->gdim, &od->gdim, sizeof(struct global_dimension));

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
    ls_add_obj(dsg->ls, od);

    free(msg);
#ifdef DEBUG
    uloga("'%s()': server %d finished receiving  %s, version %d.\n",
        __func__, DSG_ID, od->obj_desc.name, od->obj_desc.version);
#endif

    return 0;
}

/*
*/
static int dsgrpc_obj_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_put *hdr = (struct hdr_obj_put *)cmd->pad;
        struct obj_descriptor *odsc = &(hdr->odsc);
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

        od->obj_desc.owner = DSG_ID;
        memcpy(&od->gdim, &hdr->gdim, sizeof(struct global_dimension));

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg)
                goto err_free_data;

        msg->msg_data = od->data;
        msg->size = obj_data_size(&od->obj_desc);
        msg->private = od;
        msg->cb = obj_put_completion;

#ifdef DEBUG
        uloga("'%s()': server %d start receiving %s, version %d.\n", 
            __func__, DSG_ID, odsc->name, odsc->version);
#endif
        rpc_mem_info_cache(peer, msg, cmd); 
        err = rpc_receive_direct(rpc_s, peer, msg);
        rpc_mem_info_reset(peer, msg, cmd);

        if (err < 0)
                goto err_free_msg;

	/* NOTE: This  early update, has  to be protected  by external
	   locks in the client code. */

        err = obj_put_update_dht(dsg, od);
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
        struct sspace* ssd = lookup_sspace(dsg, oh->u.o.odsc.name, &oh->gdim);
        struct dht_entry *de_tab[ssd->dht->num_entries];
        struct msg_buf *msg;
        int *peer_id_tab, peer_num, i;
        int err = -ENOMEM;

        peer = ds_get_peer(dsg->ds, cmd->id);

        peer_num = ssd_hash(ssd, &oh->u.o.odsc.bb, de_tab);
        peer_id_tab = malloc(sizeof(int) * (dsg->ds->size_sp+1));
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
        msg->size = sizeof(int) * (dsg->ds->size_sp+1);
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
/*
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

        peer_num = ssd_hash(dsg->ssd, &oh->u.o.odsc.bb, de_tab);
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
*/

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
        struct sspace* ssd = lookup_sspace(dsg, oh->u.o.odsc.name, &oh->gdim);
        const struct obj_descriptor *podsc[ssd->ent_self->odsc_num];
        int obj_versions[ssd->ent_self->odsc_size];
        int num_odsc, i;
        struct msg_buf *msg;
        int err = -ENOENT;

        num_odsc = dht_find_entry_all(ssd->ent_self, &oh->u.o.odsc, podsc);
        if (!num_odsc) {
#ifdef DEBUG
		char *str = 0;

        asprintf(&str, "S%2d: obj_desc not found for ", DSG_ID); 
		str = str_append(str, obj_desc_sprint(&oh->u.o.odsc));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
#endif
		i = dht_find_versions(ssd->ent_self, &oh->u.o.odsc, obj_versions);
                err = obj_desc_not_found(peer, oh->qid, i, obj_versions);
                if (err < 0)
                        goto err_out;

                return 0;
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
        from_obj = ls_find(dsg->ls, &oh->u.o.odsc);
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
        from = ls_find(dsg->ls, &hf->odsc);
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
	hsi->num_dims = ds_conf.ndim;
    int i; 
    for(i = 0; i < hsi->num_dims; i++){
        hsi->dims.c[i] = ds_conf.dims.c[i]; 
    }
	hsi->num_space_srv = dsg->ds->size_sp;
    hsi->hash_version = ds_conf.hash_version;
    hsi->max_versions = ds_conf.max_versions;

	err = rpc_send(rpc_s, peer, msg);
	if (err == 0)
		return 0;
 err_out:
	ERROR_TRACE();
}

/*
  Public API starts here.
*/

struct ds_gspace *dsg_alloc(int num_sp, int num_cp, char *conf_name, void *comm)
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
        ds_conf.hash_version = ssd_hash_version_v1;

        err = parse_conf(conf_name);
        if (err < 0) {
            uloga("%s(): ERROR failed to load config file '%s'.", __func__, conf_name);
            goto err_out;
        }

        // Check number of dimension
        if (ds_conf.ndim > BBOX_MAX_NDIM) {
            uloga("%s(): ERROR maximum number of array dimension is %d but ndim is %d"
                " in file '%s'\n", __func__, BBOX_MAX_NDIM, ds_conf.ndim, conf_name);
            err = -ENOMEM;
            goto err_out;
        }

        // Check hash version
        if ((ds_conf.hash_version < ssd_hash_version_v1) ||
            (ds_conf.hash_version >= _ssd_hash_version_count)) {
            uloga("%s(): ERROR unknown hash version %d in file '%s'\n",
                __func__, ds_conf.hash_version, conf_name);
            err = -ENOMEM;
            goto err_out;
        }

        struct bbox domain;
        memset(&domain, 0, sizeof(struct bbox));
        domain.num_dims = ds_conf.ndim;
        int i;
        for(i = 0; i < domain.num_dims; i++){
            domain.lb.c[i] = 0;
            domain.ub.c[i] = ds_conf.dims.c[i] - 1;
        }

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
        rpc_add_service(cp_remove, dsgrpc_remove_service);
        rpc_add_service(ss_info, dsgrpc_ss_info);
#ifdef DS_HAVE_ACTIVESPACE
        rpc_add_service(ss_code_put, dsgrpc_bin_code_put);
#endif
        INIT_LIST_HEAD(&dsg_l->cq_list);
        INIT_LIST_HEAD(&dsg_l->obj_desc_req_list);
        INIT_LIST_HEAD(&dsg_l->obj_data_req_list);
        INIT_LIST_HEAD(&dsg_l->locks_list);

        dsg_l->ds = ds_alloc(num_sp, num_cp, dsg_l, comm);
        if (!dsg_l->ds)
                goto err_free;

        err = init_sspace(&domain, dsg_l);
        if (err < 0) {
            goto err_free;
        }

        dsg_l->ls = ls_alloc(ds_conf.max_versions);
        if (!dsg_l->ls) {
            uloga("%s(): ERROR ls_alloc() failed\n", __func__);
            goto err_free;
        }

        return dsg_l;
 err_free:
        free(dsg_l);
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
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

void dsg_free(struct ds_gspace *dsg)
{
        ds_free(dsg->ds);
        free_sspace(dsg);
        ls_free(dsg->ls);
        free(dsg);
}


int dsg_process(struct ds_gspace *dsg)
{
	int err;
    
    err = ds_process(dsg->ds);
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

        od->obj_desc.owner = DSG_ID; // dsg->ds->self->id;
        msg = msg_buf_alloc(dsg->ds->rpc_s, dsg->ds->self, 0);
        if (!msg)
                goto err_out;

        msg->private = od;
        err = obj_put_completion(dsg->ds->rpc_s, msg);
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
