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
#include <unistd.h>

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
        lock_custom,
        lock_v3,
        _lock_type_count
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
        {"dims",                (int *)&ds_conf.dims},
        {"max_versions",        &ds_conf.max_versions},
        {"max_readers",         &ds_conf.max_readers},
        {"lock_type",           &ds_conf.lock_type},
        {"hash_version",        &ds_conf.hash_version}, 
};

static int dsg_block_put(struct block_data * bd_data[], struct block_data * bd_code[]);//duan
static int dsg_block_get(struct block_data * bd_data[], struct block_data * bd_code[], struct block_data * bd);//duan
static int dsg_block_get_with_delete(struct block_data * bd_data[], struct block_data * bd_code[], struct block_data * bd);//duan
static int dsg_block_get_without_delete(struct block_data * bd_data[], struct block_data * bd_code[], struct block_data * bd);//duan
static int dsg_obj_sync(struct obj_data *od, int num_odsc, enum encode_token token, int peer_id);//duan
static int dsg_block_put(struct block_data * bd_data[], struct block_data * bd_code[]);//duan
static int dsg_block_sync(struct ds_gspace *dsg, struct obj_data *od);//duan
static int dsg_obj_replicate(struct obj_data *od);//duan


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
                        uloga("The number of coordination should the same as the number of dimensions!\n");
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

static inline void dsg_inc_pending(void)//duan
{
	dsg->num_pending++;
}

static inline void dsg_dec_pending(void)//duan
{
	dsg->num_pending--;
}

static int init_sspace(struct bbox *default_domain, struct ds_gspace *dsg_l)
{
    int err = -ENOMEM;

    ulog("rank %d creating default gdomain\n", DSG_ID);

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
        uloga("%s(): ssd_alloc failed for '%s'\n", __func__, var_name);
        return dsg_l->ssd;
    }

    err = ssd_init(ssd_entry->ssd, ds_get_rank(dsg_l->ds));
    if (err < 0) {
        uloga("%s(): ssd_init failed\n", __func__); 
        return dsg_l->ssd;
    }

#ifdef DEBUG
/*
    ulog("%s(): add new shared space ndim= %d global dimension= %llu %llu %llu\n",
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

/*
  Lock v3 service.
*/
void lock_init_v3(struct dsg_lock *dl, int max_readers)
{

        dl->rd_lock_state = 
        dl->wr_lock_state = unlocked;

        dl->rd_cnt =
        dl->wr_cnt = 0;

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

static enum lock_action 
lock_process_request_v3(struct dsg_lock *dl, struct lockhdr *lh, int may_grant)
{
        enum lock_action f_lock = la_none;

        switch (lh->type) {
        case lk_read_get:
                if (dl->wr_lock_state == unlocked && dl->wr_epoch > 0 && may_grant) {
                        dl->rd_lock_state = locked;
                        dl->rd_cnt++;
                        f_lock = la_grant;
                }
        else    f_lock = la_wait;
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
        else    f_lock = la_wait;
                break;

        case lk_write_release:
                dl->wr_cnt--;
                if (dl->wr_cnt == 0) {
                        dl->wr_lock_state = unlocked;
                        dl->wr_epoch++;
                        f_lock = la_notify;
                }
        }

        return f_lock;
}

static int lock_process_wait_list_v3(struct dsg_lock *dl)
{
        struct req_pending *rr; // , *tmp;
        struct lockhdr *lh;
        struct node_id *peer;
        int err;

        while (! list_empty(&dl->wait_list)) {
                rr = list_entry(dl->wait_list.next, 
                                struct req_pending, req_entry);

                lh = (struct lockhdr *) rr->cmd.pad;
                switch (lock_process_request_v3(dl, lh, 1)) {
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

static int 
lock_service_v3(struct dsg_lock *dl, struct rpc_server *rpc, struct rpc_cmd *cmd)
{
        struct lockhdr *lh = (struct lockhdr *) cmd->pad;
        struct node_id *peer;
        int err = 0;

        switch (lock_process_request_v3(dl, lh, list_empty(&dl->wait_list))) {

        case la_none:
        break;

        case la_wait:
                err = dsg_lock_put_on_wait(dl, cmd);
                break;

        case la_notify:
                err = lock_process_wait_list_v3(dl);
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
                ulog("generic lock %s created.\n", lock_name);
                break;

        case lock_custom:
                dl->init = &sem_init;
                dl->process_request = &sem_process_request;
                dl->process_wait_list = &sem_process_wait_list;
                dl->service = &sem_service;
                ulog("custom lock %s created.\n", lock_name);
                break;
        case lock_v3:
                dl->init = &lock_init_v3;
                dl->process_request = &lock_process_request_v3;
                dl->process_wait_list = &lock_process_wait_list_v3;
                dl->service = &lock_service_v3;
                ulog("type 3 lock %s created.\n", lock_name);
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

	dl = dsg_lock_find_by_name(lh->name);

	if (!dl) {
		dl = dsg_lock_alloc(lh->name, 
			ds_conf.lock_type, 
			ds_conf.max_readers);

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

static char *obj_desc_sprint(const struct obj_descriptor *odsc)
{
	char *str;
	int nb;

    str = alloc_sprintf("obj_descriptor = {\n"
                "\t.name = %s,\n"
                "\t.owner = %d,\n"
                "\t.version = %d\n"
                "\t.bb = ", odsc->name, odsc->owner, odsc->version);
	str = str_append_const(str_append(str, bbox_sprint(&odsc->bb)), "}\n");

	return str;
}


//>>>>>>>duan

/*
Rpc routine to update (add or insert) an object descriptor in the
dht table.
*/
static int dsgrpc_peer_update(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	int err;
	struct hdr_peer_update *oh = (struct hdr_peer_update *) cmd->pad;
#ifdef DEBUG
	{
		char *str;
		str = alloc_sprintf( "S%2d: peer_id = %d status = %d from S%2d",
			DSG_ID, oh->peer_id, oh->status, cmd->id);
		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif
	/**/
	uloga("'%s()' in S%2d: node failure test!\n", __func__, DSG_ID);
	int i;
	for (i = oh->peer_id; i < 64; i = i + 4){//16 process fail for Titan & Caliburn compute node
		set_rpc_server_status(dsg->ds->rpc_s, i, oh->status);//duan node failure
	}
	
	//set_rpc_server_status(dsg->ds->rpc_s, oh->peer_id, oh->status);//duan process failure
	if (oh->peer_id == DSG_ID && 1==2){//duan
		err = -ENOMEM;
		dsg->kill = 1;
		uloga("'%s()': Server %d received kill command. Going to shut down...\n", __func__, DSG_ID);
		sleep(3);
	}
	return 0;
err_out:
	ERROR_TRACE();
}


/*
Update the peer status, only be called by master server. duan
*/
static int peer_status_update(struct ds_gspace *dsg, int peer_id, int status)//duan
{

	/* TODO: create a separate header structure for object
	updates; for now just abuse the hdr_obj_get. */
	struct hdr_peer_update *oh;
	struct msg_buf *msg;
	struct node_id *peer;
	int num_sp, num_cp, j, err;

	/* Update object descriptors on the corresponding nodes. */
	num_sp = ds_get_num_sp(dsg->ds);//duan
	num_cp = ds_get_num_cp(dsg->ds);//duan
	uloga("'%s()': for server peers num_sp = %d, num_cp = %d, peer_id = %d, status = %d \n", __func__, num_sp, num_cp, peer_id, status);
	
	for (j = 0; j < num_sp + num_cp; j++){//duan
		peer = ds_get_peer(dsg->ds, j);//duan

		#ifdef DEBUG
		{
			char *str;
			str = alloc_sprintf( "S%2d: peer_id = %d status = %d update to S%2d", DSG_ID, peer_id, status, j);
			uloga("'%s()': %s\n", __func__, str);
			free(str);
		}
		#endif

		if (j == DSG_ID){
		//if (peer == dsg->ds->self) {
			/**/
			uloga("'%s()' in S%2d: node failure test!\n", __func__, DSG_ID);
			int i;
			for (i = oh->peer_id; i < 64; i = i + 4){//16 process fail for Titan & Caliburn compute node
				set_rpc_server_status(dsg->ds->rpc_s, i, status);//duan node failure
			}
			
			//set_rpc_server_status(dsg->ds->rpc_s, peer_id, status);//duan process failure

			#ifdef DEBUG
			{
				char *str;
				str = alloc_sprintf( "S%2d: peer_id = %d status = %d update to local", DSG_ID, peer_id, status);
				uloga("'%s()': %s\n", __func__, str);
				free(str);
			}
			#endif

			continue;
		}

		err = -ENOMEM;
		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg){
			uloga("'%s()': err 1 \n", __func__);
			goto err_out;
		}
		msg->msg_rpc->cmd = ss_peer_update;
		msg->msg_rpc->id = DSG_ID;

		oh = (struct hdr_peer_update *) msg->msg_rpc->pad;
		oh->peer_id = peer_id;
		oh->status = status;
		err = rpc_send(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			uloga("'%s()': err 2 \n", __func__);
			goto err_out;
		}
	}

	return 0;
err_out:
	ERROR_TRACE();
}

/*
Rpc routine to update (add or insert) an object descriptor in the
dht table.
*/
static int dsgrpc_peer_test(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	int err;
	struct hdr_peer_update *oh = (struct hdr_peer_update *) cmd->pad;
	#ifdef DEBUG
	{
		char *str;
		str = alloc_sprintf( "S%2d: update peer peer_id %d status %d from S%2d",
			DSG_ID, oh->peer_id, oh->status, cmd->id);
		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
	#endif


	peer_status_update(dsg, oh->peer_id, oh->status);
	return 0;
err_out:
	ERROR_TRACE();
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
    char *str;

	str = alloc_sprintf("S%2d: update obj_desc '%s' ver %d from S%2d for  ", DSG_ID,
                    oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
    str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

	uloga("'%s()': %s\n", __func__, str);
	free(str);
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

static inline int dsg_which_peer(int peer_id, int peer_id_arr[])//duan
{
	int i, j = 0, next_peer_id, peer_ftid, ft_group_id, size_group;
	struct node_id *peer;
	peer = ds_get_peer(dsg->ds, peer_id);
	peer_ftid = peer->ftmap.ftid;
	size_group = dsg->ds->size_ft_group;
	ft_group_id = peer_ftid / size_group;
	for (i = 0; i < size_group; i++){
		next_peer_id = ft_group_id*size_group + i;
		peer = ds_get_peer(dsg->ds, next_peer_id);
		if (peer->ftmap.ns == node_normal){
			peer_id_arr[j] = next_peer_id;
			j++;
		}
	}
	return j;
}

static inline int dsg_which_all_peer(int peer_id, int peer_id_arr[])//duan recovery
{
	int i, next_peer_id, peer_ftid, ft_group_id, size_group;
	struct node_id *peer;
	peer = ds_get_peer(dsg->ds, peer_id);
	peer_ftid = peer->ftmap.ftid;
	size_group = dsg->ds->size_ft_group;
	ft_group_id = peer_ftid / size_group;
	for (i = 0; i < size_group; i++){
		next_peer_id = ft_group_id*size_group + i;
		peer_id_arr[i] = next_peer_id;
	}
	return i;
}

static inline int dsg_next_peer(int peer_id)//duan
{
	int i, next_peer_id, peer_ftid, ft_group_id, size_group, peer_index;
	struct node_id *peer;
	peer = ds_get_peer(dsg->ds, peer_id);
	peer_ftid = peer->ftmap.ftid;
	size_group = dsg->ds->size_ft_group;
	ft_group_id = peer_ftid / size_group;
	peer_index = peer_ftid % size_group;
	for (i = 0; i < size_group; i++){
		peer_index = (peer_index + 1 + i) % size_group; //duan
		next_peer_id = ft_group_id*size_group + peer_index;
		peer = ds_get_peer(dsg->ds, next_peer_id);
		if (peer->ftmap.ns == node_normal){
			return next_peer_id;
		}
	}
	return next_peer_id;
}

static inline int dsg_which_code_peer(int peer_id, int peer_id_arr[])//duan
{
	int i, j = 0, next_peer_id, peer_ftid, ft_group_id, size_group;
	struct node_id *peer;
	peer = ds_get_peer(dsg->ds, peer_id);
	peer_ftid = peer->ftmap.ftid;
	size_group = dsg->ds->size_ft_code_group;
	ft_group_id = peer_ftid / size_group;
	for (i = 0; i < size_group; i++){
		next_peer_id = ft_group_id*size_group + i;
		peer = ds_get_peer(dsg->ds, next_peer_id);
		//if (peer->ftmap.ns == node_normal){
			peer_id_arr[j] = next_peer_id;
			j++;
		//}
	}
	return j;
}

static inline int dsg_number_code_peer(int peer_id)//duan
{
	int i, j = 0, next_peer_id, peer_ftid, ft_group_id, size_group;
	struct node_id *peer;
	peer = ds_get_peer(dsg->ds, peer_id);
	peer_ftid = peer->ftmap.ftid;
	size_group = dsg->ds->size_ft_code_group;
	ft_group_id = peer_ftid / size_group;
	for (i = 0; i < size_group; i++){
		next_peer_id = ft_group_id*size_group + i;
		peer = ds_get_peer(dsg->ds, next_peer_id);
		if (peer->ftmap.ns == node_normal){
			j++;
		}
	}
	return j;
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
	ulog("server %d determining object hash.", DSG_ID);
	num_de = ssd_hash(ssd, &odsc->bb, dht_tab);
	if (num_de == 0) {
		uloga("'%s()': this should not happen, num_de == 0 ?!\n",
			__func__);
	}

	min_rank = dht_tab[0]->rank;
	/* Update object descriptors on the corresponding nodes. */
	for (i = 0; i < num_de; i++) {

		int peer_id[DSG_FT_LEVEL] = {0};
		//int num_peer = dsg_which_peer(dht_tab[i]->rank, peer_id);//duan
		int num_peer = dsg_which_all_peer(dht_tab[i]->rank, peer_id);//duan
		int j;//duan
		for (j = 0; j < num_peer; j++){//duan
			peer = ds_get_peer(dsg->ds, peer_id[j]);//duan

			if (peer == dsg->ds->self) {
				// TODO: check if owner is set properly here.
				/*
				uloga("Obj desc version %d, for myself ... owner is %d\n",
				od->obj_desc.version, od->obj_desc.owner);
			*/
#ifdef DEBUG
			char *str;
            
            str = alloc_sprintf("S%2d: got obj_desc '%s' ver %d for ", DSG_ID, odsc->name, odsc->version);
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

#ifdef DEBUG
		} else {
			char *str;

            str = alloc_sprintf("S%2d: fwd obj_desc '%s' to S%2d ver %d for ",
                 DSG_ID, odsc->name, peer->ptlmap.id, odsc->version);
			str = str_append(str, bbox_sprint(&odsc->bb));

			uloga("'%s()': %s\n", __func__, str);
			free(str);
		}
#else
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
	}

	return 0;
 err_out:
	ERROR_TRACE();
}

/*
    obj_put synchronization completion
    Remove msg_ds buffer after obj_put_completion() send back rpc call
*/
#ifdef DS_SYNC_MSG
static int obj_put_sync_completion(struct rpc_server *rpc_s, struct msg_buf *msg){
    free(msg);
    return 0;
}
#endif






























/*
Update the DHT metadata with the new obj_descriptor information.duan
*/
static int block_put_update_dht(struct ds_gspace *dsg, struct block_data *bd)
{
	int i;
	int err;
	struct obj_data od;

	copy_obj_desc(&od.obj_desc, bd->obj_desc);//duan
	od.obj_desc.code_owner = DSG_ID;
	od.obj_desc.ob_status = encode;
	memcpy(&od.gdim, &bd->gdim, sizeof(struct global_dimension));
	obj_put_update_dht(dsg, &od);
	return 0;
err_out:
	ERROR_TRACE();
}

//encode the block into lsb.duan

static int dsg_block_encode(struct ds_gspace *dsg)
{
	struct block_data * bd_data[NUM_DATA_DEVICE], *bd_code[NUM_CODE_DEVICE];
	struct obj_data od, *alternative_od[MAX_OB_IN_BD];
	struct obj_descriptor * odsc;
	int nu_test = 1, i, num_od, num_bd, err;//replication
	uint64_t size = 0;
	float replica_percent, size_replica, size_coding;
	//uloga("'%s()': server %d \n", __func__, DSG_ID);
	size_replica = dsg->ls->size_obj / (DSG_FT_LEVEL + 0.0);
	size_coding = NUM_DATA_DEVICE / (NUM_DATA_DEVICE + NUM_CODE_DEVICE + 0.0)*dsg->lsb->size_block;
	replica_percent = size_replica / (size_replica + size_coding);
	//uloga("'%s()': server %d replica_percent: %f \n", __func__, DSG_ID, replica_percent);
	//uloga("'%s()': dsg->ls->num_obj = %d dsg->ls->token = %d\n", __func__, dsg->ls->num_obj, dsg->ls->token);
	//if (dsg->ls->size_obj > INIT_THRESHOLD && replica_percent > REP_PERCENT &&  dsg->ls->token == token_on){
	if (dsg->ls->num_obj > INIT_THRESHOLD && (dsg->ls->token == token_on || DSG_ID == dsg_next_peer(DSG_ID))){
		num_od = 0;
		for (i = 0; i < nu_test; i++){
			alternative_od[i] = ls_cold_data_lookup(dsg->ls, i);//duan
			if (alternative_od[i] == NULL)
				break;
			//uloga("'%s()': server %d ls_cold_data_lookup\n", __func__, DSG_ID);
			num_od++;
			size += obj_data_size(&alternative_od[i]->obj_desc);

#ifdef DEBUG //duan
			{
				char *str;
				str = alloc_sprintf("S%2d: ls_cold_data_lookup for obj_desc '%s' ver %d for  ",
					DSG_ID, alternative_od[i]->obj_desc.name, alternative_od[i]->obj_desc.version);
				str = str_append(str, bbox_sprint(&alternative_od[i]->obj_desc.bb));

				uloga("'%s()': %s\n", __func__, str);
				free(str);
			}
#endif  //duan
		}
		dsg->lsb->size_block += size;//duan
		//uloga("'%s()': server %d block_data_alloc(alternative_od, num_od, size, bd_data, bd_code)\n", __func__, DSG_ID);
		block_data_alloc(alternative_od, num_od, size, bd_data, bd_code);
		//uloga("'%s()': server %d block_data_encode(bd_data, bd_code)\n", __func__, DSG_ID);
		block_data_encode(bd_data, bd_code);

		//	uloga("'%s()': server %d block_data_encode(bd_data, bd_code); done\n", __func__, DSG_ID);
		odsc = bd_data[0]->obj_desc;
		//uloga("'%s()': server %d dsg_block_put(bd_data, bd_code); begin\n", __func__, DSG_ID);
		dsg_block_put(bd_data, bd_code);//duan
		//uloga("'%s()': server %d dsg_block_put(bd_data, bd_code); done\n", __func__, DSG_ID);
		/* Wait for transaction to complete. */
		while (!dsg->num_pending) {
			err = ds_process(dsg->ds);
			if (err < 0) {
				uloga("'%s()': failed with %d.\n", __func__, err);
				break;
			}
		}

		int peer_id[DSG_FT_LEVEL];
		int num_peer, next_peer_id;
		num_peer = dsg_which_peer(DSG_ID, peer_id);//duan
		next_peer_id = dsg_next_peer(DSG_ID);//duan
		for (i = 0; i < num_peer; i++){//duan
			od.data = od._data = odsc;
			copy_obj_desc(&(od.obj_desc), odsc);
			memcpy(&od.gdim, &alternative_od[0]->gdim, sizeof(struct global_dimension));
			if (next_peer_id == peer_id[i]){
				dsg_obj_sync(&od, num_od, token_on, peer_id[i]);//replication 
			}
			else{
				dsg_obj_sync(&od, num_od, token_off, peer_id[i]);//replication 
			}
		}
		/* Wait for transaction to complete. */
		/*
		while (!dsg->num_pending) {
		err = ds_process(dsg->ds);
		if (err < 0) {
		uloga("'%s()': failed with %d.\n", __func__, err);
		break;
		}
		}
		*/
		for (i = 0; i < num_od; i++){
			//uloga("'%s()':num_od = %d\n", __func__, num_od);
			//uloga("'%s()':alternative_od[%d] = %p\n", __func__, i, alternative_od[i]);
#ifdef DEBUG //duan
			{
				char *str;
				str = alloc_sprintf("S%2d: obj_data_free for obj_desc '%s' ver %d for  ",
					DSG_ID, alternative_od[i]->obj_desc.name, alternative_od[i]->obj_desc.version);
				str = str_append(str, bbox_sprint(&alternative_od[i]->obj_desc.bb));

				uloga("'%s()': %s\n", __func__, str);
				free(str);
			}
#endif  //duan

			ls_remove(dsg->ls, alternative_od[i]);
			obj_data_free(alternative_od[i]);
		}
		//uloga("'%s()': done\n", __func__);
	}
	return 0;
}


//decode blocks into ls.duan

static int dsg_block_decode(struct ds_gspace *dsg, struct block_data * bd)
{
	struct block_data * bd_data[NUM_DATA_DEVICE] = { NULL }, *bd_code[NUM_CODE_DEVICE] = { NULL };
	struct obj_data *od;
	int i, j;
	uint64_t size = 0;
	int err;
	struct obj_descriptor odsc;
	//#ifdef DEBUG //duan
	//uloga("'%s()': at server %d begin.\n",__func__, DSG_ID);
	//#endif  //duan
	dsg_block_get_with_delete(bd_data, bd_code, bd);//duan
	/*
	while (!dsg->num_pending) {
	err = ds_process(dsg->ds);
	if (err < 0) {
	uloga("'%s()': failed with %d.\n", __func__, err);
	break;
	}
	}
	*/
	//alternative_od = (struct obj_data*) malloc(sizeof(struct obj_data)*MAX_OB_IN_BD);
	//uloga("'%s()': at server %d before DS_WAIT_COMPLETION(dsg->lsb->f_free == 1), dsg->lsb->f_free == %d.\n", __func__, DSG_ID, dsg->lsb->f_free);
	//DS_WAIT_COMPLETION(bd->f_free == 1);
	//uloga("'%s()': at server %d DS_WAIT_COMPLETION(dsg->lsb->f_free == 1), dsg->lsb->f_free == %d.\n", __func__, DSG_ID, dsg->lsb->f_free);

	block_data_decode(bd_data, bd_code);

	//block_data_transform(alternative_od, bd_data);

	//uloga("'%s()': at server %d 6 num_od = %d.\n", __func__, DSG_ID, num_od);
	copy_obj_desc(&odsc, bd->obj_desc);//duan
	od = obj_data_alloc(&odsc);
	memcpy(&od->gdim, &bd->gdim, sizeof(struct global_dimension));
	od->obj_desc.ob_status = replicate;
	od->obj_desc.owner = DSG_ID;
	ls_add_obj(dsg->ls, od);
	err = obj_put_update_dht(dsg, od);
	if (err < 0){
		uloga("'%s()': failed with %d.\n", __func__, err);
	}
	//uloga("'%s()': server  dsg_obj_replicate(alternative_od);\n", __func__);
	dsg_obj_replicate(od);//duan

	//dsg_block_sync(dsg, od);
	lsb_remove(dsg->lsb, bd);
	for (j = 0; j < NUM_CODE_DEVICE; j++){
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[j]->obj_desc[0].version = NUM_CODE_DEVICE.\n", __func__);
		//	uloga("'%s()': block_data_free(bd_data[j]), bd_code[%d] %p.\n", __func__, j, bd_code[j]);
		//block_data_free_without_odsc(bd_code[j]);
		free(bd_code[j]->data);
		free(bd_code[j]);
		//uloga("'%s()': block_data_free(bd_data[j]), bd_code[%d] %p.\n", __func__, j, bd_code[j]);
	}
	for (j = 0; j < NUM_DATA_DEVICE; j++){
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[%d]->obj_desc[0].version = %d.\n", __func__, j, bd_data[j]->obj_desc[0].version);
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[j]->obj_desc[0].version = .\n", __func__);
		//	if (j==0){
		//		block_data_free(bd_data[j]);
		//}
		if (j == NUM_DATA_DEVICE - 1){
			//uloga("'%s()': block_data_free(bd_data[j]), bd_data[j]->obj_desc[NUM_DATA_DEVICE - 1] .\n", __func__);
			block_data_free(bd_data[j]);
		}
		else{
			block_data_free_without_odsc(bd_data[j]);
		}
		//	uloga("'%s()': block_data_free(bd_data[j]), bd_data[%d] %p.\n", __func__, j, bd_data[j]);
	}


	return 0;
err_out:
	ERROR_TRACE();
}


//decode blocks into ls.duan

static struct obj_data * dsg_block_decode_to_obj(struct ds_gspace *dsg, struct block_data * bd, struct obj_descriptor * odsc)
{
	struct block_data * bd_data[NUM_DATA_DEVICE] = { NULL }, *bd_code[NUM_CODE_DEVICE] = { NULL };
	struct obj_data *od = NULL;
	int i, j, num_od;
	uint64_t size = 0;
	int err;

	//#ifdef DEBUG //duan
	//uloga("'%s()': at server %d begin.\n",__func__, DSG_ID);
	//#endif  //duan
	dsg_block_get_without_delete(bd_data, bd_code, bd);//duan
	/*
	while (!dsg->num_pending) {
	err = ds_process(dsg->ds);
	if (err < 0) {
	uloga("'%s()': failed with %d.\n", __func__, err);
	break;
	}
	}
	*/
	//alternative_od = (struct obj_data*) malloc(sizeof(struct obj_data)*MAX_OB_IN_BD);
	//uloga("'%s()': at server %d before DS_WAIT_COMPLETION(dsg->lsb->f_free == 1), dsg->lsb->f_free == %d.\n", __func__, DSG_ID, dsg->lsb->f_free);
	//DS_WAIT_COMPLETION(bd->f_free == 1);
	//uloga("'%s()': at server %d DS_WAIT_COMPLETION(dsg->lsb->f_free == 1), dsg->lsb->f_free == %d.\n", __func__, DSG_ID, dsg->lsb->f_free);
	//uloga("'%s()': at server %d begin1.\n", __func__, DSG_ID);
	block_data_decode(bd_data, bd_code);
	//uloga("'%s()': at server %d begin2.\n", __func__, DSG_ID);
	//num_od = block_data_transform(alternative_od, bd_data);

	//for (i = 0; i < num_od; i++){
	//uloga("'%s()': server  obj_data_free(od);\n", __func__);
	//	if (obj_desc_equals_intersect(odsc, &alternative_od[i]->obj_desc)){
	//	od = alternative_od[i];
	//	break;
	//}
	//}

	for (j = 0; j < NUM_CODE_DEVICE; j++){
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[j]->obj_desc[0].version = NUM_CODE_DEVICE.\n", __func__);
		//	uloga("'%s()': block_data_free(bd_data[j]), bd_code[%d] %p.\n", __func__, j, bd_code[j]);
		//block_data_free_without_odsc(bd_code[j]);
		if (bd != bd_code[j]){
			free(bd_code[j]->data);
			free(bd_code[j]);
		}
		//uloga("'%s()': block_data_free(bd_data[j]), bd_code[%d] %p.\n", __func__, j, bd_code[j]);
	}
	for (j = 0; j < NUM_DATA_DEVICE; j++){
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[%d]->obj_desc[0].version = %d.\n", __func__, j, bd_data[j]->obj_desc[0].version);
		//uloga("'%s()': block_data_free(bd_data[j]), bd_data[j]->obj_desc[0].version = .\n", __func__);
		//	if (j==0){
		//		block_data_free(bd_data[j]);
		//}
		if (bd != bd_data[j]){
			block_data_free_without_odsc(bd_data[j]);
		}
		//	uloga("'%s()': block_data_free(bd_data[j]), bd_data[%d] %p.\n", __func__, j, bd_data[j]);
	}

	return od;
err_out:
	ERROR_TRACE();
}


//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int block_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct block_data *bd = msg->private;

	//(*msg->sync_op_id) = 1;

	block_data_free(bd);//duan
	free(msg);

	//dsg_dec_pending();

#ifdef DEBUG  //duan
	uloga("'%s()': server %d finished.\n", __func__, DSG_ID);
#endif  //duan

	return 0;
}

static int dsg_block_put(struct block_data * bd_data[], struct block_data * bd_code[])//duan
{
	struct block_data *bd;
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_block_put *hdr;
	int err = -ENOMEM;
	uint64_t new_block_size, odsc_size;
	void * temp_data;

	int peer_id[NUM_DATA_DEVICE + NUM_CODE_DEVICE] = { 0 };
	int num_peer = dsg_which_code_peer(DSG_ID, peer_id);//duan
	int i;
	for (i = 0; i < NUM_DATA_DEVICE + NUM_CODE_DEVICE; i++){//duan

		if (i < NUM_DATA_DEVICE){
			bd = bd_data[i];//duan
		}
		else{
			bd = bd_code[i - NUM_DATA_DEVICE];
		}

		if (peer_id[i] == DSG_ID){
			//bd->obj_desc = (char *)bd->data + bd->block_size;
			lsb_add_block(dsg->lsb, bd);
			block_put_update_dht(dsg, bd);
			continue;
		}
		peer = ds_get_peer(dsg->ds, peer_id[i]);
		if (peer->ftmap.ns != node_normal){
			continue;
		}

		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		odsc_size = sizeof(struct obj_descriptor);
		new_block_size = odsc_size + bd->block_size;
		temp_data = bd->data;
		bd->_data = bd->data = malloc(new_block_size + 7);
		ALIGN_ADDR_QUAD_BYTES(bd->data);
		memcpy(bd->data, bd->obj_desc, odsc_size);
		memcpy(bd->data + odsc_size, temp_data, bd->block_size);
		free(temp_data);
		bd->obj_desc = NULL;

		msg->msg_data = bd->data;
		msg->size = new_block_size;
		msg->cb = block_put_completion;
		msg->private = bd;
		//uloga("'%s()': server %d new_block_size = %llu, bd->data = %d.\n", __func__, DSG_ID, new_block_size, bd->data);
		msg->msg_rpc->cmd = ss_block_put;
		msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

		hdr = (struct hdr_block_put *)msg->msg_rpc->pad;

		memcpy(&hdr->gdim, &bd->gdim, sizeof(struct global_dimension));
		hdr->block_size = bd->block_size;
		hdr->block_index = bd->block_index;
		hdr->bd_type = bd->bd_type;

		err = rpc_send(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
		dsg_inc_pending();

	}
	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int block_dsgrpc_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	//uloga("'%s()': server %d finished receiving0.\n", __func__, DSG_ID);
	int err;
	void * temp_data;
	uint64_t odsc_size;
	struct block_data *bd = msg->private;
	temp_data = bd->data;
	odsc_size = sizeof(struct obj_descriptor);

	bd->obj_desc = malloc(odsc_size);
	memcpy(bd->obj_desc, temp_data, odsc_size);

	bd->_data = bd->data = malloc(bd->block_size + 7);
	ALIGN_ADDR_QUAD_BYTES(bd->data);
	memcpy(bd->data, temp_data + odsc_size, bd->block_size);
	free(temp_data);

	lsb_add_block(dsg->lsb, bd);
	bd->obj_desc->code_owner = DSG_ID;//duan
	free(msg);

#ifdef DEBUG //duan
	{
		char *str;

		str = alloc_sprintf("S%2d: received block for obj_desc '%s' ver %d with  ",
			DSG_ID, bd->obj_desc->name, bd->obj_desc->version);
		str = str_append(str, bbox_sprint(&bd->obj_desc->bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif  //duan

	err = block_put_update_dht(dsg, bd);
	//uloga("'%s()': server %d finished receiving.\n", __func__, DSG_ID);
	if (err == 0)
		return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}


static int dsgrpc_block_put(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	struct hdr_block_put *hdr = (struct hdr_block_put *)cmd->pad;
	struct obj_descriptor *odsc;
	struct block_data *bd;
	struct node_id *peer;
	struct msg_buf *msg;
	int err;
	uint64_t new_block_size, odsc_size;

#ifdef FALSURE_DEBUG
	{//duan
		struct node_id *peer;
		peer = ds_get_peer(dsg->ds, DSG_ID);
		if (peer->ftmap.ns == failed){
			uloga("'%s()': simulate falsure.\n", __func__);
			goto err_out;
		}
	}
#endif

	odsc_size = sizeof(struct obj_descriptor)*hdr->num_odsc;
	new_block_size = odsc_size + hdr->block_size;

	err = -ENOMEM;
	peer = ds_get_peer(dsg->ds, cmd->id);
	bd = block_data_alloc_with_size(odsc, new_block_size);
	if (!bd)
		goto err_out;

	memcpy(&bd->gdim, &hdr->gdim, sizeof(struct global_dimension));
	bd->bd_type = hdr->bd_type;
	bd->block_index = hdr->block_index;
	bd->block_size = hdr->block_size;

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg)
		goto err_free_data;

	msg->msg_data = bd->data;
	msg->size = new_block_size;
	msg->private = bd;
	msg->cb = block_dsgrpc_put_completion;

#ifdef DEBUG
	uloga("'%s()': server %d start receiving.\n",
		__func__, DSG_ID);
#endif
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);

	if (err < 0)
		goto err_free_msg;

	// NOTE: This  early update, has  to be protected  by external
	//locks in the client code. 

	//err = obj_put_update_dht(dsg, bd);qqqq
	if (err == 0)
		return 0;
err_free_msg:
	free(msg);
err_free_data:
	free(bd);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}


//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int block_get_with_delete_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct block_data *bd = msg->private;
	void *data = msg->private;
	free(data);
	//bd->f_free = 1;
	//(*msg->sync_op_id) = 1;
	//bd->refcnt++;
	//if (bd->refcnt >= NUM_DATA_DEVICE + NUM_CODE_DEVICE){
	//	bd->f_free = 1;
	//}
	free(msg);

	//dsg->lsb->bd_recv++;
	//if (dsg->lsb->bd_recv >= dsg->lsb->bd_cnt){
	//	dsg->lsb->f_free = 1;
	//}
	//dsg_dec_pending();
	//uloga("'%s()': at server %d dsg->lsb->bd_recv = %d, dsg->lsb->bd_cnt = %d, dsg->lsb->f_free = %d.\n", __func__, DSG_ID, dsg->lsb->bd_recv, dsg->lsb->bd_cnt, dsg->lsb->f_free);
	//#ifdef DEBUG  //duan
	//uloga("'%s()': server %d finished sending  %s, version %d.\n", __func__, DSG_ID, bd->obj_desc.name, bd->obj_desc.version);
	//#endif  //duan

	return 0;
}

static int dsg_block_get_with_delete(struct block_data * bd_data[], struct block_data * bd_code[], struct block_data * bd)//duan
{

	struct block_data * bd_temp;
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_obj_get *oh;
	int i, err;//duan
	//uloga("'%s()': begin.\n", __func__);

	int peer_id[NUM_DATA_DEVICE + NUM_CODE_DEVICE] = { 0 };
	int num_peer = dsg_which_code_peer(DSG_ID, peer_id);//duan
	//dsg->lsb->bd_cnt = num_peer - 1;
	//dsg->lsb->bd_recv = 0;
	//dsg->lsb->f_free = 0;
	bd->refcnt = 1;
	bd->f_free = 0;
	for (i = 0; i < NUM_DATA_DEVICE + NUM_CODE_DEVICE; i++){//duan

		peer = ds_get_peer(dsg->ds, peer_id[i]);
		if (peer_id[i] != DSG_ID){
			if (peer->ftmap.ns != node_normal || peer->ftmap.start_time > bd->obj_desc->time_stamp){//duan
				bd_temp = block_data_alloc_copy_no_data(bd, NULL);
			}
			else{
				bd_temp = block_data_alloc_copy(bd);
			}
		}
		else{
			bd_temp = bd;
		}

		if (i < NUM_DATA_DEVICE){
			bd_data[i] = bd_temp;
		}
		else{
			bd_code[i - NUM_DATA_DEVICE] = bd_temp;
		}

		if (peer_id[i] == DSG_ID){
			//++dsg->lsb->bd_recv; 
			continue;
		}

		if (peer->ftmap.ns != node_normal || peer->ftmap.start_time > bd->obj_desc->time_stamp){//duan
			continue;
		}

		err = -ENOMEM;
		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg) {
			free(bd_temp->data);
			bd_temp->data = NULL;
			goto err_out;
		}
		//uloga("'%s()': bd_temp %p, bd_temp->data %p, bd_temp->block_size %d.\n", __func__, bd_temp, bd_temp->data, bd_temp->block_size);

		void *temp_data = malloc(bd_temp->block_size + 7);
		msg->msg_data = temp_data;
		msg->size = bd_temp->block_size;
		msg->cb = block_get_with_delete_completion;
		msg->private = temp_data;

		msg->msg_rpc->cmd = ss_block_get_with_delete;
		msg->msg_rpc->id = DSG_ID;

		oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
		copy_obj_desc(&oh->u.o.odsc, bd_temp->obj_desc);//duan
		memcpy(&oh->gdim, &bd_temp->gdim,
			sizeof(struct global_dimension));

		err = rpc_receive(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			free(bd_temp->data);
			bd_temp->data = NULL;
			goto err_out;
		}
		// TODO: uncomment next line ?!
		//dsg_inc_pending();

	}
	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}



//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int block_get_without_delete_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct block_data *bd = msg->private;
	void *data = msg->private;
	free(data);
	//bd->f_free = 1;
	//(*msg->sync_op_id) = 1;
	//bd->refcnt++;
	//if (bd->refcnt >= NUM_DATA_DEVICE + NUM_CODE_DEVICE){
	//	bd->f_free = 1;
	//}
	free(msg);

	//dsg->lsb->bd_recv++;
	//if (dsg->lsb->bd_recv >= dsg->lsb->bd_cnt){
	//	dsg->lsb->f_free = 1;
	//}
	//dsg_dec_pending();
	//uloga("'%s()': at server %d dsg->lsb->bd_recv = %d, dsg->lsb->bd_cnt = %d, dsg->lsb->f_free = %d.\n", __func__, DSG_ID, dsg->lsb->bd_recv, dsg->lsb->bd_cnt, dsg->lsb->f_free);
	//#ifdef DEBUG  //duan
	//uloga("'%s()': server %d finished sending  %s, version %d.\n", __func__, DSG_ID, bd->obj_desc.name, bd->obj_desc.version);
	//#endif  //duan
	//uloga("'%s()': S%2d: end.\n", __func__, DSG_ID);
	return 0;
}

static int dsg_block_get_without_delete(struct block_data * bd_data[], struct block_data * bd_code[], struct block_data * bd)//duan
{

	struct block_data * bd_temp;
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_obj_get *oh;
	int i, err;//duan
	//uloga("'%s()': begin.\n", __func__);

	int peer_id[NUM_DATA_DEVICE + NUM_CODE_DEVICE] = { 0 };
	int num_peer = dsg_which_code_peer(DSG_ID, peer_id);//duan
	//uloga("'%s()': begin0.\n", __func__);
	//dsg->lsb->bd_cnt = num_peer - 1;
	//dsg->lsb->bd_recv = 0;
	//dsg->lsb->f_free = 0;
	bd->refcnt = 1;
	bd->f_free = 0;
	for (i = 0; i < NUM_DATA_DEVICE + NUM_CODE_DEVICE; i++){//duan
		peer = ds_get_peer(dsg->ds, peer_id[i]);
		if (peer_id[i] != DSG_ID){
			if (peer->ftmap.ns != node_normal || peer->ftmap.start_time > bd->obj_desc->time_stamp){//duan
				bd_temp = block_data_alloc_copy_no_data(bd, NULL);
			}
			else{
				bd_temp = block_data_alloc_copy(bd);
			}
		}
		else{
			bd_temp = bd;
		}
		if (i < NUM_DATA_DEVICE){
			bd_data[i] = bd_temp;
		}
		else{
			bd_code[i - NUM_DATA_DEVICE] = bd_temp;
		}
		if (peer_id[i] == DSG_ID){
			//++dsg->lsb->bd_recv; 
			continue;
		}

		if (peer->ftmap.ns != node_normal || peer->ftmap.start_time > bd->obj_desc->time_stamp){//duan
			continue;
		}
		err = -ENOMEM;
		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg) {
			free(bd_temp->data);
			bd_temp->data = NULL;
			goto err_out;
		}
		//uloga("'%s()': bd_temp %p, bd_temp->data %p, bd_temp->block_size %d.\n", __func__, bd_temp, bd_temp->data, bd_temp->block_size);

		void *temp_data = malloc(bd_temp->block_size + 7);
		msg->msg_data = temp_data;
		msg->size = bd_temp->block_size;
		msg->cb = block_get_without_delete_completion;
		msg->private = temp_data;

		msg->msg_rpc->cmd = ss_block_get_without_delete;
		msg->msg_rpc->id = DSG_ID;

		oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
		copy_obj_desc(&oh->u.o.odsc, bd_temp->obj_desc);//duan
		memcpy(&oh->gdim, &bd_temp->gdim,
			sizeof(struct global_dimension));

		err = rpc_receive(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			free(bd_temp->data);
			bd_temp->data = NULL;
			goto err_out;
		}
		// TODO: uncomment next line ?!
		//dsg_inc_pending();

	}
	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}


//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int rpc_block_get_with_delete_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct block_data *bd = msg->private;

	lsb_remove(dsg->lsb, bd);
	block_data_free(bd);//temp duan
	//(*msg->sync_op_id) = 1;
	free(msg);
	//dsg_dec_pending();
#ifdef DEBUG //duan
	{
		uloga("'%s()': server %d finished.\n", __func__, DSG_ID);
	}
#endif  //duan
	return 0;
}

static int dsgrpc_block_get_with_delete(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	struct block_data  *from_bd = NULL;//duan
	int err, fast_v;

	fast_v = 0; // ERROR: iovec operation fails after Cray Portals
	// CRITICAL: use version here !!!
	err = -ENOMEM;
	peer = ds_get_peer(dsg->ds, cmd->id);
#ifdef DEBUG //duan
	{
		char *str;

		str = alloc_sprintf( "S%2d: request for obj_desc '%s' ver %d from C%2d for  ",
			DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
		str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif  //duan

	// CRITICAL: use version here !!!
	from_bd = lsb_find(dsg->lsb, &oh->u.o.odsc);
	//uloga("'%s()':from_bd %p\n", __func__, from_bd);
	//uloga("'%s()': from_bd->data %p.\n", __func__, from_bd->data);
	//uloga("'%s()': from_bd->block_size %d.\n", __func__, from_bd->block_size);
	if (!from_bd) {
		char *str;
		str = obj_desc_sprint(&oh->u.o.odsc);
		uloga("'%s()': block data %s is no found\n", __func__, str);//
		free(str);
		goto err_out;
	}

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg) {
		goto err_out;
	}
	msg->msg_data = from_bd->data;
	msg->size = from_bd->block_size;
	msg->cb = rpc_block_get_with_delete_completion;
	msg->private = from_bd;

	rpc_mem_info_cache(peer, msg, cmd);
	err = (fast_v) ? rpc_send_directv(rpc_s, peer, msg) : rpc_send_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	if (err == 0){
		//uloga("'%s()': err == 0\n", __func__);
		return 0;
	}

	free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int rpc_block_get_without_delete_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct block_data *bd = msg->private;
	//(*msg->sync_op_id) = 1;
	free(msg);
	//dsg_dec_pending();
#ifdef DEBUG //duan
	{
		uloga("'%s()': server %d finished.\n", __func__, DSG_ID);
	}
#endif  //duan
	return 0;
}

static int dsgrpc_block_get_without_delete(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
	struct node_id *peer;
	struct msg_buf *msg;
	struct block_data  *from_bd = NULL;//duan
	int err, fast_v;

	fast_v = 0; // ERROR: iovec operation fails after Cray Portals
	// CRITICAL: use version here !!!
	err = -ENOMEM;
	peer = ds_get_peer(dsg->ds, cmd->id);
#ifdef DEBUG //duan
	{
		char *str;

		str = alloc_sprintf("S%2d: request for obj_desc '%s' ver %d from C%2d for  ",
			DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
		str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif  //duan

	// CRITICAL: use version here !!!
	from_bd = lsb_find(dsg->lsb, &oh->u.o.odsc);
	//uloga("'%s()':from_bd %p\n", __func__, from_bd);
	//uloga("'%s()': from_bd->data %p.\n", __func__, from_bd->data);
	//uloga("'%s()': from_bd->block_size %d.\n", __func__, from_bd->block_size);
	if (!from_bd) {
		char *str;
		str = obj_desc_sprint(&oh->u.o.odsc);
		uloga("'%s()': block data %s is no found\n", __func__, str);//
		free(str);
		goto err_out;
	}

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg) {
		goto err_out;
	}
	msg->msg_data = from_bd->data;
	msg->size = from_bd->block_size;
	msg->cb = rpc_block_get_without_delete_completion;
	msg->private = from_bd;

	rpc_mem_info_cache(peer, msg, cmd);
	err = (fast_v) ? rpc_send_directv(rpc_s, peer, msg) : rpc_send_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);
	if (err == 0){
		//uloga("'%s()': err == 0\n", __func__);
		return 0;
	}

	free(msg);
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

//<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<duan
//Free resources after 'dsg_obj_replicate()' inserts an object in the space. synchronize
static int obj_sync_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct obj_data *od = msg->private;

	//(*msg->sync_op_id) = 1;

	//obj_data_free(od);
	free(msg);

	dsg_dec_pending();

#ifdef DEBUG  //duan
	uloga("'%s()': server %d finished sending  %s, version %d.\n",
		__func__, DSG_ID, od->obj_desc.name, od->obj_desc.version);
#endif  //duan

	return 0;
}

static int dsg_obj_sync(struct obj_data *od, int num_odsc, enum encode_token token, int peer_id)//duan
{
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_obj_syn *hdr;
	int err = -ENOMEM;
	if (peer_id == DSG_ID){
		dsg->ls->token = token;//duan
		return 0;
	}
	peer = ds_get_peer(dsg->ds, peer_id);
	msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
	if (!msg)
		goto err_out;

	msg->msg_data = od->data;
	msg->size = sizeof(struct obj_descriptor)*num_odsc;
	msg->cb = obj_sync_completion;
	msg->private = od;

	msg->msg_rpc->cmd = ss_obj_sync;
	msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

	hdr = (struct hdr_obj_syn *) msg->msg_rpc->pad;
	hdr->odsc = od->obj_desc;
	hdr->num_odsc = num_odsc;
	hdr->token = token;
	memcpy(&hdr->gdim, &od->gdim, sizeof(struct global_dimension));

	err = rpc_send(dsg->ds->rpc_s, peer, msg);
	if (err < 0) {
		free(msg);
		goto err_out;
	}
	dsg_inc_pending();

	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

static int obj_dsgrpc_sync_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct obj_data *temp_od, *od = msg->private;
	int i, num_odsc;
	struct obj_descriptor * odsc_arr;

	num_odsc = od->obj_size;
	odsc_arr = (struct obj_descriptor *) od->data;
	for (i = 0; i < num_odsc; i++){
		temp_od = NULL;
		temp_od = ls_find(dsg->ls, &(odsc_arr[i]));
		//uloga("'%s()': server  obj_data_free(od) temp_od = %p;\n", __func__, temp_od);
		if (temp_od){
			ls_remove(dsg->ls, temp_od);
			obj_data_free(temp_od);
			//uloga("'%s()': server  obj_data_free(od) done;\n", __func__);
		}
	}
	free(msg);
	obj_data_free(od);
#ifdef DEBUG
	uloga("'%s()': server %d finished receiving  %s, version %d.\n",
		__func__, DSG_ID, od->obj_desc.name, od->obj_desc.version);
#endif

	return 0;
}


static int dsgrpc_obj_sync(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	struct hdr_obj_syn *hdr = (struct hdr_obj_syn *)cmd->pad;
	struct obj_descriptor *odsc = &(hdr->odsc);
	struct obj_data *od;
	struct node_id *peer;
	struct msg_buf *msg;
	int err;

#ifdef FALSURE_DEBUG
	{//duan
		struct node_id *peer;
		peer = ds_get_peer(dsg->ds, DSG_ID);
		if (peer->ftmap.ns == failed){
			uloga("'%s()': simulate falsure.\n", __func__);
			goto err_out;
		}
	}
#endif
#ifdef DEBUG
	uloga("'%s()': server %d start receiving %s, version %d, hdr->num_odsc  %d.\n",
		__func__, DSG_ID, odsc->name, odsc->version, hdr->num_odsc);
#endif
	odsc->owner = DSG_ID;//duan
	err = -ENOMEM;
	peer = ds_get_peer(dsg->ds, cmd->id);

	od = obj_data_alloc_with_size(odsc, sizeof(struct obj_descriptor)*hdr->num_odsc);
	od->obj_size = hdr->num_odsc;

	if (!od)
		goto err_out;

	od->obj_desc.owner = DSG_ID;//duan
	memcpy(&od->gdim, &hdr->gdim, sizeof(struct global_dimension));
	dsg->ls->token = hdr->token;//duan
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg)
		goto err_free_data;

	msg->msg_data = od->data;
	msg->size = sizeof(struct obj_descriptor)*hdr->num_odsc;
	msg->private = od;
	msg->cb = obj_dsgrpc_sync_completion;

#ifdef DEBUG
	uloga("'%s()': server %d start receiving %s, version %d,  hdr->token  %d.\n",
		__func__, DSG_ID, odsc->name, odsc->version, hdr->token);
#endif
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);

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

static int dsgrpc_block_sync(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
	struct block_data * bd;
	struct obj_data od;
	int err;

#ifdef FALSURE_DEBUG
	{//duan
		struct node_id *peer;
		peer = ds_get_peer(dsg->ds, DSG_ID);
		if (peer->ftmap.ns == failed){
			uloga("'%s()': simulate falsure.\n", __func__);
			goto err_out;
		}
	}
#endif

#ifdef DEBUG //duan
	{
		char *str;

		str = alloc_sprintf( "S%2d: update obj_desc '%s' ver %d from S%2d for  ",
			DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
		str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif //duan

	bd = lsb_find(dsg->lsb, &oh->u.o.odsc);//duan
	if (bd == NULL){
		char *str;
		str = obj_desc_sprint(&oh->u.o.odsc);
		uloga("'%s()': block data %s is no found\n", __func__, str);//
		free(str);
		return 0;
	}
	lsb_remove(dsg->lsb, bd);
	block_data_free(bd);
	if (err < 0)
		goto err_out;

	return 0;
err_out:
	ERROR_TRACE();
}

/*
Update the DHT metadata with the new obj_descriptor information.
*/
static int dsg_block_sync(struct ds_gspace *dsg, struct obj_data *od)//duan
{
	struct obj_descriptor *odsc = &od->obj_desc;
	/* TODO: create a separate header structure for object
	updates; for now just abuse the hdr_obj_get. */
	struct hdr_obj_get *oh;
	struct msg_buf *msg;
	struct node_id *peer;
	int i, err;
	int peer_id[NUM_DATA_DEVICE + NUM_CODE_DEVICE] = { 0 };
	int num_peer = dsg_which_code_peer(DSG_ID, peer_id);//duan

	for (i = 0; i < NUM_DATA_DEVICE + NUM_CODE_DEVICE; i++){//duan

		if (peer_id[i] == DSG_ID){
			//++dsg->lsb->bd_recv; 
			continue;
		}

		peer = ds_get_peer(dsg->ds, peer_id[i]);
		if (peer->ftmap.ns != node_normal){
			continue;
		}
#ifdef DEBUG
		{
			char *str;

			str = alloc_sprintf( "S%2d: fwd obj_desc '%s' to S%2d ver %d for ",
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

		msg->msg_rpc->cmd = ss_block_sync;
		msg->msg_rpc->id = DSG_ID;

		oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
		oh->u.o.odsc = *odsc;
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

//Free resources after 'dsg_obj_replicate()' inserts an object in the space.
static int obj_replicate_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct obj_data *od = msg->private;

	//(*msg->sync_op_id) = 1;

	//obj_data_free(od);
	free(msg);

	//dsg_dec_pending();

#ifdef DEBUG  //duan
	uloga("'%s()': server %d finished sending  %s, version %d.\n",
		__func__, DSG_ID, od->obj_desc.name, od->obj_desc.version);
#endif  //duan

	return 0;
}

static int dsg_obj_replicate(struct obj_data *od)//duan
{
	struct msg_buf *msg;
	struct node_id *peer;
	struct hdr_obj_put *hdr;
	int err = -ENOMEM;
	int peer_id[DSG_FT_LEVEL];
	int num_peer = dsg_which_peer(DSG_ID, peer_id);//duan
	int i;//duan
	for (i = 0; i < num_peer; i++){//duan
		if (peer_id[i] == DSG_ID){ continue; }
		peer = ds_get_peer(dsg->ds, peer_id[i]);
		msg = msg_buf_alloc(dsg->ds->rpc_s, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_data = od->data;
		msg->size = obj_data_size(&od->obj_desc);
		msg->cb = obj_replicate_completion;
		msg->private = od;

		msg->msg_rpc->cmd = ss_obj_replicate;
		msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

		hdr = (struct hdr_obj_put *) msg->msg_rpc->pad;
		hdr->odsc = od->obj_desc;
		memcpy(&hdr->gdim, &od->gdim, sizeof(struct global_dimension));

		err = rpc_send(dsg->ds->rpc_s, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}
	//dsg_inc_pending();

	return 0;
err_out:
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
*/
static int obj_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{

	//uloga("'%s()': server %d start receiving.\n",__func__, DSG_ID);
	struct block_data *from_lsb = NULL;
	struct obj_data *od = msg->private;

	ls_add_obj(dsg->ls, od);
	//duan
	///
	//dsg_block_encode(dsg);//duan replication
	if (ls_find(dsg->ls, &od->obj_desc) != NULL){
		obj_put_update_dht(dsg, od);//duan add
		dsg_obj_replicate(od);//duan replication
		from_lsb = lsb_find(dsg->lsb, &od->obj_desc);
		if (from_lsb != NULL){//duan		
			dsg_block_sync(dsg, od);//duan 
			lsb_remove(dsg->lsb, from_lsb);
		}
		//
	}

#ifdef DS_SYNC_MSG
    struct msg_buf *msg_ds;
    struct node_id *peer_ds;
    struct hdr_obj_put *hdr_ds;
    int err = -ENOMEM;




    peer_ds = (struct node_id*)msg->peer;
    msg_ds = msg_buf_alloc(rpc_s, peer_ds, 1);

    msg_ds->msg_rpc->cmd = ds_put_completion;
    msg_ds->msg_rpc->id = DSG_ID;
    msg_ds->cb = obj_put_sync_completion;

    hdr_ds = (struct hdr_obj_put *)msg_ds->msg_rpc->pad;
    hdr_ds->sync_op_id_ptr = msg->sync_op_id;

    err = rpc_send(rpc_s, peer_ds, msg_ds);


    if (err < 0){
        free(msg_ds);
        uloga("%s(): rpc_send fail from ds_put_completion\n",__func__);

    }
#endif
    
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
	struct block_data *from_lsb = NULL; //duan
	struct node_id *peer;
	struct msg_buf *msg;
	int err;



	//uloga("'%s()': sizeof obj_descriptor = %d, sizeof obj_descriptor_orignal = %d\n", __func__, sizeof(struct obj_descriptor), sizeof(struct obj_descriptor_orignal));//duan
	odsc->owner = DSG_ID;//duan
	//odsc->num_owner = DSG_FT_LEVEL;//duan
	err = -ENOMEM;
	peer = ds_get_peer(dsg->ds, cmd->id);

#ifdef SHMEM_OBJECTS
	od = shmem_obj_data_alloc(odsc, DSG_ID);
#endif

#ifndef SHMEM_OBJECTS
	od = obj_data_alloc(odsc);
#endif
	if (!od)
		goto err_out;
	od->obj_desc.owner = DSG_ID;//duan
	//od->obj_desc.num_owner = DSG_FT_LEVEL;//duan
	od->obj_desc.ob_status = replicate;//duan
	memcpy(&od->gdim, &hdr->gdim, sizeof(struct global_dimension));
	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg)
		goto err_free_data;

        msg->msg_data = od->data;
        msg->size = obj_data_size(&od->obj_desc);
        msg->private = od;
        msg->cb = obj_put_completion;
#ifdef DS_SYNC_MSG
        msg->sync_op_id = hdr->sync_op_id_ptr; //synchronization lock pointer passed from client
        msg->peer = peer;
#endif
	//#ifdef DEBUG //duan
	{//duan
		char *str;

		str = alloc_sprintf( "S%2d: dspaces_put obj_desc '%s' ver %d for  ",
			DSG_ID, odsc->name, odsc->version);
		str = str_append(str, bbox_sprint(&odsc->bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
	//#endif  //duan
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);

	if (err < 0)
		goto err_free_msg;

	/* NOTE: This  early update, has  to be protected  by external
	locks in the client code. */
	//from_lsb = lsb_find(dsg->lsb, &hdr->odsc);
	//if (from_lsb && (dsg->ls->token == token_on || DSG_ID == dsg_next_peer(DSG_ID))){//duan replication
	//if (from_lsb){//duan replication
	//dsg_block_decode(dsg, from_lsb);//duan
	//}//duan
	//err = obj_put_update_dht(dsg, od);//duan delete
	//if (from_lsb == NULL){
	//dsg_block_encode(dsg);//duan
	//}
#ifdef DEBUG //duan
	uloga("'%s()': server %d end receiving %s, version %d.\n",
		__func__, DSG_ID, odsc->name, odsc->version);
#endif  //duan
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

//duan>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

/*

static int ft_status_update(struct ds_gspace *dsg, struct obj_data *od)
{

}
*/

/*
Rpc routine to update (add or insert) an object descriptor in the
dht table.

static int dsgrpc_ft_status_update(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
struct sspace* ssd = lookup_sspace(dsg, oh->u.o.odsc.name, &oh->gdim);
struct dht_entry *de = ssd->ent_self;
int err;

#ifdef DEBUG
{
char *str;

str = alloc_sprintf( "S%2d: update obj_desc '%s' ver %d from S%2d for  ",
DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

uloga("'%s()': %s\n", __func__, str);
free(str);
}
#endif
oh->u.o.odsc.owner = cmd->id;//duan
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
*/



static int obj_dsgrpc_replicate_completion(struct rpc_server *rpc_s, struct msg_buf *msg)//duan
{
	struct obj_data *od = msg->private;
	ls_add_obj(dsg->ls, od);

	//dsg_block_encode(dsg);//duan replication

	if (ls_find(dsg->ls, &od->obj_desc) != NULL){
		obj_put_update_dht(dsg, od);//duan add
	}
	free(msg);
#ifdef DEBUG
	uloga("'%s()': server %d finished receiving  %s, version %d.\n",
		__func__, DSG_ID, od->obj_desc.name, od->obj_desc.version);
#endif

	return 0;
}


static int dsgrpc_obj_replicate(struct rpc_server *rpc_s, struct rpc_cmd *cmd)//duan
{
	struct hdr_obj_put *hdr = (struct hdr_obj_put *)cmd->pad;
	struct obj_descriptor *odsc = &(hdr->odsc);
	struct obj_data *od;
	struct node_id *peer;
	struct msg_buf *msg;
	int err;

#ifdef FALSURE_DEBUG
	{//duan
		struct node_id *peer;
		peer = ds_get_peer(dsg->ds, DSG_ID);
		if (peer->ftmap.ns == failed){
			uloga("'%s()': simulate falsure.\n", __func__);
			goto err_out;
		}
	}
#endif

	odsc->owner = DSG_ID;//duan
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
	msg->cb = obj_dsgrpc_replicate_completion;


#ifdef DEBUG
	uloga("'%s()': server %d start receiving %s, version %d.\n",
		__func__, DSG_ID, odsc->name, odsc->version);
#endif
	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
	rpc_mem_info_reset(peer, msg, cmd);

	//if (err < 0)
	//goto err_free_msg;

	/* NOTE: This  early update, has  to be protected  by external
	locks in the client code. */

	//err = obj_put_update_dht(dsg, od);
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

static int obj_meta_get_completion_data(struct rpc_server *rpc_s, struct msg_buf *msg)
{
		free(msg->msg_data);
        free(msg);
        return 0;
}

static int obj_meta_get_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        free(msg);
        return 0;
}

static int dsgrpc_obj_get_next_meta(struct rpc_server *rpc_s, struct rpc_cmd *cmd){
    struct msg_buf *msg;
    struct hdr_nvars_get *oh = (struct hdr_nvars_get *) cmd->pad;
    struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
    int curr_version = oh->current_version;
    enum storage_type st = column_major;
    struct obj_data *od, *from_obj;
    int latest_v;
    int *var_data;
	var_data=malloc(sizeof(int)*2);
	var_data[0]=-3;
	var_data[1]=-3;

    struct obj_descriptor *pref_odsc;
    pref_odsc= (struct obj_descriptor *) malloc(sizeof(struct obj_descriptor));
    pref_odsc->version = curr_version;
    pref_odsc->owner = -1;
    pref_odsc->st = st;
    pref_odsc->size = sizeof(char);
    pref_odsc->bb.num_dims = 1;

    memset(pref_odsc->bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(pref_odsc->bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

    sprintf(pref_odsc->name, "VARMETA@%s", oh->f_name);
    int err = -ENOMEM;

    from_obj = ls_find_next(dsg->ls, pref_odsc);
    if (!from_obj) {
        uloga("End of stream. Current step is the maximum\n");
        goto send_data;
    }

    var_data[0] = (from_obj->obj_desc.bb.ub.c[0]+1)*sizeof(char);
    var_data[1] = from_obj->obj_desc.version;
    send_data:
	msg = msg_buf_alloc(rpc_s, peer, 0);
    	if (!msg) {
            goto err_out;
   	 }
    	msg->msg_data = var_data;
    	msg->size = sizeof(int)*2;
    	msg->cb = obj_meta_get_completion_data;

    	rpc_mem_info_cache(peer, msg, cmd);
    	err = rpc_send_direct(rpc_s, peer, msg);
    	rpc_mem_info_reset(peer, msg, cmd);
    	if (err == 0)
            return 0;
    	free(msg);
    err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

static int dsgrpc_obj_get_latest_meta(struct rpc_server *rpc_s, struct rpc_cmd *cmd){
    struct msg_buf *msg;
    struct hdr_nvars_get *oh = (struct hdr_nvars_get *) cmd->pad;
    struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
    int curr_version = oh->current_version;
    enum storage_type st = column_major;
    struct obj_data *od, *from_obj;
    int latest_v;
    int *var_data;
	var_data=malloc(sizeof(int)*2);
	var_data[0]=-3;
	var_data[1]=-3;

    struct obj_descriptor *pref_odsc;
    pref_odsc= (struct obj_descriptor *) malloc(sizeof(struct obj_descriptor));
    pref_odsc->version = curr_version;
    pref_odsc->owner = -1;
    pref_odsc->st = st;
    pref_odsc->size = sizeof(char);
    pref_odsc->bb.num_dims = 1;

    memset(pref_odsc->bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(pref_odsc->bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    int err = -ENOMEM;
    sprintf(pref_odsc->name, "VARMETA@%s", oh->f_name);
    from_obj = ls_find_latest(dsg->ls, pref_odsc);
    if (!from_obj) {
        uloga("End of stream. Current data is the latest\n");
        goto send_data;
    }

    //var_data[0] = ((int*)(from_obj->data))[0];
    var_data[0] = (from_obj->obj_desc.bb.ub.c[0]+1)*sizeof(char);
    var_data[1] = from_obj->obj_desc.version;
    send_data:
    msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg) {
            goto err_out;
     }
        msg->msg_data = var_data;
        msg->size = sizeof(int)*2;
        msg->cb = obj_meta_get_completion_data;

        rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_send_direct(rpc_s, peer, msg);
        rpc_mem_info_reset(peer, msg, cmd);
        if (err == 0)
            return 0;
        free(msg);
    err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

static int dsgrpc_obj_get_var_meta(struct rpc_server *rpc_s, struct rpc_cmd *cmd){
    struct msg_buf *msg;
    struct hdr_var_meta_get *oh = (struct hdr_var_meta_get *) cmd->pad;
    struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
    enum storage_type st = column_major;
    struct obj_data *od, *from_obj;
    int latest_v;
    int var_data[2];

    struct obj_descriptor *pref_odsc;
    pref_odsc= (struct obj_descriptor *) malloc(sizeof(struct obj_descriptor));
    pref_odsc->version = oh->current_version;
    pref_odsc->owner = -1;
    pref_odsc->st = st;
    pref_odsc->size = sizeof(char);
    pref_odsc->bb.num_dims = 1;
    memset(pref_odsc->bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(pref_odsc->bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
	//    pref_odsc->bb.lb.c[0] = sizeof(int)/sizeof(char);
  	//  pref_odsc->bb.ub.c[0] = oh->length + pref_odsc->bb.lb.c[0]-1;
    sprintf(pref_odsc->name, "VARMETA@%s", oh->f_name);
    int err = -ENOMEM;
    from_obj = ls_find(dsg->ls, pref_odsc);
    if (!from_obj) {
        uloga("'%s()': Metdata Object not found. Should not happen\n", __func__);
        goto err_out;
    }
    msg = msg_buf_alloc(rpc_s, peer, 0);
    if (!msg) {
            goto err_out;
    }
    msg->msg_data = from_obj->data;
    msg->size = oh->length;
    msg->cb = obj_meta_get_completion;

    rpc_mem_info_cache(peer, msg, cmd);
    err = rpc_send_direct(rpc_s, peer, msg);
    rpc_mem_info_reset(peer, msg, cmd);
    if (err == 0)
            return 0;

    free(msg);
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
        peer_id_tab = malloc(sizeof(int) * (peer_num+1));
        if (!peer_id_tab)
                goto err_out;
        for (i = 0; i < peer_num; i++){
                peer_id_tab[i] = de_tab[i]->rank;
                //uloga("Id in peer tab %d \n", peer_id_tab[i]);
            }
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
  Rpc routine to  locate the servers that may  have object descriptors
  that overlap the descriptor in the query.
*/
static int dsgrpc_obj_query(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct dht_entry *de_tab[dsg->ssd->dht->num_entries];
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
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int dsgrpc_obj_info(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct hdr_obj_get *oh = (struct hdr_obj_get *) cmd->pad;
        struct node_id *peer = ds_get_peer(dsg->ds, cmd->id);
        int err = -ENOENT;

        peer = ds_get_peer(dsg->ds, oh->rank);
        err = obj_info_reply_descriptor(peer, &oh->u.o.odsc);
        if (err < 0)
                goto err_out;
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

static inline int obj_owner_redirect(struct obj_descriptor *odsc)//duan
{
	int i, j, err, peer_id;
	struct node_id *peer;
	int peer_id_arr[NUM_DATA_DEVICE + NUM_CODE_DEVICE] = { 0 };

	if (odsc->ob_status == encode || odsc->ob_status == encoding){
		peer_id = odsc->code_owner;
		dsg_which_code_peer(peer_id, peer_id_arr);//duan
		for (i = 0; i < NUM_DATA_DEVICE; i++){
			peer = ds_get_peer(dsg->ds, peer_id_arr[i]);
			odsc->code_owner = peer_id_arr[0];//
			//uloga("'%s()': peer_id = %d, peer->ftmap.ns = %d, peer->ftmap.start_time = %f, odsc->time_stamp = %f, peer->ftmap.start_time > odsc->time_stamp = %d.\n", __func__, peer_id, peer->ftmap.ns, peer->ftmap.start_time, odsc->time_stamp, peer->ftmap.start_time > odsc->time_stamp);
			if (peer->ftmap.ns != node_normal || peer->ftmap.start_time > odsc->time_stamp){//duan
				odsc->ob_status = deleting;
				for (j = 0; j < NUM_DATA_DEVICE + NUM_CODE_DEVICE; j++){
					peer = ds_get_peer(dsg->ds, peer_id_arr[j]);//duan
					if (peer->ftmap.ns == node_normal){
						odsc->owner = peer_id_arr[j];
						uloga("'%s()': redirect to peer_id = %d.\n", __func__, peer_id_arr[j]);
						break;
					}
				}
			}
		}
	}

#ifdef DEBUG //duan
	{
		char *str;
		str = alloc_sprintf( "S%2d: for obj_desc '%s' ver %d ob_status %d owner %d code_owner %d for  ",
			DSG_ID, odsc->name, odsc->version, odsc->ob_status, odsc->owner, odsc->code_owner);
		str = str_append(str, bbox_sprint(&odsc->bb));

		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
#endif  //duan

	return 0;
err_out:
	err = 1;
	uloga("'%s()': failed with %d.\n", __func__, err);
	return err;
}

/*
  RPC  routine to  send the  object  descriptors that  match the  data
  object being queried.
  //RPC request changed such that original request descriptor in server and intersection is 
  //both sent to the client.
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

        str = alloc_sprintf("S%2d: obj_desc not found for ", DSG_ID);
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
        #ifdef SHMEM_OBJECTS
            odsc_tab = malloc(sizeof(*odsc_tab) * num_odsc*2);
        #endif

        #ifndef SHMEM_OBJECTS
            odsc_tab = malloc(sizeof(*odsc_tab) * num_odsc);
        #endif
        if (!odsc_tab)
                goto err_out;

        for (i = 0; i < num_odsc; i++) {
            odsc = *podsc[i];
            /* Preserve storage type at the destination. */
            odsc.st = oh->u.o.odsc.st;
            #ifdef SHMEM_OBJECTS
                odsc_tab[i+num_odsc] = odsc;
            #endif
            bbox_intersect(&oh->u.o.odsc.bb, &odsc.bb, &odsc.bb);
			//uloga("'%s()':num_odsc %d\n", __func__, num_odsc);//duan
			obj_owner_redirect(&odsc);//duan
            odsc_tab[i] = odsc;

            //need to prefetch here based on obj_owner for prefetch staging

            //uloga("Asked from obj from server %d in current server %d for object owner %d\n", ds_get_peer(dsg->ds, odsc.owner)->ptlmap.id, DSG_ID, odsc.owner);

        }

        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (!msg) {
                free(odsc_tab);
                goto err_out;
        }
        #ifdef SHMEM_OBJECTS
        msg->size = sizeof(*odsc_tab) * num_odsc*2;
        #endif
        #ifndef SHMEM_OBJECTS
        msg->size = sizeof(*odsc_tab) * num_odsc;
        #endif
        msg->msg_data = odsc_tab;
        msg->cb = obj_get_desc_completion;

        msg->msg_rpc->cmd = ss_obj_get_desc;
        msg->msg_rpc->id = DSG_ID; // dsg->ds->self->id;

        i = oh->qid;
        oh = (struct hdr_obj_get *) msg->msg_rpc->pad;
        #ifdef SHMEM_OBJECTS
        oh->u.o.num_de = num_odsc*2;
        #endif
        #ifndef SHMEM_OBJECTS
        oh->u.o.num_de = num_odsc;
        #endif
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
		struct obj_data *od, *from_obj = NULL, *from_ls_obj = NULL;//duan
		struct obj_data  from_lsb_obj;//duan
		struct block_data  *from_block = NULL;//duan

        int err = -ENOENT; 

        peer = ds_get_peer(dsg->ds, cmd->id);

#ifdef DEBUG
 {
	 char *str;
	 
	 str = alloc_sprintf("S%2d: request for obj_desc '%s' ver %d from C%2d for  ",
    	  DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version, cmd->id);
     str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));

 	 uloga("'%s()': %s\n", __func__, str);
	 free(str);
 }
#endif
/*
        // CRITICAL: use version here !!!
        from_obj = ls_find(dsg->ls, &oh->u.o.odsc);
        if (!from_obj) {
            char *str;
            str = obj_desc_sprint(&oh->u.o.odsc);
            uloga("'%s()': %s\n", __func__, str);
            free(str);
            goto err_out;
        }
*/
		if(oh->u.o.odsc.ob_status == replicate || oh->u.o.odsc.ob_status == replicating){
			if (from_obj = ls_find(dsg->ls, &oh->u.o.odsc)) {//duan
				//uloga("'%s()': S%2d: from_obj = ls_find(dsg->ls, &oh->u.o.odsc)\n", __func__, DSG_ID);
				//TODO:  if required  object is  not  found, I  should send  a
				//proper error message back, and the remote node should handle
				//the error.

        /*
          Problem: How can I send  a transfer status together with the
          data ? (1)  piggyback it with the data,  (2) send a separate
          RPC in response to a transfer.
        */

        // Update (oh->odsc.st == from_obj->obj_desc.st);

        err = -ENOMEM;
        // CRITICAL:     experimental    stuff,     assumption    data
        // representation is the same on both ends.
        // od = obj_data_alloc(&oh->odsc);
        od = obj_data_alloc(&oh->u.o.odsc);
        if (!od)
                goto err_out;
        ssd_copy(od, from_obj);
        od->obj_ref = from_obj;

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg) {
                obj_data_free(od);
                goto err_out;
        }

        msg->msg_data = od->data;
        msg->size = obj_data_size(&od->obj_desc);
        msg->cb = obj_get_completion;
        msg->private = od;

				rpc_mem_info_cache(peer, msg, cmd);
				err = rpc_send_direct(rpc_s, peer, msg);
				rpc_mem_info_reset(peer, msg, cmd);
				if (err == 0){
					return 0;
				}
				uloga("'%s()': err != 0\n", __func__);
			}else{
				char *str;
				str = alloc_sprintf( "S%2d is NO object data for obj_desc '%s' ver %d  ",
					DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version);
				str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));
				uloga("'%s()': %s\n", __func__, str);
				free(str);
				//goto err_out;

				od = obj_data_alloc_with_size(&oh->u.o.odsc, obj_data_size(&oh->u.o.odsc));
				if (!od)
					goto err_out;


				msg = msg_buf_alloc(rpc_s, peer, 0);
				if (!msg) {
					obj_data_free(od);
					goto err_out;
				}
				//uloga("'%s()': step 12, od %p\n", __func__, od);
				msg->msg_data = od->data;
				msg->size = obj_data_size(&od->obj_desc);
				msg->cb = obj_get_completion;
				msg->private = od;
				rpc_mem_info_cache(peer, msg, cmd);
				err = rpc_send_direct(rpc_s, peer, msg);
				rpc_mem_info_reset(peer, msg, cmd);
				if (err == 0)
					return 0;
			}
		}else if(oh->u.o.odsc.ob_status == encode || oh->u.o.odsc.ob_status == encoding){
			if (from_block = lsb_find(dsg->lsb, &oh->u.o.odsc)){

				//uloga("'%s()': S%2d: from_block = lsb_find(dsg->lsb, &oh->u.o.odsc)\n", __func__, DSG_ID);		
				//od->obj_ref = from_block;
				int num_sub_ob = NUM_DATA_DEVICE;
				od = obj_data_alloc_with_size(&oh->u.o.odsc, obj_data_size(&oh->u.o.odsc) / num_sub_ob);
				if (!od)
					goto err_out;


				msg = msg_buf_alloc(rpc_s, peer, 0);
				if (!msg) {
					obj_data_free(od);
					goto err_out;
				}
				msg->msg_data = od->data;
				msg->size = obj_data_size(&od->obj_desc) / num_sub_ob;
				msg->cb = obj_get_completion;
				msg->private = od;
				rpc_mem_info_cache(peer, msg, cmd);
				err = rpc_send_direct(rpc_s, peer, msg);
				rpc_mem_info_reset(peer, msg, cmd);
				if (err == 0)
					return 0;
			}else{
				char *str;
				str = alloc_sprintf( "S%2d is NO block data for obj_desc '%s' ver %d  ",
					DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version);
				str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));
				uloga("'%s()': %s\n", __func__, str);
				free(str);
				//goto err_out;

				int num_sub_ob = NUM_DATA_DEVICE;
				//uloga("'%s()': num_sub_ob = %d \n", __func__, num_sub_ob);
				od = obj_data_alloc_with_size(&oh->u.o.odsc, obj_data_size(&oh->u.o.odsc) / num_sub_ob);
				if (!od)
					goto err_out;


				msg = msg_buf_alloc(rpc_s, peer, 0);
				if (!msg) {
					obj_data_free(od);
					goto err_out;
				}
				//uloga("'%s()': step 12, od %p\n", __func__, od);
				msg->msg_data = od->data;
				msg->size = obj_data_size(&od->obj_desc) / num_sub_ob;
				msg->cb = obj_get_completion;
				msg->private = od;
				rpc_mem_info_cache(peer, msg, cmd);
				err = rpc_send_direct(rpc_s, peer, msg);
				rpc_mem_info_reset(peer, msg, cmd);
				if (err == 0)
					return 0;
			}
		}else{
			if (from_block = lsb_find(dsg->lsb, &oh->u.o.odsc)){
				if (dsg_number_code_peer(DSG_ID) == NUM_DATA_DEVICE + NUM_CODE_DEVICE){
					uloga("'%s()': S%2d: lazy recovery\n", __func__, DSG_ID);
					dsg_block_decode(dsg, from_block);//duan lazy recovery
				}
				else{
					uloga("'%s()': S%2d: degrade reading\n", __func__, DSG_ID);
					dsg_block_decode_to_obj(dsg, from_block, &oh->u.o.odsc);//duan degrade reading
				}
			}
			else{
				char *str;
				str = alloc_sprintf( "S%2d is NO recovery block data for obj_desc '%s' ver %d  ",
					DSG_ID, oh->u.o.odsc.name, oh->u.o.odsc.version);
				str = str_append(str, bbox_sprint(&oh->u.o.odsc.bb));
				uloga("'%s()': %s\n", __func__, str);
				free(str);
				//goto err_out;
			}
			err = -ENOMEM;
			// CRITICAL:     experimental    stuff,     assumption    data
			// representation is the same on both ends.
			// od = obj_data_alloc(&oh->odsc);
			od = obj_data_alloc(&oh->u.o.odsc);
			if (!od)
				goto err_out;

			//ssd_copy(od, from_obj);
			//od->obj_ref = from_obj;
			msg = msg_buf_alloc(rpc_s, peer, 0);
			if (!msg) {
				obj_data_free(od);
				goto err_out;
			}
			msg->msg_data = od->data;
			msg->size = obj_data_size(&od->obj_desc);
			msg->cb = obj_get_completion;
			msg->private = od;
			rpc_mem_info_cache(peer, msg, cmd);
			err = rpc_send_direct(rpc_s, peer, msg);
			rpc_mem_info_reset(peer, msg, cmd);
			if (err == 0)
				return 0;
		}
 
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

static int dsgrpc_ss_kill(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	        int err;
	        err = -ENOMEM;
	        dsg->kill = 1;
	        uloga("Server received kill command. Going to shut down...\n");
	        sleep(3);
	        return 0;
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
            err = -EINVAL;
            goto err_out;
        }

        // Check hash version
        if ((ds_conf.hash_version < ssd_hash_version_v1) ||
            (ds_conf.hash_version >= _ssd_hash_version_count)) {
            uloga("%s(): ERROR unknown hash version %d in file '%s'\n",
                __func__, ds_conf.hash_version, conf_name);
            err = -EINVAL;
            goto err_out;
        }

       if((ds_conf.lock_type < lock_generic) ||
            (ds_conf.lock_type >= _lock_type_count)) {
            uloga("%s(): ERROR unknown lock type %d in file '%s'\n",
                __func__, ds_conf.lock_type, conf_name);
            err = -EINVAL;
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
       	rpc_add_service(ss_obj_get_next_meta, dsgrpc_obj_get_next_meta);
        rpc_add_service(ss_obj_get_latest_meta, dsgrpc_obj_get_latest_meta);
        rpc_add_service(ss_obj_get_var_meta, dsgrpc_obj_get_var_meta);
		rpc_add_service(ss_obj_sync, dsgrpc_obj_sync);//duan
		rpc_add_service(ss_obj_replicate, dsgrpc_obj_replicate);//duan
		rpc_add_service(ss_block_put, dsgrpc_block_put);//duan
		rpc_add_service(ss_block_get_with_delete, dsgrpc_block_get_with_delete);//duan
		rpc_add_service(ss_block_get_without_delete, dsgrpc_block_get_without_delete);//duan
		rpc_add_service(ss_block_sync, dsgrpc_block_sync);//duan	
		rpc_add_service(ss_peer_update, dsgrpc_peer_update);//duan
		rpc_add_service(ss_peer_test, dsgrpc_peer_test);//duan
        rpc_add_service(ss_obj_update, dsgrpc_obj_update);
        rpc_add_service(ss_obj_filter, dsgrpc_obj_filter);
        rpc_add_service(cp_lock, dsgrpc_lock_service);
        rpc_add_service(cp_remove, dsgrpc_remove_service);
        rpc_add_service(ss_info, dsgrpc_ss_info);
        rpc_add_service(ss_kill, dsgrpc_ss_kill);
#ifdef DS_HAVE_ACTIVESPACE
        rpc_add_service(ss_code_put, dsgrpc_bin_code_put);
#endif
        INIT_LIST_HEAD(&dsg_l->cq_list);
        INIT_LIST_HEAD(&dsg_l->obj_desc_req_list);
        INIT_LIST_HEAD(&dsg_l->obj_data_req_list);
        INIT_LIST_HEAD(&dsg_l->locks_list);

        dsg_l->ds = ds_alloc(num_sp, num_cp, DSG_FT_LEVEL, NUM_DATA_DEVICE + NUM_CODE_DEVICE, dsg_l, comm);//duan
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
		if (dsg_l->ds->self->ftmap.ftid % dsg_l->ds->size_ft_group == 0){ //duan
			dsg_l->ls->token = token_on; 
		}else{
			dsg_l->ls->token = token_off;
		}//duan
		dsg_l->lsb = lsb_alloc(ds_conf.max_versions);//duan
		if (!dsg_l->lsb) {
			uloga("%s(): ERROR lsb_alloc() failed\n", __func__);
			goto err_free;
		}
		dsg->kill = 0;
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
		//uloga("'%s()': server %d before lsb_free(dsg->lsb);\n", __func__, DSG_ID);//duan
		lsb_free(dsg->lsb);//duan
		//uloga("'%s()': server %d after lsb_free(dsg->lsb);\n", __func__, DSG_ID);//duan
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
    if(ds_stop(dsg->ds) || (dsg->kill == 1)) {
    	return 1;
    }
    else
    	return 0;
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
