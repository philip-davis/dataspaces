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

#include "config.h"
#include "dimes_interface.h"
#include "dimes_client.h"
#include "dimes_data.h"
/* Name mangling for C functions to adapt Fortran compiler */
#define FC_FUNC(name,NAME) name ## _

static struct dimes_client *dimes_c = NULL;
static enum storage_type st = column_major;
static int num_dims = 2;
#ifdef TIMING_PERF
static char log_header[256] = "";
static struct timer tm_perf;
#endif

static int init_sspace_dimes(struct dimes_client *d)
{
    int max_versions = 1;
    d->ssd = ssd_alloc(&d->domain, d->dcg->ss_info.num_space_srv, max_versions);
    if (!d->ssd) {
        uloga("%s(): ssd_alloc failed!\n",__func__);
        return -1;
    }

    INIT_LIST_HEAD(&d->sspace_list);
    return 0;
}

static int free_sspace_dimes(struct dimes_client *d)
{
    ssd_free(d->ssd);
    struct sspace_list_entry *ssd_entry, *temp;
    list_for_each_entry_safe(ssd_entry, temp, &d->sspace_list,
            struct sspace_list_entry, entry)
    {
        ssd_free(ssd_entry->ssd);
        list_del(&ssd_entry->entry);
        free(ssd_entry);
    }

    return 0;
}

static struct sspace* lookup_sspace_dimes(struct dimes_client *d, const char* var_name,
    const struct global_dimension* gd)
{
    struct global_dimension gdim;
    memcpy(&gdim, gd, sizeof(struct global_dimension));

    // Return the default shared space created based on
    // global data domain specified in dataspaces.conf 
    if (global_dimension_equal(&gdim, &d->dcg->default_gdim)) {
        return d->ssd;
    }

    // TODO: hard coding for 'dpot' variable in EPSI application
    int pos;
    size_t len = strlen(var_name);
    if (len >= strlen("dpot")) {
        pos = len - strlen("dpot");
        if (0 == strcmp(var_name+pos, "dpot")) {
            gdim.sizes.c[1] = 1;
        }
    }

    // Otherwise, search for shared space based on the
    // global data domain specified by application in put()/get().
    struct sspace_list_entry *ssd_entry = NULL;
    list_for_each_entry(ssd_entry, &d->sspace_list,
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
    int max_versions = 1;
    ssd_entry->ssd = ssd_alloc(&domain, d->dcg->ss_info.num_space_srv, max_versions);
    if (!ssd_entry->ssd) {
        uloga("%s(): ssd_alloc failed\n", __func__);
        return d->ssd;
    }

    //uloga("%s(): add new shared space ndim= %d global dimension= %llu %llu %llu\n",
    // __func__, gdim.ndim, gdim.sizes.c[0], gdim.sizes.c[1], gdim.sizes.c[2]);

    list_add(&ssd_entry->entry, &d->sspace_list);
    return ssd_entry->ssd;
}


struct dimes_client_option {
    int enable_dimes_ack;
    int enable_pre_allocated_rdma_buffer;
    size_t pre_allocated_rdma_buffer_size;
    struct dart_rdma_mem_handle pre_allocated_rdma_handle;
    size_t rdma_buffer_size;
    size_t rdma_buffer_write_usage;
    size_t rdma_buffer_read_usage;
    int max_num_concurrent_rdma_read_op;
};
static struct dimes_client_option options;

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
#define DIMES_RANK DIMES_CID - dimes_c->dcg->dc->cp_min_rank
#define DC dimes_c->dcg->dc
#define RPC_S dimes_c->dcg->dc->rpc_s 
#define NUM_SP dimes_c->dcg->dc->num_sp

static int is_peer_on_same_core(struct node_id *peer)
{
	return (peer->ptlmap.id == DIMES_CID);
}

static size_t get_available_rdma_buffer_size()
{
    size_t total_usage = options.rdma_buffer_write_usage +
                         options.rdma_buffer_read_usage;
    if (total_usage > options.rdma_buffer_size) {
        return 0;
    } else {
        return options.rdma_buffer_size-total_usage;
    }
}

static void print_rdma_buffer_usage()
{
    uloga("%s():    rdma_buffer_size= %u bytes\n"
          "         rdma_buffer_write_usage= %u bytes\n"
          "         rdma_buffer_read_usage= %u bytes\n"
          "         rdma_buffer_avail_size= %u bytes\n"
          "         max_num_concurrent_rdma_read= %d\n",
        __func__, options.rdma_buffer_size, options.rdma_buffer_write_usage,
        options.rdma_buffer_read_usage, get_available_rdma_buffer_size(),
        options.max_num_concurrent_rdma_read_op);
}


#define NUM_SYNC_ID 4096
static struct {
	int next;
	int sync_ids[NUM_SYNC_ID];
} sync_op_data;

static void syncop_init(void)
{
    int i;
    for (i = 0; i < NUM_SYNC_ID; i++)
        sync_op_data.sync_ids[i] = 1;
    sync_op_data.next = 0;
}

static int syncop_next_sync_id(void)
{
	int n;
	n = sync_op_data.next;
	// NOTE: this is tricky and error prone; implement a better sync opertation
	if (sync_op_data.sync_ids[n] != 1) {
		uloga("'%s()': error sync operation overflows.\n", __func__);
	}
	sync_op_data.sync_ids[n] = 0;
	sync_op_data.next = (sync_op_data.next + 1) % NUM_SYNC_ID;

	return n;
}

static int syncop_status(int sid)
{
    return sync_op_data.sync_ids[sid];
}

static void syncop_set_done(int sid)
{   
    sync_op_data.sync_ids[sid] = 1;
}

enum dimes_put_status {
	DIMES_PUT_OK = 0,
	DIMES_PUT_PENDING = 1,
};

enum dimes_ack_type {
    dimes_ack_type_msg = 0,
    dimes_ack_type_rdma,
};

struct dimes_memory_obj {
	struct list_head entry;
	int sync_id;
    enum dimes_ack_type ack_type;
    struct obj_descriptor obj_desc;
    struct global_dimension gdim;
    struct dart_rdma_mem_handle rdma_handle;
};

struct dimes_storage_group {
	struct list_head entry;
	char *name;
	// List of dimes_memory_obj
	struct list_head mem_obj_list;
};

//static struct list_head mem_obj_list;
static char *current_group_name = NULL;
const char* default_group_name = "__default_storage_group__";
static struct list_head storage;

static int mem_obj_list_add(struct list_head *l, struct dimes_memory_obj *p) {
	list_add(&p->entry, l);
	return 0;
}

static int mem_obj_list_delete(struct list_head *l, int sid) {
	struct dimes_memory_obj *p, *t;
	list_for_each_entry_safe(p, t, l, struct dimes_memory_obj, entry) {
		if (p->sync_id == sid) {
			list_del(&p->entry);
			free(p);
			return 0;
		}
	}

	return -1;
}

static struct dimes_memory_obj* mem_obj_list_lookup(struct list_head *l, int sid) {
	struct dimes_memory_obj *p;
	list_for_each_entry(p, l, struct dimes_memory_obj, entry) {
		if (p->sync_id == sid)
			return p;
	}

	return NULL;
} 

static struct dimes_storage_group* storage_add_group(const char *group_name)
{
	struct dimes_storage_group *p = malloc(sizeof(*p));
	p->name = (char *)malloc(strlen(group_name)+1);
	strcpy(p->name, group_name);
	INIT_LIST_HEAD(&p->mem_obj_list);

	list_add(&p->entry, &storage);	
	return p;
}

static void storage_init()
{
	INIT_LIST_HEAD(&storage);

    // Add the default storage group
	struct dimes_storage_group *p =
			storage_add_group(default_group_name);

	// Set current group as default
	current_group_name = p->name;	
} 

static void storage_free()
{
	struct dimes_storage_group *p, *t;
	list_for_each_entry_safe(p, t, &storage, struct dimes_storage_group, entry) 
	{
		list_del(&p->entry);
		if (p->name != NULL) free(p->name);
		free(p);
	}

	current_group_name = NULL;
}

static struct dimes_storage_group* storage_lookup_group(const char *group_name)
{
	struct dimes_storage_group *p;
	list_for_each_entry(p, &storage, struct dimes_storage_group, entry)
	{
		if (p->name == NULL) continue;
		if (0 == strcmp(p->name, group_name)) {
			return p;
		} 
	}

	return NULL;
} 

static int storage_add_obj(struct dimes_memory_obj *mem_obj)
{
	struct dimes_storage_group *p = storage_lookup_group(current_group_name);
	if (p == NULL) {
		return -1;
	}

	return mem_obj_list_add(&p->mem_obj_list, mem_obj);
}

static int storage_delete_obj(int sid)
{
	struct dimes_storage_group *p;
	list_for_each_entry(p, &storage, struct dimes_storage_group, entry)
	{
		if(0 == mem_obj_list_delete(&p->mem_obj_list, sid)) {
			return 0;
		}
	}	

	return -1;
}

static struct dimes_memory_obj* storage_lookup_obj(int sid)
{
	struct dimes_storage_group *p;
	list_for_each_entry(p, &storage, struct dimes_storage_group, entry)
	{
		struct dimes_memory_obj *mem_obj;
		mem_obj = mem_obj_list_lookup(&p->mem_obj_list, sid);
		if (mem_obj != NULL) return mem_obj;
	}	

	return NULL;
}



/**************************************************
  Data structures & functions for DIMES transaction
***************************************************/
enum dimes_memory_type {
    dimes_memory_non_rdma = 0,
    dimes_memory_rdma
};

enum fetch_status {
    fetch_ready = 0,
    fetch_posted,
    fetch_done
};

enum fetch_dst_memory_type {
    fetch_dst_memory_non_rdma = 0,
    fetch_dst_memory_rdma
};

static int dimes_memory_init()
{
    void *data_buf = NULL;
    int err;

    if (options.enable_pre_allocated_rdma_buffer) {
        data_buf = malloc(options.pre_allocated_rdma_buffer_size);
        if (!data_buf) goto err_out;
        memset(data_buf, 0, options.pre_allocated_rdma_buffer_size);

        err = dart_rdma_register_mem(&options.pre_allocated_rdma_handle,
                     data_buf, options.pre_allocated_rdma_buffer_size);
        if (err < 0) {
            uloga("%s(): #%d failed to register memory buffer\n",
                    __func__, DIMES_CID);
            goto err_out_free;
        }

        dimes_buffer_init(options.pre_allocated_rdma_handle.base_addr,
                          options.pre_allocated_rdma_buffer_size);
        options.rdma_buffer_size = options.pre_allocated_rdma_buffer_size;
    }

    return 0;
 err_out_free:
    free(data_buf);
 err_out:
    options.enable_pre_allocated_rdma_buffer = 0;
    return -1;
}

static int dimes_memory_finalize()
{
    int err;
    if (options.enable_pre_allocated_rdma_buffer) {
        // Deregister the memory buffer
        err = dart_rdma_deregister_mem(&options.pre_allocated_rdma_handle);
        if (err < 0) {
            uloga("%s(): dart_rdma_deregister_mem() failed\n", __func__);
        }

        dimes_buffer_finalize();
        free(options.pre_allocated_rdma_handle.base_addr);
    }

    return 0;
}

static int dimes_memory_alloc(struct dart_rdma_mem_handle *rdma_hndl, size_t size, enum dimes_memory_type type)
{
    int err;
    uint64_t buf;

    switch (type) {
    case dimes_memory_non_rdma:
        buf = (uint64_t)malloc(size);
        if (!buf) {
            return -1;
        }

        rdma_hndl->base_addr = buf;
        rdma_hndl->size = size;
        break;
    case dimes_memory_rdma:
        if (options.enable_pre_allocated_rdma_buffer) {
            dimes_buffer_alloc(size, &buf);
            if (!buf) {
                uloga("%s(): dimes_buffer_alloc() failed size %u bytes\n",
                      __func__, size);
                return -1;
            }

            // TODO: more clean way to do this?
            // Copy the rdma memory handle and update the base_addr, size
            memcpy(rdma_hndl, &options.pre_allocated_rdma_handle,
                   sizeof(struct dart_rdma_mem_handle));
            rdma_hndl->base_addr = buf;
            rdma_hndl->size = size;
        } else {
            buf = (uint64_t)malloc(size);
            if (!buf) {
                return -1;
            }

            err = dart_rdma_register_mem(rdma_hndl, buf, size);
            if (err < 0) {
                uloga("%s(): dart_rdma_register_mem() failed\n", __func__);
                return -1;
            }
        }
        break;
    default:
        uloga("%s(): unknow dimes_memory_type %d\n", __func__, type);
        break;
    }

    return 0;
}

static int dimes_memory_free(struct dart_rdma_mem_handle *rdma_hndl, enum dimes_memory_type type)
{
    int err;

    switch (type) {
    case dimes_memory_non_rdma:
        free(rdma_hndl->base_addr);
        break;
    case dimes_memory_rdma:
        if (options.enable_pre_allocated_rdma_buffer) {
            dimes_buffer_free(rdma_hndl->base_addr);
        } else {
            err = dart_rdma_deregister_mem(rdma_hndl);
            if (err < 0) {
                uloga("%s(): dart_rdma_deregister_mem failed\n", __func__);
                return -1;
            }

            free(rdma_hndl->base_addr);
        }
        break;
    default:
        uloga("%s(): unknow dimes_memory_type %d\n", __func__, type);
        break;
    }

    return 0;
}

struct fetch_entry {
    struct list_head entry;
    int remote_sync_id;
    struct obj_descriptor src_odsc;
    struct obj_descriptor dst_odsc;
    // TODO: can we use array of read_tran pointers?
    struct dart_rdma_tran *read_tran;
};

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

    struct obj_data         *data_ref;
	int                     q_id;
	struct obj_descriptor   q_obj;

    int                     num_fetch;
	struct list_head        fetch_list;

	struct query_dht_d        *qh;

    unsigned int    f_dht_peer_recv:1,
					f_locate_data_complete:1,
					f_complete:1;
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
qte_alloc_d(struct obj_data *od)
{
	struct query_tran_entry_d *qte;

	qte = malloc(sizeof(*qte));
	if (!qte) {
		errno = ENOMEM;
		return NULL;
	}
	memset(qte, 0, sizeof(*qte));

	INIT_LIST_HEAD(&qte->fetch_list);
    qte->data_ref = od;
	qte->q_id = qt_gen_qid_d();
	qte->q_obj = od->obj_desc;
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
    struct fetch_entry *fetch;

    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        if (obj_desc_equals(&fetch->dst_odsc, odsc)) {
            return &fetch->dst_odsc;
        }
    }

	return NULL;
}

/*
    Release memory resources for fetch_list 
*/
static void qt_free_obj_data_d(struct query_tran_entry_d *qte)
{
    struct fetch_entry *fetch, *t;
    list_for_each_entry_safe(fetch,t,&qte->fetch_list,struct fetch_entry,entry)
    {
        dart_rdma_delete_read_tran(fetch->read_tran->tran_id);
        list_del(&fetch->entry);
        free(fetch);
        qte->num_fetch--;       
    }
}

static int qt_add_obj_with_cmd_d(struct query_tran_entry_d *qte,
                struct obj_descriptor *odsc, struct rpc_cmd *cmd)
{
    struct hdr_dimes_put *hdr;
    struct node_id *peer;
    struct fetch_entry *fetch;
    fetch = (struct fetch_entry*)malloc(sizeof(*fetch));

    // Lookup the owner peer of the remote data object
    peer = dc_get_peer(DC, odsc->owner);

    // Creat read transaction
    dart_rdma_create_read_tran(peer, &fetch->read_tran);

    // Set source and destination object descriptors
    hdr = (struct hdr_dimes_put*)cmd->pad;
    fetch->remote_sync_id = hdr->sync_id;
    fetch->src_odsc = hdr->odsc;
    fetch->dst_odsc = *odsc;

    // Set source rdma memory handle
    dart_rdma_get_memregion_from_cmd(&fetch->read_tran->src, cmd);

    list_add(&fetch->entry, &qte->fetch_list);

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

static int obj_assemble(struct fetch_entry *fetch, struct obj_data *od)
{
    int err;
    struct obj_data *from = obj_data_alloc_no_data(&fetch->dst_odsc,
                                  fetch->read_tran->dst.base_addr);
    err = ssd_copy(od, from);
    if (err == 0) {
        obj_data_free(from);
        return 0;
    }

    uloga("%s(): failed with %d\n", __func__, err);
    obj_data_free(from);
    return err;
}

static int locate_data_completion_client(struct rpc_server *rpc_s,
					 struct msg_buf *msg)
{
	struct hdr_dimes_get *oh = msg->private;
	struct rpc_cmd *tab = msg->msg_data;
	int i, err = -ENOENT;

	struct query_tran_entry_d *qte = qt_find_d(&dimes_c->qt, oh->qid);
	if (!qte) {
		uloga("%s(): can not find transaction qid= %d\n",
			__func__, oh->qid);
		goto err_out_free;
	}

	// Add received rpc_cmd information.
	qte->qh->qh_num_req_received++;
	qte->num_fetch += oh->num_obj;
	for (i = 0; i < oh->num_obj; i++) {
        struct hdr_dimes_put *hdr = (struct hdr_dimes_put*)tab[i].pad;
		struct obj_descriptor odsc = hdr->odsc;
		// Calculate the intersection of the bbox (specified by the 
		// receiver process) and the bbox (returned by the server).
		bbox_intersect(&qte->q_obj.bb, &hdr->odsc.bb, &odsc.bb);	
		if (!qt_find_obj_d(qte, &odsc)) {
            err = qt_add_obj_with_cmd_d(qte, &odsc, &tab[i]);
            if (err < 0)
                goto err_out_free;
		} else {
//#ifdef DEBUG
//			uloga("%s(): duplicate obj descriptor detected.\n",
//				__func__);
//#endif
			qte->num_fetch--;
		}
	}

	free(oh);
	free(tab);
	free(msg);

	if (qte->qh->qh_num_req_received == qte->qh->qh_num_req_posted) {
		qte->f_locate_data_complete = 1;
	}

	return 0;
err_out_free:
	free(oh);
	free(tab);
	free(msg);
	ERROR_TRACE();
}

static int dcgrpc_dimes_locate_data(struct rpc_server *rpc_s,
				    struct rpc_cmd *cmd)
{
	struct hdr_dimes_get *oht, 
			 *oh = (struct hdr_dimes_get *) cmd->pad;
	struct node_id *peer = dc_get_peer(DC, cmd->id);
	struct rpc_cmd *tab;
	struct msg_buf *msg;
	int err = -ENOMEM;

#ifdef DEBUG
	uloga("%s(): #%d oh->qid=%d, oh->rc=%d, oh->num_obj=%d\n", __func__,
		DIMES_CID, oh->qid, oh->rc, oh->num_obj);
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

	tab = malloc(sizeof(struct rpc_cmd) * oh->num_obj);
	if (!tab)
		goto err_out;

	oht = malloc(sizeof(*oh));
	if (!oht) {
		free(tab);
		goto err_out;
	}
	memcpy(oht, oh, sizeof(*oh));

	msg = msg_buf_alloc(rpc_s, peer, 0);
	if (!msg) {
		free(tab);
		goto err_out;
	}

	msg->size = sizeof(struct rpc_cmd) * oh->num_obj;
	msg->msg_data = tab;
	msg->cb = locate_data_completion_client;
	msg->private = oht;

	if (msg->size <= 0) {
		free(tab);
		free(msg);
		goto err_out;
	}	

	rpc_mem_info_cache(peer, msg, cmd);
	err = rpc_receive_direct(rpc_s, peer, msg);
    rpc_mem_info_reset(peer, msg, cmd);
	if (err == 0)
		return 0;

	free(tab);
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
    int i;
    for(i = 0; i < hsi->num_dims; i++){
        dimes_c->domain.lb.c[i] = 0;
        dimes_c->domain.ub.c[i] = hsi->dims.c[i]-1;
    }

	dimes_c->f_ss_info = 1;

	return 0;
}

static int dimes_ss_info(int *ndim)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (dimes_c->f_ss_info) {
		*ndim = dimes_c->domain.num_dims;
		return 0;
	}

    if (dimes_c->dcg->f_ss_info) {
        struct bbox *bb = &(dimes_c->dcg->ss_domain);
        dimes_c->domain.num_dims = bb->num_dims;
        int i;
        for (i = 0; i < bb->num_dims; i++) {
            dimes_c->domain.lb.c[i] = bb->lb.c[i];
            dimes_c->domain.ub.c[i] = bb->ub.c[i];
        }
        dimes_c->f_ss_info = 1;
   } else {
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
    }
	
	*ndim = dimes_c->dcg->ss_info.num_dims;
    err = init_sspace_dimes(dimes_c);
    if (err < 0) {
        goto err_out;
    }

	return 0;
err_out:
	ERROR_TRACE();
}

static int dimes_memory_obj_status(struct dimes_memory_obj *mem_obj)
{
	int ret;

/*
    if (mem_obj->ack_type == dimes_ack_type_rdma) {
        // Check the pad
        size_t data_size = obj_data_size(&mem_obj->obj_desc);
        size_t pad_size = 64; // bytes
        char *data = mem_obj->rdma_handle.base_addr+data_size;
        
        // TODO: is this safe to read/check the pad?
        int i;
        int flag = 1;
        for (i = 0; i < pad_size; i++) {
            if (data[i] != 1) {
                flag = 0;
                break;
            }
        }
        if (flag) {
            uloga("%s(): pad is set by remote peer!\n", __func__);
            dimes_memory_free(&mem_obj->rdma_handle, dimes_memory_rdma);
            syncop_set_done(mem_obj->sync_id);
        }
    }
*/
    if (syncop_status(mem_obj->sync_id) == 1) {	
		ret = DIMES_PUT_OK;
	} else {
		ret = DIMES_PUT_PENDING;
	}

	return ret;
}

static int dimes_obj_put(struct dimes_memory_obj *mem_obj)
{
	struct dht_entry *dht_nodes[dimes_c->dcg->ss_info.num_space_srv];
	struct hdr_dimes_put *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int i, num_dht_nodes;
	int err = -ENOMEM;

	// Update the DHT nodes
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, mem_obj->obj_desc.name,
                                &mem_obj->gdim);
	num_dht_nodes = ssd_hash(ssd, &mem_obj->obj_desc.bb, dht_nodes);
	if (num_dht_nodes <= 0) {
		uloga("%s(): error! ssd_hash() return %d\n",
				__func__, num_dht_nodes);
		goto err_out;
	}
	
	for (i = 0; i < num_dht_nodes; i++) {
		// TODO(fan): There is assumption here that the space servers
		// rank range from 0~(num_space_srv-1).
		peer = dc_get_peer(DC, dht_nodes[i]->rank);
		msg = msg_buf_alloc(RPC_S, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_put_msg;
		msg->msg_rpc->id = DIMES_CID;
		dart_rdma_set_memregion_to_cmd(&mem_obj->rdma_handle, msg->msg_rpc);	

		hdr = (struct hdr_dimes_put *)msg->msg_rpc->pad;
		hdr->odsc = mem_obj->obj_desc;
		hdr->sync_id = mem_obj->sync_id;
		hdr->has_rdma_data = 1;
	
		err = rpc_send(RPC_S, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

	storage_add_obj(mem_obj);
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

struct matrix_view_d {
	__u64 lb[BBOX_MAX_NDIM]; //int lb[3];
	__u64 ub[BBOX_MAX_NDIM]; //int ub[3];
};

struct matrix_d {
	//int dimx, dimy, dimz;
	int 	dist[BBOX_MAX_NDIM];
	int 	num_dims;
	size_t size_elem;
	enum storage_type mat_storage;
	struct matrix_view_d mat_view;
};

static void matrix_init_d(struct matrix_d *mat, enum storage_type st,
                        struct bbox *bb_glb, struct bbox *bb_loc, size_t se)
{
	int i;
	int ndims = bb_glb->num_dims;

	memset(mat, 0, sizeof(struct matrix_d));

    for(i = 0; i < ndims; i++){
        mat->dist[i] = bbox_dist(bb_glb, i);
        mat->mat_view.lb[i] = bb_loc->lb.c[i] - bb_glb->lb.c[i];
        mat->mat_view.ub[i] = bb_loc->ub.c[i] - bb_glb->lb.c[i];
    }

    	mat->num_dims = ndims;
    	mat->mat_storage = st;
    	mat->size_elem = se;
}

static int matrix_rdma_copy(struct matrix_d *a, struct matrix_d *b, int tran_id)
{
	__u64 src_offset = 0;
	__u64 dst_offset = 0;
	__u64 bytes = 0;
	int err = -ENOMEM;

	__u64 a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
    __u64 aloc=0, aloc1=0, aloc2=0, aloc3=0, aloc4=0, aloc5=0, aloc6=0, aloc7=0, aloc8=0, aloc9=0;
    __u64 b0, b1, b2, b3, b4, b5, b6, b7, b8, b9;
    __u64 bloc=0, bloc1=0, bloc2=0, bloc3=0, bloc4=0, bloc5=0, bloc6=0, bloc7=0, bloc8=0, bloc9=0;

	__u64 n = 0;
	n = a->mat_view.ub[0] - a->mat_view.lb[0] + 1;
	a0 = a->mat_view.lb[0];
    b0 = b->mat_view.lb[0];

    /*for(a9 = a->mat_view.lb[9], b9 = b->mat_view.lb[9];     //TODO-Q
        a9 <= a->mat_view.ub[9]; a9++, b9++){
        aloc9 = a9 * a->dist[8];
        bloc9 = a9 * b->dist[8];
    for(a8 = a->mat_view.lb[8], b8 = b->mat_view.lb[8];     //TODO-Q
        a8 <= a->mat_view.ub[8]; a8++, b8++){
        aloc8 = (aloc9 + a8) * a->dist[7];
        bloc8 = (bloc9 + b8) * b->dist[7];
    for(a7 = a->mat_view.lb[7], b7 = b->mat_view.lb[7];     //TODO-Q
        a7 <= a->mat_view.ub[7]; a7++, b7++){
        aloc7 = (aloc8 + a7) * a->dist[6];
        bloc7 = (bloc8 + b7) * b->dist[6];
    for(a6 = a->mat_view.lb[6], b6 = b->mat_view.lb[6];     //TODO-Q
        a6 <= a->mat_view.ub[6]; a6++, b6++){
        aloc6 = (aloc7 + a6) * a->dist[5];
        bloc6 = (bloc7 + b6) * b->dist[5];
    for(a5 = a->mat_view.lb[5], b5 = b->mat_view.lb[5];     //TODO-Q
        a5 <= a->mat_view.ub[5]; a5++, b5++){
        aloc5 = (aloc6 + a5) * a->dist[4];
        bloc5 = (bloc6 + b5) * b->dist[4];
    for(a4 = a->mat_view.lb[4], b4 = b->mat_view.lb[4];
        a4 <= a->mat_view.ub[4]; a4++, b4++){
        aloc4 = (aloc5 + a4) * a->dist[3];
        bloc4 = (bloc5 + b4) * b->dist[3];
    for(a3 = a->mat_view.lb[3], b3 = b->mat_view.lb[3];
        a3 <= a->mat_view.ub[3]; a3++, b3++){
        aloc3 = (aloc4 + a3) * a->dist[2];
        bloc3 = (bloc4 + b3) * b->dist[2]; */
        for(a2 = a->mat_view.lb[2], b2 = b->mat_view.lb[2];
            a2 <= a->mat_view.ub[2]; a2++, b2++){
            aloc2 = (aloc3 + a2) * a->dist[1];
            bloc2 = (bloc3 + b2) * b->dist[1];
            for(a1 = a->mat_view.lb[1], b1 = b->mat_view.lb[1];
                a1 <= a->mat_view.ub[1]; a1++, b1++){
                aloc1 = (aloc2 + a1) * a->dist[0];
                bloc1 = (bloc2 + b1) * b->dist[0];
               /* for(a0 = a->mat_view.lb[0], b0 = b->mat_view.lb[0];
                    a0 <= a->mat_view.ub[0]; a0++, b0++){
                    aloc = aloc1 + a0;
                    bloc = bloc1 + b0;
               */
                aloc = aloc1 + a0;
                bloc = bloc1 + b0;
                src_offset = bloc * a->size_elem;
                dst_offset = aloc * a->size_elem;
                bytes = n * a->size_elem;
                err = dart_rdma_schedule_read(tran_id, src_offset, dst_offset, bytes);
                if (err < 0)
                        goto err_out;
	}} /*}}}}}}}*/

    return 0;
err_out:
    ERROR_TRACE();
}

/*
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
				#ifdef DEBUG
				uloga("%s(): tran_id=%d, bk=%d, bj=%d, bi=%d, "
				  "src_offset=%u, ak=%d, aj=%d, ai=%d, "
				  "dst_offset=%u, bytes=%u\n",
					__func__, tran_id, bk, bj, bi, src_offset,
					ak, aj, ai, dst_offset, bytes);
				#endif

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
*/

static int schedule_rdma_reads(int tran_id,
        struct obj_descriptor *src_odsc, struct obj_descriptor *dst_odsc)
{
	int err = -ENOMEM;
	size_t src_offset = 0;
	size_t dst_offset = 0;
	size_t bytes = 0;

	if (obj_desc_equals_no_owner(src_odsc, dst_odsc)) {
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

static int get_num_posted_fetch(struct fetch_entry **fetch_tab, int *fetch_status_tab, int fetch_tab_size)
{
    int i;
    int num_posted_fetch = 0;

    // TODO: what if fetch_tab_size is very large, is this efficient?
    for (i = 0; i < fetch_tab_size; i++) {
        if (fetch_status_tab[i] == fetch_posted) {
            num_posted_fetch++;
        }
    }

    return num_posted_fetch;
}

static int all_fetch_done(struct query_tran_entry_d *qte, struct fetch_entry **fetch_tab, int *fetch_status_tab, int fetch_tab_size, int *all_done)
{
    *all_done = 0;
    int err, i;
    int complete_one_fetch = 0;
    int num_posted_fetch = get_num_posted_fetch(fetch_tab, fetch_status_tab,
                                            fetch_tab_size);

    while (num_posted_fetch > 0 && !complete_one_fetch) {
        err = dart_rdma_process_reads();
        if (err < 0) {
            goto err_out;
        }

        for (i = 0; i < fetch_tab_size; i++) {
            if (fetch_status_tab[i] == fetch_posted &&
                dart_rdma_check_reads(fetch_tab[i]->read_tran->tran_id)) {
                // Copy fetched data
                obj_assemble(fetch_tab[i], qte->data_ref); 
                // Free recv buffer
                err = dimes_memory_free(&fetch_tab[i]->read_tran->dst,
                                        dimes_memory_rdma); 
                if (err < 0) {
                    goto err_out;
                }

                // Update rdma buffer usage
                options.rdma_buffer_read_usage -= 
                            obj_data_size(&fetch_tab[i]->dst_odsc);
                fetch_status_tab[i] = fetch_done;
                complete_one_fetch = 1;
            } 
        }
    }

    for (i = 0; i < fetch_tab_size; i++) {
        if (fetch_status_tab[i] != fetch_done) {
            return 0;
        }
    }

    *all_done = 1;
    return 0;
 err_out:
    ERROR_TRACE();
}

static int get_next_fetch(struct fetch_entry **fetch_tab, int *fetch_status_tab, int fetch_tab_size, int *index)
{
    *index = -1;
    int err, i;
    int num_posted_fetch = get_num_posted_fetch(fetch_tab, fetch_status_tab,
                                                fetch_tab_size);
    if (num_posted_fetch >= options.max_num_concurrent_rdma_read_op) {
        return 0;
    }

    for (i = 0; i < fetch_tab_size; i++) {
        size_t read_size = obj_data_size(&fetch_tab[i]->dst_odsc);
        if (fetch_status_tab[i] == fetch_ready &&
            read_size <= get_available_rdma_buffer_size()) {
            err = dimes_memory_alloc(&fetch_tab[i]->read_tran->dst,
                                     read_size, dimes_memory_rdma);
            if (err < 0) {
                uloga("%s(): dimes_memory_alloc() failed\n", __func__);
                goto err_out;
            }
            // Update rdma buffer usage
            options.rdma_buffer_read_usage += read_size;
            *index = i;
            return 0;
        }
    }

    return 0;
 err_out:
    ERROR_TRACE();
}

/*
static int calculate_num_fetch(size_t bytes)
{
    if (!enable_customized_rdma_read_block) {
        return 1;
    }

    int num_fetch = bytes / customized_rdma_read_block_size;
    if ((bytes % customized_rdma_read_block_size) != 0) {
        num_fetch += 1;
    }

    return num_fetch;
}
*/

static int estimate_fetch_tab_capacity(struct query_tran_entry_d *qte)
{
    return qte->num_fetch;

/*
    struct fetch_entry *fetch;
    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        if (!is_peer_on_same_core(fetch->read_tran->remote_peer)) {
            // Data resides on remote core
            size_t read_size = obj_data_size(&fetch->dst_odsc);
            capacity += calculate_num_fetch(read_size);
        }
    }

    if (capacity < qte->num_fetch) {
        capacity = qte->num_fetch;
    }

    return capacity;
*/
}

static int dimes_get_ack_by_msg(struct query_tran_entry_d *qte)
{
    int err;
    struct fetch_entry *fetch;
    struct msg_buf *msg;
    struct node_id *peer;
    struct hdr_dimes_get_ack *oh;

    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        // Ack. indirectly through server
        peer = dc_get_peer(DC, fetch->src_odsc.owner % NUM_SP);
        msg = msg_buf_alloc(RPC_S, peer, 1);
        msg->msg_rpc->cmd = dimes_get_ack_msg;
        msg->msg_rpc->id = DIMES_CID;

        oh = (struct hdr_dimes_get_ack *)msg->msg_rpc->pad;
        oh->qid = qte->q_id;
        oh->sync_id = fetch->remote_sync_id;
        oh->odsc = fetch->src_odsc;
        oh->bytes_read = obj_data_size(&fetch->dst_odsc);
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

/*
static int dimes_get_ack_by_rdma(struct query_tran_entry_d *qte)
{
    int err;
    struct fetch_entry *fetch;
    struct dart_rdma_tran *tran;
    size_t pad_size = 64;

    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        err = dart_rdma_create_write_tran(fetch->read_tran->remote_peer, &tran);
        if (err < 0) {
            goto err_out;
        }

        err = dimes_memory_alloc(&tran->src, pad_size, dimes_memory_rdma);
        if (err < 0) {
            goto err_out_delete;
        }

        // Set ack data
        memset(tran->src.base_addr, 1, pad_size);
        // Set dst mem region
        tran->dst = fetch->read_tran->src; // TODO: tricky?
        // Schedule rdma write op
        size_t src_offset = 0;
        size_t dst_offset = obj_data_size(&fetch->src_odsc); // TODO: tricky?
        dart_rdma_schedule_write(tran->tran_id, src_offset, dst_offset,
                                 pad_size);
        err = dart_rdma_perform_writes(tran->tran_id);
        if (err < 0) {
            goto err_out_free;
        }

        while (!dart_rdma_check_writes(tran->tran_id)) {
            err = dart_rdma_process_writes();
            if (err < 0) {
                goto err_out_free;
            }
        }

        dimes_memory_free(&tran->src, dimes_memory_rdma);
        dart_rdma_delete_write_tran(tran->tran_id);
    }

    return 0;
 err_out_free:
    dimes_memory_free(&tran->src, dimes_memory_rdma);
 err_out_delete:
    dart_rdma_delete_write_tran(tran->tran_id);
 err_out:
    ERROR_TRACE();
}
*/

static int dimes_fetch_data(struct query_tran_entry_d *qte)
{
    struct fetch_entry *fetch;
	int i = 0, err;
	qte->f_complete = 0;

	// Allocate the array for fetch operations 
	struct fetch_entry **fetch_tab = NULL;
    int *fetch_status_tab = NULL;
    int fetch_tab_capacity = estimate_fetch_tab_capacity(qte);
    int fetch_tab_size = 0;
	fetch_tab = (struct fetch_entry **)
			malloc(sizeof(struct fetch_entry*) * fetch_tab_capacity);
    fetch_status_tab = (int *)malloc(sizeof(int) * fetch_tab_capacity);
    for (i = 0; i < fetch_tab_capacity; i++) {
        fetch_tab[i] = NULL;
        fetch_status_tab[i] = fetch_ready;
    }

    // Copy fetch entry (if any) that data object resides on remote cores
    // and does NOT require sub-array reading 
    i = 0;
    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        if (!is_peer_on_same_core(fetch->read_tran->remote_peer) &&
            obj_desc_equals_no_owner(&fetch->src_odsc, &fetch->dst_odsc))
        {
            fetch_tab[i] = fetch;

            size_t data_size, src_offset, dst_offset;
            data_size = obj_data_size(&fetch->dst_odsc);
            src_offset = 0;
            dst_offset = 0;
            // TODO: why this can not be moved to the loop below?
            dart_rdma_schedule_read(fetch_tab[i]->read_tran->tran_id,
                                  src_offset,
                                  dst_offset,
                                  data_size);
            i++; // Count number of fetch entry in the table
        }
    }
    fetch_tab_size = i;

    int loop_done = 0;
    // Perform RDMA read operations
    do {
        // Issue as much as RDMA read opertions as possible
        do { 
            err = get_next_fetch(fetch_tab, fetch_status_tab,
                                 fetch_tab_size, &i);
            if (err < 0) {
                goto err_out_free;
            }
            if (i < 0 || i >= fetch_tab_size) break; // break inner loop

            err = dart_rdma_perform_reads(fetch_tab[i]->read_tran->tran_id);
            fetch_status_tab[i] = fetch_posted;
            if (err < 0) {
                goto err_out_free;
            }
        } while (1); // TODO: fix magic number

        err = all_fetch_done(qte, fetch_tab, fetch_status_tab,
                             fetch_tab_size, &loop_done);
        if (err < 0) {
            goto err_out_free;
        }
    } while (!loop_done);

	if (fetch_tab) {
		free(fetch_tab);
	}
    if (fetch_status_tab) {
        free(fetch_status_tab);
    }

    // Read data object (if any) that (1) resides on remote cores BUT 
    // require sub-array reading, OR (2) resides on local core
    list_for_each_entry(fetch, &qte->fetch_list, struct fetch_entry, entry)
    {
        if (is_peer_on_same_core(fetch->read_tran->remote_peer)) {
            // Data on local peer (itself), fetch directly
            struct dimes_memory_obj *mem_obj =
                                    storage_lookup_obj(fetch->remote_sync_id);
            if (mem_obj == NULL) {
                uloga("%s(): failed to find dimes memory object sync_id=%d\n",
                    __func__, fetch->remote_sync_id);
                goto err_out;
            }

            // Update source memory region
            fetch->read_tran->src.base_addr = mem_obj->rdma_handle.base_addr;
            fetch->read_tran->src.size = mem_obj->rdma_handle.size;

            // Alloc receive buffer, schedle reads, perform reads
            dimes_memory_alloc(&fetch->read_tran->dst,
                               obj_data_size(&fetch->dst_odsc),
                               dimes_memory_non_rdma);
            schedule_rdma_reads(fetch->read_tran->tran_id,
                                &fetch->src_odsc, &fetch->dst_odsc);
            dart_rdma_perform_reads(fetch->read_tran->tran_id);
            // Copy fetched data
            obj_assemble(fetch, qte->data_ref);
            dimes_memory_free(&fetch->read_tran->dst, dimes_memory_non_rdma);
        } else if (!obj_desc_equals_no_owner(&fetch->src_odsc,
                                             &fetch->dst_odsc))
        {
            // Data on remote peer
            // Alloc receive buffer, schedle reads, perform reads
            err = dimes_memory_alloc(&fetch->read_tran->dst,
                               obj_data_size(&fetch->dst_odsc),
                               dimes_memory_rdma);
            if (err < 0) {
                goto err_out;
            }

            schedule_rdma_reads(fetch->read_tran->tran_id,
                                &fetch->src_odsc, &fetch->dst_odsc);

            err = dart_rdma_perform_reads(fetch->read_tran->tran_id);
            if (err < 0) {
                goto err_out;
            }

            while (!dart_rdma_check_reads(fetch->read_tran->tran_id)) {
                err = dart_rdma_process_reads();
                if (err < 0) {
                    goto err_out;
                }
            }

            // Copy fetched data
            obj_assemble(fetch, qte->data_ref);

            err = dimes_memory_free(&fetch->read_tran->dst, dimes_memory_rdma);
            if (err < 0) {
                goto err_out;
            }
        }
    }

    if (options.enable_dimes_ack) {
        // Send back ack messages to all dst. peers
        err = dimes_get_ack_by_msg(qte);
        if (err < 0) {
            goto err_out;
        }
    }

	qte->f_complete = 1;
	return 0;
err_out_free:
    for (i = 0; i < fetch_tab_size; i++) {
        if (fetch_status_tab[i] == fetch_posted) {
            dimes_memory_free(&fetch_tab[i]->read_tran->dst, dimes_memory_rdma);
        }
    } 
    free(fetch_tab);
    free(fetch_status_tab);
err_out:
	ERROR_TRACE();
}

static int dimes_obj_get(struct obj_data *od)
{
	struct dht_entry *dht_nodes[dimes_c->dcg->ss_info.num_space_srv];
	struct query_tran_entry_d *qte;
	int err = -ENOMEM;
	int num_dht_nodes, i;
#ifdef TIMING_PERF
    double tm_st, tm_end;
#endif

	qte = qte_alloc_d(od);
	if (!qte)
		goto err_out;
	qt_add_d(&dimes_c->qt, qte);

	/* get dht nodes */
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, od->obj_desc.name,
                                &od->gdim);
	num_dht_nodes = ssd_hash(ssd, &od->obj_desc.bb, dht_nodes);
	if (num_dht_nodes <= 0) {
		uloga("%s(): error! ssd_hash() return %d\n", __func__, num_dht_nodes);
		goto err_qt_free;
	}

	for ( i=0; i<num_dht_nodes && i<qte->qh->qh_size; i++ ) {
		qte->qh->qh_peerid_tab[i] = dht_nodes[i]->rank;
    }
	qte->qh->qh_peerid_tab[i] = -1;
	qte->qh->qh_num_peer = num_dht_nodes;
	qte->f_dht_peer_recv = 1;
#ifdef DEBUG
	uloga("%s(): #%d get dht peers complete!\n", __func__, DIMES_CID);
#endif

#ifdef TIMING_PERF
    tm_st = timer_read(&tm_perf);
#endif
	// Locate the RDMA buffers
	err = dimes_locate_data(qte);
	if ( err < 0 ) {
		if (err == -EAGAIN)
			goto out_no_data;
		else goto err_qt_free;
	}
	DIMES_WAIT_COMPLETION(qte->f_locate_data_complete == 1);
    // TODO: check the received data location information
#ifdef DEBUG
	uloga("%s(): #%d locate data complete!\n", __func__, DIMES_CID);
#endif
#ifdef TIMING_PERF
    tm_end = timer_read(&tm_perf);
    uloga("TIMING_PERF locate_data ts %d peer %d time %lf %s\n",
        od->obj_desc.version, DIMES_RANK, tm_end-tm_st, log_header);
    tm_st = tm_end;
#endif

	// Fetch the data
	err = dimes_fetch_data(qte);
	if (err < 0) {
		goto err_data_free;
	}

	if (!qte->f_complete) {
		err = -ENODATA;
		goto out_no_data;
	}
#ifdef DEBUG
	uloga("%s(): #%d fetch data complete!\n", __func__, DIMES_CID);
#endif
#ifdef TIMING_PERF
    tm_end = timer_read(&tm_perf);
    uloga("TIMING_PERF fetch_data ts %d peer %d time %lf %s\n",
        od->obj_desc.version, DIMES_RANK, tm_end-tm_st, log_header);
#endif
out_no_data:
	qt_free_obj_data_d(qte);
	qt_remove_d(&dimes_c->qt, qte);
	qte_free_d(qte);
	return err;
err_data_free:
	qt_free_obj_data_d(qte);
err_qt_free:
	qt_remove_d(&dimes_c->qt, qte);
	qte_free_d(qte);
err_out:
	ERROR_TRACE();
}

static int dcgrpc_dimes_get_ack(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	struct hdr_dimes_get_ack *oh = (struct hdr_dimes_get_ack *)cmd->pad;
	int sid = oh->sync_id;
	int err = -ENOMEM;

	struct dimes_memory_obj *mem_obj = storage_lookup_obj(sid);
	if (mem_obj == NULL) {
		uloga("%s(): failed to find dimes memory object sync_id=%d\n", __func__, sid);
		err = -1;
		goto err_out;
	}

    if (oh->bytes_read != obj_data_size(&mem_obj->obj_desc)) {
		uloga("%s(): should not happen...\n", __func__);
	}

	// Set the flag
    syncop_set_done(mem_obj->sync_id);

#ifdef DEBUG
	uloga("%s(): #%d get ack from #%d for sync_id=%d\n",
		__func__, DIMES_CID, cmd->id, mem_obj->sync_id);
#endif

	return 0;
 err_out:
	ERROR_TRACE();	
}

static int dimes_put_sync_with_timeout(float timeout_sec, struct dimes_storage_group *group)
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
		if (list_empty(&group->mem_obj_list)) {
			break;
		}

		struct dimes_memory_obj *p, *t;
		list_for_each_entry_safe(p, t, &group->mem_obj_list,
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
                err = dimes_memory_free(&p->rdma_handle, dimes_memory_rdma);
                if (err < 0) {
                    uloga("%s(): failed with dimes_memory_free()\n", __func__);
                }
                // Update rdma buffer usage
                size_t data_size = obj_data_size(&p->obj_desc);
                options.rdma_buffer_write_usage -= data_size;

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

static int dimes_put_free_group(struct dimes_storage_group *group)
{
    int err;
    if (!dimes_c) {
        uloga("'%s()': library was not properly initialized!\n",
                 __func__);
        return -EINVAL;
    }

    struct dimes_memory_obj *p, *t;
    list_for_each_entry_safe(p, t, &group->mem_obj_list,
                struct dimes_memory_obj, entry) {
        // Set the flag
        syncop_set_done(p->sync_id);
        err = dimes_memory_free(&p->rdma_handle, dimes_memory_rdma);
        if (err < 0) {
            uloga("%s(): failed with dimes_memory_free()\n", __func__);
        }
        // Update rdma buffer usage
        size_t data_size = obj_data_size(&p->obj_desc);
        options.rdma_buffer_write_usage -= data_size;

        list_del(&p->entry);
        free(p);
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

    options.enable_pre_allocated_rdma_buffer = 0;
    options.pre_allocated_rdma_buffer_size = DIMES_RDMA_BUFFER_SIZE*1024*1024; // bytes
    options.rdma_buffer_size = DIMES_RDMA_BUFFER_SIZE*1024*1024; // bytes 
    options.rdma_buffer_write_usage = 0;
    options.rdma_buffer_read_usage = 0;
    options.max_num_concurrent_rdma_read_op = DIMES_RDMA_MAX_NUM_CONCURRENT_READ;
#ifdef DS_HAVE_DIMES_ACK
    options.enable_dimes_ack = 1;
#else
    options.enable_dimes_ack = 0;
#endif

	dimes_c = calloc(1, sizeof(*dimes_c));
	dimes_c->dcg = (struct dcg_space*)ptr;
	if (!dimes_c->dcg) {
		uloga("'%s()': failed to initialize.\n", __func__);
		return NULL; 
	}

    syncop_init();
	qt_init_d(&dimes_c->qt);

	// Add rpc servie routines
	rpc_add_service(dimes_ss_info_msg, dcgrpc_dimes_ss_info);
	rpc_add_service(dimes_locate_data_msg, dcgrpc_dimes_locate_data);
	rpc_add_service(dimes_get_ack_msg, dcgrpc_dimes_get_ack); 

	err = dimes_ss_info(&num_dims);
	if (err < 0) {
		uloga("'%s()': failed to obtain space info, err %d.\n",
				__func__, err);
		return NULL;
	}

	storage_init();
	dart_rdma_init(RPC_S);
    dimes_memory_init();
    init_gdim_list(&dimes_c->gdim_list);

#ifdef TIMING_PERF
    timer_init(&tm_perf, 1);
    timer_start(&tm_perf);
#endif

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

    free_gdim_list(&dimes_c->gdim_list);
    free_sspace_dimes(dimes_c);
	free(dimes_c);
	dimes_c = NULL;

	storage_free();
    dimes_memory_finalize();
	dart_rdma_finalize();
}

void dimes_client_set_storage_type(int fst)
{
	if (fst == 0)
		st = row_major;
	else if (fst == 1)
		st = column_major;
}

int dimes_client_get(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data)
{
    uint64_t *gdim;
	struct obj_descriptor odsc = {
			.version = ver, .owner = -1,
			.st = st,
			.size = size,
			.bb = {.num_dims = ndim,}
	};
	memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

    memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
    memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);	

	struct obj_data *od;
	int err = -ENOMEM;

	if (!dimes_c->dcg) {
		uloga("%s(): library was not properly initialized!\n", __func__);
		err =  -EINVAL;
		goto err_out;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_no_data(&odsc, data);
	if (!od) {
		uloga("%s(): obj_data_alloc_no_data failed.\n", __func__);
		err = -ENOMEM;
		goto err_out;
	}

    set_global_dimension(&dimes_c->gdim_list, var_name,
            &dimes_c->dcg->default_gdim, &od->gdim);
	err = dimes_obj_get(od);
 
	obj_data_free(od);
	if (err < 0 && err != -EAGAIN) {
        uloga("%s(): failed with %d, can not get data object.\n",
                __func__, err);
    }

	return err;
err_out:
	ERROR_TRACE();
}

int dimes_client_put(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data)
{
    // TODO: assign owner id is important.
	struct obj_descriptor odsc = {
			.version = ver, .owner = DIMES_CID,
			.st = st,
			.size = size,
			.bb = {.num_dims = ndim,}
	};
	memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

    memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
    memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);

	int err = -ENOMEM;
	if (!dimes_c->dcg) {
		uloga("%s(): library was not properly initialized!\n", __func__);
		err = -EINVAL;
		goto err_out;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

    size_t data_size = obj_data_size(&odsc);
    // TODO: fix alignment issue, here assumes obj_data_size(&odsc) align by
    // 4 bytes ...
    struct dimes_memory_obj *mem_obj = (struct dimes_memory_obj*)
                                       malloc(sizeof(*mem_obj));
    mem_obj->sync_id = syncop_next_sync_id();
    mem_obj->ack_type = dimes_ack_type_msg;
    mem_obj->obj_desc = odsc;
    // TODO: can we memcpy safely? do we need a new function like
    // dimes_memory_alloc_with_data()?
    err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                             dimes_memory_rdma);
    if (err < 0) {
        uloga("%s(): dimes_memory_alloc() failed size %u bytes\n",
            __func__, data_size);
        free(mem_obj);
        goto err_out;
    }
    // Copy user data
    memcpy(mem_obj->rdma_handle.base_addr, data, data_size);

    set_global_dimension(&dimes_c->gdim_list, var_name,
            &dimes_c->dcg->default_gdim, &mem_obj->gdim);
	err = dimes_obj_put(mem_obj);
	if (err < 0) {
        dimes_memory_free(&mem_obj->rdma_handle, dimes_memory_rdma);
        free(mem_obj);
		uloga("'%s()': failed with %d, can not put data object.\n",
				__func__, err);
		goto err_out;
	}
    
    // Update memory usage
    options.rdma_buffer_write_usage += data_size;

	return 0;
err_out:
	ERROR_TRACE();
}

int dimes_client_put_sync_all(void)
{
	int err;
	struct dimes_storage_group *p;
	list_for_each_entry(p, &storage, struct dimes_storage_group, entry)
	{
        if (options.enable_dimes_ack) {	
            err = dimes_put_sync_with_timeout(-1.0, p);
        } else {
            err = dimes_put_free_group(p);
        }
        if (err < 0) return err;
    }

	return 0;
}

int dimes_client_put_set_group(const char *group_name, int step)
{
    struct dimes_storage_group *p;
    p = storage_lookup_group(group_name);
    if (p == NULL) {
        p = storage_add_group(group_name);
    }

    current_group_name = p->name;

	return 0;
}

int dimes_client_put_unset_group()
{
    struct dimes_storage_group *p;
    p = storage_lookup_group(default_group_name);
    if (p == NULL) {
        uloga("%s(): p == NULL should not happen!\n", __func__);
        p = storage_add_group(default_group_name);
    }

    current_group_name = p->name;

	return 0;
}

int dimes_client_put_sync_group(const char *group_name, int step)
{
    int err;
    struct dimes_storage_group *p;
    p = storage_lookup_group(group_name);
    if (p == NULL) return 0;

    if (options.enable_dimes_ack) {
        err = dimes_put_sync_with_timeout(-1, p);
    } else {
        err = dimes_put_free_group(p);
    }
    if (err < 0) return err;

    return 0;
}

#ifdef TIMING_PERF
int common_dimes_set_log_header(const char *str)
{
    strcpy(log_header, str);
    return 0;
}
#endif

#endif // end of #ifdef DS_HAVE_DIMES
