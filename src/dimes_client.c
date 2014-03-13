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

#include "config.h"

#ifdef DS_HAVE_DIMES

#include "dimes_interface.h"
#include "dimes_client.h"
#include "dimes_data.h"

static struct dimes_client *dimes_c = NULL;
//static enum storage_type st = row_major;
static enum storage_type st = column_major;
static int num_dims = 2;
#ifdef TIMING_PERF
static char log_header[256] = "";
static struct timer tm_perf;
#endif

struct sspace_conf {
    int ndims;
    uint64_t dimx, dimy, dimz;
};

// TODO: it is all hard coded for now
static int init_sspace_dimes(struct dimes_client *d, int num_sp, int max_versions)
{
    struct sspace_conf confs[MAX_NUM_SSD];
    int i, err;

    confs[0].ndims = 2;
    confs[0].dimx = 11750400;
    confs[0].dimy = 1;
    confs[0].dimz = 1;

    confs[1].ndims = 2;
    confs[1].dimx = 9;
    confs[1].dimy = 11750400;
    confs[1].dimz = 1;

    confs[2].ndims = 2;
    confs[2].dimx = 64;
    confs[2].dimy = 1;
    confs[2].dimz = 1;

    for (i = 0; i < MAX_NUM_SSD; i++) {
        struct bbox domain;
        memset(&domain, 0, sizeof(struct bbox));
        domain.num_dims = confs[i].ndims;
        domain.lb.c[0] = 0;
        domain.lb.c[1] = 0;
        domain.lb.c[2] = 0;
        domain.ub.c[0] = confs[i].dimx - 1; 
        domain.ub.c[1] = confs[i].dimy - 1; 
        domain.ub.c[2] = confs[i].dimz - 1; 

        d->spaces[i] = ssd_alloc_v2(&domain, num_sp, max_versions);

        if (!d->spaces[i]) {
            uloga("%s(): ssd_alloc failed with i= %d\n", __func__, i);
        }
    }

    return 0;
}

static int free_sspace_dimes(struct dimes_client *d)
{
    int i;
    for (i = 0; i < MAX_NUM_SSD; i++) {
        ssd_free_v2(d->spaces[i]);
    }

    return 0;
}

static struct sspace* lookup_sspace_dimes(struct dimes_client *d, const char* var_name)
{
#ifdef DS_SSD_HASH_V2
    char *var1 = "igid";
    char *var2 = "iphase";
    char *var3 = "dpot";

    int pos;
    size_t len = strlen(var_name);
    if (len >= strlen(var1)) {
        pos = len - strlen(var1);  
        if (0 == strcmp(var_name+pos, var1)) {
            return d->spaces[0];
        }
    }
    
    if (len >= strlen(var2)) {
        pos = len - strlen(var2);  
        if (0 == strcmp(var_name+pos, var2)) {
            return d->spaces[1];
        }
    }

    if (len >= strlen(var3)) {
        pos = len - strlen(var3);  
        if (0 == strcmp(var_name+pos, var3)) {
            return d->spaces[2];
        }
    }

    return d->ssd; // returns default sspace
#else
    return d->ssd;
#endif
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
static struct dimes_client *dimes_c = NULL;
static enum storage_type default_st = column_major;
#ifdef TIMING_PERF
static char log_header[256] = "";
static struct timer tm_perf;
#endif

/* Macros definitions. */
#define DIMES_WAIT_COMPLETION(x)                \
    do {                            \
        err = dc_process(dimes_c->dcg->dc);     \
        if (err < 0)                    \
            goto err_out;               \
    } while (!(x))
// Return dart id for myself.
#define DIMES_CID   dimes_c->dcg->dc->self->ptlmap.id
// Return dart rank for myself.
#define DIMES_RANK (DIMES_CID-dimes_c->dcg->dc->cp_min_rank)
// Return pointer to dart client. 
#define DART_CLIENT_PTR dimes_c->dcg->dc
// Return pointer to rpc server.
#define RPC_SERVER_PTR dimes_c->dcg->dc->rpc_s 
// Return number of servers.
#define NUM_SERVER dimes_c->dcg->dc->num_sp

/* Forward declarations. */
static int dimes_memory_free(struct dart_rdma_mem_handle *rdma_hndl);

// Allocate the default shared space dht using the global domain 
// that specified in dataspaces.conf. DIMES client uses this dht
// to determine which servers are used for sending the data update
// requests (for put operation) or data locating requests (for get operation).
//
// Linked list sspace_list stores shared space dhts that are dynamically 
// created based on global domain information provided by users through 
// dimes_define_gdim() API. 
static int init_sspace_dimes(struct dimes_client *d)
{
    int err = -ENOMEM;
    // DIMES client only use the shared space dht for selecting 
    // servers. So set max version to 1 is ok. 
    const int max_versions = 1;
    d->default_ssd = ssd_alloc(&d->dcg->ss_domain,
                        d->dcg->ss_info.num_space_srv, 
                        max_versions, d->dcg->hash_version);
    if (!d->default_ssd) {
        goto err_out;
    }

    INIT_LIST_HEAD(&d->sspace_list);
    return 0;
 err_out:
    uloga("%s(): ERROR failed.\n", __func__);
    return err;
}

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

// Deallocate the default shared space dht and shared space dhts that stored
// in linked list sspace_list. 
static int free_sspace_dimes(struct dimes_client *d)
{
    ssd_free(d->default_ssd);
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

// Lookup shared space dht by global dimension.
static struct sspace* lookup_sspace_dimes(struct dimes_client *d, const struct global_dimension* gd)
{
    // If global domain gdim equals to the one specified 
    // in dataspaces.conf, then return the default shared space dht.
    if (global_dimension_equal(gd, &d->dcg->default_gdim)) {
        return d->default_ssd;
    }

    // Otherwise, search for shared space dht in the linked list.
    struct sspace_list_entry *ssd_entry = NULL;
    list_for_each_entry(ssd_entry, &d->sspace_list,
        struct sspace_list_entry, entry)
    {
        if (gd->ndim != ssd_entry->gdim.ndim)
            continue;

        if (global_dimension_equal(gd, &ssd_entry->gdim))
            return ssd_entry->ssd;
    }

    // If not found, create new shared space dht and add it to the linked list.
    int i, err;
    struct bbox domain;
    memset(&domain, 0, sizeof(struct bbox));
    domain.num_dims = gd->ndim;
    for (i = 0; i < gd->ndim; i++) {
        domain.lb.c[i] = 0;
        domain.ub.c[i] = gd->sizes.c[i] - 1;
    }

    ssd_entry = malloc(sizeof(struct sspace_list_entry));
    memcpy(&ssd_entry->gdim, gd, sizeof(struct global_dimension));
    const int max_versions = 1;
    ssd_entry->ssd = ssd_alloc(&domain, d->dcg->ss_info.num_space_srv, max_versions,
                            d->dcg->hash_version);
    if (!ssd_entry->ssd) {
        uloga("%s(): ERROR ssd_alloc() failed\n", __func__);
        return d->default_ssd;
    }

    list_add(&ssd_entry->entry, &d->sspace_list);
    return ssd_entry->ssd;
}

// Check if peer has the same DART id as myself.
static int is_peer_myself(struct node_id *peer)
{
    return peer->ptlmap.id == DIMES_CID;
}

// Calculate size (in bytes) of available RDMA memory buffer.
static size_t get_available_rdma_buffer_size()
{
    if (options.rdma_buffer_usage > options.rdma_buffer_size) return 0; 
    else return options.rdma_buffer_size-options.rdma_buffer_usage;
}

static void print_rdma_buffer_usage()
{
#ifdef DEBUG_DIMES_BUFFER_USAGE
    uloga("DIMES rdma buffer usage: peer #%d "
          "rdma_buffer_size= %u bytes "
          "rdma_buffer_usage= %u bytes "
          "rdma_buffer_avail_size= %u bytes\n", DIMES_CID,
        options.rdma_buffer_size, options.rdma_buffer_usage,
        get_available_rdma_buffer_size());
#endif
}

static uint32_t local_obj_index_seed = 0;
static uint32_t next_local_obj_index()
{
    return local_obj_index_seed++;
}

static char current_group_name[STORAGE_GROUP_NAME_MAXLEN+1];
const char* default_group_name = "__default_storage_group__";
static void update_current_group_name(const char *name)
{
    if (!name) return;
    if (strlen(name)>STORAGE_GROUP_NAME_MAXLEN) {
        strncpy(current_group_name, name, STORAGE_GROUP_NAME_MAXLEN);
        current_group_name[STORAGE_GROUP_NAME_MAXLEN] = '\0';
    } else strcpy(current_group_name, name);
}

/*
    About DIMES storage:
    Data objects in local RDMA memory buffer are logically organized into a two-level 
    by their "group name" and "version". "group name" is an attribute specified through
    the dimes_put_set_group() function. When this attribute is not explicitly specified,
    the data objects are under group "__default_storage_group__". "version" is specified  
    through dimes_put() function. The graph below illustrates this two-level organization. 

    group 1--group 2--group 3
     |
     |
    version0--obj--obj--obj--obj
     |
     |    
    version1--obj--obj--obj--obj
     |
     |
    version2--obj--obj--obj 
*/
// Add data object to the linked list.
static int mem_obj_list_add(struct list_head *l, struct dimes_memory_obj *p) {
	list_add(&p->entry, l);
	return 0;
}

// Lookup data object (in local RDMA memory buffer) by dimes obj id.
static struct dimes_memory_obj* mem_obj_list_lookup(struct list_head *l, const struct dimes_obj_id *oid) {
	struct dimes_memory_obj *p;
	list_for_each_entry(p, l, struct dimes_memory_obj, entry) {
		if (equal_dimes_obj_id(&p->obj_id, oid)) return p;
	}

	return NULL;
} 

// Create and add a new group. 
static struct dimes_storage_group* storage_add_group(const char *group_name)
{
	struct dimes_storage_group *p = malloc(sizeof(*p));
    if (!p) goto err_out_free;

    p->version_tab = malloc(sizeof(*p->version_tab)*dimes_c->dcg->max_versions);
    if (!p->version_tab) goto err_out_free;

	strcpy(p->name, group_name);
    int i;
    for (i = 0; i < dimes_c->dcg->max_versions; i++) {
        INIT_LIST_HEAD(&p->version_tab[i]);
    }
	list_add(&p->entry, &dimes_c->storage);	
	return p;
 err_out_free:
    if (p) free(p);
    return NULL;
}

// Lookup group by its name.
static struct dimes_storage_group* storage_lookup_group(const char *group_name)
{
	struct dimes_storage_group *p;
	list_for_each_entry(p, &dimes_c->storage, struct dimes_storage_group, entry)
	{
		if (p->name == NULL) continue;
		if (0 == strcmp(p->name, group_name)) {
			return p;
		} 
	}

	return NULL;
} 

// Deallocate a group.
static int storage_free_group(struct dimes_storage_group *group)
{
    int i, err;
    struct dimes_memory_obj *p, *t;
    for (i = 0; i < dimes_c->dcg->max_versions; i++) {
        list_for_each_entry_safe(p, t, &group->version_tab[i],
                    struct dimes_memory_obj, entry) {
            dimes_memory_free(&p->rdma_handle);
            list_del(&p->entry);
            free(p);            
        }
    }

    list_del(&group->entry); 
    if (group->version_tab) free(group->version_tab);
    free(group);

    return 0;
}

// Initialize dimes storage.
static void storage_init()
{
	INIT_LIST_HEAD(&dimes_c->storage);

	// Set current group as default
    update_current_group_name(default_group_name);
} 

// Finalize dimes storage.
static void storage_free()
{
	struct dimes_storage_group *p, *t;
	list_for_each_entry_safe(p, t, &dimes_c->storage, struct dimes_storage_group, entry) 
	{
        storage_free_group(p);
	}
}

// Add a data object to dimes storage.
static int storage_add_obj(struct dimes_memory_obj *mem_obj)
{
	struct dimes_storage_group *p = storage_lookup_group(current_group_name);
	if (!p) p = storage_add_group(current_group_name);

    int tab_idx = mem_obj->obj_desc.version % dimes_c->dcg->max_versions;
    return mem_obj_list_add(&p->version_tab[tab_idx], mem_obj);    
}

// Lookup a data object by (1) dimes obj id; and (2) version.
static struct dimes_memory_obj* storage_lookup_obj(const struct dimes_obj_id *oid, int version)
{
	struct dimes_storage_group *p;
	list_for_each_entry(p, &dimes_c->storage, struct dimes_storage_group, entry)
	{
        int tab_idx = version % dimes_c->dcg->max_versions;
		struct dimes_memory_obj *mem_obj;
		mem_obj = mem_obj_list_lookup(&p->version_tab[tab_idx], oid);
		if (mem_obj != NULL) return mem_obj;
	}	

	return NULL;
}

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
            uloga("%s(): ERROR peer #%d failed to register RDMA memory\n",
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
            uloga("%s(): ERROR peer #%d failed to deregister RDMA memory\n",
                __func__, DIMES_CID);
            return err;
        }

        dimes_buffer_finalize();
        free((void*)options.pre_allocated_rdma_handle.base_addr);
    }

    return 0;
}

static int dimes_memory_alloc(struct dart_rdma_mem_handle *rdma_hndl, 
    size_t size, enum dart_memory_type type)
{
    int err;
    unsigned char use_rdma_memory = 0;
    uint64_t buf;
    rdma_hndl->mem_type = type;

    if (rdma_hndl->mem_type == dart_memory_rdma) use_rdma_memory = 1;
    // Check if there is available rdma buffer
    if (use_rdma_memory && get_available_rdma_buffer_size() < size) {
        uloga("%s(): ERROR: no sufficient RDMA memory for caching data "
            "with %u bytes. Suggested fix: increase the value of "
            "'--with-dimes-rdma-buffer-size' at configuration.\n",
            __func__, size);
        print_rdma_buffer_usage();            
        return -1;
    } 

    switch (rdma_hndl->mem_type) {
    case dart_memory_non_rdma:
        buf = (uint64_t)malloc(size);
        if (!buf) goto err_out_malloc;

        rdma_hndl->base_addr = buf;
        rdma_hndl->size = size;
        break;
    case dart_memory_rdma:
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
            if (!buf) goto err_out_malloc;

            err = dart_rdma_register_mem(rdma_hndl, (void*)buf, size);
            if (err < 0) {
                uloga("%s(): ERROR peer #%d failed to register RDMA memory\n",
                    __func__, DIMES_CID);
                goto err_out;
            }
        }
        break;
    default:
        uloga("%s(): ERROR unknow dart_memory_type %d\n", __func__, rdma_hndl->mem_type);
        goto err_out;
    }

    // Update RDMA memory usage
    if (use_rdma_memory) options.rdma_buffer_usage += size;
#ifdef DEBUG
    if (use_rdma_memory) print_rdma_buffer_usage();
#endif
    return 0;
 err_out_malloc:
    uloga("%s(): ERROR malloc() failed\n", __func__);
 err_out:
    return -1;
}

static int dimes_memory_free(struct dart_rdma_mem_handle *rdma_hndl)
{
    int err;
    unsigned char use_rdma_memory = 0;
    if (rdma_hndl->mem_type == dart_memory_rdma) use_rdma_memory = 1;

    switch (rdma_hndl->mem_type) {
    case dart_memory_non_rdma:
        free((void*)rdma_hndl->base_addr);
        break;
    case dart_memory_rdma:
        if (options.enable_pre_allocated_rdma_buffer) {
            dimes_buffer_free(rdma_hndl->base_addr);
        } else {
            err = dart_rdma_deregister_mem(rdma_hndl);
            if (err < 0) {
                uloga("%s(): ERROR peer #%d failed to deregister RDMA memory\n",
                    __func__, DIMES_CID);
                goto err_out; 
            }

            free((void*)rdma_hndl->base_addr);
        }
        break;
    default:
        uloga("%s(): ERROR unknow dart_memory_type %d\n", __func__, rdma_hndl->mem_type);
        goto err_out;
    }

    // Update RDMA buffer usage
    if (use_rdma_memory) options.rdma_buffer_usage -= rdma_hndl->size; 
#ifdef DEBUG
    if (use_rdma_memory) print_rdma_buffer_usage();
#endif
    return 0;
 err_out:
    return -1;
}

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
	qte->qh = qh_alloc_d(NUM_SERVER);
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
	qt->num_entry = 0;
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
	qt->num_entry++;
}

static void qt_remove_d(struct query_tran_d *qt, struct query_tran_entry_d *qte)
{
	list_del(&qte->q_entry);
	qt->num_entry--;
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
    struct hdr_dimes_put *hdr = (struct hdr_dimes_put*)cmd->pad;
    struct node_id *peer;
    struct fetch_entry *fetch;
    fetch = (struct fetch_entry*)malloc(sizeof(*fetch));

    // Lookup the owner peer of the remote data object
    peer = dc_get_peer(DART_CLIENT_PTR, hdr->odsc.owner);

    // Creat read transaction
    dart_rdma_create_read_tran(peer, &fetch->read_tran);

    // Set source and destination object descriptors
    fetch->remote_obj_id = hdr->obj_id;
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

// Copy fetched data into the results buffer.
static int obj_assemble(struct fetch_entry *fetch, struct obj_data *od)
{
    int err;
    struct obj_data *from = obj_data_alloc_no_data(&fetch->dst_odsc,
                                  (void*)fetch->read_tran->dst.base_addr);
    err = ssd_copy(od, from);
    if (err == 0) {
        obj_data_free(from);
        return 0;
    }

    obj_data_free(from);
    return err;
}

// Callback function that invoked when the client succesfully transfers
// the data location information from server.
static int locate_data_completion_client(struct rpc_server *rpc_s,
					 struct msg_buf *msg)
{
	struct hdr_dimes_get *oh = msg->private;
	struct rpc_cmd *tab = msg->msg_data;
	int i, err = -ENOENT;

	struct query_tran_entry_d *qte = qt_find_d(&dimes_c->qt, oh->qid);
	if (!qte) {
		uloga("%s(): ERROR can not find transaction qid= %d\n",
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
            if (err < 0) goto err_out_free;
		} else {
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

// Callback function for 'dimes_locate_data_msg' message.
static int dcgrpc_dimes_locate_data(struct rpc_server *rpc_s,
				    struct rpc_cmd *cmd)
{
	struct hdr_dimes_get *oht, 
			 *oh = (struct hdr_dimes_get *) cmd->pad;
	struct node_id *peer = dc_get_peer(DART_CLIENT_PTR, cmd->id);
	struct rpc_cmd *tab;
	struct msg_buf *msg;
	int err = -ENOMEM;

#ifdef DEBUG
	uloga("%s(): peer #%d query id %d rpc return code %d number of obj to fetch %d\n",
        __func__, DIMES_CID, oh->qid, oh->rc, oh->num_obj);
#endif

	if (oh->rc == -1) {
		// Server has no location information.
		struct query_tran_entry_d *qte =
				qt_find_d(&dimes_c->qt, oh->qid);
		if (!qte) {
			uloga("%s(): ERROR can not find transaction qid= %d\n",
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

    // Transfer data location information from server using rpc_receive_direct().
    // Location information is stored as an array of struct rpc_cmd.
	tab = malloc(sizeof(struct rpc_cmd) * oh->num_obj);
	if (!tab) goto err_out;

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
	if (err == 0) return 0;

	free(tab);
	free(msg);
err_out:
	ERROR_TRACE();
}

#ifdef HAVE_PAMI
/*
    Auxiliary functions used for PAMI network interface.
*/
static int completion_dimes_obj_put(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    if (msg) {
        int *pflag = msg->private;
        *pflag = 1;
        free(msg);
    }
    return 0;
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

static int dimes_ss_info(int *num_dims)
{
	struct msg_buf *msg;
	struct node_id *peer;
	int err = -ENOMEM;

	if (dimes_c->f_ss_info) {
		*num_dims = dimes_c->domain.num_dims;
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
	
	*num_dims = dimes_c->dcg->ss_info.num_dims;
#ifdef DS_SSD_HASH_V2
	dimes_c->ssd = ssd_alloc_v2(&dimes_c->domain,
                             dimes_c->dcg->ss_info.num_space_srv, 1);
    init_sspace_dimes(dimes_c, dimes_c->dcg->ss_info.num_space_srv, 1);
#else
	dimes_c->ssd = ssd_alloc(&dimes_c->domain,
                             dimes_c->dcg->ss_info.num_space_srv, 1);
#endif
	if (!dimes_c->ssd) {
		uloga("%s(): ssd_alloc failed!\n",__func__);
		err = -1;
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

static int rpc_send_complete(const int *send_flags, int num_send)
{
    int i;
    for (i = 0; i < num_send; i++) {
        if (!send_flags[i]) return 0;
    }

    return 1;
}
#endif

// Send update messages to corresponding dht nodes (servers) and add new
// data object. 
static int dimes_obj_put(struct dimes_memory_obj *mem_obj)
{
	struct dht_entry *dht_nodes[dimes_c->dcg->ss_info.num_space_srv];
	struct hdr_dimes_put *hdr;
	struct msg_buf *msg;
	struct node_id *peer;
	int i, num_dht_nodes;
	int err = -ENOMEM;

	// Update the DHT nodes
#ifdef DS_SSD_HASH_V2
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, mem_obj->obj_desc.name);
	num_dht_nodes = ssd_hash_v2(ssd, &mem_obj->obj_desc.bb, dht_nodes);
#else
	num_dht_nodes = ssd_hash(dimes_c->ssd, &mem_obj->obj_desc.bb, dht_nodes);
#endif
	if (num_dht_nodes <= 0) {
		uloga("%s(): ERROR: ssd_hash() return %d but the value should be > 0\n",
				__func__, num_dht_nodes);
		goto err_out;
	}

#ifdef HAVE_PAMI
    // TODO: do we need to do the same for all other platforms?
    int *send_flags = (int*)malloc(sizeof(int)*num_dht_nodes);
    memset(send_flags, 0, sizeof(int)*num_dht_nodes);
#endif
	
	for (i = 0; i < num_dht_nodes; i++) {
		// TODO(fan): There is assumption here that the space servers
		// rank range from 0~(num_space_srv-1).
		peer = dc_get_peer(DART_CLIENT_PTR, dht_nodes[i]->rank);
		msg = msg_buf_alloc(RPC_SERVER_PTR, peer, 1);
		if (!msg)
			goto err_out;

		msg->msg_rpc->cmd = dimes_put_msg;
		msg->msg_rpc->id = DIMES_CID;
#ifdef HAVE_PAMI
        msg->cb = completion_dimes_obj_put;
        msg->private = &(send_flags[i]);
#endif
		dart_rdma_set_memregion_to_cmd(&mem_obj->rdma_handle, msg->msg_rpc);	

		hdr = (struct hdr_dimes_put *)msg->msg_rpc->pad;
        hdr->ptlmap = dimes_c->dcg->dc->rpc_s->ptlmap; 
		hdr->odsc = mem_obj->obj_desc;
		hdr->obj_id = mem_obj->obj_id;
	
		err = rpc_send(RPC_SERVER_PTR, peer, msg);
		if (err < 0) {
			free(msg);
			goto err_out;
		}
	}

#ifdef HAVE_PAMI
    // block for all the rpc_send to complete
    while (!rpc_send_complete(send_flags, num_dht_nodes))
    {
        err = rpc_process_event(RPC_SERVER_PTR);
        if (err < 0) {
            free(send_flags);
            goto err_out;
        }
    }
    free(send_flags);
#endif

	storage_add_obj(mem_obj);
	return 0;
err_out:
	ERROR_TRACE();			
}

// Lookup data locations information for a dimes_get query.
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
		peer = dc_get_peer(DART_CLIENT_PTR, *peer_id);
		err = -ENOMEM;
		msg = msg_buf_alloc(RPC_SERVER_PTR, peer, 1);
		if (!msg) goto err_out;

		msg->msg_rpc->cmd = dimes_locate_data_msg;
		msg->msg_rpc->id = DIMES_CID;

		oh = (struct hdr_dimes_get *) msg->msg_rpc->pad;
		oh->qid = qte->q_id;
		oh->odsc = qte->q_obj;

		qte->qh->qh_num_req_posted++;
		err = rpc_send(RPC_SERVER_PTR, peer, msg);
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
	uint64_t lb[BBOX_MAX_NDIM];
	uint64_t ub[BBOX_MAX_NDIM];
};

struct matrix_d {
	uint64_t 	dist[BBOX_MAX_NDIM];
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
			uloga("%s(): ERROR failed with dart_rdma_schedule_read()\n",
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
			uloga("%s(): ERROR failed with matrix_rdma_copy()\n", __func__);
            goto err_out;
		}	
	}	

	return 0;
err_out:
	ERROR_TRACE();
} 

static int is_remote_data_contiguous_in_memory(struct obj_descriptor *src_odsc,
                struct obj_descriptor *dst_odsc)
{
    if (!src_odsc || !dst_odsc) return 0;

    // check dimension first
    if (src_odsc->bb.num_dims != dst_odsc->bb.num_dims) return 0;

    struct matrix_d from;
    struct bbox bb;
    bbox_intersect(&dst_odsc->bb, &src_odsc->bb, &bb);
    matrix_init_d(&from, src_odsc->st, &src_odsc->bb, &bb, src_odsc->size);
    
    // check dimension 0 -> n-2 (fast to slow)
    int i, ret = 1;
    for (i = 0; i < from.num_dims-1; i++) {
        if ((from.mat_view.ub[i]-from.mat_view.lb[i]+1) != from.dist[i]) {
            ret = 0;
            break;
        }
    }
    
    return ret; 
}

static size_t calculate_offset_for_remote_data(struct obj_descriptor *src_odsc,
                struct obj_descriptor *dst_odsc)
{
    struct matrix_d from;
    struct bbox bb;
    bbox_intersect(&dst_odsc->bb, &src_odsc->bb, &bb);
    matrix_init_d(&from, src_odsc->st, &src_odsc->bb, &bb, src_odsc->size);

    size_t num_elem = 1, offset = 0;
    int i;
    for (i = 0; i < from.num_dims-1; i++) {
        num_elem *= from.dist[i];
    }
    num_elem *= from.mat_view.lb[from.num_dims-1];
    offset = num_elem*from.size_elem;

    return offset;
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
                err = dimes_memory_free(&fetch_tab[i]->read_tran->dst);
                if (err < 0) {
                    goto err_out;
                }

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
        if (fetch_status_tab[i] == fetch_ready) {
            err = dimes_memory_alloc(&fetch_tab[i]->read_tran->dst,
                                     read_size, dart_memory_rdma);
            if (err < 0) goto err_out;
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

// Fetching data for a dimes_get query.
static int dimes_fetch_data(struct query_tran_entry_d *qte)
{
    struct fetch_entry *fetch;
	int i = 0, err;
	qte->f_complete = 0;

	// Allocate the array for storing fetch operations 
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
        // check the size of remote data object
        if (obj_data_size(&fetch->dst_odsc) > get_available_rdma_buffer_size())
        {
            uloga("%s(): ERROR no sufficient RDMA memory for fetching "
                "remote data object with size %u bytes. Suggested fix: "
                "increase the value of '--with-dimes-rdma-buffer-size' "
                "at configuration.\n",
                __func__, obj_data_size(&fetch->dst_odsc));
            print_rdma_buffer_usage();
            goto err_out_free;
        }

        if (!is_peer_myself(fetch->read_tran->remote_peer) &&
            is_remote_data_contiguous_in_memory(&fetch->src_odsc, &fetch->dst_odsc))
        {
            fetch_tab[i] = fetch;

            size_t data_size, src_offset, dst_offset;
            data_size = obj_data_size(&fetch->dst_odsc);
            src_offset = calculate_offset_for_remote_data(&fetch->src_odsc, &fetch->dst_odsc);
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
        if (is_peer_myself(fetch->read_tran->remote_peer)) {
#ifdef DEBUG
            uloga("%s(): peer %d fetch data from local memory.\n", __func__, DIMES_CID);
#endif
            // Data is in local memory, fetch directly
            struct dimes_memory_obj *mem_obj = storage_lookup_obj(&fetch->remote_obj_id,
                                                       fetch->src_odsc.version);
            if (mem_obj == NULL) {
                uloga("%s(): ERROR failed to find data object in local memory.\n", __func__);
                goto err_out;
            }

            // Update source memory region
            fetch->read_tran->src.base_addr = mem_obj->rdma_handle.base_addr;
            fetch->read_tran->src.size = mem_obj->rdma_handle.size;

            // Alloc receive buffer, schedle reads, perform reads
            dimes_memory_alloc(&fetch->read_tran->dst,
                               obj_data_size(&fetch->dst_odsc),
                               dart_memory_non_rdma);
            schedule_rdma_reads(fetch->read_tran->tran_id,
                                &fetch->src_odsc, &fetch->dst_odsc);
            dart_rdma_perform_reads(fetch->read_tran->tran_id);
            // Copy fetched data
            obj_assemble(fetch, qte->data_ref);
            dimes_memory_free(&fetch->read_tran->dst);
        } 
        else if (!is_remote_data_contiguous_in_memory(&fetch->src_odsc, &fetch->dst_odsc))
        {
            // Data on remote peer
            // Alloc receive buffer, schedle reads, perform reads
            err = dimes_memory_alloc(&fetch->read_tran->dst,
                               obj_data_size(&fetch->dst_odsc),
                               dart_memory_rdma);
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
            dimes_memory_free(&fetch->read_tran->dst);
        }
    }

	qte->f_complete = 1;
	return 0;
err_out_free:
    for (i = 0; i < fetch_tab_size; i++) {
        if (fetch_status_tab[i] == fetch_posted) {
            dimes_memory_free(&fetch_tab[i]->read_tran->dst);
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
#ifdef DS_SSD_HASH_V2
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, od->obj_desc.name);
	num_dht_nodes = ssd_hash_v2(ssd, &od->obj_desc.bb, dht_nodes);
#else
	num_dht_nodes = ssd_hash(dimes_c->ssd, &od->obj_desc.bb, dht_nodes);
#endif
	if (num_dht_nodes <= 0) {
		uloga("%s(): ERROR ssd_hash() return %d but value should be > 0\n",
            __func__, num_dht_nodes);
		goto err_qt_free;
	}

	for ( i=0; i<num_dht_nodes && i<qte->qh->qh_size; i++ ) {
		qte->qh->qh_peerid_tab[i] = dht_nodes[i]->rank;
    }
	qte->qh->qh_peerid_tab[i] = -1;
	qte->qh->qh_num_peer = num_dht_nodes;

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
	//uloga("%s(): #%d locate data complete!\n", __func__, DIMES_CID);
#endif
#ifdef TIMING_PERF
    tm_end = timer_read(&tm_perf);
    uloga("TIMING_PERF locate_data ts %d peer %d time %lf %s\n",
        od->obj_desc.version, DIMES_RANK, tm_end-tm_st, log_header);
    tm_st = tm_end;
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
	//uloga("%s(): #%d fetch data complete!\n", __func__, DIMES_CID);
#endif
#ifdef TIMING_PERF
    tm_end = timer_read(&tm_perf);
    uloga("TIMING_PERF fetch_data ts %d peer %d time %lf %s\n",
        od->obj_desc.version, DIMES_RANK, tm_end-tm_st, log_header);
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
    options.rdma_buffer_usage = 0;
    options.max_num_concurrent_rdma_read_op = DIMES_RDMA_MAX_NUM_CONCURRENT_READ;

	dimes_c = calloc(1, sizeof(*dimes_c));
	dimes_c->dcg = (struct dcg_space*)ptr;
	if (!dimes_c->dcg) {
        goto err_free;
	}

	qt_init_d(&dimes_c->qt);

	// Add rpc servie routines
	rpc_add_service(dimes_locate_data_msg, dcgrpc_dimes_locate_data);

	if (!dimes_c->dcg->f_ss_info) {
		uloga("%s(): ERROR failed to retrieve the default global domain "
            "information that configured in dataspaces.conf.\n", __func__);
        goto err_free;
	}

    err = init_sspace_dimes(dimes_c);
    if (err < 0) {
        goto err_free;
    }

	storage_init();
	dart_rdma_init(RPC_SERVER_PTR);
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
 err_free:
    if (dimes_c) free(dimes_c);
    dimes_c = NULL;
    return NULL;
}

void dimes_client_free(void) {
	if (!dimes_c) {
		uloga("'%s()': library was not properly initialized!\n",
				 __func__);
		return;
	}

    if (dimes_c->ssd) {
#ifdef DS_SSD_HASH_V2
        ssd_free_v2(dimes_c->ssd);
        free_sspace_dimes(dimes_c);
#else
        ssd_free(dimes_c->ssd);
#endif
    }
	free(dimes_c);
	dimes_c = NULL;

	storage_free();
    dimes_memory_finalize();
	dart_rdma_finalize();

	free(dimes_c);
	dimes_c = NULL;
}

int dimes_client_get(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data)
{
	struct obj_descriptor odsc = {
			.version = ver, .owner = -1,
			.st = default_st,
			.size = size,
			.bb = {.num_dims = ndim,}
	};
	memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

    memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
    memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);	

	struct obj_data *od;
	int err = -ENOMEM;

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_no_data(&odsc, data);
	if (!od) {
		uloga("%s(): ERROR obj_data_alloc_no_data() failed.\n", __func__);
		err = -ENOMEM;
		goto err_out;
	}

    set_global_dimension(&dimes_c->gdim_list, var_name,
            &dimes_c->dcg->default_gdim, &od->gdim);
	err = dimes_obj_get(od);
	obj_data_free(od);
	if (err < 0 && err != -EAGAIN) {
        goto err_out;
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
	struct obj_descriptor odsc = {
			.version = ver, .owner = DIMES_CID,
			.st = default_st,
			.size = size,
			.bb = {.num_dims = ndim,}
	};
	memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
    memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

    memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
    memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);

	int err = -ENOMEM;
	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

    size_t data_size = obj_data_size(&odsc);
    // TODO: fix alignment issue, here assumes obj_data_size(&odsc) align by
    // 4 bytes ...
    struct dimes_memory_obj *mem_obj = (struct dimes_memory_obj*)
                                       malloc(sizeof(*mem_obj));
    mem_obj->obj_id.dart_id = DIMES_CID;
    mem_obj->obj_id.local_obj_index = next_local_obj_index();
    mem_obj->obj_desc = odsc;
    err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                             dart_memory_rdma);
    if (err < 0) {
        uloga("%s(): dimes_memory_alloc() failed size %u bytes\n",
            __func__, data_size);
        free(mem_obj);
        goto err_out;
    }

    // Copy user data
    memcpy((void*)mem_obj->rdma_handle.base_addr, data, data_size);

    set_global_dimension(&dimes_c->gdim_list, var_name,
            &dimes_c->dcg->default_gdim, &mem_obj->gdim);
	err = dimes_obj_put(mem_obj);
	if (err < 0) {
        dimes_memory_free(&mem_obj->rdma_handle);
        free(mem_obj);
		goto err_out;
	}
    
	return 0;
err_out:
	ERROR_TRACE();
}

int dimes_client_put_sync_all(void)
{
	int err;
	struct dimes_storage_group *p, *t;
	list_for_each_entry_safe(p, t, &dimes_c->storage, struct dimes_storage_group, entry)
	{
        err = storage_free_group(p);
        if (err < 0) return err;
    }

	return 0;
}

int dimes_client_put_set_group(const char *group_name, int step)
{
    update_current_group_name(group_name);
	return 0;
}

int dimes_client_put_unset_group()
{
    update_current_group_name(default_group_name);
	return 0;
}

int dimes_client_put_sync_group(const char *group_name, int step)
{
    int err;
    struct dimes_storage_group *p;
    p = storage_lookup_group(group_name);
    if (p == NULL) return 0;

    err = storage_free_group(p);
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
