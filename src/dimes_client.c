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

#ifdef DS_HAVE_DIMES_SHMEM
/* shm_* stuff, and mmap() */
#include <sys/mman.h>
#include <sys/types.h>
/* exit() etc */
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#endif


#include "dimes_interface.h"
#include "dimes_client.h"
#include "dimes_data.h"

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
#ifdef DS_HAVE_DIMES_SHMEM
static void init_node_peer_tab();
static int build_node_local_obj_index(void *restart_buf);
#endif

#ifdef DS_HAVE_DIMES_SHMEM
// Compares two (struct node_id) elements by their dart id values.
// This function is used and called by qsort. 
int compare_peer_by_dart_id(const void *a, const void *b)
{
    return (*(const struct node_id**)a)->ptlmap.id -
           (*(const struct node_id**)b)->ptlmap.id;
}
#endif

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

#ifdef DS_HAVE_DIMES_SHMEM
// Check if peer runs on the same compute node as myself.
static int is_peer_on_same_node(struct node_id *peer)
{
    int i;
    for (i = 0; i < dimes_c->num_local_peer; i++) {
        if (dimes_c->local_peer_tab[i]->ptlmap.id == peer->ptlmap.id) return 1;
    }
    return 0;
}

// Lookup dart id by node_rank. Valid value for node_rank 
// is 0~(num of peers on local node - 1).
static int get_peer_dart_id_on_same_node(int node_rank)
{
    if (node_rank < 0 || node_rank >= dimes_c->num_local_peer) return -1;
    else return dimes_c->local_peer_tab[node_rank]->ptlmap.id;
}

// Lookup pointer to node_id object by node_rank.
static struct node_id* get_peer_on_same_node(int node_rank)
{
    if (node_rank < 0 || node_rank >= dimes_c->num_local_peer) return NULL;
    else return dimes_c->local_peer_tab[node_rank];
}
#endif

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

#ifdef DS_HAVE_DIMES_SHMEM
static struct dimes_storage_group* node_local_obj_index_add_group(const char *group_name)
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
    list_add(&p->entry, &dimes_c->node_local_obj_index);
    return p;
 err_out_free:
    if (p) free(p);
    return NULL;    
}

static struct dimes_storage_group* node_local_obj_index_lookup_group(const char *group_name)
{
    struct dimes_storage_group *p;
    list_for_each_entry(p, &dimes_c->node_local_obj_index, struct dimes_storage_group, entry)
    {
        if (p->name == NULL) continue;
        if (0 == strcmp(p->name, group_name)) {
            return p;
        }
    }

    return NULL;
}

static int node_local_obj_index_free_group(struct dimes_storage_group *group)
{
    int i;
    struct dimes_memory_obj *p, *t;
    for (i = 0; i < dimes_c->dcg->max_versions; i++) {
        list_for_each_entry_safe(p, t, &group->version_tab[i],
                    struct dimes_memory_obj, entry) {
            list_del(&p->entry);
            free(p);
        }
    }

    list_del(&group->entry);
    if (group->version_tab) free(group->version_tab);
    free(group);

    return 0;
}

static void node_local_obj_index_init()
{
    INIT_LIST_HEAD(&dimes_c->node_local_obj_index);
}

static void node_local_obj_index_free()
{
    struct dimes_storage_group *p, *t;
    list_for_each_entry_safe(p, t, &dimes_c->node_local_obj_index, struct dimes_storage_group, entry)
    {
        node_local_obj_index_free_group(p);
    }
}
#endif


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

#ifdef DS_HAVE_DIMES_SHMEM
static int get_next_shmem_obj_id()
{
    static int shmem_obj_id_seed = 0;
    return (DIMES_CID * 1000 + shmem_obj_id_seed++); 
}

static struct shared_memory_obj* create_shmem_obj(const char* shmem_obj_path,
    const size_t shmem_obj_size, int shmem_obj_id, int owner_node_rank)
{
    int seg_fd;
    void *seg_ptr = NULL;

    seg_fd = shm_open(shmem_obj_path, O_CREAT | O_EXCL | O_RDWR,
                    S_IRWXU | S_IRWXG);
    if (seg_fd < 0) { 
        uloga("%s(): failed with shm_obj_path %s\n",
            __func__, shmem_obj_path);
        perror("shm_open() failed");
        goto err_out;
    }    

    if (ftruncate(seg_fd, shmem_obj_size) < 0) { 
        perror("ftruncate() failed");
        goto err_out;
    }    

    seg_ptr = mmap(NULL, shmem_obj_size, PROT_READ | PROT_WRITE, MAP_SHARED,
                    seg_fd, 0);
    if (seg_ptr == NULL) {
        perror("mmap() failed");
        goto err_out;
    }    

    // debug print
    //printf("%s: #%d create '%s' "
    //    "id= %d size= %u bytes ptr= %p\n", __func__, DIMES_CID, 
    //    shmem_obj_path, shmem_obj_id, shmem_obj_size, seg_ptr);

    struct shared_memory_obj *shmem_obj = (struct shared_memory_obj*)
                                            malloc(sizeof(*shmem_obj));
    shmem_obj->id = shmem_obj_id;
    strcpy(shmem_obj->path, shmem_obj_path);
    shmem_obj->size = shmem_obj_size;
    shmem_obj->ptr = seg_ptr;
    shmem_obj->fd = seg_fd;
    shmem_obj->owner_node_rank = owner_node_rank;

    return shmem_obj;
 err_out:
    uloga("%s: ERROR failed to create shared memory obj %s.\n",
        __func__, shmem_obj_path);
    return NULL;
}

static struct shared_memory_obj* open_shmem_obj(const char* shmem_obj_path,
    const size_t shmem_obj_size, int shmem_obj_id, int owner_node_rank)
{
    int seg_fd;
    void *seg_ptr = NULL;

    // open/mmap
    seg_fd = shm_open(shmem_obj_path, O_RDWR, S_IRWXU | S_IRWXG);
    if (seg_fd < 0 ) {
        perror("shm_open() failed");
        goto err_out;
    }

    seg_ptr = mmap(NULL, shmem_obj_size, PROT_READ | PROT_WRITE, MAP_SHARED,
                    seg_fd, 0);
    if (seg_ptr == NULL) {
        perror("mmap() failed");
        goto err_out;
    }

    // debug print
    //printf("%s: #%d open '%s' "
    //    "id= %d size= %u bytes ptr= %p\n", __func__, DIMES_CID,
    //    shmem_obj_path, shmem_obj_id, shmem_obj_size, seg_ptr);
    
    struct shared_memory_obj *shmem_obj = (struct shared_memory_obj*)
                                            malloc(sizeof(*shmem_obj));
    shmem_obj->id = shmem_obj_id;
    strcpy(shmem_obj->path, shmem_obj_path);
    shmem_obj->size = shmem_obj_size;
    shmem_obj->ptr = seg_ptr;
    shmem_obj->fd = seg_fd;
    shmem_obj->owner_node_rank = owner_node_rank;
    
    return shmem_obj;
 err_out:
    uloga("%s: ERROR failed to open shared memory obj %s.\n",
        __func__, shmem_obj_path);
    return NULL;
}

static void unmap_shmem_obj(struct shared_memory_obj *shmem_obj)
{
    // unmap
    if (munmap(shmem_obj->ptr, shmem_obj->size) < 0) {
        perror("munmap() failed");
    }
}

static void remove_shmem_obj(struct shared_memory_obj *shmem_obj, unsigned int unlink)
{
    // unlink
    if (dimes_c->node_rank == shmem_obj->owner_node_rank && unlink) {
        if (shm_unlink(shmem_obj->path) != 0) {
            perror("shm_unlink() failed");
        }
        // debug print
        //printf("%s(): #%d unlink shmem_obj %s\n", __func__,
        //    DIMES_CID, shmem_obj->path);
    }

    shmem_obj->ptr = NULL;
    shmem_obj->size = 0;

    free(shmem_obj);
}

static struct shared_memory_obj* find_shmem_obj(int id)
{
    struct shared_memory_obj* shmem_obj = NULL;
    list_for_each_entry(shmem_obj, &dimes_c->shmem_obj_list,
            struct shared_memory_obj, entry)
    {
        if (shmem_obj->id == id) {
            return shmem_obj;
        }
    }

    return NULL;
}

static struct shared_memory_obj* find_my_shmem_obj()
{
    struct shared_memory_obj* shmem_obj = NULL;
    list_for_each_entry(shmem_obj, &dimes_c->shmem_obj_list,
            struct shared_memory_obj, entry)
    {
        if (shmem_obj->owner_node_rank == dimes_c->node_rank) {
            return shmem_obj;
        }
    }

    return NULL;
}
#endif // end of #ifdef DS_HAVE_DIMES_SHMEM

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
#ifdef DS_HAVE_DIMES_SHMEM
    if (rdma_hndl->mem_type == dart_memory_shmem_rdma) use_rdma_memory = 1;
#endif    
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
                uloga("%s(): ERROR dimes_buffer_alloc() failed\n", __func__);
                goto err_out;
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
#ifdef DS_HAVE_DIMES_SHMEM
    case dart_memory_shmem_non_rdma:
        if (options.enable_shmem_buffer) {
            dimes_buffer_alloc(size, &buf);
            if (!buf) {
                uloga("%s(): ERROR dimes_buffer_alloc() failed\n", __func__);
                return -1;
            }

            rdma_hndl->base_addr = buf;
            rdma_hndl->size = size;
        } else {
            uloga("%s(): enable_shmem_buffer= %d\n", __func__, options.enable_shmem_buffer);
            return -1;
        }
        break;
    case dart_memory_shmem_rdma:
        if (options.enable_shmem_buffer) {
            dimes_buffer_alloc(size, &buf);
            if (!buf) {
                uloga("%s(): ERROR dimes_buffer_alloc() failed\n", __func__);
                return -1;
            }

            err = dart_rdma_register_mem(rdma_hndl, buf, size);
            if (err < 0) {
                uloga("%s(): ERROR dart_rdma_register_mem() failed\n", __func__);
                return -1;
            }
        } else {
            uloga("%s(): enable_shmem_buffer= %d\n", __func__, options.enable_shmem_buffer);
            return -1;
        }
        break;
#endif /* DS_HAVE_DIMES_SHMEM */
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
#ifdef DS_HAVE_DIMES_SHMEM
    if (rdma_hndl->mem_type == dart_memory_shmem_rdma) use_rdma_memory = 1;
#endif
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
#ifdef DS_HAVE_DIMES_SHMEM
    case dart_memory_shmem_non_rdma:
        if (options.enable_shmem_buffer) {
            dimes_buffer_free(rdma_hndl->base_addr);
        } else {
            uloga("%s(): ERROR enable_shmem_buffer= %d\n", __func__, options.enable_shmem_buffer);
            return -1;
        }
        break;
    case dart_memory_shmem_rdma:
        if (options.enable_shmem_buffer) {
            err = dart_rdma_deregister_mem(rdma_hndl);
            if (err < 0) {
                uloga("%s(): dart_rdma_deregister_mem failed\n", __func__);
                return -1;
            }
    
            dimes_buffer_free(rdma_hndl->base_addr);        
        } else {
            uloga("%s(): ERROR enable_shmem_buffer= %d\n", __func__, options.enable_shmem_buffer);
            return -1;
        }
        break;
#endif /* DS_HAVE_DIMES_SHMEM */
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
#ifdef HAVE_UGNI
        if (fetch->read_tran->remote_peer->ptlmap.appid != dimes_c->dcg->dc->self->ptlmap.appid)
        { 
            dart_rdma_delete_remote_peer(fetch->read_tran->remote_peer);
        }
#endif
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

#ifdef HAVE_UGNI
    if (hdr->ptlmap.appid != dimes_c->dcg->dc->self->ptlmap.appid) {
        peer = dart_rdma_create_remote_peer(&hdr->ptlmap);
    } else {
        peer = dc_get_peer(DART_CLIENT_PTR, hdr->odsc.owner);
    }
#else
    // Lookup the owner peer of the remote data object
    peer = dc_get_peer(DART_CLIENT_PTR, hdr->odsc.owner);
#endif
    // Creat read transaction
    dart_rdma_create_read_tran(peer, &fetch->read_tran);

    // Set source and destination object descriptors
    fetch->remote_obj_id = hdr->obj_id;
    fetch->src_odsc = hdr->odsc;
    fetch->dst_odsc = *odsc;
#ifdef DS_HAVE_DIMES_SHMEM
    if (options.enable_shmem_buffer) {
        fetch->src_shmem_desc = hdr->shmem_desc;
    }
#endif

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
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, &mem_obj->gdim);
	num_dht_nodes = ssd_hash(ssd, &mem_obj->obj_desc.bb, dht_nodes);
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
#ifdef DS_HAVE_DIMES_SHMEM
        hdr->has_shmem_data = 0;
        if (options.enable_shmem_buffer) {
            hdr->has_shmem_data = 1; 
            hdr->shmem_desc = mem_obj->shmem_desc;
        }
#endif
	
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

static int matrix_rdma_copy(struct matrix_d *a, struct matrix_d *b, int tran_id)
{
    uint64_t src_offset = 0;
    uint64_t dst_offset = 0;
    uint64_t bytes = 0;
    int err = -ENOMEM;

    uint64_t a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
    uint64_t aloc=0, aloc1=0, aloc2=0, aloc3=0, aloc4=0, aloc5=0, aloc6=0, aloc7=0, aloc8=0, aloc9=0;
    uint64_t b0, b1, b2, b3, b4, b5, b6, b7, b8, b9;
    uint64_t bloc=0, bloc1=0, bloc2=0, bloc3=0, bloc4=0, bloc5=0, bloc6=0, bloc7=0, bloc8=0, bloc9=0;

    uint64_t n = 0;
    n = a->mat_view.ub[0] - a->mat_view.lb[0] + 1;
    a0 = a->mat_view.lb[0];
    b0 = b->mat_view.lb[0];

    switch(a->num_dims){
        case(1):
                        goto dim1;
                        break;
                case(2):
                        goto dim2;
                        break;
                case(3):
                        goto dim3;
                        break;
                case(4):
                        goto dim4;
                        break;
                case(5):
                        goto dim5;
                        break;
                case(6):
                        goto dim6;
                        break;
                case(7):
                        goto dim7;
                        break;
                case(8):
                        goto dim8;
                        break;
                case(9):
                        goto dim9;
                        break;
                case(10):
                        goto dim10;
                        break;
                default:
                        break;
    }

dim10:    for(a9 = a->mat_view.lb[9], b9 = b->mat_view.lb[9];     //TODO-Q
        a9 <= a->mat_view.ub[9]; a9++, b9++){
        aloc9 = a9 * a->dist[8];
        bloc9 = a9 * b->dist[8];
dim9:    for(a8 = a->mat_view.lb[8], b8 = b->mat_view.lb[8];     //TODO-Q
        a8 <= a->mat_view.ub[8]; a8++, b8++){
        aloc8 = (aloc9 + a8) * a->dist[7];
        bloc8 = (bloc9 + b8) * b->dist[7];
dim8:    for(a7 = a->mat_view.lb[7], b7 = b->mat_view.lb[7];     //TODO-Q
        a7 <= a->mat_view.ub[7]; a7++, b7++){
        aloc7 = (aloc8 + a7) * a->dist[6];
        bloc7 = (bloc8 + b7) * b->dist[6];
dim7:    for(a6 = a->mat_view.lb[6], b6 = b->mat_view.lb[6];     //TODO-Q
        a6 <= a->mat_view.ub[6]; a6++, b6++){
        aloc6 = (aloc7 + a6) * a->dist[5];
        bloc6 = (bloc7 + b6) * b->dist[5];
dim6:    for(a5 = a->mat_view.lb[5], b5 = b->mat_view.lb[5];     //TODO-Q
        a5 <= a->mat_view.ub[5]; a5++, b5++){
        aloc5 = (aloc6 + a5) * a->dist[4];
        bloc5 = (bloc6 + b5) * b->dist[4];
dim5:    for(a4 = a->mat_view.lb[4], b4 = b->mat_view.lb[4];
        a4 <= a->mat_view.ub[4]; a4++, b4++){
        aloc4 = (aloc5 + a4) * a->dist[3];
        bloc4 = (bloc5 + b4) * b->dist[3];
dim4:    for(a3 = a->mat_view.lb[3], b3 = b->mat_view.lb[3];
        a3 <= a->mat_view.ub[3]; a3++, b3++){
        aloc3 = (aloc4 + a3) * a->dist[2];
        bloc3 = (bloc4 + b3) * b->dist[2]; 
dim3:        for(a2 = a->mat_view.lb[2], b2 = b->mat_view.lb[2];
            a2 <= a->mat_view.ub[2]; a2++, b2++){
            aloc2 = (aloc3 + a2) * a->dist[1];
            bloc2 = (bloc3 + b2) * b->dist[1];
dim2:            for(a1 = a->mat_view.lb[1], b1 = b->mat_view.lb[1];
                a1 <= a->mat_view.ub[1]; a1++, b1++){
                aloc1 = (aloc2 + a1) * a->dist[0];
                bloc1 = (bloc2 + b1) * b->dist[0];
               /* for(a0 = a->mat_view.lb[0], b0 = b->mat_view.lb[0];
                    a0 <= a->mat_view.ub[0]; a0++, b0++){
                    aloc = aloc1 + a0;
                    bloc = bloc1 + b0;
               */
dim1:           aloc = aloc1 + a0;
                bloc = bloc1 + b0;
                src_offset = bloc * a->size_elem;
                dst_offset = aloc * a->size_elem;
                bytes = n * a->size_elem;
                err = dart_rdma_schedule_read(tran_id, src_offset, dst_offset, bytes);
                if (err < 0)
                        goto err_out;
        if(a->num_dims == 1)    return 0;
    }
    if(a->num_dims == 2)    return 0;
    }
    if(a->num_dims == 3)    return 0;
    }
    if(a->num_dims == 4)    return 0;
    }
    if(a->num_dims == 5)    return 0;
    }
    if(a->num_dims == 6)    return 0;
    }
    if(a->num_dims == 7)    return 0;
    }
    if(a->num_dims == 8)    return 0;
    }
    if(a->num_dims == 9)    return 0;
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
#ifdef DS_HAVE_DIMES_SHMEM
        if (options.enable_shmem_buffer &&
            is_peer_on_same_node(fetch->read_tran->remote_peer)) {
            continue;
        }
#endif
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
#ifdef DS_HAVE_DIMES_SHMEM 
        if (options.enable_shmem_buffer &&
                 is_peer_on_same_node(fetch->read_tran->remote_peer)) {
            //printf("%s(): peer %d fetch from peer %d on local node\n", __func__,
            //    DIMES_CID, fetch->read_tran->remote_peer->ptlmap.id);

            // Data on the same node, and is in the shared memory segment
            struct shared_memory_obj *shmem_obj =
                        find_shmem_obj(fetch->src_shmem_desc.shmem_obj_id);
            if (!shmem_obj) {
                uloga("%s(): failed to find shmem object obj_id %d\n",
                    __func__, fetch->src_shmem_desc.shmem_obj_id);
                goto err_out;
            }

            // Update source memory region
            fetch->read_tran->src.base_addr =
                (shmem_obj->ptr+fetch->src_shmem_desc.offset);
            fetch->read_tran->src.size = fetch->src_shmem_desc.size;
    
            // Allocate receive buffer, schedule reads, perform reads
            dimes_memory_alloc(&fetch->read_tran->dst,
                                obj_data_size(&fetch->dst_odsc),
                                dart_memory_non_rdma);
            schedule_rdma_reads(fetch->read_tran->tran_id,
                                &fetch->src_odsc, &fetch->dst_odsc);
            dart_rdma_perform_reads_local(fetch->read_tran->tran_id);
            // Copy fetched data
            obj_assemble(fetch, qte->data_ref);
            dimes_memory_free(&fetch->read_tran->dst);
            continue;
        }
#endif /* DS_HAVE_DIMES_SHMEM */

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
    struct sspace *ssd = lookup_sspace_dimes(dimes_c, &od->gdim);
	num_dht_nodes = ssd_hash(ssd, &od->obj_desc.bb, dht_nodes);
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

#ifdef DS_HAVE_DIMES_SHMEM
    options.enable_shmem_buffer = 0;
    options.enable_get_local = 0;
#endif
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

#ifdef DS_HAVE_DIMES_SHMEM
    init_node_peer_tab();
#endif

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
    free_gdim_list(&dimes_c->gdim_list);
    free_sspace_dimes(dimes_c);

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
#ifdef DS_HAVE_DIMES_SHMEM
    if (options.enable_shmem_buffer) {
        err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                            dart_memory_shmem_rdma);
    } else {
        err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                            dart_memory_rdma);
    }
#else
    err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                             dart_memory_rdma);
#endif    
    if (err < 0) {
        free(mem_obj);
        goto err_out;
    }

    // Copy user data
    memcpy((void*)mem_obj->rdma_handle.base_addr, data, data_size);
#ifdef DS_HAVE_DIMES_SHMEM
    // Set shmem information
    if (options.enable_shmem_buffer) {
        // TODO: hardcoded for now, and assume there is 
        // only one shared mem obj that belongs to me ...
        struct shared_memory_obj *shmem_obj = find_my_shmem_obj();
        if (shmem_obj) {
            mem_obj->shmem_desc.size = data_size;
            mem_obj->shmem_desc.offset =
                mem_obj->rdma_handle.base_addr -
                (uint64_t)shmem_obj->ptr;
            mem_obj->shmem_desc.shmem_obj_id = shmem_obj->id;
            mem_obj->shmem_desc.owner_node_rank = shmem_obj->owner_node_rank;
        } else {
            uloga("%s(): ERROR find_my_shmem_obj() failed\n", __func__);
        }
    }
#endif /* DS_HAVE_DIMES_SHMEM */

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

#ifdef DS_HAVE_DIMES_SHMEM
    if (options.enable_shmem_buffer && options.enable_get_local) {
        p = node_local_obj_index_lookup_group(group_name);
        if (p == NULL) return 0;
        err = node_local_obj_index_free_group(p);
        if (err < 0) return err; 
    }
#endif

    return 0;
}

#ifdef DS_HAVE_DIMES_SHMEM
static int local_search_mem_obj_list(struct list_head *mem_obj_list,
        struct query_tran_entry_d *qte)
{
    struct dimes_memory_obj *mem_obj;
    list_for_each_entry(mem_obj, mem_obj_list, struct dimes_memory_obj, entry)
    {
        if (!obj_desc_equals_intersect(&mem_obj->obj_desc, &qte->q_obj))
            continue;

        struct obj_descriptor obj_desc = mem_obj->obj_desc;
        // Calculate the bbox intersection
        bbox_intersect(&qte->q_obj.bb, &mem_obj->obj_desc.bb, &obj_desc.bb);
        if (qt_find_obj_d(qte, &obj_desc))
            continue;
 
        // Add to qte->fetch_list
        struct node_id *peer =
                    get_peer_on_same_node(mem_obj->shmem_desc.owner_node_rank);
        if (!peer) {
            uloga("%s(): ERROR data is not on local node.\n", __func__);
            continue;
        }
#ifdef HAVE_UGNI
        if (peer->ptlmap.appid != dimes_c->dcg->dc->self->ptlmap.appid) {
            peer = dart_rdma_create_remote_peer(&peer->ptlmap);
        }
#endif
        struct fetch_entry *fetch = (struct fetch_entry*)malloc(sizeof(*fetch));
        dart_rdma_create_read_tran(peer, &fetch->read_tran);
        fetch->remote_obj_id = mem_obj->obj_id;
        fetch->src_odsc = mem_obj->obj_desc;
        fetch->dst_odsc = obj_desc;
        fetch->src_shmem_desc = mem_obj->shmem_desc;
        list_add(&fetch->entry, &qte->fetch_list);
        qte->num_fetch++; 
    }
    return 0;
}

static int local_search_group_list(struct list_head *group_list, 
    struct query_tran_entry_d *qte)
{
    struct dimes_storage_group *p;
    list_for_each_entry(p, group_list, struct dimes_storage_group, entry) {
        int tab_idx = qte->q_obj.version % dimes_c->dcg->max_versions;
        local_search_mem_obj_list(&p->version_tab[tab_idx], qte); 
    }

    return 0; 
}

static int dimes_obj_get_local(struct obj_data *od)
{
    struct query_tran_entry_d *qte;
    int err = -ENOMEM;
#ifdef TIMING_PERF
    double tm_st, tm_end;
#endif

    qte = qte_alloc_d(od);
    if (!qte)
        goto err_out;
    qt_add_d(&dimes_c->qt, qte);

    // No need to interact with dht nodes, but set the values.
    qte->qh->qh_num_peer = 0;

#ifdef TIMING_PERF
    tm_st = timer_read(&tm_perf);
#endif
    // Locate data objects
    local_search_group_list(&dimes_c->storage, qte);
    local_search_group_list(&dimes_c->node_local_obj_index, qte);
    if (qte->num_fetch == 0) {
        uloga("%s(): ERROR qte->num_fetch= %d for obj '%s' version= %d\n",
            __func__, qte->num_fetch, qte->q_obj.name, qte->q_obj.version);
        goto err_qt_free;
    } 
    qte->f_locate_data_complete = 1;
#ifdef TIMING_PERF
    tm_end = timer_read(&tm_perf);
    uloga("TIMING_PERF locate_data ts %d peer %d time %lf %s\n",
        od->obj_desc.version, DIMES_RANK, tm_end-tm_st, log_header);
    tm_st = tm_end;
#endif

    // Fetch data
    err = dimes_fetch_data(qte);
    if (err < 0) {
        goto err_data_free;
    }

    if (!qte->f_complete) {
        err = -ENODATA;
        goto out_no_data;
    }
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

int dimes_client_shmem_get_local(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data)
{
    if (!options.enable_shmem_buffer || !options.enable_get_local) {
        uloga("%s(): ERROR DIMES SHMEM runtime is not initialized.\n", __func__);
        return -1;
    }

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
    err = dimes_obj_get_local(od);
    obj_data_free(od);
    if (err < 0 && err != -EAGAIN) {
        goto err_out;
    }

    return err;
err_out:
    ERROR_TRACE();
}

int dimes_client_shmem_put_local(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        void *data)
{
    if (!options.enable_shmem_buffer || !options.enable_get_local) {
        uloga("%s(): ERROR DIMES SHMEM runtime is not initialized.\n", __func__);
        return -1;
    }

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
    struct dimes_memory_obj *mem_obj = (struct dimes_memory_obj*)
                                        malloc(sizeof(*mem_obj));
    mem_obj->obj_id.dart_id = DIMES_CID;
    mem_obj->obj_id.local_obj_index = next_local_obj_index();
    mem_obj->obj_desc = odsc;
    err = dimes_memory_alloc(&mem_obj->rdma_handle, data_size,
                            dart_memory_shmem_non_rdma);
    if (err < 0) {
        free(mem_obj);
        goto err_out;
    }
    // Copy user data
    memcpy((void*)mem_obj->rdma_handle.base_addr, data, data_size);
    // TODO: currently hardcoded. Assume there is only one shared memory
    // object that belongs to me.
    struct shared_memory_obj *shmem_obj = find_my_shmem_obj();
    if (shmem_obj) {
        mem_obj->shmem_desc.size = data_size;
        mem_obj->shmem_desc.offset =
            mem_obj->rdma_handle.base_addr -
            (uint64_t)shmem_obj->ptr;
        mem_obj->shmem_desc.shmem_obj_id = shmem_obj->id;
        mem_obj->shmem_desc.owner_node_rank = shmem_obj->owner_node_rank;
    } else {
        uloga("%s(): ERROR find_my_shmem_obj() failed\n", __func__);
        dimes_memory_free(&mem_obj->rdma_handle);
        free(mem_obj);
        err = -ENOMEM;
        goto err_out;
    }

    set_global_dimension(&dimes_c->gdim_list, var_name, 
            &dimes_c->dcg->default_gdim, &mem_obj->gdim);
    storage_add_obj(mem_obj);

    return 0;
 err_out:
    ERROR_TRACE(); 
}

static void init_node_peer_tab()
{
    // build local peer tab
    rpc_server_find_local_peers(RPC_SERVER_PTR, dimes_c->local_peer_tab,
        &dimes_c->num_local_peer,
        MAX_NUM_PEER_PER_NODE);

    // sort the node-local peers by dart id 
    qsort(dimes_c->local_peer_tab, dimes_c->num_local_peer,
        sizeof(struct node_id*), compare_peer_by_dart_id);

    // find the node-local master peer (which has the smallest dart id)
    dimes_c->node_master_dart_id = dimes_c->local_peer_tab[0]->ptlmap.id;
    dimes_c->node_id = rpc_server_get_nid(RPC_SERVER_PTR);

    // set node rank value
    int i;
    for (i = 0; i < dimes_c->num_local_peer; i++) {
        if (dimes_c->local_peer_tab[i]->ptlmap.id == DIMES_CID) {
            dimes_c->node_rank = i;
            break;
        }
    }
}

static int init_node_mpi_comm(void *comm)
{
    int ret;
    MPI_Comm *mpi_comm = (MPI_Comm*)comm;
    MPI_Barrier(*mpi_comm);
   
    // create node-local mpi communicator for inter-process communication
    int node_color = dimes_c->node_master_dart_id; //TODO: this is unsafe...
    ret = MPI_Comm_split(*mpi_comm, node_color,
        dimes_c->node_rank, &dimes_c->node_mpi_comm);
    if (ret != MPI_SUCCESS) {
        uloga("%s(): mpi_comm_split() failed with %d\n", __func__, ret);
        return -1;
    }                
    MPI_Barrier(dimes_c->node_mpi_comm);
    int node_mpi_rank;
    MPI_Comm_rank(dimes_c->node_mpi_comm, &node_mpi_rank);
    if (node_mpi_rank != dimes_c->node_rank) {
        uloga("%s(): ERROR dimes_c->node_rank is %d but node_mpi_rank is %d\n",
            __func__, dimes_c->node_rank, node_mpi_rank);
        return -1;
    }
    return 0;
}

static int gather_node_shmem_obj(struct shared_memory_obj *shmem_obj)
{
    // gather/scatter node-local shared memory segments information
    // from/to all node-local clients
    int num_bytes = sizeof(struct shared_memory_obj);
    struct shared_memory_obj *shmem_obj_tab = (struct shared_memory_obj*)
            malloc(num_bytes*dimes_c->num_local_peer);
    if (!shmem_obj_tab) {
        uloga("%s: ERROR malloc() failed.\n", __func__);
        return -1;
    }

    MPI_Allgather(shmem_obj, num_bytes, MPI_BYTE, shmem_obj_tab,
                    num_bytes, MPI_BYTE, dimes_c->node_mpi_comm);
    int i;
    for (i = 0; i < dimes_c->num_local_peer; i++) {
        // debug print
        //printf("%s: #%d shmem obj id %d owner_node_rank %d path %s size %u\n",
        //    __func__, DIMES_CID, shmem_obj_tab[i].id,
        //    shmem_obj_tab[i].owner_node_rank, shmem_obj_tab[i].path, shmem_obj_tab[i].size);
        if (shmem_obj_tab[i].owner_node_rank != dimes_c->node_rank) {
            struct shared_memory_obj *o = open_shmem_obj(shmem_obj_tab[i].path,
                        shmem_obj_tab[i].size, shmem_obj_tab[i].id,
                        shmem_obj_tab[i].owner_node_rank);
            if (!o) {
                uloga("%s: ERROR open_shmem_obj() failed.\n", __func__);
                return -1;
            }
            list_add(&o->entry, &dimes_c->shmem_obj_list);
        }
    }
    free(shmem_obj_tab);
    return 0;
}

int dimes_client_shmem_init(void *comm, size_t shmem_obj_size)
{
    options.enable_shmem_buffer = 1;
    options.enable_get_local = 1;

    INIT_LIST_HEAD(&dimes_c->shmem_obj_list);

    // init node-local mpi communicator
    if (init_node_mpi_comm(comm) < 0) return -1;

    // each client creates a node-local shared memory segment
    char path[SHMEM_OBJ_PATH_MAX_LEN+1];
    sprintf(path, "%s_%d", SHMEM_OBJ_PATH_PREFIX, dimes_c->node_rank);
    struct shared_memory_obj *shmem_obj =
            create_shmem_obj(path, shmem_obj_size, get_next_shmem_obj_id(),
                            dimes_c->node_rank);
    if (!shmem_obj) return -1;
    list_add(&shmem_obj->entry, &dimes_c->shmem_obj_list);

    // gather and open node-local shared memory objects
    gather_node_shmem_obj(shmem_obj);

    // init dimes buffer with my shared memory object
    dimes_buffer_init(shmem_obj->ptr, shmem_obj->size);

    if (options.enable_get_local) {
        node_local_obj_index_init();
    }
    return 0;
}

int dimes_client_shmem_finalize(unsigned int unlink)
{
    if (options.enable_get_local) {
        node_local_obj_index_free();
    }
    dimes_buffer_finalize();

    struct shared_memory_obj *shmem_obj, *temp;
    // unmap
    list_for_each_entry_safe(shmem_obj, temp, &dimes_c->shmem_obj_list,
                             struct shared_memory_obj, entry)
    {
        unmap_shmem_obj(shmem_obj);
    }

    // synchronize across node-local clients
    MPI_Barrier(dimes_c->node_mpi_comm);

    // remove (TODO: add unlink flag)
    list_for_each_entry_safe(shmem_obj, temp, &dimes_c->shmem_obj_list,
                             struct shared_memory_obj, entry)
    {
        list_del(&shmem_obj->entry);
        remove_shmem_obj(shmem_obj, unlink);
    }

    return 0;
}

int dimes_client_shmem_clear_testing()
{
    // clear DIMES storage (similar to dimes_client_free()) 
    free_gdim_list(&dimes_c->gdim_list);
    storage_free();
    dimes_memory_finalize();

    // reset
    storage_init();
    options.rdma_buffer_usage = 0;
}

int dimes_client_shmem_checkpoint()
{
    int err;
    struct dimes_cr_shmem_obj_info *shmem_obj_info =
        (struct dimes_cr_shmem_obj_info*)malloc(sizeof(*shmem_obj_info));
    struct dimes_cr_shmem_obj_info *shmem_obj_info_tab = NULL;
    struct shared_memory_obj *shmem_obj = find_my_shmem_obj();
    if (!shmem_obj) {
        uloga("%s: ERROR failed to find my shared memory obj.\n", __func__);
        goto err_out;
    }
    
    shmem_obj_info->id = shmem_obj->id;
    strcpy(shmem_obj_info->path, shmem_obj->path);
    shmem_obj_info->size = shmem_obj->size;

    size_t bytes = 0;
    // construct DIMES storage restart buf for my shared memory object
    bytes = estimate_storage_restart_buf_size();
    if (bytes == 0) {
        uloga("%s: WARNING storage restart buf size is 0\n");
    }
    shmem_obj_info->storage_restart_buf_size = bytes;
    sprintf(shmem_obj_info->path_storage_restart_buf,
            "%s_storage_restart", shmem_obj_info->path);
    struct shared_memory_obj *temp1 = create_shmem_obj(
                    shmem_obj_info->path_storage_restart_buf,
                    shmem_obj_info->storage_restart_buf_size,
                    get_next_shmem_obj_id(), dimes_c->node_rank);
    if (!temp1) {
        goto err_out_free;
    }
    err = dimes_client_shmem_checkpoint_storage(shmem_obj->id, temp1->ptr); 
    unmap_shmem_obj(temp1);
    remove_shmem_obj(temp1, 0);

    // construct DIMES allocator restart buf for my shared memory object
    bytes = estimate_allocator_restart_buf_size(DIMES_CID);
    if (bytes == 0) {
        uloga("%s: WARNING allocator restart buf size is 0\n");
    }
    shmem_obj_info->allocator_restart_buf_size = bytes;
    sprintf(shmem_obj_info->path_allocator_restart_buf,
            "%s_allocator_restart", shmem_obj_info->path);
    struct shared_memory_obj *temp2 = create_shmem_obj(
                    shmem_obj_info->path_allocator_restart_buf,
                    shmem_obj_info->allocator_restart_buf_size,
                    get_next_shmem_obj_id(), dimes_c->node_rank);
    if (!temp2) {
        goto err_out_free;
    }
    err = dimes_client_shmem_checkpoint_allocator(shmem_obj->id, temp2->ptr);
    unmap_shmem_obj(temp2);
    remove_shmem_obj(temp2, 0);

    // gather intra-node dimes_cr_shmem_obj_info and write to restart buffer 
    int root = 0;
    int tab_entry_size = sizeof(struct dimes_cr_shmem_obj_info);
    int num_shmem_obj = dimes_c->num_local_peer; // TODO:
    size_t tab_size = tab_entry_size * num_shmem_obj; 
    shmem_obj_info_tab = malloc(tab_size); 
    if (!shmem_obj_info_tab) {
        uloga("%s: ERROR malloc() failed.\n", __func__);
        goto err_out_free;
    }

    MPI_Barrier(dimes_c->node_mpi_comm);
    err = MPI_Gather(shmem_obj_info, tab_entry_size, MPI_BYTE,
                shmem_obj_info_tab, tab_entry_size, MPI_BYTE, 
                root, dimes_c->node_mpi_comm);
    if (dimes_c->node_rank == root) {
        bytes = estimate_node_shmem_restart_buf_size();
        char node_shmem_restart_buf_path[SHMEM_OBJ_PATH_MAX_LEN+1];
        sprintf(node_shmem_restart_buf_path, "%s_node_restart",
                SHMEM_OBJ_PATH_PREFIX);
        struct shared_memory_obj *temp3 = create_shmem_obj(
                    node_shmem_restart_buf_path,
                    bytes, get_next_shmem_obj_id(), dimes_c->node_rank);
        if (!temp3) {
            goto err_out_free;
        }
        void *buf = temp3->ptr;
        struct dimes_cr_shmem_info *node_shmem_info = 
                (struct dimes_cr_shmem_info *)buf;
        node_shmem_info->num_shmem_obj = num_shmem_obj;
        buf += sizeof(struct dimes_cr_shmem_info);
        memcpy(buf, shmem_obj_info_tab, tab_size);
        unmap_shmem_obj(temp3);
        remove_shmem_obj(temp3, 0);
        // debug print
        //struct dimes_cr_shmem_obj_info *tab = buf;
        //int i;
        //for (i = 0; i < num_shmem_obj; i++) {
        //    printf("%s: #%d shmem obj path=%s id= %d size= %u "
        //        "storage_restart_buf= %s size= %u "
        //        "allocator_restart_buf= %s size= %u\n", __func__,
        //        DIMES_CID, tab[i].path, tab[i].id, tab[i].size,
        //        tab[i].path_storage_restart_buf,
        //        tab[i].storage_restart_buf_size,
        //        tab[i].path_allocator_restart_buf,
        //        tab[i].allocator_restart_buf_size);
        //}
    }

    free(shmem_obj_info);
    free(shmem_obj_info_tab);

    return 0;
 err_out_free:
    if (shmem_obj_info) free(shmem_obj_info);
    if (shmem_obj_info_tab) free(shmem_obj_info_tab);
 err_out:
    return -1;
}

int dimes_client_shmem_restart(void *comm)
{
    options.enable_shmem_buffer = 1;
    options.enable_get_local = 1;

    if (options.enable_get_local) {
        node_local_obj_index_init();
    }
    INIT_LIST_HEAD(&dimes_c->shmem_obj_list);

    // init node-local mpi communicator
    if (init_node_mpi_comm(comm) < 0) return -1;

    char path[SHMEM_OBJ_PATH_MAX_LEN+1];
    size_t bytes;
    void *buf = NULL;

    // read node restart buf
    sprintf(path, "%s_node_restart", SHMEM_OBJ_PATH_PREFIX);
    bytes = estimate_node_shmem_restart_buf_size();
    struct shared_memory_obj *o1= open_shmem_obj(path,
                bytes, get_next_shmem_obj_id(), 0 /* hard coded as 0*/);
    if (!o1) {
        goto err_out_free;
    }
    buf = o1->ptr; 
    struct dimes_cr_shmem_info *shmem_info = buf;
    if (shmem_info->num_shmem_obj != dimes_c->num_local_peer) {
        uloga("%s: WARNING shmem_info->num_shmem_obj= %u "
            "but dimes_c->num_local_peer= %d\n",
            __func__, shmem_info->num_shmem_obj, dimes_c->num_local_peer);
    }
    // debug print
    //printf("%s: #%d shmem_info->num_shmem_obj= %d\n",
    //        __func__, DIMES_CID, shmem_info->num_shmem_obj);

    buf += sizeof(struct dimes_cr_shmem_info);
    struct dimes_cr_shmem_obj_info *shmem_obj_info = NULL;
    struct dimes_cr_shmem_obj_info *shmem_obj_info_tab = buf;
    // debug print
    //int i;
    //for (i = 0; i < shmem_info->num_shmem_obj; i++) {
    //    printf("%s: #%d shmem obj path= %s id= %d size= %u "
    //        "storage_restart_buf= %s size= %u "
    //        "allocator_restart_buf= %s size= %u\n", __func__,
    //        DIMES_CID, tab[i].path, tab[i].id, tab[i].size,
    //        tab[i].path_storage_restart_buf,
    //        tab[i].storage_restart_buf_size,
    //        tab[i].path_allocator_restart_buf,
    //        tab[i].allocator_restart_buf_size);
    // }

    // assign shmem obj to node peer
    if (dimes_c->node_rank < shmem_info->num_shmem_obj)
        shmem_obj_info = &shmem_obj_info_tab[dimes_c->node_rank];
    else {
        uloga("%s: ERROR dimes_c->node_rank= %d num_shmem_obj= %d\n",
            __func__, dimes_c->node_rank, shmem_info->num_shmem_obj);
        goto err_out_free;
    }
    // debug print
    //printf("%s: #%d to restart shmem obj path= %s id= %d size= %u\n",
    //    __func__, DIMES_CID, shmem_obj_info->path,
    //    shmem_obj_info->id, shmem_obj_info->size);

    // each client opens a node-local shared memory segment
    struct shared_memory_obj *shmem_obj = open_shmem_obj(shmem_obj_info->path,
                shmem_obj_info->size, shmem_obj_info->id, dimes_c->node_rank); 
    if (!shmem_obj) {
        goto err_out_free;
    }
    list_add(&shmem_obj->entry, &dimes_c->shmem_obj_list);
    
    // gather and open node-local shared memory objects
    gather_node_shmem_obj(shmem_obj);

    // init dimes buffer with my shared memory object
    dimes_buffer_init(shmem_obj->ptr, shmem_obj->size);

    // read allocator restart buf
    sprintf(path, "%s", shmem_obj_info->path_allocator_restart_buf);
    bytes = shmem_obj_info->allocator_restart_buf_size;
    struct shared_memory_obj *o2= open_shmem_obj(path,
                bytes, get_next_shmem_obj_id(), dimes_c->node_rank);
    if (!o2) {
        goto err_out_free;
    }
    dimes_client_shmem_restart_allocator(o2->ptr, DIMES_CID);

    // read storage restart buf
    sprintf(path, "%s", shmem_obj_info->path_storage_restart_buf);
    bytes = shmem_obj_info->storage_restart_buf_size;
    struct shared_memory_obj *o3= open_shmem_obj(path,
                bytes, get_next_shmem_obj_id(), dimes_c->node_rank);
    if (!o3) {
        goto err_out_free;
    }
    dimes_client_shmem_restart_storage(o3->ptr);

    // construct node_local_obj_index
    if (options.enable_get_local) {
        int i;
        for (i = 0; i < shmem_info->num_shmem_obj; i++) {
            if (i != dimes_c->node_rank) {
                sprintf(path, "%s", shmem_obj_info_tab[i].path_storage_restart_buf);
                bytes = shmem_obj_info_tab[i].storage_restart_buf_size;
                struct shared_memory_obj *o4 = open_shmem_obj(path,
                            bytes, get_next_shmem_obj_id(), dimes_c->node_rank);
                if (!o3) {
                    goto err_out_free;
                }
                
                build_node_local_obj_index(o4->ptr); 
                unmap_shmem_obj(o4);
                remove_shmem_obj(o4, 0); 
            }
        }        
    }

    MPI_Barrier(dimes_c->node_mpi_comm);
    unmap_shmem_obj(o1);
    remove_shmem_obj(o1, 1);
    unmap_shmem_obj(o2);
    remove_shmem_obj(o2, 1);
    unmap_shmem_obj(o3);
    remove_shmem_obj(o3, 1);

    return 0;
 err_out_free:
    if (o1) free(o1);
    if (o2) free(o2);
    if (o3) free(o3);
    return -1; 
}

int dimes_client_shmem_update_server_state()
{
    int dht_update_stat[NUM_SERVER];
    struct dht_entry *dht_nodes[NUM_SERVER];
    struct hdr_dimes_put *hdr;
    struct msg_buf *msg;
    struct node_id *peer;
    int i, j, num_dht_nodes;
    int err = -ENOMEM;

    for (i = 0; i < NUM_SERVER; i++)
        dht_update_stat[i] = 0;

    struct dimes_storage_group *p;
    list_for_each_entry(p, &dimes_c->storage, struct dimes_storage_group, entry)
    {
        for (j = 0; j < dimes_c->dcg->max_versions; j++) {
            struct dimes_memory_obj *mem_obj;
            list_for_each_entry(mem_obj, &p->version_tab[j], struct dimes_memory_obj,
                                entry)
            {
                if (mem_obj->rdma_handle.mem_type == dart_memory_shmem_non_rdma)
                    continue;

                struct sspace *ssd = lookup_sspace_dimes(dimes_c, &mem_obj->gdim);
                num_dht_nodes = ssd_hash(ssd, &mem_obj->obj_desc.bb, dht_nodes);
                if (num_dht_nodes <= 0) {
                    uloga("%s(): ERROR ssd_hash() return %d but the value should "
                            "be > 0.\n", __func__, num_dht_nodes);
                }

                for (i = 0; i < num_dht_nodes; i++) {
                    peer = dc_get_peer(DART_CLIENT_PTR, dht_nodes[i]->rank);
                    msg = msg_buf_alloc(RPC_SERVER_PTR, peer, 1);
                    if (!msg)
                        goto err_out;
                    msg->msg_rpc->cmd = dimes_shmem_update_server_msg;
                    msg->msg_rpc->id = DIMES_CID;
                    dart_rdma_set_memregion_to_cmd(&mem_obj->rdma_handle,
                                                    msg->msg_rpc);
                    hdr = (struct hdr_dimes_put*)msg->msg_rpc->pad;
                    hdr->odsc = mem_obj->obj_desc;
                    hdr->obj_id = mem_obj->obj_id;
                    hdr->has_shmem_data = 1;
                    hdr->shmem_desc = mem_obj->shmem_desc;

                    err = rpc_send(RPC_SERVER_PTR, peer, msg);
                    if (err < 0) {
                        free(msg);
                        goto err_out;
                    }

                    // update dht_update_stat
                    dht_update_stat[dht_nodes[i]->rank]++; 
                }

                // update rdma buffer usage 
                options.rdma_buffer_usage += mem_obj->shmem_desc.size;
            }     
        }
    }    

    // debug print
    for (i = 0; i < NUM_SERVER; i++) {
        if (dht_update_stat[i] > 0) {
            printf("%s: #%d update dht node %d with %d mem objs.\n",
                __func__, DIMES_CID, i, dht_update_stat[i]);
        }
    }
   
    return 0;
 err_out:
    ERROR_TRACE(); 
}

int dimes_client_shmem_checkpoint_storage(int shmem_obj_id, void *restart_buf)
{
    struct dimes_cr_storage_info *storage_info;
    struct dimes_cr_group_info *group_info;
    struct dimes_cr_mem_obj_info *obj_info;
    void *buf = restart_buf;
    if (!buf) return -1;

    uint32_t num_group = 0;
    storage_info = buf;
    buf += sizeof(struct dimes_cr_storage_info);

    struct dimes_storage_group *g, *gt;
    struct dimes_memory_obj *o, *ot;
    list_for_each_entry_safe(g, gt, &dimes_c->storage, struct dimes_storage_group, entry)
    {
        num_group++;
        uint32_t num_obj = 0;
        group_info = buf;
        buf += sizeof(struct dimes_cr_group_info);
        int i;
        for (i = 0; i < dimes_c->dcg->max_versions; i++) {
            list_for_each_entry_safe(o, ot, &g->version_tab[i], struct dimes_memory_obj, entry)
            {
                num_obj++;
                obj_info = buf;
                obj_info->obj_desc = o->obj_desc;
                obj_info->gdim = o->gdim;
                obj_info->size = o->shmem_desc.size;
                obj_info->offset = o->shmem_desc.offset;
                obj_info->mem_type = o->rdma_handle.mem_type;
                buf += sizeof(struct dimes_cr_mem_obj_info); 
            } 
        }
        strcpy(group_info->name, g->name);
        group_info->num_obj = num_obj;
    }

    storage_info->num_group = num_group;
    storage_info->shmem_obj_id = shmem_obj_id;
    
    return 0; 
}

static int restart_storage_insert_mem_obj(const char *group_name,
    int shmem_obj_id, struct dimes_cr_mem_obj_info *obj_info)
{
    struct dimes_memory_obj *mem_obj = malloc(sizeof(*mem_obj));
    mem_obj->obj_id.dart_id = DIMES_CID;
    mem_obj->obj_id.local_obj_index = next_local_obj_index();
    mem_obj->obj_desc = obj_info->obj_desc;
    mem_obj->gdim = obj_info->gdim;
    // update owner of data
    mem_obj->obj_desc.owner = DIMES_CID;
    // set mem_obj->shmem_desc 
    mem_obj->shmem_desc.size = obj_info->size; 
    mem_obj->shmem_desc.offset = obj_info->offset;
    mem_obj->shmem_desc.shmem_obj_id = shmem_obj_id;
    mem_obj->shmem_desc.owner_node_rank = dimes_c->node_rank;

    // set mem_obj->rdma_handle
    struct shared_memory_obj *shmem_obj = find_shmem_obj(shmem_obj_id);
    if (!shmem_obj) {
        uloga("%s: ERROR failed to find shmem_obj with id %d\n",
            __func__, shmem_obj_id);
        goto err_out_free;
    }
    uint64_t buf = (uint64_t)shmem_obj->ptr + obj_info->offset; 
    mem_obj->rdma_handle.mem_type = obj_info->mem_type;
    if (mem_obj->rdma_handle.mem_type == dart_memory_shmem_rdma) {
        int err = dart_rdma_register_mem(&mem_obj->rdma_handle, buf,
                    obj_info->size);
        if (err < 0) {
            uloga("%s: ERROR dart_rdma_register_mem() failed.\n", __func__);
            goto err_out_free;
        }
    } else if (mem_obj->rdma_handle.mem_type == dart_memory_shmem_non_rdma) {
        mem_obj->rdma_handle.size = obj_info->size;
        mem_obj->rdma_handle.base_addr = buf;    
    } else { 
        uloga("%s(): ERROR unknonw dart_memory_type %d\n", __func__, mem_obj->rdma_handle.mem_type);
        goto err_out_free;
    }

    // add memory object
    struct dimes_storage_group *p = storage_lookup_group(group_name);
    if (!p) p = storage_add_group(group_name);
    int tab_idx = mem_obj->obj_desc.version % dimes_c->dcg->max_versions;
    mem_obj_list_add(&p->version_tab[tab_idx], mem_obj);

    return 0;
 err_out_free:
    if (mem_obj) free(mem_obj);
    return -1;
}

int dimes_client_shmem_restart_storage(void *restart_buf)
{
    void *buf = restart_buf;
 
    struct dimes_cr_storage_info *storage_info = buf;
    
    buf += sizeof(struct dimes_cr_storage_info);
    int i, j;
    for (i = 0; i < storage_info->num_group; i++) {
        struct dimes_cr_group_info *group_info = buf; 
        buf += sizeof(struct dimes_cr_group_info);
        //printf("%s: #%d group name= %s num_obj= %u\n", __func__, DIMES_CID,
        //    group_info->name, group_info->num_obj);
        for (j = 0; j < group_info->num_obj; j++) {
            struct dimes_cr_mem_obj_info *obj_info = buf;
            // add obj
            restart_storage_insert_mem_obj(group_info->name,
                        storage_info->shmem_obj_id, obj_info);
            buf += sizeof(struct dimes_cr_mem_obj_info);
            //printf("%s: #%d obj name= %s version= %d size= %u offset= %u\n", __func__,
            //    DIMES_CID, obj_info->obj_desc.name, obj_info->obj_desc.version,
            //    obj_info->size, obj_info->offset);
        }
    }        

    return 0;
}

static int node_local_obj_index_insert_mem_obj(const char *group_name,
    int shmem_obj_id, struct dimes_cr_mem_obj_info *obj_info)
{
    struct dimes_memory_obj *mem_obj = malloc(sizeof(*mem_obj));
    struct shared_memory_obj *shmem_obj = find_shmem_obj(shmem_obj_id);
    if (!shmem_obj) {
        uloga("%s: ERROR failed to find shmem_obj with id %d\n",
            __func__, shmem_obj_id);
        goto err_out_free;
    }

    // set mem_obj information
    mem_obj->obj_id.dart_id = DIMES_CID;
    mem_obj->obj_id.local_obj_index = next_local_obj_index();
    mem_obj->obj_desc = obj_info->obj_desc;
    mem_obj->gdim = obj_info->gdim;    
    mem_obj->shmem_desc.size = obj_info->size;
    mem_obj->shmem_desc.offset = obj_info->offset;
    mem_obj->shmem_desc.shmem_obj_id = shmem_obj_id;

    // update owner node rank and dart id of the data object
    mem_obj->shmem_desc.owner_node_rank = shmem_obj->owner_node_rank;
    mem_obj->obj_desc.owner = get_peer_dart_id_on_same_node(shmem_obj->owner_node_rank);

    // TODO: check if the code below works on all systems.
    // set mem_obj->rdma_handle
    uint64_t buf = (uint64_t)shmem_obj->ptr + obj_info->offset;
    mem_obj->rdma_handle.base_addr = buf;
    mem_obj->rdma_handle.size = obj_info->size;

    // add memory object
    struct dimes_storage_group *p = node_local_obj_index_lookup_group(group_name);
    if (!p) {
        p = node_local_obj_index_add_group(group_name);
    }
    int tab_idx = mem_obj->obj_desc.version % dimes_c->dcg->max_versions;
    mem_obj_list_add(&p->version_tab[tab_idx], mem_obj);

    return 0;
 err_out_free:
    if (mem_obj) free(mem_obj);
    return -1;
}

static int build_node_local_obj_index(void *restart_buf) {
    if (!options.enable_get_local) {
        uloga("%s(): ERROR options.enable_get_local is %d\n",
            __func__, options.enable_get_local);
        return -1;
    }

    void *buf = restart_buf;
    struct dimes_cr_storage_info *storage_info = buf;
    buf += sizeof(struct dimes_cr_storage_info);
    int i, j;
    for (i = 0; i < storage_info->num_group; i++) {
        struct dimes_cr_group_info *group_info = buf;
        buf += sizeof(struct dimes_cr_group_info);
        //printf("%s: #%d group name= %s num_obj= %u\n", __func__, DIMES_CID,
        //    group_info->name, group_info->num_obj);
        for (j = 0; j < group_info->num_obj; j++) {
            struct dimes_cr_mem_obj_info *obj_info = buf;
            // add obj information
            node_local_obj_index_insert_mem_obj(group_info->name,
                    storage_info->shmem_obj_id, obj_info);
            buf += sizeof(struct dimes_cr_mem_obj_info);
            //printf("%s: #%d obj name= %s version= %d size= %u offset= %u\n", __func__,
            //    DIMES_CID, obj_info->obj_desc.name, obj_info->obj_desc.version,
            //    obj_info->size, obj_info->offset);
        }
    }

    return 0;    
}

int dimes_client_shmem_reset_server_state(int dart_id)
{
    struct msg_buf *msg;
    struct node_id *peer;
    int err = -ENOMEM;

    peer = dc_get_peer(DART_CLIENT_PTR, dart_id);
    msg = msg_buf_alloc(RPC_SERVER_PTR, peer, 1);
    if (!msg)
        goto err_out;
    msg->msg_rpc->cmd = dimes_shmem_reset_server_msg;
    msg->msg_rpc->id = DIMES_CID;
    err = rpc_send(RPC_SERVER_PTR, peer, msg);
    if (err < 0) {
        free(msg);
        goto err_out;
    }

    printf("%s: #%d reset server #%d\n", __func__, DIMES_CID, dart_id);                                            
    return 0;
 err_out:
    ERROR_TRACE();     
}

uint32_t dimes_client_shmem_get_nid()
{
    return dimes_c->node_id;
}

int dimes_client_shmem_get_node_rank()
{
    return dimes_c->node_rank;
}

MPI_Comm dimes_client_shmem_get_node_mpi_comm()
{
    return dimes_c->node_mpi_comm;
}

size_t estimate_node_shmem_restart_buf_size()
{
    size_t bytes = sizeof(struct dimes_cr_shmem_obj_info) * MAX_NUM_PEER_PER_NODE + sizeof(struct dimes_cr_shmem_info);
    size_t page_size = PAGE_SIZE;
    bytes = (bytes/page_size+1)*page_size;

    //printf("%s: #%d node_shmem_restart_buf num_page= %u size= %u bytes\n",
    //    __func__, DIMES_CID, bytes/page_size, bytes);
    return bytes;
}

size_t estimate_storage_restart_buf_size()
{
    uint32_t num_group = 0, num_obj = 0;
    struct dimes_storage_group *g, *gt;
    struct dimes_memory_obj *o, *ot;
    list_for_each_entry_safe(g, gt, &dimes_c->storage, struct dimes_storage_group, entry)
    {
        num_group++;
        int i;
        for (i = 0; i < dimes_c->dcg->max_versions; i++) {
            list_for_each_entry_safe(o, ot, &g->version_tab[i], struct dimes_memory_obj, entry)
            {
                num_obj++;
            }
        }
    }

    size_t bytes = sizeof(struct dimes_cr_storage_info) +
                    sizeof(struct dimes_cr_group_info) * num_group +
                    sizeof(struct dimes_cr_mem_obj_info) * num_obj;
    // size is a multiple of PAGESIZE
    size_t page_size = PAGE_SIZE;
    bytes = (bytes/page_size+1)*page_size;

    //printf("%s: #%d num_group= %u num_obj= %u storage_restart_buf num_page= %u size= %u bytes\n", __func__, DIMES_CID, num_group, num_obj, bytes/page_size, bytes);
    return bytes; 
}
#endif /* DS_HAVE_DIMES_SHMEM */

#ifdef TIMING_PERF
int common_dimes_set_log_header(const char *str)
{
    strcpy(log_header, str);
    return 0;
}
#endif

#endif // end of #ifdef DS_HAVE_DIMES
