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
#include <stdint.h>

#include "config.h"
#include "debug.h"
#include "ss_data.h"
#include "timer.h"

#ifdef DS_HAVE_DIMES
#include "dimes_data.h"

int equal_dimes_obj_id(const struct dimes_obj_id *oid1, const struct dimes_obj_id *oid2)
{
    return (oid1->dart_id == oid2->dart_id) && (oid1->local_obj_index == oid2->local_obj_index);
}

static struct var_list_node* var_node_lookup(struct list_head *var_list,
                                    const char* var_name)
{
    struct var_list_node *n;
    list_for_each_entry(n, var_list, struct var_list_node, entry)
    {
        if (0 == strcmp(n->name, var_name)) {
            return n;
        }
    }

    // not found, add new list node
    n = calloc(1, sizeof(struct var_list_node));
    strcpy(n->name, var_name); // TODO: here assume destination has large enough buffer size   
    INIT_LIST_HEAD(&n->obj_location_list);
    list_add(&n->entry, var_list); 
    return n;
}

static struct list_head* obj_location_list_lookup(struct metadata_storage *s,
                                    int version, const char* var_name)
{
    int index = version % s->max_versions;
    struct list_head *l = &s->version_tab[index];

    struct var_list_node *var_node;
    var_node = var_node_lookup(l, var_name);
    if (var_node == NULL) {
        return NULL;
    }
    
    return &(var_node->obj_location_list);
}

static int var_node_free(struct var_list_node *var_node)
{
    struct obj_location_list_node *n, *tmp;
    list_for_each_entry_safe(n, tmp, &var_node->obj_location_list,
                        struct obj_location_list_node, entry)
    {
        list_del(&n->entry);
        free(n);
    }

    return 0;
}

struct metadata_storage *metadata_s_alloc(int max_versions)
{
    struct metadata_storage *s = malloc(sizeof(*s) + 
                                sizeof(struct list_head)*max_versions);
    if (!s) {
        errno = ENOMEM;
        return s;
    }

    memset(s, 0, sizeof(*s));
    int i;
    for (i = 0; i < max_versions; i++) {
        INIT_LIST_HEAD(&s->version_tab[i]);
    }
    s->max_versions = max_versions;

    return s;
}

int metadata_s_free(struct metadata_storage *s)
{
    int i;
    for (i = 0; i < s->max_versions; i++) {
        struct var_list_node *n, *tmp;
        list_for_each_entry_safe(n, tmp, &s->version_tab[i],
                            struct var_list_node, entry)
        {
            var_node_free(n);
            list_del(&n->entry);
            free(n);
        }
    }

    free(s);
    s = NULL;

    return 0;
}

int metadata_s_add_obj_location(struct metadata_storage *s,
                                struct rpc_cmd *cmd)
{
    int err;
    struct list_head *l = NULL;
    struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)cmd->pad;
    struct obj_descriptor *odsc = &hdr->odsc;

    // Lookup  
    l = obj_location_list_lookup(s, odsc->version, odsc->name);
    if (l == NULL) {
        err = -1;
        goto err_out;
    }

    // First remove any existing obj location info whose bbox intersects with 
    // the newly inserted obj location info
    struct obj_location_list_node *n, *tmp;
    list_for_each_entry_safe(n, tmp, l, struct obj_location_list_node, entry)
    {
        hdr = (struct hdr_dimes_put*)(n->cmd.pad);
        if (obj_desc_by_name_intersect(odsc, &hdr->odsc))
        {
            list_del(&n->entry);
            free(n);
        }
    }

    n = calloc(1, sizeof(struct obj_location_list_node));
    n->cmd = *cmd;
    list_add(&n->entry, l);

    return 0;
 err_out:
    ERROR_TRACE();
}

int metadata_s_find_obj_location(struct metadata_storage *s,
                                 struct obj_descriptor *odsc,
                                 struct list_head *out_list, int *out_num_items)
{
    int err;
    struct list_head *l = NULL;
    
    // lookup
    l = obj_location_list_lookup(s, odsc->version, odsc->name);
    if (l == NULL) {
        err = -1;
        goto err_out;
    }

    *out_num_items = 0;
    struct obj_location_list_node *n;
    list_for_each_entry(n, l, struct obj_location_list_node, entry)
    {
        struct hdr_dimes_put *hdr = (struct hdr_dimes_put *)(n->cmd.pad);
        if (obj_desc_equals_intersect(odsc, &hdr->odsc)) {
            struct obj_location_wrapper *p;
            p = malloc(sizeof(struct obj_location_wrapper));
            p->data_ref = &(n->cmd);
            list_add(&p->entry, out_list);
            (*out_num_items)++;
        }
    }
    
    return 0;
 err_out:
    ERROR_TRACE();
}

/*
  Allocate and init the local storage structure.
*/
struct ss_storage *dimes_ls_alloc(int max_versions)
{
	struct ss_storage *ls = 0;
	int i;

	ls = malloc(sizeof(*ls) + sizeof(struct list_head) * max_versions);
	if (!ls) {
		errno = ENOMEM;
		return ls;
	}

	memset(ls, 0, sizeof(*ls));
	for (i = 0; i < max_versions; i++)
		INIT_LIST_HEAD(&ls->obj_hash[i]);
	ls->size_hash = max_versions;

	return ls;
}

/*
  Free the memory of stored data objects 
*/
int dimes_ls_free(struct ss_storage *ls)
{
	int max_versions = ls->size_hash;
	int i;

	for ( i = 0; i < max_versions; i++) {
		struct obj_data_wrapper *od_w, *tmp;
		list_for_each_entry_safe(od_w, tmp, &ls->obj_hash[i],
				struct obj_data_wrapper, obj_entry){
			list_del(&od_w->obj_entry);
			obj_data_free(od_w->od);
			free(od_w);
		}
	}

	return 0;
}

/*
  Find an object in the local storage that has the same name and
  version with the object descriptor 'odsc'.
*/
struct obj_data_wrapper*
dimes_ls_find(struct ss_storage *ls, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int index;

	//obj versions start from 0 !
	index = odsc->version % ls->size_hash;
	list = &ls->obj_hash[index];

	list_for_each_entry(od_w, list, struct obj_data_wrapper, obj_entry) {
		if (obj_desc_equals_intersect(odsc, &od_w->od->obj_desc))
			return od_w;
	}

	return NULL;
}

void dimes_ls_add_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w)
{
	int index;
	struct list_head *bin;
	struct obj_data_wrapper *od_w_existing;

	od_w_existing = dimes_ls_find_no_version(ls, &od_w->od->obj_desc);
	if (od_w_existing) {
#ifdef DEBUG
		uloga("%s(): to evict data obj, name=%s, version=%u\n", 
					__func__, od_w_existing->od->obj_desc.name,
		od_w_existing->od->obj_desc.version);
#endif

		od_w_existing->od->f_free = 1;
		if (od_w_existing->od->refcnt == 0){
			dimes_ls_remove_obj(ls, od_w_existing);
			obj_data_free(od_w_existing->od);
			free(od_w_existing);
		} else {
			uloga("'%s()': object eviction delayed.\n", __func__);
		}
	}

	index = od_w->od->obj_desc.version % ls->size_hash;
	bin = &ls->obj_hash[index];

	//NOTE: new object comes first in the list.
	list_add(&od_w->obj_entry, bin);
	ls->num_obj++;
#ifdef DEBUG
	uloga("%s(): add new obj, name=%s, version=%u, ls->num_obj=%d\n",
				__func__, od_w->od->obj_desc.name, od_w->od->obj_desc.version,
	ls->num_obj);
#endif
}

/*
  Search for an object in the local storage that is mapped to the same
  bin, and that has the same  name and object descriptor, but may have
  different version.
*/
struct obj_data_wrapper *
dimes_ls_find_no_version(struct ss_storage *ls, struct obj_descriptor *odsc)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int index;

	index = odsc->version % ls->size_hash;
	list = &ls->obj_hash[index];

	list_for_each_entry(od_w, list, struct obj_data_wrapper, obj_entry) {
		if (obj_desc_by_name_intersect(odsc, &od_w->od->obj_desc))
		return od_w;
	}

	return NULL;
}

void dimes_ls_remove_obj(struct ss_storage *ls, struct obj_data_wrapper *od_w)
{      
	list_del(&od_w->obj_entry);
	ls->num_obj--;
}

int dimes_ls_count_obj_no_version(struct ss_storage *ls, int query_tran_id)
{
	struct obj_data_wrapper *od_w;
	struct list_head *list;
	int cnt = 0;
	int version;

	for (version = 0; version < ls->size_hash; version++) {
		int i = version % ls->size_hash;
		list = &ls->obj_hash[i];

		list_for_each_entry(od_w, list, struct obj_data_wrapper,
				    obj_entry) {
			if (od_w->q_id == query_tran_id)
				cnt++;
		}
	}

	return cnt;
}

size_t dimes_buffer_size = 0;
//Starting pointer to the memory buffer
uint64_t dimes_buffer_ptr = 0;

/*linked list for free dimes_buf_block. And the blocks in the list
are sorted by block_size in ascending order.
*/
struct list_head free_blocks_list;

/*linked list for in use dimes_buf_block. And the blocks in the list
are sorted by block_size in ascending order.
*/
struct list_head used_blocks_list;

enum mem_block_status {
    free_block = 0,
    used_block
};

/*dynamic mem block data structure*/
struct dimes_buf_block {
    struct list_head mem_block_entry;
    size_t block_size;
    uint64_t block_ptr;
    enum mem_block_status block_status;
};

#define SIZE_DART_MEM_BLOCK sizeof(struct dimes_buf_block)

//
// Cleanup a blocks list
//
static void dimes_buf_cleanup_list(struct list_head* blocks_list)
{
    struct dimes_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, blocks_list, struct dimes_buf_block, mem_block_entry) {
        list_del(&mb->mem_block_entry);
        free(mb);//free the memory
    }
}

//
// Insert a memory block into the list that sorted by block_size.
//
static void dimes_buf_insert_block(
    struct dimes_buf_block *block,
    struct list_head* blocks_list)
{
    struct dimes_buf_block *mb, *tmp;
    int inserted = 0;

    list_for_each_entry_safe(mb, tmp, blocks_list, struct dimes_buf_block, mem_block_entry) {
        if(block->block_size <= mb->block_size) {
            //add the block befor the position of mb in the list.
            list_add_before_pos(&block->mem_block_entry, &mb->mem_block_entry);
            inserted = 1;
            break;
        }
    }

    //add the block to the tail
    if (!inserted)
        list_add_tail(&block->mem_block_entry, blocks_list);
}

/* 
   Insert a memory  block into the  free blocks list,  and merge
   contiguous free blocks into larger new one.
*/
static void dimes_buf_insert_block_to_freelist(
    struct dimes_buf_block *block,
    struct list_head *blocks_list)
{
    /*    low_memory_addresses ------->  high_memory_addresses
    |             |    |                         |
    | contiguous_block_before |  block |  contiguous_block_after | 
    |                         |        |                 |
    */
    //the contiguous free block before 'block' in the buffer
    struct dimes_buf_block *contiguous_block_before = NULL;
    //the contiguous free block after 'block' in the buffer
    struct dimes_buf_block *contiguous_block_after = NULL;

    struct dimes_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, blocks_list, struct dimes_buf_block, mem_block_entry) {
        if(block->block_ptr == (mb->block_ptr + mb->block_size) )
            contiguous_block_before = mb;
        if(mb->block_ptr == (block->block_ptr + block->block_size) )
            contiguous_block_after = mb;
    }

    if(contiguous_block_after) {
        //update the size of the merged new free block
        block->block_size = block->block_size + contiguous_block_after->block_size;
        //the starting address of the merged new free block does NOT change

        //remove from free blocks list
        list_del(&contiguous_block_after->mem_block_entry);
        free(contiguous_block_after);
    }

    if(contiguous_block_before) {
        //update the size of the merged new free block
        block->block_size = block->block_size + contiguous_block_before->block_size;
        //update the starting address of the merged new free block  
        block->block_ptr = contiguous_block_before->block_ptr;

        //remove from free blocks list
        list_del(&contiguous_block_before->mem_block_entry);
        free(contiguous_block_before);
    }

    //add the new free block into list
    block->block_status = free_block;
    dimes_buf_insert_block(block, blocks_list);
}

//
// Initialize DART Buffer
//
int dimes_buffer_init(uint64_t base_addr, size_t size)
{
    INIT_LIST_HEAD(&free_blocks_list);
    INIT_LIST_HEAD(&used_blocks_list);

    dimes_buffer_ptr = base_addr;
    if (dimes_buffer_ptr != 0) {
        dimes_buffer_size = size;

        //create the first free block
        struct dimes_buf_block *block;
        block = (struct dimes_buf_block*)malloc(SIZE_DART_MEM_BLOCK);
        block->block_size = size;
        block->block_ptr = dimes_buffer_ptr;
        block->block_status = free_block;

        //add the block into free blocks list 
        dimes_buf_insert_block(block, &free_blocks_list);

        return 0;
    }
    else    return -1;
}

//
// Finalize DART Buffer 
//
int dimes_buffer_finalize()
{
    //cleanup the free_blocks_list
    dimes_buf_cleanup_list(&free_blocks_list);

    //cleanup the used_blocks_list
    dimes_buf_cleanup_list(&used_blocks_list);

    return 0;
}

size_t dimes_buffer_total_size()
{
    return dimes_buffer_size;
}

void dimes_buffer_alloc(size_t size, uint64_t *ptr)
{
    //If requested buffer size exceeds the total available
    if (size > dimes_buffer_size) {
        fprintf(stderr, "%s(): requested size %u exceeds buffer size %u\n",
            __func__, size, dimes_buffer_size); 
        goto err_out;
    }

    //Search for usable free block with the smallest data size
    struct dimes_buf_block * mb, *tmp;
    int found = 0;
    list_for_each_entry_safe(mb, tmp, &free_blocks_list,
        struct dimes_buf_block, mem_block_entry){
        if(mb->block_size >= size) {
            //Usable free block found
            found = 1;
            break;
        }
    }

    if (found) {
        /*Usable free block 'mb' found*/

        //new_free_mb is the new free memory block after allocating space from 'mb'
        //new_used_mb is the new in_use memory block after allocating space from 'mb'
        struct dimes_buf_block * new_free_mb, * new_used_mb;

        //remove 'mb' from free blocks list
        list_del(&mb->mem_block_entry);

        if(mb->block_size == size){
            new_free_mb = NULL;
            new_used_mb = mb;
            new_used_mb->block_status = used_block;
        }

        if(mb->block_size > size){
            new_free_mb = (struct dimes_buf_block*)malloc(SIZE_DART_MEM_BLOCK);
            new_free_mb->block_status = free_block;
            new_free_mb->block_ptr = mb->block_ptr + size;
            new_free_mb->block_size = mb->block_size - size;
            new_used_mb = mb;
            new_used_mb->block_status = used_block;
            new_used_mb->block_size = size;
        }

        //add 'new_free_mb'  into free blocks list
        if(new_free_mb != NULL)
            dimes_buf_insert_block(new_free_mb, &free_blocks_list);
        //add 'new_used_mb' into used blocks list 
        if(new_used_mb != NULL)
            dimes_buf_insert_block(new_used_mb, &used_blocks_list);

        *ptr = (uint64_t)new_used_mb->block_ptr;
        return;
    }
    else {
        /*Could not find usable free block*/
        fprintf(stderr, "%s: failed! no space\n", __func__);
        goto err_out;
    }

 err_out:
    *ptr = (uint64_t)0;
    return;
}

void dimes_buffer_free(uint64_t ptr)
{
    /*test if the value of ptr is valid*/
    if (ptr == 0 || dimes_buffer_ptr == 0)
        return;

    uint64_t start_ptr = dimes_buffer_ptr;
    uint64_t end_ptr = dimes_buffer_ptr + dimes_buffer_size - 1;
    if ( ptr < start_ptr || ptr > end_ptr) {
        fprintf(stderr, "%s(): error invalid address! start_ptr %llx end_ptr %llx ptr %llx\n", __func__, start_ptr, end_ptr, ptr);
        return;
    }

    /*search for the corresponding in_use memory block for ptr*/
    struct dimes_buf_block * mb, *tmp;
    unsigned int found = 0;//flag value init as 0!
    list_for_each_entry_safe(mb, tmp, &used_blocks_list, struct dimes_buf_block, mem_block_entry){
        if(mb->block_ptr == ptr) {
            found = 1;
            break;
        }
    }

    if (found) {

        //remove 'mb' from used blocks list
        list_del(&mb->mem_block_entry);

        //add back 'mb' into free blocks list
        dimes_buf_insert_block_to_freelist(mb, &free_blocks_list);
    }
    else return;
}

//For testing
void print_free_blocks_list()
{
    printf("#####Free Blocks List####\n");

    struct dimes_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, &free_blocks_list, struct dimes_buf_block, mem_block_entry){
        printf("free block: ptr=%p, size=%zu, status=%u\n",
            mb->block_ptr, mb->block_size, mb->block_status);
    }
}

void print_used_blocks_list()
{
    printf("#####Used Blocks List####\n");

    struct dimes_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, &used_blocks_list, struct dimes_buf_block, mem_block_entry){
        printf("used block: ptr=%p, size=%zu, status=%u\n",
            mb->block_ptr, mb->block_size, mb->block_status);
    }
}
#ifdef DS_HAVE_DIMES_SHMEM
int dimes_client_shmem_checkpoint_allocator(int shmem_obj_id, void *restart_buf)
{
    struct dimes_cr_allocator_info *alloc_info;
    struct dimes_cr_allocator_block_info *alloc_blk_info;
    void *buf = restart_buf;
    if (!buf) return -1;

    alloc_info = buf;
    buf += sizeof(struct dimes_cr_allocator_info);

    uint32_t num_used_blocks = 0, num_free_blocks = 0;
    struct dimes_buf_block *mb, *t;
    list_for_each_entry_safe(mb, t, &free_blocks_list, struct dimes_buf_block,
        mem_block_entry)
    {
        num_free_blocks++;
        alloc_blk_info = buf;
        alloc_blk_info->size = mb->block_size;
        alloc_blk_info->offset = (mb->block_ptr-dimes_buffer_ptr); // TODO: safe?
        alloc_blk_info->block_status = mb->block_status;
        buf += sizeof(struct dimes_cr_allocator_block_info);
    }
    list_for_each_entry_safe(mb, t, &used_blocks_list, struct dimes_buf_block,
        mem_block_entry)
    {
        num_used_blocks++;
        alloc_blk_info = buf;
        alloc_blk_info->size = mb->block_size;
        alloc_blk_info->offset = (mb->block_ptr-dimes_buffer_ptr); // TODO: safe?
        alloc_blk_info->block_status = mb->block_status;
        buf += sizeof(struct dimes_cr_allocator_block_info);
    }

    alloc_info->num_free_blocks = num_free_blocks;
    alloc_info->num_used_blocks = num_used_blocks;
    alloc_info->shmem_obj_id = shmem_obj_id;

    return 0;
}

static void restart_allocator_insert_block(struct dimes_cr_allocator_block_info *blk_info,
        struct list_head *blocks_list)
{
    struct dimes_buf_block *mb = malloc(SIZE_DART_MEM_BLOCK);
    mb->block_size = blk_info->size;
    mb->block_ptr = dimes_buffer_ptr + blk_info->offset;
    mb->block_status = (enum mem_block_status)blk_info->block_status;
    list_add_tail(&mb->mem_block_entry, blocks_list); 
}

int dimes_client_shmem_restart_allocator(void *restart_buf, int dart_id)
{
    void *buf = restart_buf;
    struct dimes_cr_allocator_info *alloc_info = buf;

    // debug print
    //printf("%s: #%d num_used_blocks= %u num_free_blocks= %u shmem_obj_id= %d\n",
    //    __func__, dart_id, alloc_info->num_used_blocks, alloc_info->num_free_blocks,
    //    alloc_info->shmem_obj_id);

    buf += sizeof(struct dimes_cr_allocator_info);
    struct dimes_cr_allocator_block_info *alloc_blk_info;
    int i;
    for (i = 0; i < alloc_info->num_free_blocks; i++) {
        alloc_blk_info = buf;
        restart_allocator_insert_block(alloc_blk_info, &free_blocks_list);
        buf += sizeof(struct dimes_cr_allocator_block_info);
        // debug print
        //printf("%s: #%d size= %u offset= %u block_status= %u\n",
        //    __func__, dart_id, alloc_blk_info->size,
        //    alloc_blk_info->offset, alloc_blk_info->block_status);
    }

    for (i = 0; i < alloc_info->num_used_blocks; i++) {
        alloc_blk_info = buf;
        restart_allocator_insert_block(alloc_blk_info, &used_blocks_list);
        buf += sizeof(struct dimes_cr_allocator_block_info);
        // debug print
        //printf("%s: #%d size= %u offset= %u block_status= %u\n",
        //    __func__, dart_id, alloc_blk_info->size,
        //    alloc_blk_info->offset, alloc_blk_info->block_status);
    }

    return 0;
}

size_t estimate_allocator_restart_buf_size(int dart_id)
{
    uint32_t num_used_blocks = 0, num_free_blocks = 0;
    struct dimes_buf_block *mb, *t;
    list_for_each_entry_safe(mb, t, &free_blocks_list, struct dimes_buf_block,
        mem_block_entry)
    {
        num_free_blocks++;
    }
    list_for_each_entry_safe(mb, t, &used_blocks_list, struct dimes_buf_block,
        mem_block_entry)
    {
        num_used_blocks++;
    }

    size_t bytes = sizeof(struct dimes_cr_allocator_info) +
                    sizeof(struct dimes_cr_allocator_block_info) *
                    (num_free_blocks+num_used_blocks);
    size_t page_size = PAGE_SIZE;
    bytes = (bytes/page_size+1)*page_size;

    //printf("%s: #%d num_free_blocks= %u num_used_blocks= %u allocator_restart_buf num_page= %u size= %u bytes\n", __func__, dart_id, num_free_blocks, num_used_blocks, bytes/page_size, bytes);
    return bytes;
}
#endif
#endif // end of #ifdef DS_HAVE_DIMES
