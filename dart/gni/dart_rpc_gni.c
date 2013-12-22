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
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "dart_rpc_gni.h"
#include "debug.h"
//#include "gni_pub.h"
//#include "pmi.h"

#define DEVICE_ID	0
//#define RECVHEADER	sizeof(struct hdr_sys)
#define RECVHEADER	0
#define SYSPAD          24   //24 default for 10 credit
#define SYSNUM		rpc_s->num_buf
#define SENDCREDIT	rpc_s->num_buf/2-2
#define RECVCREDIT	rpc_s->num_buf/2-4
#define SEC 0.1

#define ENTRY_COUNT             65535
#define INDEX_COUNT             65536

#define SYS_WAIT_COMPLETION(x)					\
	while (!(x)) {						\
		err = sys_process_event(rpc_s);			\
		if ( err != 0 )					\
			goto err_out;				\
	}


#define myid(rpc_s)		(rpc_s->ptlmap.id)
#define rank2id(rpc_s, rank)	((rank) + (rpc_s->app_minid))
#define id2rank(rpc_s, id)	((id) - (rpc_s->app_minid))
#define myrank(rpc_s)		id2rank(rpc_s, myid(rpc_s))


static int first_spawned;
static int rank_id_pmi; //this global rank_id is fetched by using pmi library.
static int num_of_rank_pmi;

static uint32_t cookie;
static uint8_t ptag;

static struct rpc_server *rpc_s_instance;

static int num_service = 0;
static int sys_process_event(struct rpc_server *rpc_s);
static int sys_send(struct rpc_server *rpc_s, struct node_id *to, struct hdr_sys *hs);
static struct node_id *rpc_get_peer(struct rpc_server *rpc_s, int peer_id);

static struct list_head index_list;

static struct {
        enum cmd_type   rpc_cmd;
        rpc_service     rpc_func;
} rpc_commands[64];
/* =====================================================
	Fabric:
	(1)barrier functions
	(2)Index functions
	(3)rpc operation functions
	(4)message passing & data transfering functions
	Public APIs
 =====================================================*/

#ifdef DART_UGNI_PREALLOC_RDMA 
size_t dart_buffer_size = 0;
//Starting pointer to the memory buffer
uint64_t dart_buffer_ptr = 0;

/*linked list for free dart_buf_block. And the blocks in the list
are sorted by block_size in ascending order.
*/
struct list_head free_blocks_list;

/*linked list for in use dart_buf_block. And the blocks in the list
are sorted by block_size in ascending order.
*/
struct list_head used_blocks_list;

enum mem_block_status {
    free_block,
    used_block
};

/*dynamic mem block data structure*/
struct dart_buf_block {
    struct list_head mem_block_entry;
    size_t block_size;
    uint64_t block_ptr;
    enum mem_block_status block_status;
};

#define SIZE_DART_MEM_BLOCK sizeof(struct dart_buf_block)

//
// Cleanup a blocks list
//
static void dart_buf_cleanup_list(struct list_head* blocks_list)
{
    struct dart_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, blocks_list, struct dart_buf_block, mem_block_entry) {
        list_del(&mb->mem_block_entry);
        free(mb);//free the memory
    }
}

//
// Insert a memory block into the list that sorted by block_size.
//
static void dart_buf_insert_block(
    struct dart_buf_block *block,
    struct list_head* blocks_list)
{
    struct dart_buf_block *mb, *tmp;
    int inserted = 0;

    list_for_each_entry_safe(mb, tmp, blocks_list, struct dart_buf_block, mem_block_entry) {
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
static void dart_buf_insert_block_to_freelist(
    struct dart_buf_block *block,
    struct list_head *blocks_list)
{
    /*    low_memory_addresses ------->  high_memory_addresses
    |             |    |                         |
    | contiguous_block_before |  block |  contiguous_block_after | 
    |                         |        |                 |
    */
    //the contiguous free block before 'block' in the buffer
    struct dart_buf_block *contiguous_block_before = NULL;
    //the contiguous free block after 'block' in the buffer
    struct dart_buf_block *contiguous_block_after = NULL;

    struct dart_buf_block * mb, *tmp;
    list_for_each_entry_safe(mb, tmp, blocks_list, struct dart_buf_block, mem_block_entry) {
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
    dart_buf_insert_block(block, blocks_list);
}

//
// Initialize DART Buffer
//
int dart_buffer_init(uint64_t base_addr, size_t size)
{
    INIT_LIST_HEAD(&free_blocks_list);
    INIT_LIST_HEAD(&used_blocks_list);

    dart_buffer_ptr = base_addr;
    if (dart_buffer_ptr != 0) {
        dart_buffer_size = size;

        //create the first free block
        struct dart_buf_block *block;
        block = (struct dart_buf_block*)malloc(SIZE_DART_MEM_BLOCK);
        block->block_size = size;
        block->block_ptr = dart_buffer_ptr;
        block->block_status = free_block;

        //add the block into free blocks list 
        dart_buf_insert_block(block, &free_blocks_list);

        return 0;
    }
    else    return -1;
}

//
// Finalize DART Buffer 
//
int dart_buffer_finalize()
{
    //cleanup the free_blocks_list
    dart_buf_cleanup_list(&free_blocks_list);

    //cleanup the used_blocks_list
    dart_buf_cleanup_list(&used_blocks_list);

    return 0;
}

size_t dart_buffer_total_size()
{
    return dart_buffer_size;
}

void dart_buffer_alloc(size_t size, uint64_t *ptr)
{
    //If requested buffer size exceeds the total available
    if (size > dart_buffer_size)
        goto err_out;

    //Search for usable free block with the smallest data size
    struct dart_buf_block * mb, *tmp;
    int found = 0;
    list_for_each_entry_safe(mb, tmp, &free_blocks_list, struct dart_buf_block, 
mem_block_entry){
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
        struct dart_buf_block * new_free_mb, * new_used_mb;

        //remove 'mb' from free blocks list
        list_del(&mb->mem_block_entry);

        if(mb->block_size == size){
            new_free_mb = NULL;
            new_used_mb = mb;
            new_used_mb->block_status = used_block;
        }

        if(mb->block_size > size){
            new_free_mb = (struct dart_buf_block*)malloc(SIZE_DART_MEM_BLOCK);
            new_free_mb->block_status = free_block;
            new_free_mb->block_ptr = mb->block_ptr + size;
            new_free_mb->block_size = mb->block_size - size;
            new_used_mb = mb;
            new_used_mb->block_status = used_block;
            new_used_mb->block_size = size;
        }

        //add 'new_free_mb'  into free blocks list
        if(new_free_mb != NULL)
            dart_buf_insert_block(new_free_mb, &free_blocks_list);
        //add 'new_used_mb' into used blocks list 
        if(new_used_mb != NULL)
            dart_buf_insert_block(new_used_mb, &used_blocks_list);

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

void dart_buffer_free(uint64_t ptr)
{
    /*test if the value of ptr is valid*/
    if (ptr == 0 || dart_buffer_ptr == 0)
        return;

    uint64_t start_ptr = dart_buffer_ptr;
    uint64_t end_ptr = dart_buffer_ptr + dart_buffer_size - 1;
    if ( ptr < start_ptr || ptr > end_ptr) {
        fprintf(stderr, "%s(): error invalid address! start_ptr %llx end_ptr %llx ptr %llx\n", __func__, start_ptr, end_ptr, ptr);
        return;
    }

    /*search for the corresponding in_use memory block for ptr*/
    struct dart_buf_block * mb, *tmp;
    unsigned int found = 0;//flag value init as 0!
    list_for_each_entry_safe(mb, tmp, &used_blocks_list, struct dart_buf_block, mem_block_entry){
        if(mb->block_ptr == ptr) {
            found = 1;
            break;
        }
    }

    if (found) {

        //remove 'mb' from used blocks list
        list_del(&mb->mem_block_entry);

        //add back 'mb' into free blocks list
        dart_buf_insert_block_to_freelist(mb, &free_blocks_list);
    }
    else return;
}

static inline uint64_t rpc_dart_mem_malloc(size_t size)
{
    uint64_t buf;
    dart_buffer_alloc(size, &buf);
    if (!buf) {
        printf("%s(): dart_buffer_alloc() failed\n", __func__);
        return 0;
    }

    return buf;
}

static inline void rpc_dart_mem_free(void *ptr)
{
    dart_buffer_free((uint64_t)ptr);
}

static int rpc_dart_mem_init(struct rpc_server *rpc_s, size_t dart_mem_buffer_size)
{
    int err;
    void *buf;
    gni_return_t status;

    buf = malloc(dart_mem_buffer_size);
    if (!buf) {
        printf("%s(): malloc failed\n", __func__);
        goto err_out;
    }
    memset(buf, 0, dart_mem_buffer_size);

    status = GNI_MemRegister(rpc_s->nic_hndl,
                            (uint64_t)buf,
                            (uint64_t)dart_mem_buffer_size,
                            rpc_s->dst_cq_hndl,
                            GNI_MEM_READWRITE,
                            -1,
                            &rpc_s->dart_mem_mdh);
    if (status != GNI_RC_SUCCESS) {
        printf("Fail: %d GNI_MemRegister returned error with %d in %s.\n",
                rpc_s->ptlmap.id, status, __func__);
        err = -1;
        free(buf);
        goto err_out;
    }

    dart_buffer_init((uint64_t)buf, dart_mem_buffer_size);
    return 0;
err_out:
    return err;
}

static int rpc_dart_mem_finalize(struct rpc_server *rpc_s)
{
    int err;
    gni_return_t status;

    status = GNI_MemDeregister(rpc_s->nic_hndl, &rpc_s->dart_mem_mdh);
    if ( status != GNI_RC_SUCCESS ) {
        printf("Fail: %d GNI_MemDeregister returned error with %d in %s.\n",
                rpc_s->ptlmap.id, status, __func__);
    }

    uint64_t addr = dart_buffer_ptr;
    dart_buffer_finalize();
    free(addr);

    return 0;
}
#endif


/* =====================================================
	barrier
 =====================================================*/

static int log2_ceil(int n)
{
	unsigned int i;
	int k = -1;

	i = ~(~0U >> 1);
	while (i && !(i & n))
		i = i >> 1;
	if (i != n)
		i = i << 1;

	while (i) 
	{
		k++;
		i = i >> 1;
	}

	return k;
}

//A remote peer entered the barrier. 

static int sys_bar_arrive(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
	rpc_s->bar_tab[hs->sys_id] = hs->sys_pad1;

	return 0;
}


static int sys_bar_send(struct rpc_server *rpc_s, int peerid)
{
  struct node_id *peer = rpc_get_peer(rpc_s, peerid);
	struct hdr_sys hs;
	int err;

	memset(&hs, 0, sizeof(hs));
	hs.sys_cmd = sys_bar_enter;
	hs.sys_pad1 = rpc_s->bar_num;
	hs.sys_id = myrank(rpc_s);

	err = sys_send(rpc_s, peer, &hs);
	if(err!=0)
	  printf("(%s) failed with (%d).\n", __func__, err);
	return err;
}


/* =====================================================
	Index functions
 =====================================================*/
static int rpc_index_init(struct rpc_server *rpc_s)
{
	int n, i, j=0;
	struct rr_index *ri;

	INIT_LIST_HEAD(&index_list);
	for(i=1; i<INDEX_COUNT-1; i++)
	{
		ri = calloc(1, sizeof(struct rr_index));
		if(ri==NULL)
		{
			printf("(%s) failed when calloc.", __func__);
			return -ENOMEM;
		}

		ri->index = i;
		list_add_tail(&ri->index_entry, &index_list);
	}

	return 0;
}

static uint32_t rpc_get_index(void)
{
	struct rr_index *ri, *tmp;
	uint32_t current_index = -1;
	list_for_each_entry_safe(ri, tmp, &index_list, struct rr_index, index_entry)
	{
		current_index = ri->index;
		list_del(&ri->index_entry);
		free(ri);
		break;
	}
	//	printf("Rank %d: get index %d.\n", rpc_s_instance->ptlmap.id, current_index);
	return current_index;
}

static int rpc_free_index(int index)
{
	struct rr_index *ri;
	ri = calloc(1, sizeof(struct rr_index));
	if(ri==NULL)
	{
		printf("(%s) failed when calloc.", __func__);
		return -ENOMEM;
	}

	ri->index = index;
	list_add_tail(&ri->index_entry, &index_list);

	//	printf("Rank %d: free index %d.\n", rpc_s_instance->ptlmap.id, index);
	
	return 0;
}

/* =====================================================
	sys operation
 =====================================================*/

static int init_gni (struct rpc_server *rpc_s)
{
    int err;
    int modes = 0;
    int device_id = DEVICE_ID;
    gni_return_t status;

    /* Try from environment first DSPACES_GNI_PTAG (decimal) and DSPACES_GNI_COOKIE (hexa)*/
    ptag = get_ptag_env("DSPACES_GNI_PTAG");
    if (ptag != 0)
        cookie = get_cookie_env("DSPACES_GNI_COOKIE");
    //uloga(" ****** (%s) from env: ptag=%d  cookie=%x.\n", __func__, ptag, cookie);

    if (ptag == 0 || cookie == 0) {
#ifdef GNI_PTAG
        ptag = GNI_PTAG;
        cookie = GNI_COOKIE;
        //uloga(" ****** (%s) from configure: ptag=%d  cookie=%x.\n", __func__, ptag, cookie);
#else
        err = get_named_dom("ADIOS", &ptag, &cookie);
        if(err != 0){
            printf("Fail: ptag and cookie returned error. %d.\n", err);
            goto err_out;
        }
    //uloga(" ****** (%s) from apstat: ptag=%d  cookie=%x.\n", __func__, ptag, cookie);
#endif
    }

	status = GNI_CdmCreate(rank_id_pmi, ptag, cookie, modes, &rpc_s->cdm_handle);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CdmCreate returned error. Used ptag=%d cookie=%x status=%d.\n", 
                        ptag, cookie, status);
		goto err_out;
	}

	status = GNI_CdmAttach(rpc_s->cdm_handle, device_id, &rpc_s->ptlmap.nid, &rpc_s->nic_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CdmAttach returned error. Used ptag=%d cookie=%x status=%d.\n", 
                        ptag, cookie, status);
		goto err_out;
	}

	rpc_s->ptlmap.pid = getpid();
	
	return 0;

err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;	
}

int sys_smsg_init (struct rpc_server *rpc_s, int num)//done
{
	int i, j, err = -ENOMEM;
	unsigned int responding_remote_addr;
	int responding_remote_id;

	gni_mem_handle_t sys_local_memory_handle;
	gni_return_t status;
	gni_post_state_t post_state;	

	// Allocate memory for system message
	rpc_s->sys_mem = calloc(SYSNUM * num, sizeof(struct hdr_sys)+SYSPAD);
	if(!rpc_s->sys_mem)
	{
		printf("Fail: SYS MSG MAILBOX calloc error.\n");
		err =  -ENOMEM;
		goto err_free;
	}

	status = GNI_MemRegister(rpc_s->nic_hndl, (uint64_t)rpc_s->sys_mem, (uint64_t)(SYSNUM * num * (sizeof(struct hdr_sys)+SYSPAD)), rpc_s->sys_cq_hndl, GNI_MEM_READWRITE, -1, &sys_local_memory_handle);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_MemRegister SYS returned error. %d.\n", status);
		goto err_out;
	}

	//sys_msg attributes init
	rpc_s->sys_local_smsg_attr.msg_type = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
	rpc_s->sys_local_smsg_attr.mbox_maxcredit = SYSNUM;
	rpc_s->sys_local_smsg_attr.msg_maxsize = sizeof(struct hdr_sys)+SYSPAD;
	rpc_s->sys_local_smsg_attr.msg_buffer = rpc_s->sys_mem;
	rpc_s->sys_local_smsg_attr.buff_size = SYSNUM * (sizeof(struct hdr_sys)+SYSPAD);
	rpc_s->sys_local_smsg_attr.mem_hndl = sys_local_memory_handle;
	rpc_s->sys_local_smsg_attr.mbox_offset = 0;

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
}

int sys_smsg_config(struct rpc_server *rpc_s, struct node_id *peer)
{
	int err = -ENOMEM;
	gni_return_t status;

	rpc_s->sys_local_smsg_attr.mbox_offset = SYSNUM * (sizeof(struct hdr_sys)+SYSPAD) * peer->ptlmap.id; 
	peer->sys_remote_smsg_attr.mbox_offset = SYSNUM * (sizeof(struct hdr_sys)+SYSPAD) * rpc_s->ptlmap.id; 

	//peer_smsg_check(rpc_s, peer, &peer->sys_remote_smsg_attr);

	status = GNI_SmsgInit(peer->sys_ep_hndl, &rpc_s->sys_local_smsg_attr, &(peer->sys_remote_smsg_attr));
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_SmsgInit SYS returned error. %d.\n", status);
		goto err_out;
	}
	
	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
}

int sys_ep_smsg_config(struct rpc_server *rpc_s, struct node_id *peer)
{
	int err = -ENOMEM;
	gni_return_t status;

	  printf("Rank %d: Fail GNI_EpBind nid %d, sys_ep_hndl %d rpc_ep_hndl %d.\n", peer->ptlmap.id, peer->ptlmap.nid, peer->sys_ep_hndl, peer->ep_hndl);

	status = GNI_EpCreate(rpc_s->nic_hndl, rpc_s->sys_cq_hndl, &peer->sys_ep_hndl);
	if (status != GNI_RC_SUCCESS)
	{
		printf("Rank %d: Fail GNI_EpCreate SYS returned error. %d.\n", rpc_s->ptlmap.id, status);
		goto err_out;
	}
	status = GNI_EpBind(peer->sys_ep_hndl, peer->ptlmap.nid, peer->ptlmap.id-1);
	if (status != GNI_RC_SUCCESS)
	{
	  printf("Rank %d: Fail GNI_EpBind nid %d, sys_ep_hndl %d rpc_ep_hndl %d.\n", peer->ptlmap.id, peer->ptlmap.nid, peer->sys_ep_hndl, peer->ep_hndl);
		printf("Rank %d: Fail GNI_EpBind SYS returned error. %d.\n", rpc_s->ptlmap.id, status);
		goto err_out;
	}

	err = sys_smsg_config(rpc_s, peer);
	if (err != 0){
		printf("Rank %d: Failed for config SYS SMSG for %d. (%d)\n", rpc_s->ptlmap.id, peer->ptlmap.id, err);
		goto err_free;
	}

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
}

/*
  Generic routine to send a system message.
*/
static int sys_send(struct rpc_server *rpc_s, struct node_id *peer, struct hdr_sys *hs)
{
	int hdr_size = sizeof(*hs);
	gni_return_t status;
	int err;
	uint32_t local, remote;

	remote = rpc_s->ptlmap.id+INDEX_COUNT;
	local = rpc_s->ptlmap.id;
 
        status = GNI_EpSetEventData(peer->sys_ep_hndl, local, remote);
	if(status != GNI_RC_SUCCESS)
	  {
	    printf("(%s) Fail: SYS GNI_EpSetEventData returned error. (%d)\n", __func__, status);
	    goto err_out;
	  }

	while(peer->sys_msg_at_peer == 0)
	  {
	    err = sys_process_event(rpc_s);
	    if( err != 0 )
	      goto err_out_ack;
	  }

	status = GNI_SmsgSend(peer->sys_ep_hndl, NULL, 0, (void *)hs, (uint32_t)hdr_size, rpc_s->ptlmap.id);
	if((status != GNI_RC_SUCCESS) && (status != GNI_RC_NOT_DONE))
	{
		printf("Fail: SYS GNI_SmsgSend returned error. (%d)\n", status);
		goto err_out;
	}

	peer->sys_msg_at_peer--;

        return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
err_out_ack:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int sys_process_ack(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
  struct node_id *peer;
  peer = rpc_get_peer(rpc_s, (int)hs->sys_id);
  if(peer == NULL)
    {
      printf("(%s): rpc_get_peer err.\n", __func__);
      return -ENOMEM;
    }
  peer->sys_msg_at_peer = SENDCREDIT;
  return 0;
}

/*
  This is the  entry point for processing system  messages.  It can be
  called  from sys_process_event()  or  rpc_process_event() to  handle
  system events. System messages are not subject to flow control.
*/
static int sys_dispatch_event(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
	int err = 0;

	switch (hs->sys_cmd) 
	{
		case sys_none:
			break;

/*
		case sys_msg_ret:
			err = sys_update_credits(rpc_s, hs, 0);
			break;
*/
		case sys_msg_ack:
			err = sys_process_ack(rpc_s, hs);
			break;

		case sys_bar_enter:
			err = sys_bar_arrive(rpc_s, hs);
			break;
	}

	if(err!=0)
	  printf("(%s) failed with (%d).\n", __func__, err);

        return err;
}

/*
when receiver gets a certain number of system messages, 
it returns an ACK with credits cleanup notification to the sender.
*/
static int sys_credit_return(struct rpc_server *rpc_s, struct node_id *peer)
{
  struct hdr_sys hs;
  int err;
  
  memset(&hs, 0, sizeof(hs));
  hs.sys_cmd = sys_msg_ack;
  hs.sys_id = rpc_s->ptlmap.id;

  err = sys_send(rpc_s, peer, &hs);
  if(err != 0)
    printf("(%s) failed with (%d).\n", __func__, err);

  return err;
}


static int sys_process_event(struct rpc_server *rpc_s)
{
        gni_cq_entry_t	event_data = 0;
	uint64_t	event_type;
	int		event_id;
	gni_return_t	status = GNI_RC_SUCCESS;

	struct node_id 	*peer;
        struct hdr_sys *hs;
        int err, n;
	int cnt;

        status = GNI_CqWaitEvent(rpc_s->sys_cq_hndl, 300, &event_data);
        if (status == GNI_RC_SUCCESS)
	{
	  if(GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG)
	    printf("Successfully get system message.\n");//debug

		event_id = GNI_CQ_GET_INST_ID(event_data);

		if(event_id == rpc_s->ptlmap.id)
		  return 0;
		if(event_id != rpc_s->ptlmap.id)
		{
			peer = rpc_get_peer(rpc_s, (int)event_id);
			if(peer == NULL)
			{
				printf("(%s): rpc_get_peer err.\n", __func__);
				return -ENOMEM;
			}
			do
			{
				status = GNI_SmsgGetNext(peer->sys_ep_hndl, (void **) &hs);
				cnt++;
			} while(status != GNI_RC_SUCCESS && cnt < 20000);

			err = sys_dispatch_event(rpc_s, hs);
			do
			  status = GNI_SmsgRelease(peer->sys_ep_hndl);
			while(status == GNI_RC_NOT_DONE);
			  if(status != GNI_RC_SUCCESS)
			  {
			    printf("(%s): GNI_SmsgRelease failed with (%d).\n", __func__, status);
			      return status;
			  }

			peer->sys_msg_recv++;
			if(peer->sys_msg_recv == RECVCREDIT)
			  {
			    err = sys_credit_return(rpc_s, peer);
			    if(err!=0)
			      {
				printf("(%s): sys_credit_return failed with err (%d).\n", __func__, err);
			      return err;
			      }
			    peer->sys_msg_recv = 0;
			  }

			return err;
		}
	}
	else if (status == GNI_RC_TIMEOUT)
		return 0;

	else
	{
		printf("(%s): SYSTEM MESSAGE PROCESSING ERROR.\n", __func__);
		return status;
	}

}

static int sys_cleanup (struct rpc_server *rpc_s)
{
	gni_return_t status;
	int i, err;
	void *tmp;
	/*
	status = GNI_MemDeregister(rpc_s->nic_hndl, &rpc_s->sys_local_smsg_attr.mem_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_MemDeregister returned error. %d.\n", status);
		goto err_out;
	}

	free(rpc_s->sys_mem);
	*///SCA SYS

	for(i=0; i < rpc_s->num_rpc_per_buff; i++)
	{
	  if(rpc_s->peer_tab[i].ptlmap.id==rpc_s->ptlmap.id)
	    continue;

	  
		status = GNI_EpUnbind(rpc_s->peer_tab[i].sys_ep_hndl);
		if (status != GNI_RC_NOT_DONE && status != GNI_RC_SUCCESS) 
		{
		  printf("(%d)Fail: GNI_EpUnbind(%d) returned error. %d.\n", rank_id_pmi, rpc_s->peer_tab[i].ptlmap.id, status);
			goto err_out;
		}

		status = GNI_EpDestroy(rpc_s->peer_tab[i].sys_ep_hndl); 
		if (status != GNI_RC_SUCCESS) 
		{
			printf("Fail: GNI_EpDestroy returned error. %d.\n", status);
			goto err_out;
		}
	}

	status = GNI_CqDestroy(rpc_s->sys_cq_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CqDestroy returned error. %d.\n", status);
		goto err_out;
	}

	return 0;
err_out:
	printf("(%s): failed. (%d)\n",__func__, status);
	return status;
}


static int clean_gni (struct rpc_server *rpc_s)
{
	gni_return_t status;

	status = GNI_CdmDestroy(rpc_s->cdm_handle);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CdmDestroy returned error. %d.\n", status);
		return status;
	}

	return 0;
}



/* =====================================================
	rpc operation
 =====================================================*/
/*
 * gather_node_id gather all of the node_id structure for all of the
 *                      other ranks.
 *
 *   Returns: an array of node_id from all of the
 *            other ranks.
 */

struct node_id *gather_node_id(int appid)
{
	size_t		addr_len;
	struct node_id	*all_addrs;
	struct node_id	local_addr;
	int		rc;
	int		size;

	memset(&local_addr, 0, sizeof(struct node_id));

	rc = PMI_Get_size(&size);
	assert(rc == PMI_SUCCESS);

	rc = PMI_Get_rank(&local_addr.ptlmap.id);
	assert(rc == PMI_SUCCESS);
	local_addr.ptlmap.nid = get_gni_nic_address(0);
	local_addr.ptlmap.pid = getpid();
	local_addr.ptlmap.appid = appid;
    
	addr_len = sizeof(struct node_id);

 	//Allocate a buffer to hold the node_id from all of the other ranks.
	all_addrs = (struct node_id *) malloc(addr_len * size);
	assert(all_addrs != NULL);

	//Get the node_id from all of the other ranks.
	allgather(&local_addr, all_addrs, sizeof(struct node_id));

	return (struct node_id *)all_addrs;
}

int rpc_smsg_init(struct rpc_server *rpc_s, int num)
{
	int i, j, err = -ENOMEM;
	gni_return_t status;
	gni_post_state_t post_state;
	gni_mem_handle_t rpc_local_memory_handle;
	unsigned int	responding_remote_addr;
	int		responding_remote_id;
	unsigned int bytes_per_mbox;

	// Allocate memory for rpc message
	rpc_s->rpc_mem = calloc(rpc_s->num_buf * num, sizeof(struct rpc_cmd) + RECVHEADER);
	if(!rpc_s->rpc_mem)
	{
		printf("Fail: RPC MSG MAILBOX calloc error.\n");
		err =  -ENOMEM;
		goto err_free;
	}

	status = GNI_MemRegister(rpc_s->nic_hndl, (uint64_t)rpc_s->rpc_mem, (uint64_t)(rpc_s->num_buf * num * (sizeof(struct rpc_cmd) + RECVHEADER)), rpc_s->dst_cq_hndl, GNI_MEM_READWRITE, -1, &rpc_local_memory_handle);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_MemRegister RPC returned error. %d.\n", status);
		goto err_out;
	}	

	rpc_s->local_smsg_attr.msg_type = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
	rpc_s->local_smsg_attr.mbox_maxcredit = rpc_s->num_buf;
	rpc_s->local_smsg_attr.msg_maxsize = sizeof(struct rpc_cmd) + RECVHEADER;
	rpc_s->local_smsg_attr.msg_buffer = rpc_s->rpc_mem;
	rpc_s->local_smsg_attr.buff_size = rpc_s->num_buf * (sizeof(struct rpc_cmd) + RECVHEADER);
	rpc_s->local_smsg_attr.mem_hndl = rpc_local_memory_handle;
	rpc_s->local_smsg_attr.mbox_offset = 0; // At the beginning of SMSG init, we put offset as 0. It changes while configuring the real peer.

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
}

int rpc_smsg_config(struct rpc_server *rpc_s, struct node_id *peer)
{
	int err = -ENOMEM;
	gni_return_t status;

	//configure the ep for RPC messages; rpc_msg init.

	rpc_s->local_smsg_attr.mbox_offset = rpc_s->num_buf * sizeof(struct rpc_cmd) * peer->ptlmap.id; 
	peer->remote_smsg_attr.mbox_offset = rpc_s->num_buf * sizeof(struct rpc_cmd) * rpc_s->ptlmap.id; 

	//peer_smsg_check(rpc_s, peer, &peer->remote_smsg_attr);

	status = GNI_SmsgInit(peer->ep_hndl, &rpc_s->local_smsg_attr, &(peer->remote_smsg_attr));
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_SmsgInit RPC returned error. %d.\n", status);
		goto err_free;
	}
	
	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;
}

int rpc_ep_smsg_config(struct rpc_server *rpc_s, struct node_id *peer)
{
	int err = -ENOMEM;
	gni_return_t status;

	status = GNI_EpCreate(rpc_s->nic_hndl, rpc_s->src_cq_hndl, &peer->ep_hndl);
	if (status != GNI_RC_SUCCESS)
	{
		printf("Rank %d: Fail GNI_EpCreate returned error. %d.\n", rpc_s->ptlmap.id, status);
		goto err_free;
	}

	status = GNI_EpBind(peer->ep_hndl, peer->ptlmap.nid, peer->ptlmap.id);
	if (status != GNI_RC_SUCCESS)
	{
		printf("Rank %d: Fail GNI_EpBind returned error. %d.\n", rpc_s->ptlmap.id, status);
		goto err_free;
	}

	err = rpc_smsg_config(rpc_s, peer);
	if (err != 0){
		printf("Rank %d: Fail for config SMSG for %d. (%d)\n", rpc_s->ptlmap.id, peer->ptlmap.id, err);
		goto err_out;
	}

	return 0;

err_free:
	printf("'%s()': failed with %d.\n", __func__, err);
        return err;
err_out:
	printf("'%s()': failed with %d.\n", __func__, status);
        return status;

}

// this function can only be used after rpc_server is fully initiated
static struct node_id *rpc_get_peer(struct rpc_server *rpc_s, int peer_id)
{
		return rpc_s->peer_tab + peer_id;
}

/* 
   Decode  and   service  a  command  message  received   in  the  rpc
   buffer. This routine is called by 'rpc_process_event()' in response
   to new incomming rpc request.
*/
static int rpc_cb_decode(struct rpc_server *rpc_s, struct rpc_request *rr)
{
	struct rpc_cmd *cmd;
	int err, i;




	cmd = (struct rpc_cmd *) (rr->msg->msg_rpc);

	for (i = 0; i < num_service; i++) 
	{
		if (cmd->cmd == rpc_commands[i].rpc_cmd) 
		{
			err = rpc_commands[i].rpc_func(rpc_s, cmd);
			break;
		}
	}

	if (i == num_service) {
		printf("Network command unknown %d!\n", cmd->cmd);
		err = -EINVAL;
	}

	if(err<0)
		printf("(%s): err.\n", __func__);

	return err;
}

/*
  Allocate   an  RPC  structure   for  communication   buffers.
*/
static struct rpc_request *rr_comm_alloc(int num_rpc)
{
	struct rpc_request *rr;
	size_t size;

	size = sizeof(*rr) + sizeof(*rr->msg) + sizeof(struct rpc_cmd) * num_rpc + 7;
	rr = malloc(size);
	if (!rr)
		return 0;

	memset(rr, 0, size);
	rr->cb = (async_callback)rpc_cb_decode;

	rr->msg = (struct msg_buf *) (rr+1);
	rr->msg->msg_data =  rr->msg->msg_rpc = NULL;
	rr->msg->size = sizeof(struct rpc_cmd);

	return rr;
}

/*
  Default  completion  routine for  rpc  messages  we initiate.   This
  routine is called by  'rpc_process_event()' to complete rpc requests
  which were locally initiated.
*/

static int rpc_cb_req_completion(struct rpc_server *rpc_s, struct rpc_request *rr)
{
	int err;
	gni_return_t status = GNI_RC_SUCCESS;

    if (rr->f_use_dart_mem) {
#ifdef DART_UGNI_PREALLOC_RDMA
        rr->refcont--;
        if (rr->refcont == 0) {
            if (rr->type == 1 || rr->f_data == 1) {
                if (rr->rr_type == DART_RPC_RECEIVE ||
                    rr->rr_type == DART_RPC_RECEIVE_DIRECT) {
                    void *buf = rr->msg->msg_data;
                    rr->msg->msg_data = rr->msg->original_msg_data;
                    memcpy(rr->msg->msg_data, buf, rr->msg->size);
                    rpc_dart_mem_free(buf);                    
                }

                if (rr->rr_type == DART_RPC_SEND ||
                    rr->rr_type == DART_RPC_SEND_DIRECT) {
                    rpc_dart_mem_free(rr->msg->msg_data);
                }
            }

            (*rr->msg->cb)(rpc_s, rr->msg);
        }
#endif
    } else {
        rr->refcont--;
        if (rr->refcont == 0) {
           if(rr->type == 1 || rr->f_data == 1)
           {
	            status = GNI_MemDeregister(rpc_s->nic_hndl, &rr->mdh_data);
	            if(status != GNI_RC_SUCCESS)
	            {
		            printf("(%s) Fail: GNI_MemDeregister returned error. (%d)\n", __func__, status);
		            return status;
	            }

           }

	       err = (*rr->msg->cb)(rpc_s, rr->msg);
	       if(err!=0)
                 return -1;
	    }
    }

	return 0;
}
/* =====================================================
	message passing and data transfer
 =====================================================*/
static int rpc_prepare_buffers(struct rpc_server *rpc_s, const struct node_id *peer, struct rpc_request *rr, enum io_dir dir)
{
	int err;
	gni_return_t status;
	gni_mem_handle_t mdh;

    if (rr->f_use_dart_mem) {
#ifdef DART_UGNI_PREALLOC_RDMA
        rr->msg->original_msg_data = rr->msg->msg_data;
        rr->msg->msg_data = rpc_dart_mem_malloc(rr->msg->size);
        if ( !rr->msg->msg_data) {
            printf("%s(): rpc_dart_mem_malloc failed size %u\n", __func__,
                rpc_s->ptlmap.id, rr->msg->size);
            err = -1;
            goto err_out;
        }
        //printf("%s(): original_msg_data= %llu msg_data= %llu size= %u\n",
        //        __func__, rr->msg->original_msg_data, rr->msg->msg_data, rr->msg->size);
        memcpy(rr->msg->msg_data, rr->msg->original_msg_data, rr->msg->size);

        rr->f_data = 1;
        rr->mdh_data = rr->msg->msg_rpc->mdh_addr.mdh = rpc_s->dart_mem_mdh;
        rr->msg->msg_rpc->mdh_addr.address = (uint64_t)rr->msg->msg_data;
        rr->msg->msg_rpc->mdh_addr.length = rr->msg->size;
        rr->msg->msg_rpc->mdh_addr.index = rr->index;
        rr->refcont++;
#endif
    } else {
        //No Vector Operation in this version
        //added for alignment
        status = GNI_MemRegister(rpc_s->nic_hndl, (uint64_t)rr->msg->msg_data, (uint64_t)(rr->msg->size), rpc_s->dst_cq_hndl, GNI_MEM_READWRITE, -1, &mdh);
        if (status != GNI_RC_SUCCESS)
        {
            printf("Fail: GNI_MemRegister returned error %d data size %u\n",
                status, rr->msg->size);
            err = -1;
            goto err_out;
        }

        rr->f_data = 1;
        rr->mdh_data = rr->msg->msg_rpc->mdh_addr.mdh = mdh;
        rr->msg->msg_rpc->mdh_addr.address = (uint64_t)rr->msg->msg_data;
        rr->msg->msg_rpc->mdh_addr.length = rr->msg->size;
        rr->msg->msg_rpc->mdh_addr.index = rr->index;
        rr->refcont++;
    }

	return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

//Generic routine to post a request message to a peer node
static int rpc_post_request(struct rpc_server *rpc_s, struct node_id *peer, struct rpc_request *rr, const struct hdr_sys *hs)
{
	int err;
	gni_return_t status = GNI_RC_SUCCESS;
	gni_post_descriptor_t rdma_data_desc;
	uint32_t local, remote;

	local = rr->index;

	uint32_t hdr_size = hs ? (uint32_t)(sizeof(struct hdr_sys)) : 0;

RESEND:

	if (rr->type == 0)
	{
		remote = rpc_s->ptlmap.id+INDEX_COUNT;

	        status = GNI_EpSetEventData(peer->ep_hndl, local, remote);
	        if(status != GNI_RC_SUCCESS)
	        {
		        printf("(%s) 1 Fail: GNI_EpSetEventData returned error. (%d)\n", __func__, status);
		        goto err_status;
	        }

		rr->mdh_rpc = rpc_s->local_smsg_attr.mem_hndl;

		status = GNI_SmsgSend(peer->ep_hndl, NULL, 0, (void *)rr->data, (uint32_t)rr->size, rr->index);// MSG_ID (last parameter) uses address of rr as reference
		if((status != GNI_RC_SUCCESS) && (status != GNI_RC_NOT_DONE))
		{
			printf("Fail: GNI_SmsgSend returned error. (%d)\n", status);
			err = -1;
			goto err_out;
		}

        if (status == GNI_RC_NOT_DONE) {
            // printf("%s(): GNI_SmsgSend returns GNI_RC_NOT_DONE but peer->num_msg_at_peer is %d\n", __func__, peer->num_msg_at_peer);
            if (rpc_s->cmp_type == DART_SERVER) {
                printf("%s(): GNI_RC_NOT_DONE should not happen on server\n",
                        __func__, peer->num_msg_at_peer);
                return 0;
            } else {
                peer->num_msg_at_peer = 0;
                while (peer->num_msg_at_peer <= 0) {
                    err = rpc_process_event_with_timeout(rpc_s, 1);
                    if (err < 0 && err != GNI_RC_TIMEOUT) {
                        printf("%s(): rpc_process_event_with_timeout err %d\n",
                                __func__, err);
                        break;
                    }
                }

                if (peer->num_msg_at_peer > 0) {
                    goto RESEND;
                }
            }
        }
	}

    if (rr->type == 1 && rr->f_use_dart_mem) {
#ifdef DART_UGNI_PREALLOC_RDMA
        remote = peer->mdh_addr.index;
        status = GNI_EpSetEventData(peer->ep_hndl, local, (uint32_t)remote);
        if(status != GNI_RC_SUCCESS)
        {
            printf("(%s) 2 Fail: GNI_EpSetEventData returned error. (%d)\n", __func__, status);
            goto err_status;
        }

        rr->msg->original_msg_data = rr->msg->msg_data;
        rr->msg->msg_data = rpc_dart_mem_malloc(rr->msg->size);
        if ( !rr->msg->msg_data) {
            printf("%s(): rpc_dart_mem_malloc failed size %u\n", __func__,
                rpc_s->ptlmap.id, rr->msg->size);
            err = -1;
            goto err_out;
        }
        //printf("%s(): original_msg_data= %llu msg_data= %llu size= %u\n",
        //        __func__, rr->msg->original_msg_data, rr->msg->msg_data, rr->msg->size);
        memcpy(rr->msg->msg_data, rr->msg->original_msg_data, rr->msg->size);

        rdma_data_desc.type = GNI_POST_RDMA_PUT;
        rdma_data_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT | GNI_CQMODE_REMOTE_EVENT;
        rdma_data_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
        rdma_data_desc.local_addr = (uint64_t) rr->msg->msg_data;
        rdma_data_desc.local_mem_hndl = rpc_s->dart_mem_mdh;
        rdma_data_desc.remote_addr = peer->mdh_addr.address;
        rdma_data_desc.remote_mem_hndl = peer->mdh_addr.mdh;
        rdma_data_desc.length = rr->msg->size;
        rdma_data_desc.rdma_mode = 0;
        rdma_data_desc.src_cq_hndl = rpc_s->src_cq_hndl;

        status = GNI_PostRdma(peer->ep_hndl, &rdma_data_desc);
        if (status != GNI_RC_SUCCESS)
        {
            printf("Fail: GNI_PostRdma returned error. %d\n", status);
            goto err_status;
        }
#endif
    } else if (rr->type == 1) {
        remote = peer->mdh_addr.index;
		status = GNI_EpSetEventData(peer->ep_hndl, local, (uint32_t)remote);
		if(status != GNI_RC_SUCCESS)
		{
		    printf("(%s) 2 Fail: GNI_EpSetEventData returned error. (%d)\n", __func__, status);
		    goto err_status;
		}
	
		status = GNI_MemRegister(rpc_s->nic_hndl, (uint64_t)rr->msg->msg_data, (uint64_t)(rr->msg->size), NULL, GNI_MEM_READWRITE, -1, &rr->mdh_data);
		if (status != GNI_RC_SUCCESS)
		{
		        printf("Fail: GNI_MemRegister returned error. %d\n", status);
			goto err_status;
		}
	
		rdma_data_desc.type = GNI_POST_RDMA_PUT;
		rdma_data_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT | GNI_CQMODE_REMOTE_EVENT;
		rdma_data_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
		rdma_data_desc.local_addr = (uint64_t) rr->msg->msg_data;
		rdma_data_desc.local_mem_hndl = rr->mdh_data;
		rdma_data_desc.remote_addr = peer->mdh_addr.address;
		rdma_data_desc.remote_mem_hndl = peer->mdh_addr.mdh;
		rdma_data_desc.length = rr->msg->size;
		rdma_data_desc.rdma_mode = 0;
		rdma_data_desc.src_cq_hndl = rpc_s->src_cq_hndl;

		status = GNI_PostRdma(peer->ep_hndl, &rdma_data_desc);
		if (status != GNI_RC_SUCCESS)
		{
		  printf("Fail: GNI_PostRdma returned error. %d\n", status);
			goto err_status;
		}
	}

	rr->refcont++;

	return 0;

err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;	
err_status:
	printf("'%s()': failed with %d.\n", __func__, status);
	return -status;	
}

static int rpc_fetch_request(struct rpc_server *rpc_s, const struct node_id *peer, struct rpc_request *rr)
{
	int err;
	gni_return_t status;
	gni_post_descriptor_t rdma_data_desc;
	uint32_t local, remote;

	local = rr->index;
	remote = peer->mdh_addr.index;

	status = GNI_EpSetEventData(peer->ep_hndl, (uint32_t)local, (uint32_t)remote);
	if(status != GNI_RC_SUCCESS)
	{
		printf("(%s) Fail: GNI_EpSetEventData returned error. (%d)\n", __func__, status);
		goto err_status;
	}

	if (rr->type == 1)
	{
        if (rr->f_use_dart_mem) {
#ifdef DART_UGNI_PREALLOC_RDMA
            rr->msg->original_msg_data = rr->msg->msg_data;
            rr->msg->msg_data = rpc_dart_mem_malloc(rr->msg->size);
            if ( !rr->msg->msg_data) {
                printf("%s(): rpc_dart_mem_malloc failed size %u\n", __func__,
                    rpc_s->ptlmap.id, rr->msg->size);
                err = -1;
                goto err_out;
            }                        
            //printf("%s(): original_msg_data= %llu msg_data= %llu size= %u\n",
            //    __func__, rr->msg->original_msg_data, rr->msg->msg_data, rr->msg->size);

            rdma_data_desc.type = GNI_POST_RDMA_GET;
            rdma_data_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT | GNI_CQMODE_REMOTE_EVENT; //?reconsider, need some tests.
            rdma_data_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
            rdma_data_desc.local_addr = (uint64_t) rr->msg->msg_data;
            rdma_data_desc.local_mem_hndl = rpc_s->dart_mem_mdh;
            rdma_data_desc.remote_addr = peer->mdh_addr.address;
            rdma_data_desc.remote_mem_hndl = peer->mdh_addr.mdh;
            rdma_data_desc.length = rr->msg->size;//Must be a multiple of 4-bytes for GETs
            rdma_data_desc.rdma_mode = 0;
            rdma_data_desc.src_cq_hndl = rpc_s->src_cq_hndl;
            status = GNI_PostRdma(peer->ep_hndl, &rdma_data_desc);
            if (status != GNI_RC_SUCCESS)
            {
                  if(status == 7)
                  {
                     printf("status == 7.\n");

                  }
                  printf("Fail: GNI_PostRdma returned error with %d.\n", status);
                  goto err_status;
            }
#endif
        } else {
            status = GNI_MemRegister(rpc_s->nic_hndl, (uint64_t)rr->msg->msg_data, (uint64_t)rr->msg->size, NULL, GNI_MEM_READWRITE, -1, &rr->mdh_data);
            if (status != GNI_RC_SUCCESS)
            {
              printf("Fail: GNI_MemRegister returned error with %d.\n", status);
                goto err_status;
            }

            rdma_data_desc.type = GNI_POST_RDMA_GET;
            rdma_data_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT | GNI_CQMODE_REMOTE_EVENT; //?reconsider, need some tests.
            rdma_data_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
            rdma_data_desc.local_addr = (uint64_t) rr->msg->msg_data;
            rdma_data_desc.local_mem_hndl = rr->mdh_data;
            rdma_data_desc.remote_addr = peer->mdh_addr.address;
            rdma_data_desc.remote_mem_hndl = peer->mdh_addr.mdh;
            rdma_data_desc.length = rr->msg->size;//Must be a multiple of 4-bytes for GETs
            rdma_data_desc.rdma_mode = 0;
            rdma_data_desc.src_cq_hndl = rpc_s->src_cq_hndl;
            status = GNI_PostRdma(peer->ep_hndl, &rdma_data_desc);
            if (status != GNI_RC_SUCCESS)
            {
                  if(status == 7)
                  {
                     printf("status == 7.\n");

                  }
                  printf("Fail: GNI_PostRdma returned error with %d.\n", status);
                  goto err_status;
            }
        }
	}

	rr->refcont++;
	return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;	
err_status:
	printf("'%s()': failed with %d.\n", __func__, status);
	return status;	
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer)
{
	struct rpc_request *rr;
	struct hdr_sys hs;
	int err, i;

	while (!list_empty(&peer->req_list))
	{
	    if(peer->num_msg_at_peer == 0)
	    {
	      if (rpc_s->cmp_type == DART_SERVER) {
               //printf("%s(): peer->num_msg_at_peer == 0 should not happen on server\n", __func__);                 
	           break;                         
          }                       	      

	      err = rpc_process_event_with_timeout(rpc_s, 1);
	      if (err < 0 && err != GNI_RC_TIMEOUT)
            goto err_out;
	    

	      continue;
	    }


		// Sending credit is good, we will send the message.

		rr = list_entry(peer->req_list.next, struct rpc_request, req_entry);
		if (rr->msg->msg_data)
		{
			err = rpc_prepare_buffers(rpc_s, peer, rr, rr->iodir);
			if (err != 0)
				goto err_out;

			/*if (rr->f_vec)
			  rr->f_vec = unset;*/
		}

		// post request
		err = rpc_post_request(rpc_s, peer, rr, 0);
		if (err != 0)
			goto err_out;

		// Message is sent, consume one credit. 
		peer->num_msg_at_peer--;
		list_del(&rr->req_entry);
		peer->num_req--;

		list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
		rpc_s->rr_num++;
	}

	return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;	
}

//This function sends back ACK message from rpc_s to peer. It returns underlying GNI credits to remote peer.
static int rpc_credit_return(struct rpc_server *rpc_s, struct node_id *peer)
{
    struct msg_buf *msg;
    int err;

    msg = msg_buf_alloc(rpc_s, peer, 1);
    if(!msg)
     goto err_out;
    msg->size = sizeof(struct rpc_cmd);
    msg->msg_rpc->cmd = cn_ack_credit;
    msg->msg_rpc->id = rpc_s->ptlmap.id;

    //peer->num_msg_at_peer++; // cn_ack_credit msg NOT consume send credit
    //printf("%s(): peer->num_req= %d\n", __func__, peer->num_req);
    //err = rpc_send(rpc_s, peer, msg);
    //if (err < 0) {
    //  free(msg);
    //  goto err_out;
    //}

    struct rpc_request *rr;
    rr = calloc(1, sizeof(struct rpc_request));
    if(!rr)
        goto err_out;

    rr->type = 0;//0 represents cmd ; 1 for data
    rr->msg = msg;
    rr->iodir = io_send;
    rr->cb = (async_callback)rpc_cb_req_completion;
    rr->data = msg->msg_rpc;
    rr->size = sizeof(*msg->msg_rpc);
    do
        rr->index = rpc_get_index();
    while(rr->index == -1);

    // post request
    err = rpc_post_request(rpc_s, peer, rr, 0);
    if (err != 0)
        goto err_out;

    list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
    rpc_s->rr_num++;

    return 0;
 err_out:
    printf("'%s()' failed with %d.\n", __func__, err);
    return err;
}

static int rpc_process_ack(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
  struct node_id *peer;
  peer = rpc_get_peer(rpc_s, (int)cmd->id);
  if(peer == NULL)
    {
      printf("(%s): rpc_get_peer err.\n", __func__);
      return -ENOMEM;
    }

  peer->num_msg_at_peer += RECVCREDIT;
  if (peer->num_msg_at_peer > SENDCREDIT)
    peer->num_msg_at_peer = SENDCREDIT;

  return 0;
}

/* =====================================================
	Public API
 =======================================================*/
/*
  Determine nic address or socket address of master node, i.e., {nid, pid}. First
  try to read the values from environment vars, then try the 'conf'
  file.
*/

int rpc_read_socket(struct sockaddr_in *address)
{
        char *ip;
	char *port;
        FILE *f;
        int err;
	char ipstore[16];
	char *tmp_ip;
	tmp_ip = ipstore;
	int tmp_port;

	system("sleep 5");

        ip = getenv("P2TNID");
        port = getenv("P2TPID");

        if (ip && port) {
                address->sin_addr.s_addr = inet_addr(ip);
                address->sin_port = htons(atoi(port));

                return 0;
        }

        f = fopen("conf", "rt");
        if (!f) {
		err = -ENOENT;
		goto err_out;
        }

        err = fscanf(f, "P2TNID=%s\nP2TPID=%d\n", tmp_ip, &tmp_port);

	address->sin_addr.s_addr = inet_addr(tmp_ip);
	address->sin_port = htons(tmp_port);

        fclose(f);
        if (err == 2)
                return 0;

	err = -EIO;

 err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_write_socket(struct rpc_server *rpc_s)
{
        FILE *f;
        int err;

        f = fopen("conf", "wt");
        if (!f)
                goto err_out;

        err = fprintf(f, "P2TNID=%s\nP2TPID=%d\n", inet_ntoa(rpc_s->address.address.sin_addr), ntohs(rpc_s->address.address.sin_port));

        if (err < 0)
                goto err_out_close;
        fclose(f);

        return 0;

 err_out_close:
        fclose(f);
 err_out:
        printf("'%s()' failed with %d.", __func__, err);
        return -EIO;
}

int rpc_read_config(struct ptlid_map *ptlmap)
{
        char *nid, *pid;
        FILE *f;
        int err;

        nid = getenv("P2TNID");
        pid = getenv("P2TPID");

        if (nid && pid) {
                ptlmap->nid = atoi(nid);
                ptlmap->pid = atoi(pid);

                return 0;
        }

        f = fopen("conf", "rt");
        if (!f) {
		err = -ENOENT;
		goto err_out;
        }

        err = fscanf(f, "P2TNID=%u\nP2TPID=%hu\n", 
                        &ptlmap->nid, &ptlmap->pid);

        fclose(f);
        if (err == 2)
                return 0;

	err = -EIO;

 err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_write_config(struct rpc_server *rpc_s)
{
        FILE *f;
        int err;

        f = fopen("conf", "wt");
        if (!f)
                goto err_out;

        err = fprintf(f, "P2TNID=%u\nP2TPID=%u\n", 
                        rpc_s->ptlmap.nid, rpc_s->ptlmap.pid);
        if (err < 0)
                goto err_out_close;
        fclose(f);

        return 0;
 err_out_close:
        fclose(f);
 err_out:
        printf("'%s()' failed with %d.", __func__, err);
        return -EIO;
}


inline static int __process_event (struct rpc_server *rpc_s, uint64_t timeout)
{
	gni_cq_handle_t cq_array[] = {rpc_s->src_cq_hndl, rpc_s->dst_cq_hndl, rpc_s->sys_cq_hndl};
	gni_cq_entry_t event_data = 0;
	uint64_t event_type;
	int event_id;
	gni_post_descriptor_t *post_des;
	gni_return_t status;

	struct node_id *peer;
	struct rpc_request *rr, *tmp;
	struct hdr_sys *hs;
	int err =  -ENOMEM;
	uint32_t n;
	int check=0;
	int cnt=0;
	void *tmpcmd;

	status = GNI_CqVectorWaitEvent(cq_array, 3, (uint64_t)timeout, &event_data, &n); 
	if (status == GNI_RC_TIMEOUT)
		return status;

	else if (status != GNI_RC_SUCCESS)
	{
		printf("(%s): GNI_CqVectorWaitEvent PROCESSING ERROR.\n", __func__);
		return status;
	}

	event_type = GNI_CQ_GET_TYPE(event_data);
	event_id = GNI_CQ_GET_MSG_ID(event_data);

	if(GNI_CQ_STATUS_OK(event_data) == 0)
		printf("Rank %d: receive event_id (%d) not done.\n",rank_id_pmi, event_id);

  if(n == 0)
    {
      if(1)
	{
	  if(event_id == 0)
	    return 0;
	  list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry)
	    {
	      if(rr->index == event_id)
		{
		  check=1;
		  break;
		}
	    }

	  while(!GNI_CQ_STATUS_OK(event_data));

	  if(check == 0)
	    {
	      printf("Rank %d: SRC Indexing err with event_id (%d), rr_num (%d) in (%s).\n", rank_id_pmi, event_id, rpc_s->rr_num,  __func__);
	      list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry)
		{
		  printf("Rank(%d):rest Index(%d) with rr_num(%d).\n",rank_id_pmi, rr->index, rpc_s->rr_num);
		}
	      goto err_out;
	    }


	  if(rr->type == 1)
	    {
	      status = GNI_GetCompleted(rpc_s->src_cq_hndl, event_data, &post_des);
	      if (status != GNI_RC_SUCCESS)
		{
		  printf("(%s): GNI_GetCompleted PROCESSING ERROR.\n", __func__);
		  goto err_status;
		}
	    }

	  err = rpc_cb_req_completion(rpc_s, rr);
	  if(err!=0)
	    goto err_out;

	  if(rr->refcont == 0)
	    {
	      list_del(&rr->req_entry);
	      rpc_s->rr_num--;
	      err = rpc_free_index(rr->index);
	      if(err!=0)
		goto err_out;
	      free(rr);
	    }
	}
    }

  if(n == 1)
    {
      if(event_id >= INDEX_COUNT)
	{
	  rr = rr_comm_alloc(0);
	  if(rr == NULL)
	    {
	      printf("rr_comm_alloc err (%d).\n", err);
	      goto err_out;
	    }
	  
	  peer = rpc_get_peer(rpc_s, (int)event_id-INDEX_COUNT);
	  if(peer == NULL)
	    {
	      printf("(%s): rpc_get_peer err.\n", __func__);
	      return -ENOMEM;
	    }

	  rr->msg->msg_rpc = calloc(1, sizeof(struct rpc_cmd));
	  if(rr->msg->msg_rpc == NULL)
	    {
	      printf("Rank %d: calloc error.\n", rank_id_pmi);
	      return -ENOMEM;
	    }

	  do
	    {
	      status = GNI_SmsgGetNext(peer->ep_hndl, (void **) &tmpcmd);
	      cnt++;
	    } while(status == GNI_RC_NOT_DONE);

	  cnt=0;

	  if(status == GNI_RC_NOT_DONE){ 
	    printf("Rank %d: GNI_RC_NOT_DONE.\n",rank_id_pmi);//debug
	    return 0;
	  }

	  if(status != GNI_RC_SUCCESS)
	    {
	      cnt=0;
	      printf("Rank %d: receive wrong event.\n", rank_id_pmi);//debug
	      free(rr);
	      goto err_out;
	    }

	  memcpy(rr->msg->msg_rpc, tmpcmd, sizeof(struct rpc_cmd));

	  do
	    {
	      status = GNI_SmsgRelease(peer->ep_hndl);
	      if(status != GNI_RC_SUCCESS && status != GNI_RC_NOT_DONE)
		{
			  printf("GNI_SmsgRelease failed with %d.\n", status);
			  goto err_status;
		}
	    }while(status == GNI_RC_NOT_DONE);

      if (rr->msg->msg_rpc->cmd != cn_ack_credit) {
        peer->num_msg_recv++;
      }

      if(peer->num_msg_recv == RECVCREDIT)
        {
          err = rpc_credit_return(rpc_s, peer);
          if(err!=0)
            goto err_out;
          peer->num_msg_recv = 0;
        }

	  err = rpc_cb_decode(rpc_s, rr);
	  if(err!=0)
	    goto err_out;

	  free(rr->msg->msg_rpc);
	  free(rr);
	  }

	 if( event_id < INDEX_COUNT )
	   {
	     while(!GNI_CQ_STATUS_OK(event_data));
		       
	     list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry)
	       {
		 if( rr->index == event_id )
		   {
		     check = 1;
		     break;
		   }
	       }
		     
	     if(check == 0)
	       {
		 printf("Rank %d: DST Indexing err with event_id (%d), rr_num (%d) in (%s).\n", rank_id_pmi, event_id, rpc_s->rr_num,  __func__);
		 list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry)
		   {
		     printf("Rank(%d):Index(%d) with rr_num(%d).\n",rank_id_pmi, rr->index, rpc_s->rr_num);
		   }

		 goto err_out;
	       }
		     
	     if(check == 1)
	       {

		 err = rpc_cb_req_completion(rpc_s, rr);
		 if(err!=0)
		   goto err_out;
		 if(rr->refcont == 0)
		   {
		     list_del(&rr->req_entry);
		     rpc_s->rr_num--;
		     err = rpc_free_index(rr->index);
		     if(err!=0)
		       goto err_out;
		     free(rr);
		   }
	       }
	   }
	 }

    if(n == 2)
      {
	if(event_id == rpc_s->ptlmap.id);
	if(event_id != rpc_s->ptlmap.id)
	  {
	    peer = rpc_get_peer(rpc_s, (int)event_id);
	    if(peer == NULL)
	      {
		printf("(%s): rpc_get_peer err.\n", __func__);
		return -ENOMEM;
	      }
	    do
	      {
		status = GNI_SmsgGetNext(peer->sys_ep_hndl, (void **) &hs);
	      } while(status != GNI_RC_SUCCESS);

	    err = sys_dispatch_event(rpc_s, hs);
	    if(err != 0)
	      goto err_out;
	    do
	      status = GNI_SmsgRelease(peer->sys_ep_hndl);
	    while(status == GNI_RC_NOT_DONE);
	    if(status != GNI_RC_SUCCESS)
	      {
		printf("GNI_SmsgRelease failed with (%d).\n", status);
		goto err_status;
	      }

	    peer->sys_msg_recv++;
	    if(peer->sys_msg_recv == RECVCREDIT)
	      {
		err = sys_credit_return(rpc_s, peer);
		if(err!=0)
		  {
		    printf("(%s): sys_credit_return failed with err (%d).\n", __func__, err);
		    return err;
		  }
		peer->sys_msg_recv = 0;
	     }
	 }

      }
	  
  return 0;

err_out:
  printf("(%s): err (%d).\n", __func__, err);
  return err;
err_status:
  printf("(%s): status (%d).\n", __func__, status);
  return status;
}

int rpc_process_msg_resend(struct rpc_server *rpc_s, struct node_id *peer_tab, int num_peer)
{
    struct node_id *peer;
    int i;
    for (i = 0; i < num_peer; i++) {
        peer = peer_tab + i;
        if (peer->num_req > 0 && peer->num_msg_at_peer > 0) {
            printf("%s(): %d resend to %d num_req= %d num_msg_at_peer= %d\n", __func__, 
                rpc_s->ptlmap.id, peer->ptlmap.id, peer->num_req, peer->num_msg_at_peer);
            peer_process_send_list(rpc_s, peer);
        }
    }

    return 0;
}

int rpc_process_event(struct rpc_server *rpc_s)
{
	int err;

	err = __process_event(rpc_s, 300);
	if(err == 0 || err == GNI_RC_TIMEOUT)
		return 0;

	printf("(%s): err (%d).\n", __func__, err);
	return err;
}

int rpc_process_event_with_timeout(struct rpc_server *rpc_s, int timeout)
{
	int err;

	err = __process_event(rpc_s, (uint64_t)timeout);
	if(err == 0 || err == GNI_RC_TIMEOUT)
		return err;

	printf("(%s): err (%d).\n", __func__, err);
	return err;
}

struct rpc_server *rpc_server_init(int num_buff, int num_rpc_per_buff, void *dart_ref, enum rpc_component cmp_type, int appid)
{
	struct rpc_server *rpc_s = 0;
	struct rpc_request *rr;

	int i, j, err = -ENOMEM;
	gni_return_t status;

	gni_mem_handle_t rpc_local_memory_handle;

	if (rpc_s_instance)
		return rpc_s_instance;

	rpc_s = calloc(1, sizeof(*rpc_s));
	if (!rpc_s)
		goto err_free;

	rpc_s->dart_ref = dart_ref;
	rpc_s->num_buf = num_buff;

	rpc_s->max_num_msg = SENDCREDIT;
	rpc_s->cmp_type = cmp_type;
	rpc_s->ptlmap.appid = appid;

	//Use PMI library to get necessary global information.
    PMI_BOOL initialized;
    if (PMI_SUCCESS == PMI_Initialized(&initialized) ) {
            if (PMI_TRUE != initialized) {
                    err = PMI_Init(&first_spawned);
                    assert(err == PMI_SUCCESS);
            }
    }
	err = PMI_Get_size(&num_of_rank_pmi);
	assert(err == PMI_SUCCESS);
	err = PMI_Get_rank(&rank_id_pmi);	
	assert(err == PMI_SUCCESS);

	rpc_s->ptlmap.id = rank_id_pmi;
	rpc_s->num_rpc_per_buff = num_rpc_per_buff;

	err = init_gni(rpc_s);
	if (err != 0)
		goto err_free;

	rpc_s->peer_tab = gather_node_id(appid);
	if(rpc_s->peer_tab == NULL)
		goto err_free;


        status = GNI_CqCreate(rpc_s->nic_hndl, ENTRY_COUNT, 0, GNI_CQ_BLOCKING, NULL, NULL, &rpc_s->sys_cq_hndl);
        if (status != GNI_RC_SUCCESS)
        {
                printf("Fail: GNI_CqCreate SYS returned error. %d.\n", status);
                goto err_out;
        }


	status = GNI_CqCreate(rpc_s->nic_hndl,ENTRY_COUNT, 0, GNI_CQ_BLOCKING, NULL, NULL, &rpc_s->src_cq_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CqCreate SRC returned error. %d.\n", status);
		goto err_out;
	}

	status = GNI_CqCreate(rpc_s->nic_hndl, ENTRY_COUNT, 0, GNI_CQ_BLOCKING, NULL, NULL, &rpc_s->dst_cq_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_CqCreate DST returned error. %d.\n", status);
		goto err_out;
	}

	err = PMI_Barrier();	
	assert(err == PMI_SUCCESS);

	INIT_LIST_HEAD(&rpc_s->rpc_list);
	err = rpc_index_init(rpc_s);
	if (err != 0)
		goto err_free;


	rpc_s->bar_tab = malloc(sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);
	if (!rpc_s->bar_tab) {
		err = -ENOMEM;
		goto err_free;
	}
	memset(rpc_s->bar_tab, 0, sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);

	rpc_add_service(cn_ack_credit, rpc_process_ack);

#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rpc_dart_mem_init(rpc_s, 16*1024*1024);
    }
#endif

	//	printf("rpc_cmd size is %d.\n", sizeof(struct rpc_cmd));

	// Init succeeded, set the instance reference here.
	rpc_s_instance = rpc_s;
	return rpc_s;

err_free:
	free(rpc_s);
	printf("'%s()': failed with %d.\n", __func__, err);
        return 0;
err_out:
	free(rpc_s);
	printf("'%s()': failed with %d.\n", __func__, status);
        return 0;	
}

static int rpc_server_finish(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	int i, err;
	
	peer=rpc_s->peer_tab;
	for(i=0;i<rpc_s->num_peers; i++, peer++)
	{
		while (peer->num_req)
		{
			err = peer_process_send_list(rpc_s, peer);
			if (err<0)
				printf("'%s()': encountered an error %d, skipping.\n", __func__, err);
		}
	}
	return 0;
}


int rpc_server_free(struct rpc_server *rpc_s)
{
	gni_return_t status;
	struct rpc_request *rr, *tmp;
	struct node_id *peer;
	int err, i;
	struct rr_index *ri, *ri_tmp;

	rpc_server_finish(rpc_s);

	/* From this  point on,  we should  not accept  any new
	   incomming  requests; so,  if they  still arrive,  we should
	   just  drop them and  drain the  event queue  to be  able to
	   release resources. */

	/* Process any remaining events from the event queue. */
	while (rpc_s->rr_num != 0) 
	{
	        err = rpc_process_event_with_timeout(rpc_s, 100);
		{
			if(err != 0 && err != GNI_RC_TIMEOUT)
				printf("'%s()': error at flushing the event queue %d!\n", __func__, err);
		}
	}

#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rpc_dart_mem_finalize(rpc_s);
    }
#endif

	//Free memory to index_list
	list_for_each_entry_safe(ri, ri_tmp, &index_list, struct rr_index, index_entry)
	{
		list_del(&ri->index_entry);
		free(ri);
	}

	// Clean system message related
        sys_cleanup(rpc_s);

	// Clean rpc_smsg_init
	status = GNI_MemDeregister(rpc_s->nic_hndl, &rpc_s->local_smsg_attr.mem_hndl);
        if (status != GNI_RC_SUCCESS)
	  {
	    printf("Fail: GNI_MemDeregister returned error. %d.\n", status);
	    goto err_out;
	  }	
	
	free(rpc_s->rpc_mem);

	for(i=0; i < rpc_s->num_rpc_per_buff; i++)
	{
	  if(rpc_s->peer_tab[i].ptlmap.id==rpc_s->ptlmap.id)
	    continue;

		status = GNI_EpUnbind(rpc_s->peer_tab[i].ep_hndl); //Unbind the remote address from the endpoint handler.
		if (status != GNI_RC_SUCCESS && status != GNI_RC_NOT_DONE) 
		{
			printf("Fail: GNI_EpUnbind returned error. %d.\n", status);
			goto err_out;
		}
		status = GNI_EpDestroy(rpc_s->peer_tab[i].ep_hndl); //You must do an EpDestroy for each endpoint pair.
		if (status != GNI_RC_SUCCESS) 
		{
			printf("Fail: GNI_EpDestroy returned error. %d.\n", status);
			goto err_out;
		}
	}

	status = GNI_CqDestroy(rpc_s->src_cq_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_MemDestory returned error. %d.\n", status);
		goto err_out;
	}

	status = GNI_CqDestroy(rpc_s->dst_cq_hndl);
	if (status != GNI_RC_SUCCESS) 
	{
		printf("Fail: GNI_MemDestory returned error. %d.\n", status);
		goto err_out;
	}

	// Clean GNI related
        clean_gni(rpc_s);

        free(rpc_s);

	PMI_Barrier();

	PMI_Finalize();

	return 0;

err_out:
	return status;
}

/*
static void list_credits(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	int i;

	for (i=0; i < rpc_s->num_peers; i++)
	{
		peer = rpc_s->peer_tab + i;
		printf("Peer %d : credits for remote Peer %d: {send = %d, return %d}\n", rpc_s->ptlmap.id, peer->ptlmap.id, peer->num_msg_at_peer, peer->num_msg_ret);
	}
}

*/////

struct rpc_server *rpc_server_get_instance(void)
{
	/* Blindly  get  the  rpc_server   reference;  if  it  is  not
	   initialized, should call rpc_server_init() */
	return rpc_s_instance;
}

/*
  Return the id of the rpc server.
*/
int rpc_server_get_id(void)
{
	// TODO: if server is not initialized, should return -1.
	return rpc_s_instance->ptlmap.id;
}

void rpc_server_set_peer_ref(struct rpc_server *rpc_s, struct node_id peer_tab[], int num_peers)
{
	rpc_s->num_peers = num_peers;
	rpc_s->peer_tab = peer_tab;
}

void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff)
{
	rpc_s->num_rpc_per_buff = num_rpc_per_buff;
}

/*
  System barrier implementation.
*/
// PMI barrier
int rpc_barrier(struct rpc_server *rpc_s)
{
  int err = PMI_Barrier();
  if(err == PMI_SUCCESS)
    return 0;
  else
    goto err_out;

 err_out:
  printf("Rank %d: (%s) failed (%d).\n", rank_id_pmi, __func__, err);
  return err;
}

// original barrier
/*
int rpc_barrier(struct rpc_server *rpc_s)
{
	//	struct node_id *peer;
	int round, np;
	int next, prev;
	int err;

	np = log2_ceil(rpc_s->app_num_peers);
	round = -1;

	rpc_s->bar_num = (rpc_s->bar_num + 1) & 0xFF;

	while (round < np-1) 
	{
		round = round + 1;

		next = (myrank(rpc_s) + (1 << round)) % rpc_s->app_num_peers;
		prev = (rpc_s->app_num_peers + myrank(rpc_s) - (1 << round)) % rpc_s->app_num_peers;

		err = sys_bar_send(rpc_s, rank2id(rpc_s, next));////
		if (err != 0)
			goto err_out;

		SYS_WAIT_COMPLETION(rpc_s->bar_tab[prev] == rpc_s->bar_num || rpc_s->bar_tab[prev] == ((rpc_s->bar_num+1) & 0xFF))
	}

	return 0;

 err_out:
	printf("Rank %d: (%s) failed (%d).\n", rank_id_pmi, __func__, err);
	return err;
}
*/

//rpc operation
void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)
{
	rpc_commands[num_service].rpc_cmd = rpc_cmd;
	rpc_commands[num_service].rpc_func = rpc_func;
	num_service++;
}

//Added by Tong: to decouple DART layer and DataSpaces layer
void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
  peer->mdh_addr = cmd->mdh_addr;
  //debug SCA
  // printf("Rank %d: peer mdh_addr index is %d, cmd->mdh_addr->index is %d.\n", rpc_s_instance->ptlmap.id, peer->mdh_addr.index, cmd->mdh_addr.index);
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg,
                        struct rpc_cmd *cmd) {
    return;
}

struct msg_buf *msg_buf_alloc (struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs)
{
	struct msg_buf *msg;
	size_t size;

	size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd) * num_rpcs + 7;
	msg = calloc(1, size);
	if (!msg)
		return NULL;

	msg->peer = peer;
	msg->cb = default_completion_callback;
	if (num_rpcs > 0)
	{
		msg->msg_rpc = (struct rpc_cmd *)(msg+1);
		ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
		msg->msg_rpc->dstnid = peer->ptlmap.nid;
		msg->msg_rpc->dstpid = peer->ptlmap.pid;
		msg->msg_rpc->srcnid = rpc_s->ptlmap.nid;
		msg->msg_rpc->srcpid = rpc_s->ptlmap.pid;
	}

	return msg;
}

//message/data transfer functions
/*
  Generic interface to send a rpc message to a remote node; peer is subject to flow control.
*/
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

    rr->type = 0;//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->iodir = io_send;
	rr->cb = (async_callback)rpc_cb_req_completion;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rr->f_use_dart_mem = 1;
        rr->rr_type = DART_RPC_SEND;
    } else rr->f_use_dart_mem = 0;
#else
    rr->f_use_dart_mem = 0;
#endif
	do
		rr->index = rpc_get_index();
	while(rr->index == -1);

	list_add_tail(&rr->req_entry, &peer->req_list);
	peer->num_req++;

	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

inline static int __send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg, flag_t f_vec)
{
	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

    rr->type = 1;//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->cb = (async_callback)rpc_cb_req_completion;
	rr->data = msg->msg_data;
	rr->size = msg->size;
	rr->f_vec = 0;
#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rr->f_use_dart_mem = 1;
        rr->rr_type = DART_RPC_SEND_DIRECT;
    } 
#else
    rr->f_use_dart_mem = 0;
#endif
	do
		rr->index = rpc_get_index();
	while(rr->index == -1);

	list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
	rpc_s->rr_num++;

	err = rpc_post_request(rpc_s, peer, rr, 0);
	if (err != 0)
	{
        free(rr);
		goto err_out;
	}

	return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	int err;
	
	err = __send_direct(rpc_s, peer, msg, unset);
	if (err == 0)
		return 0;

	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}
//not be used in GEMINI version. Just keep an empty func here
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	return 0;
}

int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

    rr->type = 1;//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->cb = (async_callback)rpc_cb_req_completion;
	rr->data = msg->msg_data;
	rr->size = msg->size;
#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rr->f_use_dart_mem = 1;
        rr->rr_type = DART_RPC_RECEIVE_DIRECT;
    } else rr->f_use_dart_mem = 0;
#else
    rr->f_use_dart_mem = 0;
#endif
	do
		rr->index = rpc_get_index();
	while(rr->index == -1);

	list_add_tail(&rr->req_entry, &rpc_s->rpc_list);
	rpc_s->rr_num++;

	err = rpc_fetch_request(rpc_s, peer, rr);
	if (err == 0)
		return 0;
err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

inline static int __receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg, flag_t f_vec)
{
	struct rpc_request *rr;
	int err = -ENOMEM;

	rr = calloc(1, sizeof(struct rpc_request));
	if(!rr)
		goto err_out;

    rr->type = 0;//0 represents cmd ; 1 for data
	rr->msg = msg;
	rr->iodir = io_receive;
	rr->cb = (async_callback)rpc_cb_req_completion;
	rr->data = msg->msg_rpc;
	rr->size = sizeof(*msg->msg_rpc);
	rr->f_vec = 0;
#ifdef DART_UGNI_PREALLOC_RDMA
    if (rpc_s->cmp_type == DART_CLIENT) {
        rr->f_use_dart_mem = 1;
        rr->rr_type = DART_RPC_RECEIVE;
    } else rr->f_use_dart_mem = 0;
#else
    rr->f_use_dart_mem = 0;
#endif
	do
		rr->index = rpc_get_index();
	while(rr->index == -1);

	list_add_tail(&rr->req_entry, &peer->req_list);
	peer->num_req++;

	err = peer_process_send_list(rpc_s, peer);
	if(err == 0)
		return 0;

err_out:
	printf("'%s()': failed with %d.\n", __func__, err);
	return err;
}

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	int err;
	
	err = __receive(rpc_s, peer, msg, unset);
	if(err == 0)
		return 0;

	printf("'%s()': failed with %d.\n", __func__, err);
	return err;	
}

void rpc_report_md_usage(struct rpc_server *rpc_s)
{
	printf("'%s()': MD posted %d, MD released %d, MD in use %d.\n", __func__, rpc_s->num_md_posted, rpc_s->num_md_unlinked, rpc_s->num_md_unlinked - rpc_s->num_md_posted);
}

//Not be used in GEMINI version. 
int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	return 0;
}

// for debug:
void rpc_smsg_check(struct rpc_server *rpc_s){
  printf("Rank %d: rpc_s->local_smsg_attr[type(%d),maxcredit(%d),maxsize(%d),buffer(%d),buff_size(%d), mem_hndl(%ld,%ld), offset(%d)]\n", rpc_s->ptlmap.id, rpc_s->local_smsg_attr.msg_type, rpc_s->local_smsg_attr.mbox_maxcredit, rpc_s->local_smsg_attr.msg_maxsize, rpc_s->local_smsg_attr.msg_buffer, rpc_s->local_smsg_attr.buff_size, rpc_s->local_smsg_attr.mem_hndl.qword1, rpc_s->local_smsg_attr.mem_hndl.qword2, rpc_s->local_smsg_attr.mbox_offset);
}

void sys_smsg_check(struct rpc_server *rpc_s){
  printf("Rank %d: rpc_s->sys_local_smsg_attr[type(%d),maxcredit(%d),maxsize(%d),buffer(%d),buff_size(%d), mem_hndl(%ld,%ld), offset(%d)]\n", rpc_s->ptlmap.id, rpc_s->sys_local_smsg_attr.msg_type, rpc_s->sys_local_smsg_attr.mbox_maxcredit, rpc_s->sys_local_smsg_attr.msg_maxsize, rpc_s->sys_local_smsg_attr.msg_buffer, rpc_s->sys_local_smsg_attr.buff_size, rpc_s->sys_local_smsg_attr.mem_hndl.qword1, rpc_s->sys_local_smsg_attr.mem_hndl.qword2, rpc_s->sys_local_smsg_attr.mbox_offset);
}

void peer_smsg_check(struct rpc_server *rpc_s, struct node_id *peer, gni_smsg_attr_t *smsg_attr){
  printf("Rank %d: peer(%d) [type(%d),maxcredit(%d),maxsize(%d),buffer(%d),buff_size(%d), mem_hndl(%ld,%ld), offset(%d)]\n", rpc_s->ptlmap.id, peer->ptlmap.id, smsg_attr->msg_type, smsg_attr->mbox_maxcredit, smsg_attr->msg_maxsize, smsg_attr->msg_buffer, smsg_attr->buff_size, smsg_attr->mem_hndl.qword1, smsg_attr->mem_hndl.qword2, smsg_attr->mbox_offset);
}
