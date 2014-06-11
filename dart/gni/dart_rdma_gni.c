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
*  Fan Zhang (2013) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#include <stdint.h>
#include "dart_rdma_gni.h"

#ifdef DS_HAVE_DIMES

#define MAX_NUM_RDMA_OP_POSTED 65536

static struct dart_rdma_handle *drh = NULL;
static int tran_id_seed = 0;

static struct dart_rdma_tran*
common_dart_rdma_find_tran(int tran_id, enum dart_rdma_tran_type type)
{
    struct list_head *tran_list = NULL;
    if (type == dart_rdma_tran_type_read) {
        tran_list = &drh->read_tran_list;
    } else if (type == dart_rdma_tran_type_write) {
        tran_list = &drh->write_tran_list;
    }

	struct dart_rdma_tran *tran = NULL;
	list_for_each_entry(tran, tran_list,
		struct dart_rdma_tran, entry) {
		if (tran->tran_id == tran_id) {
			return tran;
		}
	}

    uloga("%s(): tran with id %d not found!\n", __func__, tran_id);
	return NULL;
}

static int common_dart_rdma_create_tran(struct node_id *remote_peer,
                                        struct dart_rdma_tran **pp,
                                        enum dart_rdma_tran_type type)
{
    struct dart_rdma_tran *tran = (struct dart_rdma_tran*)
                                    malloc(sizeof(*tran));
    if (tran == NULL) {
        return -1;
    }
    tran->tran_id = tran_id_seed++;
    tran->remote_peer = remote_peer;
    tran->num_rdma_op = 0;
    tran->f_all_rdma_op_posted = 0;
    tran->type = type;
    INIT_LIST_HEAD(&tran->rdma_op_list);
    if (type == dart_rdma_tran_type_read) {
        list_add(&tran->entry, &drh->read_tran_list);
    } else if (type == dart_rdma_tran_type_write) {
        list_add(&tran->entry, &drh->write_tran_list);
    }

    *pp = tran;
    return 0;
}

static int common_dart_rdma_delete_tran(int tran_id, enum dart_rdma_tran_type type)
{
    struct dart_rdma_tran *tran = NULL;
    tran = common_dart_rdma_find_tran(tran_id, type);
    if (tran == NULL) {
        return -1;
    }

    if (!list_empty(&tran->rdma_op_list)) {
        uloga("%s(): dart tran with id= %d not complete!\n",
            __func__, tran_id);
        return -1;
    }

    list_del(&tran->entry);
    free(tran);
    return 0;
}

static int common_dart_rdma_schedule_op(int tran_id, enum dart_rdma_tran_type type, size_t src_offset, size_t dst_offset, size_t bytes)
{
    int err = -ENOMEM;
    if (!drh) {
        uloga("%s(): dart rdma not init!\n", __func__);
        return -1;
    }

    struct dart_rdma_tran *tran = NULL;
    tran = common_dart_rdma_find_tran(tran_id, type);
    if (tran == NULL) {
        return -1;
    }

    // Add the RDMA operation into the transaction list
    struct dart_rdma_op *op = (struct dart_rdma_op*)malloc(sizeof(*op));
    if (op == NULL) {
        uloga("%s(): malloc() failed\n", __func__);
        return -1;
    }

    memset(op, 0, sizeof(*op));
    op->tran_id = tran_id;
    op->src_offset = src_offset;
    op->dst_offset = dst_offset;
    op->bytes = bytes;

    list_add(&op->entry, &tran->rdma_op_list);
    tran->num_rdma_op++;

    return 0;
}

static int common_dart_rdma_check_tran(int tran_id, enum dart_rdma_tran_type type) 
{
    int err = -ENOMEM;
    int done = 0;
    if (!drh) {
        uloga("%s(): dart rdma not init!\n", __func__);
        return done;
    }

    struct dart_rdma_tran *tran = NULL;
    tran = common_dart_rdma_find_tran(tran_id, type);
    if (tran == NULL) {
        return done;
    }

    if (!tran->f_all_rdma_op_posted) {
        return done;
    }

    if (list_empty(&tran->rdma_op_list)) {
        done = 1;
    }

    return done;
}

static int dart_perform_local_copy(struct dart_rdma_tran *tran)
{
	struct dart_rdma_op *op, *t;
	list_for_each_entry_safe(op, t, &tran->rdma_op_list,
				struct dart_rdma_op, entry) {
		memcpy((void *)(tran->dst.base_addr + op->dst_offset),
			   (void *)(tran->src.base_addr + op->src_offset),
			   op->bytes);
		list_del(&op->entry);
		free(op);
	}

	tran->f_all_rdma_op_posted = 1;
	return 0;
}

static int dart_rdma_get(struct dart_rdma_tran *read_tran,
                         struct dart_rdma_op *read_op)
{
	gni_return_t ret;
	int err = -ENOMEM;

	// Configure rdma post descriptor
	read_op->post_desc.type = GNI_POST_RDMA_GET;
	read_op->post_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
	read_op->post_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
	read_op->post_desc.local_addr = read_tran->dst.base_addr +
                                    read_op->dst_offset;
	read_op->post_desc.local_mem_hndl = read_tran->dst.mdh;
	read_op->post_desc.remote_addr = read_tran->src.base_addr +
                                     read_op->src_offset;
	read_op->post_desc.remote_mem_hndl = read_tran->src.mdh;
	read_op->post_desc.length = read_op->bytes;
	read_op->post_desc.rdma_mode = 0;
	read_op->post_desc.src_cq_hndl = drh->post_cq_hndl;
	ret = GNI_PostRdma(read_tran->remote_peer->ep_hndl,&read_op->post_desc);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_PostRdma() failed %d\n", __func__, ret);
		err = -ret;
		goto err_out;
	}

	drh->num_rdma_op_posted++;
	return 0;
err_out:
	ERROR_TRACE();
}

static int dart_rdma_put(struct dart_rdma_tran *write_tran,
                         struct dart_rdma_op *write_op)
{
    gni_return_t ret;
    int err = -ENOMEM;

    // Configure rdma post descriptor
    write_op->post_desc.type = GNI_POST_RDMA_PUT;
    write_op->post_desc.cq_mode = GNI_CQMODE_GLOBAL_EVENT;
    write_op->post_desc.dlvr_mode = GNI_DLVMODE_PERFORMANCE;
    write_op->post_desc.local_addr = write_tran->src.base_addr +
                                     write_op->src_offset;
    write_op->post_desc.local_mem_hndl = write_tran->src.mdh;
    write_op->post_desc.remote_addr = write_tran->dst.base_addr +
                                      write_op->dst_offset;
    write_op->post_desc.remote_mem_hndl = write_tran->dst.mdh;
    write_op->post_desc.length = write_op->bytes;
    write_op->post_desc.rdma_mode = 0;
    write_op->post_desc.src_cq_hndl = drh->post_cq_hndl;
    ret = GNI_PostRdma(write_tran->remote_peer->ep_hndl,&write_op->post_desc);
    if (ret != GNI_RC_SUCCESS) {
        uloga("%s(): GNI_PostRdma() failed %d\n", __func__, ret);
        err = -ret;
        goto err_out;
    }

    drh->num_rdma_op_posted++;
    return 0;
 err_out:
    ERROR_TRACE();
}

static int dart_rdma_process_post_cq(uint64_t timeout)
{
	int err = -ENOMEM;
	gni_return_t ret;
	gni_cq_entry_t event_data = 0;
	gni_post_descriptor_t *pd;

	ret = GNI_CqWaitEvent(drh->post_cq_hndl, timeout, &event_data);
	if (ret == GNI_RC_TIMEOUT)
		return 0;
	else if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_CqWaitEvent() failed %d\n", __func__, ret);
        err = -ret;
		goto err_out;
	}

	//while(!GNI_CQ_STATUS_OK(event_data)); // TODO(fan): Need this?

	ret = GNI_GetCompleted(drh->post_cq_hndl, event_data, &pd);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_GetCompleted() failed %d\n", __func__, ret);
        err = -ret;
		goto err_out;
	}

	struct dart_rdma_op *op = NULL;
	// Get struct address from the field address
    op = (struct dart_rdma_op *)
			(((uintptr_t)pd)-offsetof(struct dart_rdma_op, post_desc));
	list_del(&op->entry);
	free(op);

	drh->num_rdma_op_posted--;
	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_init(struct rpc_server *rpc_s)
{
	gni_return_t ret;
	int err = -ENOMEM;

	if (drh) {
		uloga("%s(): dart rdma already init!\n", __func__);
		return 0;
	}

	drh = (struct dart_rdma_handle*)malloc(sizeof(*drh));
	memset(drh, 0, sizeof(*drh));

	drh->rpc_s = rpc_s;
	INIT_LIST_HEAD(&drh->read_tran_list);
    INIT_LIST_HEAD(&drh->write_tran_list);
	drh->num_rdma_op_posted = 0;

	// Create completion queue for RDMA post operation
	ret = GNI_CqCreate(drh->rpc_s->nic_hndl, MAX_NUM_RDMA_OP_POSTED,
		0, GNI_CQ_BLOCKING, NULL, NULL, &drh->post_cq_hndl);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_CqCreate() failed with %d\n",
						__func__, ret);
		goto err_out_free;
	}

	return 0;
err_out_free:
	free(drh);
	drh = NULL;
err_out:
	ERROR_TRACE();
}

int dart_rdma_finalize()
{
	int err = -ENOMEM;

	if (drh) {
		gni_return_t ret = GNI_CqDestroy(drh->post_cq_hndl);
		if (ret != GNI_RC_SUCCESS) {
			uloga("%s(): GNI_CqDestroy() failed with %d\n",
							__func__, ret);
			goto err_out;
		}

		free(drh);
		drh = NULL;
	}
	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl,
									 void *data, size_t bytes)
{
	int err = -ENOMEM;
	gni_return_t ret;

	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	// Register the RDMA memory region
	ret = GNI_MemRegister(drh->rpc_s->nic_hndl, (uint64_t)data, (uint64_t)bytes,
									NULL, GNI_MEM_READWRITE, -1, &mem_hndl->mdh);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_MemRegister() failed with %d\n",
						__func__, ret);
		goto err_out;
	}

	mem_hndl->size = bytes;
	mem_hndl->base_addr = (uint64_t)data;

	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_deregister_mem(struct dart_rdma_mem_handle *mem_hndl)
{
	int err = -ENOMEM;
	gni_return_t ret;

	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	// Deregister the RDMA memory region
	ret = GNI_MemDeregister(drh->rpc_s->nic_hndl, &mem_hndl->mdh);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_MemDeregister() failed with %d\n",
						__func__, ret);
		goto err_out;
	}

	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_schedule_read(int tran_id, size_t src_offset, size_t dst_offset,
							size_t bytes)
{
    return common_dart_rdma_schedule_op(tran_id, dart_rdma_tran_type_read,
                src_offset, dst_offset, bytes);
}

int dart_rdma_perform_reads(int tran_id)
{
	int err = -ENOMEM;
	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	struct dart_rdma_tran *tran = NULL;
    tran = common_dart_rdma_find_tran(tran_id, dart_rdma_tran_type_read);
	if (tran == NULL) {
		return -1;
	}

#ifdef DEBUG
	uloga("%s(): tran_id= %d num_rdma_op= %d\n", __func__, tran_id, tran->num_rdma_op);
#endif
	if (drh->rpc_s->ptlmap.id == tran->remote_peer->ptlmap.id) {
		dart_perform_local_copy(tran);
		return 0;
	}

	struct dart_rdma_op *op;
	list_for_each_entry(op, &tran->rdma_op_list, struct dart_rdma_op, entry) {
        while (drh->num_rdma_op_posted >= MAX_NUM_RDMA_OP_POSTED) {
            err = dart_rdma_process_post_cq(1);
            if (err < 0) {
                return -1;
            }
        }

		err = dart_rdma_get(tran, op);
		if (err < 0) {
			goto err_out;
		}
	}

	tran->f_all_rdma_op_posted = 1;
	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_check_reads(int tran_id)
{
    return common_dart_rdma_check_tran(tran_id, dart_rdma_tran_type_read);
}

int dart_rdma_process_reads()
{
    return dart_rdma_process_post_cq(50);
}

int dart_rdma_create_read_tran(struct node_id *remote_peer,
                               struct dart_rdma_tran **pp)
{
    return common_dart_rdma_create_tran(remote_peer, pp,
                                        dart_rdma_tran_type_read);
}

int dart_rdma_delete_read_tran(int tran_id)
{
    return common_dart_rdma_delete_tran(tran_id, dart_rdma_tran_type_read);
}

int dart_rdma_create_write_tran(struct node_id *remote_peer,
                                struct dart_rdma_tran **pp)
{
    return common_dart_rdma_create_tran(remote_peer, pp, dart_rdma_tran_type_write);
}

int dart_rdma_delete_write_tran(int tran_id)
{
    return common_dart_rdma_delete_tran(tran_id, dart_rdma_tran_type_write);
}

int dart_rdma_schedule_write(int tran_id, size_t src_offset, size_t dst_offset, size_t bytes)
{
   return common_dart_rdma_schedule_op(tran_id, dart_rdma_tran_type_write,
            src_offset, dst_offset, bytes);
}

int dart_rdma_perform_writes(int tran_id)
{
    int err = -ENOMEM;
    if (!drh) {
        uloga("%s(): dart rdma not init!\n", __func__);
        return -1;
    }

    struct dart_rdma_tran *tran = NULL;
    tran = common_dart_rdma_find_tran(tran_id, dart_rdma_tran_type_write);
    if (tran == NULL) {
        return -1;
    }

#ifdef DEBUG
    uloga("%s(): tran_id= %d num_rdma_op= %d\n", __func__, tran_id, tran->num_rdma_op);
#endif
    if (drh->rpc_s->ptlmap.id == tran->remote_peer->ptlmap.id) {
        dart_perform_local_copy(tran);
        return 0;
    }

    struct dart_rdma_op *op;
    list_for_each_entry(op, &tran->rdma_op_list,
        struct dart_rdma_op, entry) {
        while (drh->num_rdma_op_posted >= MAX_NUM_RDMA_OP_POSTED) {
            err = dart_rdma_process_post_cq(1);
            if (err < 0) {
                return -1;
            }
        }

        err = dart_rdma_put(tran, op);
        if (err < 0) {
            goto err_out;
        }
    }

    tran->f_all_rdma_op_posted = 1;
    return 0;
err_out:
    ERROR_TRACE();
}

int dart_rdma_check_writes(int tran_id)
{
    return common_dart_rdma_check_tran(tran_id, dart_rdma_tran_type_write);
}

int dart_rdma_process_writes()
{
    return dart_rdma_process_post_cq(50);
}

int dart_rdma_set_memregion_to_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                   struct rpc_cmd *cmd)
{
	cmd->mdh_addr.mdh = mem_hndl->mdh;
	cmd->mdh_addr.length = mem_hndl->size;
	cmd->mdh_addr.address = mem_hndl->base_addr;
	return 0;
}

int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                     struct rpc_cmd *cmd)
{
	mem_hndl->mdh = cmd->mdh_addr.mdh;
	mem_hndl->size = cmd->mdh_addr.length;
	mem_hndl->base_addr = cmd->mdh_addr.address;
	return 0;
}

#endif
