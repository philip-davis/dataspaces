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
#ifdef DS_HAVE_DIMES
#include <stdint.h>
#include "dart_rdma_gni.h"

static struct dart_rdma_handle *drh = NULL;

/*
static int dart_rdma_insert_read_op(struct list_head *tran_list,
                                struct dart_rdma_read_op *read_op)
{
	int find_tran = 0;
	struct dart_rdma_read_tran *read_tran = NULL;
	list_for_each_entry(read_tran, tran_list, entry) {
					if (read_tran->tran_id == read_op->tran_id) {
									find_tran = 1;
									break;
					}
	}

	if (!find_tran) {
					uloga("%s(): Failed to find read tran with id=%d\n",
									__func__, read_op->tran_id);
					return -1;
	}

	list_add(&read_op->entry, &read_tran->read_ops_list);

	return 0;
}
*/

static struct dart_rdma_read_tran*
dart_rdma_find_read_tran(int tran_id, struct list_head *tran_list) {
	struct dart_rdma_read_tran *read_tran = NULL;
	list_for_each_entry(read_tran, tran_list,
		struct dart_rdma_read_tran, entry) {
		if (read_tran->tran_id == tran_id) {
			return read_tran;
		}
	}

	return NULL;
}

static int dart_rdma_get(struct dart_rdma_read_tran *read_tran,
											 struct dart_rdma_read_op *read_op)
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
		uloga("%s(): GNI_CqWaitEvent() failed\n", __func__);
		goto err_out;
	}

	while(!GNI_CQ_STATUS_OK(event_data)); // TODO(fan): Need this?

	ret = GNI_GetCompleted(drh->post_cq_hndl, event_data, &pd);
	if (ret != GNI_RC_SUCCESS) {
		uloga("%s(): GNI_GetCompleted() failed\n", __func__);
		goto err_out;
	}

	struct dart_rdma_read_op *read_op = NULL;
	// Get struct address from the field address
	read_op = (struct dart_rdma_read_op *)
							(((uintptr_t)pd)-offsetof(struct dart_rdma_read_op, post_desc));
	list_del(&read_op->entry);
	free(read_op);

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

	// Create completion queue for RDMA post operation
	ret = GNI_CqCreate(drh->rpc_s->nic_hndl, 65535, 0, GNI_CQ_BLOCKING, NULL,
					NULL, &drh->post_cq_hndl);
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

/*
[in/out] mem_hndl 
*/
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

/*
[in/out] mem_hndl 
*/
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
	int err = -ENOMEM;
	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	struct dart_rdma_read_tran *read_tran =
					dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if (read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n",
						__func__, tran_id);
		return -1;
	}

	// Add the RDMA read operation into the transaction list
	struct dart_rdma_read_op *read_op =
					(struct dart_rdma_read_op*)malloc(sizeof(*read_op));
	if (read_op == NULL) {
		uloga("%s(): malloc() failed\n", __func__);
		return -1;
	}

	memset(read_op, 0, sizeof(*read_op));
	read_op->tran_id = tran_id;
	read_op->src_offset = src_offset;
	read_op->dst_offset = dst_offset;
	read_op->bytes = bytes;

	list_add(&read_op->entry, &read_tran->read_ops_list);
/*
#ifdef DEBUG
			uloga("%s(): read_op->src_offset= %u, read_op->dst_offset= %u,"
							" read_op->bytes= %u, src_addr= %u, dst_addr= %u\n", __func__,
							read_op->src_offset, read_op->dst_offset, read_op->bytes,
							read_tran->src.base_addr, read_tran->dst.base_addr); 
#endif
*/
	return 0;
}

int dart_rdma_perform_reads(int tran_id)
{
	int err = -ENOMEM;
	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	struct dart_rdma_read_tran *read_tran =
					dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if (read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n",
							__func__, tran_id);
		return -1;
	}

	int cnt = 0;
	struct dart_rdma_read_op *read_op;
	list_for_each_entry(read_op, &read_tran->read_ops_list,
		struct dart_rdma_read_op, entry) {
		err = dart_rdma_get(read_tran, read_op);
		if (err < 0) {
			goto err_out;
		}
		cnt++;
	}

#ifdef DEBUG
	uloga("%s(): tran_id=%d num_read_ops=%d\n", __func__, tran_id, cnt);
#endif

	return 0;
err_out:
	ERROR_TRACE();
}

int dart_rdma_check_reads(int tran_id)
{
	// Block till all RDMA reads complete
	int err = -ENOMEM;
	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	struct dart_rdma_read_tran *read_tran =
					dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if (read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n",
						__func__, tran_id);
		return -1;
	}

	while (!list_empty(&read_tran->read_ops_list))
	{
		err = dart_rdma_process_post_cq(1);
		if (err < 0) {
			uloga("%s(): process_rdma_post_cq() failed\n", __func__);
			return -1;
		}
	}

#ifdef DEBUG
	uloga("%s(): read transaction %d complete!\n", __func__, tran_id);
#endif
	return 0;
}

/*
int dart_rdma_gen_read_tran_id()
{
	static int tran_id_ = 0;
	return tran_id_++;
}
*/

int dart_rdma_create_read_tran(struct node_id *remote_peer,
                               struct dart_rdma_read_tran **pp)
{
	static int tran_id_ = 0;
	struct dart_rdma_read_tran *read_tran = (struct dart_rdma_read_tran*)
									malloc(sizeof(*read_tran));
	if (read_tran == NULL) {
		return -1;
	}
	read_tran->tran_id = tran_id_++;
	read_tran->remote_peer = remote_peer;
	INIT_LIST_HEAD(&read_tran->read_ops_list);
	list_add(&read_tran->entry, &drh->read_tran_list);

	*pp = read_tran;
	return 0;
}

int dart_rdma_delete_read_tran(int tran_id)
{
	struct dart_rdma_read_tran *read_tran =
					dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if (read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n",
						__func__, tran_id);
		return -1;
	}

	if (!list_empty(&read_tran->read_ops_list)) {
		uloga("%s(): read tran with id= %d not complete!\n",
						__func__, tran_id);
		return -1;
	}

	list_del(&read_tran->entry);
	free(read_tran);
	return 0;
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
