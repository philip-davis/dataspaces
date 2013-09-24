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
*  Fan Zhang (2011) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#ifdef DS_HAVE_DIMES

#include "dart_rdma_dcmf.h"
#include "debug.h"

static struct dart_rdma_handle *drh = NULL;

/*
static int dart_rdma_insert_read_op(struct list_head *tran_list,
                                struct dart_rdma_read_op *read_op)
{
        int find_tran = 0;
        struct dart_rdma_read_tran *read_tran = NULL;
        list_for_each_entry(read_tran, tran_list,
                        struct dart_rdma_read_tran, entry) {
                if (read_tran->tran_id == read_op->tran_id) {
                        find_tran = 1;
                        break;
                }
        }

        if (!find_tran) {
                // Add new read transaction
                read_tran = (struct dart_rdma_read_tran *)
                                malloc(sizeof(*read_tran));
                read_tran->tran_id = read_op->tran_id;
                INIT_LIST_HEAD(&read_tran->read_ops_list);
                list_add(&read_tran->entry, tran_list);
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

static void dart_rdma_cb_get_completion(void *clientdata, DCMF_Error_t *err_dcmf)
{
	struct dart_rdma_read_op *read_op = (struct dart_rdma_read_op*)clientdata;
	list_del(&read_op->entry);
	free(read_op);
	return;
}

static int dart_perform_local_reads(struct dart_rdma_read_tran *read_tran)
{
	struct dart_rdma_read_op *read_op, *t;
	list_for_each_entry_safe(read_op, t, &read_tran->read_ops_list,
				struct dart_rdma_read_op, entry) {
		memcpy(read_tran->dst.base_addr + read_op->dst_offset,
			read_tran->src.base_addr + read_op->src_offset,
			read_op->bytes);
		list_del(&read_op->entry);
		free(read_op);
	}

	return 0;	
}

static int dart_rdma_get(struct dart_rdma_read_tran *read_tran,
                         struct dart_rdma_read_op *read_op)
{
	int err = -ENOMEM;
	DCMF_Request_t *request =
			(DCMF_Request_t*)calloc(1, sizeof(DCMF_Request_t));

	// Get remote data
	DCMF_Callback_t cb_get_done =
			{dart_rdma_cb_get_completion, (void*)read_op};
	DCMF_Result ret = DCMF_Get(&drh->dcmf_get_protocol,
							request,
							cb_get_done,
							DCMF_MATCH_CONSISTENCY,
							read_tran->remote_peer->ptlmap.rank_dcmf,
							read_op->bytes,
							&read_tran->src.memregion,
							&read_tran->dst.memregion,
							read_op->src_offset,
							read_op->dst_offset);
	if (ret != DCMF_SUCCESS) {
		uloga("%s(): DCMF_Get failed with %d\n", __func__, ret);
		goto err_out_free;
	}

	return 0;
err_out_free:
	free(request);
err_out:
	ERROR_TRACE();
}

int dart_rdma_init(struct rpc_server *rpc_s)
{
	int err = -ENOMEM;

	if (drh) {
		uloga("%s(): dart rdma already init!\n", __func__);
		return 0;
	}

	drh = (struct dart_rdma_handle*)malloc(sizeof(*drh));
	memset(drh, 0, sizeof(*drh));

	drh->rpc_s = rpc_s;
	INIT_LIST_HEAD(&drh->read_tran_list);

	// Register the p2p get protocol implementation.
	DCMF_Get_Configuration_t dcmf_get_config;
	dcmf_get_config.protocol = DCMF_DEFAULT_GET_PROTOCOL;
	dcmf_get_config.network = DCMF_DEFAULT_NETWORK;

	DCMF_Result ret = DCMF_Get_register(&drh->dcmf_get_protocol,
                                            &dcmf_get_config);
	if (ret != DCMF_SUCCESS) {
		uloga("%s(): DCMF_Get_register() failed with %d\n",
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
	if (drh) {
		free(drh);
	}
	return 0;
}

/*
  [in/out] mem_hndl 
*/
int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl,
                           void *data, size_t bytes)
{
	int err = -ENOMEM;

	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	// Register the RDMA memory region
	size_t bytes_out;
	DCMF_Result ret = DCMF_Memregion_create(
			&mem_hndl->memregion,
			&bytes_out,
			bytes,
			data,
			0);

	if (ret != DCMF_SUCCESS) {
		uloga("%s(): DCMF_Memregion_create() failed with %d\n",
				__func__, ret);
		goto err_out;
	}

#ifdef DEBUG
	if (bytes_out != bytes) {
		uloga("%s(): ERROR bytes_out=%u, bytes_in=%u\n",
				__func__, bytes_out, bytes);
	}
#endif
	mem_hndl->size = bytes;
	mem_hndl->base_addr = data;

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

	if (!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	// Deregister the RDMA memory region
	DCMF_Result ret = DCMF_Memregion_destroy(&mem_hndl->memregion);
	if (ret != DCMF_SUCCESS) {
		uloga("%s(): DCMF_Memregion_destroy() failed with %d\n",
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

	if (drh->rpc_s->ptlmap.id == read_tran->remote_peer->ptlmap.id) {
		dart_perform_local_reads(read_tran);
		return 0;
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
		err = rpc_process_event(drh->rpc_s);
		if (err < 0) {
			uloga("%s(): rpc_process_event failed\n", __func__);
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
	memcpy(&cmd->mem_region, &mem_hndl->memregion,
		   sizeof(DCMF_Memregion_t));
	cmd->mem_size = mem_hndl->size;
	return 0;
}

int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                     struct rpc_cmd *cmd)
{
	memcpy(&mem_hndl->memregion, &cmd->mem_region,
		   sizeof(DCMF_Memregion_t));
	mem_hndl->size = cmd->mem_size;
	mem_hndl->base_addr = NULL; 
	return 0;
}

#endif
