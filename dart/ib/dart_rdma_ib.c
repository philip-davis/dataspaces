#include "dart_rdma_ib.h"

#ifdef DS_HAVE_DIMES
#include <stdint.h>
#include <poll.h>

#include "debug.h"

static struct dart_rdma_handle *drh = NULL;
static int ibv_rdma_error_flag = 0; // used to propagate rdma error state from check_reads() to process_reads()

static struct dart_rdma_tran *dart_rdma_find_read_tran(int tran_id, struct list_head *tran_list)
{
	struct dart_rdma_tran *read_tran = NULL;
	list_for_each_entry(read_tran, tran_list, struct dart_rdma_tran, entry) {
		if(read_tran->tran_id == tran_id) {
			return read_tran;
		}
	}

	return NULL;
}

static int dart_rdma_get(struct dart_rdma_tran *read_tran, struct dart_rdma_op *read_op)
{
	int err = -ENOMEM;
	struct ibv_send_wr *bad_wr = NULL;

	// Configure rdma work request descriptor
	memset(&read_op->wr, 0, sizeof(struct ibv_send_wr));
	read_op->wr.wr_id = (uintptr_t) read_op;	// use address of the read_op as the unique wr id
	read_op->wr.opcode = IBV_WR_RDMA_READ;
	read_op->wr.sg_list = &read_op->sge;
	read_op->wr.num_sge = 1;
	read_op->wr.send_flags = IBV_SEND_SIGNALED;
	read_op->wr.wr.rdma.remote_addr = read_tran->src.mr.addr + read_op->src_offset;
	read_op->wr.wr.rdma.rkey = read_tran->src.mr.rkey;

	read_op->sge.addr = read_tran->dst.mr.addr + read_op->dst_offset;
	read_op->sge.length = read_op->bytes;
	read_op->sge.lkey = read_tran->dst.mr.lkey;

	err = ibv_post_send(read_tran->remote_peer->rpc_conn.qp, &read_op->wr, &bad_wr);
	if(err < 0) {
		uloga("%s(): ibv_post_send() failed\n", __func__);
		goto err_out;
	}

	return 0;
      err_out:
	ERROR_TRACE();
}

static int dart_rdma_process_ibv_cq(struct dart_rdma_tran *read_tran)
{
	struct rpc_server *rpc_s = drh->rpc_s;
	struct node_id *peer = read_tran->remote_peer;
	struct pollfd pollfd_array[1];
	int ret, err;
	struct ibv_cq *ev_cq;
	struct ibv_wc wc;
	void *ev_ctx;
	struct dart_rdma_op *read_op = NULL;

	if(peer->rpc_conn.f_connected) {
		err = ibv_get_cq_event(peer->rpc_conn.comp_channel, &ev_cq, &ev_ctx);
		if(err == -1 && errno == EAGAIN) {
			return 0;
		}

		if(err == -1 && errno != EAGAIN) {
			uloga("%s(): failed with ibv_get_cq_event: %s\n", __func__, strerror(errno));
			err = errno;
			goto err_out;
		}

		ibv_ack_cq_events(ev_cq, 1);
		err = ibv_req_notify_cq(ev_cq, 0);
		if(err != 0) {
			uloga("%s(): failed with ibv_req_notify_cq.\n", __func__);
			goto err_out;
		}

		do {
			ret = ibv_poll_cq(ev_cq, 1, &wc);
			if(ret < 0) {
				uloga("%s(): failed with ibv_poll_cq.\n", __func__);
				err = ret;
				goto err_out;
			}

			if(ret == 0)
				continue;

			if(wc.status != IBV_WC_SUCCESS) {
				uloga("%s(): wc.status (%d).\n", __func__, wc.status);
				err = -wc.status;
				goto err_out;
			}

			if(wc.opcode == IBV_WC_RECV || wc.opcode == IBV_WC_SEND || wc.opcode == IBV_WC_RDMA_WRITE) {
				uloga("%s(): should not happen wc.opcode= %u\n", __func__, wc.opcode);
				return -1;
			}

			if(wc.opcode == IBV_WC_RDMA_READ) {
				read_op = (struct dart_rdma_op *) (uintptr_t) wc.wr_id;
				list_del(&read_op->entry);
				free(read_op);
				read_tran->avail_rdma_credit++;
			} else {
				uloga("%s(): weird wc.opcode= %u\n", __func__, wc.opcode);
			}
		} while(ret);
	} else {
		uloga("%s(): peer->rpc_conn.f_connected= %d\n", __func__, peer->rpc_conn.f_connected);
		return -1;
	}

	return 0;
      err_out:
	ERROR_TRACE();
}

static int dart_perform_local_copy(struct dart_rdma_tran *tran)
{
    struct dart_rdma_op *op, *t;
    list_for_each_entry_safe(op, t, &tran->read_ops_list,
                struct dart_rdma_op, entry) {
        memcpy(tran->dst.base_addr + op->dst_offset,
               tran->src.base_addr + op->src_offset,
               op->bytes);
        list_del(&op->entry);
        free(op);
    }

#ifdef DEBUG
    uloga("%s(): read tran %d complete.\n", __func__, tran->tran_id);
#endif
    return 0;
}

int dart_rdma_init(struct rpc_server *rpc_s)
{
	int err = -ENOMEM;
	if(drh) {
		uloga("%s(): dart rdma already init!\n", __func__);
		return 0;
	}

	drh = (struct dart_rdma_handle *) malloc(sizeof(*drh));
	memset(drh, 0, sizeof(*drh));

	drh->rpc_s = rpc_s;
	INIT_LIST_HEAD(&drh->read_tran_list);

	return 0;
      err_out:
	ERROR_TRACE();
}

int dart_rdma_finalize()
{
	if(drh) {
		free(drh);
	}
	return 0;
}

int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl, void *data, size_t bytes)
{
	int err = -ENOMEM;
	if(!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}
	// Register the RDMA memory region
	mem_hndl->mr_ref = ibv_reg_mr(drh->rpc_s->global_pd, data, bytes, IBV_ACCESS_LOCAL_WRITE | IBV_ACCESS_REMOTE_READ | IBV_ACCESS_REMOTE_WRITE);
	if(mem_hndl->mr_ref == NULL) {
		uloga("%s(): ibv_reg_mr() failed.\n", __func__);
		goto err_out;
	}

	mem_hndl->mr = *mem_hndl->mr_ref;
	mem_hndl->size = bytes;
	mem_hndl->base_addr = (uint64_t) data;

	return 0;
      err_out:
	ERROR_TRACE();
}

int dart_rdma_deregister_mem(struct dart_rdma_mem_handle *mem_hndl)
{
	int err = -ENOMEM;
	// Deregister the RDMA memory region
	err = ibv_dereg_mr(mem_hndl->mr_ref);
	if(err != 0) {
		uloga("%s(): ibv_dereg_mr() failed with %d.\n", __func__, err);
		goto err_out;
	}

	return 0;
      err_out:
	ERROR_TRACE();
}

int dart_rdma_set_memregion_to_cmd(struct dart_rdma_mem_handle *mem_hndl, struct rpc_cmd *cmd)
{
	cmd->mr = mem_hndl->mr;
	return 0;
}

int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl, struct rpc_cmd *cmd)
{
	mem_hndl->mr = cmd->mr;
	return 0;
}

int dart_rdma_create_read_tran(struct node_id *remote_peer, struct dart_rdma_tran **pp)
{
    if (!remote_peer) uloga("%s(): ERROR remote_peer is NULL.\n", __func__);
	if (!remote_peer->rpc_conn.f_connected &&
        drh->rpc_s->ptlmap.id != remote_peer->ptlmap.id) {
#ifdef DEBUG
		uloga("%s(): #%d to connect peer #%d\n", __func__, drh->rpc_s->ptlmap.id, remote_peer->ptlmap.id);
#endif
		rpc_connect(drh->rpc_s, remote_peer);
	}

	static int tran_id_ = 0;
	struct dart_rdma_tran *read_tran = (struct dart_rdma_tran *)
		malloc(sizeof(*read_tran));
	if(read_tran == NULL) {
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
	struct dart_rdma_tran *read_tran = dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if(read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n", __func__, tran_id);
		return -1;
	}

	if(!list_empty(&read_tran->read_ops_list)) {
		uloga("%s(): read tran with id= %d not complete!\n", __func__, tran_id);
		return -1;
	}

	list_del(&read_tran->entry);
	free(read_tran);
	return 0;
}

int dart_rdma_schedule_read(int tran_id, size_t src_offset, size_t dst_offset, size_t bytes)
{
	int err = -ENOMEM;
	if(!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		return -1;
	}

	struct dart_rdma_tran *read_tran = dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if(read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n", __func__, tran_id);
		return -1;
	}
	// Add the RDMA read operation into the transaction list
	struct dart_rdma_op *read_op = (struct dart_rdma_op *) malloc(sizeof(*read_op));
	if(read_op == NULL) {
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
	// Delayed reads in the check function.

    if(!drh) {
        uloga("%s(): dart rdma not init!\n", __func__);
        return -1;
    }

    struct dart_rdma_tran *read_tran = dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
    if(read_tran == NULL) {
        uloga("%s(): read tran with id= %d not found!\n", __func__, tran_id);
        return -1;
    }

    // if the data is on local process
    if (drh->rpc_s->ptlmap.id == read_tran->remote_peer->ptlmap.id ) {
        dart_perform_local_copy(read_tran);
    }
	return 0;
}

#ifdef DS_HAVE_DIMES_SHMEM
int dart_rdma_perform_reads_local(int tran_id)
{
    if(!drh) {
        uloga("%s(): dart rdma not init!\n", __func__);
        return -1;
    }

    struct dart_rdma_tran *read_tran = dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
    if(read_tran == NULL) {
        uloga("%s(): read tran with id= %d not found!\n", __func__, tran_id);
        return -1;
    }

    dart_perform_local_copy(read_tran);
    return 0;
}
#endif

int dart_rdma_check_reads(int tran_id)
{
	// Block till all RDMA reads for current transaction complete
	// TODO: need to check reads completion and block for each transaction of a DIMES GET. How about non-blocking?
	int err = -ENOMEM;
	int done = 0;

	if(!drh) {
		uloga("%s(): dart rdma not init!\n", __func__);
		goto err_out;
	}

	struct dart_rdma_tran *read_tran = dart_rdma_find_read_tran(tran_id, &drh->read_tran_list);
	if(read_tran == NULL) {
		uloga("%s(): read tran with id= %d not found!\n", __func__, tran_id);
		goto err_out;
	}

	read_tran->avail_rdma_credit = drh->rpc_s->num_buf;

	int cnt = 0;
	struct dart_rdma_op *read_op;
	list_for_each_entry(read_op, &read_tran->read_ops_list, struct dart_rdma_op, entry) {
		while(read_tran->avail_rdma_credit <= 0) {
			err = dart_rdma_process_ibv_cq(read_tran);
			if(err < 0) {
				goto err_out_rdma;
			}
		}

		err = dart_rdma_get(read_tran, read_op);
		if(err < 0) {
			goto err_out_rdma;
		}
		read_tran->avail_rdma_credit--;
		cnt++;
	}

	while(!list_empty(&read_tran->read_ops_list)) {
		err = dart_rdma_process_ibv_cq(read_tran);
		if(err < 0) {
			goto err_out_rdma;
		}
	}

#ifdef DEBUG
	uloga("%s(): tran_id=%d num_read_ops=%d available_rdma_credit= %d\n", __func__, tran_id, cnt, read_tran->avail_rdma_credit);
#endif

#ifdef DEBUG
	uloga("%s(): read transaction %d complete!\n", __func__, tran_id);
#endif

	done = 1;
	return done;
 err_out_rdma:
    ibv_rdma_error_flag = 1;
 err_out:
	return done;
}

int dart_rdma_process_reads()
{
    if (ibv_rdma_error_flag) {
        ibv_rdma_error_flag = 0;
        return -1;
    }
	return 0;
}
#endif
