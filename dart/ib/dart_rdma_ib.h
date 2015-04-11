/*
 *  Fan Zhang (2013) TASSL Rutgers University
 *  zhangfan@cac.rutgers.edu
 */

#ifndef __DART_RDMA_IB_H__
#define __DART_RDMA_IB_H__

#include "config.h"

#ifdef DS_HAVE_DIMES
#include "dart_rpc_ib.h"

enum dart_memory_type {
    dart_memory_non_rdma = 0,
    dart_memory_rdma,
};

struct dart_rdma_mem_handle {
	// TODO: fix using both mr and mr_ref
	struct ibv_mr mr;	// memory region 
	struct ibv_mr *mr_ref;	// keep the pointer value returned by ibv_reg_mr
	uint64_t base_addr;
	size_t size;
    enum dart_memory_type mem_type;
};

struct dart_rdma_op {
	struct list_head entry;
	int tran_id;
	struct ibv_sge sge;	// describes a scatter/gather enry TODO: ??
	struct ibv_send_wr wr;	// describes the work request to the send queue of the queue pair
	size_t src_offset;
	size_t dst_offset;
	size_t bytes;
	int ret;
};

struct dart_rdma_tran {
	struct list_head entry;
	struct list_head read_ops_list;
	int tran_id;
	struct node_id *remote_peer;
	struct dart_rdma_mem_handle src;
	struct dart_rdma_mem_handle dst;
	int avail_rdma_credit;
};

struct dart_rdma_handle {
	struct rpc_server *rpc_s;
	struct list_head read_tran_list;
};

int dart_rdma_init(struct rpc_server *rpc_s);
int dart_rdma_finalize();

int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl, void *data, size_t bytes);
int dart_rdma_deregister_mem(struct dart_rdma_mem_handle *mem_hndl);

int dart_rdma_set_memregion_to_cmd(struct dart_rdma_mem_handle *mem_hndl, struct rpc_cmd *cmd);
int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl, struct rpc_cmd *cmd);

int dart_rdma_create_read_tran(struct node_id *remote_peer, struct dart_rdma_tran **pp);
int dart_rdma_delete_read_tran(int tran_id);

int dart_rdma_schedule_read(int tran_id, size_t src_offset, size_t dst_offset, size_t bytes);
int dart_rdma_perform_reads(int tran_id);
int dart_rdma_process_reads();
int dart_rdma_check_reads(int tran_id);
#endif

#endif
