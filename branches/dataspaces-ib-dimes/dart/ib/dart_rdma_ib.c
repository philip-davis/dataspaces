#ifdef DS_HAVE_DIMES
#include <stdint.h>
#include "dart_rdma_ib.h"

static struct dart_rdma_handle *drh = NULL;

int dart_rdma_init(struct rpc_server *rpc_s)
{
	return -1;
}

int dart_rdma_finalize()
{
	return -1;
}

int dart_rdma_register_mem(struct dart_rdma_mem_handle *mem_hndl,
                           void *data, size_t bytes)
{
	return -1;
}

int dart_rdma_deregister_mem(struct dart_rdma_mem_handle *mem_hndl)

{
	return -1;
}

int dart_rdma_set_memregion_to_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                   struct rpc_cmd *cmd)
{
	return -1;
}

int dart_rdma_get_memregion_from_cmd(struct dart_rdma_mem_handle *mem_hndl,
                                   struct rpc_cmd *cmd)
{
	return -1;
}

int dart_rdma_create_read_tran(struct node_id *remote_peer,
                struct dart_rdma_read_tran **pp)
{
	return -1;
}

int dart_rdma_delete_read_tran(int tran_id)
{
	return -1;
}

int dart_rdma_schedule_read(int tran_id, size_t src_offset, size_t dst_offset,
                size_t bytes)
{
	return -1;
}

int dart_rdma_perform_reads(int tran_id)
{
	return -1;
}

int dart_rdma_check_reads(int tran_id)
{
	return -1;
}

#endif
