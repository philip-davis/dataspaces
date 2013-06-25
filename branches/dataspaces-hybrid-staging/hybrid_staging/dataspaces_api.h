#ifndef __DATASPACES_API_H__
#define __DATASPACES_API_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

enum component_type {
	IN_SITU = 1,
	IN_TRANSIT
};

enum op_type {	
	TOPOLOGY = 1,
	VISUALIZATION,
	DESCRIPTIVE_STATS,
	AUTOCORRELATION
};

struct data_descriptor {
	size_t size; // size of the data to be transferred
	int rank; // rank of the process
	int tstep; // time step
	int type; // type of application
	int num_obj; // number of data objects 
};

struct data_item {
	struct list_head item_entry;
	struct data_descriptor desc;
	void * buf; // pointer to retrieved data block
}; 

/* Initialize the dataspaces library. */
int ds_init(int num_peers, enum component_type type);

/* Finalize the dataspaces library. */
int ds_finalize();

/* Put/write memory block. */ 
int ds_put_obj_data(void * in_ptr, struct data_descriptor * in_desc);

/*
 Request to get job from RR & get data blocks from insitu RDMA memory & chain retrieved blocks into a data item list. 
 Output: the application type and head pointer to the data item list.
*/
int ds_request_job(enum op_type *type, struct list_head *head);

/*
  Currently this function is only invoked by in-transit driver code NOT user's in-transit operation code.
*/
int ds_free_data_list(struct list_head *head);

//TO-DO: remove
int ds_rank();
void ds_do_barrier();
#ifdef __cplusplus
}
#endif

#endif
