#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "dataspaces_api.h"
#include "mpi.h"

struct stats {
	double mean;
	double mom2;
	double mom3;
	double mom4;
	double minF;
	double maxF;
};

struct variables {
	int numSamples;
	int numStats;
	struct stats st[0];
};

static int validate_data(void *data, size_t size, int type, int rank, int tstep)
{
	int num_double = size/sizeof(double);
	double value = type*10000 + rank + 0.00001*tstep;
	double *tmp = (double *)data;
	int i, ret = 1;
	for (i=0; i<num_double; i++) {
		if (tmp[i] != value)
			ret = 0;
	}

	return ret;
}

static int perform_intransit_topology(int rank, struct list_head *data_list)
{
	struct data_item *item;
	int count=0, is_valid = 1;
	int type, tstep;
	list_for_each_entry(item, data_list, struct data_item, item_entry) {
		if ( count == 0 ) {
			type = item->desc.type;
			tstep = item->desc.tstep;
		}

		if ( !validate_data(item->buf, item->desc.size, item->desc.type,
						item->desc.rank, item->desc.tstep) )
			is_valid = 0;

		if ( type != item->desc.type || tstep != item->desc.tstep )
			is_valid = 0;

		/*
		printf("%s(): Bucket %d, get size=%u rank=%d tstep=%d num_obj=%d is_valid=%d\n",
			__func__, rank, item->desc.size, item->desc.rank, item->desc.tstep, item->desc.num_obj, is_valid);
		*/
		count++;
	}

	uloga("%s(): Bucket %d, tstep=%d, is_valid=%d, get %d data objs\n",
			__func__, rank, tstep, is_valid, count);
	return 0;
}

static int perform_intransit_viz(int rank, struct list_head *data_list)
{
	struct data_item *item;
	int count=0, is_valid = 1;
	int type, tstep;
	list_for_each_entry(item, data_list, struct data_item, item_entry) {
		if ( count == 0 ) {
			type = item->desc.type;
			tstep = item->desc.tstep;
		}

		if ( !validate_data(item->buf, item->desc.size, item->desc.type,
							item->desc.rank, item->desc.tstep) )
			is_valid = 0;

		if ( type != item->desc.type || tstep != item->desc.tstep )
			is_valid = 0;
		/*
		printf("%s(): Bucket %d, get size=%u rank=%d tstep=%d num_obj=%d is_valid=%d\n",
			__func__, rank, item->desc.size, item->desc.rank, item->desc.tstep, item->desc.num_obj, is_valid);
		*/
		count++;
	}

	uloga("%s(): Bucket %d, tstep=%d, is_valid=%d, get %d data objs\n",
			__func__, rank, tstep, is_valid, count);
	return 0;
}

static int perform_intransit_stat_v1(int rank, struct list_head *data_list)
{
	struct data_item *item;
	int count=0, is_valid = 1;
	int type, tstep;
	list_for_each_entry(item, data_list, struct data_item, item_entry) {
		struct variables *var = (struct variables *)item->buf;
		struct stats *st = (struct stats *)(var->st);
		double val = item->desc.tstep + 0.00001*item->desc.rank;
		int r;
		if ( 0 == count) {
			type = item->desc.type;
			tstep = item->desc.tstep;
		}

		/*
		printf("%s(): var->numSamples=%d, var->numStats= %d\n", 
				__func__, var->numSamples, var->numStats);
		*/
		for (r=0; r < var->numStats; r++) {
			if (st[r].mean != val)
				is_valid = 0;
			if (st[r].mom2 != val)
				is_valid = 0;
			if (st[r].mom3 != val)
				is_valid = 0;
			if (st[r].mom4 != val)
				is_valid = 0;
			if (st[r].minF != val)
				is_valid = 0;
			if (st[r].maxF != val)
				is_valid = 0;
		}

		count++;
	}

	uloga("%s(): Bucket %d, tstep=%d, is_valid=%d, get %d data objs\n",
			__func__, rank, tstep, is_valid, count);

	return 0;
}

static int perform_intransit_stat(int rank, struct list_head *data_list)
{
	struct data_item *item;
	int count=0, is_valid = 1;
	int type, tstep;
	list_for_each_entry(item, data_list, struct data_item, item_entry) {
		if ( count == 0 ) {
			type = item->desc.type;
			tstep = item->desc.tstep;
		}

		if ( !validate_data(item->buf, item->desc.size, item->desc.type,
										item->desc.rank, item->desc.tstep) )
			is_valid = 0;

		if ( type != item->desc.type || tstep != item->desc.tstep )
			is_valid = 0;
		/*
		printf("%s(): Bucket %d, get size=%u rank=%d tstep=%d num_obj=%d is_valid=%d\n",
		__func__, rank, item->desc.size, item->desc.rank, item->desc.tstep, item->desc.num_obj, is_valid);
		*/
		count++;
	}

	uloga("%s(): Bucket %d, tstep=%d, is_valid=%d, get %d data objs\n",
			__func__, rank, tstep, is_valid, count);
	return 0;
}

int dummy_s3d_staging(MPI_Comm comm, int num_buckets)
{
	int i, err;
	int nprocs, mpi_rank, dart_rank;
	
	MPI_Comm_size(comm, &nprocs);
	MPI_Comm_rank(comm, &mpi_rank);
	uloga("Dummy S3D staging: mpi_rank= %d of total %d\n", mpi_rank, nprocs);

	err = ds_init(num_buckets, IN_TRANSIT);
	dart_rank = ds_rank();

	enum op_type type;
	struct list_head data_list;
	INIT_LIST_HEAD(&data_list);

	while ( !ds_request_job(&type, &data_list)) {
		switch (type) {
		case TOPOLOGY:
			/* 
				 (1) User function should not attempt to free data blocks chained 
				 by the list;
				 (2) struct list_head is defined in list.h;
			*/
			perform_intransit_topology(mpi_rank, &data_list);
			sleep(4); //TODO: why call sleep?
			break;
		case VISUALIZATION:
			perform_intransit_viz(mpi_rank, &data_list);
			sleep(4);
			break;
		case DESCRIPTIVE_STATS:
			//perform_intransit_stat(rank, &data_list);
			perform_intransit_stat_v1(mpi_rank, &data_list);
			sleep(3);
			break;
		default:
			uloga("error: unknown type...\n");
			break;
		}

		/*free the retrieved memroy blocks data*/
		ds_free_data_list(&data_list);
	}

	ds_finalize();

	return 0;
err_out:
	return -1;
}
