#include "dataspaces_api.h"
#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "intransitDescriptiveStats.h"
#include "s3d_box_viz.h"
#include "intransitTopology.h"

int main(int argc, char *argv[])
{
	int num_buckets;
	int i, err;
        MPI_Comm comm_new;
        int nprocs, mpi_rank;
	int color;

	if (argc != 2)  {
        printf("wrong number of args...\n");
        return -1;
    }

	num_buckets = atoi(argv[1]);

	/***/
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    color = MPI_UNDEFINED;
    MPI_Comm_split(MPI_COMM_WORLD, color, mpi_rank, &comm_new);
   	/***/

	err = ds_init(num_buckets, IN_TRANSIT);

	enum op_type type;
	struct list_head data_list;
	INIT_LIST_HEAD(&data_list);

	double idle_time = -1 * MPI_Wtime();
	while ( !ds_request_job(&type, &data_list)) {
		idle_time += MPI_Wtime();
		fprintf(stderr, "EVAL: bucket %d idle time is %.05f\n", mpi_rank, idle_time);

		switch (type) {
		case TOPOLOGY:
			/* 
			   (1) User function should not attempt to free data blocks chained 
			   by the list;
			   (2) struct list_head is defined in list.h;
			*/
			double top_time = -1 * MPI_Wtime();

			perform_intransit_topology(&data_list);
			
			top_time += MPI_Wtime();
			fprintf(stderr, "EVAL: bucket %d in-transit topology time is %.05f\n", mpi_rank, top_time);
			break;
		case VISUALIZATION:
            double viz_time = -1 * MPI_Wtime();

			perform_intransit_viz(MPI_COMM_NULL, &data_list);

            viz_time += MPI_Wtime();
            fprintf(stderr, "EVAL: bucket %d in-transit render time is %.05f\n", mpi_rank, viz_time);
            		
			break;
		case DESCRIPTIVE_STATS:
			double stat_time = -1 * MPI_Wtime();

			perform_intransit_stat(&data_list);

			stat_time += MPI_Wtime();
			fprintf(stderr, "EVAL: bucket %d in-transit stats time is %.05f\n", mpi_rank, stat_time);
			break;
		default:
			printf("error: unknown type...\n");
			break;
		}

		/*free the retrieved memroy blocks data*/
		ds_free_data_list(&data_list);
		idle_time = -1 * MPI_Wtime();
	} 

	idle_time += MPI_Wtime();
	fprintf(stderr, "EVAL: bucket %d idle time is %0.5f\n", mpi_rank, idle_time);

	ds_finalize();

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();

	return 0;
err_out:
	MPI_Finalize();
	return -1;	
}
