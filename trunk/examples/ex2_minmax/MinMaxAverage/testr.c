#include "dataspaces.h"
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

int main(int argc, char **argv)
{
        int err;
        int nprocs, rank;
        MPI_Comm gcomm;
        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Barrier(MPI_COMM_WORLD);
        gcomm = MPI_COMM_WORLD;


	int size = 512;

        dspaces_init(1,3);
        //dspaces_set_storage_type(1);

	int ts =0;
	while(ts<1){
		ts++;
	        dspaces_lock_on_read("metadata_lock", &gcomm);
        	char var_name[128];
	        sprintf(var_name, "sample");

		int *data = malloc(3*sizeof(int));
		
	        dspaces_get(var_name, ts, 3*sizeof(int), 0, 0, 0, 0, 0, 0, data);

//		printf("Timestep %d: get data %d %d %d\n", ts, data[0], data[1], data[2]);


                int i,j;
		int max = 1000;

		int bitmap[size][size];
//                int **bitmap = malloc(sizeof(int)*size*size);
//
//		for(i=0;i<size;i++) {
//                      bitmap[i] = malloc(size*sizeof(int));
//		}
                sprintf(var_name, "bitmap");

                dspaces_get(var_name, ts, size*size*sizeof(int), 0, 0, 0, 0, 0, 0, bitmap);

		dspaces_unlock_on_read("metadata_lock", &gcomm);

                printf("Timestep %d: get data %d %d %d\n", ts, data[0], data[1], data[2]);

	}


 //     MPI_Barrier(gcomm_);
        dspaces_finalize();


        MPI_Barrier(gcomm);
        MPI_Finalize();


	return 0;
}
