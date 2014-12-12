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



	dspaces_init(1,2);
//	dspaces_set_storage_type(1);

        int ts=0;
        while(ts<1){
                ts++;
                dspaces_lock_on_write("metadata_lock", &gcomm);
                char var_name[128];
                sprintf(var_name, "sample");

                int *data = malloc(3*sizeof(int));
		srand(time(NULL));
		data[0] = rand()%100;
		data[1] = rand()%100;
		data[2] = rand()%100;

		printf("Timestep %d: put data %d %d %d\n", ts, data[0], data[1], data[2]);

                dspaces_put(var_name, ts, 3*sizeof(int), 0, 0, 0, 0, 0, 0, data);



		int i,j;

		int *bitmap;
		int size;	
	
		sprintf(var_name, "bitmap");
		dspaces_put(var_name, ts, size*size*sizeof(int), 0, 0, 0, 0, 0, 0, bitmap);
sleep(5);                dspaces_unlock_on_write("metadata_lock", &gcomm);
        }


 //     MPI_Barrier(gcomm_);
	dspaces_finalize();


        MPI_Barrier(gcomm);
        MPI_Finalize();

	return 0;
}
