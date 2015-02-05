/* get.c : Example 3: DataSpaces get Bounding Box tutorial
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
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

	// DataSpaces: Initalize and identify application
	// Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
	// Note: appid for get.c is 2 [for put.c, it was 1]
	dspaces_init(1, 2, &gcomm, NULL);

	int timestep=0;

	while(timestep<10){
		timestep++;

		// DataSpaces: Read-Lock Mechanism
		dspaces_lock_on_read("my_test_lock", &gcomm);

		// Name our data.
		char var_name[128];
		sprintf(var_name, "ex3_sample_data");

		// We plan to access 10 values from two boxes
		int *data = malloc(10*sizeof(int));

		// Define the dimensionality of the data to be received 
		int ndim = 1; 
		
		// Prepare LOWER and UPPER bound dimensions
		uint64_t lb[3], ub[3];

		// One Dimension, so Y and Z coordinates always 0
		lb[1]=lb[2]=ub[1]=ub[2]=0;

		// In put example, we had 10 boxes. Let's acess values in
		// boxes 3,0,0 and 4,0,0 at each timestep
		lb[0]=3;
		ub[0]=4;

		// DataSpaces: Get data array from the space
		// Usage: dspaces_get(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		dspaces_get(var_name, timestep, 5*sizeof(int), ndim, 
			    lb, ub, data);
	
			
		printf("Timestep %d: get data in Bounding Box: LB: %d,%d,%d UB: %d,%d,%d\n", timestep, (int)lb[0],(int)lb[1],(int)lb[2], (int)ub[0],(int)ub[1],(int)ub[2]);

		printf("Data:\n");
		int i;
		for(i=0;i<10;i++){
			printf("%d\t", data[i]);
		}
		printf("\n");

		free(data);

		// DataSpaces: Release our lock on the data
		dspaces_unlock_on_read("my_test_lock", &gcomm);
	}

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
