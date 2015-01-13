/* put.c : Example 3: DataSpaces Bounding Box Tutorial 
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"

//compare function: used to quicksort array values
int compare (const void * a, const void * b)
{
  return ( *(int*)a - *(int*)b );
}

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

	// Initalize DataSpaces
	// # of Peers, Application ID, ptr MPI comm, additional parameters
	// # Peers: Number of connecting clients to the DS server
	// Application ID: Unique idenitifier (integer) for application
	// Pointer to the MPI Communicator, allows DS Layer to use MPI barrier func
	// Addt'l parameters: Placeholder for future arguments, currently NULL.
	dspaces_init(1, 1, &gcomm, NULL);

	int timestep=0;

	while(timestep<10){
		timestep++;
		sleep(2);
		// DataSpaces: Lock Mechanism
		// Usage: Prevent other process from modifying 
		// 	  data at the same time as ours
		dspaces_lock_on_write("my_test_lock", &gcomm);

		//Name the Data that will be writen
		char var_name[128];
		sprintf(var_name, "ex3_sample_data");

		// Initialize Random Number Generator
		srand(time(NULL));

		// Create integer array, size 50
		int *data = malloc(50*sizeof(int));

		// Generate 50 random numbers between 0 and 128
		// for each timestep -- these numbers should be different
		int j;
		for(j=0;j<50;j++){
			data[j] = rand()%128;
		}

		// Sort array
		qsort(data, 50, sizeof(int), compare);	

		// We will use 1D
		int ndim = 1;

		// We want to put 5 values in each bounding box
		uint64_t lb[3] = {0}, ub[3] = {0};
		ub[0]=9;

		//Put 5 values into each of 10 boxes (0,0,0 to 9,0,0)
		dspaces_put(var_name, timestep, 5*sizeof(int), ndim, lb, ub, data);

		printf("Finished put 50 values.\n");
		
		free(data);

		// DataSpaces: Release our lock on the data
		dspaces_unlock_on_write("my_test_lock", &gcomm);
	}

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
