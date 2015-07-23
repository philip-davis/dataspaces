/* put.c : Example 1: DataSpaces put tutorial 
 * This example will show you the simplest way 
 * to put a 1D array of 3 elements into the DataSpace.
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"

int main(int argc, char **argv)
{

	// Initalize DataSpaces
	// # of Peers, Application ID, ptr MPI comm, additional parameters
	// # Peers: Number of connecting clients to the DS server
	// Application ID: Unique idenitifier (integer) for application
	// Pointer to the MPI Communicator: 
	//      when NOT NULL, allows DS Layer to use MPI barrier func
	// Addt'l parameters: Placeholder for future arguments, currently NULL.
	dspaces_init(1, 1, NULL, NULL);

	
	int timestep=0;

	while(timestep<10){
		// DataSpaces: Lock Mechanism
		// Usage: Prevent other process from modifying 
		// 	  data at the same time as ours
		// The NULL parameter is for a pointer to the 
		//MPI Communicator which we are not using in this example
		dspaces_lock_on_write("my_test_lock", NULL);
		
		timestep++;
		sleep(2);

		//Name the Data that will be writen
		char var_name[128];
		sprintf(var_name, "ex1_sample_data");

		// Create integer array, size 3
		int *data = malloc(3*sizeof(int));

		// Initialize Random Number Generator
		srand(time(NULL));

		// Populate data array with random values from 0 to 99
		data[0] = rand()%100;
		data[1] = rand()%100;
		data[2] = rand()%100;
		
		printf("Timestep %d: put data %d %d %d\n", 
			timestep, data[0], data[1], data[2]);
		
		// ndim: Dimensions for application data domain
		// In this case, our data array is 1 dimensional
		int ndim = 1; 
		
		// Prepare LOWER and UPPER bound dimensions
		// In this example, we will put all data into a 
		// small box at the origin upper bound = lower bound = (0,0,0)
		// In further examples, we will expand this concept.
		uint64_t lb[3] = {0}, ub[3] = {0};

		// DataSpaces: Put data array into the space
		// Usage: dspaces_put(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		dspaces_put(var_name, timestep, 3*sizeof(int), ndim, lb, ub, data);
	
		free(data);
		
		// DataSpaces: Release our lock on the data
		dspaces_unlock_on_write("my_test_lock", NULL);
	
	}
	
	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	return 0;
}
