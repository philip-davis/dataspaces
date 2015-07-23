/* get.c : Example 1: DataSpaces get tutorial
 *  This example will show you the simplest way 
 *  to get a 1D array of 3 elements out of the DataSpace
 *  and store it in a local variable..
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"

int main(int argc, char **argv)
{
	// DataSpaces: Initalize and identify application
	// Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
	// Note: appid for get.c is 2 [for put.c, it was 1]
	dspaces_init(1, 2, NULL, NULL);
	

	int timestep=0;

	while(timestep<10){
		// DataSpaces: Read-Lock Mechanism
		// Usage: Prevent other processies from changing the 
		// 	  data while we are working with it
		dspaces_lock_on_read("my_test_lock", NULL);
		
		timestep++;

		// Name our data.
		char var_name[128];
		sprintf(var_name, "ex1_sample_data");

		// Create integer array, size 3
		// We will store the data we get out of the DataSpace
		// in this array.
		int *data = malloc(3*sizeof(int));

		// Define the dimensionality of the data to be received 
		int ndim = 1; 
		
		// Prepare LOWER and UPPER bound dimensions
		uint64_t lb[3] = {0}, ub[3] = {0};

		// DataSpaces: Get data array from the space
		// Usage: dspaces_get(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		dspaces_get(var_name, timestep, 3*sizeof(int), ndim, 
			    lb, ub, data);
		
		printf("Timestep %d: get data %d %d %d\n", 
			timestep, data[0], data[1], data[2]);
		
		free(data);

		// DataSpaces: Release our lock on the data
		dspaces_unlock_on_read("my_test_lock", NULL);
	}
		
	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	return 0;
}
