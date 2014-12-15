/* get.c : Example 1: DataSpaces get tutorial
 *  This example will show you the simplest way 
 *  to get a 1D array of 3 elements out of the DataSpace
 *  and store it in a local variable.
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define ARRAY_SIZE 128

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
	
 	// Name our data.
	char var_name[128];
	sprintf(var_name, "ex3_sample_data");

	// DataSpaces: Read-Lock Mechanism
	// Usage: Prevent other processies from changing the 
	// 	  data while we are working with it
	dspaces_lock_on_read("my_test_lock", &gcomm);

	// Each process will need to compute its DataSpace index
	int tasks_per_proc = ARRAY_SIZE/nprocs;
	int tasks_left_over = ARRAY_SIZE%nprocs;
	int ds_lb_index, ds_ub_index;
	
	if(rank<tasks_left_over){
		ds_lb_index = rank*(tasks_per_proc+1);
		ds_ub_index = ds_lb_index + tasks_per_proc;
	}else{
		ds_lb_index = rank*tasks_per_proc+tasks_left_over;
		ds_ub_index = ds_lb_index + tasks_per_proc-1;
	}
	
	// Allocate temporary buffer for computing min/max
	int *tempDataBuffer = malloc((ds_ub_index-ds_lb_index+1)*sizeof(int));

	// Define the dimensionality of the data to be received 
	int ndim = 1; 
		
	// Prepare LOWER and UPPER bound dimensions
	uint64_t lb[3] = {0}, ub[3] = {0};
	lb[0]= ds_lb_index;
	ub[0] = ds_ub_index;

	// DataSpaces: Get data array from the space
	// Usage: dspaces_get(Name of variable, version num, 
	// size (in bytes of each element), dimensions for bounding box,
	// lower bound coordinates, upper bound coordinates,
	// ptr to data buffer 
	dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, tempDataBuffer);
	
	int i;	
	int array_size = sizeof(tempDataBuffer)/sizeof(int);
	int local_max=tempDataBuffer[0];
	int local_min=tempDataBuffer[0];
	int sum; //for avg
	
	// Find Max in our local buffer
	for(i=0;i<array_size;i++){

		sum += tempDataBuffer[i];

		if(local_max < tempDataBuffer[i]){
			local_max = tempDataBuffer[i];
		}else if(local_min > tempDataBuffer[i]){
			local_min = tempDataBuffer[i];
		}
	
	}

	int local_avg = sum/i;
	
	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_read("my_test_lock", &gcomm);

	int global_max, global_min, global_avg;

	MPI_Reduce(&local_max, &global_max, 1, MPI_INT, MPI_MAX, 0, gcomm);
	MPI_Reduce(&local_min, &global_min, 1, MPI_INT, MPI_MIN, 0, gcomm);
	MPI_Reduce(&local_avg, &global_avg, 1, MPI_INT, MPI_SUM, 0, gcomm);
	global_avg = global_avg/nprocs;

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize;

	return 0;
}
