/* put.c : Example 1: DataSpaces put tutorial 
 * This example will show you the simplest way 
 * to put a 1D array of 3 elements into the DataSpace.
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define MATRIX_DIM 500

int** createMatrix(int xdim, int ydim, int rank){
	int** mat;

	if(xdim != ydim){
		printf("ERROR: Only square matrices supported.");
		exit(1);
	}

	mat = (int**) malloc(xdim*sizeof(int*));

	int i;
	for(i=0; i<xdim; i++){
		mat[i] = (int*) malloc(ydim*sizeof(int));
	}

	int j, k;

	
	srand(time(NULL)+rank);

	for(j=0;j<xdim;j++){
		for(k=0;k<ydim;k++){
			mat[j][k] = rand()%32768;
		}
	}

	return mat; 
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
	dspaces_init(nprocs, 1, &gcomm, NULL);
	
	// DataSpaces: Lock Mechanism
	// Usage: Prevent other process from modifying 
	// 	  data at the same time as ours
	dspaces_lock_on_write("my_test_lock", &gcomm);

	if(rank==0){

		//Name the Data that will be writen
		char var_name[128];
		sprintf(var_name, "matrix_A");

		// Create our 2D matrix, 500x500
		int **matA = createMatrix(MATRIX_DIM, MATRIX_DIM, rank);

		// ndim: Dimensions for application data domain
		// In this case, our matrix is 2 dimensional
		int ndim = 2; 
			
		// Prepare LOWER and UPPER bound dimensions
		uint64_t lb[3] = {0}, ub[3] = {0};
		ub[1] = ub[2] = MATRIX_DIM;
	
		// DataSpaces: Put data array into the space
		// Usage: dspaces_put(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matA);
		
		free(matA);
	}else if(rank==1 || nprocs==1){
		//Name the Data that will be writen
		char var_name[128];
		sprintf(var_name, "matrix_B");

		// Create our 2D matrix, 500x500
		int **matB = createMatrix(MATRIX_DIM, MATRIX_DIM, rank);

		// ndim: Dimensions for application data domain
		// In this case, our matrix is 2 dimensional
		int ndim = 2; 
		
		// Prepare LOWER and UPPER bound dimensions
		uint64_t lb[3] = {0}, ub[3] = {0};
		ub[1] = ub[2] = MATRIX_DIM;
	
		// DataSpaces: Put data array into the space
		// Usage: dspaces_put(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matB);
	
		free(matB);
	}else{
		printf("ERROR: Currently only works with 2 processes");
	}


	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_write("my_test_lock", &gcomm);
	
	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
