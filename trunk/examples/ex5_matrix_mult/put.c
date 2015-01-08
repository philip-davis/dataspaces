/* put.c : Example 1: DataSpaces put tutorial 
 * This example will show you the simplest way 
 * to put a 1D array of 3 elements into the DataSpace.
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define MATRIX_DIM 5

int** createMatrix(int xdim, int ydim, int rank){

	if(xdim != ydim){
		printf("ERROR: Only square matrices supported.");
		exit(1);
	}

	int** mat = (int**) malloc(xdim*sizeof(int*));

	int i;
	for(i=0; i<xdim; i++){
		mat[i] = (int*) malloc(ydim*sizeof(int));
	}

	int j, k;

	
	srand(time(NULL)+rank);

	for(j=0;j<xdim;j++){
		for(k=0;k<ydim;k++){
			mat[j][k] = rand()%10;
		}
	}

	return mat; 
}

void freeMatrix(int** mat, int xdim){
	int i;

	for(i=0;i<xdim;i++){
		free(mat[i]);
	}

	free(mat);
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
		int** matA = createMatrix(MATRIX_DIM, MATRIX_DIM, rank);

		// ndim: Dimensions for application data domain
		// In this case, our matrix is 2 dimensional
		int ndim = 2; 
			
		// Prepare LOWER and UPPER bound dimensions
		uint64_t lb[3] = {0}, ub[3] = {0};
		ub[0] = MATRIX_DIM-1;
	
		// DataSpaces: Put data array into the space
		// Usage: dspaces_put(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer
		int i;
		for(i=0;i<MATRIX_DIM;i++){
                	lb[1] = ub[1]= i;
   			dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matA[i]);
		}

		//dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matA);
	
		printf("Matrix A, Put into DataSpace.\n");
		int j;
		for(i=0;i<MATRIX_DIM;i++){
			for(j=0;j<MATRIX_DIM;j++){
				printf("%d\t",matA[i][j]);
			}
			printf("\n");
		}		
		
		freeMatrix(matA, MATRIX_DIM);
	}

	if(rank==1 || nprocs==1){	
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
		ub[0] = MATRIX_DIM-1;
	
		// DataSpaces: Put data array into the space
		// Usage: dspaces_put(Name of variable, version num, 
		// size (in bytes of each element), dimensions for bounding box,
		// lower bound coordinates, upper bound coordinates,
		// ptr to data buffer 
		//dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matB);
		int i;
		for(i=0;i<MATRIX_DIM;i++){
                	lb[1] = ub[1]= i;
   			dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, matB[i]);
		}
	
		printf("Matrix B, Put into DataSpace.\n");
		int j;
		for(i=0;i<MATRIX_DIM;i++){
			for(j=0;j<MATRIX_DIM;j++){
				printf("%d\t",matB[i][j]);
			}
			printf("\n");
		}
		
		freeMatrix(matB, MATRIX_DIM);
	}

	printf("\n");
	printf("\n");

	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_write("my_test_lock", &gcomm);
	
	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
