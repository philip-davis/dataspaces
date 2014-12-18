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
#define MATRIX_DIM 10

int** createMatrix(int xdim, int ydim){
        int** mat;

        mat = (int**) malloc(xdim*sizeof(int*));

        int i;
        for(i=0; i<xdim; i++){
                mat[i] = (int*) malloc(ydim*sizeof(int));
        }

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

	// DataSpaces: Initalize and identify application
	// Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
	// Note: appid for get.c is 2 [for put.c, it was 1]
	dspaces_init(1, 2, &gcomm, NULL);


	dspaces_lock_on_read("my_test_lock", &gcomm);

	// Name our data.
	char var_name[128];
	sprintf(var_name, "matrix_A");

	// Create integer array, size 3
	// We will store the data we get out of the DataSpace
	// in this array.
	int **mat = createMatrix(MATRIX_DIM, MATRIX_DIM);

	// Define the dimensionality of the data to be received 
	int ndim = 2; 
		
	// Prepare LOWER and UPPER bound dimensions
	// 0,0,0 to 9,9,0
	uint64_t lb[3] = {0}, ub[3] = {0};
	ub[0]=ub[1]=MATRIX_DIM-1;

	dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, *mat);
		
	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_read("my_test_lock", &gcomm);

	int i, j;

	for(i=0;i<MATRIX_DIM;i++){
		for(j=0;j<MATRIX_DIM;j++){
			printf("%d\t",mat[i][j]);
		}
		printf("\r\n");
	}
	

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize;

	return 0;
}
