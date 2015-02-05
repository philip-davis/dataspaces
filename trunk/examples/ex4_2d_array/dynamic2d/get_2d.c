/* get_2d.c : Example 4: Get a 2D array from the DataSpace
 * when the incoming data was not a contiguous memory block,
 * e.g., 2D array of pointers
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define MATRIX_DIM 10

int** createMatrix(int xdim, int ydim){
        int** mat = malloc(xdim*sizeof(int*));

        int i;
        for(i=0; i<xdim; i++){
                mat[i] = malloc(ydim*sizeof(int));
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

	// DataSpaces: Initalize and identify application
	// Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
	// Note: appid for get.c is 2 [for put.c, it was 1]
	dspaces_init(1, 2, &gcomm, NULL);


	dspaces_lock_on_read("my_test_lock", &gcomm);

	// Name our data.
	char var_name[128];
	sprintf(var_name, "matrix_A");

	// Create 2D integer array, size 10x10
	// We will store the data we get out of the DataSpace
	// in this array.
	int **mat = createMatrix(MATRIX_DIM, MATRIX_DIM);

	// Define the dimensionality of the data to be received 
	int ndim = 2; 
		
	// Prepare LOWER and UPPER bound dimensions
	// 0,0,0 to 9,9,0
	uint64_t lb[3] = {0}, ub[3] = {0};
	ub[0]=MATRIX_DIM-1;

        int i;
        for(i=0;i<MATRIX_DIM;i++){
                lb[1] = ub[1]= i;
		dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, mat[i]);
        }

		
	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_read("my_test_lock", &gcomm);

	printf("Matrix retrieved from the DataSpace");
	int j,k;
	for(j=0;j<MATRIX_DIM;j++){
		for(k=0;k<MATRIX_DIM;k++){
			printf("%d\t",mat[j][k]);
		}
		printf("\n");
	}

	freeMatrix(mat, MATRIX_DIM);

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
