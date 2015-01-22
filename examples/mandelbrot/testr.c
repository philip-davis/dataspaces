/* get_2d.c : Example 4: Get a 2D array from the DataSpace
 * for a static array
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#include "bitmap.h"
#define MATRIX_DIM 512

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

	struct bitmap *bm = bitmap_create(MATRIX_DIM,MATRIX_DIM);
	bitmap_reset(bm,MAKE_RGBA(0,0,255,0));
	// DataSpaces: Initalize and identify application
	// Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
	// Note: appid for get.c is 2 [for put.c, it was 1]
	dspaces_init(1, 2, &gcomm, NULL);

	// Name our data.
	char var_name[128];
	sprintf(var_name, "bitmap");

	// Create integer array, size 3
	// We will store the data we get out of the DataSpace
	// in this array.
	int bitmap[MATRIX_DIM][MATRIX_DIM];
	int i,j;

	int max = 1000;
	
	// Define the dimensionality of the data to be received 
	int ndim = 2; 
		
	// Prepare LOWER and UPPER bound dimensions
	// 0,0,0 to 9,9,0
	uint64_t lb[3] = {0}, ub[3] = {0};
	ub[0]=ub[1]=MATRIX_DIM-1;

	dspaces_lock_on_read("my_test_lock", &gcomm);
	dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, bitmap);
	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_read("my_test_lock", &gcomm);

/*	printf("Matrix retrieved from the DataSpace.\n");
	int k;
	for(j=0;j<MATRIX_DIM;j++){
		for(k=0;k<MATRIX_DIM;k++){
			printf("%d\t",bitmap[j][k]);
		}
		printf("\n");
	}*/

	for(j=0;j<MATRIX_DIM;j++){
		for(i=0;i<MATRIX_DIM;i++){
			int gray = 255*bitmap[j][i]/max;
			int color = MAKE_RGBA(gray,gray,gray,0);
			bitmap_set(bm,i,j,color);
		}
	}

	bitmap_save(bm, "mandel1.bmp");

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
