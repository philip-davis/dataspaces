/* put_2d.c : Example 4: Put a 2D array into the DataSpace
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#include "bitmap.h"
#define MATRIX_DIM 512

int iteration_to_color(int i, int max){
	int gray = 255*i/max;
	return MAKE_RGBA(gray,gray,gray,0);
}

int iterations_at_point(double x, double y, int max){
	double x0 = x;
	double y0 = y;

	int iter = 0;

	while((x*x+y*y<=4) && iter<max){
		double xt = x*x - y*y + x0;
		double yt = 2*x*y + y0;

		x = xt;
		y = yt;

		iter++;
	}

	return iteration_to_color(iter,max);
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

	double  xcenter = 0.286932;
        double  ycenter = 0.014287;
        double  scale = 0.0005;
        int     max = 1000;

        double xmin = xcenter - scale;
        double ymin = ycenter - scale;
        double xmax = xcenter + scale;
        double ymax = ycenter + scale;

	struct bitmap *bm = bitmap_create(MATRIX_DIM,MATRIX_DIM);
	bitmap_reset(bm,MAKE_RGBA(0,0,255,0));

	// Initalize DataSpaces
	// # of Peers, Application ID, ptr MPI comm, additional parameters
	// # Peers: Number of connecting clients to the DS server
	// Application ID: Unique idenitifier (integer) for application
	// Pointer to the MPI Communicator, allows DS Layer to use MPI barrier func
	// Addt'l parameters: Placeholder for future arguments, currently NULL.
	dspaces_init(1, 1, &gcomm, NULL);
	
	// DataSpaces: Lock Mechanism
	// Usage: Prevent other process from modifying 
	// 	  data at the same time as ours
	dspaces_lock_on_write("my_test_lock", &gcomm);

	srand(time(NULL));
	
	//Name the Data that will be writen
	char var_name[128];
	sprintf(var_name, "bitmap");

	int bitmap[MATRIX_DIM][MATRIX_DIM];	
	
	int i, j;
	for(i=0;i<MATRIX_DIM;i++){
		for(j=0;j<MATRIX_DIM;j++){
			double x = xmin + j*(xmax-xmin)/MATRIX_DIM;
			double y = ymin + i*(ymax-ymin)/MATRIX_DIM;

			bitmap[i][j] = iterations_at_point(x,y,max);

			int iters = iterations_at_point(x,y,max);

			bitmap_set(bm,j,i,iters);
		}
	}

	// ndim: Dimensions for application data domain
	// In this case, our matrix is 2 dimensional
	int ndim = 2; 
			
	// Prepare LOWER and UPPER bound dimensions
	// We will go from 0,0,0 to 9,9,0 in this example
	uint64_t lb[3] = {0}, ub[3] = {0};
	ub[0] = ub[1] = MATRIX_DIM-1;
        	
	// DataSpaces: Put data array into the space
	// Usage: dspaces_put(Name of variable, version num, 
	// size (in bytes of each element), dimensions for bounding box,
	// lower bound coordinates, upper bound coordinates,
	// ptr to data buffer 
	dspaces_put(var_name, 0, sizeof(int), ndim, lb, ub, bitmap);

	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_write("my_test_lock", &gcomm);
	
	printf("put matrix into dataspace\n");
		
	for(i=0;i<MATRIX_DIM;i++){
		for(j=0;j<MATRIX_DIM;j++){
			printf("%d\t", bitmap[i][j]); 
		}
		printf("\n");
	}

	bitmap_save(bm,"testw.bmp");

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
