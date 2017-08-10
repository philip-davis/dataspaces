/* worker.c:  Mandelbrot Image Worker
 * Sample bag of tasks workflow where
 * n Processes pull their task data 
 * from the DataSpace and then operate on it 
 * using the escape time algorithm 
 * for mandelbrot computation
 * For simplicity, we will use -np 5
 * and generate 50 images. Each proc will
 * generate 10 images.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#include "bitmap.h"
#include <inttypes.h>
#define MATRIX_DIM 500
#define IMAGES 50

struct mbrotData{
	double xCenter;
	double yCenter;
	double scale;
	int max_iter;
	int index;
};

/* Bitmap/Mandelbrot Function Credit: 
http://www3.nd.edu/~cpoellab/teaching/cse30341/project3.html
*/
int iteration_to_color(int i, int max){
	int gray = 255*i/max;
	return MAKE_RGBA(gray,gray,gray,0);
}

/* Escape Time Mandelbrot Calculation */
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

/* Helper Function to Iterate over all the pixels and set relevant ones*/ 
void compute_image(struct bitmap *bm, double xmin, double xmax, double ymin, double ymax, int max){

	int i,j;

	int width = bitmap_width(bm);
	int height = bitmap_height(bm);

	for(j=0;j<height;j++){
		for(i=0;i<width;i++){
			double x = xmin + i*(xmax-xmin)/width;
			double y = ymin + j*(ymax-ymin)/height;

			int iters = iterations_at_point(x,y,max);

			bitmap_set(bm,i,j,iters);
		}

	}

}


int main(int argc, char **argv){

	int err;
	int nprocs, rank;
	MPI_Comm gcomm;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);
	gcomm = MPI_COMM_WORLD;

	//Initialize the DataSpace
	dspaces_init(nprocs,2,&gcomm,NULL);
	
	
	// Simple Divide-and-Conquer Work Strategy
	int per_proc = IMAGES/nprocs;
	int remainder = IMAGES%nprocs;
	int start, stop;
	if(rank<remainder){
		start = rank*(per_proc+1);
		stop = start + per_proc;
	}else{
		start = rank*per_proc+remainder;
		stop = start+per_proc-1;
	}


	double scale_inc, scale_adj, xmin, ymin, xmax, ymax;
	int k;
	int ndim=1;
	int timestep=0;
	char var_name[128];
	sprintf(var_name, "tasks");
	
	uint64_t lb[3] = {0}, ub[3]={0};
	lb[0] = start;
	ub[0] = stop;
	
	int count = stop-start+1;
	struct mbrotData mb[IMAGES];

	dspaces_lock_on_read("taskLock",&gcomm);
	dspaces_get(var_name, timestep, sizeof(struct mbrotData), ndim, lb, ub, &mb);
	dspaces_unlock_on_read("taskLock",&gcomm);

	for(k=0;k<count;k++){
			#ifdef DEBUG
			printf("rank:%d\n",rank);
			printf("xCenter:%f\n",mb[k].xCenter);
			printf("yCenter:%f\n",mb[k].yCenter);
			printf("Scale:%f\n",mb[k].scale);
			printf("Max Iter:%d\n",mb[k].max_iter);
			printf("Index:%d\n",mb[k].index);
			printf("\n\n");
			#endif

		// We will scale from 2 down to the 
		// input scale size (given via CL to master.c)
		// Each image will be at a different scale.
		scale_inc = (2-(mb[k].scale))/IMAGES;
		scale_adj = 2-((mb[k].index+1)*(scale_inc)); 

		xmin = mb[k].xCenter - scale_adj;
		ymin = mb[k].yCenter - scale_adj;
		xmax = mb[k].xCenter + scale_adj;
		ymax = mb[k].yCenter + scale_adj;

		struct bitmap *bm = bitmap_create(MATRIX_DIM,MATRIX_DIM);
		bitmap_reset(bm,MAKE_RGBA(0,0,255,0));

		// Create image
		compute_image(bm,xmin,xmax,ymin,ymax,mb[k].max_iter);

		// Save the image
		char mandel_name[128];
		sprintf(mandel_name, "mandel%d.bmp", mb[k].index);
		bitmap_save(bm, mandel_name);
		free(bm);
	}

	dspaces_finalize();
	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
