#include "dataspaces.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "mpi.h"
#include "bitmap.h"


int iteration_to_color( int i, int max )
{
	int gray = 255*i/max;
	return MAKE_RGBA(gray,gray,gray,0);
}

int iterations_at_point( double x, double y, int max )
{
	double x0 = x;
	double y0 = y;

	int iter = 0;

	while( (x*x + y*y <= 4) && iter < max ) {

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

	int size = 512;

        struct bitmap *bm = bitmap_create(size,size);
        bitmap_reset(bm,MAKE_RGBA(0,0,255,0));

        printf("%lf %lf %lf %lf\n",xmin, xmax,ymin,ymax);

	dspaces_init(1, 1, &gcomm, NULL);

        int ts=0;
	char var_name[128];

        while(ts<1){
                ts++;
                dspaces_lock_on_write("metadata_lock", &gcomm);

		int i,j;

		int bitmap[size][size];
	
		for(j=0;j<size;j++) {

			for(i=0;i<size;i++) {
			// Determine the point in x,y space for that pixel.
			double x = xmin + i*(xmax-xmin)/size;
			double y = ymin + j*(ymax-ymin)/size;

			// Compute the iterations at that point.
			bitmap[j][i] = iterations_at_point(x,y,max);

			int iters = iterations_at_point(x,y,max);
			if(j==0){
				printf("%d ", iters);
			}
	
			bitmap_set(bm,i,j,iters);
			}
		}	
	
		sprintf(var_name, "bitmap");
	
		//TODO: Ask Hoang if this is really 1D or is it 2D? Bitmap is 2D but data domain still 1D?
                int ndim = 1;
		uint64_t lb[3] = {0}, ub[3] = {0};
                dspaces_put(var_name, ts, size*size*sizeof(int), ndim, lb, ub, bitmap);
		sleep(5);

		dspaces_unlock_on_write("metadata_lock", &gcomm);
		bitmap_save(bm,"test.bmp");
        }


	dspaces_finalize();

        MPI_Barrier(gcomm);
        MPI_Finalize();

	return 0;
}
