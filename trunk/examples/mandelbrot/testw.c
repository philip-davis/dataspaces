#include "dataspaces.h"
#include <stdio.h>
#include <stdlib.h>
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


	dspaces_init(1,2);
//	dspaces_set_storage_type(1);

        int ts=0;
        while(ts<1){
                ts++;
                dspaces_lock_on_write("metadata_lock", &gcomm);
                char var_name[128];
                sprintf(var_name, "sample");

                int *data = malloc(3*sizeof(int));
		srand(time(NULL));
		data[0] = rand()%100;
		data[1] = rand()%100;
		data[2] = rand()%100;

		printf("Timestep %d: put data %d %d %d\n", ts, data[0], data[1], data[2]);

                dspaces_put(var_name, ts, 3*sizeof(int), 0, 0, 0, 0, 0, 0, data);



		int i,j;

		//int **bitmap = malloc(sizeof(int*)*size);
		
		int bitmap[size][size];
	
		for(j=0;j<size;j++) {
//			bitmap[j] = malloc(size*sizeof(int));

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
		dspaces_put(var_name, ts, size*size*sizeof(int), 0, 0, 0, 0, 0, 0, bitmap);
sleep(5);                dspaces_unlock_on_write("metadata_lock", &gcomm);
		bitmap_save(bm,"test.bmp");
        }


 //     MPI_Barrier(gcomm_);
	dspaces_finalize();


        MPI_Barrier(gcomm);
        MPI_Finalize();

	return 0;
}
