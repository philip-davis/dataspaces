#include "dataspaces.h"
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include "bitmap.h"

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


	int size = 512;
	struct bitmap *bm = bitmap_create(size,size);
	bitmap_reset(bm,MAKE_RGBA(0,0,255,0));

	dspaces_init(1, 2, &gcomm, NULL);

	int ts =0;

        // Variable name for the data we will retrieve
	char var_name[128];

	while(ts<1){
		ts++;
	        dspaces_lock_on_read("metadata_lock", &gcomm);
	        sprintf(var_name, "sample");


                int i,j;
		int max = 1000;

		int bitmap[size][size];
//                int **bitmap = malloc(sizeof(int)*size*size);
//
//		for(i=0;i<size;i++) {
//                      bitmap[i] = malloc(size*sizeof(int));
//		}
                sprintf(var_name, "bitmap");

                dspaces_get(var_name, ts, size*size*sizeof(int), 0, 0, 0, 0, 0, 0, bitmap);

		dspaces_unlock_on_read("metadata_lock", &gcomm);

                printf("Timestep %d: get data %d %d %d\n", ts, data[0], data[1], data[2]);

                for(j=0;j<size;j++) {
                        for(i=0;i<size;i++) {
//				int gray = 255*bitmap[j][i]/max;
//				int color= MAKE_RGBA(gray,gray,gray,0);
				bitmap_set(bm,i,j,bitmap[j][i]);
                	}
        	}

		bitmap_save(bm,"mandel1.bmp");
/*
                for(j=0;j<size;j++) {
                        for(i=0;i<size;i++) {
				if(i==0)
					printf("%d ",bitmap[i][j]);
                                int gray = 255*bitmap[i][j]/max;
                                int color= MAKE_RGBA(gray,gray,gray,0);
                                bitmap_set(bm,i,j,color);
                        }
                }

                bitmap_save(bm,"mandel2.bmp");
                for(j=0;j<size;j++) {
                        for(i=0;i<size;i++) {
                                int gray = 255*bitmap[i][j]/max;
                                int color= MAKE_RGBA(gray,gray,gray,0);
                                bitmap_set(bm,i,j,color);
                        }
                }

                bitmap_save(bm,"mandel3.bmp");
                for(j=0;j<size;j++) {
                        for(i=0;i<size;i++) {
                                int gray = 255*bitmap[i][j]/max;
                                int color= MAKE_RGBA(gray,gray,gray,0);
                                bitmap_set(bm,j,i,color);
                        }
                }

                bitmap_save(bm,"mandel4.bmp");
*/

	}


        dspaces_finalize();


        MPI_Barrier(gcomm);
        MPI_Finalize();


	return 0;
}
