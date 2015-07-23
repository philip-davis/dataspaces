/* master.c
 * Master bitmap
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#include "bitmap.h"
#define MATRIX_DIM 512
#define IMAGES 50

struct mbrotData{
	double xCenter;
	double yCenter;
	double scale;
	int max_iter;
	int index;
};


int main(int argc, char *argv[]){

	int err, rank, nprocs;
	MPI_Comm gcomm;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);
	gcomm=MPI_COMM_WORLD;

	struct mbrotData mb[IMAGES];
	int i;

	dspaces_init(nprocs, 1, &gcomm, NULL);
	char var_name[128];
	sprintf(var_name, "tasks");

	for(i=0;i<IMAGES;i++){
		mb[i].xCenter = atof(argv[1]);
		mb[i].yCenter = atof(argv[2]);
		mb[i].scale = (atof(argv[3])-0.000001);
		mb[i].max_iter = atoi(argv[4]);
		mb[i].index = i;

	}

	int ndim = 1;
	int timestep = 0;
	uint64_t lb[3] = {0}, ub[3]={0};

	ub[0]=IMAGES-1;

	int j;
	for(j=0;j<IMAGES;j++){
		printf("xCenter:%f\n",mb[j].xCenter);
		printf("yCenter:%f\n",mb[j].yCenter);
		printf("scale:%f\n",mb[j].scale);
	        printf("max_iter:%d\n",mb[j].max_iter);	
		printf("index:%d\n",mb[j].index);
	}
	dspaces_lock_on_write("taskLock",&gcomm);
	sleep(2);
	dspaces_put(var_name, timestep, sizeof(struct mbrotData), ndim, lb, ub, &mb);

	dspaces_unlock_on_write("taskLock", &gcomm);

	dspaces_finalize();
	MPI_Barrier(gcomm);
	MPI_Finalize();
	return 0;
}
