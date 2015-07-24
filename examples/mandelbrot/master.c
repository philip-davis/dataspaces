/* master.c : Mandelbrot Bitmap
 * A Bag of Tasks Implementation using DataSpaces
 * The master creates and inserts data for seperate tasks 
 * into the space. Worker processes (worker.c) will then pull
 * this data and use them to create images in the Mandelbrot set 
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define MATRIX_DIM 500
#define IMAGES 50

// Define the data that will be contained
// in the task
struct mbrotData{
	double xCenter;
	double yCenter;
	double scale;
	int max_iter;
	int index;
};


int main(int argc, char *argv[]){

	//We will have # tasks = IMAGES
	//Create array of structures
	struct mbrotData mb[IMAGES];
	int i;

	//Initialize DataSpaces
	//Recall: dspaces_init(#peers, appId, comm, params)
	dspaces_init(1, 1, NULL, NULL);


	for(i=0;i<IMAGES;i++){
		mb[i].xCenter = atof(argv[1]);
		mb[i].yCenter = atof(argv[2]);
		mb[i].scale = atof(argv[3]);
		mb[i].max_iter = atoi(argv[4]);
		mb[i].index = i;
	}


	/* Set up necessary variables for put API */
	char var_name[128];
	sprintf(var_name, "tasks");

	int ndim = 1;
	int timestep = 0;
	uint64_t lb[3] = {0}, ub[3]={0};
	// Lower bound at 0,0,0
	// Upper bound at 49,0,0
	ub[0]=IMAGES-1;

	// Sanity Check the data prior to space insertion
	// You may comment this out if you wish
	int j;
	for(j=0;j<IMAGES;j++){
		printf("xCenter:%f\n",mb[j].xCenter);
		printf("yCenter:%f\n",mb[j].yCenter);
		printf("scale:%f\n",mb[j].scale);
	        printf("max_iter:%d\n",mb[j].max_iter);	
		printf("index:%d\n",mb[j].index);
	}

	// For dspaces_put, each mb[i] will occupy one index in the DS
	// because we specify lb,ub as above and each space to have sizeof(mbrotData)
	dspaces_lock_on_write("taskLock",NULL);
	dspaces_put(var_name, timestep, sizeof(struct mbrotData), ndim, lb, ub, &mb);
	dspaces_unlock_on_write("taskLock",NULL);

	dspaces_finalize();
	
	return 0;
}
