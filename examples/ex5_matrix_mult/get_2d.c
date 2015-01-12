/* get_2d.c : Example 4: Get a 2D array from the DataSpace
 *  */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"
#define MATRIX_DIM 5
// Define resultant matrix container
int resultMat[MATRIX_DIM][MATRIX_DIM];

// Create 2D array
int** createMatrix(int xdim, int ydim){
        int** mat = malloc(xdim*sizeof(int*));

        int i;
        for(i=0; i<xdim; i++){
                mat[i] = malloc(ydim*sizeof(int));
        }

	return mat;
}

// clean up memory
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
	dspaces_init(nprocs, 2, &gcomm, NULL);

	dspaces_lock_on_read("my_test_lock", &gcomm);

	// Each process will retrieve a full copy of MatB.
	char var_name[128];
	sprintf(var_name, "matrix_B");

	int **matB = createMatrix(MATRIX_DIM, MATRIX_DIM);

	// Define the dimensionality of the data to be received 
	int ndim = 2; 
		
	// Prepare LOWER and UPPER bound dimensions
	uint64_t lb[3] = {0}, ub[3] = {0};
	ub[0]=MATRIX_DIM-1;

        int i;
	
	//Retrive matB, row by row
        for(i=0;i<MATRIX_DIM;i++){
                lb[1] = ub[1]= i;
		dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, matB[i]);
        }
	
	// Sanity check, print contents of B for users.
	if(rank==0){
		printf("Matrix B retrieved from DataSpace.\n");
		int y, z;
		for(y=0;y<MATRIX_DIM;y++){
			for(z=0;z<MATRIX_DIM;z++){
				printf("%d\t",matB[y][z]);
			}
			printf("\n");
		}
	}

	//at this point, each process has retrieved matB
	//for a large number of processes, this may not be the most 
	//practical approach. see note in tutorial docs.
	
	// Each process will need to compute its DataSpace index
	int j;
	int portion = (MATRIX_DIM)/nprocs;
	int lower_bound, upper_bound = 0;
	lower_bound = rank*portion;
	if(rank==nprocs && MATRIX_DIM%nprocs != 0){
		upper_bound=MATRIX_DIM;
	}else{
		upper_bound = lower_bound+portion;
	}
	
	// We will now retrieve the portion of matrix A used for calc.
	sprintf(var_name, "matrix_A");

	//Reset vars.
	lb[0] = lb[1] = lb[2] = 0;
	ub[1] = ub[2] = 0;
	ub[0] = MATRIX_DIM-1;

	int k;

	// We can retrieve an individual row from DS, operate on it,
	// then retrieve the next row	
	int* rowA = malloc(MATRIX_DIM*sizeof(int));

	// Populate the portion of matA that will be used for this calculation
	ndim = 2;

	printf("Retrieving Matrix A.\n");
	for(i=lower_bound;i<upper_bound;i++){
		//Select row using bounding box: 0,i,0 - MAT_DIM, i, 0	
		lb[1] = ub[1]= i;
		dspaces_get(var_name, 0, sizeof(int), ndim, lb, ub, rowA);

		int y;
		printf("Row %d of A, retrieved by process %d\n",i,rank);
		for(y=0;y<MATRIX_DIM;y++){
			printf("%d\t",rowA[y]);
		}
		printf("\n");

		//Each process performs matrix multiplication. One row versus
		//all values of B. Stores result in resultant matrix.
		for(j=0;j<MATRIX_DIM;j++){ //cols B
			resultMat[i][j] = 0;
			for(k=0;k<MATRIX_DIM;k++){//rows B
				resultMat[i][j] += rowA[k]*matB[k][j];
			}
		}
	}
	
	// Each process cleans up memory
	free(rowA);
	freeMatrix(matB, MATRIX_DIM);
	
	// Gather all of the individual results from each process, store in final resultant matrix
	MPI_Gather(resultMat[lower_bound],MATRIX_DIM*MATRIX_DIM/nprocs, MPI_INT, resultMat, MATRIX_DIM*MATRIX_DIM/nprocs, MPI_INT, 0, gcomm);

	// Print results to user for verification purposes
	if(rank==0){
		printf("Resultant Matrix, C\n");
		int l, m;
		for(l=0;l<MATRIX_DIM;l++){
			for(m=0;m<MATRIX_DIM;m++){
				printf("%d\t",resultMat[l][m]);
			}
			printf("\n");
		}
	}
	
	// DataSpaces: Release our lock on the data
	dspaces_unlock_on_read("my_test_lock", &gcomm);

	// DataSpaces: Finalize and clean up DS process
	dspaces_finalize();

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
}
