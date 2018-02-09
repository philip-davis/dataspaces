/* get_hdf5.c
 *  This example demonstrates how to get an HDF5 file
 *  from DataSpaces into memory and write it to a file.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hdf5.h>

#include "dataspaces.h"
#include "mpi.h"

static void usage(char *cmd)
{
	      fprintf(stderr, "Usage: %s <variable> <path>\n", cmd);
				exit(42);
}

int main(int argc, char **argv)
{
        int err;
        int nprocs, rank;
        MPI_Comm gcomm;

        MPI_Init(&argc, &argv);

				if(argc != 3) {
					MPI_Finalize();
					usage("get");
				}

        MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Barrier(MPI_COMM_WORLD);
        gcomm = MPI_COMM_WORLD;

        // DataSpaces: Initalize and identify application
        // Usage: dspaces_init(num_peers, appid, Ptr to MPI comm, parameters)
        // Note: appid for get.c is 2 [for put.c, it was 1]
        dspaces_init(1, 2, &gcomm, NULL);

				// DataSpaces: Read-Lock Mechanism
				// Usage: Prevent other processes from changing the
				//        data while we are working with it
				dspaces_lock_on_read("my_test_lock", &gcomm);

				// Name our data
				char *var_name = argv[1];

				// Get the size of the object and allocate a buffer
				// (Note: files are always stored in 1-dimension variables.)
				uint64_t fsize;

				printf("Querying dimensions for variable %s\n", var_name);

				err = dspaces_get_dims(
						var_name,      // Name of the variable
						1,             // Version num
						1,             // Dimensions for the bounding box
						&fsize         // Ptr to result buffer
				);

				if(err < 0) {
					fprintf(stderr, "Could not get dimensions for version %d of variable %s\n",
							1, var_name);
					fprintf(stderr, "dspaces_get_dims returned %d\n", err);
					return -1;
				}

				printf("Getting %s of size %llu bytes...\n", var_name, fsize);

				// Create a buffer to hold the HDF5 file in memory
				// (unsigned char) is "guaranteed" to be a byte)
				int *data = malloc(fsize * sizeof(unsigned char));

				uint64_t lb = 0;

				// DataSpaces: Get data array from the space
				dspaces_get(var_name,       // Name of variable
						1,                      // Version num
						sizeof(unsigned char),  // Size (in bytes of each element)
						1,                      // Dimensions for bounding box
						&lb,                    // Lower bound coordinates
						&fsize,                 // Upper bound coordinates
						data                    // Ptr to data buffer
				);

				// DataSpaces: Release our lock on the data
				dspaces_unlock_on_read("my_test_lock", &gcomm);

				// Write the file
				printf("Writing to %s\n", argv[2]);
				FILE *hdf5_file = fopen(argv[2], "w");
				fwrite(data, fsize, 1, hdf5_file);
				fclose(hdf5_file);

				free(data);

        // DataSpaces: Finalize and clean up DS process
        dspaces_finalize();

        MPI_Barrier(gcomm);
        MPI_Finalize();

        return 0;
}
