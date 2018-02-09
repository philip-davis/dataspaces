/* put_hdf5.c : DataSpaces HDF5 put
 * This example will demonstrates a quick way to store
 * a HDF5 file as a 1D array in DataSpaces.
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <libgen.h>
#include <hdf5.h>

#include "dataspaces.h"
#include "mpi.h"

static void usage(char *cmd)
{
        fprintf(stderr, "Usage: %s <file>\n", cmd);
				exit(42);
}

int main(int argc, char **argv)
{
        int err;
        int nprocs, rank;
        MPI_Comm gcomm;
				MPI_Init(&argc, &argv);

				if(argc != 2) {
					MPI_Finalize();
					usage("put");
				}

				MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Barrier(MPI_COMM_WORLD);
        gcomm = MPI_COMM_WORLD;

        // Initalize DataSpaces
        // # of Peers, Application ID, ptr MPI comm, additional parameters
        // # Peers: Number of connecting clients to the DS server
        // Application ID: Unique idenitifier (integer) for application
        // Pointer to the MPI Communicator, allows DS Layer to use MPI barrier func
        // Addt'l parameters: Placeholder for future arguments, currently NULL.
        dspaces_init(1, 1, &gcomm, NULL);

        // DataSpaces: Lock Mechanism
        // Usage: Prevent other process from modifying
        //        data at the same time as ours
        dspaces_lock_on_write("my_test_lock", &gcomm);

        // This is the file we will read and put in DS
        char* hdf5_path = argv[1];
        char* var_name = basename(hdf5_path);

				printf("Opening %s\n", hdf5_path);

        // Open the HDF5 file
        hid_t hdf5_file = H5Fopen(hdf5_path, H5F_ACC_RDONLY, H5P_DEFAULT);

        if(hdf5_file < 0)
        {
          fprintf(stderr, "Could not read %s\n", hdf5_path);
          return -1;
        }

				printf("Creating in-memory image...\n");

        // Make an image of the HDF5 file
				ssize_t fsize = H5Fget_file_image(hdf5_file, NULL, 0);
				void* fimage = malloc(fsize);
				H5Fget_file_image(hdf5_file, fimage, fsize);

				// Close the file
				H5Fclose(hdf5_file);

				uint64_t lb[1] = {0}, ub[1] = {fsize};

				printf("Putting in-memory buffer into DataSpace\n");

        // DataSpaces: Put data array into the space
        // Usage: dspaces_put(
        // size (in bytes of each element), dimensions for bounding box,
        // lower bound coordinates, upper bound coordinates,
        // ptr to data buffer
        err = dspaces_put(
						var_name,               // Name of variable
						1,                      // Version num
						sizeof(unsigned char),  // Size (in bytes of each element)
						1,                      // Dimensions for bounding box
						lb,                     // Lower bound coordinates
						ub,                     // Upper bound coordinates
						fimage                  // Ptr to data buffer
				);

				sleep(3);

				if(err < 0) {
					fprintf(stderr, "Put failed: dspaces_put(() returned %d\n", err);
					return -1;
				}

				printf("Variable %s created\n", var_name);

        // DataSpaces: Release our lock on the data
        dspaces_unlock_on_write("my_test_lock", &gcomm);
				free(var_name);
				free(fimage);

        // DataSpaces: Finalize and clean up DS process
        dspaces_finalize();

        MPI_Barrier(gcomm);
        MPI_Finalize();

        return 0;
}
