//---------------------------------------------------------------------------
//
// example of using DIY to perform blocking, assignment of blocks to
//  processes, reading blocks from storage, creating a custom datatype
//  for the block data, and using it to write the data out to storage
//  as a vector of the original data points
//
// Tom Peterka
// Argonne National Laboratory
// 9700 S. Cass Ave.
// Argonne, IL 60439
// tpeterka@mcs.anl.gov
//
// (C) 2011 by Argonne National Laboratory.
// See COPYRIGHT in top-level directory.
//
//--------------------------------------------------------------------------
#include <string.h>
#include "mpi.h"
#include "diy.h"

//
// user-defined callback function for creating a DIY datatype for
//   writing a block
//
// item: pointer to the item
// did: domain id
// lid: local block number
// dtype: pointer to the datatype
//
// side effects: commits the DIY datatype but DIY will cleanup datatype for you
//
void CreateWriteType(void *item, int did, int lid, DIY_Datatype *dtype) {

  // in this example we're just writing a vector af all the data points
  // in the original data block
  int min[3], size[3]; // block extents
  DIY_Block_starts_sizes(did, lid, min, size);
  int block_size = size[0] * size[1] * size[2];
  DIY_Create_vector_datatype(block_size, 1, DIY_INT, dtype);

}

//
// user-defined callback function for allocaing a block and 
//   creating a DIY datatype for reading the block
//
// did: domain id
// lid: local block number
// hdr: block header (not used in this example)
// base_addr: base address associated with the datatype
// dtype: pointer to the datatype
//
// side effects: allocates space for block
//  commits the DIY datatype but DIY will cleanup datatype for you
//
// returns: address of allocated block
//
void *CreateReadType(int did, int lid, int *hdr, DIY_Datatype *dtype) {

  // in this example we're just reading the vector af data points stored earlier
  // we'll need to allocate space for that vector and return that address

  // in a real problem, quantity information would be written in the header
  // in this example, we know the sizes from the original data block size
  int min[3], size[3]; // block extents
  DIY_Block_starts_sizes(did, lid, min, size);
  int block_size = size[0] * size[1] * size[2];

  // create datatype
  DIY_Create_vector_datatype(block_size, 1, DIY_INT, dtype);

  // allocate space and return its address
  int *read_buf = new int[block_size];
  return read_buf;

}

int main(int argc, char **argv) {

  int dim = 3;
  int tot_blocks = 8;
  int data_size[3] = {10, 10, 10}; // {x_size, y_size, z_size}
  int given[3] = {0, 0, 0}; // no constraints on decomposition in {x, y, z}
  int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
  int min[3], max[3], size[3]; // block extents
  char infile[] = "test.dat"; // a pre-made 10x10x10 grid of ints
  char outfile[] = "test.out"; // analysis output file
  int num_threads = 4; // number of threads DIY can use
  int nblocks; // my local number of blocks
  int rank; // MPI process
  int did; // domain id

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // examples don't do any error checking, but real apps should

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size, num_threads, MPI_COMM_WORLD);

  // decompose domain
  did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 1, ghost, 
		      given);

  // allocate pointers to data, in this example, the data type is int
  // the memset to 0 is needed to tell DIY to allocate the memory for us
  int *data[nblocks];
  memset(data, 0, sizeof(int*) * nblocks);

  // read blocks and print block bounds
  for (int i = 0; i < nblocks; i++) { // for all my blocks
    DIY_Block_starts_sizes(did, i, min, size);
    // post a read for the block
    DIY_Add_data_raw(min, size, infile, DIY_INT, (void**)&(data[i]));
    // print the block bounds
    for (int j = 0; j < 3; j++)
      max[j] = min[j] + size[j] - 1;
    fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
	    "max = [%d %d %d] size = [%d %d %d]\n", 
	    rank, i, min[0], min[1], min[2], max[0], max[1], max[2],
	    size[0], size[1], size[2]);
  }
  fprintf(stderr, "\n");

  // read all the blocks that were posted
  DIY_Read_data_all();

  // print the data values in the blocks
  for (int i = 0; i < nblocks; i++) {
    fprintf(stderr, "\nData values for local block %d global block %d:\n", 
	    i, DIY_Gid(did, i));
    DIY_Block_starts_sizes(did, i, min, size);
    for (int j = 0; j < size[0] * size[1] * size[2]; j++)
      fprintf(stderr, "%d ", data[i][j]);
    fprintf(stderr, "\n\n");
  }

  // an example of using headers
  // write the size of each block in a header
  int **hdrs = new int*[nblocks];
  for (int i = 0; i < nblocks; i++) {
    hdrs[i] = new int[DIY_MAX_HDR_ELEMENTS];
    DIY_Block_starts_sizes(did, i, min, size);
    hdrs[i][0] = size[0] * size[1] * size[2];
  }

  // write the results
  // in this example, we are just writing the original data back out as
  // one vector per block
  if (rank == 0)
    fprintf(stderr, "\nWriting analysis results out to storage\n\n");
  DIY_Write_open_all(did, outfile, 0);
  DIY_Write_blocks_all(did, (void **)data, nblocks, hdrs, &CreateWriteType);
  DIY_Write_close_all(did);

  // read the results back in
  // demonstrates how some follow-on task would read analysis results
  // previously written in the DIY format
  void **analysis_blocks;
  int glo_analysis_blocks; // global number of blocks in the file
  int loc_analysis_blocks; // local number of blocks in the file
  DIY_Read_open_all(did, outfile, 0, 0, &glo_analysis_blocks, 
		    &loc_analysis_blocks);
  DIY_Read_blocks_all(did, &analysis_blocks, hdrs, &CreateReadType);
  DIY_Read_close_all(did);
  if (rank == 0)
    fprintf(stderr, "\nReading analysis results back from storage\n\n");
  for (int i = 0; i < loc_analysis_blocks; i++) {
    // Quantity information would normally be stored in a header
    // accompanying the block. Here we just recompute based on the
    // original data block size.
    DIY_Block_starts_sizes(did, i, min, size);
    int num_pts = size[0] * size[1] * size[2];
    fprintf(stderr, "\nAnalysis results read back into local block %d: ", i);
    for (int j = 0; j < num_pts; j++)
      fprintf(stderr, "%d ", ((int **)analysis_blocks)[i][j]);
    fprintf(stderr, "\n\n");
  }

  // cleanup
  for (int i = 0; i < nblocks; i++)
    delete[] hdrs[i];
  delete[] hdrs;

  // finalize DIY before finalizing MPI
  DIY_Finalize();

  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
