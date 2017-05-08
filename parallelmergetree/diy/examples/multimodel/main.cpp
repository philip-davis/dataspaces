//---------------------------------------------------------------------------
//
// example of using DIY to analyze multiple domains
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
// user-defined callback function for creating an MPI datatype for
//   writing a block
//
// item: pointer to the item
// did: domain id
// lid: local block number
// dtype: pointer to the datatype
//
// side effects: commits the MPI datatype but DIY will cleanup datatype for you
//
void CreateWriteType(void *item, int did, int lid, DIY_Datatype *dtype) {

  // in this example we're just writing a vector af all the data points
  // in the original data block
  int min[3], size[3]; // block extents
  DIY_Block_starts_sizes(did, lid, min, size);
  int block_size = size[0] * size[1] * size[2];
  DIY_Create_vector_datatype(block_size, 1, DIY_INT, dtype);

}

int main(int argc, char **argv) {

  int dim = 3;
  int tot_blocks = 8;
  int data_size[3] = {10, 10, 10}; // {x_size, y_size, z_size}
  int given[3]; // no constraints on decomposition in {x, y, z}
  int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
  int min[3], max[3], size[3]; // block extents
  char infile0[] = "test0.dat"; // a pre-made 10x10x10 grid of ints
  char infile1[] = "test1.dat"; // another test data set
  char outfile0[] = "test0.out"; // analysis output file
  char outfile1[] = "test1.out"; // analysis output file
  int num_threads = 4; // number of threads DIY can use
  int nblocks[2]; // my local number of blocks in each domain
  int dids[2]; // domain ids
  int rank; // MPI process

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size, num_threads, MPI_COMM_WORLD);

  // decompose first domain
  tot_blocks = 8;
  given[0] = 0; given[1] = 0; given[2] = 0;
  dids[0] = DIY_Decompose(ROUND_ROBIN_ORDER, 
			 tot_blocks, &nblocks[0], 1, ghost, given);

  // decompose second domain
  tot_blocks = 4;
  given[0] = 1; given[1] = 0; given[2] = 0;
  dids[1] = DIY_Decompose(ROUND_ROBIN_ORDER, 
			 tot_blocks, &nblocks[1], 1, ghost, given);

  // allocate pointers to data, in this example, the data type is int
  // the memset to 0 is needed to tell DIY to allocate the memory for us
  int *data0[nblocks[0]];
  memset(data0, 0, sizeof(int*) * nblocks[0]);
  int *data1[nblocks[1]];
  memset(data1, 0, sizeof(int*) * nblocks[1]);

  // read first data set and print block bounds
  fprintf(stderr, "Domain 0\n");
  for (int i = 0; i < nblocks[0]; i++) { // for all my blocks
    DIY_Block_starts_sizes(0, i, min, size);
    DIY_Add_data_raw(min, size, infile0, DIY_INT, (void**)&(data0[i]));
    for (int j = 0; j < 3; j++)
      max[j] = min[j] + size[j] - 1;
    fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
	    "max = [%d %d %d] size = [%d %d %d]\n", 
	    rank, i, min[0], min[1], min[2], max[0], max[1], max[2],
	    size[0], size[1], size[2]);
  }
  fprintf(stderr, "\n");
  DIY_Read_data_all();

  // read second data set and print block bounds
  fprintf(stderr, "Domain 1\n");
  for (int i = 0; i < nblocks[1]; i++) { // for all my blocks
    DIY_Block_starts_sizes(1, i, min, size);
    DIY_Add_data_raw(min, size, infile1, DIY_INT, (void**)&(data1[i]));
    for (int j = 0; j < 3; j++)
      max[j] = min[j] + size[j] - 1;
    fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
	    "max = [%d %d %d] size = [%d %d %d]\n", 
	    rank, i, min[0], min[1], min[2], max[0], max[1], max[2],
	    size[0], size[1], size[2]);
  }
  fprintf(stderr, "\n");
  DIY_Read_data_all();

  // print the data values in the first domain (just the first block)
  fprintf(stderr, "Domain 0\n");
  fprintf(stderr, "\nData values for local block 0 global block %d:\n", 
	  DIY_Gid(0, 0));
  DIY_Block_starts_sizes(0, 0, min, size);
  for (int j = 0; j < size[0] * size[1] * size[2]; j++)
    fprintf(stderr, "%d ", data0[0][j]);
  fprintf(stderr, "\n\n");

  // print the data values in the second domain (just the first block)
  fprintf(stderr, "Domain 1\n");
  fprintf(stderr, "\nData values for local block 0 global block %d:\n", 
	  DIY_Gid(1, 0));
  DIY_Block_starts_sizes(1, 0, min, size);
  for (int j = 0; j < size[0] * size[1] * size[2]; j++)
    fprintf(stderr, "%d ", data1[0][j]);
  fprintf(stderr, "\n\n");

  // to send data from one domain to another, use DIY_Send and DIY_Recv
  // these are the "remote" functions that can cross domains
  // other communication patterns (merge, swap, neighborhood) are restricted
  // to stay in a particular domain

  // send 10 data values from the first block of domain 0 to the first
  // block of domain 1, identified by its gid
  DIY_Send(dids[0], 0, data0[0], 10, MPI_INT, DIY_Gid(dids[1], 0));

  // receive at the second domain
  void **recv_data = new void*[1]; // only expecting one item
  int count;
  int src_gids[1]; // source gids (only valid for MPI-3) only one item expected
  int sizes[1]; // sizes of each item in datatype units (not bytes)
  DIY_Recv(dids[1], 0, recv_data, &count, 1, MPI_INT, src_gids, sizes);
  fprintf(stderr, "Received at block 0 of domain 1: ");
  for (int i = 0; i < count; i++) {
    for(int j = 0; j < sizes[i]; j++)
      fprintf(stderr, "%d ", ((int **)recv_data)[0][j]);
    fprintf(stderr, "\n");
  }
  DIY_Flush_send_recv(0);

  // write the results
  // in this example, we are just writing the original data back out as
  // one vector per block

  // first domain
  if (rank == 0)
    fprintf(stderr, "\nWriting analysis results of domain 0 to storage\n\n");
  DIY_Write_open_all(dids[0], outfile0, 0);
  DIY_Write_blocks_all(dids[0], (void **)data0, nblocks[0], NULL, 
		       &CreateWriteType);
  DIY_Write_close_all(dids[0]);

  // seond domain
  if (rank == 0)
    fprintf(stderr, "\nWriting analysis results of domain 1 to storage\n\n");
  DIY_Write_open_all(dids[1], outfile1, 0);
  DIY_Write_blocks_all(dids[1], (void **)data1, nblocks[1], NULL, 
		       &CreateWriteType);
  DIY_Write_close_all(dids[1]);

  // cleanup
  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
