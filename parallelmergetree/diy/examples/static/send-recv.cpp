//---------------------------------------------------------------------------
//
// Example of using DIY to perform sasynchronous sending
//  and receiving of data to remote blocks
//
// Tom Peterka
// Argonne National Laboratory
// 9700 S. Cass Ave.
// Argonne, IL 60439
// tpeterka@mcs.anl.gov
//
// All rights reserved. May not be used, modified, or copied
// without permission
//
//--------------------------------------------------------------------------
#include <mpi.h>
#include <stdlib.h>
#include <stddef.h>
#include "diy.h"

using namespace std;

//--------------------------------------------------------------------------
//
// main
//
int main(int argc, char **argv) {

  int dim = 3; // number of dimensions in the problem
  int tot_blocks = 8; // total number of blocks
  int data_size[3] = {10, 10, 10}; // data size 10x10x10
  int min[3], size[3]; // block extents
  int given[3] = {0, 0, 0}; // constraints on blocking (none so far)
  int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
  char infile[] = "test.dat";
  int num_threads = 4; // number of threads DIY can use
  int nblocks; // my local number of blocks
  int rank; // MPI process
  int did; // domain id

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // examples don't do any error checking, but real apps should

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size,
	   num_threads, MPI_COMM_WORLD);

  // decompose domain
  did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 1, ghost, 
		      given);

  // read data, assume integer, raw format
  int *data[nblocks];
  memset(data, 0, sizeof(int*) * nblocks); // memset tells DIY to allocate
                                           // data for us
  for (int i = 0; i < nblocks; i++) {
    DIY_Block_starts_sizes(did, i, min, size);
    fprintf(stderr, "block %d min = %d %d %d "
	    "size = %d %d %d\n", i, min[0], min[1], min[2], 
	    size[0], size[1], size[2]);
    DIY_Add_data_raw(min, size, infile, DIY_INT, (void**)&(data[i]));
  }
  DIY_Read_data_all();

  fprintf(stderr, "\n");

  // test DIY point to point asynchronous communication by sending
  // the first 4 data values (for example) in a ring of blocks 
  // ordered by gid
  for (int i = 0; i < nblocks; i++) {
    int dest_gid = (DIY_Gid(did, i) + 1 ) % tot_blocks;
    int src_gid = (DIY_Gid(did, i) + tot_blocks - 1) % tot_blocks;
    DIY_Send(did, i, data[i], 4, MPI_INT, dest_gid);
  }
  void **recv_data = new void*[nblocks];
  int src_gids[1]; // only one source for each local block in this example
  int sizes[1]; // size of each item in datatype units (not bytes)
  int count; // number of received items (should be 1 in this example)
  for (int i = 0; i < nblocks; i++) {
    DIY_Recv(did, i, recv_data, &count, 1, MPI_INT, src_gids, sizes);
    assert(count == 1);
    fprintf(stderr, "gid %d received data values %d - %d from gid %d\n",
	    DIY_Gid(did, i), ((int **)recv_data)[0][0], 
	    ((int **)recv_data)[0][3], src_gids[0]);
  }
  DIY_Flush_send_recv(0);

  // cleanup
  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
