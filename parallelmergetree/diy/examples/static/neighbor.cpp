//---------------------------------------------------------------------------
//
// Example of using DIY to perform a neighborhood exchange, for example,
//  to exchange ghost zone region.
//  In this example, data is transferred only to neighbors in the -X, -Y, -Z
//  directions (asymmetric exchange).
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
#include <vector>
#include <stdlib.h>
#include <stddef.h>
#include "diy.h"

using namespace std;

//
// computation for my local block number lid (local ID)
//
void Compute(int did, int lid) {

  // local computation here, producing an item that needs to be sent to 
  // a neighboring block

  // in this case, we are sending a scalar value to its left, bottom, back
  // neighbors
  int val; // value to be sent

  // for the ghost data to be sent, this example just sends its global block id
  val = DIY_Gid(did, lid);

  fprintf(stderr, "Local block %d sending value %d\n",
	  lid, val);

  // locate the destination block with an array of directions
  // in this example sending to half the neighbors
  unsigned char dirs[13] = {DIY_X0, 
			    DIY_Y0, 
			    DIY_Z0,
			    DIY_X0 | DIY_Y0,
			    DIY_Y0 | DIY_Z0,
			    DIY_X0 | DIY_Z0, 
			    DIY_X0 | DIY_Y1,
			    DIY_Y0 | DIY_Z1,
			    DIY_X1 | DIY_Z0,
			    DIY_X0 | DIY_Y0 | DIY_Z0,
			    DIY_X0 | DIY_Y1 | DIY_Z0,
			    DIY_X0 | DIY_Y0 | DIY_Z1,
			    DIY_X1 | DIY_Y0 | DIY_Z0};

  // enqueue the item for sending to neighbor
  // in this example the point by which the neighbor is identified (5th arg.)
  // is the same as the item (2nd arg.), but this need not be the case
  // because the item can be any generic data
  DIY_Enqueue_item_dirs(did, lid, (void *)&val, NULL, sizeof(int), 
			  dirs, 13, NULL);

}
//
// makes DIY datatype for sending and receiving one item
//
// dtype: pointer to the datatype
//
void ItemDtype(DIY_Datatype *dtype) {

  struct map_block_t map[1] = {
    {DIY_INT, OFST, 1, 0},
  };
  DIY_Create_struct_datatype(0, 1, map, dtype);

}
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
  int num_threads = 1; // number of threads DIY can use
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

  // do some local analysis
  for (int b = 0; b < nblocks; b++)
    Compute(did, b);

  // received items from neighbors
  void ***items = new void**[nblocks]; // received items
  int *num_items = new int[nblocks]; // number of received items in each block

  // exchange neighbors
  DIY_Exchange_neighbors(did, items, num_items, 1.0, &ItemDtype);

  // do something useful with the ghost cells
  // in this example just print them out
  for (int i = 0; i < nblocks; i++) {
    if (num_items[i]) {
      fprintf(stderr, "%d ghost cells received by local block %d: ", 
	      num_items[i], i);
      for (int j = 0; j < num_items[i]; j++)
	fprintf(stderr, "%d ",
		((int *)items[i][j])[0]);
      fprintf(stderr, "\n");
    }
  }

  // flush any remaining messages
  // if multiple rounds of compute / exchange neighbors, call FlushNeighbors
  // once for each time block, after the rounds complete
  DIY_Flush_neighbors(did, items, num_items, &ItemDtype);

  // cleanup
  delete[] num_items;

  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
