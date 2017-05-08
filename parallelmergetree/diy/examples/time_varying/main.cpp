//---------------------------------------------------------------------------
//
// example of using DIY to perform a time-varying analysis consisting of 
//  blocking & process assignment, then during each time block,
//  parallel reading of data, local computation, and 
//  nearest neighbor communication of results,
//  After all time blocks are executed, parallel writing of analyis results
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
// delete local block lid (local ID)
//
void DeleteBlock(int lid) {

  // free memory here for this data block

}
//
// computation for my local block number lid (local ID)
//
void Compute(int did, int lid) {

  // local computation here, producing an item that needs to be sent to 
  // a neighboring block

  // in this case, we are sending a 4D point at the right face of a block to
  // its right-hand (+x) neighbor block
  float p[4]; // point to be sent
  float dest_p[4]; // point marking the destination block
  int mins[4], sizes[4]; // block bounds

  // get a point at the right end of the block bounds
  DIY_Block_starts_sizes(did, lid, mins, sizes);
  p[0] = mins[0] + sizes[0] - 1; // far right side of block in x
  p[1] = mins[1] + sizes[1] * 0.5; // center of block in other dimensions
  p[2] = mins[2] + sizes[2] * 0.5;
  p[3] = mins[3] + sizes[3] * 0.5;

  // locate the destination block with any point inside it
  dest_p[0] = p[0] + 0.1; // past the right edge puts the destination 
                          // one block over to the right
  dest_p[1] = p[1];
  dest_p[2] = p[2];
  dest_p[3] = p[3];

  // enqueue the item for sending to neighbor
  // in this example the point by which the neighbor is identified (5th arg.)
  // is the same as the item (2nd arg.), but this need not be the case
  // because the item can be any generic data
  DIY_Enqueue_item_points(did, lid, (void *)p, NULL, 4 * sizeof(float), 
			  dest_p, 1, NULL);

  // debug
//   fprintf(stderr, "sending point %.1f %.1f %.1f %.1f to the block containing "
// 	  "the point %.1f %.1f %.1f %.1f\n", 
// 	  p[0], p[1], p[2], p[3], dest_p[0], dest_p[1], dest_p[2], dest_p[3]);

}
//
// makes MPI datatype for sending and receiving one item
//
// dtype: pointer to the datatype
//
void ItemDtype(DIY_Datatype *dtype) {

  struct map_block_t map[1] = {
    {DIY_FLOAT, OFST, 4, 0},
  };
  DIY_Create_struct_datatype(0, 1, map, dtype);

}
//
// main
//
int main(int argc, char **argv) {

  int dim = 4; // number of dimensions in the problem
  int space_blocks = 8; // number of spatial blocks
  int time_blocks = 2; // number of temporal blocks
  int tot_blocks = space_blocks * time_blocks; // total number of blocks
  int data_size[4] = {10, 10, 10, 4}; // data size 10x10x10, 4 timesteps
  int min[4], size[4]; // block extents
  int given[4] = {0, 0, 0, 0}; // constraints on blocking (none so far)
  given[3] = time_blocks; // impose a constraint on the time blocking
  int ghost[8] = {0, 0, 0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z, -t, +t
  char *infiles[] = { (char *)"test0.dat", (char *)"test1.dat", 
		      (char *)"test2.dat", (char *)"test3.dat" };
  char outfile[] = "test.out";
  int num_threads = 4; // number of threads DIY can use
  int nblocks; // my local number of blocks
  int time_block; // a certain time block
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

  // allocate data, integer for this example
  int *data[nblocks];
  for (int i = 0; i < nblocks; i++) {
    DIY_Block_starts_sizes(did, i, min, size);
    data[i] = new int[size[0] * size[1] * size[2] * size[3]];
  }

  // do the analysis
  for (int g = 0; g < time_blocks; g++) {  // for all time blocks

    // delete old blocks
    if (g > 0) {
      for (int i = 0; i < nblocks; i++) {
	DIY_In_time_block(did, i, &time_block);
	if (time_block < g)
	  DeleteBlock(i);
      }
    }

    // load blocks for this time block
    for (int i = 0; i < nblocks; i++) { // for all my blocks
      DIY_In_time_block(did, i, &time_block);
      if (time_block == g) {
	DIY_Block_starts_sizes(did, i, min, size);
	// post a read for each time step in this block
	int *p = data[i];
	for (int j = min[3]; j < min[3] + size[3]; j++) {
	  fprintf(stderr, "group = %d block %d min = %d %d %d "
		  "size = %d %d %d timestep = %d\n", 
		  g, i, min[0], min[1], min[2], size[0], size[1], size[2], j);
	  DIY_Add_data_raw(min, size, infiles[j], DIY_INT, (void **)&p);
	  p += size[0] * size[1] * size[2];
	}
      }
    }
    DIY_Read_data_all();
    fprintf(stderr, "Read group %d successfully\n", g);

    // do the computation for the current blocks in memory
    for (int i = 0; i < nblocks; i++) {
      DIY_In_time_block(did, i, &time_block);
      if (time_block == g)
	Compute(did, i);
    }

    // received items from neighbors
    void ***items = new void**[nblocks]; // received items
    int *num_items = new int[nblocks]; // number of received items in each block

    // exchange neighbors
    DIY_Exchange_neighbors(did, items, num_items, 1.0, &ItemDtype);

    // do something useful with the ghost cells
    // in this example just print them out
    for (int i = 0; i < nblocks; i++) {
      if (num_items[i]) {
	fprintf(stderr, "%d ghost cells received by block %d: ", 
		num_items[i], i);
	for (int j = 0; j < num_items[i]; j++)
	  fprintf(stderr, "[%.1f %.1f %.1f %.1f] ",
		  ((float *)items[i][j])[0],
		  ((float *)items[i][j])[1],
		  ((float *)items[i][j])[2],
		  ((float *)items[i][j])[3]);
	fprintf(stderr, "\n");
      }
    }

    // flush any remaining messages
    // if multiple rounds of compute / exchange neighbors, call FlushNeighbors
    // once for each time block, after the rounds complete
    DIY_Flush_neighbors(did, items, num_items, &ItemDtype);

    delete[] num_items;

  } // for all time blocks

  // write the results
  DIY_Write_open_all(did, outfile, 0);
  // this example isn't quite done yet, not ready to write out
//   DIY_Write_blocks_all((void **)&items[0], space_blocks, 0, &CreateType);
  DIY_Write_close_all(did);

  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
