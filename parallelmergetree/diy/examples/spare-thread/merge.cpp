//---------------------------------------------------------------------------
//
// example of using DIY in a spare thread to compute a
// probability mass function for a dataset
//
// this example uses pthreads, so diy must be configured using the
// --disable-openmp flag because pthreads and openmp do not like to coexist
//
// Tom Peterka
// Argonne National Laboratory
// 9700 S. Cass Ave.
// Argonne, IL 60439
// tpeterka@mcs.anl.gov
//
// (C) 2012 by Argonne National Laboratory.
// See COPYRIGHT in top-level directory.
//
//--------------------------------------------------------------------------
#include <string.h>
#include "mpi.h"
#include <math.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>
#include "diy.h"

// some globals for the sample problem
int time_steps = 10; // number of simulation time steps
int num_bins = 16; // number of histogram bins
int min_data_val; // minimum data value
int max_data_val; // maximum data value
int dim = 3; // number of dimensions in the problem
int tot_blocks = 8; // total number of blocks
int nblocks; //local number of blocks
int data_size[3] = {10, 10, 10}; // data size
int given[3] = {0, 0, 0}; // constraints on blocking (none)
int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
int block_size = 125; // each block is this many values
int rounds = 2; // two rounds of merging
int kvalues[2] = {4, 2}; // k-way merging, eg 4-way followed by 2-way merge
char outfile[] = "test.out";
int num_threads = 1; // number of threads DIY can use

//
// Counts the number of data values in each bin
//
void Count(int *data, int *bins, int num_data) {

  // step size of each histogram bin
  int step =  (int)(ceil(((double)max_data_val - min_data_val) / num_bins)); 

  memset(bins, 0, num_bins * sizeof(int));
  for (int i = 0; i < num_data; i++)
    bins[data[i]  / step]++;

}
//
// user-defined callback function for merging an array of items
// in this example we compute a global histogram by merging individual ones
//
// items: pointers to input / output items, result in items[0]
// gids: gloabl ids of items to be reduced (not used in this example, but
//  needed when reduction is noncommutative and item order matters
// num_items: total number of input items
// hdr: quantity information (unused in this example)
//
// char* is used as a generic pointers to bytes, not necessarily to strings
//
void ComputeMerge(char **items, int *gids, int num_items, int *hdr) {

  for (int i = 1; i < num_items; i++) {
    for (int j = 0; j < num_bins; j++)
      ((int **)items)[0][j] += ((int **)items)[i][j];
  }

}
//
// user-defined callback function for creating a received item
//
// hdr: quantity information for allocating custom parts of the item
//  (not used in this example)
// char* is used as a generic pointers to bytes, not necessarily to strings
//
// side effects: allocates the item
//
// returns: pointer to the item
//
char *CreateItem(int *hdr) {

  int *bins = new int[num_bins];
  return (char *)bins;

}
//
// user-defined callback function for destroying a received item
//
// item: item to be destroyed
//
void DestroyItem(void *item) {

  delete[] (int *)item;

}
//
// user-defined callback function for creating an MPI datatype for the
//   received item being merged
//
// item: pointer to the item
// dtype: pointer to the datatype
// hdr: quantity information (unused in this example)
//
// side effects: commits the datatype which DIY will cleanup for you
//
// returns: base address associated with the datatype
//
void *CreateMergeType(void *item, DIY_Datatype *dtype, int *hdr) {

  DIY_Create_vector_datatype(num_bins, 1, DIY_INT, dtype);
  return item;

}
//
// user-defined callback function for creating an MPI datatype for the
//   item being written
//
// item: pointer to the item
// did: domain id
// lid: local block number (unused in this example)
// dtype: pointer to the datatype
//
// side effects: commits the datatype which DIY will cleanup for you
//
// returns: base address associated with the datatype
//
void *CreateWriteType(void *item, int did, int lid, DIY_Datatype *dtype) {

  DIY_Create_vector_datatype(num_bins, 1, DIY_INT, dtype);
  return item;

}
//
// find local and global data range
//
void FindRange(int rank, int **data) {

  int global_min, global_max; // global min, max
  int local_min = data[0][0];
  int local_max = data[0][0];
  for (int i = 0; i < nblocks; i++) {
    for (int j = 0; j < block_size; j++) {
      if (data[i][j] < local_min)
	local_min = data[i][j];
      if (data[i][j] > local_max)
	local_max = data[i][j];
    }
  }
  MPI_Reduce(&local_min, &global_min, 1, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&local_max, &global_max, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
  if (rank == 0)
    fprintf(stderr, "global min = %d global max = %d\n", global_min,
	    global_max);
}
//
// analysis function
//
// ptr: data to be analyzed (pass NULL if done)
//
void *analysis_fun(void *ptr) {

  static int time_step = 0; // current time step
  static int did; // domain id

  // first time, initialize DIY
  if (time_step == 0) {
    DIY_Init(dim, data_size,
	     num_threads, MPI_COMM_WORLD);
    did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 0, ghost, 
			given);
  }

  // check if done, finalize DIY
  if (ptr == NULL) {
    DIY_Finalize();
    return NULL;
  }

  int **data = (int **)ptr; // assign void* to correct type

  // perform a local analysis, for example, compute a histogram
  int **bins; // histogram bins for each data block
  bins = new int*[nblocks];
  for (int b = 0; b < nblocks; b++) { // all my blocks
    bins[b] = new int[num_bins];
    Count(data[b], bins[b], block_size);
  }

  // merge the analysis results
  int nb_merged; // number of output merged blocks
  DIY_Merge_blocks(did, (char**)bins, (int **)NULL, rounds, kvalues, 
		   &ComputeMerge, &CreateItem, &DestroyItem, &CreateMergeType, 
		   &nb_merged);

  // print the histograms
  for (int b = 0; b < nb_merged; b++) {
    for (int i = 0; i < num_bins; i++)
      fprintf(stderr, "bins[%d][%d] = %d\n", b, i, bins[b][i]);
  }
  fprintf(stderr, "\n");

  // write the results
  char outfile_step[256];
  snprintf(outfile_step, 256, "%s-%d", outfile, time_step);
  DIY_Write_open_all(did, outfile_step, 0);
  DIY_Write_blocks_all(did, (void **)bins, nb_merged, NULL, &CreateWriteType);
  DIY_Write_close_all(did);

  // cleanup
  for (int b = 0; b < nblocks; b++)
    delete[] bins[b];
  delete[] bins;

  time_step++;
  return NULL;

}
//
// main
//
int main(int argc, char **argv) {

  int t; // time step
  min_data_val = 0; // minimum data value
  max_data_val = 1000 * time_steps - 1; // maximum data value

  // init MPI
  int rank, groupsize; // MPI usual
  int mpi_thread_level; // provided MPI thead level
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &mpi_thread_level);
  assert(MPI_THREAD_MULTIPLE == mpi_thread_level);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &groupsize);

  // init simulation and analysis data
  int bp = tot_blocks / groupsize; // blocks per process
  nblocks = (rank == groupsize - 1 ? tot_blocks - (rank * bp) : bp);
  int *sim_data[nblocks];
  int *analysis_data[nblocks];
  for (int i = 0; i < nblocks; i++) {
    sim_data[i] = new int[block_size];
    analysis_data[i] = new int[block_size];
  }

  // compute the first time step (t = 0) of simulation data 
  // outside the main time step loop
  t = 0;
  for (int i = 0; i < nblocks; i++) {
    for (int j = 0; j < block_size; j++)
      sim_data[i][j] = (t * 1000) + (rank * bp + i) * block_size + j;
  }

  // simulation time step loop
  for (t = 1; t < time_steps; t++) { // t = 0 computed above

    // copy simulation data to analysis data
    for (int i = 0; i < nblocks; i++) {
      for (int j = 0; j < block_size; j++)
	analysis_data[i][j] = sim_data[i][j];
    }

    // spawn a thread for analysis
    pthread_t analysis_thread;
    int tid = pthread_create(&analysis_thread, NULL, &analysis_fun, 
			     (void *)analysis_data);

    // meanwhile, the simulation does its own MPI communication
    // in this example, a reduction to find global data extents
    FindRange(rank, sim_data);

    // compute the next time step of simulation data
    for (int i = 0; i < nblocks; i++) {
      for (int j = 0; j < block_size; j++)
	sim_data[i][j] = t * 1000 + (rank * bp + i) * block_size + j;
    }

    // join main thread and analysis thread
    pthread_join(analysis_thread, NULL);

    // find range of last time step
    if (t == time_steps - 1)
      FindRange(rank, sim_data);

  } // simulation time step loop

  // copy last time step of simulation data to analysis data
  for (int i = 0; i < nblocks; i++) {
    for (int j = 0; j < block_size; j++)
      analysis_data[i][j] = sim_data[i][j];
  }

  // last time step still needs to be analyzed
  analysis_fun((void *)analysis_data);

  // cleanup
  analysis_fun(NULL); // shuts down analysis
  MPI_Finalize();
  for (int i = 0; i < nblocks; i++) {
    delete[] sim_data[i];
    delete[] analysis_data[i];
  }
  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
