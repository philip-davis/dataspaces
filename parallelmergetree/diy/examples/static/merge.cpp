//---------------------------------------------------------------------------
//
// example of using DIY to perform static analysis of computing a
// probability mass function for the dataset
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
#include <math.h>
#include <stdlib.h>
#include <stddef.h>
#include "diy.h"

// some globals for the sample problem
int num_bins = 10; // number of histogram bins
int min_data_val = 0; // minimum data value
int max_data_val = 999; // maximum data value
int dim = 3; // number of dimensions in the problem
int tot_blocks = 8; // total number of blocks
int data_size[3] = {10, 10, 10}; // data size
int given[3] = {0, 0, 0}; // constraints on blocking (none)
int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
int rounds = 2; // two rounds of merging
int kvalues[2] = {4, 2}; // k-way merging, eg 4-way followed by 2-way merge
char infile[] = "test.dat";
char outfile[] = "test.out";

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
//   char * is used as a generic pointers to bytes, not strings
// gids: gloabl ids of items to be reduced (not used in this example, but
//  needed for example, when reduction is noncommutative and item order matters
// num_items: total number of input items
// hdr: quantity information for items[0] (unused in this example)
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
//
// char * is used as a generic pointers to bytes, not necessarily to strings
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
// side effects: commits the DIY datatype but DIY will cleanup datatype for you
//
void CreateMergeType(void *item, DIY_Datatype *dtype, int *hdr) {

  DIY_Create_vector_datatype(num_bins, 1, DIY_INT, dtype);

}
//
// main
//
int main(int argc, char **argv) {

  int min[3], size[3]; // block extents
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
  did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 0, ghost, 
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

  // perform a local analysis, for example, compute a histogram
  int **bins; // histogram bins for each data block
  bins = new int*[nblocks];
  for (int b = 0; b < nblocks; b++) { // all my blocks
    bins[b] = new int[num_bins];
    DIY_Block_starts_sizes(did, b, min, size);
    Count(data[b], bins[b], size[0] * size[1] * size[2]);
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

  // if we want to covert the resulting type, say from histogram to
  // probability (int to float), do that here, not in the merge function,
  // where the data type must remain consistent

  // compute probabilities
  int tot_data = data_size[0] * data_size[1] * data_size[2];
  // again, can be > 1 pmf block, depending on degree of merging
  // (hence, double pointer)
  float **pmf = NULL; // probability mass function

  if (nb_merged) {
    // again, can be > 1 pmf block, depending on degree of merging
    pmf = new float*[nb_merged]; // probability mass function
    for (int b = 0; b < nb_merged; b++) {
      pmf[b] = new float[num_bins];
      for (int i = 0; i < num_bins; i++)
	pmf[b][i] = (float)bins[b][i] / tot_data;
    }

    // print the probabilities
    for (int b = 0; b < nb_merged; b++) {
      for (int i = 0; i < num_bins; i++)
	fprintf(stderr, "pmf[%d][%d] = %lf\n", b, i, pmf[b][i]);
    }
  }

  // cleanup
  for (int b = 0; b < nblocks; b++)
    delete[] bins[b];
  delete[] bins;
  for (int b = 0; b < nb_merged; b++)
    delete[] pmf[b];
  delete[] pmf;

  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
