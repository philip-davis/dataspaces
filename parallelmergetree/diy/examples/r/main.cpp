//---------------------------------------------------------------------------
//
// example of parallelizing R over DIY
//
// R and the RInside and Rcpp packages need to be installed for this to work
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
#include "mpi.h"
#include "diy.h"
#include <RInside.h>
#include <Rcpp.h>

using namespace Rcpp;

int main(int argc, char *argv[]) {

  // made-up data sizes, number of blocks
  int dim = 3;
  int tot_blocks = 8;
  int data_size[3] = {10, 10, 10}; // {x_size, y_size, z_size}
  int given[3] = {0, 0, 0}; // no constraints on decomposition in {x, y, z}
  int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
  int nblocks; // my local number of blocks
  int num_threads = 4; // number of threads DIY can use
  int rank; // MPI process
  int did; // domain id

  SEXP res; // standard R output type

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size,
	   num_threads, MPI_COMM_WORLD);

  // decompose domain
  did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 1, ghost, 
		      given);

  // compute a normal distribution for each of my local blocks

  RInside R(argc, argv); // create an embedded R instance 

  for (int b = 0; b < nblocks; b++) {

    Language call("rnorm", 10, Named("sd", 100.0)); // create an R call
    res = call.eval(); // evaluate the call
    NumericVector v(res); // convert SEXP res to Rcpp vector of doubles

    // print the results
    for (int i = 0; i < v.size(); i++)
      fprintf(stderr, "Block %d: v[%d] = %.3lf\n", b, i, v[i]);

  }

  // todo: merge the distributions and compute a global pdf (using R)

  // cleanup
  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");

  return 0;

}

//----------------------------------------------------------------------------
