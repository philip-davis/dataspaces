#include "insituTopology.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "ParallelTopology/PointIndex.h"
#include "ParallelTopology/TBox.h"
#include "ParallelTopology/Patch.h"
#include "ParallelTopology/DataSpaceOutputStream.h"
#include "ParallelTopology/LocalComputeAlgorithm.h"
#include "insitu_data_description.h"

bool liftedEthylene = true;

int varIds[gNumInsituVars_ghost] = {0, 1};
double *dataPtr = NULL;
SubBox *box;
vector<const double * > patchData(gNumInsituVars_ghost);
Patch *patch;
int nprocs;
PointIndex globalDimensions;

void topology_init(int *myid, MPI_Comm *comm, int *global_dim, int *xyzpes, MultiFab *insitu_vars, int num_ghost)
{
  if ( ! *myid ) printf("Initializing topology \n" );
  Box domain;
  for( MFIter mfi(*(insitu_vars)); mfi.isValid(); ++mfi){
    BL_ASSERT( dataPtr == NULL );
    dataPtr = ((*insitu_vars)[mfi]).dataPtr();
    domain = ((*insitu_vars)[mfi]).box();
  }
 

  int nx = (domain.bigEnd(0)-domain.smallEnd(0))+1;
  int ny = (domain.bigEnd(1)-domain.smallEnd(1))+1;
  int nz = (domain.bigEnd(2)-domain.smallEnd(2))+1;
  int npts = nx*ny*nz;
  nprocs = xyzpes[0]*xyzpes[1]*xyzpes[2];

  PointIndex zeroPoint(0.0, 0.0, 0.0);
  globalDimensions = PointIndex(global_dim[0], global_dim[1], global_dim[2]);
  PointIndex globalLow(-1.*num_ghost, -1.*num_ghost, -1.*num_ghost);
  PointIndex globalHigh(globalDimensions[0] + num_ghost, globalDimensions[1] + num_ghost, globalDimensions[2] + num_ghost); 
  TBox* globalBox = new TBox(zeroPoint, globalDimensions, globalLow, globalHigh);
  GlobalIndexType globalSize = globalDimensions.size(); 

  PointIndex lowerLeft(domain.smallEnd(0), domain.smallEnd(1), domain.smallEnd(2));
  PointIndex upperRight(domain.bigEnd(0), domain.bigEnd(1), domain.bigEnd(2));

  PointIndex low(lowerLeft[0]+num_ghost, lowerLeft[1]+num_ghost, lowerLeft[2]+num_ghost);
  PointIndex high;
  for(int i=0; i < 3; i++) {
    if(upperRight[i] == globalHigh[i]) high[i] = globalDimensions[i];  
    else high[i] = upperRight[i];
    //otherwise it is already set with 1 ghost
  }

  box = new SubBox(globalBox, low, high, lowerLeft, upperRight);
 
  
//  char filename[256];
  

  for(uint32_t i=0; i < gNumInsituVars_ghost; i++) {
    patchData[i] = dataPtr + npts * i;
 //   sprintf(filename, "output%d_%d.txt", i, *myid);
 //   ofstream file(filename);
 //   for(uint32_t j=0; j < npts; j++) {
 //     file << patchData[i][j] << " ";
 //   }
 //   file << endl;
 //   file << "npts : = " << npts << ": "  << nx << ", " << ny << ", " << nz << endl;
 //   file << "lowerLeft = " << lowerLeft[0] << ", " << lowerLeft[1] << ", " << lowerLeft[2] << endl;
 //   file << "low= " << low[0] << ", " << low[1] << ", " << low[2] << endl;
 //   file << "upperRight = " << upperRight[0] << ", " << upperRight[1] << ", " << upperRight[2] << endl;
 //   file << "high= " << high[0] << ", " << high[1] << ", " << high[2] << endl;
 //   file << "globalLow = " << globalLow[0] << ", " << globalLow[1] << ", " << globalLow[2] << endl;
 //   file << "zero = " << zeroPoint[0] << ", " << zeroPoint[1] << ", " << zeroPoint[2] << endl;
 //   file << "globalHigh = " << globalHigh[0] << ", " << globalHigh[1] << ", " << globalHigh[2] << endl;
 //   file << "globalDimensions = " << globalDimensions[0] << ", " << globalDimensions[1] << ", " << globalDimensions[2] << endl;
 //   file.close();
  }
  patch = new Patch(patchData, *box);
 
}

void topology_tstep( int *myid, int tstep, MPI_Comm *comm)
{

  if ( ! *myid )
  {
    printf("Running topology %d \n", tstep );
  }

  SegmentedMergeTree* local_tree;
  DataSpaceOutputStream* output;
  uint8_t block_bits = 10;
  LocalComputeAlgorithm* algorithm = LocalComputeAlgorithm::make(LOCAL_SORTED_UF,false);

  // Create the local tree and the coresponding stream
  local_tree = new SegmentedMergeTree(*myid, block_bits);
  output = new DataSpaceOutputStream(*myid, tstep, nprocs);

  // Compute the local tree
  if(liftedEthylene) {
  	algorithm->apply(patch,0,*local_tree,output, 0.00001);
  } else {
        if(!*myid) std::cout << "timestep " << tstep << " lowerbound = " << 0.000000001 << std::endl;
  	algorithm->apply(patch,0,*local_tree,output, 0.000000001);
  }

  // Flush the info to the stream
  output->flush();

  delete output;
  delete local_tree;

 return;
}

void topology_finalize( int * myid )
{
  if(! *myid) printf("finalizing topology!\n");
  delete box;
  delete patch;

}

