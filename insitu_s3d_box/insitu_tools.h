#ifndef INSITU_TOOLS_H
#define INSITU_TOOLS_H

#include "multifab_tools.h"
//#include "insituDescriptiveStats.h"
//#include "s3d_box_viz.h"
//#include "insituTopology.h"
// #include "autocorrelation.h"

static const int gNumAnalyses = 4;

/* frequency for each analysis operation */
static const int gAnalysisFrequency[gNumAnalyses] = { 4, 4, 4, 4, };

/* a string describing each analysis */
static const char* gAnalyisTypes[gNumAnalyses] = {
  "topology",
  "visualization",
  "skewness statistics",
  "autocorrelation",
};

void initialize_insitu(int *myid, MPI_Comm *comm, int *xyzpes, int *xyzid, int *global_dim, MultiFab *insitu_vars_ghost, MultiFab *insitu_vars_noghost, int num_ghost);
void perform_insitu_analysis(int *myid, MPI_Comm *comm, int tstep);
void finalize_insitu(int *myid, MPI_Comm *comm);
bool insitu_timestep(int tstep);


#endif
