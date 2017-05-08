#include <Box.H>
#include <BoxArray.H>
#include <MultiFab.H>
#include "stdio.h"
#include "insitu_tools.h"


void initialize_insitu(int *myid, MPI_Comm *comm, int *xyzpes, int *xyzid, int * totalDimensions, MultiFab *insitu_vars_ghost, MultiFab *insitu_vars_noghost, int num_ghost) {
//  printf("initializing insitu computations\n");
  //s3d_box_viz_init(myid, comm, xyzpes, xyzid, insitu_vars_noghost);
  //topology_init(myid, comm, totalDimensions, xyzpes, insitu_vars_ghost, num_ghost);
  //descriptiveStats_init_entirely_insitu(myid, comm, xyzpes, totalDimensions, insitu_vars_noghost);
  //descriptiveStats_init_partial_insitu(myid, comm, xyzpes, totalDimensions, insitu_vars_noghost);
  //autocorrelation_init(myid, comm, xyzpes, insitu_vars_noghost);
}


void perform_insitu_analysis(int *myid, MPI_Comm *comm, int tstep) {
  //printf("instiu analysis for timestep: %d\n", tstep);
  /*
  if((tstep % gAnalysisFrequency[1]) == 0) {
    MPI_Barrier(*comm);
    double render_time = -1 * MPI_Wtime();

    s3d_box_insitu_render_tstep(myid, comm, tstep);

    MPI_Barrier(*comm);
    render_time += MPI_Wtime();   
    if (*myid == 0) {
        fprintf(stderr, "EVAL: tstep %d in-situ render time is %0.5f seconds\n", tstep, render_time);
    }
  }

  static int count = 0; 
  if((tstep % gAnalysisFrequency[1]) == 0) {
    MPI_Barrier(*comm);
    double reduction_time = -1 * MPI_Wtime();
    
    int sample_step = 2 << (count % 3);
    count++;
    
    s3d_box_insitu_reduction_tstep(myid, comm, tstep, sample_step);

    MPI_Barrier(*comm);
    reduction_time += MPI_Wtime();
    if (*myid == 0) {
        fprintf(stderr, "EVAL: tstep %d in-situ reduction (sample step %d) time is %0.5f seconds\n", tstep, sample_step, reduction_time);
    }
  }

  if((tstep % gAnalysisFrequency[0]) == 0) {
    MPI_Barrier(*comm);
    double topo_time = -1 * MPI_Wtime();

    topology_tstep(myid, tstep, comm); 

    MPI_Barrier(*comm);
    topo_time += MPI_Wtime();
    if (*myid == 0) {
        fprintf(stderr, "EVAL: tstep %d in-situ topology time is %0.5f seconds\n", tstep, topo_time);
     }
  }

  if((tstep % gAnalysisFrequency[2]) == 0) {
    MPI_Barrier(*comm);
    double stat_time = -1 * MPI_Wtime();

    descriptiveStats_tstep_entirely_insitu(myid, tstep);

    MPI_Barrier(*comm);
    stat_time += MPI_Wtime();
    if (*myid == 0) {
        fprintf(stderr, "EVAL: tstep %d entirely in-situ statistics time is %0.5f seconds\n", tstep, stat_time);
    }
  }
  if((tstep % gAnalysisFrequency[2]) == 0) {
    MPI_Barrier(*comm);
    double stat_time = -1 * MPI_Wtime();

    descriptiveStats_tstep_partial_insitu(myid, tstep);

    MPI_Barrier(*comm);
    stat_time += MPI_Wtime();
    if (*myid == 0) {
        fprintf(stderr, "EVAL: tstep %d partial in-situ statistics time is %0.5f seconds\n", tstep, stat_time);
    }
  }
*/
  //if((tstep % gAnalysisFrequency[3]) == 0) autocorrelation_tstep(myid, tstep); 
}

void finalize_insitu(int *myid, MPI_Comm *comm) {
  //printf("finalizing insitu computations\n");
  //s3d_box_viz_finalize(myid, comm);
  //topology_finalize(myid);
  //descriptiveStats_finalize_entirely_insitu(myid);
  //descriptiveStats_finalize_partial_insitu(myid);
  //autocorrelation_finalize();
}


bool insitu_timestep(int tstep) {
  for(int i=0; i < gNumAnalyses; i++) {
    if((tstep % gAnalysisFrequency[i]) == 0) return true;
  }
  return false;
}
