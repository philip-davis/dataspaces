#ifndef _INSITUDESCRIPTIVESTATS_H
#define _INSITUDESCRIPTIVESTATS_H

#include <MultiFab.H>
#include "mpi.h"

void descriptiveStats_init_entirely_insitu(int *myid, MPI_Comm *comm, int *xyzpes,  int *global_dims, MultiFab *insitu_vars);
void descriptiveStats_init_partial_insitu(int *myid, MPI_Comm *comm, int *xyzpes,  int *global_dims, MultiFab *insitu_vars);
void descriptiveStats_tstep_entirely_insitu( int *myid, int tstep);
void descriptiveStats_tstep_partial_insitu( int *myid, int tstep);
void descriptiveStats_finalize_entirely_insitu( int * myid );
void descriptiveStats_finalize_partial_insitu( int * myid );

#endif
