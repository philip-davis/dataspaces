#ifndef _INSITUTOPOLOGY_H
#define _INSITUTOPOLOGY_H

#include <MultiFab.H>
#include "mpi.h"

void topology_init(int *myid, MPI_Comm *comm, int *global_dim, int *xyzpes, MultiFab *insitu_vars, int num_ghost);
void topology_tstep(int *myid, int tstep, MPI_Comm *comm);
void topology_finalize(int *myid);

#endif
