/*
*  Fan Zhang (2013)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/
#ifndef __DS_HYBRID_STAGING_H__
#define __DS_HYBRID_STAGING_H__

#include "mpi.h"

enum core_type {
	hs_manager_core = 1,
	hs_worker_core
};

enum worker_type {
	hs_simulation_worker = 1, 
	hs_staging_worker
};

enum location_type {
	hs_insitu = 1,
	hs_intransit
};

enum execution_mode {
	hs_staging_mode = 1,
	hs_hybrid_staging_mode
};

int hs_comm_init(int, char **);
int hs_comm_fin();

// TODO: are these functions useful at all?
// int hs_comm_get_rank();
// int hs_comm_get_nprocs();

int hs_comm_perform_split(enum execution_mode mode, enum core_type coretype, enum location_type loc, enum worker_type* workertype);

MPI_Comm hs_get_communicator_l1(); 
MPI_Comm hs_get_communicator_l2(); 

#endif
