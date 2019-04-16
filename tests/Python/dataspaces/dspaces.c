/* put.c : Example 1: DataSpaces put tutorial 
 * This example will show you the simplest way 
 * to put a 1D array of 3 elements into the DataSpace.
 * */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "dataspaces.h"
#include "mpi.h"

void wrapper_dspaces_init(MPI_Comm pgcomm, int num_peers, int appid)
{
    int nprocs;
    int rank;
    MPI_Comm_size(pgcomm, &nprocs);
    MPI_Comm_rank(pgcomm, &rank);
    MPI_Comm gcomm = pgcomm;
    MPI_Barrier(gcomm);

    printf("num_peers %d appid %d\n", num_peers, appid);

    // Initalize DataSpaces
    // # of Peers, Application ID, ptr MPI comm, additional parameters
    // # Peers: Number of connecting clients to the DS server
    // Application ID: Unique idenitifier (integer) for application
    // Pointer to the MPI Communicator, allows DS Layer to use MPI barrier func
    // Addt'l parameters: Placeholder for future arguments, currently NULL.
    // the first parameter should be same with the number of threads to access server (namely -n after mpiexec)
    // the second parameter is used to label current application, one application/program should have unique appid
    // init is only needed to call once for multiple timesteps
    dspaces_init(num_peers, appid, &gcomm, NULL);

    return;
}

int wrapper_put_data(const char *var_name,
                     unsigned int timestep, int size,
                     int ndim, unsigned long long *lb, int n1, unsigned long long *ub, int n2, double *data, int n)
{

    //printf("Timestep %d: put data %lf %lf %lf\n", timestep, data[0], data[1], data[2]);
    //TODO if debug
    /*
    int i=0;
    for (i=0;i<ndim;i++){
        printf("index lb %d is %d\n", i, lb[i]);
    }

    for (i=0;i<ndim;i++){
        printf("index ub %d is %d\n", i, ub[i]);
    }
    */
    
    
    return dspaces_put(var_name, timestep, size, ndim, (uint64_t *)lb, (uint64_t *)ub, data);
}

int wrapper_get_data(const char *var_name,
                     unsigned int timestep, int size,
                     int ndim, unsigned long long *lb, int n1, unsigned long long *ub, int n2, double *data, int n)
{

    //TODO if debug
    /*
    int i=0;
    for (i=0;i<ndim;i++){
        printf("index lb %d is %d\n", i, lb[i]);
    }

    for (i=0;i<ndim;i++){
        printf("index ub %d is %d\n", i, ub[i]);
    }
    */

    int rcode = dspaces_get(var_name, timestep, size, ndim, (uint64_t *)lb, (uint64_t *)ub, data);
    return rcode;
}

void wrapper_dspaces_lock_on_write(MPI_Comm pgcomm, char *varname)
{
    //printf("get varname %s\n", varname);
    //MPI_Comm gcomm = MPI_COMM_WORLD;
    dspaces_lock_on_write(varname, &pgcomm);
    return;
}

void wrapper_dspaces_unlock_on_write(MPI_Comm pgcomm, char *varname)
{
    //MPI_Comm gcomm = MPI_COMM_WORLD;
    dspaces_unlock_on_write(varname, &pgcomm);
    return;
}

void wrapper_dspaces_lock_on_read(MPI_Comm pgcomm, char *varname)
{
    //printf("get varname %s\n", varname);
    //MPI_Comm gcomm = MPI_COMM_WORLD;
    dspaces_lock_on_read(varname, &pgcomm);
    return;
}

void wrapper_dspaces_unlock_on_read(MPI_Comm pgcomm, char *varname)
{
    dspaces_unlock_on_read(varname, &pgcomm);
    return;
}

void wrapper_finalize()
{
    dspaces_finalize();
    return;
}
