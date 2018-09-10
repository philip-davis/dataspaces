%module example

%{
#include <mpi.h>
#include "example.c"
%}

%include mpi4py/mpi4py.i

%mpi4py_typemap(Comm, MPI_Comm);

void sayhello(MPI_Comm comm);

/*
 * Local Variables:
 * mode: C
 * End:
 */