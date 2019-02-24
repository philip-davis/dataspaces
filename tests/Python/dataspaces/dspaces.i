%module dspaces

%{
#define SWIG_FILE_WITH_INIT
#define SWIG_PYTHON_STRICT_BYTE_CHAR
#include "mpi.h"
#include "dspaces.c"
%}


%include mpi4py/mpi4py.i
%include "numpy.i"

%init %{
    import_array();
%}


%mpi4py_typemap(Comm, MPI_Comm);

%apply (double* IN_ARRAY1, int DIM1) {(double* data, int n)};

%apply (unsigned long long  * IN_ARRAY1, int DIM1) {(unsigned long long * lb, int n1)}; 

%apply (unsigned long long * IN_ARRAY1, int DIM1) {(unsigned long long * ub, int n2)}; 

void wrapper_dspaces_init(MPI_Comm pgcomm,int num_peers, int appid);

void wrapper_dspaces_lock_on_write(MPI_Comm pgcomm, char *varname);

void wrapper_dspaces_unlock_on_write(MPI_Comm pgcomm, char *varname);

void wrapper_dspaces_lock_on_read(MPI_Comm pgcomm, char *varname);

void wrapper_dspaces_unlock_on_read(MPI_Comm pgcomm, char *varname);

void wrapper_finalize();

int wrapper_put_data(const char *var_name,
                     unsigned int ver, int size,
                     int ndim, unsigned long long *lb,int n1, unsigned long long*ub, int n2, double *data, int n);

int wrapper_get_data(const char *var_name,
                     unsigned int ver, int size,
                     int ndim, unsigned long long *lb,int n1, unsigned long long*ub, int n2, double *data, int n);