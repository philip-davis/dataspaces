SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

SET(CMAKE_Fortran_MODDIR_FLAG "-module ")
SET(CMAKE_Fortran_FLAGS "-cpp -fdefault-real-8 ")
SET(CMAKE_Fortran_FLAGS_INIT "-cpp -fdefault-real-8 ")
SET(CMAKE_Fortran_FLAGS_DEBUG_INIT "-Warray-bounds -g -O0 -fbounds-check -ffpe-trap=invalid,zero,overflow,underflow,denormal")
SET(CMAKE_Fortran_FLAGS_MINSIZEREL_INIT "-O2 -s")
SET(CMAKE_Fortran_FLAGS_RELEASE_INIT "-O3 ")
SET(CMAKE_Fortran_FLAGS_RELWITHDEBINFO_INIT "-O2 -gopt")

#SET(COMPILER_OPTIMIZATION_FLAGS "-Mfpmisalign -fastsse -Mipa=fast -tp barcelona-64 -byteswapio -r8 -c -Mpreprocess ")
#SET(COMPILER_OPTIMIZATION_FFLAGS "-fastsse -O4 -Mpreprocess -Munroll -Mvect=prefetch -byteswapio -tp k8-64 -Mpreprocess ")

set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/opt/mpich2-1.2.1/lib")
set(MPI_INCL_DIRS "/opt/mpich2-1.2.1/include")

