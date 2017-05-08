  MESSAGE("jaguar is host; using custom compiler comand and flags")
SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

SET(CMAKE_Fortran_MODDIR_FLAG "-module ")
SET(CMAKE_Fortran_FLAGS_INIT " ")
SET(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -Mbounds -fpic")
SET(CMAKE_Fortran_FLAGS_RELEASE "")
message("using linux cmake")
SET(COMPILER_OPTIMIZATION_FFLAGS "-Mfpmisalign -fastsse -Mipa=fast -byteswapio -r8 -Mpreprocess ")
#SET(COMPILER_OPTIMIZATION_FFLAGS "-fastsse -O4 -Mpreprocess -Munroll -Mvect=prefetch -byteswapio -tp k8-64 -Mpreprocess ")

set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/usr/lib64/MPICH/p4/pgi-7.1-2; /opt/mpich-pgi/lib")
set(MPI_INCL_DIRS " /opt/mpich-pgi/include")

SET( CMAKE_Fortran_COMPILER "ftn" )
SET( CMAKE_Fortran_FLAGS_RELEASE "-Mfpmisalign -fastsse -Mipa=fast -tp barcelona-64 -byteswapio -r8 -Mpreprocess ")


SET( DEF_CPQ OFF CACHE BOOL "Define CPQ")
