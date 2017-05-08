MESSAGE("Using Linux PGI Fortran")
SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

SET(CMAKE_Fortran_MODDIR_FLAG "-module ")
SET(CMAKE_Fortran_FLAGS "-Mpreprocess -r8 -byteswapio")
SET(CMAKE_Fortran_FLAGS_DEBUG " -g -O0 -Mbounds -byteswapio")
SET(CMAKE_Fortran_FLAGS_RELEASE "-Mfpmisalign -fastsse -Mipa=fast ")
SET(CMAKE_Fortran_FLAGS_PROF "-Mprof=hwcts -Mprof=func -Mfpmisalign -fastsse -Mipa=fast  ")

set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/usr/lib64/MPICH/p4/pgi-7.1-2; /opt/mpich-pgi/lib")
set(MPI_INCL_DIRS " /opt/mpich-pgi/include")

