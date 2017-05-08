# For PGI Fortran
# Intended for hopper, may be useful elsewhere 
# Target arch is istanbul
# Updated by RG 2010

# This file is loaded by S3D CMakeLists.txt if machinename matches hopper
MESSAGE("hopper is host; using custom compiler comand and flags for pgi fortran compiler")

# On jaguar always use ftn - expands to pgf90 + flags and libs
SET( CMAKE_Fortran_COMPILER "ftn" )

# This is used if CMAKE_BUILD_TYPE isn't set
SET( CMAKE_Fortran_FLAGS " -byteswapio -r8 -Mpreprocess ")

# These are addded if CMAKE_BUILD_TYPE is set to Release, Init, Debug (*after* CMAKE_Fortran_FLAGS)
SET(CMAKE_Fortran_MODDIR_FLAG "-module ")
SET(CMAKE_Fortran_FLAGS_INIT "-Kieee ")
SET(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -Mbounds")
SET(CMAKE_Fortran_FLAGS_RELEASE " -fastsse -Mipa=fast,nocp ")

SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

# RG I don't think this is actually used anywhere...
#SET(COMPILER_OPTIMIZATION_FFLAGS "-Mfpmisalign -fastsse -Mipa=fast,nocp -byteswapio -r8 -Mpreprocess ")
#SET(COMPILER_OPTIMIZATION_FFLAGS "-fastsse -O4 -Mpreprocess -Munroll -Mvect=prefetch -byteswapio -tp k8-64 -Mpreprocess ")

# Linking stuff
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/usr/lib64/MPICH/p4/pgi-7.1-2; /opt/mpich-pgi/lib")
set(MPI_INCL_DIRS " /opt/mpich-pgi/include")
SET( CMAKE_EXE_LINKER_FLAGS "-target=linux -fastsse -Mipa=fast,nocp  -byteswapio -r8 -v -Wl,-zmuldefs")
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")


# Need to make sure this is defined on x86
SET( DEF_CPQ ON CACHE BOOL "Define CPQ")

