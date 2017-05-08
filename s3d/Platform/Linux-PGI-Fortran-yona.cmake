# For PGI Fortran
# Intended for yona, may be useful elsewhere 
# Target arch is istanbul
# Updated by RG 2010

# This file is loaded by S3D CMakeLists.txt if machinename matches yona
MESSAGE("yona is host; using custom compiler comand and flags for pgi fortran compiler")

# On jaguar always use ftn - expands to pgf90 + flags and libs
SET( CMAKE_Fortran_COMPILER "mpif90" )

# This is used if CMAKE_BUILD_TYPE isn't set
SET( CMAKE_Fortran_FLAGS " -r8 -Mpreprocess -Mfpmisalign -fastsse -Mipa=fast,nocp -tp istanbul-64")

# These are addded if CMAKE_BUILD_TYPE is set to Release, Init, Debug (*after* CMAKE_Fortran_FLAGS)
SET(CMAKE_Fortran_MODDIR_FLAG "-module ")
SET(CMAKE_Fortran_FLAGS_INIT "-Kieee ")
SET(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -Mbounds")
SET(CMAKE_Fortran_FLAGS_RELEASE "-Mfpmisalign -fastsse -Mipa=fast,nocp -tp istanbul-64")

SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

# Linking stuff
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/sw/yona/ompi/1.4.2/centos5.5_pgi10.6/lib")
set(MPI_INCL_DIRS "-I/sw/yona/ompi/1.4.2/centos5.5_pgi10.6/include")
#SET( CMAKE_EXE_LINKER_FLAGS "-target=linux -fastsse -Mipa=fast,nocp -tp istanbul-64 -byteswapio -r8 -v -Wl,-zmuldefs")
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")


# Need to make sure this is defined on x86
SET( DEF_CPQ ON CACHE BOOL "Define CPQ")

