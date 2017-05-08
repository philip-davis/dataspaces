# Configuration for Intel fortran 
# Intended for Red Mesa but may be useful elsewhere
# Updated by RG 2010

# Use mpif90 -  expands to ifort + flags and libs
SET( CMAKE_Fortran_COMPILER "mpif90" )

# This is used if CMAKE_BUILD_TYPE isn't set
SET( CMAKE_Fortran_FLAGS "-convert big_endian -r8 -fpp -fp-model strict ")
#SET( CMAKE_Fortran_FLAGS "-convert big_endian -r8 -fpp -fp-model strict ")

# These are addded if CMAKE_BUILD_TYPE is set to Release, Init, Debug (*after* CMAKE_Fortran_FLAGS)
SET(CMAKE_Fortran_FLAGS_INIT " ")

# -check all includes -check bounds
SET(CMAKE_Fortran_FLAGS_DEBUG "-debug all -debug-parameters  -traceback -O0 -check bounds -check format -check output_conversion -check pointers -check uninit")

# Other options for ifort -m: -mssse3, -msse3, -msse2, -msse, -mia32
# -fma is use fused multiply-add (disabled by default with -fp-model strict)
SET(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xT -prefetch  -ip ")

# Other stuff
SET(CMAKE_Fortran_MODDIR_FLAG "-module ")

# Linking stuff
SET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
set(MPI_LIB_DIRS "/usr/lib64/MPICH/p4/pgi-7.1-2; /opt/mpich-pgi/lib")
set(MPI_INCL_DIRS " /opt/mpich-pgi/include")
SET( CMAKE_EXE_LINKER_FLAGS "-target=linux -fastsse -Mipa=fast,nocp -tp istanbul-64 -byteswapio -r8 -v -Wl,-zmuldefs")
set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")


# Need to make sure this is defined on x86
SET( DEF_CPQ ON CACHE BOOL "Define CPQ")

