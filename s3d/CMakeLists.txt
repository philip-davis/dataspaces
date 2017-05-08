CMAKE_MINIMUM_REQUIRED(VERSION 2.6)


# Basic setup
SET(CMAKE_Fortran_COMPILER_INIT ftn pgf90 mpif90)
PROJECT( s3d Fortran )

#SET( CMAKE_BASE_NAME "cray" )


SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake_Modules/")
SET(FIND_LIBRARY_USE_LIB64_PATHS TRUE)
SET(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "../s3d_run" CACHE PATH "Location to install run tree" FORCE)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran.cmake )
# On jaguar, brute force the compile command and flags
exec_program(hostname
  OUTPUT_VARIABLE HOST
  )
if( ${HOST} MATCHES "^jaguar")

if( ${HOST} MATCHES "^jaguarpf")
INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran-jaguarpf.cmake )
else( ${HOST} MATCHES "^jaguarpf")
INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran-jaguar.cmake )
endif( ${HOST} MATCHES "^jaguarpf")

else( ${HOST} MATCHES "^jaguar")
INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran.cmake )
endif( ${HOST} MATCHES "^jaguar")

if( ${HOST} MATCHES "^yona")
INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran-yona.cmake )
endif( ${HOST} MATCHES "^yona")

if( ${HOST} MATCHES "^hopper")
INCLUDE(${CMAKE_SOURCE_DIR}/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_Fortran_COMPILER_ID}-Fortran-hopper.cmake )
endif( ${HOST} MATCHES "^hopper")

# CDash setup
ENABLE_TESTING()
INCLUDE( Dart )

# Build options
SET(CMAKE_VERBOSE_MAKEFILE TRUE)

# Chemistry and transport models
SET( MECHANISM "C2H4" CACHE STRING "Chemical mechanism name" )
SET( TRANSPORT_MODEL "mixavg" CACHE STRING "Options: mixavg, lewis, constant" )
IF (${TRANSPORT_MODEL} MATCHES "mixavg" )
    SET(MIXAVG_TRANSPORT ON )
ELSE (${TRANSPORT_MODEL} MATCHES "mixavg" )
    SET(MIXAVG_TRANSPORT OFF )
ENDIF (${TRANSPORT_MODEL} MATCHES "mixavg" )
SET( GAMMAPRESS OFF CACHE BOOL "Override IGL EOS with constant gamma" )

# Time integration method
OPTION( ARK "Use adaptive RK" )

# Optional build components
OPTION( FILTER_FEEDDATA "Include filtering routines for feeddata" )

# I/O options
OPTION( BUNSENPOST "Use file name format for bunsen, jet cases (a9)" )
SET( SAVEFILEINSEPDIR ON CACHE BOOL "Save files in separate directory" )
SET( BLOCKIO ON CACHE BOOL "Use blocking with native posix I/O" )
OPTION( MPIIO "Attempt to use collective MPI-IO routines from Wei-keng Liao" )
OPTION( ADIOS "Build support for ADIOS" )

# External libraries 
OPTION( USE_ACML "Use AMD Core Math Library if available" )

# Architecture specific
OPTION( ARCH_SGI "Define SGI" )
OPTION( ARCH_PC "Define PC" )
OPTION( ARCH_X1 "Define X1" )
OPTION( ARCH_SP2 "Define SP2" )
OPTION( ARCH_T3E "Define T3E" )
SET( ARCH_OPTERON ON CACHE BOOL "Define CPQ" )

# Post processing options
OPTION( POST_RANS_BUNSEN "Support for rans analysis of bunsen flame " )
OPTION( POST_LES_FILTER "Support for volume averaging (needs POST_HDF5)" )
OPTION( POST_HDF5 "Support for hdf5 output " )
OPTION( POST_KLM "Support for klm output " )
OPTION( POST_SURFACE "Support for surface analysis " )
OPTION( POST_TAU "Support for tau_1stpass, etc." )
OPTION( PQR_CLASSIFICATION "Support pqr based flow topology classification" )

# Optional library build
OPTION( BUILD_LIBS3D "Build libs3d.a" )



# Resulting optimization flags:
MESSAGE("Base Fortran flags: " ${CMAKE_Fortran_FLAGS})
MESSAGE("Debug fortran flags: " ${CMAKE_Fortran_FLAGS_DEBUG})
MESSAGE("Release Fortran flags: " ${CMAKE_Fortran_FLAGS_RELEASE})




# System capabilities
# -------------------------------------------------------------
# Try to find AMD core math library
if( USE_ACML )
find_library( ACML_LIB
  NAMES libacml.a
  PATHS /usr/lib /opt/lib /opt/acml4.1.0/pgi64_int64/lib
  )

find_library( ACML_MV_LIB
  NAMES libacml_mv.a
  PATHS /usr/lib /opt/lib /opt/acml4.1.0/pgi64_int64/lib
)


SET( ACML_LIB ${ACML_LIB} ${ACML_MV_LIB} )
IF( ACML_LIB )
  MESSAGE( "Found acml library at " ${ACML_LIB} )
ELSE( ACML_LIB )
IF( ${HOST} MATCHES "^jaguar")
  SET( ACML_LIB "-lacml_mv" )
  MESSAGE( "Using -lacml_mv because of building on jaguar" )
ELSEIF( ${HOST} MATCHES "^jaguar")
  MESSAGE( "Could not find acml library" )
ENDIF( ${HOST} MATCHES "^jaguar")
ENDIF( ACML_LIB )

endif( USE_ACML )



IF( ${HOST} MATCHES "^jaguarpf")
# Try to find FAST library
find_library( FAST_MV_LIB
  NAMES lfast_mv.a
  PATHS /opt/cray/libfast/1.0/lib 
)

SET( FAST_MV_LIB ${FAST_MV_LIB} )
IF( FAST_MV_LIB )
  MESSAGE( "Found fast library at " ${FAST_MV_LIB} )
ELSE( FAST_MV_LIB )
  SET( FAST_MV_LIB "-lfast_mv" )
  MESSAGE( "Using -lfast_mv because of building on jaguarpf" )
ENDIF( FAST_MV_LIB )
ENDIF( ${HOST} MATCHES "^jaguarpf")

# See if we can call system calls directly
file (WRITE ${CMAKE_BINARY_DIR}/CMakeTmp/sysCallTestCode.f90
  "program testCode
  #####ifdef SYSTEMCALLWONTWORK
  ! when system calls do not work, as on infiniband
  write(*,*) 'Assume code here works...'
  #else
  ! when system calls work ok
  call execute_command( 'mkdir testdir2' )
  #endif
  write(*,*) "Hello world"
  end "
  )

try_run ( CMAKE_SYSTEMCALL_TEST_RUN_RESULT
  CMAKE_SYSTEMCALL_TEST_COMPILES
  ${CMAKE_BINARY_DIR}/CMakeTmp
  ${CMAKE_BINARY_DIR}/CMakeTmp/sysCallTestCode.f90
  OUTPUT_VARIABLE OUTPUT
  )

if( CMAKE_SYSTEMCALL_TEST_COMPILES )
  if( NOT CMAKE_SYSTEMCALL_TEST_RUN_RESULT )
    MESSAGE("System call works")
  else ( NOT CMAKE_SYSTEMCALL_TEST_RUN_RESULT )
    MESSAGE (" System call test compiled but did not run, using c workaround" )
    SET( SYSTEMCALLWORKS FALSE )
  endif( NOT CMAKE_SYSTEMCALL_TEST_RUN_RESULT )
else (CMAKE_SYSTEMCALL_TEST_COMPILES )
  MESSAGE( " System call test did not compile, using c workaround" )
  SET( SYSTEMCALLWORKS FALSE )
endif ( CMAKE_SYSTEMCALL_TEST_COMPILES)

IF( ACML_LIB AND USE_ACML )
  SET( CHEM_PATH "${MECHANISM}/vec/" )
  ADD_DEFINITIONS(-DUSE_ACML_VEC_CHEM) 
ELSE( ACML_LIB AND USE_ACML )
  SET( CHEM_PATH "${MECHANISM}/std/" )
ENDIF( ACML_LIB AND USE_ACML )

SET( MECH_NEEDS_DIFFUSION "C7H16_58species C7H16_52species C7H16_77species CH3OCH3_30species CH3OCH3_31species" )
if( ${MECH_NEEDS_DIFFUSION} MATCHES ${MECHANISM} )
    MESSAGE( "Matched check on needing diffusion")
    SET( GETRATES_NEEDS_DIFFUSION TRUE)
else( ${MECH_NEEDS_DIFFUSION} MATCHES ${MECHANISM} )
    MESSAGE( "Did not match check on needing diffusion")
    SET( GETRATES_NEEDS_DIFFUSION FALSE)
endif( ${MECH_NEEDS_DIFFUSION} MATCHES ${MECHANISM} )

SET( MECH_NEEDS_OWN_TRANLIB "C7H16_58species C7H16_52species C7H16_77species CH3OCH3_30species CH3OCH3_31species" )
if( ${MECH_NEEDS_OWN_TRANLIB} MATCHES ${MECHANISM} )
    MESSAGE( "Matched check on needing mechanism specific tranlib" )
    SET( MECH_SPECIFIC_TRANLIB TRUE )
    SET( TRAN_FILE "${CHEM_PATH}/tranlib.f")
    # Use this one if the first doesn't exist
    SET( TRAN_FILE_2 "${CHEM_PATH}/../std/tranlib.f")
else( ${MECH_NEEDS_OWN_TRANLIB} MATCHES ${MECHANISM} )
    MESSAGE( "Did no match check on needing mechanism specific tranlib" )
    SET( MECH_SPECIFIC_TRANLIB FALSE )
    SET( TRAN_FILE "tranlib.f")
endif( ${MECH_NEEDS_OWN_TRANLIB} MATCHES ${MECHANISM} )


# Look for awk
FIND_PROGRAM(AWK 
  NAMES gawk awk nawk
)

# Get svn version information if possible
exec_program(
  "svn info ${PROJECT_SOURCE_DIR} |grep -i Revision"
  OUTPUT_VARIABLE SVNREV
  )
exec_program(
  "svn info ${PROJECT_SOURCE_DIR} |grep -i URL"
  OUTPUT_VARIABLE SVNURL
  )

configure_file (
    ${PROJECT_SOURCE_DIR}/source/include/globalDefines.h.in
    ${PROJECT_BINARY_DIR}/globalDefines.h
    )
exec_program(
 "${PROJECT_SOURCE_DIR}/fixcomments ${PROJECT_BINARY_DIR}/globalDefines.h"
 OUTPUT_VARIABLE TMP
)

# Write a file to record the configuration information
file (WRITE ${CMAKE_BINARY_DIR}/build_options.f90
  "#include \"globalDefines.h\"
subroutine writeopts()
write(*,*)'=============================================================='
write(*,*) '--------------------S3D BUILD CONFIGURATION-----------------'
write(*,*) 'Build directory:'
write(*,*) ' ${CMAKE_BINARY_DIR} '
write(*,*) '------------------------------------------------------'
write(*,*) ' S3D Options: '
write(*,*) 'Using transport: ${TRANSPORT_MODEL}'
write(*,*) 'Using adaptive RK integration: ${ARK}'
write(*,*) 'Using chemical mechanism: ${MECHANISM}'
write(*,*) 'Using srcs for timescale analysis: ${POST_TAU}'
write(*,*) '------------------------------------------------------'
write(*,*) ' Defines:'
#ifdef SYSTEMCALLWONTWORK
write(*,*) 'SYSTEMCALLWONTWORK'
#endif
#ifdef USE_ACML_VEC_CHEM
write(*,*) 'USE_ACML_VEC_CHEM'
#endif
#ifdef SAVEFILEINSEPDIR
write(*,*) 'SAVEFILEINSEPDIR'
#endif
#ifdef SGI
write(*,*) 'SGI'
#endif
#ifdef ARCH_SGI
write(*,*) 'ARCH_SGI'
#endif
#ifdef PC
write(*,*) 'PC'
#endif
#ifdef ARCH_PC
write(*,*) 'ARCH_PC'
#endif
#ifdef X1
write(*,*) 'X1'
#endif
#ifdef ARCH_X1
write(*,*) 'ARCH_X1'
#endif
#ifdef SP2
write(*,*) 'SP2'
#endif
#ifdef ARCH_SP2
write(*,*) 'ARCH_SP2'
#endif
#ifdef T3E
write(*,*) 'T3E'
#endif

#ifdef ARCH_T3E
write(*,*) 'ARCH_T3E'
#endif
#ifdef CPQ
write(*,*) 'CPQ'
#endif
#ifdef ARCH_OPTERON
write(*,*) 'ARCH_OPTERON'
#endif
#ifdef MPIIO
write(*,*) 'MPIIO'
#endif
#ifdef _BUILD_PNETCDF_F
write(*,*) '_BUILD_PNETCDF_F'
#endif
#ifdef _BUILD_HDF5_F
write(*,*) '_BUILD_HDF5_F'
#endif
#ifdef HDF5
write(*,*) 'HDF5'
#endif
#ifdef BLOCKIO
write(*,*) 'BLOCKIO'
#endif
#ifdef NETCDF
write(*,*) 'NETCDF'
#endif
#ifdef BUILD_TAU_POST_SOURCES
write(*,*) 'BUILD_TAU_POST_SOURCES'
#endif
#ifdef BUNSENPOST
write(*,*) 'BUNSENPOST'
#endif
#ifdef MIXAVG
write(*,*) 'MIXAVG'
#endif
#ifdef LEWIS
write(*,*) 'LEWIS'
#endif
#ifdef MIXAVG
#ifdef LEWIS
write(*,*) 'Something has gone very wrong - both MIXAVG and LEWIS'
write(*,*) 'Aborting....'
STOP
#endif
#endif
#ifdef POST_TAU
write(*,*) 'POST_TAU'
#endif
#ifdef POST_SURFACE
write(*,*) 'POST_SURFACE'
#endif
#ifdef POST_KLM
write(*,*) 'POST_KLM'
#endif
#ifdef POST_HDF5
write(*,*) 'POST_HDF5'
#endif
#ifdef POST_LES_FILTER
write(*,*) 'POST_LES_FILTER'
#endif
#ifdef POST_RANS_BUNSEN
write(*,*) 'POST_RANS_BUNSEN'
#endif
#ifdef PQR_CLASSIFICATION
write(*,*) 'PQR_CLASSIFICATION'
#endif
write(*,*) '------------------------------------------------------'
write(*,*) ' FORTRAN COMPILER FLAGS: '
write(*,*) ' ${CMAKE_Fortran_FLAGS} '
write(*,*) ' C COMPILER FLAGS: '
write(*,*) ' ${CMAKE_C_FLAGS} '
write(*,*) '------------------------------------------------------'
write(*,*) ' S3D Version: '
write(*,*) ' ${SVNREV} ' 
write(*,*) ' ${SVNURL} ' 
write(*,*) '------------------------------------------------------'
write(*,*) ' Chemistry routine header: '
call write_getrates_head_auto ( )
write(*,*) '------------------------------------------------------'
write(*,*) ' Transport routine header: '
call write_tranlib_head_auto ( )
write(*,*) '------------------------------------------------------'
write(*,*)'=============================================================='
end subroutine writeopts"
  )

FILE( GLOB inputfiles "${CMAKE_CURRENT_SOURCE_DIR}/input/*.in*" "${CMAKE_CURRENT_SOURCE_DIR}/input/*.asc*")

SET( inputfiles ${inputfiles} "${CMAKE_CURRENT_SOURCE_DIR}/input/makeinputfiles" )
SET( inputfiles ${inputfiles} "${CMAKE_CURRENT_SOURCE_DIR}/input/inputfiles" )

FILE( GLOB chem_inputfiles
  "${CMAKE_CURRENT_SOURCE_DIR}/input/chemistry_dependent/${MECHANISM}/*.asc"
  "${CMAKE_CURRENT_SOURCE_DIR}/input/chemistry_dependent/${MECHANISM}/*.in")


SET( ${runfiles} "${CMAKE_CURRENT_SOURCE_DIR}/run/job_XT4.sh")
SET( runfiles ${runfiles} "${CMAKE_CURRENT_SOURCE_DIR}/run/s3d_template.xml")

ADD_SUBDIRECTORY(source)
ADD_SUBDIRECTORY(tests)

# Install - copy input, run, data, post directories from source tree ( executable comes from from build tree)

INSTALL(FILES ${inputfiles} DESTINATION input)
INSTALL(FILES ${chem_inputfiles} DESTINATION input)
INSTALL(FILES ${runfiles} DESTINATION run)
INSTALL(CODE "FILE(MAKE_DIRECTORY \${ENV}\${CMAKE_INSTALL_PREFIX}/data)")
INSTALL(CODE "FILE(MAKE_DIRECTORY \${ENV}\${CMAKE_INSTALL_PREFIX}/post)")
INSTALL(CODE "FILE(MAKE_DIRECTORY \${ENV}\${CMAKE_INSTALL_PREFIX}/post/tecplot)")

