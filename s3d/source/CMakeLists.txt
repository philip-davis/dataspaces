set (PROJECT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)
set (PROJECT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

set (MAIN ${PROJECT_SOURCE_DIR}/drivers PARENT_SCOPE)
set (INIT ${PROJECT_SOURCE_DIR}/f90_files/init PARENT_SCOPE)
set (SOLVE ${PROJECT_SOURCE_DIR}/f90_files/solve PARENT_SCOPE)
set (BC ${PROJECT_SOURCE_DIR}/f90_files/solve/bc_stuff PARENT_SCOPE)
set (POST ${PROJECT_SOURCE_DIR}/f90_files/post PARENT_SCOPE)
set (FSEVEN ${PROJECT_SOURCE_DIR}/f77_files PARENT_SCOPE)
set (OTHER ${PROJECT_SOURCE_DIR}/other_files PARENT_SCOPE)
set (MODULES ${PROJECT_SOURCE_DIR}/modules PARENT_SCOPE)
set (JOHNMC ${PROJECT_SOURCE_DIR}/f77_files/johnmc PARENT_SCOPE)


set (MAIN ${PROJECT_SOURCE_DIR}/drivers)
set (INIT ${PROJECT_SOURCE_DIR}/f90_files/init)
set (SOLVE ${PROJECT_SOURCE_DIR}/f90_files/solve)
set (BC ${PROJECT_SOURCE_DIR}/f90_files/solve/bc_stuff)
set (POST ${PROJECT_SOURCE_DIR}/f90_files/post)
set (FSEVEN ${PROJECT_SOURCE_DIR}/f77_files)
set (OTHER ${PROJECT_SOURCE_DIR}/other_files)
set (MODULES ${PROJECT_SOURCE_DIR}/modules)
set (JOHNMC ${PROJECT_SOURCE_DIR}/f77_files/johnmc)


include_directories( ${MODULES} )
include_directories( ${PROJECT_SOURCE_DIR}/include )
include_directories( ${PROJECT_BINARY_DIR}/)

SET( BC_FILES
  ${BC}/bc_flux.f90                 
  ${BC}/compute_d.f90               
  ${BC}/compute_L.f90               
  ${BC}/impose_hard_bc.f90          
  ${BC}/inflowA.f90                 
  ${BC}/nscbc.f90                   
  ${BC}/point_der1.f90
  ${BC}/update_L.f90
  )

SET( INIT_FILES
  ${INIT}/init_1d_flame_test.f90     
  ${INIT}/init_compheat_test.f90     
  ${INIT}/init_ignition_test.f90     
  ${INIT}/init_ignition_H2_test.f90     
  ${INIT}/init_bomb.f90     
  ${INIT}/init_opthinrad_test.f90    
  ${INIT}/init_pressure_wave_test.f90
  ${INIT}/init_turbulent_air_test.f90
  ${INIT}/init_lifted.f90
  ${INIT}/init_lifted_feed.f90
  ${INIT}/init_tj.f90
  ${INIT}/init_bunsen.f90
  ${INIT}/init_bunsen_feed.f90
  ${INIT}/init_strat.f90
  ${INIT}/init_strat_feed.f90
  ${INIT}/vortex.f90
  ${INIT}/init_burgers.f90
  ${INIT}/initialize_hcci.f90
  ${INIT}/rndini2d_mix.f90
  ${INIT}/rndini2d_temp.f90
  ${INIT}/rndini3d_mix.f90
  ${INIT}/rndini3d_temp.f90
  ${INIT}/rin2dy_mix.f90
  ${INIT}/ran2.f90
  )



SET( POST_FILES
#  ${POST}/calc_heat_release.f90      
#  ${POST}/calc_vorticity.f90         
#  ${POST}/write_tecplot_skip.f90     
#  ${POST}/write_temporal_data.f90     
#  ${POST}/write_basic_tecplot_file.f90  
#  ${POST}/stat_util_m.f90
#  ${POST}/write_output_data.f90
#  ${POST}/compute_chi.f90
#  ${POST}/calc_max_temp_grad.f90
#  ${POST}/calc_avg_temp_grad.f90
#  ${POST}/write_tecplot_xyplane.f90
#  ${POST}/write_tecplot_yzplane.f90
#  ${POST}/write_usdn.f90
#  ${POST}/filter_feeddata.f90
#  ${POST}/lesfilt_m.f90
#  ${POST}/writebin_append.c
#  ${POST}/writebin.c
  )

IF( FILTER_FEEDDATA)
    SET( POST_FILES ${POST_FILES}
#        ${POST}/filter_feeddata.f90   
        )
ENDIF( FILTER_FEEDDATA)

IF( POST_TAU )
    SET( POST_FILES ${POST_FILES}
	${POST}/tau_1stpass.f90
	${POST}/tau_2ndpass_m.f90
	${POST}/tau_accum.f90
	${POST}/tau_finish.f90
	${POST}/tau_utils.f90
	)
ENDIF( POST_TAU )

IF( POST_RANS_BUNSEN )
    SET( POST_FILES ${POST_FILES}
	${POST}/rans_bunsen.f90
	)
ENDIF( POST_RANS_BUNSEN )

IF( POST_LES_FILTER )
    SET( POST_FILES ${POST_FILES}
	${POST}/par_filter_m.f90
	)
ENDIF( POST_LES_FILTER )

IF( POST_KLM )
    SET( POST_FILES ${POST_FILES}
	${POST}/dump_klm_file.f90
	)
ENDIF( POST_KLM )

IF( PQR_CLASSIFICATION )
FIND_LIBRARY( LAPACK_LIBRARY
	NAMES liblapack.a
	PATHS "/usr/lib64"
	ENV LIBRARY_PATH
)
FIND_LIBRARY( BLAS_LIBRARY
	NAMES libblas.a
	PATHS "/usr/lib64"
	ENV LIBRARY_PATH
)
IF( LAPACK_LIBRARY AND BLAS_LIBRARY )
    SET(LAPACK_FOUND  TRUE)
    SET( POST_FILES ${POST_FILES}
	${POST}/pqr_classification.f90
	)
ENDIF( LAPACK_LIBRARY AND BLAS_LIBRARY )
ENDIF( PQR_CLASSIFICATION )


SET( SOLVE_FILES 
  ${SOLVE}/calc_cfl_limit.f90
  ${SOLVE}/check_resolution.f90
  ${SOLVE}/derivative_x.f90
  ${SOLVE}/derivative_y.f90
  ${SOLVE}/derivative_z.f90
  ${SOLVE}/init_field.f90
  ${SOLVE}/io.f90
  ${SOLVE}/rhs_tools.f90
  ${SOLVE}/set_variables.f90
  ${SOLVE}/terminate_run.f90
  ${SOLVE}/deriv_inplane_1.f90
  ${SOLVE}/deriv_inplane_2.f90
  )

SET( RHS_FILES 
    ${SOLVE}/rhsf.f90
  )

SET( MODULE_FILES
 ${MODULES}/ghost_nice_m.f90
# ${POST}/premix_drvd_var.f90
 ${MODULES}/param_m.f90             
 ${MODULES}/chemkin_m.f90           
 ${MODULES}/topology_m.f90          
 ${MODULES}/work_m.f90              
 ${MODULES}/bc_m.f90                
 ${MODULES}/compheat_m.f90          
 ${MODULES}/derivative_m.f90        
 ${MODULES}/expTab_m.f90
 ${MODULES}/frozenfeed_m.f90
 ${MODULES}/temporalfeed_m.f90
 ${MODULES}/filter_m.f90            
 ${MODULES}/damping_filter_m.f90            
 ${MODULES}/grid_m.f90              
# ${MODULES}/isosurf_m.f90
 ${MODULES}/mixfrac_m.f90
 ${MODULES}/opthinrad_m.f90
 ${MODULES}/reference_m.f90
 ${MODULES}/runtime_m.f90
 ${MODULES}/thermchem_m.f90
 ${MODULES}/espectrum_m.f90
 ${MODULES}/triangulate_m.f90
 ${MODULES}/turbulence_m.f90
 ${MODULES}/clookup_m.f90
 ${MODULES}/zclookup_m.f90
 ${MODULES}/variables_m.f90
 )

SET( FSEVEN_FILES
  ${FSEVEN}/ck2f90_tools.f                 
  ${FSEVEN}/ckheadersetup.f               
  ${FSEVEN}/cklib.f                      
  ${FSEVEN}/fft_brenner_f.f             
  ${FSEVEN}/ckerr.f                 
  ${FSEVEN}/xerror.f
  ${FSEVEN}/math.f                   
  ${FSEVEN}/vkp.f
  )


IF( X1E_VECTORVERSION )
  SET( VEC_FILES
    ${MODULES}/gibbs_rxn_table_m_${MECHANISM}.f90
    ${SOLVE}/vecrates_${MECHANISM}.f90
  )
ENDIF( X1E_VECTORVERSION )


IF( MPIIO )
  SET( MPIIO_FILES 
    ${MODULES}/mpi_io_m.f90             
    ${MODULES}/pnetcdf_m.f90             
    ${MODULES}/hdf5_m.f90             
    )
ENDIF( MPIIO )



SET(GETRATES_SRC ${FSEVEN}/${CHEM_PATH}/getrates.f )
SET(TRANLIB_SRC  ${FSEVEN}/${TRAN_FILE} )

# If tranlib doesn't exist, try in ../std (in case vec ver unavailable)
IF( NOT EXISTS  ${TRANLIB_SRC} )
    SET(TRANLIB_SRC  ${FSEVEN}/${TRAN_FILE_2} )
ENDIF( NOT EXISTS  ${TRANLIB_SRC} )

# If there are other files supporting getrates, add them
SET(GETRATES_EXTRA_SRCS "" )
FOREACH(EXTRA_SRC ytcp ratt ratt1 ratt2 ratt3 ratt4 ratx qssa stif rdot)
    IF( EXISTS ${FSEVEN}/${CHEM_PATH}/${EXTRA_SRC}.f )
	SET(GETRATES_EXTRA_SRCS ${GETRATES_EXTRA_SRCS} ${FSEVEN}/${CHEM_PATH}/${EXTRA_SRC}.f )
    ENDIF( EXISTS ${FSEVEN}/${CHEM_PATH}/${EXTRA_SRC}.f )
ENDFOREACH(EXTRA_SRC)


SET(GETRATES_ALL_SRC ${GETRATES_SRC} ${GETRATES_EXTRA_SRCS} PARENT_SCOPE )

MESSAGE( "GETRATES_SRC=" ${GETRATES_SRC} )
MESSAGE( "EXTRA GETRATES SOURCES=" ${GETRATES_EXTRA_SRCS} )
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_BINARY_DIR}/build_getrates_head.f90
  COMMAND ${AWK}
  'BEGIN{i=0\; print \"subroutine write_getrates_head_auto\(\)\"} {if\(i==0\)
  print \"write\(*,*\) '"'"' \" $$0 \"'"'"'\" } tolower\($$0\) ~/subroutine/ {i=1} END{print \"end subroutine\"}' ${GETRATES_SRC} >   ${CMAKE_BINARY_DIR}/build_getrates_head.f90
  DEPENDS ${GETRATES_SRC}
  )

MESSAGE( "TRANLIB_SRC=" ${TRANLIB_SRC} )
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_BINARY_DIR}/build_tranlib_head.f90
  COMMAND ${AWK}
  'BEGIN{i=0\; print \"subroutine write_tranlib_head_auto\(\)\"} {if\(i==0\)
  print \"write\(*,*\) '"'"' \" $$0 \"'"'"'\" } tolower\($$0\) ~/subroutine/ {i=1} END{print \"end subroutine\"}' ${TRANLIB_SRC} >   ${CMAKE_BINARY_DIR}/build_tranlib_head.f90
  DEPENDS ${TRANLIB_SRC}
  )


SET( GEN_SRC
 ${CMAKE_BINARY_DIR}/build_getrates_head.f90
 ${CMAKE_BINARY_DIR}/build_tranlib_head.f90
)

SET( DRIVER_SRC
  ${MAIN}/main.f90
  ${MAIN}/solve_driver.f90
  ${MAIN}/post_driver.f90
  )

# A build option should control if we build with erk or ark and set a flag
# that will record which in the executable appropriately
IF ( ARK )
  SET( RK_SRC
    ${MODULES}/ark_m.f90              
    ${SOLVE}/integrate_ark.f90     
    ${FSEVEN}/g_getrates.f      
    ${FSEVEN}/g_cklib.f      
    ${FSEVEN}/eshfid.f 
    )
ELSE ( ARK )
SET( RK_SRC
  ${MODULES}/erk_m.f90
  )
ENDIF ( ARK )


IF (${TRANSPORT_MODEL} MATCHES "mixavg" )
  MESSAGE( "Including source files for mixture averaged transport - JOHNMC
  version " )
SET( TRAN_SRC
  ${MODULES}/mixavg_transport_m.f90 
  ${JOHNMC}/diffflux_gen_uj.f 
  ${JOHNMC}/mcavis_new_lt_gen.f 
  ${JOHNMC}/heatflux_lt_gen.f
  )
ENDIF (${TRANSPORT_MODEL} MATCHES "mixavg" )
IF (${TRANSPORT_MODEL} MATCHES "lewis" )
  MESSAGE( "Including source files for lewis transport")
SET( TRAN_SRC
  ${MODULES}/lewis_transport_m.f90
  )
ENDIF (${TRANSPORT_MODEL} MATCHES "lewis" )
IF (${TRANSPORT_MODEL} MATCHES "constant" )
  MESSAGE( "Including source files for constant transport coefficients")
SET( TRAN_SRC
  ${MODULES}/constant_transport_m.f90
  )
ENDIF (${TRANSPORT_MODEL} MATCHES "constant" )

IF( ADIOS )
  SET( SOLVE_FILES ${SOLVE_FILES}
    ${SOLVE}/add_species_to_xml.c
    )
ENDIF( ADIOS )



# Set based on result of top level system introspecton
IF( NOT SYSTEMCALLWORKS )
  MESSAGE("Configuring to work around system call not working")
  SET( SOLVE_FILES ${SOLVE_FILES}
    ${SOLVE}/mkdir.c
    )
  ENABLE_LANGUAGE(C)
  ADD_DEFINITIONS(-DSYSTEMCALLWONTWORK) 
ENDIF( NOT SYSTEMCALLWORKS )


FIND_LIBRARY( NETCDF_LIB
  NAMES libnetcdf.a
  PATHS "/lib ${ENV}${HOME}/lib ${ENV}${LIBRARY_PATH}"
  ENV LIBRARY_PATH
)

FIND_PATH( NETCDF_INCLUDE_PATH
  NAMES netcdf.mod
  PATHS "/include ${ENV}${HOME}/include" 
  ENV INCLUDE_PATH
)

IF( ADIOS )
    FIND_PACKAGE( ADIOS )
ENDIF( ADIOS )

IF( ADIOS_FOUND )
    INCLUDE_DIRECTORIES( ${ADIOS_INCLUDE_DIR} )
ENDIF( ADIOS_FOUND )

set( HDF5_PATH "")
# Extract include paths from compile command line
  string(REGEX MATCHALL "-I([^\" ]+|\"[^\"]+\")"  HDF5_PATH_WORK "$ENV{HDF5_FLIB}")
  foreach(IPATH ${HDF5_PATH_WORK})
    string(REGEX REPLACE "^-I" "" IPATH ${IPATH})
    string(REGEX REPLACE "//" "/" IPATH ${IPATH})
    list(APPEND HDF5_PATH ${IPATH})
  endforeach(IPATH)
  string(REGEX MATCHALL "-L([^\" ]+|\"[^\"]+\")"  HDF5_PATH_WORK "$ENV{HDF5_FLIB}")
  foreach(IPATH ${HDF5_PATH_WORK})
    string(REGEX REPLACE "^-L" "" IPATH ${IPATH})
    string(REGEX REPLACE "//" "/" IPATH ${IPATH})
    list(APPEND HDF5_PATH ${IPATH})
  endforeach(IPATH)

FIND_LIBRARY( HDF5_LIB
    NAMES libhdf5.a
    PATHS ${HDF5_PATH}
    ENV LIBRARY_PATH
    )
FIND_LIBRARY( HDF5_FLIB
    NAMES libhdf5_fortran.a libhdf5f90.a
    PATHS ${HDF5_PATH}
    ENV LIBRARY_PATH
    )
FIND_LIBRARY( Z_LIB
    NAMES libz.a
    PATHS ${HDF5_PATH}
    ENV LIBRARY_PATH
    )
FIND_LIBRARY( SZ_LIB
    NAMES libsz.a
    PATHS ${HDF5_PATH}
    ENV LIBRARY_PATH
    )

FIND_PATH( HDF5_INCLUDE_PATH
    NAMES hdf5.mod
    PATHS ${HDF5_PATH}
    ENV INCLUDE_PATH
    )
SET( MPIIO_PATH /ccs/home/wkliao/ROMIO )
SET( PNETCDF_PATH /ccs/home/wkliao/PnetCDF )

IF( MPIIO )
  FIND_LIBRARY( MPIIO_LIB
    NAMES libmpio.a
    PATHS ${MPIIO_PATH}/lib
    ENV LIBRARY_PATH
    )
  FIND_LIBRARY( PNETCDF_LIB
    NAMES libpnetcdf.a
    PATHS ${PNETCDF_PATH}
    ENV LIBRARY_PATH
    )
  FIND_PATH( PNETCDF_INCLUDE_PATH
    NAMES pnetcdf.inc
    PATHS ${PNETCDF_PATH}/include
    ENV INCLUDE_PATH
    )
ENDIF( MPIIO )

IF( HDF5_LIB )
  INCLUDE_DIRECTORIES( ${HDF5_INCLUDE_PATH} )
  SET( S3D_SOURCE_FILES ${S3D_SOURCE_FILES}
    ${POST}/s3d_hdf_interface_m.f90
  )
  ADD_DEFINITIONS(-DHDF5) 
ENDIF( HDF5_LIB )

IF( NETCDF_LIB AND NETCDF_INCLUDE_PATH )
  INCLUDE_DIRECTORIES( ${NETCDF_INCLUDE_PATH} )
  SET( S3D_SOURCE_FILES ${S3D_SOURCE_FILES}
    ${POST}/write_netcdf_plane.f90
  )
  ADD_DEFINITIONS(-DNETCDF) 
ENDIF( NETCDF_LIB AND NETCDF_INCLUDE_PATH )

# Include file with build options
SET( GEN_SRC ${GEN_SRC}
  ${CMAKE_BINARY_DIR}/build_options.f90
  )

SET( S3D_SOURCE_FILES 
    ${BC_FILES} 
    ${INIT_FILES} 
    ${POST_FILES}
    ${SOLVE_FILES}
    ${RHS_FILES}
    ${MODULE_FILES}
    ${FSEVEN_FILES}
    ${VEC_FILES}
    ${MPIIO_FILES}
    ${GETRATES_SRC}
    ${GETRATES_EXTRA_SRCS}
    ${TRANLIB_SRC}
    ${GEN_SRC}
    ${DRIVER_SRC}
    ${RK_SRC}
    ${TRAN_SRC}
    )

SET( LIBS3D_SRC
    ${INIT_FILES} 
    ${SOLVE_FILES}
    ${MODULE_FILES}
    ${FSEVEN_FILES}
    ${VEC_FILES}
    ${MPIIO_FILES}
    ${GETRATES_SRC}
    ${GETRATES_EXTRA_SRCS}
    ${TRANLIB_SRC}
    ${GEN_SRC}
    ${TRAN_SRC}
    ${RK_SRC}
    ${BC_FILES} 
    )

IF( BUILD_LIBS3D )
  ADD_LIBRARY(s3d 
      ${LIBS3D_SRC} )
ELSE( BUILD_LIBS3D )
  ADD_EXECUTABLE(
      s3d.x
      ${S3D_SOURCE_FILES}
      )
  IF( ACML_LIB )
  TARGET_LINK_LIBRARIES(s3d.x ${ACML_LIB})
ENDIF( ACML_LIB )

  IF( NETCDF_LIB AND NETCDF_INCLUDE_PATH )
  TARGET_LINK_LIBRARIES(s3d.x ${NETCDF_LIB})
ENDIF( NETCDF_LIB AND NETCDF_INCLUDE_PATH )

IF( ADIOS_FOUND AND ADIOS )
    TARGET_LINK_LIBRARIES( s3d.x ${ADIOS_LIBRARIES} "-pgcpplibs" )
ENDIF( ADIOS_FOUND AND ADIOS )


IF( MPIIO )

  IF( MPIIO_LIB )
  TARGET_LINK_LIBRARIES( s3d.x ${MPIIO_LIB} )
  ADD_DEFINITIONS(-DMPIIO) 
ENDIF( MPIIO_LIB )

  IF( PNETCDF_LIB )
  TARGET_LINK_LIBRARIES( s3d.x ${PNETCDF_LIB} )
  ADD_DEFINITIONS(-D_BUILD_PNETCDF_F) 
ENDIF( PNETCDF_LIB )

  IF ( HDF5_LIB )
  ADD_DEFINITIONS(-D_BUILD_HDF5_F) 
ENDIF ( HDF5_LIB )

ENDIF( MPIIO )

  IF( LAPACK_FOUND )
  TARGET_LINK_LIBRARIES( s3d.x ${LAPACK_LIBRARY})
  TARGET_LINK_LIBRARIES( s3d.x ${BLAS_LIBRARY})
ENDIF( LAPACK_FOUND )

  IF( HDF5_LIB )
  IF( HDF5_FLIB )
  TARGET_LINK_LIBRARIES( s3d.x ${HDF5_FLIB} )
  ADD_DEFINITIONS(-DHDF5) 
ENDIF( HDF5_FLIB )

TARGET_LINK_LIBRARIES( s3d.x ${HDF5_LIB} )

  IF ( SZ_LIB )
  TARGET_LINK_LIBRARIES( s3d.x ${SZ_LIB} )
ELSE ( SZ_LIB )
  MESSAGE("Need szlib to link hdf5")
  ENDIF ( SZ_LIB )
  IF ( Z_LIB )
  TARGET_LINK_LIBRARIES( s3d.x ${Z_LIB} )
ELSE ( Z_LIB )
  MESSAGE("Need zlib to link hdf5")
ENDIF ( Z_LIB )

ENDIF( HDF5_LIB )

  set( MPI_LIB_DIRS ${MPI_LIB_DIRS} /usr/lib64)
set( MPI_INCL_DIRS ${MPI_INCL_DIRS} /usr/include)

  FIND_LIBRARY( MPI_LIBRARY
      NAMES libmpich.a
      PATHS ${MPI_LIB_DIRS}
      ENV LIBRARY_PATH
      )
  FIND_LIBRARY( MPIF_LIBRARY
      NAMES libmpichf90.a
      PATHS ${MPI_LIB_DIRS}
      ENV LIBRARY_PATH
      )
  FIND_PATH( MPI_INCLUDE_PATH
      NAMES mpif.h
      PATHS ${MPI_INCL_DIRS} 
      ENV INCLUDE_PATH
      )

  IF(MPI_LIBRARY)
  IF( MPIF_LIBRARY)
  INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
  TARGET_LINK_LIBRARIES(s3d.x ${MPI_LIBRARY} )
  TARGET_LINK_LIBRARIES(s3d.x ${MPIF_LIBRARY} )
  ENDIF( MPIF_LIBRARY)
ENDIF(MPI_LIBRARY)

  INSTALL(TARGETS s3d.x DESTINATION run)
ENDIF( BUILD_LIBS3D )



