#===============================================================================!
#    Program: s3d
#    File:    $HeadURL$
#
#    S3D was developed at SNL (CRF) under the direction of Dr. J.H. Chen
# 
#    This file
#    authored by:   Ray W. Grout (National Renewable Energy Laboratory)
#                   ray.grout@nrel.gov
#
#    Modified by:   
#
#             On:   04Oct2011
#
#    Description:   Build files for unit tests
#
#
#    Last Revision: $Rev$
#    Revision by:   $Author$
#    Date:          $Date$
#===============================================================================

set (PROJECT_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR})

include_directories( ${MODULES} )
include_directories( ${PROJECT_SOURCE_DIR}/include )
include_directories( ${PROJECT_BINARY_DIR}/)

# Directory with bare-bones replacements for some s3d modules
set (STUBS ${PROJECT_TEST_DIR}/stubs)


# Derivative unit test
set (DERIV ${PROJECT_TEST_DIR}/derivative)

SET( DERIV_TEST_FILES
  ${DERIV}/deriv_test.f90                 
  ${STUBS}/__test__terminate_run.f90
  ${STUBS}/__test__write_header.f90
  ${STUBS}/__test__param_m.f90          
  ${MODULES}/topology_m.f90          
  ${SOLVE}/derivative_x.f90          
  ${SOLVE}/derivative_y.f90          
  ${SOLVE}/derivative_z.f90          
  ${MODULES}/derivative_m.f90          
  ${MODULES}/grid_m.f90          
  ${MODULES}/reference_m.f90          
  )

ADD_EXECUTABLE(
    deriv_test
    ${DERIV_TEST_FILES}
    )

ADD_TEST( derivative ${EXECUTABLE_OUTPUT_PATH}/deriv_test 1)

# Thermochem unit test
set (TCHEM ${PROJECT_TEST_DIR}/thermochem)

SET( TCHEM_TEST_FILES
  ${MODULES}/chemkin_m.f90          
  ${TCHEM}/calc_temp_test.f90                 
  ${STUBS}/__test__terminate_run.f90
  ${STUBS}/__test__write_header.f90
  ${STUBS}/__test__inquire_about_input_file.f90
  ${TCHEM}/__test__param_m.f90          
  ${MODULES}/topology_m.f90          
  ${MODULES}/grid_m.f90          
  ${MODULES}/reference_m.f90          
  ${MODULES}/thermchem_m.f90          
  ${FSEVEN}/cklib.f                      
  ${FSEVEN}/ck2f90_tools.f                 
  ${FSEVEN}/ckheadersetup.f               
  ${FSEVEN}/ckerr.f                 
  ${GETRATES_ALL_SRC}
  )

ADD_EXECUTABLE(
    tchem_test
    ${TCHEM_TEST_FILES}
    )

ADD_TEST( tchem ${EXECUTABLE_OUTPUT_PATH}/tchem_test 1)

FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/input)
FILE(COPY ${chem_inputfiles} DESTINATION ${CMAKE_BINARY_DIR}/input)
