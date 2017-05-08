cmake_minimum_required(VERSION 2.8)

PROJECT(inistu_driver)

SET(SRCS
   intransit_driver.cpp
   ../statistics/intransitDescriptiveStats.cpp
)

SET(DSPACE_LIBS
/ccs/proj/e2e/habbasi/s3dinsitu/dataspaces/lib/libdspace.a 
/opt/cray/pmi/3.0.0-1.0000.8661.28.2807.gem/lib64/libpmi.a   
/opt/cray/ugni/2.3-1.0400.3912.4.29.gem/lib64/libugni.a   
/opt/cray/rca/default/lib64/librca.a 
)

SET(DSPACE_INC
/ccs/proj/e2e/habbasi/s3dinsitu/dataspaces/include 
/opt/cray/rca/default/include
/opt/cray-hss-devel/6.0.0/include 
/opt/cray/krca/1.0.0-2.0400.29855.4.28.gem/include
)

INCLUDE_DIRECTORIES(${DSPACE_INC} ../statistics ../)
ADD_LIBRARY(intransit_driver ${SRCS}) 
TARGET_LINK_LIBRARIES(intransit_driver ${DSPACE_LIBS}) 

