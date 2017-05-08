cmake_minimum_required(VERSION 2.8)

PROJECT(stats)
FIND_PACKAGE(VTK REQUIRED)
INCLUDE(${VTK_USE_FILE})

SET(SRCS
   insituDescriptiveStats.cpp
   vtkS3DSource.cpp
)

FIND_PATH(MULTIFAB_INCLUDE MultiFab.H
   ../../BoxLib/Src/C_BaseLib/
)


ADD_LIBRARY(descriptiveStats ${SRCS}) 
TARGET_LINK_LIBRARIES(descriptiveStats ${VTK_LIBRARIES}) 
INCLUDE_DIRECTORIES(${VTK_INCLUDES} ${MULTIFAB_INCLUDE})

ADD_EXECUTABLE(testDriver testDriver.cpp)
TARGET_LINK_LIBRARIES(testDriver descriptiveStats) 
