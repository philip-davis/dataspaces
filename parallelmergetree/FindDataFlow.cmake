
#
# Try to find the DataFlow libraries
# Once done this will define
#
# DATAFLOW_FOUND          - system has DataFlow/TaskGraph.h
# DATAFLOW_INCLUDE_DIR    - path to DataFlow/TaskGraph.h
# DATAFLOW_LIBRARIES      - path to libdataflow.a
#
#


IF (NOT DEFINED DATAFLOW_FOUND)
   SET (DATAFLOW_FOUND FALSE)
ENDIF ()

FIND_PATH(DATAFLOW_INCLUDE_DIR DataFlow/TaskGraph.h
      ${CMAKE_SOURCE_DIR}/../DataFlow/build/include
      /usr/include
      /usr/X11/include
      /usr/X11R6/include
)

FIND_LIBRARY(DATAFLOW_LIBRARIES dataflow
       ${CMAKE_SOURCE_DIR}/../DataFlow/build/lib
       /usr/lib
       /sw/lib
)

       MESSAGE("Using DATAFLOW_INCLUDE_DIR = " ${DATAFLOW_INCLUDE_DIR}) 
       MESSAGE("Using DATAFLOW_LIBRARIES   = " ${DATAFLOW_LIBRARIES}) 

IF (DATAFLOW_INCLUDE_DIR AND DATAFLOW_LIBRARIES)

    SET(DATAFLOW_FOUND TRUE)
    IF (CMAKE_VERBOSE_MAKEFILE)
       MESSAGE("Using DATAFLOW_INCLUDE_DIR = " ${DATAFLOW_INCLUDE_DIR}) 
       MESSAGE("Using DATAFLOW_LIBRARIES   = " ${DATAFLOW_LIBRARIES}) 
    ENDIF (CMAKE_VERBOSE_MAKEFILE)

ELSE()
   
    MESSAGE("ERROR DataFlow library not found on the system")
 
ENDIF()

