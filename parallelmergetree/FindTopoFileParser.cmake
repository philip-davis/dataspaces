#
# Try to find the TopologyFileParser libraries
# Once done this will define
#
# TOPO_PARSER_FOUND          - system has TopologyFileParser
# TOPO_PARSER_INCLUDE_DIR    - path to TopologyFileParser/TopoFileDefinitions.h
# TOPO_PARSER_LIBRARIES      - path to libTopologyFileParser.a
#
#

IF (NOT DEFINED TOPO_PARSER_FOUND)
   SET (TOPO_PARSER_FOUND FALSE)
ENDIF ()

FIND_PATH(TOPO_PARSER_INCLUDE_DIR TalassConfig.h
    ${CMAKE_SOURCE_DIR}/../TopologyFileParser/build/include/talass
    /usr/include
    /usr/X11/include
    /usr/X11R6/include
)

FIND_LIBRARY(TOPO_PARSER_LIBRARIES TopologyFileParser
    /usr/lib
    /sw/lib
)


      MESSAGE("Using TOPO_PARSER_INCLUDE_DIR = " ${TOPO_PARSER_INCLUDE_DIR}) 
      MESSAGE("Using TOPO_PARSER_LIBRARIES   = " ${TOPO_PARSER_LIBRARIES}) 

IF (TOPO_PARSER_INCLUDE_DIR AND TOPO_PARSER_LIBRARIES)

   SET(TOPO_PARSER_FOUND TRUE)
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using TOPO_PARSER_INCLUDE_DIR = " ${TOPO_PARSER_INCLUDE_DIR}) 
      MESSAGE("Using TOPO_PARSER_LIBRARIES   = " ${TOPO_PARSER_LIBRARIES}) 
   ENDIF (CMAKE_VERBOSE_MAKEFILE)

ENDIF()
                         
