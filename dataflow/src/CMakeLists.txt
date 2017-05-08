

SET (DATAFLOW_HEADERS
    Definitions.h
    Task.h
    
    TaskGraph.h 
    RelayTask.h
    Controller.h
   
    ModuloMap.h
)

SET (DATAFLOW_SRC
    Task.cpp    
    TaskGraph.cpp
    Controller.cpp
    ModuloMap.cpp
)

include_directories(${MPI_CXX_INCLUDE_PATH})

add_library(dataflow STATIC ${DATAFLOW_SRC})

target_link_libraries(dataflow ${MPI_CXX_LIBRARIES})

INSTALL(FILES ${DATAFLOW_HEADERS}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include/DataFlow
)

INSTALL(TARGETS dataflow
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
)

