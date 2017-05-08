set(SRC
    Reduction.h
    Reduction.cpp
    main.cpp
)

include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${MPI_CXX_INCLUDE_PATH}
)

add_executable(reduction ${SRC})
target_link_libraries(reduction dataflow ${MPI_CXX_LIBRARIES})
    
INSTALL(TARGETS reduction
        DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
