set(SRC
    Broadcast.h
    Broadcast.cpp
    main.cpp
)

include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${MPI_CXX_INCLUDE_PATH}
)

add_executable(broadcast ${SRC})
target_link_libraries(broadcast dataflow ${MPI_CXX_LIBRARIES})

INSTALL(TARGETS broadcast
        DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
    