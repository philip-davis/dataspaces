include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${DATAFLOW_INCLUDE_DIR}
    ${MPI_CXX_INCLUDE_PATH}
    ${DIY_INCLUDE_DIR}
   )

if (ENABLE_TOPO_FILE_PARSER)
    include_directories(${TOPO_PARSER_INCLUDE_DIR})
endif()

if (ENABLE_PIDX)
    include_directories( ${PIDX_INCLUDE_DIR})
endif()

add_executable(parallel_merge_tree parallel_merge_tree.cpp)
target_link_libraries(parallel_merge_tree pmt ${DATAFLOW_LIBRARIES} 
  ${MPI_CXX_LIBRARIES} ${DIY_LIBRARIES})
if (ENABLE_TOPO_FILE_PARSER)
  target_link_libraries(parallel_merge_tree ${TOPO_PARSER_LIBRARIES})
endif()


add_executable(k_way_test k_way_test.cpp)
target_link_libraries(k_way_test pmt ${DATAFLOW_LIBRARIES} ${MPI_CXX_LIBRARIES})
if (ENABLE_TOPO_FILE_PARSER)
  target_link_libraries(k_way_test ${TOPO_PARSER_LIBRARIES})
endif()


add_executable(local_compute_test local_compute_test.cpp)
target_link_libraries(local_compute_test pmt)
if (ENABLE_TOPO_FILE_PARSER)
  target_link_libraries(local_compute_test ${TOPO_PARSER_LIBRARIES})
endif()


add_executable(multi_task_local_compute_test multi_task_local_compute_test.cpp)
target_link_libraries(multi_task_local_compute_test pmt ${DATAFLOW_LIBRARIES}
  ${MPI_CXX_LIBRARIES} ${DIY_LIBRARIES})
if (ENABLE_TOPO_FILE_PARSER)
  target_link_libraries(multi_task_local_compute_test ${TOPO_PARSER_LIBRARIES})
endif()

add_executable(comprehensive_parallel_merge_tree_test comprehensive_parallel_merge_tree_test.cpp)
target_link_libraries(comprehensive_parallel_merge_tree_test pmt ${DATAFLOW_LIBRARIES}
  ${MPI_CXX_LIBRARIES} ${DIY_LIBRARIES})
if (ENABLE_TOPO_FILE_PARSER)
  target_link_libraries(comprehensive_parallel_merge_tree_test ${TOPO_PARSER_LIBRARIES})
endif()

add_executable(construct_full_tree construct_full_tree.cpp)

if (ENABLE_PIDX) 
  add_executable(rst_parallelMT rst_parallelMT.cpp)
  target_link_libraries(rst_parallelMT pmt ${DATAFLOW_LIBRARIES}
    ${MPI_CXX_LIBRARIES} ${DIY_LIBRARIES} ${PIDX_LIBRARIES})
  if (ENABLE_TOPO_FILE_PARSER)
    target_link_libraries(rst_parallelMT ${TOPO_PARSER_LIBRARIES})
  endif()
endif()

