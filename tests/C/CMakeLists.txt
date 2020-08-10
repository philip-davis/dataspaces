add_executable(dataspaces_server 
  common.c
  dataspaces_server.c
)

target_link_libraries(dataspaces_server
  dspaces
  dscommon
  dart
)

target_include_directories(dataspaces_server PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)

install(TARGETS dataspaces_server
  EXPORT DataSpaces RUNTIME
)

add_executable(test_writer
  common.c
  test_common.c
  test_put_run.c
  test_writer.c
)

target_link_libraries(test_writer
  dspaces
  dscommon
  dart
)

target_include_directories(test_writer PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)

add_executable(test_reader
  common.c
  test_common.c
  test_get_run.c
  test_reader.c
)

target_link_libraries(test_reader
  dspaces
  dscommon
  dart
)

target_include_directories(test_reader PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)
