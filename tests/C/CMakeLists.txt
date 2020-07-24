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
