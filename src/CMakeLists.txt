add_library(dscommon
  bbox.c
  ss_data.c
  timer.c
  util.c
)

target_include_directories(dscommon PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)

target_link_libraries(dscommon pthread m rt)

add_library(dspaces
  dataspaces.c
  common_dataspaces.c
  ds_gspace.c
  dc_gspace.c
  dimes_data.c
  dimes_server.c
  dimes_client.c
)

target_link_libraries(dspaces pthread m rt)

target_include_directories(dspaces PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)
