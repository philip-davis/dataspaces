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

set_target_properties(dscommon PROPERTIES
  OUTPUT_NAME dscommon
  VERSION ${DataSpaces_VERSION}
)

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

get_target_property(DS_LINK_OPTIONS dspaces LINK_OPTONS)

target_include_directories(dspaces PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)

set_target_properties(dspaces PROPERTIES
  OUTPUT_NAME dspaces
  VERSION ${DataSpaces_VERSION}
)

install(TARGETS dspaces dscommon)
