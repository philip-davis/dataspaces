set(dart_SOURCES)

if(DSPACES_FABRIC STREQUAL INFINIBAND)
  list(APPEND dart_SOURCES
    ib/dart_rdma_ib.c
    ib/dart_rpc_ib.c
    ib/dc_base_ib.c
    ib/ds_base_ib.c
  )
elseif(DSPACES_FABRIC STREQUAL GNI)
  list(APPEND dart_SOURCES
    gni/dart_rdma_gni.c
    gni/dart_rpc_gni.c
    gni/dc_base_gni.c
    gni/ds_base_gni.c
  )
else()
  list(APPEND dart_SOURCES
    tcp/dart_rpc_tcp.c
    tcp/ds_base_tcp.c
    tcp/dc_base_tcp.c
  )
endif()

add_library(dart ${dart_SOURCES})

target_include_directories(dart PRIVATE
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/dart>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
)

set_target_properties(dart PROPERTIES
  OUTPUT_NAME dart
  VERSION ${DataSpaces_VERSION}
)
