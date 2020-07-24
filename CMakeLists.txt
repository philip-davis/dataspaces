cmake_minimum_required(VERSION 3.1...3.16)

project(DataSpaces VERSION 1.9.0 LANGUAGES C Fortran)

### Require out-of-source builds
file(TO_CMAKE_PATH "${PROJECT_BINARY_DIR}/CMakeLists.txt" LOC_PATH)
if(EXISTS "${LOC_PATH}")
    message(FATAL_ERROR "You cannot build in a source directory (or any directory with a CMakeLists.txt file). Please make a build subdirectory. Feel free to remove CMakeCache.txt and CMakeFiles.")
endif()

list(INSERT CMAKE_MODULE_PATH 0 "${PROJECT_SOURCE_DIR}/cmake")

# Default to a debug build if not specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
endif()

set(DSPACES_FABRIC AUTO CACHE STRING "Communication fabric to use")
set_property(CACHE DSPACES_FABRIC PROPERTY STRINGS "AUTO;TCP;INFINIBAND;GNI")
include(${PROJECT_SOURCE_DIR}/cmake/DetectTransport.cmake)

set(DS_MAX_NDIM 3 CACHE STRING "Maximum dimensionality of shared space")
set_property(CACHE DS_MAX_NDIM PROPERTY STRINGS "1;2;3;4;5;6;7;8;9;10")

configure_file(
  ${PROJECT_SOURCE_DIR}/DSpacesConfig.h.in
  ${PROJECT_BINARY_DIR}/config.h
)

add_subdirectory(dart)
add_subdirectory(src)
#add_subdirectory(scripts)
add_subdirectory(tests)
