Project(ParallelMergeTree)
cmake_minimum_required(VERSION 2.8)
set(CMAKE_VERBOSE_MAKEFILE TRUE)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_MODULE_PATH})

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/" CACHE PATH "default install path" FORCE )
endif()


Find_Package(MPI)
set(CMAKE_CXX_COMPILER ${MPI_CXX_COMPILER})
set(CMAKE_C_COMPILER ${MPI_C_COMPILER})

include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)


if(COMPILER_SUPPORTS_CXX11)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")# -stdlib=libc++")
elseif(COMPILER_SUPPORTS_CXX0X)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")# -stdlib=libc++")
else()
        message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler.")
endif()


Find_Package(DataFlow)
Find_Package(DIY)
FIND_PACKAGE(PIDX)

if (ENABLE_TOPO_FILE_PARSER)
    find_package(TopoFileParser)
    add_definitions(-DUSE_TOPO_FILE_PARSER)
endif()

if (TOPO_PARSER_FOUND)
    add_definitions(-DUSE_TOPO_FILE_PARSER)
endif()

add_subdirectory(src)
add_subdirectory(utilities)
