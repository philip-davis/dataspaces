# Try to find the DIY library in the system
# Once done this will define
#
# DIY_FOUND -- boolean that indicates whether DIY was found
# DIY_INCLUDE_DIR -- the include path for DIY
# DIY_LIBRARIES -- the DIY libraries to link against

## Try to find the include directory
find_path(DIY_INCLUDE_DIR
    NAMES diy.h
    PATHS_SUFFIXES diy
    PATHS /usr/include /usr/local/diy/include)

## Try to find the DIY library
find_library(DIY_LIBRARIES diy
    PATHS_SUFFIXES diy diy/lib # Necessary for diy's weird install
    /usr/local/diy/lib 
    /usr/lib
    )


#include(FindPackageHandleStandardArgs)
#find_package_handle_standard_args(
#    DIY DEFAULT_MSG DIY_INCLUDE_DIR DIY_LIBRARIES)
#    mark_as_advanced(DIY_INCLUDE_DIR)
#    mark_as_advanced(DIY_LIBRARIES)

