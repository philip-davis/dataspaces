# Try to find the PIDX library in the system
# Once done this will define
#
# PIDX_FOUND -- boolean that indicates whether PIDX was found
# PIDX_INCLUDE_DIR -- the include path for PIDX
# PIDX_LIBRARIES -- the PIDX libraries to link against

## Try to find the include directory
find_path(PIDX_INCLUDE_DIR
    NAMES PIDX_rst.h 
    NAMES PIDX_data_structs.h
    PATHS /usr/include /usr/local/pidx/include)

## Try to find the PIDX library
find_library(PIDX_LIBRARIES
    NAMES pidx
    PATHS /usr/local/pidx/lib /usr/lib)

#include(FindPackageHandleStandardArgs)
#find_package_handle_standard_args(
#    PIDX DEFAULT_MSG PIDX_INCLUDE_DIR PIDX_LIBRARIES)
#    mark_as_advanced(PIDX_INCLUDE_DIR)
#    mark_as_advanced(PIDX_LIBRARIES)

