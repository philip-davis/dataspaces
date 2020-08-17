if(NOT (PC_GNI_FOUND STREQUAL "IGNORE"))
    find_package(PkgConfig)
    if(PKG_CONFIG_FOUND)
        set(SAVED_CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH})
        if(PMI_ROOT)
            list(INSERT CMAKE_PREFIX_PATH 0 
                "${PMI_ROOT}" 
                "${PMI_ROOT}/lib64/pkgconfig"
            )
        elseif(NOT ENV{PMI_ROOT} STREQUAL "")
            list(INSERT CMAKE_PREFIX_PATH 0
                "$ENV{PMI_ROOT}"
                "$ENV{PMI_ROOT}/lib64/pkgconfig"
            )
        endif()

        if(UGNI_ROOT)
            list(INSERT CMAKE_PREFIX_PATH 0
                "${UGNI_ROOT}"
                "${UGNI_ROOT}/lib64/pkgconfig"
            )
        elseif(NOT ENV{PMI_ROOT} STREQUAL "")
            list(INSERT CMAKE_PREFIX_PATH 0
                "$ENV{UGNI_ROOT}"
                "$ENV{UGNI_ROOT}/lib64/pkgconfig"
            )
        endif()
    
        set(PKG_CONFIG_USE_CMAKE_PREFIX_PATH ON)
        pkg_check_modules(PC_GNI pmi cray-ugni)
        
        set(CMAKE_PREFIX_PATH ${SAVED_CMAKE_PREFIX_PATH})
        unset(SAVED_CMAKE_PREFIX_PATH)

        if(PC_GNI_FOUND)
            if(BUILD_SHARED_LIBS)
                set(_PC_TYPE)
            else()
                set(_PC_TYPE _STATIC)
            endif()
            set(GNI_INCLUDE_DIRS ${PC_GNI${_PC_TYPE}_INCLUDE_DIRS})
            set(GNI_LIBRARY_DIRS ${PC_GNI${_PC_TYPE}_LIBRARY_DIRS})
            set(GNI_LIBRARIES ${PC_GNI${_PC_TYPE}_LIBRARIES})
            set(GNI_DEFINITIONS ${PC_GNI${_PC_TYPE}_CFLAGS_OTHER})

            set(GNI_LINK_LIBRARIES)
            foreach(newlib ${GNI_LIBRARIES})
               unset(libpath)
               string(PREPEND newlib "lib")
               if(BUILD_SHARED_LIBS)
                   string(APPEND newlib ".so")
               else()
                   string(APPEND newlib ".a")
               endif()
               find_library(${newlib}_libpath ${newlib} HINTS ${GNI_LIBRARY_DIRS})
               list(APPEND GNI_LINK_LIBRARIES "${${newlib}_libpath}")
            endforeach()
        endif()
    endif()
endif()
       
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(GNI DEFAULT_MSG  GNI_LIBRARIES)

if(GNI_FOUND)
    if(NOT TARGET gni::gni)
        add_library(gni::gni INTERFACE IMPORTED)
        if(GNI_INCLUDE_DIRS)
            set_target_properties(gni::gni PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${GNI_INCLUDE_DIRS}"
            )
        endif()
        if(GNI_DEFINITIONS)
            set_target_properties(gni::gni PROPERTIES
                INTERFACE_COMPILE_OPTIONS "${GNI_DEFINITIONS}"
            )
        endif()
        if(GNI_LIBRARIES)
            set_target_properties(gni::gni PROPERTIES
                INTERFACE_LINK_LIBRARIES "${GNI_LINK_LIBRARIES}"
            )
        endif()
    endif()
endif()
