AM_CPPFLAGS = -I./include -I../ -I../.. -I../../include $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES = libibcpp.a

libibcpp_a_SOURCES = dart_rpc_ib.cpp \
                     ds_base_ib.cpp \
                     dc_base_ib.cpp \
                     dart_rdma_ib.cpp \
                     patch_ib_cpp.cpp

noinst_HEADERS = include/dart_rpc_ib.h \
                 include/ds_base_ib.h \
                 include/dc_base_ib.h \
                 include/dart_rdma_ib.h \
                 include/patch_ib_cpp.h