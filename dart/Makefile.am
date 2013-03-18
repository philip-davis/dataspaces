AM_CLAGS = -DLINUX -g
AM_CPPFLAGS = -I./ -I../ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES = libdart.a

include_HEADERS = dart.h

noinst_HEADERS = debug.h \
		 list.h

if HAVE_UGNI
libdart_a_SOURCES = gni/dart_rpc_gni.c \
		    gni/ds_base_gni.c \
                    gni/dc_base_gni.c
noinst_HEADERS += gni/utility_functions.h \
		  gni/dart_rpc_gni.h \
                  gni/ds_base_gni.h \
		  gni/dc_base_gni.h
endif # HAVE_UGNI
if HAVE_PORTALS
libdart_a_SOURCES = portals/dart_rpc_portals.c \
		    portals/ds_base_portals.c \
                    portals/dc_base_portals.c
noinst_HEADERS += portals/dart_rpc_portals.h \
                  portals/ds_base_portals.h \
		  portals/dc_base_portals.h
endif # HAVE_PORTALS
if HAVE_INFINIBAND 
libdart_a_SOURCES = ib/dart_rpc_ib.c \
		    ib/ds_base_ib.c \
                    ib/dc_base_ib.c
noinst_HEADERS += ib/dart_rpc_ib.h \
                  ib/ds_base_ib.h \
		  ib/dc_base_ib.h
endif # HAVE_INFINIBAND
if HAVE_DCMF
libdart_a_SOURCES = dcmf/dart_rpc_dcmf.c \
		    dcmf/ds_base_dcmf.c \
                    dcmf/dc_base_dcmf.c \
		    dcmf/dart_rdma_dcmf.c
noinst_HEADERS += dcmf/dart_rpc_dcmf.h \
                  dcmf/ds_base_dcmf.h \
		  dcmf/dc_base_dcmf.h \
		  dcmf/dart_rdma_dcmf.h
endif # HAVE_DCMF


