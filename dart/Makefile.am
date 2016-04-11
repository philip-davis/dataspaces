AM_CLAGS = -DLINUX -g
AM_CPPFLAGS = -I./ -I../ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES = libdart.a

noinst_HEADERS = dart.h \
		 debug.h \
		 list.h

if HAVE_UGNI
libdart_a_SOURCES = gni/dart_rpc_gni.c \
					gni/ds_base_gni.c \
					gni/dc_base_gni.c \
					gni/dart_rdma_gni.c
noinst_HEADERS +=	gni/utility_functions.h \
					gni/dart_rpc_gni.h \
					gni/ds_base_gni.h \
					gni/dc_base_gni.h \
					gni/dart_rdma_gni.h
endif # HAVE_UGNI
if HAVE_PORTALS
libdart_a_SOURCES = portals/dart_rpc_portals.c \
					portals/ds_base_portals.c \
					portals/dc_base_portals.c
noinst_HEADERS +=	portals/dart_rpc_portals.h \
					portals/ds_base_portals.h \
					portals/dc_base_portals.h
endif # HAVE_PORTALS
if HAVE_INFINIBAND
if ENABLE_IB_CPP
AM_CPPFLAGS += -I./../include -I./ib_cpp/include
AM_LDFLAGS += -lmpi
  
libdart_a_SOURCES = ib_cpp/dart_rpc_ib_.cpp \
                    ib_cpp/ds_base_ib_.cpp \
                    ib_cpp/dc_base_ib_.cpp \
                    ib_cpp/dart_rdma_ib_.cpp \
                    ib_cpp/patch_ib_cpp.cpp
  
noinst_HEADERS +=  ib_cpp/include/dart_rpc_ib.h \
                   ib_cpp/include/ds_base_ib.h \
                   ib_cpp/include/dc_base_ib.h \
                   ib_cpp/include/dart_rdma_ib.h \
                   ib_cpp/include/patch_ib.h \
                   ib_cpp/include/patch_ib_cpp.h
else
libdart_a_SOURCES = ib/dart_rpc_ib.c \
          					ib/ds_base_ib.c \
          					ib/dc_base_ib.c \
          					ib/dart_rdma_ib.c
noinst_HEADERS +=	ib/dart_rpc_ib.h \
        					ib/dc_base_ib.h \
        					ib/ds_base_ib.h \
        					ib/dart_rdma_ib.h
endif # ENABLE_IB_CPP
endif # HAVE_INFINIBAND
if HAVE_DCMF
libdart_a_SOURCES = dcmf/dart_rpc_dcmf.c \
					dcmf/ds_base_dcmf.c \
					dcmf/dc_base_dcmf.c \
					dcmf/dart_rdma_dcmf.c
noinst_HEADERS +=	dcmf/dart_rpc_dcmf.h \
					dcmf/ds_base_dcmf.h \
					dcmf/dc_base_dcmf.h \
					dcmf/dart_rdma_dcmf.h
endif # HAVE_DCMF

if HAVE_PAMI
libdart_a_SOURCES = pami/dart_rpc_pami.c \
                                        pami/ds_base_pami.c \
                                        pami/dc_base_pami.c \
                                        pami/dart_rdma_pami.c
noinst_HEADERS +=       pami/dart_rpc_pami.h \
                                        pami/ds_base_pami.h \
                                        pami/dc_base_pami.h \
                                        pami/dart_rdma_pami.h
endif # HAVE_PAMI

if HAVE_TCP_SOCKET
libdart_a_SOURCES = tcp/dart_rpc_tcp.c \
					tcp/ds_base_tcp.c \
					tcp/dc_base_tcp.c 
noinst_HEADERS +=	tcp/dart_rpc_tcp.h \
					tcp/ds_base_tcp.h \
					tcp/dc_base_tcp.h
endif # HAVE_TCP_SOCKET
