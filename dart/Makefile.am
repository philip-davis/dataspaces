AM_CFLAGS = -DLINUX -g
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
if HAVE_INFINIBAND
libdart_a_SOURCES = ib/dart_rpc_ib.c \
          					ib/ds_base_ib.c \
          					ib/dc_base_ib.c \
          					ib/dart_rdma_ib.c
noinst_HEADERS +=	ib/dart_rpc_ib.h \
        					ib/dc_base_ib.h \
        					ib/ds_base_ib.h \
        					ib/dart_rdma_ib.h
endif # HAVE_INFINIBAND
if HAVE_TCP_SOCKET
libdart_a_SOURCES = tcp/dart_rpc_tcp.c \
					tcp/ds_base_tcp.c \
					tcp/dc_base_tcp.c 
noinst_HEADERS +=	tcp/dart_rpc_tcp.h \
					tcp/ds_base_tcp.h \
					tcp/dc_base_tcp.h
endif # HAVE_TCP_SOCKET
