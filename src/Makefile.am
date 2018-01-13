AM_CFLAGS = -DLINUX -g 
AM_CPPFLAGS = -I../dart/ -I../include/ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = -L../dart -ldart $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES =  libdscommon.a libdspaces.a libdspacesf.a

libdscommon_a_SOURCES = bbox.c \
			mem_persist.c \
			ss_data.c \
			timer.c \
			util.c \
			CppWrapper.cc \
			MapWrap.cc

libdspaces_a_SOURCES =  dataspaces.c \
			common_dataspaces.c \
			ds_gspace.c \
			dc_gspace.c \
			dimes_data.c \
			dimes_server.c \
			dimes_client.c \
			#ml_pthread.c 

libdspacesf_a_SOURCES = dataspaces_fortran.c \
			common_dataspaces.c \
			ds_gspace.c \
			dc_gspace.c \
			dimes_data.c \
			dimes_server.c \
			dimes_client.c \
			#ml_pthread.c 

include_HEADERS = ../include/dataspaces.h \
		  ../include/dimes_interface.h

noinst_HEADERS = ../include/sfc.h \
		 ../include/timer.h \
		 ../include/merge.h \
		 ../include/queue.h \
		 ../include/ds_gspace.h \
		 ../include/mem_persist.h \
		 ../include/ml_pthread.h \
		 ../include/dc_gspace.h \
		 ../include/ss_data.h \
		 ../include/bbox.h \
		 ../include/util.h \
		 ../include/dimes_data.h \
		 ../include/dimes_server.h \
		 ../include/dimes_client.h \
		 ../include/common_dataspaces.h
