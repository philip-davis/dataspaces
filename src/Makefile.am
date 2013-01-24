AM_CFLAGS = -DLINUX -g 
AM_CPPFLAGS = -I../dart/ -I../include/ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = -L../dart -ldart $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES =  libdspaces.a

libdspaces_a_SOURCES = dataspaces.c \
			ds_gspace.c \
			dc_gspace.c \
			ss_data.c \
			bbox.c \
			timer.c
#libdspaces_a_DEPENDENCIES = libdart.a

include_HEADERS = ../include/dataspaces.h

noinst_HEADERS = ../include/sfc.h \
		 ../include/timer.h \
		 ../include/merge.h \
		 ../include/queue.h \
		 ../include/ds_gspace.h \
		 ../include/dc_gspace.h \
		 ../include/ss_data.h \
		 ../include/bbox.h

