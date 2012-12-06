AM_CFLAGS = -DLINUX -g 
AM_CPPFLAGS = -I../dart/ -I../include/ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = -L../dart -ldart $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES = libdscommon.a libdspaces.a

libdscommon_a_SOURCES = common.c

libdspaces_a_SOURCES = dataspaces.c
libdspaces_a_DEPENDENCIES = libdscommon.a

include_HEADERS = ../include/dataspaces.h

noinst_HEADERS = ../include/sfc.h \
		 ../include/timer.h \
		 ../include/merge.h \
		 ../include/queue.h

