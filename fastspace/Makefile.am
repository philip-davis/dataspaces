AM_CFLAGS = -DLINUX -g
AM_CPPFLAGS = -I../dart/ -I../include/ $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = -L../dart -ldart -L../src -ldscommon $(DSPACESLIB_LDFLAGS) $(DSPACESLIB_LDADD)

lib_LIBRARIES =  libfastspace.a

libfastspace_a_SOURCES = carraysearch.cpp \
			 fastspace.c 

#include_HEADERS = ./fastspace.h 

noinst_HEADERS = ./fastspace.h \
		 ./carraysearch.h

