AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
AM_CPPFLAGS = -I./ -I../include -I../dart $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = testC testF

testC_SOURCES = testC.c
testC_LDADD = -L../src -ldspaces -ldscommon -L../dart -ldart $(DSPACESLIB_LDADD)

testF_SOURCES = testF.F90
testF_LDADD = -L../src -ldspaces -ldscommon -L../dart -ldart $(DSPACESLIB_LDADD)
