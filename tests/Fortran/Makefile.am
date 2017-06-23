#AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
#AM_CPPFLAGS = -I./ -I../../include -I../../dart $(DSPACESLIB_CPPFLAGS)
#AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = test_put test_get

test_put_SOURCES = couple_mod.F90 test_put.F90
test_put_LDADD = -L../../src -ldspacesf -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_get_SOURCES = couple_mod.F90 test_get.F90
test_get_LDADD = -L../../src -ldspacesf -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_put.o: test_put.F90 couple_mod.o
test_get.o: test_get.F90 couple_mod.o
couple_mod.o: couple_mod.F90
