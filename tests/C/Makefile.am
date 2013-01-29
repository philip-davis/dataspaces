AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
AM_CPPFLAGS = -I./ -I../../include -I../../dart $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = test_dspaces test_put test_get test_dspaces_spmd

test_dspaces_SOURCES = common.c test_dspaces.c
test_dspaces_LDADD = -L../../src -ldspaces -L../../dart -ldart $(DSPACESLIB_LDADD)

test_put_SOURCES = common.c test_put.c
test_put_LDADD = -L../../src -ldspaces -L../../dart -ldart $(DSPACESLIB_LDADD)

test_get_SOURCES = common.c test_get.c
test_get_LDADD = -L../../src -ldspaces -L../../dart -ldart $(DSPACESLIB_LDADD)

test_dspaces_spmd_SOURCES = common.c test_put_spmd.c test_get_spmd.c \
				test_dspaces_spmd.c 
test_dspaces_spmd_LDADD = -L../../src -ldspaces -L../../dart -ldart $(DSPACESLIB_LDADD)
