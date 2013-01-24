AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
AM_CPPFLAGS = -I./ -I../include -I../dart $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = dataspaces_server test_put test_get 

dataspaces_server_SOURCES = test_dsspace.c
dataspaces_server_LDADD = -L../src -ldspaces -L../dart -ldart $(DSPACESLIB_LDADD)

test_put_SOURCES = couple_mod.F90 test_put.F90
test_put_LDADD = -L../src -ldspaces -L../dart -ldart $(DSPACESLIB_LDADD)


test_get_SOURCES = couple_mod.F90 test_get.F90
test_get_LDADD = -L../src -ldspaces -L../dart -ldart $(DSPACESLIB_LDADD)

