AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
AM_CPPFLAGS = -I./ -I../../include -I../../dart $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = test_writer test_reader test_space_spmd test_space 

#dataspaces_server_SOURCES = common.c dataspaces_server.c
#dataspaces_server_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_writer_SOURCES = common.c test_put_run.c test_writer.c
test_writer_LDADD = -L../../src -ldspaces -ldscommon  -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_SOURCES = common.c test_get_run.c test_reader.c
test_reader_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

#dataspaces_server_mpmd_SOURCES = common.c dataspaces_server_mpmd.c
#dataspaces_server_mpmd_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

#test_writer_mpmd_SOURCES = common.c test_put_run.c test_writer_mpmd.c
#test_writer_mpmd_LDADD = -L../../src -ldspaces -ldscommon  -L../../dart -ldart $(DSPACESLIB_LDADD)

#test_reader_mpmd_SOURCES = common.c test_get_run.c test_reader_mpmd.c
#test_reader_mpmd_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

#test_coupling_SOURCES = common.c test_put_run.c test_get_run.c \
#				test_coupling.c 
#test_coupling_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)
test_space_spmd_SOURCES = common.c test_space_spmd.c test_put_run.c test_get_run.c
test_space_spmd_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_space_SOURCES = test_space.c common.c
test_space_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)


noinst_HEADERS = common.h
