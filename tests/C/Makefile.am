AM_CFLAGS = -DLINUX -g $(DSPACESLIB_CFLAGS)
AM_CPPFLAGS = -I./ -I../../include -I../../dart $(DSPACESLIB_CPPFLAGS)
AM_FCFLAGS = -g $(DSPACESLIB_CPPFLAGS)
AM_LDFLAGS = $(DSPACESLIB_LDFLAGS)

bin_PROGRAMS = dataspaces_server test_writer test_reader test_reader_01 test_reader_02 test_reader_03 test_reader_04

dataspaces_server_SOURCES = common.c dataspaces_server.c
dataspaces_server_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_writer_SOURCES = common.c test_put_run.c test_writer.c
test_writer_LDADD = -L../../src -ldspaces -ldscommon  -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_SOURCES = common.c test_get_run.c test_reader.c
test_reader_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_01_SOURCES = common.c test_get_run.c test_reader_01.c
test_reader_01_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_02_SOURCES = common.c test_get_run.c test_reader_02.c
test_reader_02_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_03_SOURCES = common.c test_get_run.c test_reader_03.c
test_reader_03_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

test_reader_04_SOURCES = common.c test_get_run.c test_reader_04.c
test_reader_04_LDADD = -L../../src -ldspaces -ldscommon -L../../dart -ldart $(DSPACESLIB_LDADD)

noinst_HEADERS = common.h
