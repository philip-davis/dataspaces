#----------------------------------------------------------------------------
#
# Tom Peterka
# Argonne National Laboratory
# 9700 S. Cass Ave.
# Argonne, IL 60439
# tpeterka@mcs.anl.gov
#
# (C) 2011 by Argonne National Laboratory.
# See COPYRIGHT in top-level directory.
#
#----------------------------------------------------------------------------
include ../../config_defs

INCLUDE += -I../../include
LIBS += -L../../lib -ldiy

ifeq ($(OPENMP), YES)
ifeq ($(ARCH), BGP)
CXX = mpixlcxx_r
CCFLAGS += -O3 -qarch=450d -qtune=450
endif
ifeq ($(ARCH), BGQ)
CXX = mpixlcxx_r
endif
endif

ifeq ($(PNETCDF), YES)
INCLUDE += -I$(NETCDF_INC)
LIBS += -lpnetcdf
endif
ifeq ($(HDF5), YES)
INCLUDE += -I$(HDF_INC)
LIBS += -lz  -lhdf5
endif
ifeq ($(ZOLTAN), YES)
INCLUDE += -I$(ZOLTAN_INC)
LIBS += -lzoltan
endif
ifeq ($(ZLIB), YES)
INCLUDE += -I$(ZLIB_INC)
LIBS += -lz
endif

CCFLAGS = -g

.SUFFIXES: .cpp

.cpp.o:
	$(CXX) -c $(CCFLAGS) $(INCLUDE) $<

default: all

all: merge swap neighbor send-recv

merge: merge.o
	$(CXX) -o merge merge.o $(LIBS)

swap: swap.o
	$(CXX) -o swap swap.o $(LIBS)

neighbor: neighbor.o
	$(CXX) -o neighbor neighbor.o $(LIBS)

send-recv: send-recv.o
	$(CXX) -o send-recv send-recv.o $(LIBS)

clean:
	rm -f *.o 
