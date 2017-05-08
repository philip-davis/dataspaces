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

# imesh and moab variables
IMESH_CXXFLAGS =  -Wall -pipe -pedantic -Wno-long-long -O2 -DNDEBUG 
IMESH_CFLAGS =  -Wall -pipe -pedantic -Wno-long-long -O2 -DNDEBUG
IMESH_LDFLAGS = -L$(HDF_LIB)
IMESH_FC = mpif90
IMESH_FCFLAGS =  -O2
IMESH_FCDEFS = -DHAVE_CONFIG_H
IMESH_INCLUDES = -I$(IMESH_INCLUDEDIR) $(IMESH_INCLUDEDIR2)
IMESH_LIBS = $(IMESH_LDFLAGS) -L$(MOAB_LIBDIR) \
             -L$(IMESH_LIBDIR) -liMesh -lMOAB \
              -lhdf5  -lz -lm
IMESH_INCLUDEDIR = $(MOAB_BASE)/itaps/imesh
IMESH_INCLUDEDIR2 = -I$(MOAB_BASE)/itaps/imesh -I$(MOAB_BASE)/itaps/imesh/.. \
	-I$(MOAB_BASE)/itaps/imesh/.. -I$(MOAB_BASE)/src
IMESH_LIBDIR = $(MOAB_BASE)/itaps/imesh/.libs
MOAB_LIBDIR = $(MOAB_BASE)/src/.libs
IMESH_INCLUDEDIR=$(MOAB_BASE)/include
IMESH_INCLUDEDIR2=
IMESH_LIBDIR=$(MOAB_BASE)/lib
MOAB_LIBDIR=$(MOAB_BASE)/lib


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

moab: main.o ${IMESH_FILES} 
	$(CXX) $(CXXFLAGS) -o $@ main.o \
	${IMESH_LIBS} ${LIBS}

.cpp.o:
	${CXX} -c ${CXXFLAGS} ${IMESH_INCLUDES} ${INCLUDE} $<

clean:
	rm -f *.o moab
