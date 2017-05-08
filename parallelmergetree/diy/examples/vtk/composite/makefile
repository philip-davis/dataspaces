#----------------------------------------------------------------------------
#
# Tom Peterka
# Argonne National Laboratory
# 9700 S. Cass Ave.
# Argonne, IL 60439
# tpeterka@mcs.anl.gov
#
# (C) 2012 by Argonne National Laboratory.
# See COPYRIGHT in top-level directory.
#
#----------------------------------------------------------------------------
include ../../../config_defs

INCLUDE = -I../../../include \
	-I$(HOME)/software/VTK/include/vtk-5.8 \

LIBS = -L../../../lib \
	-L$(HOME)/software/VTK/lib/vtk-5.8 \

LIBS += -ldiy \
	-lvtkRendering -lvtkGraphics -lvtkIO -lvtkFiltering \
	-lvtkCommon -lvtkexpat -lvtkpng -lvtktiff -lvtkzlib -lvtkjpeg -lvtksys

ifeq ($(ARCH), MAC)
LIBS += -framework OpenGL \
	-framework Cocoa -framework IOKit
else
LIBS += -lGL -lXt
endif

CXX = mpicxx
CCFLAGS = -g -Wno-deprecated

OBJS = composite.o

.SUFFIXES: .cpp

.cpp.o:
	$(CXX) -c $(CCFLAGS) $(INCLUDE) $<

default: all

all: example

example: $(OBJS)
	$(CXX) -o example $(OBJS) $(LIBS)

clean:
	rm -f *.o 
