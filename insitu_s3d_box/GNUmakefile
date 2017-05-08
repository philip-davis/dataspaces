#
# $Id: GNUmakefile,v 1.90 2009-11-24 18:01:41 jbb Exp $
#
BOXLIB_HOME = ../BoxLib
#VTK = /ccs/proj/e2e/habbasi/s3dinsitu/vtkmodular/install
#TOPOLOGY = /ccs/proj/e2e/habbasi/s3dinsitu/topology/

# Variables settable by users.
PRECISION     = DOUBLE
PROFILE       = TRUE
PROFILE       = FALSE
COMP          = gcc
FCOMP         = gfortran
DEBUG         = TRUE
DEBUG         = FALSE
DIM           = 2
DIM           = 3
USE_MPI       = FALSE
USE_MPI       = TRUE
USE_ARRAYVIEW = TRUE
USE_ARRAYVIEW = FALSE

# Base name of the executable.
EBASE = s3d_box

# Here's where we decide which of the problems to build.
#
# PROB_DIR is the name of a subdirectory of the directory
# containing this GNUmakefile.  It contains the problem-specific
# Fortran code for the problem type that you wish to run.
#

# For gnu toolchain ------------ 
#include $(BOXLIB_HOME)/Tools/C_mk/Make.defs
#fFLAGS += -cpp -fdefault-real-8
#fincludes += ./libs3d/source
#DEFINES += -fexceptions -cpp
#BL_MPI_LIBS += -L./libs3d/source -ls3d 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersParallelStatistics-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersStatistics-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkalglib-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkInfovisCore-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersExtraction-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersGeneral-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonComputationalGeometry-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersCore-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkParallelMPI-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkParallelCore-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkIOGeometry-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkIOCore-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonExecutionModel-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonDataModel-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonMisc-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonSystem-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonTransforms-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonMath-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonCore-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtksys-5.11.a
#BL_MPI_LIBS += $(VTK)/lib/libvtkzlib-5.11.a
#BL_MPI_LIBS += -L/usr/lib -L/usr/lib -Wl,-rpath -Wl,/usr/lib -lmpichcxx -lmpich -lopa -lpthread -lrt 
### For PGI toolchain -----------
COMP          = CC
FCOMP         = ftn
include Make.pgi

SHELL = /bin/sh
PMI_CFLAGS = $(shell pkg-config --cflags cray-pmi)
PMI_LIBS = $(shell pkg-config --libs cray-pmi)
UGNI_CFLAGS = $(shell pkg-config --cflags cray-ugni)
UGNI_LIBS = $(shell pkg-config --libs cray-ugni)

CFLAGS += -g ${UGNI_CFLAGS} ${PMI_CLFASG} #-DHYBRID

BL_MPI_LIBS += -L./libs3d/source -ls3d  -lm 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersParallelStatistics-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersStatistics-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkalglib-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkInfovisCore-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersExtraction-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersGeneral-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonComputationalGeometry-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkFiltersCore-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkParallelMPI-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkParallelCore-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkIOGeometry-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkIOCore-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonExecutionModel-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonDataModel-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonMisc-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonSystem-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonTransforms-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonMath-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkCommonCore-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtksys-5.11.a 
#BL_MPI_LIBS += $(VTK)/lib/libvtkzlib-5.11.a 
#BL_MPI_LIBS += -L/ccs/proj/e2e/habbasi/s3dinsitu/dataspaces/lib -ldspace
#BL_MPI_LIBS += ${PMI_LIBS} ${UGNI_LIBS} -L/opt/cray/rca/default/lib64 -lrca
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libparallelMT.a  
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libStreamingParser.a  
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libStreamingTopology.a  
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libStatistics.a  
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libTopologyFileParser.a
#BL_MPI_LIBS += $(TOPOLOGY)/lib/libFlexArray.a  

##### ----------------------------


include ./Make.package




include $(BOXLIB_HOME)/Src/C_BaseLib/Make.package
#include $(BOXLIB_HOME)/Src/C_AMRLib/Make.package
#include $(BOXLIB_HOME)/Src/C_BoundaryLib/Make.package


INCLUDE_LOCATIONS += $(BOXLIB_HOME)/Src/C_BaseLib
INCLUDE_LOCATIONS += $(BOXLIB_HOME)/Src/C_AMRLib
INCLUDE_LOCATIONS += $(BOXLIB_HOME)/Src/C_BoundaryLib #/usr/include/mpich2
#INCLUDE_LOCATIONS += ./statistics
#INCLUDE_LOCATIONS += ./topology
INCLUDE_LOCATIONS += ./libs3d/source
#INCLUDE_LOCATIONS += $(VTK)/include/VTK-5.11
#INCLUDE_LOCATIONS += $(TOPOLOGY)/include/
#INCLUDE_LOCATIONS +=  /ccs/proj/e2e/habbasi/s3dinsitu/dataspaces/include
INCLUDE_LOCATIONS += /opt/cray/rca/default/include
INCLUDE_LOCATIONS += /opt/cray-hss-devel/default/include    
INCLUDE_LOCATIONS += /opt/cray/krca/default/include
VPATH_LOCATIONS   += . $(BOXLIB_HOME)/Src/C_BaseLib
VPATH_LOCATIONS   += . $(BOXLIB_HOME)/Src/C_AMRLib
VPATH_LOCATIONS   += . $(BOXLIB_HOME)/Src/C_BoundaryLib
#VPATH_LOCATIONS   += ./statistics
#VPATH_LOCATIONS   += ./topology

#------visualzation--------------
#INCLUDE_LOCATIONS += ./visualization
#INCLUDE_LOCATIONS += ./visualization/hpgv
#VPATH_LOCATIONS   += ./visualization
#VPATH_LOCATIONS   += ./visualization/hpgv
#BL_MPI_LIBS       += ./visualization/hpgv/libhpgv.a
#--------------------------------


vpath %.c   $(VPATH_LOCATIONS)
vpath %.cpp $(VPATH_LOCATIONS)
vpath %.h   $(VPATH_LOCATIONS)
vpath %.H   $(VPATH_LOCATIONS)
vpath %.F   $(VPATH_LOCATIONS)
vpath %.f   $(VPATH_LOCATIONS)
vpath %.f90 $(VPATH_LOCATIONS)


all: $(executable)

$(executable):

#include $(BOXLIB_HOME)/Tools/C_mk/Make.rules
include Make.rules.pgi

