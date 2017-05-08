##################################
#ViSUS Visualization Project                    
# Copyright (c) 2010 University of Utah          
# Scientific Computing and Imaging Institute     
# 72 S Central Campus Drive, Room 3750           
# Salt Lake City, UT 84112                       
#                                                
#For information about this project see:        
#http://www.pascucci.org/visus/                 
#                                                
#      or contact: pascucci@sci.utah.edu         
###################################

PIDX_RST_OBJS = PIDX_rst.o
PIDX_RST_HDRS = PIDX_rst.h

MPICC = mpicc
MPI_LDFLAGS_PIDX_NEW = -L. -lPIDX_rst -lm
MPI_CFLAGS = -Wall -g

all: libPIDX_rst.a  PIDX_rst.o rst-phase

$(PIDX_RST_OBJS): %.o: %.c $(PIDX_RST_HDRS)
	$(MPICC) $(MPI_CFLAGS) $< -c  -o $@


libPIDX_rst.a: $(PIDX_RST_OBJS) PIDX_rst.o PIDX_rst.h PIDX_rst.c PIDX_data_structs.h
	ar rcs $@ $(PIDX_RST_OBJS) PIDX_rst.o

rst-phase: rst-phase.c PIDX_rst.h PIDX_rst.c libPIDX_rst.a PIDX_rst.o
	$(MPICC) $(MPI_CFLAGS) $< -o $@ $(MPI_LDFLAGS_PIDX_NEW) $(PIDX_LIBS_NEW)

clean::
	rm -f PIDX_rst.o rst-phase libPIDX_rst.a *.o
