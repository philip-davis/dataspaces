ifndef OPT
    OPT = OFF
endif

ifndef DEBUG
    DEBUG = ON
endif

ifndef SYSTEM
    SYSTEM = JAGUARPF_BOX
endif

ifeq ($(SYSTEM), SANDIA)
    MPI = $(HOME)/local/lib64/mpich2
    INCLUDE =
    LIB = 
    LDFLAGS = -lm 
    CC = $(MPI)/bin/mpicc
    CFLAGS += -Wall
endif

ifeq ($(SYSTEM), LENS)
    CC = mpicc	
    CFLAGS += -DSYS_LENS
    CFLAGS += -Wall
endif

ifeq ($(SYSTEM), JAGUARPF)
    CC = cc	
    CFLAGS += -DSYS_JAGUAR
endif

ifeq ($(SYSTEM), JAGUARPF_BOX)
    CC = CC	
    CFLAGS += -DSYS_JAGUAR -DMPICH_SKIP_MPICXX -DMPICH_IGNORE_CXX_SEEK
endif


ifeq ($(SYSTEM), MAGRITTE)
    CC = mpicc	
    CFLAGS += -DSYS_MAGRITTE
    CFLAGS += -Wall
endif

ifeq ($(SYSTEM), SITH)
    CC = mpicc
endif
	
ifeq ($(OPT), ON)    
    CFLAGS += -O3
endif

ifeq ($(DEBUG), ON)
    CFLAGS += -g #-pg
endif



