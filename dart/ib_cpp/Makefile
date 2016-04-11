# For localmake
# Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com

MPICXX=mpicxx
MPICC=mpicc
# 
# GLOG_INC = -I$(GLOG_DIR)/include
# GLOG_LIB = -L$(GLOG_DIR)/lib -lglog

IBVERBS_LIB = -L/usr/lib64 -libverbs -lrdmacm
# IB_LIB = -lrdmacm -libverbs -lpthread

# IB_CPP_DIR=/cac/u01/mfa51/Desktop/dataspaces/dataspaces/dart/ib_cpp
# IB_CPP_INC=-I$(IB_CPP_DIR)/include -I$(IB_CPP_DIR)/.. -I$(IB_CPP_DIR)/../.. -I$(IB_CPP_DIR)/../../include
IB_CPP_INC=-I./include -I./.. -I./../.. -I./../../include
# 
INC = $(GLOG_INC) $(IB_CPP_INC)
LIB = $(GLOG_LIB) $(IBVERBS_LIB) -lstdc++ -lmpi_cxx

ODIR = obj
OBJS = $(ODIR)/exp.o $(ODIR)/ds_base_ib.o $(ODIR)/dc_base_ib.o $(ODIR)/dart_rpc_ib.o $(ODIR)/patch_ib_cpp.o

.PHONY: all lclean clean

APPS := exp

all: $(APPS)

exp: $(OBJS)
	$(MPICXX) $(CPP_OPTS) -o $@ $^ $(INC) $(LIB)

$(ODIR)/%.o: %.cpp
	$(MPICXX) $(CPP_OPTS) -c -o $@ $< $(INC) $(LIB)

$(ODIR)/%.o: %.c
	$(MPICC) $(CPP_OPTS) -c -o $@ $< $(INC) $(LIB)

lclean:
	rm -f $(ODIR)/*.o ${APPS} 

clean:
	rm -f obj/*.o ${APPS}