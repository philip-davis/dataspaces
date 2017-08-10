in-SSD DataSpaces version based on dataspaces as service 

SSD file setting in src/mem_persist.c

/* using 64M ssd file for this example */
#define PMEM_SIZE 64*1024*1024L

/* using /ccs/home/sd904/ ssd file path for this example */
#define PMEM_PATH "/ccs/home/sd904/"


Add/update file list:

tests/C/common.c
src/ds_gspace.c
src/Makefile.AM
src/mem_persist.c
src/ss_data.c
include/mem_persist.h
include/ss_data.h
