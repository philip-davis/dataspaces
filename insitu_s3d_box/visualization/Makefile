include systemdef.mk

TARGET = libs3d_box_viz_intransit.a
OBJ_DIR = .

OBJ_DIR = .

CFILES = s3d_box_viz.cpp

HFILES = s3d_box_viz.h

INCLUDE  = -I./hpgv
INCLUDE += -I../
INCLUDE += -I../../BoxLib/Src/C_BaseLib
INCLUDE += -I/ccs/proj/e2e/habbasi/s3dinsitu/dataspaces/include

#CFLAGS += -DMPICH_IGNORE_CXX_SEEK -DNDEBUG -DBL_USE_MPI -DMPICH_SKIP_MPICXX -DBL_SPACEDIM=3 -DBL_FORT_USE_UNDERSCORE -DBL_PGI

CFLAGS += -DS3D_BOX_INTRANSIT

OBJECT_FILES = $(patsubst %.cpp,$(OBJ_DIR)/%.o,$(CFILES))

all: $(OBJECT_FILES) 
	ar rcs $(TARGET) $(OBJECT_FILES)
	ranlib $(TARGET)
	
#.depend: $(CFLIES)
#	$(CC) -MM -E $(INCLUDE) $(CFILES) > .depend	
	
$(OBJ_DIR)/%.o: %.cpp
	$(CC) -c $(CFLAGS) -I./ $(INCLUDE) -o $@ $<

clean:
	rm -f core.* $(OBJ_DIR)/*.o $(TARGET) .depend 
	
#include .depend
