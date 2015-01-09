DATASPACES_DIR=/cac/u01/mmr205/testComp/DS_install/bin
DATASPACES_INC=-I/cac/u01/mmr205/testComp/DS_install/include
DATASPACES_LIB=-L/cac/u01/mmr205/testComp/DS_install/lib -ldspaces -ldscommon -ldart -lrdmacm
CC=mpicc

DEPS=put_2d.c get_2d.c
OBJ=put_2d.o get_2d.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(DATASPACES_INC) $(DATASPACES_LIB)


all: put_2d get_2d

put_2d: put_2d.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)


get_2d: get_2d.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)

.PHONY: clean

clean:
	rm -f *.o put_2d get_2d

