DATASPACES_DIR=/cac/u01/hdb20/usr/bin
DATASPACES_INC=-I/cac/u01/hdb20/usr/include
DATASPACES_LIB=-L/cac/u01/hdb20/usr/lib -ldspaces -ldscommon -ldart -lrdmacm
CC=mpicc

DEPS=put.c get.c
OBJ=put.o get.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(DATASPACES_INC) $(DATASPACES_LIB)


all: put get

put: put.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)


get: get.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)

.PHONY: clean

clean:
	rm -f *.o put get

