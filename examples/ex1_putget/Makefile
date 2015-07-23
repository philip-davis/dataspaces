DATASPACES_DIR=/home/user/software/dataspaces-1.5.0/bin
DATASPACES_INC=-I/home/user/software/dataspaces-1.5.0/include
DATASPACES_LIB=-L/home/user/software/dataspaces-1.5.0/lib -ldspaces -ldscommon -ldart -lrdmacm -libverbs -lm
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

