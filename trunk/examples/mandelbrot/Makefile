DATASPACES_DIR=/home/user/software/dataspaces-1.5.0/bin
DATASPACES_INC=-I/home/user/software/dataspaces-1.5.0/include
DATASPACES_LIB=-L/home/user/software/dataspaces-1.5.0/lib -ldspaces -ldscommon -ldart -lrdmacm -libverbs -lm
CC=mpicc

DEPS=bitmap.h bitmap.c testw.c testr.c
OBJ=bitmap.o testw.o testr.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(DATASPACES_INC) $(DATASPACES_LIB)

all: testw testr

testw: bitmap.o testw.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)


testr: bitmap.o testr.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)

.PHONY: clean

clean:
	rm -f *.o testw testr 
