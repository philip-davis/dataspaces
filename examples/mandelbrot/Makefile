DATASPACES_DIR=/home/user/software/dataspaces-1.5.0/bin
DATASPACES_INC=-I/home/user/software/dataspaces-1.5.0/include
DATASPACES_LIB=-L/home/user/software/dataspaces-1.5.0/lib -ldspaces -ldscommon -ldart -lrdmacm -libverbs -lm
CC=mpicc

DEPS=bitmap.h bitmap.c master.c worker.c
OBJ=bitmap.o master.o worker.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(DATASPACES_INC) $(DATASPACES_LIB)

all: master worker

master: bitmap.o master.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)


worker: bitmap.o worker.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)

.PHONY: clean

clean:
	rm -f *.o master worker 
