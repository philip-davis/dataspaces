DATASPACES_DIR=/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4
DATASPACES_INC=-I/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4/include
DATASPACES_LIB=-L/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4/lib -ldspaces -ldscommon -ldart -lrdmacm
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
