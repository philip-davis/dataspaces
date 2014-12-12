DATASPACES_DIR=/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4
DATASPACES_INC=-I/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4/include
DATASPACES_LIB=-L/sw/redhat6/dataspaces/1.3.0/rhel6_pgi13.4/lib -ldspaces -ldscommon -ldart -lrdmacm
CC=mpicc

DEPS=testw.c testr.c generate_array.c
OBJ=testw.o testr.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(DATASPACES_INC) $(DATASPACES_LIB)

all: generate_array testw testr

generate_array: generate_array.c
	$(CC) -o $@ $^
testw: testw.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)


testr: testr.o
	$(CC) -o $@ $^ $(DATASPACES_INC) $(DATASPACES_LIB)

.PHONY: clean

clean:
	rm -f *.o testw testr generate_array 
