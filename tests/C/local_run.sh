#!/bin/bash
DIR=.
CONF_DIMS_1=32768
CONF_DIMS_2=16384

NUM_SERVER=1
NUM_WRITER=64
NUM_READER=4

rm -f conf cred dataspaces.conf srv.lck
bash /cac/u01/yq47/Documents/ds_hybrid_mt/tests/C/cleanall.sh

echo "## Config file for DataSpaces
ndim = 2
dims = $CONF_DIMS_1, $CONF_DIMS_2

max_versions = 1
lock_type = 2
" > dataspaces.conf

#./dataspaces_server -s 4 -c 72 & sleep 2

#./test_writer DATASPACES 64 3 4 4 4 256 256 256 2 1 > test_writer.out 2>&1 &
#./test_reader DATASPACES 8 3 4 2 1 256 512 1024 2 2 > test_reader.out 2>&1 &

#mpirun -n 1 ./dataspaces_server -s 1 -c 5 >& $DIR/server_$CONF_DIMS.log & SERVER_PID=$! sleep 2
#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 $CONF_DIMS/4 $CONF_DIMS 3 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 $CONF_DIMS $CONF_DIMS 3 2 > $DIR/reader_$CONF_DIMS.log 2>&1 & READER_PID=$!
#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 512 2048 5 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 2048 2048 5 2 > $DIR/reader_$CONF_DIMS.log 2>&1 &
#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 256 1024 5 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 1024 1024 5 2 > $DIR/reader_$CONF_DIMS.log 2>&1 &

mpirun -machinefile host_server -n $NUM_SERVER $DIR/dataspaces_server -s $NUM_SERVER -c $(($NUM_WRITER+$NUM_READER)) >& $DIR/server_$CONF_DIMS_1.log & sleep 2
mpirun -machinefile host_client -n $NUM_WRITER $DIR/test_writer DATASPACES $NUM_WRITER 2 $NUM_WRITER 1 $(($CONF_DIMS_1/$NUM_WRITER)) $CONF_DIMS_2 3 1 >& $DIR/writer_$CONF_DIMS_1.log &
mpirun -machinefile host_client -n $NUM_READER $DIR/test_reader DATASPACES $NUM_READER 2 $NUM_READER 1 $(($CONF_DIMS_1/$NUM_READER)) $CONF_DIMS_2 3 2 >& $DIR/reader_$CONF_DIMS_1.log &

#time mpirun -machinefile hostfile.txt -n 2 $DIR/dataspaces_server -s 2 -c 16 >& $DIR/server_$CONF_DIMS.log & SERVER_PID=$!  sleep 2
#mpirun -machinefile hostfile.txt -n 8 $DIR/test_writer DATASPACES 8 2 8 1 $CONF_DIMS/8 $CONF_DIMS 5 1  > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -machinefile hostfile.txt -n 8 $DIR/test_reader DATASPACES 8 2 8 1 $CONF_DIMS/8 $CONF_DIMS 5 2  > $DIR/reader_$CONF_DIMS.log 2>&1 & READER_PID=$!

wait 