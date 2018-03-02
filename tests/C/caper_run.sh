#!/bin/bash
DIR=.
CONF_DIMS=1024
NUM_SERVER=1
NUM_WRITER=32
NUM_READER=4

bash $DIR/cleanall.sh

#kill all previous dataspaces process
ps -aux | grep dataspaces| cut -c 9-15|xargs kill -9
ps -aux | grep test| cut -c 9-15|xargs kill -9

rm -f conf cred dataspaces.conf srv.lck

echo "## Config file for DataSpaces
ndim = 2
dims = $CONF_DIMS, $CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf


mpirun -machinefile host_server -n $NUM_SERVER $DIR/dataspaces_server -s $NUM_SERVER -c $(($NUM_WRITER+$NUM_READER)) & sleep 2
mpirun -machinefile host_client -n $NUM_WRITER $DIR/test_writer DATASPACES $NUM_WRITER 2 $NUM_WRITER 1 $(($CONF_DIMS/$NUM_WRITER)) $CONF_DIMS 3 1  &
mpirun -machinefile host_client -n $NUM_READER $DIR/test_reader DATASPACES $NUM_READER 2 $NUM_READER 1 $(($CONF_DIMS/$NUM_READER)) $CONF_DIMS 3 2  &

#mpirun -n 1 $DIR/dataspaces_server -s 1 -c 2 & sleep 2
#mpirun -n 1 $DIR/test_writer DATASPACES 1 2 1 1 $CONF_DIMS $CONF_DIMS 1 1  &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 $CONF_DIMS $CONF_DIMS 1 2  &

#time mpirun -machinefile hostfile.txt -n 2 $DIR/dataspaces_server -s 2 -c 16 & sleep 2
#mpirun -machinefile hostfile.txt -n 8 $DIR/test_writer DATASPACES 8 2 8 1 $CONF_DIMS/8 $CONF_DIMS 1 1  &
#mpirun -machinefile hostfile.txt -n 8 $DIR/test_reader DATASPACES 8 2 8 1 $CONF_DIMS/8 $CONF_DIMS 1 2  &
wait