#!/bin/sh
#SBATCH -J DS_example
#SBATCH -o DS_example.%J.stdout
#SBATCH -e DS_example.%J.stderr
#SBATCH -p debug
#SBATCH -N 1
#SBATCH -n 36
#SBATCH -t 00:01:00
#SBATCH --mail-type=END
#SBATCH --mail-user=qybo123@gmail.com
#DIR=./data_normal
#DIR=./data_pthread
DIR=.
CONF_DIMS_1=2048
CONF_DIMS_2=2048
NUM_SERVER=1
NUM_WRITER=32
NUM_READER=1


rm -f conf cred dataspaces.conf srv.lck

echo "## Config file for DataSpaces
ndim = 2
dims = $CONF_DIMS_1, $CONF_DIMS_2

max_versions = 1
lock_type = 2
" > dataspaces.conf


mpirun -n $NUM_SERVER $DIR/dataspaces_server -s $NUM_SERVER -c $(($NUM_WRITER+$NUM_READER)) >& $DIR/server_$CONF_DIMS_1.log & sleep 2
mpirun -n $NUM_WRITER $DIR/test_writer DATASPACES $NUM_WRITER 2 $NUM_WRITER 1 $(($CONF_DIMS_1/$NUM_WRITER)) $CONF_DIMS_2 3 1 >& $DIR/writer_$CONF_DIMS_1.log &
mpirun -n $NUM_READER $DIR/test_reader DATASPACES $NUM_READER 2 $NUM_READER 1 $(($CONF_DIMS_1/$NUM_READER)) $CONF_DIMS_2 3 2 >& $DIR/reader_$CONF_DIMS_1.log &
wait
