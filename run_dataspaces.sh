#!/bin/bash
#SBATCH -J JOB_NAME
#SBATCH -o JOB_NAME.%J.stdout
#SBATCH -e JOB_NAME.%J.stderr
#SBATCH -p debug
#SBATCH -N 1
#SBATCH -n 36
#SBATCH -t 00:01:00


DIR=.

#Configure DataSpaces input variables
CONF_DIMS_1=1024
CONF_DIMS_2=1024

NUM_SERVER=1
NUM_WRITER=2
NUM_READER=2



rm -f conf cred dataspaces.conf srv.lck


#Create the configure file
echo "## Config file for DataSpaces
ndim = 2
dims = $CONF_DIMS_1, $CONF_DIMS_2

max_versions = 1
lock_type = 2
" > dataspaces.conf


#Create the output file
echo "server = $NUM_SERVER"
echo $NUM_SERVER &>> server.log
echo $NUM_SERVER &>> writer.log
echo $NUM_SERVER &>> reader.log


#Initiate dataspaces_server
mpirun -n $NUM_SERVER -npernode $NUM_SERVER $DIR/dataspaces_server -s $NUM_SERVER -c $(($NUM_WRITER+$NUM_READER)) &>> server.log & sleep 2

#The sleep is necessary for launching server
sleep 10

#Initiate test_writer/reader
mpirun -n $NUM_WRITER $DIR/test_writer DATASPACES $NUM_WRITER 2 $NUM_WRITER 1 $(($CONF_DIMS_1/$NUM_WRITER)) $CONF_DIMS_2 2 1 &>> writer.log &
mpirun -n $NUM_READER $DIR/test_reader DATASPACES $NUM_READER 2 $NUM_READER 1 $(($CONF_DIMS_1/$NUM_READER)) $CONF_DIMS_2 2 2 &>> reader.log &

#Done
wait