#!/bin/sh
#SBATCH -J DS_example
#SBATCH -o DS_example.%J.stdout
#SBATCH -e DS_example.%J.stderr
#SBATCH -p development
#SBATCH -N 4
##SBATCH -n 8
#SBATCH --ntasks-per-node=2
#SBATCH -t 00:03:00
#SBATCH --mail-type=END
#SBATCH --mail-user=qybo123@gmail.com
#DIR=./data_normal
#DIR=./data_pthread
DIR=.
CONF_DIMS=8192

rm -f conf cred dataspaces.conf srv.lck

echo "## Config file for DataSpaces
ndim = 2
dims = $CONF_DIMS, $CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf
#./dataspaces_server -s 4 -c 72 & sleep 2

#./test_writer DATASPACES 64 3 4 4 4 256 256 256 2 1 > test_writer.out 2>&1 & 
#./test_reader DATASPACES 8 3 4 2 1 256 512 1024 2 2 > test_reader.out 2>&1 &

mpirun -n 2 $DIR/dataspaces_server -s 2 -c 5 >&$DIR/server_$CONF_DIMS.log & sleep 2

#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 4096 16384 2 1 -d > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 16384 16384 2 2 -d > $DIR/reader_$CONF_DIMS.log 2>&1 &
#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 1024 4096 5 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 4096 4096 5 2 > $DIR/reader_$CONF_DIMS.log 2>&1 &
#mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 512 2048 5 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
#mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 2048 2048 5 2 > $DIR/reader_$CONF_DIMS.log 2>&1 &
mpirun -n 4 $DIR/test_writer DATASPACES 4 2 4 1 2046 8192 2 1 > $DIR/writer_$CONF_DIMS.log 2>&1 &
mpirun -n 1 $DIR/test_reader DATASPACES 1 2 1 1 8192 8192 2 2 > $DIR/reader_$CONF_DIMS.log 2>&1 &
wait
