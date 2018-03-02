#!/bin/bash
rm -f conf* srv.lck* 
rm -f dataspaces.conf


echo "## Config file for DataSpaces
ndim = 2
dims = 8192,8192
 
max_versions = 1
lock_type = 2
hash_version = 2" > dataspaces.conf

# Start dataspaces servers
mpirun --hostfile hostfile-server -npernode 2 -n 2 dataspaces_server -s 2 -c 48 >& server.log &

# Start testing applications
mpirun --hostfile hostfile-app1 -npernode 16 -n 32 test_writer DATASPACES 32 2 8 4 1024 2048 1 1 >& writer.log &
mpirun --hostfile hostfile-app2 -npernode 16 -n 16 test_reader DATASPACES 16 2 4 4 2048 2048 1 2 >& reader.log
#mpirun -n 2 dataspaces_server -s 2 -c 48 >& server.log &

# Start testing applications
#mpirun -n 32 test_writer DATASPACES 32 2 8 4 1024 2048 1 1 >& writer.log &
#mpirun -n 16 test_reader DATASPACES 16 2 4 4 2048 2048 1 2 >& reader.log

#wait
