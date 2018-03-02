#!/bin/bash
#SBATCH -J DS_example
#SBATCH -o DS_example.%J.stdout
#SBATCH -e DS_example.%J.stderr
#SBATCH -p development
#SBATCH -N 4
#SBATCH -n 72
#SBATCH -t 00:03:00
#SBATCH --mail-type=BEGIN
#SBATCH --mail-user=qybo123@gmail.com


rm -f hostfile*
rm -f conf* srv.lck* 
rm -f dataspaces.conf

#nodes=(`sort $PBS_NODEFILE | uniq`)
#nodes=(`cat $PBS_NODEFILE | uniq`)
nodes="$(hostname)"

num_nodes_server=1
idx=0
# Put first $num_nodes_server to hostfile-server
for ((i=0; i < $num_nodes_server; i++))
do
    echo "$nodes slots=24 max_slots=24" >> hostfile-server
    let "idx=idx+1"
done

num_nodes_app1=2
num_nodes_app2=1
# Put the first $num_nodes_app1 nodes to hostfile-app1
for ((i=0; i < $num_nodes_app1; i++))
do
    echo "$nodes slots=24 max_slots=24" >> hostfile-app1
    let "idx=idx+1"
done

# Put the next $num_nodes_app2 nodes to hostfile-app2
for ((i=0; i < $num_nodes_app2; i++))
do
    echo "$nodes slots=24 max_slots=24" >> hostfile-app2
    let "idx=idx+1"
done

# Write dataspaces config file
echo "## Config file for DataSpaces
ndim = 2
dims = 8192, 8192
 
max_versions = 1
lock_type = 2
hash_version = 2" > dataspaces.conf

# Start dataspaces servers
#mpirun --hostfile hostfile-server -npernode 2 -n 2 dataspaces_server -s 2 -c 48 >& server.log &

# Start testing applications
#mpirun --hostfile hostfile-app1 -npernode 16 -n 32 test_writer DATASPACES 32 2 8 4 1024 2048 1 1 >& writer.log &
#mpirun --hostfile hostfile-app2 -npernode 16 -n 16 test_reader DATASPACES 16 2 4 4 2048 2048 1 2 >& reader.log

mpirun -n 2 -npernode 2 dataspaces_server -s 2 -c 48 >& server.log & sleep 2

# Start testing applications
mpirun -n 32 -npernode 16 test_writer DATASPACES 32 2 8 4 1024 2048 10 1 >& writer.log &
mpirun -n 16 -npernode 16 test_reader DATASPACES 16 2 4 4 2048 2048 10 2 >& reader.log &

wait
