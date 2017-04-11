#!/bin/bash
#    Begin SBATCH directives
#SBATCH -p debug
#SBATCH -N 3
#SBATCH -C haswell
#SBATCH -J dataspaces
#SBATCH -t 00:05:00
#SBATCH -V
#DW jobdw capacity=10GB access_mode=striped type=scratch
#    End SBATCH directives and begin shell commands

#cd $SBATCH_O_WORKDIR
## Clean up
rm -f conf *.log
rm -f dataspaces.conf
rm -f cred

echo $DW_JOB_PRIVATE

## Create dataspaces configuration file
echo "## Config file for DataSpaces
ndim = 3
dims = 256,256,256
max_versions = 20
max_readers = 1
lock_type = 2
" > dataspaces.conf
date
pwd
srun -N 1 -n 1 -c 16 --cpu_bind=cores dataspaces_server -s1 -c2 > server.log &

while [ ! -f conf ]; do
	sleep 1s
done
sleep 5s
srun -N 1 -n 1 -c 16 --cpu_bind=cores test_writer DATASPACES 1 3 1 1 1 128 128 128 10 1 &
sleep 2s
## Run reader application
srun -N 1 -n 1 --cpu_bind=cores test_reader DATASPACES 1 3 1 1 1 128 128 128 10 2 &

## Wait for the entire workflow to finish
wait
echo "ls -l $DW_JOB_STRIPED"
ls -l  $DW_JOB_STRIPED
