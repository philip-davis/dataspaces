#!/bin/bash 

uri_file=abc
ip_addr=192.168.56.191
export DATASPACES_TCP_INTERFACE=enp0s8

rm conf srv.lck ${uri_file}

echo -n "Launching daos_server..."
orterun -np 1 --host ${ip_addr} --enable-recovery --report-uri ${uri_file} daos_server &
daos_pid=$!
echo "done."

while [ ! -f ${uri_file} ]; do
    sleep 1s
done

echo -n "Launching dataspaces_server..."
orterun -np 1 --ompi-server file:${uri_file} dataspaces_server -s 1 -c 2 &
ds_pid=$!
echo "done."

while [ ! -f conf ] ; do
    sleep 1s
done

echo "Running reader and writer..."
mpirun -np 1 --host 192.168.56.191 test_writer DATASPACES 1 3 1 1 1 128 128 128 10 1 &
mpirun -np 1 --host 192.168.56.191 test_reader DATASPACES 1 3 1 1 1 128 128 128 10 2  
echo "Done with dataspaces test. Waiting for dataspaces_server to finish..."
while [ -e /proc/${ds_pid} ] ; do
    sleep 1s
done
echo "Okay, dataspaces_server is done. Killing daos_server."
kill ${daos_pid}
echo "All done."
