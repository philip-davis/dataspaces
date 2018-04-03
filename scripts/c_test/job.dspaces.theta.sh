#!/bin/bash
#COBALT -n 3
#COBALT -t 5
#COBALT -A YOUR_ALLOC_HERE
#COBALT -q debug-cache-quad

## For Theta, you must provide the GNI cookie via the env var DSPACES_GNI_COOKIE or by 
## compiling with the --with-gni-cookie configure flag

aprun -n 2 -p YOUR_PDOMAIN_ID ./dataspaces_server -s 2 -c 4 > server.log 2>&1 < /dev/null &

sleep 5

aprun -n 2 -p YOUR_PDOMAIN_ID ./test_writer DATASPACES 2 3 4 4 4 64 64 64 20 1 > writer.log 2>&1 < /dev/null &

aprun -n 2 -p YOUR_PDOMAIN_ID ./test_reader DATASPACES 2 3 2 4 4 64 64 64 20 2 > reader.log 2>&1 < /dev/null &

wait
