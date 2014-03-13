#!/bin/sh

  echo "Starting Cobalt job script"
  # IMPORTANT - do not use plain "mpirun"
  cobalt-mpiexec -mode VN -verbose 2 : -n 256 -wdir . dataspaces_server -s 256 -c 768 : -n 256 -wdir . test_writer DATASPACES 256 16 16 1 32 32 1 100 1 2 : -n 512 -wdir . test_reader DATASPACES 512 32 16 1 16 32 1 100 2 2
