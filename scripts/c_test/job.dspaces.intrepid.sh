#!/bin/sh

  echo "Starting Cobalt job script"
  # IMPORTANT - do not use plain "mpirun"
  cobalt-mpiexec -mode VN -verbose 2 : -n 512 -wdir . dataspaces_server -s 512 -c 1536 : -n 512 -wdir . test_writer 512 16 32 1 16 8 256 10 : -n 1024 -wdir . test_reader 1024 32 32 1 8 8 256 10
