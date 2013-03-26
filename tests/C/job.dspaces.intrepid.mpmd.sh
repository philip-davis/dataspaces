#!/bin/sh

  echo "Starting Cobalt job script"
  # IMPORTANT - do not use plain "mpirun"
  cobalt-mpiexec -mode VN -verbose 2 : -n 512 -wdir . dataspaces_server -s 512 -c 1536 : -n 512 -wdir . test_writer : -n 1024 -wdir . test_reader
