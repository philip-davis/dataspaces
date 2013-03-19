#!/bin/sh

  echo "Starting Cobalt job script"
  # IMPORTANT - do not use plain "mpirun"
  cobalt-mpirun -mode VN -np 512 -verbose 2 test_coupling
