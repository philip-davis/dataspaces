#!/bin/sh

  echo "Starting Cobalt job script"
  # IMPORTANT - do not use plain "mpirun"
  cobalt-mpiexec -mode VN -verbose 2 : -n 256 -wdir . scheduler_driver -s 256 -c 768 : -n 256 -wdir . simulation_driver 256 256 1 1 1 256 256 50 : -n 256 -wdir . insitu_staging_driver : -n 256 -wdir . intransit_staging_driver
