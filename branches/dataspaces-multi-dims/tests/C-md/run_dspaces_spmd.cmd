#!/bin/sh
#@ job_name = bgp_dataspace
#@ job_type = bluegene
#@ wall_clock_limit = 10:00
#@ bg_size = 128
#@ output = bgp_dataspace.out
#@ error = bgp_dataspace.err
#@ initialdir = /scratch/qiansun/test_nd
#@ environment = COPY_ALL;MP_TIMEOUT=1200;
#@ class = small
#@ queue
/bgsys/drivers/ppcfloor/bin/mpirun -mode VN ./test_space_spmd
