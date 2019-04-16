#!/bin/bash 

module load python/3.6.3
module unload openmpi/1.10.1
module load mpich
make
