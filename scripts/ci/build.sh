#!/bin/bash

BUILD_SYS=$1

if [ "$1" == "cmake" ] ; then
    mkdir build
    cd build
    cmake .. -DCMAKE_C_COMPILER=mpicc
    make
    make install
else
    ./autogen.sh
    ./configure CC=mpicc FC=mpifort
    make
    make install
fi
