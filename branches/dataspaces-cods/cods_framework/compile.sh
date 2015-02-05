#!/bin/bash

make -f Makefile-lib-titan clean
make -f Makefile-test-titan clean
make -f Makefile-lib-titan
make -f Makefile-test-titan
