#!/bin/bash

# assume ./dataspaces_server -s 1 -c 2 have already started

PYTHON=python
MPIEXEC=mpiexec
NP_FLAG=-n
NP=1


# update the configuration files and restart server if test the 1d case

${MPIEXEC} ${NP_FLAG} ${NP} ${PYTHON} ./testput1d.py
${MPIEXEC} ${NP_FLAG} ${NP} ${PYTHON} ./testget1d.py


# update the configuration files and restart server if test the 2d case

# ${MPIEXEC} ${NP_FLAG} ${NP} ${PYTHON} ./testput2d.py
# ${MPIEXEC} ${NP_FLAG} ${NP} ${PYTHON} ./testget2d.py
