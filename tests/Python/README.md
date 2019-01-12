./Python/basicexample shows a basic case to combine python and c code based on swig and mpi4py
./Python/dspaceswrapper shows an example to get and put data from the DataSpaces project

(This python wrapper is only tested on Caliburn with dependency: python/3.6.3 mpi4py/3.0.0 numpy)

following steps should be finished if you want to add python wrapper to call the DataSpaces clients:

1> add -fPIC flag at AM_CPPFLAGS in following .am file: ./test/C/Makefile.am ./src/Makefile.am ./dart/Makefile.am

2> load depedency:

```
# it is also ok to use openmpi
module load mpich
module load python/3.6.3
```
Please remove `~/.cache/pip` and `*.so` in dspaceswrapper and reinstall mpi4py if you switch from mpich to openmpi.

We use python virtual env as an example but the following is also suitable for other python enviroment
enter python virtual env and install the mpi4py, for example:

```
# activate python virtual env
cd ~/pythonWorkSpace/nbodysimulation/
source ./bin/activate
# if mpi4py is not installed, then
pip install mpi4py
# please reinstall the mpi4py if you change the mpi implementation
```

3> add `-enable-python-bindings` for configurataion, for example:

```
./configure CC=mpicc FC=mpif90 CFLAGS=-DDEBUG -enable-python-bindings --prefix=/home1/zw241/dataspaces/install/
```

4> execute `make` and `make install`

5> set configurations of DataSpaces in dataspaces/tests/C

```
# For 1d test case:
echo "## Config file for DataSpaces
ndim = 1
dims = 128

max_versions = 10
lock_type = 1
" > dataspaces.conf


# For 2d test case:
echo "## Config file for DataSpaces
ndim = 2
dims = 128, 128

max_versions = 10
lock_type = 1
" > dataspaces.conf
```

6>  run dataspace server in `dataspaces/tests/C`:

```
./dataspaces_server -c 2 -s 1
```

7> run test case in `./test/Python/dspaceswrapper` or other folder you created:

```
/bin/bash testrun.sh
```

Attention:

the python depedency is acquired automatically by python-config, you could check the commands here to make sure the every thing is right.

For Caliburn cluster, it is compiled ok to uncomment line 62 in python-config

For other system, it is compiled ok to modify makefile for LDFLAGS variable

```
LDFLAGS = -shared ${shell ${PYTHON_CONFIG} --ldflags --libs}
```