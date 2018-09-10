./Python/example shows a basic case to combine python and c code based on swig and mpi4py
./Python shows an example to wrap put.c and get.c by python

(This python wrapper is only tested on Caliburn with dependency: python/3.6.3 mpi4py/3.0.0 numpy)

following steps should be finished if you want to add python wrapper to call the DataSpaces clients:

1> add -fPIC flag at AM_CPPFLAGS in following .am file: ./test/C/Makefile.am ./src/Makefile.am ./dart/Makefile.am

2> use `./dspaces_config -l` at dataspaces dir to get the link path, and give the value to `LFLAG` in `./test/Python/dspaceswrapper/Makefile.am`

3> add the `tests/Python/dspaceswrapper/Makefile` in `AC_CONFIG_FILES` of `configure.ac` in dataspaces folder

4> load softwares:

```
module unload openmpi/1.10.1
module load mpich
module load python/3.6.3
```

enter python virtual env and install the mpi4py, for example:

```
cd ~/testPython3/
source ./bin/activate
# if mpi4py is not installed, then
pip install mpi4py
```

5> set configurations of DataSpaces

```
echo "## Config file for DataSpaces
ndim = 1
dims = 128

max_versions = 10
lock_type = 1
" > dataspaces.conf
```

6>  run dataspace server in `dataspaces/tests/C`:

```
./dataspaces_server -c 2 -s 1
```

7> run test case in `./test/Python/dspaceswrapper`:

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