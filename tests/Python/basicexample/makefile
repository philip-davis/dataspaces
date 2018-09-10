.PHONY: default
default: build example clean

PYTHON = python
PYTHON_CONFIG = ${PYTHON} ../python-config
MPI4PY_INCLUDE = ${shell ${PYTHON} -c 'import mpi4py; print( mpi4py.get_include() )'}


SWIG = swig
SWIG_PY = ${SWIG} -python
.PHONY: src
src: example_wrap.c
example_wrap.c: example.i
	${SWIG_PY} -I${MPI4PY_INCLUDE} -o $@ $<

MPICC = mpicc
CFLAGS = -fPIC ${shell ${PYTHON_CONFIG} --includes}
LDFLAGS = -shared ${shell ${PYTHON_CONFIG} --libs}
SO = ${shell ${PYTHON_CONFIG} --extension-suffix}
.PHONY: build
build: _example${SO}
_example${SO}: example_wrap.c
	${MPICC} ${CFLAGS} -I${MPI4PY_INCLUDE} -o $@ $< ${LDFLAGS}


MPIEXEC = mpiexec
NP_FLAG = -n
NP = 5
.PHONY: example
example: build
	${MPIEXEC} ${NP_FLAG} ${NP} ${PYTHON} testexample.py


.PHONY: clean
clean:
	${RM} example_wrap.c example.py* _example${SO} __pycache__