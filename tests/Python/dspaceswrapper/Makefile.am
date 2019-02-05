AUTOMAKE_OPTIONS = foreign

PYTHON_CONFIG = $(PYTHON) ../python-config
MPI4PY_INCLUDE = ${shell $(PYTHON) -c 'import mpi4py; print( mpi4py.get_include() )'}
NUMPY_INCLUDE = ${shell $(PYTHON) -c 'import numpy; print( numpy.get_include() )'}
PREINCLUDES = -I${abs_top_srcdir}/include
LFLAG = -L${abs_top_srcdir}/src -ldspaces -ldscommon -ldspacesf -L${abs_top_srcdir}/dart -ldart $(DSPACESLIB_LDADD)

SWIG = swig
SWIG_PY = ${SWIG} -python
.PHONY: src
src: dspaces_wrap.c
dspaces_wrap.c: dspaces.i
	[ -f ./numpy.i ] && echo "numpy.i already here, good" || curl -O https://raw.githubusercontent.com/numpy/numpy/master/tools/swig/numpy.i
	${SWIG_PY} -I${MPI4PY_INCLUDE} -I${NUMPY_INCLUDE} $(PREINCLUDES) $(LFLAG) -o $@ $<

MPICC = mpicc
CFLAGS = -fPIC ${shell ${PYTHON_CONFIG} --includes} -Wall -g
LDFLAGS = -shared ${shell ${PYTHON_CONFIG} --libs}
SO = ${shell ${PYTHON_CONFIG} --extension-suffix}

.PHONY: buildwrap
buildwrap: _dspaces${SO} 
_dspaces${SO}:  dspaces_wrap.c 
	${MPICC} ${CFLAGS} -I${MPI4PY_INCLUDE} -I${NUMPY_INCLUDE} $(PREINCLUDES) $(LFLAG) -o $@ $< ${LDFLAGS}

all: buildwrap

install-exec-local:
	cd .. && python3 setup.py install

clean-local:
	${RM} -r dspaces_wrap.c dspaces.py* _dspaces*.so *pycache*
