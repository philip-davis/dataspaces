dnl @synopsis AC_PYTHON_MODULE(modname[, fatal])
dnl
dnl Checks for Python 2 module.
dnl
dnl If fatal is non-empty then absence of a module will trigger an
dnl error.
dnl
dnl @category InstalledPackages
dnl @author Andrew Collier <colliera@nu.ac.za>.
dnl @version 2014-09-08
dnl @license AllPermissive
AC_DEFUN([AC_PYTHON_MODULE],[
    AC_MSG_CHECKING(python module: $1)
    ${PYTHON} -c "import $1" 2>/dev/null
    if test $? -eq 0;
    then
        AC_MSG_RESULT(yes)
        eval AS_TR_CPP(HAVE_PYMOD_$1)=yes
    else
        AC_MSG_RESULT(no)
        eval AS_TR_CPP(HAVE_PYMOD_$1)=no
        #
        if test -n "$2"
        then
            AC_MSG_ERROR(failed to find required module $1)
            exit 1
        fi
    fi
])

AC_DEFUN([AC_PYTHON_WRAPPER],[
    AM_CONDITIONAL(BUILD_PYTHON_WRAPPER, false)
    AC_ARG_ENABLE(python-bindings,
    [AS_HELP_STRING([--enable-python-bindings],
        [Enable building python bindings for DataSpaces])])
    if test "x${enable_python_bindings}" == "xyes"; then
        AM_PATH_PYTHON([3.0])
        AX_PKG_SWIG([2.0.10]) 
        dnl AM_CONDITIONAL([HAVE_PYTHON], [test "$PYTHON" != :])
        if test -n "${PYTHON}"
        then    
            AC_PYTHON_MODULE([numpy],yes)
            AC_PYTHON_MODULE([mpi4py],yes)
            AM_CONDITIONAL(BUILD_PYTHON_WRAPPER, true)
        fi
    fi
])
