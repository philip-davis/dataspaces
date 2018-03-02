AC_INIT([dataspaces],[1.6.5])
AC_CONFIG_AUX_DIR(config)

dnl AM_INIT_AUTOMAKE([-Wall -Wno-extra-portability -Werror subdir-objects foreign])
AM_INIT_AUTOMAKE([-Wall -Werror subdir-objects foreign])

AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_PROG_RANLIB
AM_PROG_CC_C_O

AC_FC_WRAPPERS

m4_ifdef([AM_PROG_AR],[AM_PROG_AR])

dnl AC_PROG_LIBTOOL
dnl AC_CRAY_PMI
dnl AC_CRAY_UGNI
AC_PORTALS
AC_INFINIBAND
AC_DCMF
AC_UGNI
AC_GNI_PTAG
AC_PAMI
AC_TCP_SOCKET

AC_CONFIGURE_OPTIONS

ACX_PTHREAD

save_LIBS=$LIBS
LIBS=""
AC_CHECK_LIB([m], [cos])
if test -n $LIBS; then
    MATH_LIBS=$LIBS
fi
LIBS=$save_LIBS

dnl Generate flags for dataspaces lib creation which depends on the particular network transport layer. DSPACESLIB_* is used for compiling the lib, and linking testing codes.
DSPACESLIB_CFLAGS="${PTHREAD_CFLAGS}"
DSPACESLIB_CPPFLAGS="${PTHREAD_CFLAGS}"
DSPACESLIB_LDFLAGS="${PTHREAD_CFLAGS}"
DSPACESLIB_LDADD="${PTHREAD_LIBS} ${MATH_LIBS}"
dnl These flags will be present in the output of dspaces_config
DSPACES_EXT_CFLAGS="${PTHREAD_CFLAGS}"
DSPACES_EXT_CPPFLAGS="${PTHREAD_CFLAGS}"
DSPACES_EXT_LDFLAGS="${PTHREAD_CFLAGS}"
DSPACES_EXT_LDADD="${PTHREAD_LIBS} ${MATH_LIBS}"
dnl configure input arguments
CONFIG_ARG="$ac_configure_args"

dnl Check if DART_TCP is enabled
AC_ARG_ENABLE(dart_tcp,
[AS_HELP_STRING([--enable-dart-tcp],
[Build DART over tcp socket.])])
if test "x$enable_dart_tcp" == "xyes"; then
  if test -z "${HAVE_UGNI_TRUE}"; then
    AM_CONDITIONAL(HAVE_UGNI, false)
    AC_DEFINE(HAVE_UGNI, 0, [])
  elif test -z "${HAVE_DCMF_TRUE}"; then
    AM_CONDITIONAL(HAVE_DCMF, false)
    AC_DEFINE(HAVE_DCMF, 0, [])
  elif test -z "${HAVE_PAMI_TRUE}"; then
    AM_CONDITIONAL(HAVE_PAMI, false)
    AC_DEFINE(HAVE_PAMI, 0, [])
  elif test -z "${HAVE_INFINIBAND_TRUE}"; then
    AM_CONDITIONAL(HAVE_INFINIBAND, false)
    AC_DEFINE(HAVE_INFINIBAND, 0, [])
  elif test -z "${HAVE_PORTALS_TRUE}"; then
    AM_CONDITIONAL(HAVE_PORTALS, false)
    AC_DEFINE(HAVE_PORTALS, 0, [])
  fi
else
  if test -z "${HAVE_UGNI_TRUE}"; then
    AM_CONDITIONAL(HAVE_TCP_SOCKET, false)
    AC_DEFINE(HAVE_TCP_SOCKET, 0, [])
  elif test -z "${HAVE_DCMF_TRUE}"; then
    AM_CONDITIONAL(HAVE_TCP_SOCKET, false)
    AC_DEFINE(HAVE_TCP_SOCKET, 0, [])
  elif test -z "${HAVE_PAMI_TRUE}"; then
    AM_CONDITIONAL(HAVE_TCP_SOCKET, false)
    AC_DEFINE(HAVE_TCP_SOCKET, 0, [])
  elif test -z "${HAVE_INFINIBAND_TRUE}"; then
    AM_CONDITIONAL(HAVE_TCP_SOCKET, false)
    AC_DEFINE(HAVE_TCP_SOCKET, 0, [])
  elif test -z "${HAVE_PORTALS_TRUE}"; then
    AM_CONDITIONAL(HAVE_TCP_SOCKET, false)
    AC_DEFINE(HAVE_TCP_SOCKET, 0, [])
  fi
fi

dnl uGNI and Infiniband are mutually exclusive. 
dnl This is probably a Cray machine that happens to have working verbs (Cori)
if test -z "${HAVE_UGNI_TRUE}" -a -z "${HAVE_INFINIBAND_TRUE}" ; then
    echo "Both Cray UGNI and Infiniband are available...choosing UGNI."
    AM_CONDITIONAL(HAVE_INFINIBAND, false)
    AC_DEFINE(HAVE_INFINIBAND, 0, [])
fi

dnl Check if DIMES is enabled
AC_ARG_ENABLE(dimes,
    [AS_HELP_STRING([--enable-dimes],
        [Include the dimes mechanism. By default this is not included])])
AM_CONDITIONAL([DIMES], [test "x$enable_dimes" == "xyes"])

if test "x$enable_dimes" == "xyes"; then
        if test -z "${HAVE_UGNI_TRUE}"; then
            AC_DEFINE(DS_HAVE_DIMES, 1, [DIMES is enabled])
            AM_CONDITIONAL(BUILD_DIMES, true)
            echo "DIMES enabled!"
        elif test -z "${HAVE_DCMF_TRUE}"; then
            AC_DEFINE(DS_HAVE_DIMES, 1, [DIMES is enabled])
            AM_CONDITIONAL(BUILD_DIMES, true)
            echo "DIMES enabled!"
        elif test -z "${HAVE_PAMI_TRUE}"; then
            AC_DEFINE(DS_HAVE_DIMES, 1, [DIMES is enabled])
            AM_CONDITIONAL(BUILD_DIMES, true)
            echo "DIMES enabled!"
        elif test -z "${HAVE_INFINIBAND_TRUE}"; then
            AC_DEFINE(DS_HAVE_DIMES, 1, [DIMES is enabled])
            AM_CONDITIONAL(BUILD_DIMES, true)
            echo "DIMES enabled!"
        else
            echo "DIMES only supported on Cray UGNI, IBM PAMI, IBM DCMF"
        fi
else
        AM_CONDITIONAL(BUILD_DIMES, false)
fi

dnl Check if DataSpaces location-aware write is enabled
AC_ARG_ENABLE(dataspaces_location_aware_write,
    [AS_HELP_STRING([--enable-dataspaces-location-aware-write],
        [Include experimental feature. DataSpaces client writes data to
         servers that running on local compute node. This feature is useful
         when co-locating the execution of DataSpaces servers with application
         on the same set of compute nodes. By default this is not included.])])
AM_CONDITIONAL([DATASPACES_LOCATION_AWARE_WRITE], [test "x$enable_dataspaces_location_aware_write" == "xyes"])

dnl Check the max array dimension

if test -z "${HAVE_UGNI_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${PMI_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=""
  DSPACESLIB_LDFLAGS+=" ${PMI_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${PMI_LIBS}"
  DSPACESLIB_CPPFLAGS+=" ${UGNI_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=""
  DSPACESLIB_LDFLAGS+=" ${UGNI_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${UGNI_LIBS}"
  if test -z "${HAVE_DRC_TRUE}" ; then
    DSPACESLIB_CPPFLAGS+=" ${DRC_CPPFLAGS}"
    DSPACES_EXT_CPPFLAGS+=" ${DRC_CPPFLAGS}"
    DSPACESLIB_LDADD+=" ${DRC_LIBS}"
    DSPACES_EXT_LDADD+=" ${DRC_LIBS}"
  fi
fi

if test -z "${HAVE_PORTALS_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${PORTALS_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=" ${PORTALS_CFLAGS}"
  DSPACESLIB_LDFLAGS+=" ${PORTALS_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${PORTALS_LIBS}"
fi

if test -z "${HAVE_INFINIBAND_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${INFINIBAND_CPPFLAGS}"
  DSPACESLIB_CFLAGS+="$ ${INFINIBAND_CFLAGS}"
  DSPACESLIB_LDFLAGS+=" ${INFINIBAND_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${INFINIBAND_LIBS} -lrdmacm"
fi

if test -z "${HAVE_DCMF_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${DCMF_CPPFLAGS} -DHAVE_DCMF"
  DSPACESLIB_CFLAGS+=" ${DCMF_CFLAGS} -DHAVE_DCMF"
  DSPACESLIB_LDFLAGS+=" ${DCMF_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${DCMF_LIBS}"
fi

if test -z "${HAVE_PAMI_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${PAMI_CPPFLAGS} -DHAVE_PAMI"
  DSPACESLIB_CFLAGS+=" ${PAMI_CFLAGS} -DHAVE_PAMI"
  DSPACESLIB_LDFLAGS+=" ${PAMI_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${PAMI_LIBS}"
fi

if test -z "${HAVE_TCP_SOCKET_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${TCP_SOCKET_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=" ${TCP_SOCKET_CFLAGS}"
  DSPACES_EXT_CFLAGS+=" ${TCP_SOCKET_CFLAGS}"
  DSPACESLIB_LDFLAGS+=" ${TCP_SOCKET_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${TCP_SOCKET_LIBS}"
  DSPACES_EXT_LDADD+=" ${TCP_SOCKET_LIBS}"

fi

AC_SUBST(DSPACESLIB_CFLAGS)
AC_SUBST(DSPACESLIB_CPPFLAGS)
AC_SUBST(DSPACESLIB_LDFLAGS)
AC_SUBST(DSPACESLIB_LDADD)
AC_SUBST(DSPACES_EXT_CFLAGS)
AC_SUBST(DSPACES_EXT_CPPFLAGS)
AC_SUBST(DSPACES_EXT_LDFLAGS)
AC_SUBST(DSPACES_EXT_LDADD)
AC_SUBST(CONFIG_ARG)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([
                Makefile
                dart/Makefile
                src/Makefile
                tests/Makefile
                tests/C/Makefile
                tests/Fortran/Makefile
                scripts/dspaces_config.makesrc
                ])

AC_OUTPUT


echo "Network transport layer"
if test -z "${HAVE_UGNI_TRUE}"; then
  echo "Cray UGNI enabled.";
  echo "    - CRAY_PMI_CPPFLAGS = $PMI_CPPFLAGS";
  echo "    - CRAY_PMI_LDFLAGS = $PMI_LDFLAGS";
  echo "    - CRAY_PMI_LIBS = $PMI_LIBS";
  echo "    - CRAY_UGNI_CPPFLAGS = $UGNI_CPPFLAGS";
  echo "    - CRAY_UGNI_LDFLAGS = $UGNI_LDFLAGS";
  echo "    - CRAY_UGNI_LIBS = $UGNI_LIBS";
  if test -z "${HAVE_ARIES_TRUE}" ; then
    echo "uGNI Aries version enabled."
  fi
  if test -z "${HAVE_DRC_TRUE}" ; then
    echo "    - CRAY_DRC_CPPFLAGS = $DRC_CPPFLAGS";
    echo "    - CRAY_DRC_LIBS = $DRC_LIBS";
  fi
else
  echo "No Cray UGNI."
fi

if test -z "${HAVE_PORTALS_TRUE}"; then
  echo "Cray Portals enabled.";
  echo "	- PORTALS_CFLAGS = $PORTALS_CFLAGS";
  echo "	- PORTALS_CPPFLAGS = $PORTALS_CPPFLAGS";
  echo "	- PORTALS_LDFLAGS = $PORTALS_LDFLAGS";
  echo "	- PORTALS_LIBS = $PORTALS_LIBS";
else
  echo "No Cray Portals."
fi

if test -z "${HAVE_INFINIBAND_TRUE}"; then
  echo "Infiniband available.";
  echo "	- INFINIBAND_CFLAGS = $INFINIBAND_CFLAGS";
  echo "	- INFINIBAND_CPPFLAGS = $INFINIBAND_CPPFLAGS";
  echo "	- INFINIBAND_LDFLAGS = $INFINIBAND_LDFLAGS";
  echo "	- INFINIBAND_LIBS = $INFINIBAND_LIBS";
else
  echo "No Infiniband."
fi

if test -z "${HAVE_DCMF_TRUE}"; then
  echo "IBM DCMF enabled.";
  echo "	- DCMF_CFLAGS = $DCMF_CFLAGS";
  echo "	- DCMF_CPPFLAGS = $DCMF_CPPFLAGS";
  echo "	- DCMF_LDFLAGS = $DCMF_LDFLAGS";
  echo "	- DCMF_LIBS = $DCMF_LIBS";
else
  echo "No IBM DCMF."
fi

if test -z "${HAVE_PAMI_TRUE}"; then
  echo "IBM PAMI enabled.";
  echo "        - PAMI_CFLAGS = $PAMI_CFLAGS";
  echo "        - PAMI_CPPFLAGS = $PAMI_CPPFLAGS";
  echo "        - PAMI_LDFLAGS = $PAMI_LDFLAGS";
  echo "        - PAMI_LIBS = $PAMI_LIBS";
else
  echo "No IBM PAMI."
fi

if test -z "${HAVE_TCP_SOCKET_TRUE}"; then
  echo "TCP socket DART is enabled.";
  echo "        - TCP_SOCKET_CFLAGS = $TCP_SOCKET_CFLAGS";
  echo "        - TCP_SOCKET_CPPFLAGS = $TCP_SOCKET_CPPFLAGS";
  echo "        - TCP_SOCKET_LDFLAGS = $TCP_SOCKET_LDFLAGS";
  echo "        - TCP_SOCKET_LIBS = $TCP_SOCKET_LIBS";
else
  echo "No TCP socket DART."
fi
