AC_INIT([dataspaces-codemerge-template],[1.2.0])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_PROG_RANLIB

dnl AC_CRAY_PMI
dnl AC_CRAY_UGNI
AC_PORTALS
AC_INFINIBAND
AC_DCMF
AC_UGNI
AC_GNI_PTAG

dnl Generate flags for dataspaces lib creation which depends on the particular network transport layer. DSPACESLIB_* is used for compiling the lib, and linking testing codes.
DSPACESLIB_CFLAGS=
DSPACESLIB_CPPFLAGS=
DSPACESLIB_LDFLAGS=
DSPACESLIB_LDADD=

dnl Check if DIMES is enabled
AC_ARG_ENABLE(dimes,
    [AS_HELP_STRING([--enable-dimes],
        [Include the dimes mechanism. By default this is not included])])
AM_CONDITIONAL([DIMES], [test "x$enable_dimes" == "xyes"])

if test "x$enable_dimes" == "xyes"; then
        if test -z "${HAVE_UGNI_TRUE}"; then
                DSPACESLIB_CFLAGS+="-DDS_HAVE_DIMES"
		DSPACESLIB_CPPFLAGS+="-DDS_HAVE_DIMES"
		echo "DIMES enabled!"
        elif test -z "${HAVE_DCMF_TRUE}"; then
                DSPACESLIB_CFLAGS+="-DDS_HAVE_DIMES"
		DSPACESLIB_CPPFLAGS+="-DDS_HAVE_DIMES"
		echo "DIMES enabled!"
        else
                echo "DIMES only supported on Cray UGNI, IBM DCMF"
        fi
fi

dnl if test -z "${HAVE_CRAY_PMI_TRUE}";  then
dnl   DSPACESLIB_CPPFLAGS+=" ${CRAY_PMI_CPPFLAGS}"
dnl   DSPACESLIB_CFLAGS+=""
dnl   DSPACESLIB_LDFLAGS+=" ${CRAY_PMI_LDFLAGS}"
dnl   DSPACESLIB_LDADD+=" ${CRAY_PMI_LIBS}"
dnl fi

dnl if test -z "${HAVE_CRAY_UGNI_TRUE}"; then
dnl   DSPACESLIB_CPPFLAGS+=" ${CRAY_UGNI_CPPFLAGS}"
dnl   DSPACESLIB_CFLAGS+=""
dnl   DSPACESLIB_LDFLAGS+=" ${CRAY_UGNI_LDFLAGS}"
dnl   DSPACESLIB_LDADD+=" ${CRAY_UGNI_LIBS}"
dnl fi
if test -z "${HAVE_UGNI_TRUE}"; then
  DSPACESLIB_CPPFLAGS+=" ${PMI_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=""
  DSPACESLIB_LDFLAGS+=" ${PMI_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${PMI_LIBS}"
  DSPACESLIB_CPPFLAGS+=" ${UGNI_CPPFLAGS}"
  DSPACESLIB_CFLAGS+=""
  DSPACESLIB_LDFLAGS+=" ${UGNI_LDFLAGS}"
  DSPACESLIB_LDADD+=" ${UGNI_LIBS}"
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

AC_SUBST(DSPACESLIB_CFLAGS)
AC_SUBST(DSPACESLIB_CPPFLAGS)
AC_SUBST(DSPACESLIB_LDFLAGS)
AC_SUBST(DSPACESLIB_LDADD)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([
		Makefile
		dart/Makefile
		src/Makefile
		tests/Makefile
		tests/C/Makefile
		])

AC_OUTPUT


echo "Network transport layer"
dnl if test -z "${HAVE_CRAY_UGNI_TRUE}"; then
dnl   echo "Cray UGNI enabled.";
dnl   echo "	- CRAY_PMI_CPPFLAGS = $CRAY_PMI_CPPFLAGS";
dnl   echo "	- CRAY_PMI_LDFLAGS = $CRAY_PMI_LDFLAGS";
dnl   echo "	- CRAY_PMI_LIBS = $CRAY_PMI_LIBS";
dnl   echo "	- CRAY_UGNI_CPPFLAGS = $CRAY_UGNI_CPPFLAGS";
dnl   echo "	- CRAY_UGNI_LDFLAGS = $CRAY_UGNI_LDFLAGS";
dnl   echo "	- CRAY_UGNI_LIBS = $CRAY_UGNI_LIBS";
if test -z "${HAVE_UGNI_TRUE}"; then
  echo "Cray UGNI enabled.";
  echo "	- CRAY_PMI_CPPFLAGS = $PMI_CPPFLAGS";
  echo "	- CRAY_PMI_LDFLAGS = $PMI_LDFLAGS";
  echo "	- CRAY_PMI_LIBS = $PMI_LIBS";
  echo "	- CRAY_UGNI_CPPFLAGS = $UGNI_CPPFLAGS";
  echo "	- CRAY_UGNI_LDFLAGS = $UGNI_LDFLAGS";
  echo "	- CRAY_UGNI_LIBS = $UGNI_LIBS";
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
