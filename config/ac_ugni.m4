dnl @synopsis AC_UGNI([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro tries to find out how to compile programs that
dnl use the Gemini API.
dnl
dnl
AC_DEFUN([AC_UGNI],[

AC_LANG_SAVE
AC_LANG_C

AM_CONDITIONAL(HAVE_UGNI,true)
have_ugni=yes
ac_ugni_hdr_ok=no
ac_ugni_lib_ok=no
ac_pmi_hdr_ok=no
ac_pmi_lib_ok=no

PMI_CPPFLAGS=""
PMI_LDFLAGS=""
PMI_LIBS=""
UGNI_CPPFLAGS=""
UGNI_LDFLAGS=""
UGNI_LIBS=""

AC_ARG_WITH(pmi-incdir,
	[AS_HELP_STRING([--with-pmi-incdir=DIR],
		[Path to Cray PMI include directory.])],
	[ac_pmi_hdr_ok=yes;
	 PMI_CPPFLAGS="-I$withval";])

AC_ARG_WITH(pmi-libdir,
 	[AS_HELP_STRING([--with-pmi-libdir=DIR],
		[Path to Cray PMI lib directory.])],
	[ac_pmi_lib_ok=yes;
	 PMI_LDFLAGS="-I$withval";])

AC_ARG_WITH(ugni-incdir,
	[AS_HELP_STRING([--with-ugni-incdir=DIR],
		[Path to Cray UGNI include directory.])],
	[ac_ugni_hdr_ok=yes;
	 UGNI_CPPFLAGS="-I$withval";])

AC_ARG_WITH(ugni-libdir,
	[AS_HELP_STRING([--with-ugni-libdir=DIR],
		[Path to Cray UGNI lib directory.])],
	[ac_ugni_lib_ok=no;
	 UGNI_LDFLAGS="-I$withval";])

AC_ARG_ENABLE([drc],
    AS_HELP_STRING([--enable-drc], [Enable Cray Dynamic RDMA Credential]))

PMI_LIBS="-lpmi"
UGNI_LIBS="-lugni"
save_CPPFLAGS="$CPPFLAGS"
save_LIBS="$LIBS"
save_LDFLAGS="$LDFLAGS"

if test x"$ac_pmi_hdr_ok" = xno; then
	PMI_CPPFLAGS=$(pkg-config cray-pmi --cflags 2>/dev/null)
fi

if test x"$ac_pmi_lib_ok" = xno; then
	PMI_LDFLAGS=$(pkg-config cray-pmi --libs-only-L 2>/dev/null)
fi

if test x"$ac_ugni_hdr_ok" = xno; then
	UGNI_CPPFLAGS=$(pkg-config cray-ugni --cflags 2>/dev/null)
fi

if test x"$ac_ugni_lib_ok" = xno; then
	UGNI_LDFLAGS=$(pkg-config cray-ugni --libs-only-L 2>/dev/null)
fi

if test "x$enable_drc" = "xyes" ; then 
    have_drc=yes
    DRC_CPPFLAGS=$(pkg-config cray-drc --cflags 2>/dev/null)
    DRC_LIBS=$(pkg-config cray-drc --libs 2>/dev/null)
else
    AM_CONDITIONAL(HAVE_DRC,false)
fi

CPPFLAGS="$CPPFLAGS $PMI_CPPFLAGS $UGNI_CPPFLAGS $DRC_CPPFLAGS"
LDFLAGS="$LDFLAGS $PMI_LDFLAGS $UGNI_LDFLAGS"
LIBS="$LIBS -lpmi -lugni $DRC_LIBS"

AC_CHECK_HEADERS(pmi.h, [:], [have_ugni=no])
AC_CHECK_HEADERS(gni_pub.h, [:], [have_ugni=no])

if test "x$enable_drc" = "xyes" ; then
    AC_CHECK_HEADERS("rdmacred.h", [:], [have_drc=no])
    AC_CHECK_LIB([drc], [drc_acquire], [:], [have_drc=no])
fi

if test "x$have_drc" = "xyes" ; then
    AM_CONDITIONAL(HAVE_DRC,true)
    AC_DEFINE([DS_HAVE_DRC], [1],
        ["uGNI Dynamic Credentials is enabled"])
elif test "x$have_drc" = "xno" ; then
    AC_MSG_ERROR([Dynamic Credentials enabled, but not found on this machine.])
fi

if test "$have_ugni" = no; then
  AM_CONDITIONAL(HAVE_UGNI,false)
fi

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(PMI_CPPFLAGS)
AC_SUBST(PMI_LDLAGS)
AC_SUBST(PMI_LIBS)
AC_SUBST(UGNI_CPPFLAGS)
AC_SUBST(UGNI_LDLAGS)
AC_SUBST(UGNI_LIBS)
AC_SUBST(DRC_CPPFLAGS)
AC_SUBST(DRC_LIBS)

if test "${CRAYPE_NETWORK_TARGET}" = aries -o \
        "${XTPE_NETWORK_TARGET}" = aries; then
    AM_CONDITIONAL(HAVE_ARIES, true)
else
    AM_CONDITIONAL(HAVE_ARIES, false)
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_UGNI_TRUE}"; then
        ifelse([$1],,[AC_DEFINE(HAVE_UGNI,1,[Define if you have the Gemini.])],[$1])
        if test -z "${HAVE_ARIES_TRUE}" ; then
            AC_DEFINE(DS_HAVE_ARIES, 1, [uGNI-Aries is enabled])
        fi         
else
        $2
        :
fi
])

