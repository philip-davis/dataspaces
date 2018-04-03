dnl @synopsis AC_GNI_PTAG([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro handles configure options --with-gni-ptag and --with-gni-cookie
dnl For using the uGNI API.
dnl
dnl
AC_DEFUN([AC_GNI_PTAG],[

have_gni_ptag=no
have_gni_cookie=no


AC_ARG_WITH(gni-cookie,
	[AS_HELP_STRING([--with-gni-cookie=cookie hexa value],
		[GNI cookie to be used at communications /see apstat -P/. Must be used with --with-gni-ptag on systems with Gemini fabrics. On systems with Aries fabrics, it can be used independently.])],
	[have_gni_cookie=yes;
	 GNI_COOKIE="$withval";])

AC_ARG_WITH(gni-ptag,
	[AS_HELP_STRING([--with-gni-ptag=ptag decimal value],
		[GNI ptag to be used at communications /see apstat -P/. Must be used with --with-gni-cookie on systems with Gemini network. Cannot be used on systems with Aries network.])],
	[have_gni_ptag=yes;
	 GNI_PTAG="$withval";])

dnl ASSUME GEMINI SYSTEM IS TITAN (ORNL) and hardcode ptag/cookie to use ADIOS system-level domain
dnl If we get a different Gemini system, we must not hardcode PTAG/COOKIE. 
if test "${CRAYPE_NETWORK_TARGET}" = gemini -o "${XTPE_NETWORK_TARGET}" = gemini; then
	dnl Ensure user-level ptag/cookie were not provided
	if test "x$have_gni_cookie" = "xno" -o "x$have_gni_ptag" = "xno"; then
		have_gni_ptag=yes
		have_gni_cookie=yes
		GNI_PTAG="250"
		GNI_COOKIE="0x5420000"
	fi
fi

if test "x$have_gni_cookie" = "xyes"; then
    AC_DEFINE_UNQUOTED(GNI_COOKIE,$GNI_COOKIE,[User-provided cookie])
fi

if test "x$have_gni_ptag" = "xyes"; then
    AC_DEFINE_UNQUOTED(GNI_PTAG,$GNI_PTAG,[User-provided ptag])
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test "x$have_gni_cookie" = "xyes"; then
        ifelse([$1],,,[$1])
        :
else
        $2
        :
fi
])
