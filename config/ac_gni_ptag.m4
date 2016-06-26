dnl @synopsis AC_GNI_PTAG([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro handles configure options --with-gni-ptag and --with-gni-cookie
dnl For using the uGNI API.
dnl
dnl
AC_DEFUN([AC_GNI_PTAG],[

have_gni_ptag=no
have_gni_cookie=no
cond_ptag=yes

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

if test "x$have_gni_cookie" = "xno"; then
    cond_ptag=no
fi

if test "x$have_gni_ptag" = "xno"; then
    cond_ptag=no
fi

if test "x$cond_ptag" = "xyes"; then
    AC_DEFINE_UNQUOTED(GNI_PTAG,$GNI_PTAG,[Fixed Gemini ptag])
    AC_DEFINE_UNQUOTED(GNI_COOKIE,$GNI_COOKIE,[Fixed Gemini cookie])
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test "x$cond_ptag" = "xyes"; then
        ifelse([$1],,,[$1])
        :
else
        $2
        :
fi
])

