dnl ######################################################################
dnl
dnl Finds TCP socket 
dnl
dnl ######################################################################

AC_DEFUN([AC_TCP_SOCKET],[
ac_socket_lib_ok=no

TCP_SOCKET_CFLAGS=""
TCP_SOCKET_CPPFLAGS=""
TCP_SOCKET_LDFLAGS=""
TCP_SOCKET_LIBS=""

AC_MSG_NOTICE([=== checking for TCP socket ===])

AM_CONDITIONAL(HAVE_TCP_SOCKET,true)

save_CPPFLAGS="$CPPFLAGS"
save_LDFLAGS="$LDFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$CPPFLAGS $TCP_SOCKET_CPPFLAGS"
LDFLAGS="$LDFLAGS $TCP_SOCKET_LDFLAGS"
LIBS="$LIBS $TCP_SOCKET_LIBS"

dnl Check for the header file.
if test -z "${HAVE_TCP_SOCKET_TRUE}"; then
	  AC_CHECK_HEADERS(sys/socket.h,
		  ,
		  [AM_CONDITIONAL(HAVE_TCP_SOCKET,false)])
fi

dnl Check for the library.

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(TCP_SOCKET_CFLAGS)
AC_SUBST(TCP_SOCKET_CPPFLAGS)
AC_SUBST(TCP_SOCKET_LDFLAGS)
AC_SUBST(TCP_SOCKET_LIBS)

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_TCP_SOCKET_TRUE}"; then
	ac_socket_lib_ok=yes
 	ifelse([$1],,[AC_DEFINE(HAVE_TCP_SOCKET,1,[Define if you have the TCP socket.])],[$1])
	:
else
	$2
	:
fi
])dnl AC_TCP_SOCKET
