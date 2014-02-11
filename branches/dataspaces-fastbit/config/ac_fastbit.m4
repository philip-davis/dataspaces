dnl ######################################################################
dnl
dnl Finds FastBit Installation 
dnl
dnl ######################################################################

AC_DEFUN([AC_FASTBIT],[
ac_fastbit_lib_ok=no

FASTBIT_CFLAGS=""
FASTBIT_CPPFLAGS=""
FASTBIT_LDFLAGS=""
FASTBIT_LIBS=""

AC_MSG_NOTICE([=== checking for FastBit install ===])

AM_CONDITIONAL(HAVE_FASTBIT,true)

AC_ARG_WITH(fastbit,
            [  --with-fastbit=DIR      Location of FastBit library],
    	    [ FASTBIT_CPPFLAGS="-I$withval/include";
              FASTBIT_LDFLAGS="-L$withval/lib -Wl,-rpath=$withval/lib";
	      FASTBIT_LIBS="-lfastbit -lrt -lm";])

save_CPPFLAGS="$CPPFLAGS"
save_LDFLAGS="$LDFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$CPPFLAGS $FASTBIT_CPPFLAGS"
LDFLAGS="$LDFLAGS $FASTBIT_LDFLAGS"
LIBS="$LIBS $FASTBIT_LIBS"

dnl Check for the header file.
if test -z "${HAVE_FASTBIT_TRUE}"; then
	  AC_CHECK_HEADERS(capi.h,
		  ,
		  [AM_CONDITIONAL(HAVE_FASTBIT,false)])
fi

dnl Check for the library.
if test -z "${HAVE_FASTBIT_TRUE}"; then
	AC_MSG_CHECKING([if FastBit code can be linked])
	AC_LANG_PUSH([C++])
	AC_TRY_LINK([#include "capi.h"],
		[int ret;
                 fastbit_init("data");
		 ret = fastbit_set_verbose_level(3);
                 fastbit_cleanup();],
		[FASTBIT_LIBS="-lfastbit -lrt -lm"
		 AC_MSG_RESULT(yes)],
		[AM_CONDITIONAL(HAVE_FASTBIT, false)
		 AC_MSG_RESULT(no)])
	AC_LANG_POP([C++])
fi

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(FASTBIT_CFLAGS)
AC_SUBST(FASTBIT_CPPFLAGS)
AC_SUBST(FASTBIT_LDFLAGS)
AC_SUBST(FASTBIT_LIBS)

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_FASTBIT_TRUE}"; then
	ac_fastbit_lib_ok=yes
 	ifelse([$1],,[AC_DEFINE(HAVE_FASTBIT,1,[Define if you have the FastBit.])],[$1])
	:
else
	$2
	:
fi
])dnl AC_FASTBIT
