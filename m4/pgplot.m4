dnl Macro to check for pgplot

AC_DEFUN([STTL_LIB_PGPLOT],
[AC_REQUIRE([AC_PATH_X])
if test "$no_x" = yes; then
AC_MSG_WARN([X is not enabled])
fi
if test "$no_x" = disable; then
AC_MSG_WARN([X is not enabled])
fi

if test -n "$x_libraries"; then
ucm_pgplot_ladd="-L$x_libraries -lX11"
else
ucm_pgplot_ladd="-lX11"
fi

AC_LANG_PUSH([Fortran 77])
AC_CHECK_LIB(pgplot,pgopen,ucm_has_pgp=yes,ucm_have_pgp=no,$ucm_pgplot_ladd)
AC_LANG_POP([Fortran 77])
AC_MSG_CHECKING(for working Fortran pgplot lib)
if test "$ucm_has_pgp" = yes; then
AC_MSG_RESULT(yes)
AC_SUBST(ucm_has_pgp)
PGPLOT_LIBS="-lpgplot $ucm_pgplot_ladd"
else
AC_MSG_ERROR(can't find pgplot)
fi
])

