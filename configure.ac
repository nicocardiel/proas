# Process this file with autoconf to produce a configure script.

# $Id$

AC_INIT(proas,2.1.2,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([foreign])

AC_REVISION($Revision$)
AC_PROG_CC
AC_PROG_F77

PKG_CHECK_MODULES(PLPLOT, plplotd-f77, , 
  AC_MSG_ERROR([missing f77 bindings of plplot]))
AC_CONFIG_FILES([Makefile
                m4/Makefile
                src/Makefile])
AC_OUTPUT
