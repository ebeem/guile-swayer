AC_INIT([guile-swayer], [1.0.0], [ebeem2@gmail.com])
AM_INIT_AUTOMAKE([foreign])
AC_PROG_RANLIB
AC_CONFIG_FILES([Makefile])
dnl We require pkg.m4 (from pkg-config) and guile.m4.
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([^GUILE_PKG])

dnl Check for Guile 2.x.
GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

dnl Guile prefix and libdir.
GUILE_PREFIX=`$PKG_CONFIG --print-errors --variable=prefix guile-$GUILE_EFFECTIVE_VERSION`
GUILE_LIBDIR=`$PKG_CONFIG --print-errors --variable=libdir guile-$GUILE_EFFECTIVE_VERSION`
AC_SUBST(GUILE_PREFIX)
AC_SUBST(GUILE_LIBDIR)
AC_OUTPUT

# configure.ac ends here
