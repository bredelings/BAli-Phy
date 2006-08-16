#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(BAli-Phy, 1.9.9-b1, benjamin_redelings@ncsu.edu)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([1.9 foreign])
AC_CONFIG_SRCDIR([src/bali-phy.C])
AC_CONFIG_HEADER([config.h])

# Specify extra include paths
AC_ARG_WITH([extra-includes], AS_HELP_STRING([--with-extra-includes=DIR],[Add extra include paths]),
  use_extra_includes="$withval",
  use_extra_includes=NO
)

if test -n "$use_extra_includes" && test "$use_extra_includes" != "NO"; then
  ac_save_ifs=$IFS
  IFS=':'
  for dir in $use_extra_includes; do
    extra_includes="$extra_includes -I$dir"
  done
  IFS=$ac_save_ifs
fi

CPPFLAGS="$CPPFLAGS $extra_includes"
AC_SUBST(extra_includes)

# Specify extra library paths
AC_ARG_WITH([extra-libs], AS_HELP_STRING([--with-extra-libs=DIR],[Add extra library paths]),
  use_extra_libs="$withval",
  use_extra_libs=NO
)

if test -n "$use_extra_libs" && test "$use_extra_libs" != "NO"; then
  ac_save_ifs=$IFS
  IFS=':'
  for dir in $use_extra_libs; do
    extra_libs="$extra_libs -L$dir"
  done
  IFS=$ac_save_ifs
fi

LDFLAGS="$LDFLAGS $extra_libs"

# 1. Checks for programs.
ORIG_CFLAGS=$CFLAGS
ORIG_CXXFLAGS=$CXXFLAGS

AC_PROG_CXX
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_INSTALL

CFLAGS=$ORIG_CFLAGS
CXXFLAGS=$ORIG_CXXFLAGS

# Set language
AC_LANG_CPLUSPLUS
AC_LANG(C++)

# check to see how to make objects in subdirs
AM_PROG_CC_C_O

# 2. Checks for libraries.
# FIXME: Replace `main' with a function in `-lboost_program_options':

AC_ARG_VAR(CHIP,
           [What CPU type should we specialize for to make a faster program?)])
if test "$CHIP" = "" ; then
   echo ""
elif test "$CHIP" = "G3" ; then
   CHIP="-mcpu=G3 -mtune=G3"
elif test "$CHIP" = "G4" ; then
   CHIP="-mcpu=G4 -mtune=G4"
elif test "$CHIP" = "G5" ; then
   CHIP="-mcpu=G5 -mtune=G5"
else
   CHIP="-march=$CHIP -mtune=$CHIP"
fi

AC_TYPE_SIZE_T

AC_ARG_ENABLE([debug],
              AS_HELP_STRING([--enable-debug], [Compile in debug mode]),
             [debug=$enableval])

AC_ARG_ENABLE([profile],
              AS_HELP_STRING([--enable-profile], [Compile with profiling information]),
             [profile=$enableval],
             [profile=no])

AC_ARG_ENABLE([static],
              AS_HELP_STRING([--enable-static], [Link statically]),
             [static=$enableval],
             [static=no])

CXXFLAGS="-I\$(top_srcdir)/boost/include $CXXFLAGS $CPPFLAGS"
CXXFLAGS="-DBALI_PHY_VERSION=\\\"$PACKAGE_VERSION\\\" $CXXFLAGS"

# we only add extra flags if we recognize the compiler (g++)
if test "$GXX" = yes ; then

  CXXFLAGS="$CXXFLAGS -ffast-math -pipe"

  # set flags for --enable-profile
  if test "$profile" = yes ; then
    CXXFLAGS="$CXXFLAGS -pg"
    LDFLAGS="$LDFLAGS -pg"
  fi

  # set flags for --enable-debug
  if test "$debug" = yes ; then
    CXXFLAGS="$CXXFLAGS -g"

  # set flags for optimization
  else
    CXXFLAGS="$CXXFLAGS $CHIP"
    CXXFLAGS="$CXXFLAGS -O3 -funroll-loops -fweb"
    CXXFLAGS="$CXXFLAGS -DNDEBUG -DNDEBUG_DP"
    if test "$profile" != yes ; then 
      CXXFLAGS="$CXXFLAGS -fomit-frame-pointer"
    fi
  fi
fi

ldflags=$LDFLAGS
AC_SUBST(ldflags)

AM_CONDITIONAL(GCC, test "$GXX" = yes)
AM_CONDITIONAL(DEBUG, test "$debug" = yes)
AM_CONDITIONAL(STATIC, test "$static" = yes)

#determine the OS that is being targeted
AM_CONDITIONAL(HOST_MSWINDOWS,test "${host_os}" = mingw32)
AM_CONDITIONAL(HOST_LINUX,test "${host_os}" = linux-gnu || test "${host_os}" = linux)
AM_CONDITIONAL(HOST_DARWIN,test "${host_os}" = darwin)

#determine the CPU that is being targetted:
#AM_CONDITIONAL(TRUE, true)
AM_CONDITIONAL(HOST_I386,test "${host_cpu}" = i386)
AM_CONDITIONAL(HOST_I686,test "${host_cpu}" = i686)
AM_CONDITIONAL(HOST_X86_64,test "${host_cpu}" = x86_64)
AM_CONDITIONAL(HOST_PPC,test "${host_cpu}" = powerpc)

# Checks for library functions.
AC_HEADER_STDC
AC_FUNC_MALLOC
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([floor pow sqrt strchr])

#AC_CONFIG_FILES([GNUmakefile])
AC_OUTPUT([
  Makefile 
  src/Makefile 
  boost/Makefile 
  boost/lib/Makefile 
  boost/lib/filesystem/Makefile 
  boost/lib/program_options/Makefile
])


echo "CXXFLAGS = $CXXFLAGS"
echo "LDFLAGS = $LDFLAGS"
