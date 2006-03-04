#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(BAli-Phy, 1.9.9+, bredelin@ucla.edu)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([1.9 foreign])
AC_CONFIG_SRCDIR([src/bali-phy.C])
AC_CONFIG_HEADER([config.h])

# 1. Checks for programs.
AC_PROG_CXX
AC_PROG_CPP
AC_PROG_INSTALL

# Set language
AC_LANG_CPLUSPLUS
AC_LANG(C++)

# check to see how to make objects in subdirs
AM_PROG_CC_C_O

# 2. Checks for libraries.
# FIXME: Replace `main' with a function in `-lboost_program_options':

# Boost libraries have a string suffix that identifies the compiler
# they were built with, among other details.  For example, it can be
# '-gcc', '-gcc-mt', '-gcc-mt-1_31', and many other combinations
# depending on the build system.
# Some systems provide symlinks that hide these suffixes, avoiding
# this mess.  However, other systems don't; we have to provide a way
# to let the user manually specify a suffix.  Guessing can be very
# difficult, given the variety of possibilities.
AC_ARG_VAR(BOOST_SUFFIX,
           [String suffix that identifies Boost libraries (empty by default)])
AC_SUBST(BOOST_SUFFIX)

AC_ARG_VAR(CHIP,
           [What CPU type should we specialize for to make a faster program?)])
if test "$CHIP" = "" ; then
   echo ""
else
   CHIP="-march=$CHIP -mtune=$CHIP"
fi
AC_SUBST(CHIP)

AC_CHECK_LIB(boost_program_options, main)
# AC_CHECK_LIB(gsl, gsl_cdf_beta_P)
AC_CHECK_LIB(gslcblas, main)

LIBS="-lboost_program_options${BOOST_SUFFIX} $LIBS"
AC_MSG_CHECKING(for the libboost_program_options${BOOST_SUFFIX} library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/program_options.hpp>
  using namespace boost::program_options;
]],
[[options_description invisible("Invisible options");]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_program_options failure])])

# 3. Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
# AC_HEADER_STDBOOL
# AC_C_CONST
# AC_C_INLINE
AC_DEFUN([BOOST_VERSION_CHECK],
[AC_CACHE_CHECK([boost version 1.32 or newer], ac_cv_version_boost,
[
  AC_TRY_RUN(
  [#include <boost/version.hpp>
  int main() { return (BOOST_VERSION < 103200); }],
  ac_cv_version_boost=yes,
  ac_cv_version_boost=no)
])
  if test $ac_cv_version_boost = no; then
	AC_MSG_ERROR([boost 1.32 or newer required])
  fi
])
BOOST_VERSION_CHECK


AC_TYPE_SIZE_T

AC_ARG_ENABLE([debug],
              AS_HELP_STRING([--enable-debug], [Compile in debug mode]),
             [debug=$enableval],
             [debug=no])


AC_ARG_ENABLE([static],
              AS_HELP_STRING([--enable-static], [Link statically]),
             [static=$enableval],
             [static=no])


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
AC_OUTPUT([Makefile src/Makefile])


