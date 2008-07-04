#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(BAli-Phy, 2.0.2beta1, benjamin_redelings@ncsu.edu)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([1.9 foreign])
AC_CONFIG_SRCDIR([src/bali-phy.C])
AC_CONFIG_HEADER([config.h])

# Specify extra include paths
AC_ARG_WITH([extra-includes], AS_HELP_STRING([--with-extra-includes=DIR],[Add extra include paths]),
  use_extra_includes="$withval",
  use_extra_includes=NO
)

# Specify extra library paths
AC_ARG_WITH([extra-libs], AS_HELP_STRING([--with-extra-libs=DIR],[Add extra library paths]),
  use_extra_libs="$withval",
  use_extra_libs=NO
)

if test -n "$use_extra_includes" && test "$use_extra_includes" != "NO"; then
  ac_save_ifs=$IFS
  IFS=':'
  for dir in $use_extra_includes; do
    extra_includes="$extra_includes -I$dir"
  done
  IFS=$ac_save_ifs
fi

if test -n "$use_extra_libs" && test "$use_extra_libs" != "NO"; then
  ac_save_ifs=$IFS
  IFS=':'
  for dir in $use_extra_libs; do
    extra_libs="$extra_libs -L$dir"
    extra_libs2="$extra_libs2 $dir"
  done
  IFS=$ac_save_ifs
fi

case "${host_os}" in  
  cygwin* ) host_os_noversion=cygwin ;;
  darwin*) host_os_noversion=darwin ;;
  mingw32*) host_os_noversion=mingw32 ;;
  linux*) host_os_noversion=linux ;;
  *) ;;
esac

if test "${host_os_noversion}" = darwin ; then 

  CXXFLAGS="$CXXFLAGS -fvisibility=default"

  # try to include Fink automagically
  for dir in /sw /opt /opt/sw ; do
    extra_includes="$extra_includes -I$dir/include"
    extra_libs="$extra_libs -L$dir/lib"
    extra_libs2="$extra_libs2 $dir/lib"
  done
fi

AC_SUBST(extra_includes)
AC_SUBST(extra_libs2)

CPPFLAGS="$CPPFLAGS $extra_includes"
LDFLAGS="$LDFLAGS $extra_libs"

# Specify extra library paths
AC_ARG_WITH([system-boost], AS_HELP_STRING([--with-system-boost],[Add extra library paths]),
  use_system_boost=yes,
  use_system_boost=no
)

AC_ARG_VAR(CHIP,
           [Use features of what CPU type? (-march=?/-mcpu=?: see 'man gcc'])

AC_ARG_VAR(TUNE,
           [Optimize for what CPU type? (-mtune=?: see 'man gcc')])

if test "$CHIP" = "" ; then
   echo " No chip architecture specified: using default."
elif test "$CHIP" = "G3" ; then
   CHIP="-mcpu=G3"
elif test "$CHIP" = "G4" ; then
   CHIP="-mcpu=G4"
elif test "$CHIP" = "G5" ; then
   CHIP="-mcpu=G5"
elif test "$CHIP" = "fast" ; then
   CHIP="-fast"
else
   CHIP="-march=$CHIP"
fi

if test "$TUNE" != "" ; then
   TUNE="-mtune=$TUNE"
fi

CXXFLAGS="$CXXFLAGS $CHIP $TUNE"

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

AC_ARG_VAR(BOOST_SUFFIX,
	   [What suffix do the BOOST libraries have? (e.g. libboost_filesystem-st.so would have suffice '-st')])

AC_TYPE_SIZE_T

AC_ARG_ENABLE([debug],
              AS_HELP_STRING([--enable-debug], [Compile in debug mode]),
             [debug=$enableval],
	     [debug=no])

AC_ARG_ENABLE([bounds_checking],
              AS_HELP_STRING([--enable-bounds-checking], [Compile with bounds checking enabled=yes,no,boost]),
             [bounds_checking=$enableval],
	     [bounds_checking=no])

AC_ARG_ENABLE([profile],
              AS_HELP_STRING([--enable-profile], [Compile with profiling information]),
             [profile=$enableval],
             [profile=no])

AC_ARG_ENABLE([static],
              AS_HELP_STRING([--enable-static], [Link statically]),
             [static=yes],
             [static=no])



# we only add extra flags if we recognize the compiler (g++)
if test "$GXX" = yes ; then

  CXXFLAGS="$CXXFLAGS -pipe"

  # --enable-profile
  if test "$profile" = yes ; then
      CXXFLAGS="$CXXFLAGS -pg"
      LDFLAGS="$LDFLAGS -pg"
  elif test "$profile" = oprofile ; then
      CXXFLAGS="$CXXFLAGS -g -fno-omit-frame-pointer"
  fi

  # --enable-bounds-checking
  if test "$bounds_checking" = yes ; then
      CXXFLAGS="$CXXFLAGS -D_GLIBCXX_DEBUG"
      if test "$use_system_boost" = yes ; then
          echo "Error: cannot bounds check STL if compiled with system BOOST libraries"
          exit 1
      fi  
  elif test "$bounds_checking" = no ; then
      if test "$debug" != no ; then
          CXXFLAGS="$CXXFLAGS -DBOOST_UBLAS_INLINE=inline"
          CXXFLAGS="$CXXFLAGS -DBOOST_UBLAS_CHECK_ENABLE=0"
          CXXFLAGS="$CXXFLAGS -DBOOST_UBLAS_TYPE_CHECK=0"
      fi
  elif test "$bounds_checking" != boost ; then
      echo "I don't recognize --enable-bounds-checking=$bounds_checking."
      echo "Please use values yes,no, or boost"
      exit 1
  fi  

  # --enable-debug
  #   If $enableval !=yes and !=no, then the user can specify their own optimization flags.
  if test "$debug" = yes ; then
      CXXFLAGS="$CXXFLAGS -g"
      # no -ffast-math because it affects handling of NaNs, which result from
      # uninitialized values, among other things
  fi

  if test "$debug" = no ; then
      CXXFLAGS="-ffast-math $CXXFLAGS"
      CXXFLAGS="-O3 -funroll-loops -fweb $CXXFLAGS"
      CXXFLAGS="-DNDEBUG -DNDEBUG_DP $CXXFLAGS"
      if test "$profile" = no ; then 
          CXXFLAGS="-fomit-frame-pointer $CXXFLAGS"
      fi
  elif test "$debug" = O2 ; then
      CXXFLAGS="-ffast-math $CXXFLAGS"
      CXXFLAGS="-O2 -funroll-loops -fweb $CXXFLAGS"
      CXXFLAGS="-DNDEBUG -DNDEBUG_DP $CXXFLAGS"
      if test "$profile" = no ; then 
          CXXFLAGS="-fomit-frame-pointer $CXXFLAGS"
      fi
  fi

  # also set the LDFLAGS for a static build
  if test "${host_os_noversion}" != "darwin" && test "$static" = yes ; then
    LDFLAGS="$LDFLAGS -static"
  fi

fi

ldflags=$LDFLAGS
AC_SUBST(ldflags)


AM_CONDITIONAL(GCC, test "$GXX" = yes)
AM_CONDITIONAL(DEBUG, test "$debug" = yes)
AM_CONDITIONAL(STATIC, test "$static" = yes)
AM_CONDITIONAL(SYSTEM_BOOST, test "$use_system_boost" = yes)
AM_CONDITIONAL(NO_SYSTEM_BOOST, test "$use_system_boost" = no)

#determine the OS that is being targeted
AM_CONDITIONAL(HOST_MINGW32,test "${host_os_noversion}" = mingw32)
AM_CONDITIONAL(HOST_CYGWIN,test "${host_os_noversion}" = cygwin)
AM_CONDITIONAL(HOST_LINUX,test "${host_os_noversion}" = linux)
AM_CONDITIONAL(HOST_DARWIN,test "${host_os_noversion}" = darwin)

#determine the CPU that is being targetted:
AM_CONDITIONAL(HOST_I386,test "${host_cpu}" = i386)
AM_CONDITIONAL(HOST_I686,test "${host_cpu}" = i686)
AM_CONDITIONAL(HOST_X86_64,test "${host_cpu}" = x86_64)
AM_CONDITIONAL(HOST_PPC,test "${host_cpu}" = powerpc)

# Checks for library functions.
AC_HEADER_STDC
AC_FUNC_MALLOC
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([floor pow sqrt strchr log2])

CXXFLAGS="$CXXFLAGS $CPPFLAGS"


ac_search_lib_dirs="$extra_libs2 /usr/lib /usr/local/lib"

#---------------------- Check for math library ------------------#
AC_CHECK_LIB(m,main)

#---------------------- Check for GSL ------------------#
echo "------------------------------------------"
echo " * Looking for GSL headers..."


# Checks for libraries
AC_CHECK_HEADERS([gsl/gsl_rng.h])
AC_CHECK_HEADERS([gsl/gsl_cdf.h])
AC_CHECK_HEADERS([gsl/gsl_randist.h])

echo " * Looking for GSL libraries..."

AC_CHECK_LIB2(gslcblas, main, [], [echo "Error: I can't find the GSL library! (-lgslcblas)" ; exit 1])
AC_CHECK_LIB2(gsl, main, [], [echo "Error: I can't find the GSL library! (-lgsl LIBs=$LIBS)" ; exit 1])
AC_CHECK_LIB(gsl, gsl_cdf_beta_Pinv, [echo "Good... you've got GSL version 1.8 or higher."], [echo "Error: You have the GSL library, but not version 1.8 or higher" ; exit 1])

AC_MSG_CHECKING(include/compile/link for the GSL library)

AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <gsl/gsl_rng.h>
  #include <gsl/gsl_cdf.h>
  #include <gsl/gsl_randist.h>
]],
[[gsl_rng_env_setup();gsl_cdf_beta_Pinv(0.5,1,1);]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([GSL failure])])


#---------------------- Check for BOOST ------------------#
echo "------------------------------------------"
if test "$use_system_boost" = yes ; then
echo " * Looking for the system BOOST headers..."
AC_CHECK_HEADERS([boost/config.hpp],[],[echo "Error: I can't find <boost/config.hpp>."; echo "Please install the BOOST headers, or specify their location using --with-extra-inclues". ; exit 1])
AC_CHECK_HEADERS([boost/filesystem/operations.hpp],[],[echo "Error: I can't find <boost/filesystem/operations.hpp>."; echo "Please install the BOOST filesystem headers." ; exit 1])
AC_CHECK_HEADERS([boost/program_options.hpp],[],[echo "Error: I can't find <boost/program_options.hpp>."; echo "Please install the BOOST program options headers." ; exit 1])
AC_CHECK_HEADERS([boost/graph/graph_traits.hpp],[],[echo "Error: I can't find <boost/graph/graph_traits.hpp>."; echo "Please install the BOOST graph library headers." ; exit 1])

echo " * Looking for the system BOOST libraries... (suffix = '${BOOST_SUFFIX}')"
AC_CHECK_LIB(boost_program_options${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_program_options library! (-lboost_program_options${BOOST_SUFFIX})" ; exit 1])
AC_CHECK_LIB(boost_filesystem${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_filesystem library! (-lboost_filesystem${BOOST_SUFFIX})" ; exit 1])
boost_libs="-lboost_program_options${BOOST_SUFFIX} -lboost_filesystem${BOOST_SUFFIX}";

AC_MSG_CHECKING(include/compile/link for the BOOST program_options library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/program_options.hpp>
  using namespace boost::program_options;
]],
[[options_description invisible("Invisible options");]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_program_options failure])])

AC_MSG_CHECKING(include/compile/link for the BOOST filesystem library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/filesystem/operations.hpp>
  namespace fs = boost::filesystem;
]],
[[fs::path::default_name_check(fs::portable_posix_name);]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_filesystem failure])])
else
  echo " * Not using system BOOST libraries: we'll build our own."

  boost_includes="-I\$(top_srcdir)/boost/include"
  boost_libs="\$(top_builddir)/boost/lib/filesystem/libboost_filesystem.a \$(top_builddir)/boost/lib/program_options/libboost_program_options.a"

  CXXFLAGS="$boost_includes $CXXFLAGS"
  LIBS="$boost_libs $LIBS"
fi
libs=$LIBS
AC_SUBST(libs)
echo "------------------------------------------"

echo ""
echo "CXXFLAGS = $CXXFLAGS"
echo "LDFLAGS = $LDFLAGS"
echo "LIBS = $LIBS"
if test -n "$cairo" && test "$cairo" != "no"; then
  echo "CAIRO_CFLAGS = $CAIRO_CFLAGS"
  echo "CAIRO_LIBS = $CAIRO_LIBS"
fi

CXXFLAGS="$CXXFLAGS -D_CXXFLAGS_=\"\\\"$CXXFLAGS\\\"\" -D_ARCH_=\"\\\"$host\\\"\""

#AC_CONFIG_FILES([GNUmakefile])
AC_OUTPUT([
  Makefile 
  src/Makefile 
  boost/Makefile 
  boost/lib/Makefile 
  boost/lib/filesystem/Makefile 
  boost/lib/program_options/Makefile
])


#------------ 
# If src/revision.H does not exist in the source tree, then create an empty
# version in the build tree.

if test ! -f "$srcdir/src/revision.H" ; then
   touch src/revision.H
fi

