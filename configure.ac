#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([BAli-Phy],[2.3.6],[benjamin.redelings@duke.edu])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.11 foreign subdir-objects tar-ustar])
AC_CONFIG_SRCDIR([src/bali-phy.C])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])

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
    extra_includes="$extra_includes -isystem $dir"
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

AM_CXXFLAGS="$AM_CXXFLAGS -fvisibility=default"

fi

#elif test "${host_os_noversion}" = darwin ; then 
#
#AC_REQUIRE(AC_CANONICAL_HOST)_LT_SET_OPTION([LT_INIT],[win32-dll])
#AC_DIAGNOSE([obsolete],[AC_LIBTOOL_WIN32_DLL: Remove this warning and the call to _LT_SET_OPTION when you
#put the `win32-dll' option into LT_INIT's first parameter.])



AC_SUBST(extra_includes)
AC_SUBST(extra_libs2)

#Defining CPPFLAGS includes these twice - and they don't include boost!
#FIXME - What is the proper way to include boost - w/o using 
#         the variable $(top_srcdir) to refer to the location of boost?
AM_CPPFLAGS="$AM_CPPFLAGS $extra_includes"

LDFLAGS="$LDFLAGS $extra_libs"

# Specify extra library paths
AC_ARG_WITH([system-boost], AS_HELP_STRING([--with-system-boost],[Use the Boost libraries installed on the system, instead of building our own.]),
  use_system_boost=yes,
  use_system_boost=no
)

# 1. Checks for programs.
ORIG_CFLAGS=$CFLAGS
ORIG_CXXFLAGS=$CXXFLAGS

AC_PROG_CXX
AC_PROG_CPP
LT_PREREQ([2.4.2])
LT_INIT([win32-dll])
AC_PROG_INSTALL
AX_CXX_COMPILE_STDCXX_11(,[mandatory])

#CFLAGS=$ORIG_CFLAGS
#AM_CXXFLAGS=$ORIG_CXXFLAGS

# Set language
AC_LANG([C++])

# check to see how to make objects in subdirs
AM_PROG_CC_C_O

# 2. Checks for libraries.
# FIXME: Replace `main' with a function in `-lboost_program_options':

AC_ARG_VAR(BOOST_SUFFIX,
	   [What suffix do the BOOST libraries have? (e.g. libboost_filesystem-st.so would have suffix '-st')])

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

AC_ARG_ENABLE([cairo],
              AS_HELP_STRING([--enable-cairo], [Build drawing programs that depend on libcairo]),
             [cairo=yes],
             [cairo=no])


# Specify extra library paths
AC_ARG_WITH([mpi], AS_HELP_STRING([--with-mpi],[Compile with MPI]),
  use_mpi="$withval",
  use_mpi="no"
)

# Specify extra library paths
AC_ARG_WITH([openmp], AS_HELP_STRING([--with-openmp],[Compile with OpenMP]),
  use_openmp="$withval",
  use_openmp="no"
)

if test "$use_mpi" != no && test "$use_openmp" != no ; then
   echo "You cannot use both MPI and OpenMP!";
   exit 1;
fi

AC_ARG_VAR(MPICXX,
           [What is the mpicxx command for compiling C++ with MPI?])

if test -z "$MPICXX" ; then
   MPICXX=mpicxx
fi

# we only add extra flags if we recognize the compiler (g++)
if test "$GXX" = yes ; then

  AM_CXXFLAGS="$AM_CXXFLAGS -pipe"

  # --with-openmp
  if test "$use_openmp" != no ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -fopenmp"
      LDFLAGS="$LDFLAGS -fopenmp"
  fi

  # --enable-profile
  if test "$profile" = yes ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -pg"
      LDFLAGS="$LDFLAGS -pg"
  elif test "$profile" = oprofile ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -g -fno-omit-frame-pointer -fkeep-inline-functions"
  fi

  # --enable-bounds-checking
  if test "$bounds_checking" = yes ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -D_GLIBCXX_DEBUG -DDEBUG_BOUNDS_CHECK=1"
      if test "$use_system_boost" = yes ; then
          echo "Error: cannot bounds check STL if compiled with system BOOST libraries"
          exit 1
      fi  
  elif test "$bounds_checking" = boost ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -DDEBUG_BOUNDS_CHECK=1"
  elif test "$bounds_checking" != no ; then
      echo "I don't recognize --enable-bounds-checking=$bounds_checking."
      echo "Please use values yes,no, or boost"
      exit 1
  fi  

  # --enable-debug
  #   If $enableval !=yes and !=no, then the user can specify their own optimization flags.
  if test "$debug" != no ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -g"
      # no -ffast-math because it affects handling of NaNs, which result from
      # uninitialized values, among other things
  fi

  if test "$debug" = no ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -O3"
      AM_CXXFLAGS="-funroll-loops $AM_CXXFLAGS"
      AM_CXXFLAGS="-DNDEBUG -DNDEBUG_DP $AM_CXXFLAGS"
      AM_CXXFLAGS="-ffast-math $AM_CXXFLAGS"
  elif test "$debug" = O ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -O"
  elif test "$debug" = Og ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -Og"
  elif test "$debug" = O2 ; then
      AM_CXXFLAGS="$AM_CXXFLAGS -O2"
  fi

fi


AM_CONDITIONAL(GCC, test "$GXX" = yes)
AM_CONDITIONAL(DEBUG, test "$debug" = yes)
AM_CONDITIONAL(SYSTEM_BOOST, test "$use_system_boost" = yes)
AM_CONDITIONAL(NO_SYSTEM_BOOST, test "$use_system_boost" = no)
AM_CONDITIONAL(WITH_MPI, test "$use_mpi" != no)

#determine the OS that is being targeted
AM_CONDITIONAL(HOST_MINGW32,test "${host_os_noversion}" = mingw32)
AM_CONDITIONAL(HOST_CYGWIN,test "${host_os_noversion}" = cygwin)
AM_CONDITIONAL(HOST_WIN_ALL,test "${host_os_noversion}" = cygwin -o "${host_os_noversion}" = mingw32)
AM_CONDITIONAL(HOST_LINUX,test "${host_os_noversion}" = linux)
AM_CONDITIONAL(HOST_DARWIN,test "${host_os_noversion}" = darwin)

#determine the CPU that is being targetted:
AM_CONDITIONAL(HOST_I386,test "${host_cpu}" = i386)
AM_CONDITIONAL(HOST_I686,test "${host_cpu}" = i686)
AM_CONDITIONAL(HOST_X86_64,test "${host_cpu}" = x86_64)
AM_CONDITIONAL(HOST_PPC,test "${host_cpu}" = powerpc)

AM_CONDITIONAL(HAVE_CAIRO, test "${cairo}" = yes)

# Checks for library functions.
AC_HEADER_STDC
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_HEADERS([sys/resource.h])
AC_CHECK_FUNCS([floor pow sqrt strchr log2 getrlimit setrlimit])
AC_CHECK_TYPE(rlim_t, ,AC_DEFINE(rlim_t, [unsigned long],[declare rlim_t as unsigned long if not found in <sys/resource.h>]),[#include <sys/resource.h>])

AC_CHECK_HEADERS([fenv.h])

AC_CHECK_FUNCS([feenableexcept feclearexcept])

ac_search_lib_dirs="$extra_libs2 /usr/lib /usr/local/lib"

#---------------------- Check for math library ------------------#
AC_CHECK_LIB(m,main)

#---------------------- Check for libdl ------------------#

if test "${host_os_noversion}" = mingw32 ; then 
  libdl_includes="-isystem \$(top_srcdir)/src/dlfcn-win32/"
  AM_CXXFLAGS="$libdl_includes $AM_CXXFLAGS"
else
echo "------------------------------------------"
echo " * Looking for libdl headers..."
AC_CHECK_HEADERS([dlfcn.h],[],[echo "Error: I can't find <dlfcn.h> (AM_CXXFLAGS='$AM_CXXFLAGS').  If libdl is installed in a non-standard location you may need to specify --with-extra-includes=DIR to tell the configure script to look for it in directory DIR"])

echo " * Looking for the libdl library..."
AC_CHECK_LIB(dl, main, [], [echo "Error: I can't find the libdl lirary! (-ldl LIBS='$LIBS')  If libdl is installed in a non-standard location you may need to specify --with-extra-libs=DIR to tell the configure script to look for it in directory DIR" ; exit 1])
AC_CHECK_LIB(dl, dlopen, [], [echo "Error: I can't find the libdl lirary! (-ldl LIBS='$LIBS')  If libdl is installed in a non-standard location you may need to specify --with-extra-libs=DIR to tell the configure script to look for it in directory DIR" ; exit 1])
fi
#------------------- Check for OpenMPI or MPICH ------------------#
echo "------------------------------------------"

if test "$use_mpi" != no ; then
   echo " * Looking for the MPI headers and libraries..."
   if test "$use_mpi" = yes ; then
      if $MPICXX --showme:version >/dev/null 2>/dev/null; then 
      	 echo "Choosing OpenMPI to provide MPI interface."
      	 use_mpi=openmpi
      elif $MPICXX -show > /dev/null ; then
      	 echo "Choosing MPICH to provide MPI interface."
         use_mpi=mpich
      else
         echo "I can't guess which MPI implementation you are using!"
      fi
   fi

   if test "$use_mpi" = "openmpi" ; then
      echo "checking for OpenMPI ..."
      if $MPICXX --showme:version ; then 
         MPI_INCDIRS="$($MPICXX --showme:incdirs)"
	 for incdir in $MPI_INCDIRS ; do
	     MPI_CPPFLAGS="$MPI_CPPFLAGS -isystem $incdir"
	 done
         MPI_CXXFLAGS="$($MPICXX --showme:compile | sed 's/-I/-isystem /g')"
         echo "MPI_CXXFLAGS=$MPI_CXXFLAGS"
         MPI_LDFLAGS=$($MPICXX --showme:link)
         echo "MPI_LDFLAGS=$MPI_LDFLAGS"
      else
	 echo "MPI c++ wrapper '$MPICXX' does not work : failure.";
         exit 1;
      fi 

   elif test "$use_mpi" = mpich ; then
      echo "checking for mpich..."

      if $MPICXX -show >/dev/null ; then 
      	 MPI_CXXFLAGS="$($MPICXX -compile-info -CC= | sed 's/-c//') -fpermissive"
	 echo "MPI_CXXFLAGS=$MPI_CXXFLAGS"
	 [ MPI_LDFLAGS=$($MPICXX -link-info -CC= | sed 's/-D[^-\]* //g') ]
      	 echo "MPI_LDFLAGS=$MPI_LDFLAGS"
      else
	 echo "MPI c++ wrapper '$MPICXX' does not work : failure.";
         exit 1;
      fi 
   fi
   AC_DEFINE([HAVE_MPI],[] ,[You have a working OpenMPI implementation.])
   AC_SUBST(MPI_CXXFLAGS)   
   AC_SUBST(MPI_LDFLAGS)   
   AM_CXXFLAGS="$AM_CXXFLAGS $MPI_CXXFLAGS"
   AM_CPPFLAGS="$AM_CPPFLAGS $MPI_CPPFLAGS"
fi

#---------------------- Check for BOOST ------------------#

echo "------------------------------------------"
if test "$use_system_boost" = yes ; then
echo " * Looking for the system BOOST headers..."
AC_CHECK_HEADERS([boost/config.hpp],[],[echo "Error: I can't find <boost/config.hpp>."; echo "Please install the BOOST headers, or specify their location using --with-extra-inclues". ; exit 1])
AC_CHECK_HEADERS([boost/system/error_code.hpp],[],[echo "Error: I can't find <boost/system/error_code.hpp>."; echo "Please install the BOOST.system headers." ; exit 1])
AC_CHECK_HEADERS([boost/filesystem/operations.hpp],[],[echo "Error: I can't find <boost/filesystem/operations.hpp>."; echo "Please install the BOOST.filesystem headers." ; exit 1])
AC_CHECK_HEADERS([boost/program_options.hpp],[],[echo "Error: I can't find <boost/program_options.hpp>."; echo "Please install the BOOST.program options headers." ; exit 1])
AC_CHECK_HEADERS([boost/graph/graph_traits.hpp],[],[echo "Error: I can't find <boost/graph/graph_traits.hpp>."; echo "Please install the BOOST.graph library headers." ; exit 1])
AC_CHECK_HEADERS([boost/chrono.hpp],[],[echo "Error: I can't find <boost/chrono.hpp>."; echo "Please install the BOOST.chrono library headers." ; exit 1])
AC_CHECK_HEADERS([boost/random/random_device.hpp],[],[echo "Error: I can't find <boost/random/random_device.hpp>."; echo "Please install the BOOST.random library headers." ; exit 1])

if test "$use_mpi" != no ; then
   AC_CHECK_HEADERS([boost/serialization/serialization.hpp],[],[echo "Error: I can't find <boost/serialization/serialization.hpp>."; echo "Please install the BOOST.serialization library headers." ; exit 1])
   AC_CHECK_HEADERS([boost/mpi.hpp],[],[echo "Error: I can't find <boost/mpi.hpp>."; echo "Please install the BOOST.mpi library headers." ; exit 1])
fi

echo " * Looking for the system BOOST libraries... (suffix = '${BOOST_SUFFIX}')"
AC_CHECK_LIB(boost_program_options${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_program_options library! (-lboost_program_options${BOOST_SUFFIX})" ; exit 1])
AC_CHECK_LIB(boost_system${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_system library! (-lboost_system${BOOST_SUFFIX})" ; exit 1])
AC_CHECK_LIB(boost_filesystem${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_filesystem library! (-lboost_filesystem${BOOST_SUFFIX})" ; exit 1])
AC_CHECK_LIB(boost_chrono${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_chrono library! (-lboost_chrono${BOOST_SUFFIX})" ; exit 1])
AC_CHECK_LIB(boost_random${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_random library! (-lboost_random${BOOST_SUFFIX})" ; exit 1])
if test "$use_mpi" != no ; then
   AC_CHECK_LIB(boost_serialization${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_serialization library! (-lboost_serialization${BOOST_SUFFIX})" ; exit 1])
   AC_CHECK_LIB(boost_mpi${BOOST_SUFFIX},main,[],[echo "Error: I can't find the boost_serialization library! (-lboost_mpi${BOOST_SUFFIX})" ; exit 1])
fi

boost_libs="-lboost_program_options${BOOST_SUFFIX} -lboost_filesystem${BOOST_SUFFIX} -lboost_system${BOOST_SUFFIX}";

AC_MSG_CHECKING(include/compile/link for the BOOST.program_options library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/program_options.hpp>
  using namespace boost::program_options;
]],
[[options_description invisible("Invisible options");]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_program_options failure])])


AC_MSG_CHECKING(include/compile/link for the BOOST.system library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/system/system_error.hpp>
  using namespace boost::system;
]],
[[system_error* error;]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_system failure])])

AC_MSG_CHECKING(include/compile/link for the BOOST.filesystem library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/filesystem/operations.hpp>
  using namespace boost::filesystem;
]],
[[file_size("");]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_filesystem failure])])

AC_MSG_CHECKING(include/compile/link for the BOOST.chrono library)
AC_LINK_IFELSE([AC_LANG_PROGRAM([[
  #include <boost/chrono.hpp>
  using namespace boost::chrono;
]],
[[nanoseconds n;]])],
 [AC_MSG_RESULT([yes])],
 [AC_MSG_FAILURE([libboost_filesystem failure])])

else
  echo " * Not using system BOOST libraries: we'll build our own."

  boost_includes="-isystem \$(top_srcdir)/boost/include"

  AM_CXXFLAGS="$boost_includes $AM_CXXFLAGS"
fi
libs=$LIBS
AC_SUBST(libs)
AC_SUBST(BOOST_MPI_LIBS)
echo "------------------------------------------"

#----------------------- Check for cairo ----------------#
cairo_progs=""
if test -n "$cairo" && test "$cairo" != "no"; then
   temp=""
   PKG_CHECK_MODULES(CAIRO,"cairo $temp")
   AC_SUBST(CAIRO_CFLAGS)
   AC_SUBST(CAIRO_LIBS)
fi
AC_SUBST(cairo_progs)

ldflags=$LDFLAGS
if test "${host_os_noversion}" = "linux" ; then
ldflags="$ldflags '-Wl,-rpath,\$\$ORIGIN/../lib/bali-phy/' -Wl,--enable-new-dtags"
fi
AC_SUBST(ldflags)
AC_SUBST(AM_CXXFLAGS)

# Write our compilation flags to config.h so that we can report them with the version info.
AC_DEFINE_UNQUOTED([CONFIG_FLAGS],["$AM_CXXFLAGS"] , ["AM_CXXFLAGS that we used to compile."])
AC_DEFINE_UNQUOTED([_ARCH_],["$host"] , ["Host architecture."])

AC_CONFIG_FILES([
  Makefile 
  src/Makefile 
  src/builtins/Makefile 
  boost/Makefile 
  boost/lib/Makefile 
  boost/lib/chrono/Makefile 
  boost/lib/random/Makefile 
  boost/lib/filesystem/Makefile 
  boost/lib/system/Makefile 
  boost/lib/program_options/Makefile
  boost/lib/serialization/Makefile
  boost/lib/mpi/Makefile
])
AC_OUTPUT

echo ""
echo "CPPFLAGS = $CPPFLAGS"
echo "AM_CPPFLAGS = $AM_CPPFLAGS"
echo "CXXFLAGS = $CXXFLAGS"
echo "AM_CXXFLAGS = $AM_CXXFLAGS"
echo "LDFLAGS = $LDFLAGS"
echo "ldflags = $ldflags"
echo "LIBS = $LIBS"
if test -n "$cairo" && test "$cairo" != "no"; then
  echo "CAIRO_CFLAGS = $CAIRO_CFLAGS"
  echo "CAIRO_LIBS = $CAIRO_LIBS"
fi
AC_SUBST(AM_CPPFLAGS)
AC_SUBST(AM_CXXFLAGS)


# Release instructions:
# 1. Source: make dist
# 2. Mac: CXX=g++-4.8 LDFLAGS="-static-libgcc -static-libstdc++ -mmacosx-version-min=10.6" CXXFLAGS="-mmacosx-version-min=10.6"
# 3. Win: LDFLAGS="-static-libgcc -static-libstdc++" ... and hide dll for winpthreads?
# 4. Linux: --enable-static --force-static CXXFLAGS=-g -fdiagnostics-color=always -finline-limit=1000

# Question: are symbols for dynamic linking making everything MUCH larger?
# Goal: Move module defs into their own subdirectory, just define LDFLAGS once
