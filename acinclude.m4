# ac_check_lib 
# 1 = library / 2 = function / 3 = action-if-found / 4 = action-if-not-found / 5 = other-libraries

AC_DEFUN([AC_CHECK_LIB3],
[
m4_ifval([$3], , [AH_CHECK_LIB([$1])])dnl
AS_LITERAL_IF([$1],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])dnl
AC_CACHE_CHECK([for $2 in -l$1], [ac_Lib],
[ac_check_lib_save_LIBS=$LIBS
LIBS="-l$1 $5 $LIBS"
AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
               [AS_VAR_SET([ac_Lib], [yes])],
               [AS_VAR_SET([ac_Lib], [no])])
LIBS=$ac_check_lib_save_LIBS])
AS_IF([test AS_VAR_GET([ac_Lib]) = yes],
      [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1))
  LIBS="-l$1 $LIBS"
])],
      [$4])dnl
AS_VAR_POPDEF([ac_Lib])dnl
])# AC_CHECK_LIB


AC_DEFUN([AC_CHECK_LITERAL_LIB],
[
  ac_check_lib_save_LIBS=$LIBS

  LIBS="$1 $5 $LIBS"

  #echo "   trying LIBS=$LIBS"
  AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
                 [AS_VAR_SET([ac_Lib], [yes])],
                 [AS_VAR_SET([ac_Lib], [no])])
  #echo "   tried: result = AS_VAR_GET([ac_Lib])"

  LIBS=$ac_check_lib_save_LIBS

  AS_IF([test AS_VAR_GET([ac_Lib]) = yes], [$3], [$4])
]) # AC_CHECK_LITERAL_LIB

AC_DEFUN([AC_CHECK_LIB2], [
  # echo "Library search dirs = $ac_search_lib_dirs"
#  if test "x$host_os_noversion" = "xdarwin" && test "x$static" = "xyes"; then 
  if true ; then
    # if $3 isn't empty, then default to this
    m4_ifval([$3], , [AH_CHECK_LIB([$1])])

    echo "darwin + static = trouble"

  # make local variable ac_Lib from $1 and $2
    AS_LITERAL_IF([$1],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])

    AC_CACHE_CHECK([for $2 in $1], [ac_Lib],[

    ac_check_lib_found=false;
    for dir in $ac_search_lib_dirs ; do
      #echo "  checking $dir for static library $1"
      AC_CHECK_LITERAL_LIB([$dir/lib$1.a],[$2],[ac_check_lib_found=true],[ac_check_lib_found=false],[$5]) 
      AS_IF([test "x$ac_check_lib_found" = xtrue],[ LIBS="$dir/lib$1.a $LIBS" ; break ] )
    done

    ])

    # if we found it do $3, but default to define the HAVE_* if $3 is empty
    AS_IF([test "x$ac_check_lib_found" = xtrue],
    	  [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1))])],
    	  [$4])

    AS_VAR_POPDEF([ac_Lib])dnl

  else
    AC_CHECK_LIB([$1],[$2],[$3],[$4],[$5]) 
  fi
])
