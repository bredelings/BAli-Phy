#!/bin/sh
LIBTOOLIZE=libtoolize
uname="$(uname)"
echo "Running on $uname"
case "$uname" in (*Darwin*) LIBTOOLIZE=glibtoolize ;; esac

set -x
aclocal -I m4
$LIBTOOLIZE
aclocal -I m4 || exit
$LIBTOOLIZE || exit
autoheader || exit
automake -a || exit
autoconf
