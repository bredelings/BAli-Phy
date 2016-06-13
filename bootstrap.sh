#!/bin/sh
set -x
aclocal -I m4
libtoolize
aclocal -I m4 || exit
libtoolize || exit
autoheader || exit
automake -a || exit
autoconf
