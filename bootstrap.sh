#!/bin/sh
autoheader
aclocal -I m4
automake -a
autoconf
