#!/bin/sh
for dir in * ; do
    if [ -d "$dir" ] ; then
    (
	cd "$dir"
	if [ ! -e xfail ] ; then
	    cp obtained-error error
	fi
    )
    fi
done
