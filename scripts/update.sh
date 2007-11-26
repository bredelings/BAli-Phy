#!/bin/sh

svn update "$@"

rm -f REV REV_DATE

rev=`svn info | awk '/Revision:/ {print $2}'`;

branch=`svn info | sed -ne "/URL:/ {
s,.*/trunk,trunk,
s,.*/branches/,,
s,.*/tags/,,
p
}"`

date +'%b %d %Y %k:%M:%S' > REV_DATE

echo "$branch revision $rev" > REV
