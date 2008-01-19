#!/bin/sh

if ! test -d '.svn' ; then
  echo "Error: this is not an SVN source directory"
  exit 1
fi

if ! test -f src/bali-phy.C ; then
  echo "'$0' must be run from the top SVN source directory."
  exit 1
fi

svn status | grep -v '?'
echo "------------------------------"
svn update "$@" 

rev="$(svn info $srcdir | awk '/Revision: / {print $2}')"  

status="$(svn status | grep -v '?')"

if test -n "$status" ; then
    rev="${rev}+"
fi

branch=`svn info $srcdir | sed -ne "/URL:/ {
    s,.*/trunk,trunk,
    s,.*/branches/,,
    s,.*/tags/,,
    p
    }"`

date=`TZ=UTC svn info $srcdir | sed  -ne '/Last Changed Date/ {
    s/Last Changed Date: //
    p 
}'`

date2=`date -d"$date" +"%b %d %Y %k:%M:%S"`
{
echo "#define REVISION \"$branch revision $rev\""
echo "#define REVISION_DATE \"$date2\""
} > src/revision.H-tmp

if cmp -s src/revision.H src/revision.H-tmp ; then
    rm src/revision.H-tmp
else
    mv src/revision.H-tmp src/revision.H
fi
    
