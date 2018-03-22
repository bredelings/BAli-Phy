#!/bin/sh
mkdir -p man
for i in *.md ; do
    pandoc -s $i --css ../man.css -o man/${i%%.md}.html
done
