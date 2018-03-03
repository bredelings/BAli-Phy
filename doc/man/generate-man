#!/bin/sh
mkdir -p man1

for i in *.md ; do
    echo $i
    pandoc -s -t man $i > man1/${i%.md}.1
done
