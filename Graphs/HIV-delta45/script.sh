#!/bin/sh
DIR1=../HIV4
DIR2=../HIV5

../subtract.pl  $DIR1/m01 $DIR2/m01 > m01
../subtract.pl  $DIR1/m02 $DIR2/m02 > m02
../subtract.pl  $DIR1/m03 $DIR2/m03 > m03
../subtract.pl  $DIR1/m12 $DIR2/m12 > m12
../subtract.pl  $DIR1/m13 $DIR2/m13 > m13
../subtract.pl  $DIR1/m23 $DIR2/m23 > m23
