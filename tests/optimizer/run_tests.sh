#!/bin/bash

echo -n "Testing optimizer: "
SECONDS=0
PROG=$@

passed=0
failed=0
for i in test*.hs ; do
    $PROG --test-module=$i 2>$i.result
    if ! diff -u $i.opt $i.result; then
	echo "Test $i failed!"
	let "failed++"
    else
	let "passed++"
    fi
done
echo "$passed tests passed, $failed tests failed. ($SECONDS seconds)"
