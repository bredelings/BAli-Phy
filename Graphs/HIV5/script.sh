#!/bin/sh
../count CAR4081 consGenv <log > m01
../count CAR4081 consAenv <log > m02
../count CAR4081 consBenv<log > m03
../count consGenv consAenv <log > m12
../count consGenv consBenv <log > m13
../count consAenv consBenv <log > m23
