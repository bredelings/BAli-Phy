#!/bin/sh
./count H_sapiens Sulfaci1 <log > m01
./count H_sapiens Halomari <log > m02
./count H_sapiens Esch_coli3 <log > m03
./count Sulfaci1 Halomari <log > m12
./count Sulfaci1 Esch_coli3 <log > m13
./count Halomari Esch_coli3 <log > m23
