#!/bin/bash

cd ${MESON_SOURCE_ROOT}
(cd tests; python ./run-tests.py $@)
(cd testiphy; python ./testiphy $@)
