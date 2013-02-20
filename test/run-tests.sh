#!/bin/sh
if [ ${1:-""} == "-i" ]; then
    emacs -Q -batch -l run-test.el
else
    emacs -Q -l run-test.el
fi
