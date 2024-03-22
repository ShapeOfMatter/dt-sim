#!/bin/bash

# from https://tex.stackexchange.com/a/458352

BASE="main"
pdflatex $BASE.tex
if [ $? -ne 0 ]; then
    echo "Compilation error. Check log."
    exit 1
fi
bibtex $BASE
if [ $? -ne 0 ]; then
    echo "Compilation error. Check log."
    exit 1
fi
pdflatex $BASE.tex
if [ $? -ne 0 ]; then
    echo "Compilation error. Check log."
    exit 1
fi
pdflatex $BASE.tex
if [ $? -ne 0 ]; then
    echo "Compilation error. Check log."
    exit 1
fi
exit 0
