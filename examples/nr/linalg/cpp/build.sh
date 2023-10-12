#!/bin/bash
#./clean.sh
g++ -Wall -c svdcmp.cpp pythag.cpp
g++ -Wall -o xsvdcmp svdcmp.o pythag.o xsvdcmp.cpp
#gfortran -Wall -o xsvbksb xsvbksb.f90 nr.o nrutil.o nrtype.o pythag.o svdcmp.o svbksb.o

