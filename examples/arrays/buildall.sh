#!/bin/bash

# compiler options, show all warnings and optimization level 3
OPTS="-Wall -O3"

echo "This shell script builds all the array programs"

gfortran $OPTS -o staticarr.f.exe staticarr.f90
gfortran $OPTS -o dynamicarr.f.exe dynamicarr.f90
g++ $OPTS -o staticarr.cpp.exe staticarr.cpp
