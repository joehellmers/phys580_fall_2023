#!/bin/bash

# compiler options, show all warnings and optimization level 3
OPTS="-Wall -O3"

echo "This shell script builds all the generic programs"

gfortran $OPTS -o getstring.f.exe getstring.f90

gfortran $OPTS -o fileout.f.exe fileout.f90 

gfortran $OPTS -o filein.f.exe filein.f90 

gfortran $OPTS -o precisionstuff.f.exe precisionstuff.f90 
