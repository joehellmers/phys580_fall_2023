#!/bin/bash
./clean.sh
gfortran -Wall -c nrtype.f90 nr.f90 nrutil.f90 ludcmp.f90 lubksb.f90
gfortran -Wall -o xludcmp xludcmp.f90 nr.o nrutil.o nrtype.o ludcmp.o
gfortran -Wall -o xlubksb xlubksb.f90 nr.o nrutil.o nrtype.o ludcmp.o lubksb.o
