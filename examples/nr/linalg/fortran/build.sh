#!/bin/bash
./clean.sh
gfortran -Wall -c nrtype.f90 nr.f90 nrutil.f90 ludcmp.f90 lubksb.f90 pythag.f90 svdcmp.f90 svbksb.f90 tqli.f90 tred2.f90
gfortran -Wall -o xludcmp xludcmp.f90 nr.o nrutil.o nrtype.o ludcmp.o
gfortran -Wall -o xlubksb xlubksb.f90 nr.o nrutil.o nrtype.o ludcmp.o lubksb.o
gfortran -Wall -o xsvdcmp xsvdcmp.f90 nr.o nrutil.o nrtype.o pythag.o svdcmp.o
gfortran -Wall -o xsvbksb xsvbksb.f90 nr.o nrutil.o nrtype.o pythag.o svdcmp.o svbksb.o
#gfortran -Wall -o badpgm xsvbksb.f90 
gfortran -Wall -o xtqli xtqli.f90 nr.o nrutil.o nrtype.o pythag.o tqli.o tred2.o
