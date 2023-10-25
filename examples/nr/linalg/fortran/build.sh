#!/bin/bash
./clean.sh
gfortran -Wall -c nrtype.f90 nr.f90 nrutil.f90 ludcmp.f90 lubksb.f90 pythag.f90 svdcmp.f90 svbksb.f90
gfortran -Wall -o xludcmp xludcmp.f90 nr.o nrutil.o nrtype.o ludcmp.o
gfortran -Wall -o xlubksb xlubksb.f90 nr.o nrutil.o nrtype.o ludcmp.o lubksb.o
gfortran -Wall -o xsvdcmp xsvdcmp.f90 nr.o nrutil.o nrtype.o pythag.o svdcmp.o
gfortran -Wall -o xsvbksb xsvbksb.f90 nr.o nrutil.o nrtype.o pythag.o svdcmp.o svbksb.o

g++  -Wall -c ludcmp.cpp lubksb.cpp svdcmp.cpp pythag.cpp svbksb.cpp
g++  -Wall -o xludcmp ludcmp.o xludcmp.cpp


srun --partition=debug  --pty --account=TG-PHY230001 --nodes=1 --ntasks-per-node=4 \
    --mem=8G -t 00:30:00 --wait=0 --export=ALL /bin/bash

srun --partition=gpu-debug --pty --account=TG-PHY230001 --ntasks-per-node=10 \
    --nodes=1 --mem=96G --gpus=1 -t 00:30:00 --wait=0 --export=ALL /bin/bash

call ludcmp(KxK,indx,d,K)

Currently Loaded Modules:
  1) shared   2) slurm/expanse/21.08.8   3) sdsc/1.0   4) DefaultModules   5) gpu/0.15.4 (g)   6) cuda/11.0.2   7) nvhpc/20.9

  Where: 

module load  gpu/0.17.3b


 ./configure --with-cuda=$CUDA_HOME --with-cuda-cc=70 --with-cuda-runtime=11.0--prefix=/home/jhellmer/apps/quantum-espresso/7.2