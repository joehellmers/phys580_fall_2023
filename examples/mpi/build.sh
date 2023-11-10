#! /bin/bash
#
module load cpu/0.15.4
module load gcc/9.2.0
module load openmpi/4.1.1
mpifort -Wall -o wave_mpi wave_mpi.f90

echo "wave_mpi example program built"

