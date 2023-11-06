#! /bin/bash
#
module load cpu/0.15.4
module load gcc/9.2.0
module load openmpi/4.1.1
mpifort -c -Wall wave_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort wave_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wave_mpi.o
mv a.out wave_mpi
#
mpirun -np 1 ./wave_mpi > wave_mpi.np1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#rm wave_mpi
#
echo "Normal end of execution."

