#! /bin/bash
#
g++ -c -Wall -fopenmp jacobi_openmp.cpp
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
g++ -fopenmp -o jacobi jacobi_openmp.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
#  Request 1 thread.
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
time ./jacobi >> jacobi_openmp.txt
#
#  Request 2 threads.
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
time ./jacobi >> jacobi_openmp.txt
#
#  Request 4 threads.
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
time ./jacobi >> jacobi_openmp.txt
#
#  Request 8 threads.
#
echo "Run with 8 threads."
export OMP_NUM_THREADS=8
time ./jacobi >> jacobi_openmp.txt
#
#  Discard the executable.
#
rm jacobi
#
echo "Normal end of execution."
