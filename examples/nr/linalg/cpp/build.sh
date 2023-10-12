#!/bin/bash
#./clean.sh
g++  -Wall -c ludcmp.cpp lubksb.cpp svdcmp.cpp pythag.cpp svbksb.cpp
g++  -Wall -o xludcmp ludcmp.o xludcmp.cpp
g++  -Wall -o xlubksb ludcmp.o lubksb.o xlubksb.cpp
g++  -Wall -o xsvdcmp svdcmp.o pythag.o xsvdcmp.cpp
g++  -Wall -o xsvbksb svbksb.o svdcmp.o pythag.o xsvbksb.cpp

