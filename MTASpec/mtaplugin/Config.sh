#!/bin/csh
# Run as: csh compile

set COMPILER='gfortran'

cd fragmentation 
make -f Makefile.64
mv mtaplugin.so ../
cd ../

cd extra-fs

$COMPILER -o IR.exe IR.F90
$COMPILER -o RAMAN.exe RAMAN.F90

mv IR.exe RAMAN.exe ../

