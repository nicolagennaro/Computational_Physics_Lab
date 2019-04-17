#!/bin/bash

file_mcm="mcm.dat"
file_imp="imp.dat"


cat file_ex2.dat | grep "mcm" | awk '{print $2, $3, $4}' >> $file_mcm

cat file_ex2.dat | grep "imp" | awk '{print $2, $3, $4}' >> $file_imp
