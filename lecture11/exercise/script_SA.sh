#!/bin/bash

rm -f *.dat


T0=10
delta_T=0.9
n_steps=1000

# file_out='SA.dat'

# {
# ./simulated_annealing.x <<EOFF
# $T0
# $delta_T
# $n_steps
# EOFF
# }  > $file_out


## changing T0

file_out=SA_T0.dat

echo "" > $file_out


delta_T=0.5
n_steps=5000

for T0 in 2 5 7 9 10; do
{
./simulated_annealing.x <<EOFF
$T0
$delta_T
$n_steps
EOFF
}  >> $file_out
done



## changing delta_T

file_out=SA_dT.dat

echo "" > $file_out

T0=5
n_steps=5000

for delta_T in 0.9 0.7 0.5 0.3 0.1; do
{
./simulated_annealing.x <<EOFF
$T0
$delta_T
$n_steps
EOFF
}  >> $file_out
done




## changing n_steps
