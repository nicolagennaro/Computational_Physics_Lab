#!/bin/bash


sigma=1.0
x0=0.0
delta=5.0
n_bins=20


make_input(){
    file_input="${1}
${sigma}
${x0}
${delta}
${n_bins}"
}



make_plt(){
    file_plt="set title 'gaussian'
set xlabel 'x'
set ylabel 'p(x)'

set terminal png
set output 'gauss_metropolis_${1}.png'

plot 'gauss_metropolis.dat' u 1:2 w boxes title 'n=${1}'"
}



for n in 100 1000 10000 100000; do
    make_input $n
    echo "$file_input" > input_gm.txt
    make_plt $n
    echo "$file_plt" > plt_gm.plt
    ./gauss_metropolis.x < input_gm.txt
    gnuplot plt_gm.plt
done
