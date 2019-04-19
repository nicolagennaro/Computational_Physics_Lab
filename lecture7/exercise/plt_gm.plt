set title 'gaussian'
set xlabel 'x'
set ylabel 'p(x)'

set terminal png
set output 'gauss_metropolis_100000.png'

plot 'gauss_metropolis.dat' u 1:2 w boxes title 'n=100000'
