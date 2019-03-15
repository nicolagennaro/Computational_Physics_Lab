set title 'correlation of pairs'

set xlabel 'x'
set ylabel 'y'

set terminal png
set output 'corr.png'


plot [-0.1:1.1][-0.1:1.1]'correlation.dat' u 1:2