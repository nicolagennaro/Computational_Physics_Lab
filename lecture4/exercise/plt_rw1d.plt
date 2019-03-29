set title 'Random walk 1d'
set xlabel 'final position'
set ylabel 'count'


set terminal png
set output 'rw1d.png'

plot 'P_N.dat' u 1:2 w boxes
