set title 'spin configuration'
set xlabel 'X'
set ylabel 'Y'

set key outside

set terminal png
set output 'spin_config.png'


plot 'ising-up.dat' with points pointtype 7 pointsize 2 lc 'black' title 'up', \
     'ising-down.dat' with points pointtype 7 pointsize 2 lc 'red' title 'down'
