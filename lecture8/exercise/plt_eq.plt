set title 'equilibrium'
set xlabel 'N'
set ylabel 'value'

set terminal png
set output 'equilibrium.png'

plot 'equilibrium.dat' u 1:2 title 'energy', 'equilibrium.dat' u 1:3 title 'magnetization'