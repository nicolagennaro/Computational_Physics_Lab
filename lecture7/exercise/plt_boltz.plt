set title 'Boltzmann distrib'
set xlabel 'E'
set ylabel 'P(E)'


set terminal png
set output 'boltz.png'

plot 'boltzmann.dat' u 1:2 title 'T=1.0'



set xlabel 'E'
set ylabel 'log(P(E))'

set terminal png
set output 'boltz_log.png'

plot 'boltzmann.dat' u 1:(log($2)) title 'T=1.0'
