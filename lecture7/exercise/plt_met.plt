set title 'Metropolis Sampling'
set xlabel 'n'
set ylabel 'value'


set terminal png
set output 'met.png'


plot 'HO_met.dat' u 1:2 title 'E tot', 'HO_met.dat' u 1:3 title 'E kin', 'HO_met.dat' u 1:4 title 'E pot', 'HO_met.dat' u 1:5 title 'x1', 'HO_met.dat' u 1:6 title 'x2'




set title 'Metropolis vs Direct Sampling'
set xlabel 'log(n)'
set ylabel 'log(Error on E tot)'



set terminal png
set output 'met-dir.png'

plot 'HO.dat' u (log($1)):(log($7)) title 'Direct', 'HO_met.dat' u (log($1)):(log($7)) title 'Metropolis'