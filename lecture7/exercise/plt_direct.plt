set title 'Direct Sampling'
set xlabel 'n'
set ylabel 'value'


set terminal png
set output 'direct.png'


plot 'HO.dat' u 1:2 title 'E tot', 'HO.dat' u 1:3 title 'E kin', 'HO.dat' u 1:4 title 'E pot', 'HO.dat' u 1:5 title 'x1', 'HO.dat' u 1:6 title 'x2'