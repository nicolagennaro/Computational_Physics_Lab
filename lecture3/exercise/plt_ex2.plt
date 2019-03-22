set title 'Inverse Transformation Method'
set xlabel 'x'
set ylabel 'P(x)'


set terminal png
set output 'ex2_inv.png'

plot 'ex2.dat' u 1:2 w boxes



set title 'Rejection Method'
set xlabel 'x'
set ylabel 'P(x)'


set terminal png
set output 'ex2_rej.png'

plot 'ex2.dat' u 1:3 w boxes