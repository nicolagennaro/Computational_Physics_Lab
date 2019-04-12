set title 'Normal Distribution'
set xlabel 'x'
set ylabel 'count'


set terminal png
set output 'clt_unif.png'

plot 'clt.dat' u 1:2



set terminal png
set output 'clt_exp.png'

plot 'clt_exp.dat' u 1:2



set terminal png
set output 'clt_Lorentz.png'

plot 'clt_Lorentz.dat' u 1:2