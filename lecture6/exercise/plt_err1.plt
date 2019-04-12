set title 'GauLeg errors'
set xlabel 'n points'
set ylabel 'relative error'

set terminal png
set output 'err_cos.png'

plot 'err_cos.dat' u (log($1)):(log($2)) w lp


set title 'GauLeg errors'
set xlabel 'n points'
set ylabel 'relative error'

set terminal png
set output 'err_exp.png'

plot 'err_exp.dat' u (log($1)):(log($2)) w lp