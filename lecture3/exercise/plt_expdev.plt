set title 'expdev distribution'
set xlabel 'x'
set ylabel 'P(x)'

set terminal png
set output 'expdev.png'

plot 'expdev.dat' u 1:2 w boxes


set title 'log(expdev) distribution'
set xlabel 'x'
set ylabel 'log(P(x))'

set terminal png
set output 'expdev_log.png'

plot 'expdev.dat' u 1:(log($2)) w boxes





f(x) = a*x + b


fit f(x) 'expdev.dat' u 1:(log($2)) via a,b


set title 'log(expdev) fit'
set xlabel 'x'
set ylabel 'log(P(x))'


set terminal png
set output 'expdev_fit.png'


plot 'expdev.dat' u 1:(log($2)), f(x)