set title 'Derivative Errors'

set xlabel 'H'
set ylabel 'abs(err)'

set terminal png
set output 'derivative_errors.png'

plot 'derivative.dat' u 1:6 w lp title 'central diff', \
     'derivative.dat' u 1:7 w lp title 'forward diff', \
     'derivative.dat' u 1:8 w lp title 'backward diff'
     