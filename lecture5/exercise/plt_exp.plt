set title 'Exponential used for Imp Sampling'
set xlabel 'x'
set ylabel 'f(x)'

set terminal png
set output 'exp.png'


plot [0:1] exp( - x**2 ) title 'exp(-x**2)', 1.4*exp( - x ) title '1.4*exp(-x)'