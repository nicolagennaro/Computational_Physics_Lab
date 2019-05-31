set title 'simulated annealing function'
set xlabel 'x'
set ylabel 'f(x)

set terminal png
set output 'SA_function.png'


plot [-10:10] (x + 0.2) * x + cos( 14.5*x - 0.3)
