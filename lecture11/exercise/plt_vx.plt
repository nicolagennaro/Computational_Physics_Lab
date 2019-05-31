set title 'V(x)'
set xlabel 'x'
set ylabel 'V(x)'

set terminal png
set output 'Vx.png'


plot [-3:3] x**2/2 + 1/8 * x**4
