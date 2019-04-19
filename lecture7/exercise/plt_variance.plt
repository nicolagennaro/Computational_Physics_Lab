set title 'momenta'
set xlabel 'n'
set ylabel 'value'


set terminal png
set output 'variance.png'

plot [:][-0.5:2]'variance.dat' u 1:2 title 'x1', 'variance.dat' u 1:3 title 'x2', 'variance.dat' u 1:4 title 'sigma2_{est}'