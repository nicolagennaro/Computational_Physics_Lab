set title 'Gaussian'
set xlabel 'x'
set ylabel 'P(x)'

set terminal png
set output 'gasdev.png'


plot 'gasdev.dat' u 1:2 w boxes
