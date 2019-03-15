set title 'correlation and momenta'

set xlabel 'iteration'
set ylabel 'y'

set terminal png
set output 'corr_mom.png'

plot 'corr_mom.dat' u 1:2 title 'corr', 'corr_mom.dat' u 1:4 title 'momentum'



set title 'delta correlation and momenta'

set xlabel 'iteration'
set ylabel 'y'

set terminal png
set output 'delta_corr_mom.png'

plot 'corr_mom.dat' u 1:3 title 'delta corr', 'corr_mom.dat' u 1:5 title 'delta momentum'