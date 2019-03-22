set title 'exponential decay'
set xlabel 'time'
set ylabel 'nuclei'

set terminal png
set output 'decay.png'

plot 'decay.dat' u 1:2 w boxes


set title 'log(decay)'
set xlabel 'time'
set ylabel 'log(nuclei)'

set terminal png
set output 'decay_log.png'

plot 'decay.dat' u 1:(log($2)) w boxes





f(x) = a*x + b


fit f(x) 'decay.dat' u 1:(log($2)) via a,b


set title 'log(nuclei) fit'
set xlabel 'time'
set ylabel 'log(nuclei)'


set terminal png
set output 'decay_fit.png'


plot 'decay.dat' u 1:(log($2)), f(x)