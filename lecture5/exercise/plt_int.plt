set title 'Integration'
set xlabel 'log(N)'
set ylabel 'log(Delta)'


set terminal png
set output 'int.png'


plot 'int-tra-sim.dat' u (log($1)):(log($4)) title 'Trapezoidal', 'int-tra-sim.dat' u (log($1)):(log($5)) title 'Simpson'