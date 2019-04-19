set title 'Autocorrelation'
set xlabel 'n'
set ylabel 'autocorr'
set key right bottom



set terminal png
set output 'autocorr.png'

plot [:][-2:2] 'autocorr.dat' u 1:2 title 'gas1', 'autocorr.dat' u 1:3 title 'met1', 'autocorr.dat' u 1:4 title 'gas2', 'autocorr.dat' u 1:5 title 'met2', 'autocorr.dat' u 1:6 title 'gas3', 'autocorr.dat' u 1:7 title 'met3' 