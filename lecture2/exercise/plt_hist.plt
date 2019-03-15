set title 'histogram'

set xlabel 'interval'
set ylabel 'number of points'

set terminal png
set output 'hist.png'

# plot [0:STATS_max_y] 'hist.dat' w boxes
plot [0:21][0:1.2] 'hist.dat' w boxes