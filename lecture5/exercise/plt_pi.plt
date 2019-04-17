set terminal png
set output 'pi.png'


plot 'clean.txt' u 1:2:3 w yerr