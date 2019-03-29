f(x) = a*x + b

fit f(x) 'rw2d.dat' u (log($1)):(log($2)) via a, b


set title 'log log'
set xlabel 'log(N)'
set ylabel 'log(delta)'


set terminal png
set output 'rw2d.png'


plot 'rw2d.dat' u (log($1)):(log($2)), f(x)
