set title 'MC method'
set xlabel 'x'
set ylabel 'var'


set terminal png
set output 'ex2_var.png'


plot 'mcm.dat' u (log($1)):(log($2)) title 'mcm', 'imp.dat' u(log($1)):(log($2)) title 'imp samp'




set title 'MC method'
set xlabel 'x'
set ylabel 'var of mean'


set terminal png
set output 'ex2_var_of_mean.png'


plot 'mcm.dat' u (log($1)):(log($3)) title 'mcm', 'imp.dat' u(log($1)):(log($3)) title 'imp samp'