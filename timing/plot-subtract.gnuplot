set terminal png size 1024, 768
set datafile separator ","
set logscale x 10
plot 'intervals_subtract.csv' using 6:($4/$6) with lines, \
     'gb_sets_subtract.csv' using 6:($4/$6) with lines, \
     'linear_intervals_subtract.csv' using 6:($4/$6) with lines
