set terminal png size 1024, 768
set datafile separator ","
plot 'intervals_subtract3.csv' using 6:($4/$6) with lines, \
     'gb_sets_subtract3.csv' using 6:($4/$6) with lines, \
     'linear_intervals_subtract3.csv' using 6:($4/$6) with lines
