set terminal png size 1024, 768
set datafile separator ","
plot 'data/intervals_subtract2a.csv' using 6:($4/$6) with lines, \
     'data/gb_sets_subtract2a.csv' using 6:($4/$6) with lines, \
     'data/linear_intervals_subtract2a.csv' using 6:($4/$6) with lines
