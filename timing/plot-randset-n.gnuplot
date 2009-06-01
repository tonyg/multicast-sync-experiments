set terminal png size 1024, 768
set datafile separator ","
plot 'intervals_randset.csv' using 6:4 with lines, \
     'gb_sets_randset.csv' using 6:4 with lines, \
     'linear_intervals_randset.csv' using 6:4 with lines
