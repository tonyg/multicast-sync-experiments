set terminal png size 1024, 768
set datafile separator ","
plot 'data/intervals_randset.csv' using 6:($4/($6 * log($6))) with lines, \
     'data/gb_sets_randset.csv' using 6:($4/($6 * log($6))) with lines, \
     'data/linear_intervals_randset.csv' using 6:($4/($6 * log($6))) with lines
