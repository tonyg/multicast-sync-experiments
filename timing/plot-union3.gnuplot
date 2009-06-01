set terminal png size 1024, 768
set datafile separator ","
plot 'data/intervals_union3.csv' using 6:($4/$6) with lines, \
     'data/gb_sets_union3.csv' using 6:($4/$6) with lines, \
     'data/linear_intervals_union3.csv' using 6:($4/$6) with lines
