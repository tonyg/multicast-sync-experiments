set terminal png size 1024, 768
set datafile separator ","
plot 'data/intervals_intersection2.csv' using 6:4 with lines