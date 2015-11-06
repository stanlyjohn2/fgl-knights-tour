set terminal png transparent nocrop enhanced size 800,480 font "sans,11"
set output 'plot-resource.png'
set y2tics 0.5e9
set xlabel "Grid size"
set ylabel "CPU time (seconds)"
set y2label "Memory usage (bytes)"
plot 'resource.dat' using 1:2 axes x1y1 with lines title "CPU Time", \
     'resource.dat' using 1:3 axes x1y2 with lines title "Memory usage"

set output 'plot-tries.png'
unset y2tics
set xlabel "No. of tries"
set ylabel "Cumulative percentage success"
plot 'tries.dat' using 1:2 with lines title "60 x 60", \
     'tries.dat' using 1:3 with lines title "80 x 80", \
     'tries.dat' using 1:4 with lines title "100 x 100"
