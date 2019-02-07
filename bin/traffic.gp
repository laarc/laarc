#!/usr/bin/gnuplot
# gnuplot script file for plotting traffic over time
reset
set terminal png font verdana 14 size 900,1035


# set tmargin at screen 0.01
# set bmargin at screen 0.99
# set lmargin at screen 0.01
# set rmargin at screen 0.99

# unset xtics
# unset ytics

set samples 10000
set xdata time
set timefmt "%Y-%m-%dT%H:%M:%S"
set format x "%b %d"

#set xlabel "Date (day/month)"
set ylabel "Page Views"

#set title "laarc.io traffic"
set key below
set grid

set style line 1 lc rgb '#0000ff' lt 1 lw 2 pt 1 #pi -1 ps 1.5
set pointintervalbox 3

set multiplot                       # multiplot mode (prompt changes to 'multiplot')
set size 1, 0.333

set origin 0.0,0.666

set xrange [ "2018-12-13":* ]
#set yrange [0:1]
plot "traffic.csv" using 1:2 with linespoints ls 1 title ""

set origin 0.0,0.333
set ylabel "Unique IPs"

plot "uniques.csv" using 1:2 with linespoints ls 1 title ""

set origin 0.0,0.0
set ylabel "Unique Humans"

plot "visitors.csv" using 1:2 with linespoints ls 1 title ""
