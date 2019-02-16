#!/usr/bin/gnuplot
# gnuplot script file for plotting traffic over time
reset
set terminal png font verdana 20 size 3600,2070


# set tmargin at screen 0.01
# set bmargin at screen 0.99
# set lmargin at screen 0.01
# set rmargin at screen 0.99

# unset xtics
# unset ytics
 
if (exists("setlogscale")) set logscale y 2
if (exists("setlogscale")) set ytics 1,2

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
set yrange [8:*]

set multiplot                       # multiplot mode (prompt changes to 'multiplot')
set size 1, 0.5  

set origin 0.0,0.5
if ( exists("dateupto")) set xrange [ "2018-12-13" : dateupto ]
if (!exists("dateupto")) set xrange [ "2018-12-13" : * ]

plot "static/traffic-requests.csv" using 1:2 with linespoints ls 1 title ""

set origin 0.0,0.0
set ylabel "Unique IPs"

plot "static/traffic-uniques.csv" using 1:2 with linespoints ls 1 title ""
