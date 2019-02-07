sh bin/traffic.sh > traffic.csv
sh bin/traffic-logs.sh | xargs -n 1 sh bin/uniques-1.sh > uniques.csv
sh bin/traffic-logs.sh | xargs -n 1 sh bin/uniques-2.sh > visitors.csv
gnuplot bin/traffic.gp > traffic.png && open traffic.png
