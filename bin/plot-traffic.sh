mkdir -p static/traffic
sh bin/traffic.sh | sponge static/traffic/traffic.csv
sh bin/traffic-logs.sh | xargs -n 1 sh bin/uniques-1.sh | sponge static/traffic/uniques.csv
##sh bin/traffic-logs.sh | xargs -n 1 sh bin/uniques-2.sh > visitors.csv
##gnuplot bin/traffic.gp > traffic.png && open traffic.png

now=$(date "+%s")
uptos=$(echo "scale=0; $now - 60*60*24" | bc -l)
upto=$(date -u -r $uptos "+%Y-%m-%d")
gnuplot -e "dateupto=\"$upto\"               " bin/traffic.gp | sponge static/traffic/traffic-${upto}.png
gnuplot -e "dateupto=\"$upto\"; setlogscale=1" bin/traffic.gp | sponge static/traffic/traffic-${upto}-logscale.png
#gnuplot -e "                                 " bin/traffic.gp > traffic.png          
#gnuplot -e "                    setlogscale=1" bin/traffic.gp > traffic-logscale.png 

echo static/traffic/traffic-${upto}.png
echo static/traffic/traffic-${upto}-logscale.png
