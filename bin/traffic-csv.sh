mkdir -p static/traffic
sh bin/traffic.sh | sponge static/traffic.csv
cat static/traffic.csv | awk '{ print $1 " " $2; }' | sponge static/traffic-requests.csv
cat static/traffic.csv | awk '{ print $1 " " $3; }' | sponge static/traffic-uniques.csv

sh bin/weekly.sh | sponge static/traffic-weekly.csv
cat static/traffic-weekly.csv | awk '{ print $1 " " $2; }' | sponge static/traffic-weekly-requests.csv
cat static/traffic-weekly.csv | awk '{ print $1 " " $3; }' | sponge static/traffic-weekly-uniques.csv

