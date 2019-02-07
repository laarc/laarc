sh bin/traffic-logs.sh | xargs -n 1 wc -l | awk '{print $2 "\t" $1}' | rg '[^-]+-(.*$)' --replace '$1' | cat
