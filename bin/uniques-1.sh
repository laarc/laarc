printf "%s\t%s\n" "$1" "$(cat "$1" | awk '{print $2}' | sort | uniq | wc -l)" | rg '[^-]+-(.*$)' --replace '$1' | cat
