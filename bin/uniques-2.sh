printf "%s\t%s\n" "$1" "$(cat "$1" | rg 'cfduid [a-z0-9]+' -o | sort | uniq | wc -l)" | rg '[^-]+-(.*$)' --replace '$1' | cat
