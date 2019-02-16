requests=$(cat "$@" | wc -l)
uniques=$(cat "$@" | awk '!_[$2]++' | wc -l)
from="$1"
shift 1
while [ ! -z "$*" ]
do
  upto="$1"
  shift 1
done

if [ ! -z "$upto" ]
then
  printf "%s %7d %7d\n" "${from}" $requests "$uniques" | sed 's/arc[/]logs[/]srv-//g'
else
  printf "%s %7d %7d\n"  "${from}" $requests "$uniques" | sed 's/arc[/]logs[/]srv-//g'
fi
