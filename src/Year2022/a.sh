eval "$(sed '1s/.*/x=$(mktemp -d);cd $x/;s/\$ //;/^ls/d;s/^dir/mkdir/;s/^[0-9]\+/echo \0 >/')"
find $x -type d -exec sh -c 'cat $(find {} -type f) | paste -sd+ | bc' \; | sort -rn | tee >(grep -P '^.{1,5}$' | paste -sd+ | bc) >(head -n 1 | sed 's/$/-40000000/')
