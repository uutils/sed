#!/bin/sh
#
# Compare the results of two benchmarks reporting fastest for each task
#


set -eu

if [ $# -ne 2 ] ; then
  echo "Usage: $(basename $0) results-1 results-2" 1>&2

  exit 1
fi

paste -d , "$1" "$2" |
  sed "s/^/$1,$2,/" |

  awk -F, 'FNR != 1 {
    printf("%22s ", $3);
    if ($4 / $12 > 1.1)
      printf("%12s is %5.1f times faster than %s\n", $2, $4 / $12, $1);
    else if ($12 / $4 > 1.1)
      printf("%12s is %5.1f times faster than %s\n", $1, $12 / $4, $2);
    else
      printf("%12s     is similarly fast as   %s\n", $1, $2);
    }'
