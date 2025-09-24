#!/bin/sh

set -eu

if [ $# -ne 2 ] ; then
  cat <<EOF 1>&2
Usage: $(basename $0) sed-command output-file"

Examples:
$(basename $0) 'target/release/sedapp sed' rust-sed
$(basename $0) /usr/bin/sed gnu-sed
EOF
  exit 1
fi

PROG="$1"
OUT="$2"

SCRIPTS=tests/fixtures/sed/script


# Run hyperfine with the specified name and command and collect the results.
bench_run()
{
  if hyperfine --command-name "$1" --warmup 2 --export-csv out.csv --output ./out.txt "$2" ; then
    # Output the results sans-heading.
    sed 1d out.csv >>"$OUT"
  else
    # Unable to run; output a named empty record.
    echo "$1,,,,,,," >>"$OUT"
  fi

  rm out.csv out.txt
}

# Shared heading
echo 'command,mean,stddev,median,user,system,min,max' >"$OUT"

# Short line processing
awk 'BEGIN { for (i = 0; i < 50000000; i++) { print i } }' > lines.txt

# No operation
bench_run no-op-short "$PROG '' lines.txt"

# Log file processing

# Create an access.log file with the specified number of lines
create_access_log()
{
  awk 'BEGIN {
    for (i = 0; i < 5000000; i++) {
      printf("192.168.%d.%d - - [01/Jan/2024:00:00:00 +0000] \"GET /index.html HTTP/1.1\" 200 1234 \"-\" \"Mozilla/5.0\"\n", int(i/256)%256, i%256);
    }
  }' > access.log
}

# Create long file for simple commands
create_access_log 5000000

# No operation
bench_run access-log-no-op "$PROG '' access.log"

# No substitution
bench_run access-log-no-subst "$PROG s/Chrome/Chromium/ access.log"

# Substitution
bench_run access-log-subst "$PROG s/Mozilla/Chromium/ access.log"

# No deletion
bench_run access-log-no-del "$PROG /Chrome/d access.log"

# Full deletion
bench_run access-log-all-del "$PROG /Mozilla/d access.log"

# Create shorter file for more complex commands
create_access_log 50000

# Transliteration
bench_run access-log-translit "$PROG y/0123456789/9876543210/ access.log"

# Multiple substitutions
bench_run access-log-complex-sub "$PROG -f $SCRIPTS/http-log-redact.sed access.log"

# Text append
bench_run access-log-append "$PROG athe-line-ends-here access.log"

rm access.log

# Remove \r
awk 'BEGIN {
  for (i = 0; i < 1500000; i++) {
    printf("line %d with windows endings\r\n", i);
  }
}' > legacy_input.txt

bench_run remove-cr "$PROG 's/\r$//' legacy_input.txt"

rm legacy_input.txt

# Genomic data cleanup

awk 'BEGIN {
  for (i = 0; i < 1000000; i++) {
    chr = "chr" (1 + i % 22);
    printf("%s\t%d\t%s\t.\t%s\t.\t.\n", chr, i * 100, (i % 2 ? "A" : "T"), (i % 2 ? "G" : "C"));
  }
}' > genome.tsv

CMD='/^#/d; s/\t\./\tNA/g; s/\.$/NA/'
bench_run genome-subst "$PROG '$CMD' genome.tsv"

rm -f genome.tsv

# Number fixups: remove thousands separator, change , into .
awk 'BEGIN {
  for (i = 0; i < 700000; i++) {
    euros = int(i % 10000);
    cents = int(i % 100);
    thousands = int(euros / 1000);
    remainder = euros % 1000;
    printf("%d.%03d,%02d\n", thousands, remainder, cents);
  }
}' > finance.csv

CMD='s/\([0-9]\)\.\([0-9]\)/\1\2/g;s/\([0-9]\),\([0-9]\)/\1.\2/g'
bench_run number-fix "$PROG '$CMD' finance.csv"

rm -f finance.csv

# Long script compilation
for i in $(seq 1 99) ; do
   awk -v tag="$i" '{ gsub(/[tb:] [[:alnum:]_]+/, "&" tag); print }' $SCRIPTS/math.sed
done >long_script.sed

bench_run long-script "echo -n '' | $PROG -f long_script.sed "

rm long_script.sed


# Towers of Hanoi
bench_run hanoi "echo ':abcdefghijkl: : :' | $PROG -f $SCRIPTS/hanoi.sed"

# Arbitrary precision arithmetic
bench_run factorial "echo 30\! | $PROG -f $SCRIPTS/math.sed"
