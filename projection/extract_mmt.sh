#!/bin/bash
# Usage: ./parse_outfiles_parallel_fast.sh /path/to/input /path/to/output/mortality_summary.csv

set -euo pipefail

in_dir="$1"
out_file="$2"

if [[ -z "$in_dir" || -z "$out_file" ]]; then
  echo "Usage: $0 INPUT_DIR OUTPUT_FILE"
  exit 1
fi

if [[ ! -d "$in_dir" ]]; then
  echo "Error: input directory '$in_dir' not found"
  exit 1
fi

out_dir=$(dirname "$out_file")
tmpdir=$(mktemp -d "$out_dir/tmp_parseout.XXXXXX") || { echo "Failed to create temp dir"; exit 1; }
echo "Using temporary directory: $tmpdir" >&2

process_file() {
    f="$1"
    tmpout="$2/$(basename "$f").csv"

    awk '
        NR==1 {
            # Grab metadata from header line
            match($0, /rcp=([^,]+)/, a);   rcp=a[1]
            match($0, /gcm=([^,]+)/, a);   gcm=a[1]
            match($0, /year=([0-9]+)/, a); year=a[1]
            next
        }
        /Region:/ {
            match($0, /Region: ([^;]+)/, a); region=a[1]
            match($0, /Age: ([^;]+)/, a);    age=a[1]
            match($0, /MMT: ([^;]+)/, a);    mmt=a[1]
            if (region != "" && age != "" && mmt != "")
                print region "," rcp "," gcm "," year "," age "," mmt
        }
    ' "$f" > "$tmpout"

    echo "Processed $f → $(wc -l < "$tmpout") rows" >&2
}
export -f process_file

# Process in parallel
find "$in_dir" -type f -name "mortality-db-37501761-*.out" \
  | parallel -j 12 process_file {} "$tmpdir"

# Final merge
echo "region,rcp,gcm,year,age,mmt" > "$out_file"
cat "$tmpdir"/*.csv >> "$out_file"
rm -r "$tmpdir"

echo "Done. Output written to $out_file"