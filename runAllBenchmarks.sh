#!/usr/bin/env bash
set -e

declare -A LABELS=(
    [F]="Full example (F.scala)"
    [E]="All capabilities erased (E.scala)"
    [I]="Only Implicit Path-Dependent Capabilities (I.scala)"
)

for series in 17 25 33; do
    bench_dir="bench-ts/bench-DOM${series}"
    results_file="${bench_dir}/results.txt"
    > "$results_file"

    for variant in F I E; do
        file_name="${bench_dir}/${variant}.scala"
        log_dir="${bench_dir}/${variant}-logs"
        find "$log_dir" -name "*.csv" -delete

        echo "=== DOM${series} / ${variant} ==="

        # Patch benchmarks.py and run it
        sed -i "s|^FILE_NAME = .*|FILE_NAME = \"${file_name}\"|" benchmarks.py
        sed -i "s|^LOG_DIR = .*|LOG_DIR = \"${log_dir}\"|" benchmarks.py
        python3 benchmarks.py

        # Patch bcalc.py and run it, appending to results.txt
        sed -i "s|^LOG_DIR = .*|LOG_DIR = \"${log_dir}\"|" bcalc.py
        echo "${LABELS[$variant]}:" >> "$results_file"
        python3 bcalc.py | sed '/^Phase/,$!d' >> "$results_file"
        echo "" >> "$results_file"

        echo "Done. Results appended to ${results_file}"
    done
done
