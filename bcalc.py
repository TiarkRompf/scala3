import csv
import os
import statistics

# CONFIGURATION
LOG_DIR = "bench-ts/bench-DOM24/c-logs"
WARMUP_COUNT = 5
COLLECT_COUNT = 10

def calculate_bench_stats():
    print(f"Calculating stats for {LOG_DIR}")
    if not os.path.exists(LOG_DIR):
        print(f"Error: Directory '{LOG_DIR}' not found.")
        return

    all_files = os.listdir(LOG_DIR)
    valid_files = []

    for f in all_files:
        if f.startswith('profile_run_') and f.endswith('.csv'):
            try:
                # Extract number from 'profile_run_X.csv'
                num_part = f.replace('profile_run_', '').replace('.csv', '')
                valid_files.append((int(num_part), f))
            except ValueError:
                continue

    valid_files.sort()

    if len(valid_files) < (WARMUP_COUNT + COLLECT_COUNT):
        print(f"Error: Found {len(valid_files)} files, need {WARMUP_COUNT + COLLECT_COUNT}.")
        return

    # 2. Slice: Ignore first 5, take next 10 (Runs 6 through 15)
    target_files = [f[1] for f in valid_files[WARMUP_COUNT : WARMUP_COUNT + COLLECT_COUNT]]

    print(f"Ignoring first {WARMUP_COUNT} runs (Warmup).")
    print(f"Processing steady-state runs: {target_files[0]} through {target_files[-1]}\n")

    # 3. Data Processing
    phase_data = {"typer": [], "cc": [], "eff": [], "others": [], "run_totals": []}

    for filename in target_files:
        file_path = os.path.join(LOG_DIR, filename)
        r_typer = r_cc = r_eff = r_others = r_total = 0.0

        with open(file_path, mode='r', encoding='utf-8') as f:
            reader = csv.reader(f)
            for row in reader:
                if not row or row[0] != 'MAIN':
                    continue
                try:
                    phase_name = row[5].strip()
                    runtime_ms = int(row[10].strip()) / 1_000_000
                    r_total += runtime_ms
                    if phase_name == "typer": r_typer += runtime_ms
                    elif phase_name == "cc": r_cc += runtime_ms
                    elif phase_name == "eff": r_eff += runtime_ms
                    else: r_others += runtime_ms
                except (IndexError, ValueError):
                    continue

        phase_data["typer"].append(r_typer)
        phase_data["cc"].append(r_cc)
        phase_data["eff"].append(r_eff)
        phase_data["others"].append(r_others)
        phase_data["run_totals"].append(r_total)

    # 4. Output
    header = f"{'Phase ':<20} | {'Avg Time (ms)':>15} | {'Std Dev (ms)':>15} | {'Runs':>5}"
    print(header)
    print("-" * len(header))

    for label, key in [("1. typer", "typer"), ("2. cc", "cc"), ("3. eff", "eff"),
                       ("4. other phases", "others"), ("TOTAL", "run_totals")]:
        data = phase_data[key]
        avg = statistics.mean(data)
        std = statistics.stdev(data) if len(data) > 1 else 0.0
        if key == "run_totals": print("-" * len(header))
        print(f"{label:<20} | {avg:>15.2f} | {std:>15.2f} | {len(data):>5}")

if __name__ == "__main__":
    calculate_bench_stats()