import subprocess
import os

# CONFIGURATION
FILE_NAME = "bench-ts/bench-DOM24/c.scala"
LOG_DIR = "bench-ts/bench-DOM24/c-logs"
NUM_RUNS = 15
#

def run_benchmarks():
    print(f"Benchmarking {FILE_NAME}")
    if not os.path.exists(LOG_DIR):
        os.makedirs(LOG_DIR)
        print(f"Created directory: {LOG_DIR}")

    commands = []
    for i in range(1, NUM_RUNS + 1):
        log_file = os.path.join(LOG_DIR, f"profile_run_{i}.csv")
        cmd = f"scalac -Yprofile-enabled -Yprofile-destination {log_file} {FILE_NAME}"
        commands.append(cmd)

    commands.append("exit")

    process = subprocess.Popen(
        ["sbt"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1
    )

    try:
        full_command_stream = "\n".join(commands) + "\n"
        process.stdin.write(full_command_stream)
        process.stdin.flush()

        for line in process.stdout:
            print(f"[sbt] {line.strip()}")

        process.wait()

    except KeyboardInterrupt:
        print("\nTerminating process...")
        process.terminate()
    except Exception as e:
        print(f"An error occurred: {e}")
        process.kill()

if __name__ == "__main__":
    run_benchmarks()