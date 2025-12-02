#!/usr/bin/env python3
"""
High-precision benchmark runner for AoC solutions.
Usage: python3 benchmark.py <command> [runs]
"""
import subprocess
import sys
import time
import statistics
import resource

def benchmark(cmd, runs=3):
    """Run command multiple times and report statistics."""
    times = []
    max_mem = 0

    for i in range(runs):
        # Get memory before
        start = time.perf_counter()
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        end = time.perf_counter()

        elapsed = end - start
        times.append(elapsed)

        # Get max memory from rusage (in bytes on macOS)
        usage = resource.getrusage(resource.RUSAGE_CHILDREN)
        if usage.ru_maxrss > max_mem:
            max_mem = usage.ru_maxrss

    # On macOS, ru_maxrss is in bytes; on Linux it's KB
    mem_mb = max_mem / (1024 * 1024)

    avg = statistics.mean(times)
    if len(times) > 1:
        stddev = statistics.stdev(times)
    else:
        stddev = 0

    # Print last output for verification
    print(f"Output: {result.stdout.strip()}")
    print(f"Runs: {runs}")
    print(f"Avg time: {avg*1000:.2f} ms")
    print(f"Std dev: {stddev*1000:.2f} ms")
    print(f"Min: {min(times)*1000:.2f} ms")
    print(f"Max: {max(times)*1000:.2f} ms")
    print(f"Memory: {mem_mb:.2f} MB")

    return avg, mem_mb

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 benchmark.py '<command>' [runs]")
        sys.exit(1)

    cmd = sys.argv[1]
    runs = int(sys.argv[2]) if len(sys.argv) > 2 else 3

    benchmark(cmd, runs)
