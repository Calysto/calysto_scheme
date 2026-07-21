#!/usr/bin/env python3
"""Run the Calysto Scheme benchmark suite (scripts/benchmark.ss).

Usage:
    python3 scripts/benchmark.py
"""
import os
import sys

from calysto_scheme import scheme

HERE = os.path.dirname(os.path.abspath(__file__))
BENCHMARK_FILE = os.path.join(HERE, "benchmark.ss")


def main():
    result = scheme.execute_file_rm(BENCHMARK_FILE)
    if scheme.exception_q(result):
        print(scheme.get_traceback_string(result))
        sys.exit(1)


if __name__ == "__main__":
    main()
