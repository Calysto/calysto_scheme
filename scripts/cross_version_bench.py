#!/usr/bin/env python3
"""Run scripts/cross_version_bench.ss against the current interpreter.

To compare across versions, run this from a git worktree checked out at
each tag (see the "Cross-version comparison" section of
calysto_scheme/src/README-PERFORMANCE.md):

    git worktree add /tmp/cvb-v1.4.8 v1.4.8
    cd /tmp/cvb-v1.4.8 && python3 scripts/cross_version_bench.py

Usage:
    python3 scripts/cross_version_bench.py
"""
import os
import sys

from calysto_scheme import scheme

HERE = os.path.dirname(os.path.abspath(__file__))
BENCHMARK_FILE = os.path.join(HERE, "cross_version_bench.ss")


def main():
    result = scheme.execute_file_rm(BENCHMARK_FILE)
    if scheme.exception_q(result):
        print(scheme.get_traceback_string(result))
        sys.exit(1)


if __name__ == "__main__":
    main()
