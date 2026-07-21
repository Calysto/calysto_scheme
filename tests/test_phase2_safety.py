"""
Differential tests across the three execution paths in Scheme.py: the
always-correct register-machine trampoline ("slow"), Phase 2's direct-eval
interpreter alone with the JIT forced off ("phase2only"), and the normal
Phase 2 + JIT path ("fast").

Background: apply_proc only attempts the fast, direct-eval/JIT path
(Phase 2) for a closure once _is_phase2_safe has statically certified
that Phase 2 can run its entire body to completion without ever needing
to fall back mid-execution. A closure that isn't certified runs entirely
via the register-machine trampoline instead -- slower, but has always
been correct. Within Phase 2, _eval_direct (the direct-eval interpreter)
and _JitCompiler (compiles a closure's AST to a real Python function) are
two independently-maintained walks over the same AST node types, and
history here (see README-PERFORMANCE.md's Phases 6-8) shows they can
silently diverge: a fix or a new AST case landing in one but not the
other.

Each mode below runs the full test_all.ss suite once, in a fresh
subprocess (see _phase2_diff_runner.py), and the pass/fail result for
every assertion, case by case, must be identical across modes:

  - fast vs. slow:        does Phase 2 (direct-eval + JIT together) match
                           the always-correct trampoline?
  - phase2only vs. slow:  does _eval_direct alone (JIT forced off) match
                           the trampoline? Isolates _eval_direct from
                           _JitCompiler.
  - phase2only vs. fast:  does enabling the JIT change any result?
                           Isolates _JitCompiler from _eval_direct --
                           this is the shape of bug Phase 6/7 hit (a case
                           the JIT compiles differently than _eval_direct
                           would have evaluated it).

This does not prove any of the three paths correct for every possible
program -- only for what test_all.ss exercises today. It exists so that
future changes to _eval_direct, _JitCompiler, or _is_phase2_safe get an
automatic, repeatable check for this specific class of bug, rather than
relying on someone noticing a subtle mismatch by hand.
"""
import json
import os
import subprocess
import sys

import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
RUNNER = os.path.join(HERE, "_phase2_diff_runner.py")
REPO_ROOT = os.path.join(HERE, "..")

MODES = ("fast", "slow", "phase2only")


def _run(mode):
    proc = subprocess.run(
        [sys.executable, RUNNER, mode],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert proc.returncode == 0, (
        f"{mode} mode runner crashed (exit {proc.returncode}).\n"
        f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    )
    last_line = proc.stdout.strip().splitlines()[-1]
    data = json.loads(last_line)
    assert not (isinstance(data, dict) and "exception" in data), (
        f"{mode} mode raised a Scheme-level exception:\n{data['exception']}"
    )
    return data


@pytest.fixture(scope="module")
def all_results():
    return {mode: _run(mode) for mode in MODES}


def _assert_match(name_a, data_a, name_b, data_b):
    assert len(data_a) == len(data_b), (
        f"different number of test cases ran: {name_a}={len(data_a)} "
        f"{name_b}={len(data_b)} -- a case that crashes in one mode but "
        "not the other would show up this way"
    )
    mismatches = [
        {"index": i, name_a: a, name_b: b}
        for i, (a, b) in enumerate(zip(data_a, data_b))
        if a != b
    ]
    assert not mismatches, (
        f"{name_a} and {name_b} disagree on {len(mismatches)} test "
        f"case(s):\n{json.dumps(mismatches, indent=2)}"
    )


def test_phase2_safety_matches_slow_trampoline(all_results):
    fast, slow = all_results["fast"], all_results["slow"]
    _assert_match("fast", fast, "slow", slow)
    assert all(passed for _, _, passed in fast), (
        "all cases agreed between fast/slow, but some failed in both -- "
        "that's a different (pre-existing) bug, not a Phase 2 safety gap"
    )


def test_phase2_only_matches_slow_trampoline(all_results):
    """_eval_direct alone (JIT forced off) vs. the trampoline -- isolates
    _is_phase2_safe/_eval_direct's own correctness from _JitCompiler."""
    phase2only, slow = all_results["phase2only"], all_results["slow"]
    _assert_match("phase2only", phase2only, "slow", slow)


def test_jit_does_not_change_phase2_results(all_results):
    """_eval_direct alone vs. Phase 2 + JIT -- isolates _JitCompiler:
    the JIT must never change the answer, only how fast it's computed."""
    phase2only, fast = all_results["phase2only"], all_results["fast"]
    _assert_match("phase2only", phase2only, "fast", fast)
