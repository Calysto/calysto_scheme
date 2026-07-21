"""
Differential test for apply_proc's _is_phase2_safe gate (Scheme.py).

Background: apply_proc only attempts the fast, direct-eval/JIT path
(Phase 2) for a closure once _is_phase2_safe has statically certified
that Phase 2 can run its entire body to completion without ever needing
to fall back mid-execution. A closure that isn't certified runs entirely
via the register-machine trampoline instead -- slower, but has always
been correct.

If _is_phase2_safe's static walk (which hand-mirrors _eval_direct's own
dispatch) ever diverges from what _eval_direct actually does at runtime
-- as happened during development, when it initially treated
mu-lambda-aexp as safe despite _eval_direct having no case for it at
all -- a *certified* closure can crash (best case, since apply_proc no
longer silently retries a certified closure on _TrampolineFallback -- see
README-PERFORMANCE.md's "Phase 8") or, in principle, behave differently
than the always-correct slow path.

This test runs the full test_all.ss suite twice, in separate subprocesses
started fresh each time (see _phase2_diff_runner.py): once normally, and
once with _is_phase2_safe forced to always return False (every closure
forced onto the slow trampoline, regardless of what it actually
contains). The two runs' pass/fail results, case by case, must be
identical -- any mismatch (or a crash in one mode but not the other)
means _is_phase2_safe's static classification doesn't match Phase 2's
real runtime behavior for some case the existing suite exercises.

This does not prove _is_phase2_safe is correct for every possible
program -- only for what test_all.ss exercises today. It exists so that
future changes to _eval_direct, the JIT compiler, or _is_phase2_safe
itself get an automatic, repeatable check for this specific class of
bug, rather than relying on someone noticing a subtle mismatch by hand.
"""
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
RUNNER = os.path.join(HERE, "_phase2_diff_runner.py")
REPO_ROOT = os.path.join(HERE, "..")


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


def test_phase2_safety_matches_slow_trampoline():
    fast = _run("fast")
    slow = _run("slow")
    assert len(fast) == len(slow), (
        f"different number of test cases ran: fast={len(fast)} slow={len(slow)} "
        "-- a case that crashes in one mode but not the other would show up this way"
    )
    mismatches = [
        {"index": i, "fast": f, "slow": s}
        for i, (f, s) in enumerate(zip(fast, slow))
        if f != s
    ]
    assert not mismatches, (
        "Phase 2 (JIT/direct-eval) and the slow trampoline disagree on "
        f"{len(mismatches)} test case(s) -- _is_phase2_safe's static "
        "certification doesn't match _eval_direct's real behavior for "
        f"these:\n{json.dumps(mismatches, indent=2)}"
    )
    assert all(passed for _, _, passed in fast), (
        "all cases agreed between fast/slow, but some failed in both -- "
        "that's a different (pre-existing) bug, not a Phase 2 safety gap"
    )
