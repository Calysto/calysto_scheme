"""
Differential fuzz testing across the three execution paths in Scheme.py --
the fuzzing counterpart to test_phase2_safety.py. That file replays the
fixed, hand-written test_all.ss suite under three modes ("fast": Phase 2 +
JIT, "slow": trampoline only, "phase2only": Phase 2 with the JIT forced
off) and requires every case's pass/fail result to match. This file
instead generates a large batch of small random Scheme programs
(_scheme_fuzz_gen.py) targeting exactly the constructs _eval_direct,
_JitCompiler, and _phase2_safe_walk implement (arithmetic, comparisons,
`if`, self/mutual recursion, closures returned from closures, a parameter
called as a function, list ops, `begin`), runs each one under all three
modes, and requires the exact *value* (or exception) to match too.

Hand-written test cases only cover the shapes someone thought to write.
This is the standard technique for exactly the failure mode this project's
bug history keeps finding (README-PERFORMANCE.md Phases 6-9): two
independently-maintained walks over the same AST silently disagreeing on
some particular shape nobody happened to write a test for. It already
found one real, confirmed bug on first use: _JitCompiler inlined `odd?` as
`{0} % 2 != 0`, which diverges from the real primitive (`n % 2 == 1`) for
any non-integer argument -- see test_jit_odd_float.py for the isolated
regression test and the fix in Scheme.py's _UNARY table.

The default seed/count below are fixed for CI reproducibility -- a failure
here always reproduces exactly by regenerating with the same
(SCHEME_FUZZ_SEED, SCHEME_FUZZ_CASES). Override either via environment
variable to fuzz harder locally, e.g.:

    SCHEME_FUZZ_CASES=3000 python -m pytest tests/test_jit_fuzz.py -v

Each mode's whole batch runs in one interpreter process/subprocess (see
_scheme_fuzz_runner.py), and every case defines its own new, uniquely-
numbered top-level name(s) -- so per-case cost grows with how many
top-level bindings have already piled up earlier in the same batch (about
10x slower at case 5000 than case 0 in local measurement), making total
runtime for one batch roughly quadratic in count, not linear. This is a
general property of defining many thousands of top-level names in one
process, unrelated to Phase 2/JIT correctness -- keep SCHEME_FUZZ_CASES
in the low thousands; for more coverage, prefer several separate runs
with different SCHEME_FUZZ_SEED values (each starts a fresh process, so
cost resets) over one very large count.

There is no shrinker: on a mismatch, the assertion prints the exact
generated source for every offending case, which is small by construction
(bounded generation depth, one or two function definitions) and meant to
be read and reduced by hand rather than needing automated minimization.
"""
import json
import os
import subprocess
import sys

import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
RUNNER = os.path.join(HERE, "_scheme_fuzz_runner.py")
REPO_ROOT = os.path.join(HERE, "..")

sys.path.insert(0, HERE)
from _scheme_fuzz_gen import gen_cases  # noqa: E402

MODES = ("fast", "slow", "phase2only")
SEED = int(os.environ.get("SCHEME_FUZZ_SEED", "12345"))
COUNT = int(os.environ.get("SCHEME_FUZZ_CASES", "400"))


def _run(mode):
    proc = subprocess.run(
        [sys.executable, RUNNER, mode, str(SEED), str(COUNT)],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        timeout=300,
    )
    assert proc.returncode == 0, (
        f"{mode} mode runner crashed (exit {proc.returncode}).\n"
        f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    )
    return json.loads(proc.stdout.strip().splitlines()[-1])


@pytest.fixture(scope="module")
def all_results():
    return {mode: _run(mode) for mode in MODES}


@pytest.fixture(scope="module")
def cases():
    return gen_cases(SEED, COUNT)


def _assert_match(name_a, data_a, name_b, data_b, cases):
    assert len(data_a) == len(data_b) == len(cases), (
        f"different number of results: {name_a}={len(data_a)} "
        f"{name_b}={len(data_b)} cases={len(cases)}"
    )
    mismatches = [
        {
            "index": i,
            "kind": cases[i][0],
            "source": cases[i][1],
            name_a: a,
            name_b: b,
        }
        for i, (a, b) in enumerate(zip(data_a, data_b))
        if a != b
    ]
    assert not mismatches, (
        f"{name_a} and {name_b} disagree on {len(mismatches)} of "
        f"{len(cases)} fuzzed case(s) (seed={SEED}, count={COUNT}):\n"
        f"{json.dumps(mismatches, indent=2)}"
    )


def test_fuzz_fast_matches_slow_trampoline(all_results, cases):
    _assert_match("fast", all_results["fast"], "slow", all_results["slow"], cases)


def test_fuzz_phase2_only_matches_slow_trampoline(all_results, cases):
    """_eval_direct alone (JIT forced off) vs. the trampoline -- isolates
    _is_phase2_safe/_eval_direct's own correctness from _JitCompiler."""
    _assert_match(
        "phase2only", all_results["phase2only"], "slow", all_results["slow"], cases
    )


def test_fuzz_jit_does_not_change_phase2_results(all_results, cases):
    """_eval_direct alone vs. Phase 2 + JIT -- isolates _JitCompiler: the
    JIT must never change the answer, only how fast it's computed. This
    is the shape of bug this fuzzer already caught once (see
    test_jit_odd_float.py)."""
    _assert_match(
        "phase2only", all_results["phase2only"], "fast", all_results["fast"], cases
    )
