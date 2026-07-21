"""
Regression test for a bug in run-unit-test-cases (interpreter-cps.ss).

Background: define-tests bodies freely mix plain `define`s alongside
`assert` calls -- test_all.ss does this constantly. Every top-level form
in a define-tests block runs with the same error-reporting handler
installed, not just `assert` calls. That handler used to assume the
failing form was always assert-shaped (to build a diagnostic report by
destructuring it); if a `define` failed instead, that assumption was
wrong, and the destructuring itself could throw. Because nothing updated
the active handler register until after that destructuring succeeded,
the failure caused trampoline's top-level exception handler to
re-invoke the exact same handler with the exact same (still-failing)
arguments -- an unconditional infinite loop.

This reproduces with a plain (/ 1 0) inside a `define` -- no JIT/Phase-2
involvement at all -- confirmed present on the original, unmodified
interpreter (commit 41c7814, before any Phase 7/8 work). Run in a
subprocess with a hard timeout: a regression here should fail loudly
(timeout) rather than hang the whole test suite.
"""
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.join(HERE, "..")

SNIPPET = """
(clear-unit-tests)
(define-tests ordinary-fail
  (define x (/ 1 0))
  (assert equal? 1 1 "trivial")
  )
(run-tests)
"""


def test_ordinary_error_in_define_inside_define_tests_does_not_hang(tmp_path):
    script = tmp_path / "ordinary_define_fail.ss"
    script.write_text(SNIPPET)

    proc = subprocess.run(
        [sys.executable, os.path.join(REPO_ROOT, "calysto_scheme", "scheme.py"), str(script)],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        timeout=20,
    )
    assert proc.returncode == 0, (
        f"scheme.py exited {proc.returncode} on an ordinary error inside a "
        f"define-tests define -- expected a clean run\nstdout:\n{proc.stdout}\n"
        f"stderr:\n{proc.stderr}"
    )
    assert "division by zero" in proc.stdout
    assert "Total tests tested : 2" in proc.stdout
    assert "Right: 1" in proc.stdout
    assert "Wrong: 1" in proc.stdout
