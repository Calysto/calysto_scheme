"""
Regression test for a bug found by applying the same audit method as
test_fastprim_no_trampoline_fallback.py / test_jit_tag_parity.py to
_phase2_safe_walk's primitive resolution -- except this asymmetry turned
out to be a live, reproducible bug rather than an already-sound invariant.

_phase2_safe_walk_call (Scheme.py) certifies a closure as Phase-2-safe by
resolving the names it calls (e.g. `+`) against the live environment
*once*, and _is_phase2_safe caches that verdict by closure identity
forever. But _eval_direct -- the thing actually running the certified
closure -- always re-resolves every operator fresh, on every call. If a
name the closure depends on is redefined *after* certification (e.g.
`(set! + something)`), to a value _eval_direct can't run inline (a
closure that itself contains a set!, so its own "safe" flag is False),
the next call re-enters via the stale cached-safe verdict, starts a live
Phase-2 attempt, and _eval_direct raises _TrampolineFallback partway
through the body. apply_proc treats *any* mid-body _TrampolineFallback on
an already-certified closure as an unrecoverable "soundness gap" (a
deliberate choice -- see apply_proc's own comment -- because silently
retrying via the trampoline would re-run any side effects from statements
that already executed, the exact bug Phase 8 fixed). It can't tell "the
analysis has a real bug" apart from "the analysis was simply right at the
time and has since gone stale", so it propagates a loud, internal-looking
error either way, instead of the correct answer.

Fixed by _binding_write_epoch: set_binding_value_b (overridden native in
Scheme.py, added to translate_rm.py's to_ignore list so codegen doesn't
regenerate the plain version) bumps a counter on every existing-binding
mutation -- (set! ...), a top-level re-`define`, and amb/choose's own
undo-on-backtrack, since all of them funnel through it. _phase2_safe_cache
now stores (verdict, epoch-at-certification) and _phase2_safe_lookup
treats a cached verdict as a miss (forcing re-certification) if the epoch
has moved since. See _phase2_safe_lookup's own docstring for the full
reasoning, including why this is deliberately coarse (any write anywhere
invalidates every cached verdict, not just the ones that depended on what
changed).

Runs in a fresh subprocess for the same reason test_primitive_redefinition
.py does: the bug and its fix both live in process-wide singleton state
(_phase2_safe_cache, _binding_write_epoch) that must start fresh, not
depend on whatever earlier tests in this suite happened to touch first.
"""
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
RUNNER = os.path.join(HERE, "_fresh_process_eval.py")
REPO_ROOT = os.path.join(HERE, "..")


def _run_fresh(src, tmp_path):
    ss_path = tmp_path / "case.ss"
    ss_path.write_text(src)
    proc = subprocess.run(
        [sys.executable, RUNNER, str(ss_path)],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert proc.returncode == 0, (
        f"runner crashed (exit {proc.returncode})\n"
        f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
    )
    line = proc.stdout.strip()
    kind, _, rest = line.partition("\t")
    if kind == "VALUE":
        return int(rest)
    etype, _, emsg = rest.partition("\t")
    raise AssertionError(f"Scheme-level exception instead of a value: {etype}: {emsg}")


def test_redefining_a_primitive_after_certification_is_observed_not_a_crash(tmp_path):
    """The core repro: `f` gets Phase-2-certified and cached safe on its
    first call (while `+` is still the real primitive), then `+` is
    redefined to a closure that itself uses set! (so _eval_direct can't
    run it inline). Without the epoch check, the second call to `f`
    crashes with "Unhandled _TrampolineFallback" instead of computing the
    right answer."""
    src = """
    (define (weird-add a b) (set! a a) (* a b))
    (define (f x) (+ x 1))
    (f 5)
    (set! + weird-add)
    (f 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 5, (
        "a closure certified Phase-2-safe before + was redefined did not "
        f"observe the redefinition on its next call -- got {result}, "
        "expected 5 (5*1, via the redefined weird-add). Either it's still "
        "using stale semantics, or (before this fix) it crashed instead "
        "with an unhandled internal _TrampolineFallback."
    )


def test_plain_trampoline_baseline_confirms_the_expected_answer(tmp_path):
    """Sanity check / control, mirroring
    test_primitive_redefinition.py's own baseline test: a set!-tainted `f`
    (forced entirely onto the always-correct trampoline, no Phase 2
    involved) must also respect the redefinition and return 5 -- confirms
    the test above is diagnosing a Phase-2-specific gap, not disagreeing
    with the interpreter's actual semantics for this program."""
    src = """
    (define (weird-add a b) (set! a a) (* a b))
    (define (f-trampoline x)
      (set! x x)
      (+ x 1))
    (f-trampoline 5)
    (set! + weird-add)
    (f-trampoline 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 5, (
        "even the always-correct plain trampoline failed to observe the "
        f"redefinition of + -- got {result}, expected 5. If this fails, "
        "the bug this file targets is not where it's assumed to be."
    )


def test_redefining_a_primitive_to_another_fast_prim_safe_value_still_works(tmp_path):
    """Redefinition doesn't only need to be handled when it makes a name
    *unsafe* -- re-certifying on every epoch bump must also still reach
    the correct (still-safe) verdict when the new value is itself
    perfectly Phase-2-eligible, not just correctly detect danger."""
    src = """
    (define (real-add a b) (+ a b))
    (define (f x) (real-add x x))
    (f 5)
    (set! real-add (lambda (a b) (* a b)))
    (f 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 25, (
        "redefining real-add (itself an ordinary, set!-free closure -- "
        "still fully Phase-2-eligible after the change) was not observed "
        f"on the next call -- got {result}, expected 25 (5*5, via the "
        "redefined real-add; the stale, pre-redefinition answer would "
        "be 10, i.e. 5+5)"
    )
