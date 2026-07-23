"""
Regression tests for a second staleness bug found by applying the same
audit method as test_phase2_safe_cache_invalidation.py to _JitCompiler's
_capture logic -- except here the failure mode is a silently wrong
answer, not a crash, since JIT-compiled code (once compiled) is just
ordinary Python with no _TrampolineFallback path left to trigger.

_capture freezes every free variable's *resolved value* into a compiled
function's Python closure once, at JIT-compile time -- exactly three
distinct successful-capture shapes exist in _capture (Scheme.py):

  1. An already-JIT-compiled helper closure's compiled Python function
     (_jit_lookup(val) hit) -- the most common shape, for any ordinary
     call from one JIT'd function to another by name.
  2. A primitive wrapped from _fast_prim_map (e.g. `car` referenced as a
     value, not called directly in operator position -- that case is
     handled separately, by inlining, see _is_unshadowed_primitive).
  3. A plain scalar (int/float/bool/str), e.g. a global constant.

Unlike Phase 2's _eval_direct, which always re-resolves every name fresh
on every call, none of these were ever re-checked once compiled: a later
(set! h ...), (set! car ...), or (set! limit ...) left an already-
compiled function silently using the original value forever. This is the
same underlying "frozen forever" limitation _is_unshadowed_primitive's
own docstring already documents for the narrower case of inlined
arithmetic/comparison operators (see test_primitive_redefinition.py) --
just previously unguarded for these three other capture shapes.

Fixed the same way as _phase2_safe_cache (see _phase2_safe_lookup and
_jit_lookup's docstrings): _jit_cache now stores (fn-or-False, epoch),
reusing the same process-wide _binding_write_epoch counter, so a stale
compiled function is treated as a cache miss and recompiled on next
lookup -- picking up whatever the redefinition changed. If the new value
isn't itself something the JIT compiler can capture (e.g. a brand new,
not-yet-compiled ordinary closure, as in case 1 below), recompilation
correctly declines (_TrampolineFallback) and the closure falls back to
Phase 2/the trampoline instead -- slower, but still correct, the same
"failure only ever costs speed" guarantee used throughout this codebase.

Bonus (unplanned) side effect, confirmed and covered by
test_redefining_an_inlined_operator_after_first_compile_is_observed
below: since a whole compiled function is evicted and recompiled, not
just its individual captures, this same epoch check *also* closes
_is_unshadowed_primitive's own previously-documented residual risk for
inlined arithmetic/comparison operators (`+`, `<`, ...) redefined *after*
a function using them was already compiled -- see that function's own
_is_unshadowed_primitive check, which now gets re-run against the fresh
environment on every recompile. test_primitive_redefinition.py's own
"Bug 2" only ever covered a function compiled *after* a redefinition,
not one redefined *after* being compiled -- that gap is what this file's
test closes.

Runs in a fresh subprocess for the same reason test_primitive_redefinition
.py and test_phase2_safe_cache_invalidation.py do: process-wide singleton
state (_jit_cache, _binding_write_epoch) must start fresh.
"""
import ast
import os
import subprocess
import sys

import calysto_scheme.scheme as scheme

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
        return ast.literal_eval(rest)
    etype, _, emsg = rest.partition("\t")
    raise AssertionError(f"Scheme-level exception instead of a value: {etype}: {emsg}")


def test_redefining_a_captured_helper_closure_is_observed(tmp_path):
    """Case 1: `caller` calls `h` by name; both get JIT-compiled (h via a
    warmup nested call, caller via call-caller), so caller's compiled
    Python function captures h's *compiled function* directly. Without
    the epoch check, caller kept calling the pre-redefinition h forever
    (confirmed: this returned 6, i.e. 5+1, instead of 500)."""
    src = """
    (define (h a) (+ a 1))
    (define (caller x) (h x))
    (define (warmup-h y) (h y))
    (warmup-h 5)
    (define (call-caller z) (caller z))
    (call-caller 5)
    (set! h (lambda (a) (* a 100)))
    (call-caller 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 500, (
        "caller, JIT-compiled before h was redefined, did not observe the "
        f"redefinition -- got {result}, expected 500 (5*100, via the "
        "redefined h; the stale pre-redefinition answer would be 6)"
    )


def test_redefining_a_captured_scalar_is_observed(tmp_path):
    """Case 3: `check` captures the global `limit` as a frozen Python int
    at compile time. Without the epoch check, (< x limit) kept comparing
    against the original 10 forever (confirmed: check(15) stayed False
    after limit became 20, instead of becoming True)."""
    src = """
    (define limit 10)
    (define (check x) (< x limit))
    (define (warmup-check y) (check y))
    (warmup-check 1)
    (set! limit 20)
    (warmup-check 15)
    """
    result = _run_fresh(src, tmp_path)
    assert result is True, (
        f"check, JIT-compiled before limit was redefined, did not "
        f"observe the redefinition -- got {result!r}, expected #t "
        "(15 < 20; the stale pre-redefinition answer would be #f, 15 < 10)"
    )


def test_redefining_a_captured_fast_prim_is_observed():
    """Case 2: `get-car` captures `car` (referenced as a value, not
    called) wrapped from _fast_prim_map. Without the epoch check, calling
    the value returned by get-car kept running the original car forever.
    Uses in-process execution (not the fresh-subprocess runner) because
    the assertion needs to apply the *returned* function value, which
    the subprocess runner's VALUE/EXCEPTION protocol can't round-trip --
    process freshness doesn't matter for correctness here, but since this
    is in-process (unlike every other test in this file), it must restore
    car's real binding afterward itself, or every later test in the same
    pytest run -- not just in this file -- would see `car` permanently
    redefined to (lambda (p) 999)."""
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol("car"))
    original_car = scheme.binding_value(binding)
    try:
        src = """
        (define (get-car) car)
        (define (warmup-get-car) (get-car))
        (warmup-get-car)
        """
        scheme.execute_string_rm(src)
        scheme.execute_string_rm("(set! car (lambda (p) 999))")
        result = scheme.execute_string_rm("((get-car) (cons 3 4))")
        assert result == 999, (
            "get-car, JIT-compiled before car was redefined, returned a "
            f"reference that did not observe the redefinition -- got "
            f"{result!r}, expected 999 (the stale pre-redefinition answer "
            "would be 3, i.e. the real car of (3 . 4))"
        )
    finally:
        scheme.set_binding_value_b(binding, original_car)


def test_recompiled_function_stays_jit_compiled_when_the_new_capture_is_still_eligible():
    """Self-healing check, not just correctness: when the redefined value
    is itself something _capture can still freeze (a plain scalar, as in
    case 3 -- unlike case 1/2's redefinition targets, a brand new
    not-yet-compiled closure or a primitive-turned-closure, which
    correctly and harmlessly fall back to Phase 2 instead), recompilation
    should actually re-establish a real compiled function, not just
    happen to produce the right answer through some slower fallback
    path. In-process (not a fresh subprocess) since this only introspects
    _jit_lookup, it doesn't depend on any specific prior process state."""
    src = """
    (define limit2 10)
    (define (check2 x) (< x limit2))
    (define (warmup-check2 y) (check2 y))
    (warmup-check2 1)
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("check2")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: check2 should have JIT-compiled on warmup"
    )
    scheme.execute_string_rm("(set! limit2 20)")
    scheme.execute_string_rm("(warmup-check2 15)")
    assert scheme._jit_lookup(proc) is not None, (
        "check2 should have been transparently recompiled (not just "
        "fallen back to Phase 2/the trampoline) after limit2 was "
        "redefined, since the new value (20) is just as JIT-capturable "
        "as the old one"
    )


def test_redefining_an_inlined_operator_after_first_compile_is_observed(tmp_path):
    """Case 4 (found while verifying the fix above, not planned when it
    was written): _JitCompiler._app inlines `+`/`-`/`*`/comparisons/etc.
    as raw Python operators rather than going through _capture at all --
    a different mechanism, guarded by _is_unshadowed_primitive instead.
    That guard only ever protected a function being compiled *after* a
    primitive was already redefined (test_primitive_redefinition.py's own
    "Bug 2"); a primitive redefined *after* a function using it was
    already compiled had nothing to re-check the inlining, so it kept
    using the stale, baked-in Python operator forever. _jit_lookup's
    epoch check fixes this too, as a side effect: recompiling re-runs
    _is_unshadowed_primitive against the fresh environment, so it
    correctly declines to re-inline `+` once it's been shadowed."""
    src = """
    (define (f x) (+ x x))
    (define (warmup-f y) (f y))
    (warmup-f 1)
    (set! + (lambda (a b) (* a b)))
    (warmup-f 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 25, (
        "f, JIT-compiled (inlining + as a raw Python operator) before + "
        f"was redefined, did not observe the redefinition -- got "
        f"{result!r}, expected 25 (5*5, via the redefined +; the stale, "
        "still-inlined-addition answer would be 10, i.e. 5+5)"
    )
