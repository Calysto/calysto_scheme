"""
Regression tests: redefining a fast-path primitive (`+`, `<`, ... any of
the ~80 names in _FAST_PRIM_SPECS / the 9 in _JitCompiler._NARY/_CMP/_UNARY)
must behave exactly like it does on the plain register-machine trampoline --
the later definition wins, for every subsequent call, and *only* affects
calls that actually resolve to that name.

Two independent bugs were found by testing (not by inspection alone)
against the interpreter directly:

1. _fast_prim_map cache poisoning (_build_fast_prim_map, Scheme.py).
   The map is built lazily, once, and cached for the life of the process,
   keyed by *the identity of the underlying Python dispatch function*
   (proc[1]). Every ordinary user-defined closure shares the exact same
   dispatch function, b_proc_1_d -- it is not unique per closure. If a
   fast-path primitive name is already rebound to a user closure at the
   moment the map is first built, _build_fast_prim_map resolves that name
   to b_proc_1_d instead of the real primitive, so the map ends up mapping
   b_proc_1_d -> the primitive's fast handler. From then on *every*
   Phase-2/JIT-eligible closure call in the program matches that same key
   and is silently replaced by the primitive's behavior, regardless of
   what the closure's own code says -- confirmed below with a closure that
   has nothing to do with the redefined name at all.

2. _JitCompiler stale inlining (_JitCompiler._app, Scheme.py). Even when
   bug 1 doesn't fire (map already warmed up correctly before the
   redefinition), the JIT compiler decides whether to inline `+`/`-`/`*`/
   comparisons/etc. as raw Python operators by checking the operator's
   *literal spelling* in the source (_sym_name). _is_unshadowed_primitive
   guards this at the moment of compilation -- test_redefining_a_
   primitive_after_warmup_is_observed_by_later_jit_compiles below covers
   a function *compiled after* + was already redefined, confirming that
   case is handled. A *further* case -- + redefined *after* a function
   using it was already compiled, previously left this function's
   already-compiled Python code silently using the original semantics
   forever, since nothing ever re-checked it -- is a distinct bug, fixed
   later by _jit_cache's epoch-based invalidation (see _jit_lookup's
   docstring) and covered separately in
   tests/test_jit_cache_invalidation.py, not here.

Both tests run in a fresh subprocess (see _fresh_process_eval.py) because
the bug is specifically about the *first-use* state of the process-wide
_fast_prim_map/_jit_cache singletons -- running in-process here would make
the outcome depend on test execution order (whichever earlier test in the
suite happens to touch Phase 2 first).
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


def test_redefining_a_primitive_before_first_use_does_not_corrupt_unrelated_closures(tmp_path):
    """Bug 1: redefining `+` to a user closure *before* any Phase-2-eligible
    call has happened anywhere in the program must not corrupt later calls
    to an entirely unrelated closure. `unrelated` never mentions `+`."""
    src = """
    (define (weird-add a b) (* a b))
    (set! + weird-add)
    (define (unrelated y) (* y y 10))
    (define (call-unrelated n) (unrelated n))
    (call-unrelated 3)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 90, (
        "redefining + corrupted a call to a completely unrelated closure "
        f"(unrelated 3) -- got {result}, expected 90 (3*3*10). This is the "
        "_fast_prim_map cache-poisoning bug: b_proc_1_d, the dispatch "
        "function shared by *every* user closure, got mapped to the "
        "primitive's fast handler because + was already redefined to a "
        "user closure when the map was first built."
    )


def test_redefining_a_primitive_after_warmup_is_observed_by_later_jit_compiles(tmp_path):
    """Bug 2: once _fast_prim_map is safely warmed up (built before any
    redefinition, so it is not poisoned -- see the test above), a *later*
    redefinition of `+` must still be observed by any closure that gets
    JIT-compiled afterward (test/call-test below are both defined and
    first compiled *after* the redefinition), exactly like it would on
    the plain trampoline. The distinct case of a function *already*
    compiled before + is redefined is covered separately in
    tests/test_jit_cache_invalidation.py."""
    src = """
    (define (warmup a b) (+ a b))
    (define (call-warmup n) (warmup n n))
    (call-warmup 1)

    (define (weird-add a b) (* a b))
    (set! + weird-add)
    (define (test x) (+ x x))
    (define (call-test n) (test n))
    (call-test 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 25, (
        "a closure JIT-compiled after + was redefined still used the "
        f"*original* addition semantics -- got {result}, expected 25 "
        "(5*5, via the redefined weird-add). _JitCompiler._app inlines "
        "+/-/*/comparisons by literal name (_sym_name) without ever "
        "checking what that name currently resolves to."
    )


def test_plain_trampoline_baseline_respects_redefinition(tmp_path):
    """Sanity check / control: a set!-tainted closure (forced entirely onto
    the always-correct trampoline, no Phase 2/JIT involved at all) must
    respect the redefinition -- confirms the two tests above are really
    diagnosing a Phase-2/JIT-specific gap, not something wrong with `set!`
    on a primitive name in general."""
    src = """
    (define (weird-add a b) (* a b))
    (set! + weird-add)
    (define (test-trampoline x)
      (set! x x)
      (+ x x))
    (test-trampoline 5)
    """
    result = _run_fresh(src, tmp_path)
    assert result == 25, (
        "even the always-correct plain trampoline failed to observe the "
        f"redefinition of + -- got {result}, expected 25. If this fails, "
        "the bug is not where this test file assumes it is."
    )
