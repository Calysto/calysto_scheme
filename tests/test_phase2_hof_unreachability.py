"""
Guard tests recording a finding from investigating the "_apply_direct
never attempts JIT compilation for map/for-each callbacks" gap noted in
README-PERFORMANCE.md's Phase 6 section: that gap, and the closely
related "parameter used as operator" gap (`apply-twice`-shaped functions,
see the same section), are both currently *unreachable* via normal
program execution -- not just theoretically still-open.

The root cause is the same for both: _phase2_safe_walk_call can't
statically resolve a call to a concrete, provably-safe value when the
operator is (a) map/for-each specifically (deliberately excluded via
_fast_prim_hof_fns, since their callback argument isn't verified) or (b)
a local parameter / IIFE / other computed expression (_resolve_operator
returns ('unresolved', None) for anything that isn't a plain name). A
function containing such a call is certified unsafe, and -- because
_phase2_safe_walk_call recursively requires _is_phase2_safe(callee) for
every call in a checked function's body -- that unsafety poisons every
(transitive) caller, all the way to whatever eventually gets called at
the top level. Since Phase 2 (and, by extension, JIT compilation, which
is only ever attempted from inside an already-Phase-2-certified
execution -- see _jit_lookup's callers) is only entered via apply_proc's
own certified gate, nothing in such a call chain ever reaches Phase 2 at
all, so _fast_prim_direct_map/_fast_prim_direct_for_each (and _jit_call's
"parameter as operator" dispatch) are never actually exercised by a
normal program -- confirmed here both statically (_is_phase2_safe) and
dynamically (tracing real calls to the fast-prim wrappers).

This isn't a bug to fix -- it's the deliberate, understood price of
Phase 8's correctness fix (see README-PERFORMANCE.md's Phase 8 section
and JIT-OVERVIEW.md). These tests exist so the *fact* of the
unreachability is pinned down and would be caught (in either direction)
by a future change: if something makes these patterns newly reachable,
that's worth knowing (it might restore the lost Phase 6 speedup, or it
might reopen the double-execution bug Phase 8 fixed, depending on how);
if the exclusion mechanism itself regresses in some other way, that's
worth knowing too.
"""
import calysto_scheme.scheme as scheme


def _is_phase2_safe(name):
    proc = scheme.binding_value(scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name)))
    return scheme._is_phase2_safe(proc)


def test_a_function_directly_calling_map_is_not_phase2_safe():
    scheme.execute_string_rm("""
    (define (add1 y) (+ y 1))
    (define (use-map lst) (map add1 lst))
    """)
    assert _is_phase2_safe("use-map") is False


def test_a_function_directly_calling_for_each_is_not_phase2_safe():
    scheme.execute_string_rm("""
    (define (add1b y) (+ y 1))
    (define (use-for-each lst) (for-each add1b lst))
    """)
    assert _is_phase2_safe("use-for-each") is False


def test_a_function_calling_its_own_parameter_as_operator_is_not_phase2_safe():
    scheme.execute_string_rm("""
    (define (apply-twice f x) (f (f x)))
    """)
    assert _is_phase2_safe("apply-twice") is False


def test_map_unsafety_poisons_every_transitive_caller():
    scheme.execute_string_rm("""
    (define (add1c y) (+ y 1))
    (define (use-map2 lst) (map add1c lst))
    (define (wrap-use-map2 lst) (use-map2 lst))
    (define (wrap-wrap-use-map2 lst) (wrap-use-map2 lst))
    """)
    assert _is_phase2_safe("use-map2") is False
    assert _is_phase2_safe("wrap-use-map2") is False, (
        "a caller of an uncertified function must itself be uncertified -- "
        "_phase2_safe_walk_call recursively requires _is_phase2_safe(callee)"
    )
    assert _is_phase2_safe("wrap-wrap-use-map2") is False, (
        "the poisoning must propagate transitively, not just one level up"
    )


def test_apply_twice_poisons_a_self_recursive_caller_too():
    """Mirrors README-PERFORMANCE.md's own Phase 6 benchmark shape (a
    fib-shaped, non-tail-recursive driver calling apply-twice) -- confirms
    even a self-recursive caller doesn't escape the poisoning."""
    scheme.execute_string_rm("""
    (define (apply-twice2 f x) (f (f x)))
    (define (add1d y) (+ y 1))
    (define (drive n)
      (if (= n 0)
          0
          (+ (apply-twice2 add1d n) (drive (- n 1)))))
    """)
    assert _is_phase2_safe("drive") is False


def test_map_and_for_each_fast_prim_wrappers_are_never_actually_invoked():
    """Dynamic confirmation to complement the static _is_phase2_safe
    checks above: even when the calling function is exercised through a
    realistic nested-call warmup (the pattern used throughout this test
    suite to force JIT compilation for other, reachable shapes), the
    map/for-each fast-prim wrappers are never reached at all -- the
    classic trampoline's own native map/for-each dispatch handles the
    call instead, since Phase 2 is never entered for the caller.

    Traces _apply_direct itself (what _fast_prim_direct_map/_for_each call
    internally), not those wrappers directly: _FAST_PRIM_SPECS captures a
    direct reference to the original wrapper functions at module-load
    time, so rebinding scheme._fast_prim_direct_map/_for_each has no
    effect on what's actually dispatched -- confirmed while writing this
    test. _apply_direct, by contrast, is looked up by name from within
    those wrappers' own bodies at call time, so patching it here is
    reliably observed."""
    orig_apply_direct = scheme._apply_direct
    calls = []

    def traced(proc, args, env):
        calls.append(args)
        return orig_apply_direct(proc, args, env)

    scheme._apply_direct = traced
    try:
        scheme.execute_string_rm("""
        (define (add1e y) (+ y 1))
        (define (use-map3 lst) (map add1e lst))
        (define (use-for-each3 lst) (for-each add1e lst))
        (define (warmup-hof lst) (use-map3 lst) (use-for-each3 lst))
        (warmup-hof (list 1 2))
        """)
        result = scheme.execute_string_rm("(warmup-hof (list 10 20 30))")

        assert not calls, (
            "_apply_direct was invoked while running map/for-each over an "
            "inline callback, but the calling function should never have "
            "reached Phase 2 at all (it's uncertified) -- either "
            "_is_phase2_safe's exclusion of map/for-each has changed, or "
            "something else now routes this call through Phase 2's "
            "fast-prim dispatch instead of the classic trampoline's own "
            f"native map/for-each. Traced calls: {calls!r}"
        )
    finally:
        scheme._apply_direct = orig_apply_direct
