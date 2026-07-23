"""
Guard tests for a design asymmetry between apply_proc's outermost gate
into Phase 2 and _apply_direct's gate -- and a regression pin for the fix.

apply_proc (Scheme.py) only starts a live Phase-2 attempt after
_is_phase2_safe(proc) -- a transitive, whole-call-graph certification
that recursively requires every closure `proc` (directly or indirectly)
calls to *also* be phase2-safe (see _phase2_safe_walk_call: a call to
another user closure is only accepted if _is_phase2_safe(that closure)
is True too). _apply_direct -- used by _fast_prim_direct_map/_for_each
and as _jit_call's fallback for a computed-operator call -- used to gate
entry into the exact same _eval_sequence_direct with only proc[5]
(_is_direct_eval_safe): a shallow, single-closure check for "no literal
set! in THIS closure's own body", with no visibility into what the
closure calls.

Concretely: a closure whose own body has no set! (proc[5] is True) but
that calls a *different* closure which does use set! is proc[5]-safe but
NOT _is_phase2_safe-safe -- apply_proc correctly refuses to ever start
Phase 2 for it, but _apply_direct's old, cheaper check let it begin
running anyway, only to hit _TrampolineFallback when it reached the
set!-using callee (after already running any of outer's own preceding
statements). Fixed by requiring the same _is_phase2_safe(proc)
certification apply_proc already requires, so _apply_direct now refuses
up front too -- see Scheme.py's _apply_direct.

Confirmed the *old* gap was unreachable from any real program, by the
same mechanism documented in test_phase2_hof_unreachability.py: Phase
2/JIT execution can only ever be entered via apply_proc's fully
transitive gate, and _is_phase2_safe's recursive walk means a call to a
set!-using closure poisons every transitive caller, all the way up to
whatever apply_proc was asked to start -- so the outer closure in this
scenario is itself never phase2-safe either, and apply_proc never starts
Phase 2 for it in the first place. _apply_direct's old weaker check was
therefore already dead code for this scenario, exactly like the
map/for-each and apply-twice gaps already pinned down -- but it was a
real gap in _apply_direct's own contract regardless, and a future JIT
change (e.g. loosening _is_phase2_safe, or adding a new _jit_call
dispatch path) could have made it reachable without anyone realizing
this check was ever too weak. These tests exist to:

  1. Pin down the asymmetry that motivated the fix (proc[5] True,
     _is_phase2_safe False) so it's not lost/forgotten.
  2. Confirm _apply_direct's fixed gate now refuses up front, before
     running any of the closure's own statements -- strictly stronger
     than just "doesn't double-execute": zero side effects, not one.
  3. Confirm that if this ever becomes reachable through the real
     trampoline anyway (simulated here by directly patching
     _is_phase2_safe, standing in for whatever future change might do
     this for real), the result is a catchable Scheme-level exception
     object -- not a raw Python traceback escaping the interpreter, and
     not a silently wrong answer -- and the side effect still only ran
     once. This is defense in depth: it still holds even with
     _apply_direct's own fixed check bypassed by the same patch.
  4. Confirm the scenario is unreachable via a realistic, warmup-driven
     program, mirroring test_phase2_hof_unreachability.py's own dynamic
     check.
"""
import calysto_scheme.scheme as scheme


def _binding(name):
    return scheme.binding_value(scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name)))


def _define_scenario(suffix):
    """A closure (`outer`) with no set! of its own (proc[5] True) that
    does one side effect, then calls a different closure (`inner`) which
    DOES use set! (so _is_phase2_safe(inner) -- and therefore
    _is_phase2_safe(outer) too -- is False)."""
    scheme.execute_string_rm("""
    (define z{s} 0)
    (define (inner{s})
      (set! z{s} (+ z{s} 1))
      z{s})
    (define counter{s} (vector 0))
    (define (outer{s} x)
      (vector-set! counter{s} 0 (+ (vector-ref counter{s} 0) 1))
      (inner{s}))
    """.replace("{s}", suffix))
    return _binding("outer" + suffix), _binding("inner" + suffix)


def test_proc5_is_true_but_is_phase2_safe_is_false_for_the_same_closure():
    outer, inner = _define_scenario("_a")
    assert outer[scheme._PROC_SAFE] is True, (
        "outer's own body has no set!, so the shallow proc[5] check "
        "_apply_direct relies on should consider it safe"
    )
    assert scheme._is_phase2_safe(outer) is False, (
        "outer transitively calls a set!-using closure, so apply_proc's "
        "real gate must refuse to ever start Phase 2 for it -- if this "
        "becomes True, the whole premise of this test file is stale"
    )
    assert scheme._is_phase2_safe(inner) is False


def test_apply_direct_now_refuses_before_any_side_effect_runs():
    """Regression pin for the fix: _apply_direct now requires
    _is_phase2_safe(proc), not just proc[5], before starting
    _eval_sequence_direct -- so it refuses outer entirely, before outer's
    own vector-set! ever runs, rather than running that one statement and
    then failing on the call to inner. Strictly stronger than "doesn't
    double-execute": zero side effects, not one."""
    outer, inner = _define_scenario("_b")

    outcome = "no exception"
    try:
        scheme._apply_direct(outer, [1], None)
    except scheme._TrampolineFallback:
        outcome = "_TrampolineFallback"
    except Exception as e:
        outcome = repr(e)

    assert outcome == "_TrampolineFallback", (
        f"expected _apply_direct to refuse outer outright (proc[5] is "
        f"True but _is_phase2_safe(outer) is False) -- got {outcome!r} "
        f"instead"
    )

    # The critical correctness property: NEITHER side effect ran at all --
    # not outer's own vector-set! (which the old, weaker gate let through
    # before failing on the call to inner) and not inner's set! (which
    # never got a chance to start either way).
    assert scheme.vector_ref(_binding("counter_b"), 0) == 0, (
        "outer's side effect must not run at all -- the fixed gate must "
        "refuse before _eval_sequence_direct executes a single statement, "
        "not partway through outer's own body"
    )
    assert _binding("z_b") == 0, (
        "inner's set! must never have run -- _apply_direct raises before "
        "inner's body starts, so it must have no partial effect"
    )


def test_forcing_the_gap_through_the_real_trampoline_fails_loud_not_silent():
    """Simulates this gap becoming reachable for real (standing in for
    whatever future change might loosen apply_proc's gate) by patching
    _is_phase2_safe to unconditionally agree with proc[5]. Confirms the
    interpreter's own top-level error handling -- not this test -- is
    what makes the failure safe: a catchable Scheme exception object,
    not a raw Python exception escaping execute_string_rm, and no
    duplicated side effect. Defense in depth: since _apply_direct's own
    gate now also calls _is_phase2_safe, patching the same name bypasses
    that fix too here -- so this still exercises "what if the safety net
    itself is the thing that's broken," not just apply_proc alone."""
    outer, inner = _define_scenario("_c")

    orig = scheme._is_phase2_safe
    scheme._is_phase2_safe = lambda proc, *a, **kw: True
    try:
        result = scheme.execute_string_rm("(outer_c 1)")
    finally:
        scheme._is_phase2_safe = orig

    assert scheme.exception_q(result), (
        f"expected a Scheme-level exception object, got: {result!r} -- "
        "either a raw Python exception escaped, or this silently "
        "produced a value instead of failing loud"
    )
    assert scheme.vector_ref(_binding("counter_c"), 0) == 1, (
        "even routed through the real trampoline, the side effect must "
        "run exactly once -- not duplicated by any retry"
    )
    assert _binding("z_c") == 0


def test_scenario_is_currently_unreachable_via_realistic_warmup():
    """Dynamic confirmation, unpatched, mirroring
    test_phase2_hof_unreachability.py's own reachability check: even
    when outer is exercised through a realistic warmup loop (the pattern
    used elsewhere in this suite to force JIT compilation), _apply_direct
    is never actually invoked for it, and the program produces the
    correct answer via the classic trampoline instead."""
    outer, inner = _define_scenario("_d")
    scheme.execute_string_rm("""
    (define (apply-once_d f x) (f x))
    (define (warmup_d n)
      (if (= n 0)
          0
          (begin (apply-once_d outer_d n) (warmup_d (- n 1)))))
    """)

    orig_apply_direct = scheme._apply_direct
    calls = []

    def traced(proc, args, env):
        calls.append(args)
        return orig_apply_direct(proc, args, env)

    scheme._apply_direct = traced
    try:
        result = scheme.execute_string_rm("(warmup_d 50000)")
    finally:
        scheme._apply_direct = orig_apply_direct

    assert not calls, (
        "_apply_direct was invoked for a real program -- the gap this "
        "file documents has become reachable; the crash-safety tests "
        "above still apply, but this specific scenario is no longer "
        "dead code and README-PERFORMANCE.md / JIT-OVERVIEW.md should "
        f"be updated to say so. Traced calls: {calls!r}"
    )
    assert scheme.vector_ref(_binding("counter_d"), 0) == 50000
    assert _binding("z_d") == 50000
