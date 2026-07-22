"""
Guard test for an invariant that _phase2_safe_walk_call (Scheme.py) relies
on but doesn't itself enforce: every _FAST_PRIM_SPECS entry EXCEPT 'map'/
'for-each' is treated as unconditionally safe to call from inside an
already-certified Phase 2 attempt -- see _phase2_safe_walk_call's own
comment ("a known-pure _fast_prim_map primitive (excluding map/for-each,
whose safety additionally depends on their callback argument...)"). That
means apply_proc (Scheme.py:602-643) assumes those primitives can NEVER
raise _TrampolineFallback partway through a certified closure's body.

Why that matters: apply_proc only un-does a live Phase-2 attempt (falls
back to the trampoline, re-running the whole closure from the top) when
_eval_sequence_direct raises _TrampolineFallback *before* execution starts
(the arity/extend step). Once the body itself starts running,
_TrampolineFallback is no longer treated as "safe to retry" -- it
propagates as a loud failure instead (see the comment above that try/
except in apply_proc), specifically because Phase 8 found that silently
retrying after some statements had already run re-executes their side
effects. So if a _fast_prim_map entry (other than map/for-each, which are
separately excluded from certification via _fast_prim_hof_fns precisely
because their callback can raise it) ever raised _TrampolineFallback, a
multi-statement closure body containing an earlier side effect followed by
a call to that primitive would have no path back to "retry safely" -- the
whole point of _is_phase2_safe's up-front certification is that this
never needs to happen for anything it certifies.

This doesn't prove the invariant holds for primitives added later -- it
fuzzes each *current* entry with wrong arities and wrong-typed arguments
(the only two ways Scheme source can misuse a primitive) and asserts none
of that ever raises _TrampolineFallback. Any other exception
(_SchemeRuntimeError, a raw Python exception, ...) is expected and fine:
apply_proc only special-cases _TrampolineFallback, so anything else
propagates as an ordinary, non-retried Scheme-level error.
"""
import calysto_scheme.scheme as scheme

# map/for-each are the deliberate, documented exception (see module
# docstring and the "Higher-order (propagate _TrampolineFallback if proc
# not directly evaluable)" comment above their _FAST_PRIM_SPECS entries):
# their callback argument runs through _apply_direct, which ends with
# `raise _TrampolineFallback()` for anything it can't evaluate directly.
_HOF_NAMES = ('map', 'for-each')

# A small, deliberately heterogeneous pool of "wrong type for this
# position" values covering every Scheme type a fast-prim might assume:
# numbers, a string, booleans, a foreign Python object, a symbol, an
# empty list, an improper pair, a proper list, and vectors (plain lists).
_WILD_VALUES = [
    -1, 3.5,
    "x",
    True, False,
    None,
    scheme.symbol_emptylist,
    scheme.make_symbol("x"),
    scheme.cons(1, 2),
    scheme.cons(1, scheme.cons(2, scheme.symbol_emptylist)),
    [1, 2, 3],
    [],
]

_NEUTRAL = 0


def _fast_prim_callable(name):
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
    assert binding is not False, f"{name!r} not bound in toplevel_env"
    proc = scheme.binding_value(binding)
    assert isinstance(proc, tuple) and proc[0] is scheme.symbol_procedure
    if scheme._fast_prim_map is None:
        scheme._fast_prim_map = scheme._build_fast_prim_map()
    direct = scheme._fast_prim_map.get(proc[1])
    assert direct is not None, f"{name!r} has no _fast_prim_map entry"
    return direct


def _arities_to_probe(name):
    """A handful of argument counts around the primitive's declared
    bounds: one too few, exactly the min, exactly the (bounded) max, and
    one too many -- enough to exercise both the arity-check wrapper and,
    for in-range counts, the primitive's own body."""
    min_n, max_n = scheme._FAST_PRIM_ARITY.get(name, (0, None))
    hi = max_n if max_n is not None else min_n + 2
    candidates = {min_n, hi, hi + 1}
    if min_n > 0:
        candidates.add(min_n - 1)
    return sorted(n for n in candidates if n >= 0)


def _arg_lists_for(name):
    """For each arity to probe, an all-neutral baseline plus, for every
    argument position, one call per wild value substituted into just that
    position (rest held neutral) -- catches "assumed this arg is already
    the right type" bugs without a full cartesian-product blowup."""
    out = []
    for n in _arities_to_probe(name):
        out.append([_NEUTRAL] * n)
        for i in range(n):
            for wild in _WILD_VALUES:
                args = [_NEUTRAL] * n
                args[i] = wild
                out.append(args)
    return out


def test_fast_prims_never_raise_trampoline_fallback():
    offenders = []
    for name in scheme._FAST_PRIM_SPECS:
        if name in _HOF_NAMES:
            continue
        fn = _fast_prim_callable(name)
        for args in _arg_lists_for(name):
            try:
                fn(list(args))
            except scheme._TrampolineFallback:
                offenders.append((name, args))
            except Exception:
                pass  # any other exception is fine -- see module docstring
    assert not offenders, (
        "these fast-prim calls raised _TrampolineFallback, which "
        "apply_proc cannot safely tolerate mid-body once a Phase 2 "
        "attempt has started (see module docstring): "
        + ", ".join(f"{name}{args!r}" for name, args in offenders)
    )


def test_hof_fast_prims_stay_excluded_from_safety_certification():
    """map/for-each are the one documented exception to the invariant
    above -- confirm they're still recorded in _fast_prim_hof_fns (and
    therefore excluded by _phase2_safe_walk_call), not just currently
    well-behaved by coincidence."""
    if scheme._fast_prim_map is None:
        scheme._fast_prim_map = scheme._build_fast_prim_map()
    for name in _HOF_NAMES:
        binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
        proc = scheme.binding_value(binding)
        assert proc[1] in scheme._fast_prim_hof_fns, (
            f"{name!r} must stay in _fast_prim_hof_fns -- its callback "
            "argument can raise _TrampolineFallback via _apply_direct, so "
            "_phase2_safe_walk_call must keep refusing to certify it"
        )
