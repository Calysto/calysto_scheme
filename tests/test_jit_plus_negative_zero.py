"""
Regression test for a divergence found while checking whether
_JitCompiler's raw-operator inlining (_NARY/_CMP) actually matches the
numeric semantics of plus()/minus()/multiply()/LessThan()/etc. -- the
same functions the classic dispatch and _fast_prim_map both call for
these names.

_NARY inlines `+` as a bare left-associative Python expression
(`a + b + ...`), but plus() computes
`functools.reduce(operator.add, args, 0)`, i.e. `0 + a + b + ...` -- an
explicit fold starting from 0. For ordinary numbers 0 + x == x exactly,
so this never mattered -- except for IEEE-754 negative zero, where
0.0 + -0.0 == 0.0 (positive), while -0.0 alone keeps its sign. Confirmed
directly before this fix existed, forcing the JIT-compiled path via a
nested-call warmup (a top-level call alone never uses a compiled
function -- see _jit_lookup's callers): (+ -0.0) gave 0.0 (positive) via
the classic dispatch, but -0.0 (negative) via the JIT-inlined version, at
both 1 and 2 arguments.

`-`/`*` don't have this problem: subtraction never folds from an
identity value at the start (minus() begins its reduce from args[0]
itself, not a prepended identity), and multiplying by 1 never flips a
sign the way adding 0 can (1 * -0.0 == -0.0, unlike 0 + -0.0 == 0.0).

Fixed by making the `+` inlining match plus()'s exact fold order:
`(0 + a + b + ...)` instead of `(a + b + ...)`.
"""
import math

import calysto_scheme.scheme as scheme


def _sign(x):
    return math.copysign(1, x)


def test_jit_inlined_plus_matches_classic_dispatch_for_negative_zero_one_arg():
    classic = scheme.execute_string_rm("(+ -0.0)")

    scheme.execute_string_rm("""
    (define (add-one-arg x) (+ x))
    (define (warmup-add-one x) (add-one-arg x))
    (warmup-add-one 1.0)
    """)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("add-one-arg")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: add-one-arg should have JIT-compiled on warmup"
    )

    jit_result = scheme.execute_string_rm("(warmup-add-one -0.0)")
    assert _sign(jit_result) == _sign(classic) == 1.0, (
        f"classic dispatch gives {classic!r} (sign {_sign(classic)}), JIT "
        f"gives {jit_result!r} (sign {_sign(jit_result)}) -- they must "
        "agree; the pre-fix bug produced -0.0 (negative) from the JIT "
        "path while the classic dispatch correctly gave 0.0 (positive)"
    )


def test_jit_inlined_plus_matches_classic_dispatch_for_negative_zero_two_args():
    classic = scheme.execute_string_rm("(+ -0.0 -0.0)")

    scheme.execute_string_rm("""
    (define (add-two-args x y) (+ x y))
    (define (warmup-add-two x y) (add-two-args x y))
    (warmup-add-two 1.0 1.0)
    """)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("add-two-args")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: add-two-args should have JIT-compiled on warmup"
    )

    jit_result = scheme.execute_string_rm("(warmup-add-two -0.0 -0.0)")
    assert _sign(jit_result) == _sign(classic) == 1.0, (
        f"classic dispatch gives {classic!r} (sign {_sign(classic)}), JIT "
        f"gives {jit_result!r} (sign {_sign(jit_result)}) -- they must agree"
    )


def test_jit_inlined_plus_still_computes_ordinary_sums_correctly():
    """Control: the fix (prepending 0 to the fold) must not change the
    result for ordinary, non-signed-zero numbers."""
    scheme.execute_string_rm("""
    (define (add-three x y z) (+ x y z))
    (define (warmup-add-three x y z) (add-three x y z))
    (warmup-add-three 1 2 3)
    """)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("add-three")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: add-three should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("(warmup-add-three 10 20 30)")
    assert result == 60, f"got {result!r}, expected 60 (10+20+30)"
