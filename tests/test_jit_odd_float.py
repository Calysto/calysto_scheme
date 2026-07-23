"""
Regression test for a real divergence found by tests/test_jit_fuzz.py's
differential fuzzer: _JitCompiler inlined `odd?` as a raw Python
`{0} % 2 != 0` template, but the real primitive (odd_q in Scheme.py) is
`n % 2 == 1`. The two formulas agree for every integer (n % 2 is always
exactly 0 or 1 there) but silently diverge for a non-integer argument --
e.g. `(odd? 2.5)`: `2.5 % 2 == 0.5`, so `!= 0` is True (wrongly "odd")
while `== 1` is False (correct, matching the real primitive and the
trampoline). even?/zero?/not/abs were each checked against their real
primitive's implementation at the same time and confirmed to already be
exact matches -- this was the one drifted template.

A self-recursive tail loop is used (rather than a bare top-level call) so
the case is actually eligible for JIT compilation at all -- see
_is_direct_eval_safe / _is_phase2_safe's gating.
"""
import calysto_scheme.scheme as scheme


def _run(src):
    result = scheme.execute_string_rm(src)
    assert not scheme.exception_q(result), scheme.get_traceback_string(result)
    return result


def test_odd_on_a_non_integer_matches_the_real_primitive():
    assert scheme.odd_q(2.5) is False
    src = (
        "(define (f n acc)\n"
        "  (if (<= n 0) acc\n"
        "      (f (- n 1) (odd? 2.5))))\n"
        "(f 3 'unused)"
    )
    assert _run(src) is False


def test_odd_on_a_non_integer_matches_the_trampoline_forced_off():
    # Same case, JIT forced off -- confirms the fix is specifically about
    # _JitCompiler's inlining, not some other shared bug in odd_q itself.
    old = scheme._jit_compile_proc
    scheme._jit_compile_proc = lambda proc: None
    try:
        src = (
            "(define (f n acc)\n"
            "  (if (<= n 0) acc\n"
            "      (f (- n 1) (odd? 2.5))))\n"
            "(f 3 'unused)"
        )
        assert _run(src) is False
    finally:
        scheme._jit_compile_proc = old


def test_odd_still_correct_for_ordinary_integers():
    src = (
        "(define (f n)\n"
        "  (if (<= n 0) (odd? n)\n"
        "      (f (- n 1))))\n"
        "(f 7)"
    )
    assert _run(src) is False   # (odd? 0) -> #f
