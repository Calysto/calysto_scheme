"""
Regression tests for a bug found by systematically cross-checking every
_FAST_PRIM_ARITY entry claiming an unbounded max against what its
implementation actually uses (Scheme.py) -- the same method that found
the N-ary comparison crash (see test_nary_comparisons.py), but this time
the flavor was different: no crash, no divergence between paths, just
silent, wrong data loss.

zero?, expt, memv, and assv each had a `max=None` (unbounded) entry in
_FAST_PRIM_ARITY, but their implementations only ever read the first 1
(zero?) or 2 (expt/memv/assv) elements of args -- any extra arguments
were silently discarded rather than raising an arity error. Confirmed
this was not a Phase 2/JIT-specific issue: the classic dispatch
(b_proc_7_d, b_proc_11_d, b_proc_32_d, b_proc_33_d in the generated
scheme.py) had the exact same looseness at the source level
(length-at-least?, "at least N", not an exact check) -- e.g.
(zero? 0 1) silently returned #t on every path, ignoring the second
argument entirely, instead of raising a clean arity error.

Fixed at the source level (source-rm.ss: a new length-exactly? helper,
used by <proc-7>/<proc-11>/<proc-32>/<proc-33>) so the classic dispatch
and _FAST_PRIM_ARITY (Scheme.py) agree, both now requiring the exact
arity these primitives were always documented and intended to have
(zero? takes 1 argument; expt/memv/assv take exactly 2).
"""
import calysto_scheme.scheme as scheme


def _exception_type(result):
    assert scheme.exception_q(result), f"expected an exception, got {result!r}"
    return str(result.cdr.car.cdr.car)


def test_zero_q_rejects_extra_arguments():
    assert scheme.execute_string_rm("(zero? 0)") is True
    result = scheme.execute_string_rm("(zero? 0 1)")
    assert _exception_type(result) == "RunTimeError", (
        f"(zero? 0 1) should be a clean arity error, got {result!r} -- the "
        "pre-fix bug silently ignored the second argument and returned #t"
    )


def test_expt_rejects_extra_arguments():
    assert scheme.execute_string_rm("(expt 2 3)") == 8
    result = scheme.execute_string_rm("(expt 2 3 999)")
    assert _exception_type(result) == "RunTimeError", (
        f"(expt 2 3 999) should be a clean arity error, got {result!r} -- "
        "the pre-fix bug silently ignored the third argument"
    )


def test_memv_rejects_extra_arguments():
    result_ok = scheme.execute_string_rm("(memv 2 (list 1 2 3))")
    assert not scheme.exception_q(result_ok)
    result = scheme.execute_string_rm("(memv 2 (list 1 2 3) 999)")
    assert _exception_type(result) == "RunTimeError", (
        f"(memv 2 (list 1 2 3) 999) should be a clean arity error, got "
        f"{result!r} -- the pre-fix bug silently ignored the third argument"
    )


def test_assv_rejects_extra_arguments():
    result_ok = scheme.execute_string_rm("(assv 1 (list (list 1 'a)))")
    assert not scheme.exception_q(result_ok)
    result = scheme.execute_string_rm("(assv 1 (list (list 1 'a)) 999)")
    assert _exception_type(result) == "RunTimeError", (
        f"(assv 1 (list (list 1 'a)) 999) should be a clean arity error, "
        f"got {result!r} -- the pre-fix bug silently ignored the third "
        "argument"
    )


def test_arity_error_also_applies_through_the_jit_compiled_path():
    """Forces the JIT-compiled path via a nested-call warmup (a top-level
    call alone never uses a compiled function -- see _jit_lookup's
    callers), confirming the fix applies uniformly, not just to the
    classic dispatch that the tests above exercise directly."""
    scheme.execute_string_rm("""
    (define (call-zero-extra) (zero? 0 1))
    (define (warmup-zero-extra) (call-zero-extra))
    (warmup-zero-extra)
    """)
    result = scheme.execute_string_rm("(warmup-zero-extra)")
    assert _exception_type(result) == "RunTimeError", (
        f"nested-call (zero? 0 1) should be a clean arity error, got "
        f"{result!r}"
    )
