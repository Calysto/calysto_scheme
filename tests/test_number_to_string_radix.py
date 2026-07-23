"""
Regression test for a missing-feature bug found while systematically
cross-checking _FAST_PRIM_ARITY entries against their implementations
(see test_arity_tightening.py, test_nary_comparisons.py): number->string
claimed (1, None) -- accepting an optional radix argument -- but
number_to_string(number) (Scheme.py) never had a radix parameter at all,
on any path. (number->string 255 16) silently returned "255" instead of
"ff", with no error -- the radix argument was accepted syntactically and
then completely discarded.

This is a standalone missing-feature bug, not a Phase 2/JIT-vs-classic
divergence or an unreachable-code issue: the classic dispatch
(b_proc_31_d / <proc-31> in source-rm.ss) had the identical gap.

Fixed by giving number_to_string an optional radix parameter (default
10, using the existing str(number) for every type str() supports) that
for radix 2/8/16 requires an exact integer (matching R7RS: non-decimal
radixes are only defined for exact integers) and formats accordingly.
Also tightened the arity from (1, None) to (1, 2) at both the classic
dispatch (source-rm.ss, length-between? 1 2) and _FAST_PRIM_ARITY
(Scheme.py), since a 3rd argument would otherwise have been silently
ignored the same way -- see test_arity_tightening.py for that bug's
general shape.
"""
import calysto_scheme.scheme as scheme


def _exception_type(result):
    assert scheme.exception_q(result), f"expected an exception, got {result!r}"
    return str(result.cdr.car.cdr.car)


def test_default_radix_is_unchanged():
    assert scheme.execute_string_rm("(number->string 255)") == "255"
    assert scheme.execute_string_rm("(number->string 3.5)") == "3.5"


def test_hex_octal_binary_radixes():
    assert scheme.execute_string_rm("(number->string 255 16)") == "ff"
    assert scheme.execute_string_rm("(number->string 255 8)") == "377"
    assert scheme.execute_string_rm("(number->string 255 2)") == "11111111"


def test_negative_integers_keep_their_sign():
    assert scheme.execute_string_rm("(number->string -255 16)") == "-ff"


def test_non_integer_with_non_decimal_radix_is_a_clean_error():
    result = scheme.execute_string_rm("(number->string 3.5 16)")
    assert _exception_type(result) == "RunTimeError", (
        f"a non-decimal radix on a non-integer should be a clean "
        f"RunTimeError (R7RS: non-decimal radixes are only defined for "
        f"exact integers), got {result!r}"
    )


def test_unsupported_radix_is_a_clean_error():
    result = scheme.execute_string_rm("(number->string 255 7)")
    assert _exception_type(result) == "RunTimeError", (
        f"radix 7 isn't one of 2/8/10/16, should be a clean error, got "
        f"{result!r}"
    )


def test_extra_arguments_are_a_clean_arity_error():
    result = scheme.execute_string_rm("(number->string 255 16 999)")
    assert _exception_type(result) == "RunTimeError", (
        f"a 3rd argument should be a clean arity error (the pre-fix "
        f"_FAST_PRIM_ARITY entry claimed unbounded args), got {result!r}"
    )


def test_radix_works_through_the_jit_compiled_path_too():
    """Forces the JIT-compiled path via a nested-call warmup (a top-level
    call alone never uses a compiled function -- see _jit_lookup's
    callers), confirming the fix applies to _fast_prim_map's wrapper too,
    not just the classic dispatch the tests above exercise directly."""
    scheme.execute_string_rm("""
    (define (to-hex n) (number->string n 16))
    (define (warmup-to-hex n) (to-hex n))
    (warmup-to-hex 1)
    """)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("to-hex")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: to-hex should have JIT-compiled on warmup"
    )
    result = scheme.execute_string_rm("(warmup-to-hex 255)")
    assert result == "ff", f"got {result!r}, expected 'ff'"
