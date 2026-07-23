"""
Regression tests for a bug found while investigating whether
_JitCompiler's inlining/_fast_prim_map's wrappers for numeric operators
actually match the classic dispatch's reference semantics.

The classic dispatch for `<`, `>`, `<=`, `>=`, `=` (b_proc_95_d..
b_proc_99_d in the generated scheme.py) has always had an arity check
requiring "at least 2" args -- confirming the intended design was always
N-ary chained comparison (a < b < c < ...), matching standard Scheme.
But LessThan/GreaterThan/LessThanEqual/GreaterThanEqual (Scheme.py) were
fixed 2-parameter Python functions with no chaining logic, so
Apply(LessThan, args_reg) crashed with a raw, internal-looking Python
TypeError the moment the arity check let 3+ args through -- for every
one of the four inequality operators, on every call path (classic
dispatch and Phase 2/JIT alike, since they all ultimately call these
same functions).

`=` was the interesting exception: _fast_prim_numeric_equal (Phase 2/
JIT's fast-prim wrapper) already correctly implemented N-ary chaining
by looping over adjacent pairs -- but the classic dispatch still called
2-arg-only numeric_equal via Apply and crashed exactly like the others.
Confirmed directly before this fix: a function using (= a b c), reached
through Phase 2/JIT via a nested-call warmup, returned the correct
boolean, while the identical expression at the top level (classic
dispatch, no Phase 2/JIT involved) crashed -- the same program's
behavior silently depended on invisible internal state (whether the
calling closure happened to be Phase-2-certified).

Fixed by making numeric_equal/LessThan/LessThanEqual/GreaterThanEqual/
GreaterThan all properly N-ary (Scheme.py), with a fast path for the
overwhelmingly common 2-arg case (measured ~1.5x overhead there, vs ~5x
for a naive single-*args implementation) -- see their own comment in
Scheme.py for why that mattered (numeric_equal in particular is used in
b_proc_1_d's own per-call arity check, on every classic-trampoline
function call).
"""
import calysto_scheme.scheme as scheme


def test_classic_dispatch_nary_less_than():
    assert scheme.execute_string_rm("(< 1 2 3)") is True
    assert scheme.execute_string_rm("(< 3 2 1)") is False
    assert scheme.execute_string_rm("(< 1 3 2)") is False, (
        "not just checking the endpoints -- the middle element must also "
        "be in order"
    )


def test_classic_dispatch_nary_greater_than():
    assert scheme.execute_string_rm("(> 3 2 1)") is True
    assert scheme.execute_string_rm("(> 1 2 3)") is False


def test_classic_dispatch_nary_less_than_equal():
    assert scheme.execute_string_rm("(<= 1 1 2)") is True
    assert scheme.execute_string_rm("(<= 1 2 2)") is True
    assert scheme.execute_string_rm("(<= 1 2 1)") is False


def test_classic_dispatch_nary_greater_than_equal():
    assert scheme.execute_string_rm("(>= 3 3 2)") is True
    assert scheme.execute_string_rm("(>= 2 3 1)") is False


def test_classic_dispatch_nary_numeric_equal():
    assert scheme.execute_string_rm("(= 1 1 1)") is True
    assert scheme.execute_string_rm("(= 1 1 2)") is False


def test_ordinary_two_argument_comparisons_still_work():
    """Control: the N-ary fix must not disturb the overwhelmingly common
    2-argument case."""
    assert scheme.execute_string_rm("(< 1 2)") is True
    assert scheme.execute_string_rm("(< 2 1)") is False
    assert scheme.execute_string_rm("(= 1 1)") is True
    assert scheme.execute_string_rm("(= 1 2)") is False
    assert scheme.execute_string_rm("(>= 2 2)") is True


def test_too_few_args_still_gives_a_clean_arity_error_not_a_raw_typeerror():
    for src in ("(< 1)", "(<)", "(= 5)"):
        result = scheme.execute_string_rm(src)
        assert scheme.exception_q(result), f"{src} should be an exception"
        exc_obj = result.cdr.car
        etype = str(exc_obj.cdr.car)
        assert etype == "RunTimeError", (
            f"{src} gave a {etype!r} exception, expected a clean "
            "RunTimeError ('incorrect number of arguments to ...'), not a "
            "raw 'Unhandled TypeError' leaking an internal Python message"
        )


def test_nary_comparisons_work_through_the_jit_compiled_path_too():
    """Forces the JIT-compiled path via a nested-call warmup (a top-level
    call alone never uses a compiled function -- see _jit_lookup's
    callers), confirming the fix applies uniformly, not just to the
    classic dispatch."""
    scheme.execute_string_rm("""
    (define (three-equal a b c) (= a b c))
    (define (three-lt a b c) (< a b c))
    (define (warmup-nary a b c) (list (three-equal a b c) (three-lt a b c)))
    (warmup-nary 1 1 1)
    """)
    eq_proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("three-equal")))
    lt_proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("three-lt")))
    assert scheme._jit_lookup(eq_proc) is not None, (
        "sanity check: three-equal should have JIT-compiled on warmup"
    )
    assert scheme._jit_lookup(lt_proc) is not None, (
        "sanity check: three-lt should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("(warmup-nary 1 2 3)")
    cur = result
    values = []
    while isinstance(cur, scheme.cons):
        values.append(cur.car)
        cur = cur.cdr
    assert values == [False, True], (
        f"got {values!r}, expected [#f, #t] for (= 1 2 3) and (< 1 2 3)"
    )


def test_numeric_equal_no_longer_diverges_between_classic_and_jit_paths():
    """The acute bug: (= a b c) used to give a correct answer via Phase 2/
    JIT (which already had N-ary support in _fast_prim_numeric_equal) but
    crash via the classic dispatch -- a behavioral difference driven
    entirely by invisible internal state (was the calling closure
    Phase-2-certified). Confirms both paths now agree."""
    scheme.execute_string_rm("""
    (define (check-equal a b c) (= a b c))
    (define (warmup-check-equal a b c) (check-equal a b c))
    (warmup-check-equal 1 1 1)
    """)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("check-equal")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: check-equal should have JIT-compiled on warmup"
    )

    jit_result = scheme.execute_string_rm("(warmup-check-equal 1 2 3)")
    classic_result = scheme.execute_string_rm("(= 1 2 3)")
    assert jit_result is classic_result is False, (
        f"JIT path gives {jit_result!r}, classic dispatch gives "
        f"{classic_result!r} -- they must agree (both should be #f)"
    )
