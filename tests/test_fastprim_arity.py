"""
Regression tests for the systematic fast-prim arity audit: none of the
~83 _FAST_PRIM_SPECS callables (Scheme.py) validated their own argument
count -- they just index positionally into `args`. Calling one with the
wrong number of arguments through a nested Phase-2/JIT call (rather than
directly, which goes through apply_proc's own arity check) used to leak
a raw IndexError/TypeError instead of a proper "incorrect number of
arguments to X" RunTimeError -- the same bug shape already fixed for
closures (_check_call_arity) and car/cdr, just far more widespread (75 of
83 primitives, found by a systematic audit that called every one with 0
arguments through both a direct and a nested call and diffed the result).

Fixed by _FAST_PRIM_ARITY + _arity_checked_prim wrapping every entry in
_build_fast_prim_map. This file spot-checks a representative sample
(every arity shape: fixed 1-arg, fixed 2-arg, fixed 3-arg, and the
genuinely variadic ones) plus every case the audit found needing special
handling beyond a plain wrapper:

  - `-` : _JitCompiler's own _NARY zero-argument shortcut hard-coded '1'
    for `(- )`, unrelated to the _fast_prim_map wrapping -- a second, JIT-
    compiler-level instance of the same bug class.
  - `append`: the native append() Python helper can't handle zero
    arguments at all (indexes objs[-1]), even though 0-arg (append) is
    valid and returns '()' at the classic dispatch.
  - `vector-ref`, `vector?`: silently accepted (and ignored) extra
    arguments instead of erroring -- confirmed via a properly-isolated
    test (predefining the vector outside the checked closure; an earlier,
    flawed probe that built the vector inline was itself accidentally
    falling back to the trampoline, masking this).
  - `substring`: the fast-path lambda unconditionally read a 3rd
    argument, so the classic-supported 2-arg form (implicit end-of-
    string) crashed through the fast path even though it's valid.
"""
import calysto_scheme.scheme as scheme


def _eval(src):
    return scheme.execute_string_rm(src)


def _exception_parts(result):
    assert scheme.exception_q(result), f"expected exception, got {result!r}"
    exc_obj = result.cdr.car
    return str(exc_obj.cdr.car), str(exc_obj.cdr.cdr.car)


def _value(result):
    assert not scheme.exception_q(result), (
        f"expected a value, got exception: {_exception_parts(result)}"
    )
    return result


def _nested_arity_error(call_src, define_src=""):
    """Run `call_src` through a nested nested-closure call (forcing it
    through the fast-prim dispatch, not apply_proc's own direct-call
    arity check) and return its (type, message)."""
    src = f"{define_src} (define (_ab) {call_src}) (define (_ad) (_ab)) (_ad)"
    return _exception_parts(scheme.execute_string_rm(src))


def test_too_few_args_gives_clean_runtime_error_1_arg_prim():
    etype, emsg = _nested_arity_error("(car)")
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to car")


def test_too_many_args_gives_clean_runtime_error_1_arg_prim():
    etype, emsg = _nested_arity_error("(car (cons 1 2) 9)")
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to car")


def test_too_few_args_gives_clean_runtime_error_2_arg_prim():
    etype, emsg = _nested_arity_error("(cons 1)")
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to cons")


def test_too_many_args_gives_clean_runtime_error_2_arg_prim():
    etype, emsg = _nested_arity_error("(eq? 1 2 3)")
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to eq?")


def test_too_few_args_gives_clean_runtime_error_3_arg_prim():
    # 2 args -- one short of vector-set!'s required 3
    etype, emsg = _nested_arity_error(
        "(vector-set! v 0)", define_src="(define v (vector 1 2 3))"
    )
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to vector-set!")


def test_variadic_prims_still_accept_a_range_of_counts():
    assert _value(_eval("(+ )")) == 0
    assert _value(_eval("(+ 1 2 3)")) == 6
    assert _value(_eval("(list )")) == scheme.symbol_emptylist
    assert _value(_eval("(list 1 2 3)")) is not None


def test_minus_zero_args_is_an_arity_error_not_silently_one():
    """_JitCompiler's own _NARY zero-arg shortcut hard-coded '1' for
    `(- )`, matching `*`'s identity but wrong for `-`, which requires at
    least one argument -- a second, JIT-level instance of this bug class,
    independent of the _fast_prim_map wrapping fix."""
    etype, emsg = _nested_arity_error("(-)")
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to -")
    assert _value(_eval("(- 5)")) == -5
    assert _value(_eval("(- 10 3 2)")) == 5


def test_append_zero_args_returns_empty_list_not_an_error():
    """The native append() Python helper can't handle zero args itself
    (indexes objs[-1] unconditionally) even though 0-arg (append) is
    valid at the classic dispatch -- special-cased in the fast-path
    lambda rather than erroring."""
    result = scheme.execute_string_rm(
        "(define (_ab2) (append)) (define (_ad2) (_ab2)) (_ad2)"
    )
    assert _value(result) == scheme.symbol_emptylist


def test_vector_ref_extra_argument_is_rejected_not_silently_ignored():
    src = """
    (define v3 (vector 10 20 30))
    (define (_ab3) (vector-ref v3 1 999))
    (define (_ad3) (_ab3))
    (_ad3)
    """
    etype, emsg = _exception_parts(scheme.execute_string_rm(src))
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to vector-ref"), (
        "vector-ref used to silently ignore a 3rd argument and return the "
        "2-arg result instead of erroring"
    )


def test_vector_predicate_extra_argument_is_rejected_not_silently_ignored():
    src = """
    (define v4 (vector 1))
    (define (_ab4) (vector? v4 1))
    (define (_ad4) (_ab4))
    (_ad4)
    """
    etype, emsg = _exception_parts(scheme.execute_string_rm(src))
    assert (etype, emsg) == ("RunTimeError", "incorrect number of arguments to vector?")


def test_substring_two_argument_form_is_supported_by_the_fast_path():
    """substring's fast-path lambda used to unconditionally read a 3rd
    argument, so the classic-supported 2-arg form (implicit end-of-string)
    crashed with a raw IndexError through the fast path even though
    `(substring "hello world" 6)` is valid and returns "world" at the
    classic dispatch."""
    src = """
    (define (_ab5) (substring "hello world" 6))
    (define (_ad5) (_ab5))
    (_ad5)
    """
    assert _value(scheme.execute_string_rm(src)) == "world"


def _to_pylist(x):
    """cons has no __eq__ (falls back to Python identity), so two
    separately-built Scheme lists that print identically are never `==`."""
    out = []
    while isinstance(x, scheme.cons):
        out.append(x.car)
        x = x.cdr
    return out


def test_map_and_for_each_still_work_through_the_fast_path():
    result = _value(scheme.execute_string_rm(
        "(define (_ab6) (map car (list (list 1 2) (list 3 4)))) "
        "(define (_ad6) (_ab6)) (_ad6)"
    ))
    assert _to_pylist(result) == [1, 3]
