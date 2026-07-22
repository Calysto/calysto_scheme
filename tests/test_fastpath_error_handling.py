"""
Regression tests: an ordinary runtime error (wrong number of arguments,
calling `car` on a non-pair, ...) inside a function reached via a *nested*
call from already-running Phase 2/JIT code must raise the same kind of
Scheme condition it would if that function had been called directly --
a "RunTimeError" exception object with a language-level message -- not a
raw Python exception with implementation-internal names leaking through.

Found by testing directly, not by inspection: apply_proc checks arity
before entering Phase 2 for a *direct* call to a closure, and converts
any Python exception the trampoline itself raises into a proper
"RunTimeError"/"Unhandled <ClassName>" Scheme condition either way. But
none of that protects a *nested* call -- one closure, already executing
via Phase 2 or the JIT, calling another. Two call sites skip validation
entirely:

  - _eval_direct's own app_aexp dispatch (Scheme.py) calls a JIT-compiled
    function as `jit_fn(*args)` with no arity check, and calls a fast
    closure via _extend_direct (also no arity check) when Phase 2 handles
    the tail-call-reuse path with the JIT unavailable.
  - _jit_call (Scheme.py), used for computed/dynamic operator calls,
    has the same gap.

The result: the *same* user-level mistake produces a different exception
type and a different -- implementation-detail-leaking -- message,
purely depending on whether the call happened to be direct or nested:

    direct call, wrong arity:   RunTimeError / "incorrect number of
                                 arguments in application"
    nested call, wrong arity:   "Unhandled TypeError" / "_jit_fn() missing
                                 1 required positional argument: '_j_b'"
    nested call (JIT disabled): "Unhandled IndexError" / "list index out
                                 of range"
    direct call, car on ():     RunTimeError / "car called on non-pair ()"
    nested call, car on ():     "Unhandled AttributeError" / "no such
                                 attribute 'car' on '()'"

This is a bigger compatibility risk than it looks: it doesn't require
anything unusual (no redefined primitives, no exotic control flow) --
it fires on an ordinary bug in *any* function called from another
already-fast-path function, which is the common case once a program is
large enough for the JIT to matter at all. A `guard`/exception handler
that correctly catches an error when the code runs unoptimized can
silently stop catching it -- or crash the process -- purely because an
internal optimization heuristic decided to compile that particular
function.

These tests currently FAIL against the unfixed interpreter -- that is
expected; they pin down the target behavior for the fix.
"""
import calysto_scheme.scheme as scheme


def _eval(src):
    return scheme.execute_string_rm(src)


def _exception_parts(result):
    assert scheme.exception_q(result), (
        "expected a Scheme exception object, got a plain value: "
        f"{result!r}"
    )
    exc_obj = result.cdr.car
    etype = exc_obj.cdr.car
    emsg = exc_obj.cdr.cdr.car
    return etype, emsg


def _proc(name):
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
    assert binding is not False, f"{name!r} is not defined at toplevel"
    return scheme.binding_value(binding)


# ---------------------------------------------------------------------
# Arity mismatches reached through a JIT-compiled nested call
# ---------------------------------------------------------------------

def test_missing_argument_via_jit_nested_call_matches_direct_call():
    baseline = _eval("(define (add2b a b) (+ a b)) (add2b 1)")
    baseline_type, baseline_msg = _exception_parts(baseline)
    assert baseline_type == "RunTimeError"
    assert baseline_msg == "incorrect number of arguments in application"

    # Establish, with a *correctly*-arity'd nested call first, that add2 is
    # genuinely JIT-eligible -- proving this test isn't vacuously passing
    # because everything fell back to the trampoline before ever reaching
    # the JIT. The fix under test (_check_call_arity) intentionally raises
    # *before* jit_fn is ever invoked, so checking `_jit_lookup(add2) is
    # not None` after the mismatched call itself would no longer prove
    # anything -- it must be checked after a valid call instead.
    scheme.execute_string_rm(
        "(define (add2 a b) (+ a b)) (define (driver) (add2 1 2)) (driver)"
    )
    jit_fn = scheme._jit_lookup(_proc("add2"))
    assert jit_fn is not None, (
        "add2 never actually JIT-compiled -- this test needs the JIT path "
        "engaged to exercise the bug it's pinning down"
    )

    nested = scheme.execute_string_rm(
        "(define (driver) (add2 1)) (driver)"
    )
    etype, emsg = _exception_parts(nested)
    assert (etype, emsg) == (baseline_type, baseline_msg), (
        "calling add2 with too few arguments through a nested JIT'd call "
        f"produced {etype!r}/{emsg!r} instead of matching the direct-call "
        f"baseline {baseline_type!r}/{baseline_msg!r} -- a raw Python "
        "exception (naming _jit_fn/_j_b, implementation details) is "
        "leaking out instead of a proper Scheme RunTimeError"
    )


def test_excess_argument_via_jit_nested_call_matches_direct_call():
    baseline = _eval("(define (add2c a b) (+ a b)) (add2c 1 2 3)")
    baseline_type, baseline_msg = _exception_parts(baseline)
    assert baseline_type == "RunTimeError"
    assert baseline_msg == "incorrect number of arguments in application"

    scheme.execute_string_rm(
        "(define (add2x a b) (+ a b)) (define (driverx) (add2x 1 2)) (driverx)"
    )
    jit_fn = scheme._jit_lookup(_proc("add2x"))
    assert jit_fn is not None, "add2x never actually JIT-compiled"

    nested = scheme.execute_string_rm(
        "(define (driverx) (add2x 1 2 3)) (driverx)"
    )
    etype, emsg = _exception_parts(nested)
    assert (etype, emsg) == (baseline_type, baseline_msg), (
        "calling add2x with too many arguments through a nested JIT'd "
        f"call produced {etype!r}/{emsg!r} instead of matching the "
        f"direct-call baseline {baseline_type!r}/{baseline_msg!r}"
    )


# ---------------------------------------------------------------------
# Arity mismatches reached through Phase 2 with the JIT forced off
# (isolates _extend_direct's own arity handling from _JitCompiler's)
# ---------------------------------------------------------------------

def test_missing_argument_via_phase2_only_nested_call_matches_direct_call(monkeypatch):
    baseline = _eval("(define (add2d a b) (+ a b)) (add2d 1)")
    baseline_type, baseline_msg = _exception_parts(baseline)
    assert baseline_type == "RunTimeError"

    monkeypatch.setattr(scheme, "_jit_compile_proc", lambda proc: None)
    nested = scheme.execute_string_rm(
        "(define (add2e a b) (+ a b)) (define (drivere) (add2e 1)) (drivere)"
    )
    assert scheme._jit_lookup(_proc("add2e")) is None, (
        "sanity check: JIT should be disabled for this test"
    )
    etype, emsg = _exception_parts(nested)
    assert (etype, emsg) == (baseline_type, baseline_msg), (
        "calling add2e with too few arguments through a nested Phase-2 "
        f"(JIT disabled) call produced {etype!r}/{emsg!r} instead of "
        f"matching the direct-call baseline {baseline_type!r}/{baseline_msg!r}"
        " -- _extend_direct does not validate argument count either"
    )


def test_excess_argument_via_phase2_only_nested_call_matches_direct_call(monkeypatch):
    baseline = _eval("(define (add2f a b) (+ a b)) (add2f 1 2 3)")
    baseline_type, baseline_msg = _exception_parts(baseline)
    assert baseline_type == "RunTimeError"

    monkeypatch.setattr(scheme, "_jit_compile_proc", lambda proc: None)
    nested = scheme.execute_string_rm(
        "(define (add2g a b) (+ a b)) (define (driverg) (add2g 1 2 3)) (driverg)"
    )
    assert scheme._jit_lookup(_proc("add2g")) is None, (
        "sanity check: JIT should be disabled for this test"
    )
    etype, emsg = _exception_parts(nested)
    assert (etype, emsg) == (baseline_type, baseline_msg), (
        "calling add2g with too many arguments through a nested Phase-2 "
        f"(JIT disabled) call produced {etype!r}/{emsg!r} instead of "
        f"matching the direct-call baseline {baseline_type!r}/{baseline_msg!r}"
    )


# ---------------------------------------------------------------------
# Primitive type errors (car on a non-pair) reached through a nested call
# ---------------------------------------------------------------------

def test_car_on_non_pair_via_nested_call_matches_direct_call():
    baseline = _eval("(car (quote ()))")
    baseline_type, baseline_msg = _exception_parts(baseline)
    assert baseline_type == "RunTimeError"
    assert "car called on non-pair" in baseline_msg

    nested = scheme.execute_string_rm(
        "(define (bad x) (car x)) (define (driverh) (bad (quote ()))) (driverh)"
    )
    etype, emsg = _exception_parts(nested)
    assert (etype, emsg) == (baseline_type, baseline_msg), (
        "(car '()) reached through a nested Phase-2/JIT call produced "
        f"{etype!r}/{emsg!r} instead of matching the direct-call baseline "
        f"{baseline_type!r}/{baseline_msg!r} -- car is implemented as a "
        "raw `.car` attribute access in both _FAST_PRIM_SPECS and "
        "_JitCompiler._UNARY, with no Scheme-level type check"
    )
