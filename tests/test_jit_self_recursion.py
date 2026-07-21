"""
Pinned regression test for self-recursive JIT compilation.

_JitCompiler resolves a self-recursive reference (a call to the very
closure currently being compiled) by *identity* at compile time, binding
it to the literal generated Python function name (`_jit_fn`) rather than
re-looking it up dynamically -- see README-PERFORMANCE.md's Phase 7. This
is the single most safety-critical piece of logic in the JIT: get it
wrong and a closure could observe a *later* redefinition of its own name
through what should be a fixed, already-compiled recursive call.

Added alongside the Scheme.py refactor that extracted shared
_resolve_operator/_resolve_lexical_address/_resolve_var helpers, now used
by _is_self_ref, _JitCompiler.expr()/_var(), and _phase2_safe_walk_call --
this pins down the one behavior all of those call sites must keep
agreeing on, independent of the broader test_all.ss-based differential
tests in test_phase2_safety.py.
"""
import calysto_scheme.scheme as scheme


def _eval(src):
    result = scheme.execute_string_rm(src)
    assert not scheme.exception_q(result), scheme.get_traceback_string(result)
    return result


def _jit_lookup_for(name):
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
    assert binding is not False, f"{name!r} is not defined at toplevel"
    proc = scheme.binding_value(binding)
    return proc, scheme._jit_lookup(proc)


def test_self_recursive_non_tail_call_jits_and_is_correct():
    """Non-tail self-recursion (fib-shaped, Phase 7's motivating case) must
    actually JIT-compile -- not silently stay on the slow path -- and
    compute the right answer."""
    _eval("(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
    assert _eval("(fib 15)") == 610

    _, jit_fn = _jit_lookup_for("fib")
    assert jit_fn is not None, (
        "fib did not actually JIT-compile -- this test would otherwise "
        "pass vacuously via the slow path"
    )


def test_self_recursive_call_binds_to_compiled_instance_not_name():
    """Redefining a function's name after it's JIT-compiled must not be
    observed by its own already-compiled recursive calls."""
    _eval("(define (depth n) (if (= n 0) 0 (+ 1 (depth (- n 1)))))")
    assert _eval("(depth 5)") == 5

    proc, jit_fn = _jit_lookup_for("depth")
    assert jit_fn is not None, "depth did not actually JIT-compile"

    # Redefine the name to something with unrelated behavior.
    _eval("(define (depth n) 999)")

    # Calling the ORIGINAL compiled function directly: if its internal
    # self-recursive calls were (incorrectly) re-resolving the name
    # "depth" dynamically, they'd invoke the *new* depth (which ignores
    # its argument and returns 999 without recursing further), giving
    # 1 + 999 = 1000 instead of the correct 5.
    assert jit_fn(5) == 5, (
        "the original compiled closure's self-recursive calls observed "
        "the later redefinition -- the identity-binding invariant "
        "(README-PERFORMANCE.md Phase 7) is broken"
    )
    # And the freshly redefined name behaves as redefined, confirming the
    # redefinition itself took effect (the assertion above isn't passing
    # because the redefinition silently failed).
    assert _eval("(depth 5)") == 999
