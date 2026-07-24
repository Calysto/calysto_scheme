"""
Pinned regression test for the `let`/`or`/`and`/`cond`/named-`let`
IIFE-operator fix described in JIT-IIFE-GAP.md ("Fix A").

Before this fix, `_phase2_safe_walk_call` gave up unconditionally
(`'unresolved'`) on any application whose operator was a literal
`(lambda ...)` -- exactly the shape `let`/`or`/`and`/`cond`/`case`/
named-`let` all desugar to. Per `_is_phase2_safe`'s transitivity, that
poisoned not just the IIFE call itself but *every* function whose body
contained one anywhere, permanently excluding it from Phase 2/JIT --
confirmed directly in JIT-IIFE-GAP.md against the `mi-loop` case below,
which never even got a JIT-compile attempt despite containing no `set!`
at all.

The fix recurses into a literal-lambda operator's own body instead of
bailing out, reusing the same `_phase2_safe_walk_seq` walk an ordinary
resolved closure already gets. Doing that correctly requires pushing a
*real* placeholder frame for the lambda's formals before recursing --
not reusing the enclosing `env` unmodified -- because lexical-address
depths inside the body are counted relative to the actual runtime frame
stack, which includes one new frame per level of IIFE nesting. Skipping
that (an earlier, incorrect version of this fix) silently shifted every
deeper reference (e.g. a primitive like `>=` called from inside the
`let`) off by one frame per nesting level, making `_resolve_lexical_address`
resolve the wrong binding and, in this case, conservatively (but
wrongly, for the wrong reason) still report `False`.

See also: the `let_wrapped_*`/`nested_let`/`or_and`/`cond`/`named_let`
cases in _scheme_fuzz_gen.py, which fold this shape into the general
differential fuzzer (test_jit_fuzz.py) for broader coverage.
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
    return proc, scheme._is_phase2_safe(proc), scheme._jit_lookup(proc)


def test_mi_loop_plain_let_reaches_phase2_and_jit_and_is_correct():
    """The exact case from JIT-IIFE-GAP.md's "Verified, directly" section:
    a set!-free `let` inside a self-recursive loop. Must now be
    phase2-safe and actually JIT-compile, not merely produce the right
    answer via the slow trampoline."""
    _eval(
        "(define max-iter 50)\n"
        "(define (mi-loop zx zy cx cy n)\n"
        "  (let ((esc (> (+ (* zx zx) (* zy zy)) 4.0)))\n"
        "    (if (>= n max-iter) n\n"
        "        (if esc n\n"
        "            (mi-loop (+ (- (* zx zx) (* zy zy)) cx) "
        "(+ (* 2.0 zx zy) cy) cx cy (+ n 1))))))\n"
        "(define (mandelbrot-iterations cx cy) (mi-loop 0.0 0.0 cx cy 0))"
    )
    assert _eval("(mandelbrot-iterations -0.5 0.5)") == 50

    proc, safe, jit_fn = _jit_lookup_for("mi-loop")
    assert safe is True, "mi-loop should now be certified phase2-safe"
    assert jit_fn is not None, (
        "mi-loop did not actually JIT-compile -- the IIFE-operator fix "
        "should let it reach Phase 2/JIT, not just stay correct on the "
        "slow trampoline"
    )

    _, mand_safe, _ = _jit_lookup_for("mandelbrot-iterations")
    assert mand_safe is True, (
        "mi-loop's own IIFE should no longer poison its caller "
        "(transitivity of _is_phase2_safe)"
    )


def test_nested_let_multi_frame_depth_resolves_correctly():
    """Two levels of nested `let` inside a self-recursive function --
    catches the exact bug found while implementing this fix: reusing the
    enclosing env unmodified (instead of pushing a real placeholder frame
    per nesting level) silently miscounted lexical-address depth for any
    reference more than one frame beyond the innermost let, wrongly
    treating a perfectly safe body as unresolvable."""
    _eval(
        "(define (nested-let-loop n)\n"
        "  (let ((a (- n 0)))\n"
        "    (let ((done (<= a 0)))\n"
        "      (if done a\n"
        "          (+ (nested-let-loop (- a 1)) 1)))))"
    )
    assert _eval("(nested-let-loop 10)") == 10

    _, safe, jit_fn = _jit_lookup_for("nested-let-loop")
    assert safe is True
    assert jit_fn is not None


def test_or_and_cond_wrapped_recursion_reaches_phase2():
    """`or`/`and`/`cond` all bottom out in a `let` in their own expansion
    (JIT-IIFE-GAP.md), so this shape must benefit from the same fix."""
    _eval(
        "(define (f-or n) (if (or (< n 0) (= n 0)) 0 (+ 1 (f-or (- n 1)))))"
    )
    assert _eval("(f-or 5)") == 5
    _, safe, jit_fn = _jit_lookup_for("f-or")
    assert safe is True and jit_fn is not None

    _eval(
        "(define (f-cond n)\n"
        "  (cond ((<= n 0) 0)\n"
        "        (else (+ 1 (f-cond (- n 1))))))"
    )
    assert _eval("(f-cond 5)") == 5
    _, safe, jit_fn = _jit_lookup_for("f-cond")
    assert safe is True and jit_fn is not None


def test_named_let_outer_wrapper_still_excluded_but_correct():
    """Per JIT-IIFE-GAP.md's Fix C discussion: this fix does NOT unlock
    named-let's own outer `(lambda (loop) (set! loop ...) (loop ...))`
    wrapper -- its letrec desugaring contains a genuine, literal `set!`,
    which _phase2_safe_walk still has no case for (falls through to its
    `else: return False`) regardless of the new IIFE-operator branch.
    This pins that the fix doesn't *wrongly* certify a body containing a
    real set! as safe just because it's reached through the new
    recursive walk, while confirming the result is still correct via
    whichever path actually executes it."""
    _eval(
        "(define (outer-named-let n)\n"
        "  (let loop ((i n) (acc 0))\n"
        "    (if (<= i 0) acc\n"
        "        (loop (- i 1) (+ acc i)))))"
    )
    assert _eval("(outer-named-let 10)") == 55

    proc, safe, _ = _jit_lookup_for("outer-named-let")
    assert safe is False, (
        "a function whose body is a named-let should still be phase2-"
        "unsafe (the letrec set!), not wrongly certified safe by the "
        "new IIFE-operator recursion"
    )


def test_unsafe_iife_body_is_still_rejected():
    """A `let` whose body calls something unresolvable (here, its own
    formal used in operator position -- the apply-twice shape) must
    still be conservatively rejected, not wrongly certified safe just
    because the recursion into the IIFE body itself succeeds."""
    _eval(
        "(define (call-through-let f x) (let ((y (f x))) (f y)))"
    )
    _eval("(define (inc x) (+ x 1))")
    assert _eval("(call-through-let inc 5)") == 7

    proc, safe, _ = _jit_lookup_for("call-through-let")
    assert safe is False, (
        "call-through-let calls its own parameter as an operator inside "
        "the let body -- must still be rejected as unresolvable"
    )
