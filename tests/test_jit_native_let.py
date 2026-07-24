"""
Pinned regression tests for the native `let-aexp` architecture (Fix B),
described in JIT-IIFE-GAP.md.

Fix A (see test_jit_iife_operator.py) taught the JIT's static safety walk
to see through a `let`'s IIFE desugaring, letting a function containing one
reach Phase 2/JIT. But it didn't touch codegen: the `let`'s operator was
still, structurally, a literal `lambda_aexp`, so `_JitCompiler` still built
a brand-new Scheme closure every time the `let` was evaluated -- and since
`_jit_cache` is keyed by object identity, a fresh closure every tail-loop
iteration meant a real `compile()`/`exec()` call on *every iteration*.
Measured directly: a `let` inside a hot self-recursive loop ran ~640x
slower than the equivalent `let`-free code, despite Fix A having "fixed"
its JIT eligibility.

Fix B eliminates the IIFE at the source: `let` (and everything built on
it -- `or`/`and`/`cond`/`case`/`let*`/named-`let`/`letrec`/internal-defines)
now parses directly to a native `let-aexp` AST node instead of desugaring
to a lambda application, understood natively by the classic interpreter,
Phase 2, and the JIT. The JIT compiles an escape-free `let` (nothing in its
body creates a closure that could capture and outlive its bindings) as
plain Python locals -- no closure, no frame, no runtime dispatch at all --
falling back to exactly the pre-Fix-B closure-construction path when a
closure genuinely does escape.

This file pins the JIT-specific correctness surface Fix B added: the fast
path's escape analysis, its "sticky real" inheritance through nested lets,
its interaction with self-recursive tail calls, and its interaction with
`(use-lexical-address #f)`.
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


def _tail_source_for(name):
    """Compile `name`'s toplevel proc via a fresh _JitCompiler and return
    the generated Python source lines for its tail body -- lets a test
    assert *how* something compiled (fast-path locals vs. a deferred
    closure), not just that it produced the right value."""
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
    proc = scheme.binding_value(binding)
    formals, bodies, cenv = proc[3], proc[2], proc[4]
    params = []
    cur = formals
    while isinstance(cur, scheme.cons):
        params.append(cur.car.name)
        cur = cur.cdr
    jc = scheme._JitCompiler(proc, params, cenv, {})
    body_list = []
    cur = bodies
    while isinstance(cur, scheme.cons):
        body_list.append(cur.car)
        cur = cur.cdr
    return "\n".join(jc.tail_stmts(body_list[-1], ""))


def test_mi_loop_fast_path_is_actually_used_not_just_correct():
    """Structural pin, not just a value check: mi-loop's `let` must compile
    to a plain Python local (`_jc_let_`), not a deferred `_jit_make_closure`/
    `_jit_call` pair -- silently regressing back to the deferred path would
    still be *correct* (every other test here would still pass) but would
    reintroduce the ~640x slowdown this fix exists to eliminate."""
    _eval(
        "(define max-iter 50)\n"
        "(define (mi-loop zx zy cx cy n)\n"
        "  (let ((esc (> (+ (* zx zx) (* zy zy)) 4.0)))\n"
        "    (if (>= n max-iter) n\n"
        "        (if esc n\n"
        "            (mi-loop (+ (- (* zx zx) (* zy zy)) cx) "
        "(+ (* 2.0 zx zy) cy) cx cy (+ n 1))))))"
    )
    src = _tail_source_for("mi-loop")
    assert "_jc_let_0" in src, f"expected a fast-path local, got:\n{src}"
    assert "_jit_call" not in src and "_jit_make_closure" not in src, (
        f"mi-loop's let fell back to the deferred/closure path:\n{src}"
    )
    assert "continue" in src, "self-recursive tail call must still become `continue`"


def test_let_in_non_tail_expression_position():
    """`(+ (let ...) ...)` -- today (pre-Fix-B) already compiled via the
    generic IIFE path; the fast path's walrus-tuple expression form must
    keep this working, not regress it to _TrampolineFallback. Non-tail
    self-recursion (the fib shape, see test_jit_self_recursion.py) is what
    actually triggers a JIT-compile attempt here -- a plain top-level call
    with no recursion never does (apply_proc's own entry into Phase 2 runs
    _eval_sequence_direct directly; only a *nested* call from within
    already-executing Phase 2 code triggers _jit_compile_proc)."""
    _eval(
        "(define (f-expr n)\n"
        "  (if (<= n 0) 0\n"
        "      (+ (let ((x (* n n))) (* x x)) (f-expr (- n 1)))))"
    )
    assert _eval("(f-expr 3)") == 98
    _, jit_fn = _jit_lookup_for("f-expr")
    assert jit_fn is not None


def test_nested_let_three_levels_fast_path():
    _eval(
        "(define (nested n)\n"
        "  (let ((a (- n 0)))\n"
        "    (let ((b (- a 0)))\n"
        "      (let ((done (<= b 0)))\n"
        "        (if done b (+ 1 (nested (- b 1))))))))"
    )
    assert _eval("(nested 10)") == 10
    _, jit_fn = _jit_lookup_for("nested")
    assert jit_fn is not None


def test_let_bound_lambda_called_internally_never_escapes():
    """The escape scan only looks at a let's *body*, not its binding
    values -- so `(let ((f (lambda ...))) (f x))` still takes the fast
    path (f's own value is built via the ordinary _lambda mechanism, and
    the call site routes through _jit_call because _app now recognizes a
    let-scoped operator reference as 'can't prove it's a plain Python
    callable at compile time', exactly like an ordinary parameter)."""
    _eval(
        "(define (call-through-let n)\n"
        "  (let ((f (lambda (x) (* x x))))\n"
        "    (if (<= n 0) (f n) (call-through-let (- n 1)))))"
    )
    assert _eval("(call-through-let 5)") == 0
    src = _tail_source_for("call-through-let")
    assert "_jc_let_0" in src and "_jit_call(_jc_let_0" in src, (
        f"expected fast-path local holding a closure, called via "
        f"_jit_call:\n{src}"
    )


def test_escaping_closure_still_correct_via_deferred_path():
    """A `let` that legitimately builds and returns a closure capturing
    its own bound variable must stay 100% correct via the deferred/real
    path -- the highest-stakes correctness case for the escape scan."""
    _eval(
        "(define (make-adder-via-let k)\n"
        "  (let ((kk (* k 2)))\n"
        "    (lambda (x) (+ x kk))))"
    )
    assert _eval("((make-adder-via-let 5) 10)") == 20


def test_nested_escape_sticky_real_inheritance():
    """An inner let that's escape-free *in isolation* still defers to the
    real/closure path when an *outer* let's variable is captured by a
    lambda nested inside it -- the escape scan doesn't stop at let
    boundaries, so the outer let's scan already sees the inner lambda and
    marks the whole nest 'real', not just the inner let."""
    _eval(
        "(define (nested-escape n)\n"
        "  (let ((base (* n 10)))\n"
        "    (let ((offset (+ n 1)))\n"
        "      (lambda () (+ base offset)))))"
    )
    assert _eval("((nested-escape 3))") == 34


def test_let_rebinding_primitives_still_shadows_correctly():
    """(let ((- +)(+ -)) (+ 1 2)) rebinds - to the outer +, and + to the
    outer - -- so the body's (+ 1 2) actually calls the outer `-`. Pins
    that the fast path's synthetic-name scoping doesn't accidentally let
    _app's primitive-inlining (_is_unshadowed_primitive) see through the
    shadowing and wrongly inline `+`/`-` as raw operators."""
    assert _eval("(let ((- +)(+ -)) (+ 1 2))") == -1


def test_or_and_cond_still_reach_fast_path():
    _eval("(define (f-or n) (if (or (< n 0) (= n 0)) 0 (+ 1 (f-or (- n 1)))))")
    assert _eval("(f-or 5)") == 5
    _, jit_fn = _jit_lookup_for("f-or")
    assert jit_fn is not None
    src = _tail_source_for("f-or")
    assert "_jc_let_0" in src


def test_use_lexical_address_false_still_correct():
    """(use-lexical-address #f) makes aparse emit name-based var-aexp
    nodes everywhere instead of lexical addresses -- the fast path's
    name-keyed scope lookup (_scope_lookup_by_name) must still resolve
    correctly, not just silently fall back to the slow path or, worse,
    resolve to the wrong (shadowed) outer binding."""
    _eval("(use-lexical-address #f)")
    try:
        _eval(
            "(define (g-noaddr n) (let ((d (<= n 0))) (if d 0 (g-noaddr (- n 1)))))"
        )
        assert _eval("(g-noaddr 100)") == 0
    finally:
        _eval("(use-lexical-address #t)")


def test_backtracking_through_native_let():
    """A let whose binding value is a (choose ...) point, with a (require
    ...) that fails and backtracks into another choice -- the same idiom
    as test_all.ss's floors2 test (this dialect has no `amb`; `require`
    calls the fail continuation directly on a false test). Confirms the
    native let-aexp's plain frame extension (no closure/apply layer)
    still composes correctly with fail-continuation backtracking."""
    _eval(
        "(define (a-member lst) (if (null? lst) (choose) (choose (car lst) (a-member (cdr lst)))))\n"
        "(define (test-let-choose)\n"
        "  (let ((x (a-member (list 1 3 5 4 7))))\n"
        "    (require (even? x))\n"
        "    x))"
    )
    assert _eval("(test-let-choose)") == 4
