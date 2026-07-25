"""
Pinned regression tests for the environment lexical-address optimization:
the persistent-vector `_lexaddr_vec`/`_depth` machinery in Scheme.py, and
environments-cps.ss's define-native conversions for `extend`,
`make-empty-environment`, `make-initial-environment`,
`lookup-value-by-lexical-address`, `lookup-binding-by-lexical-address`,
and `set-first-frame!`.

This replaced list_ref(frames(env), depth)'s O(depth) cons-chain walk
with O(1) indexed access into a persistent, structurally-shared vector,
for any environment nested deeper than _ENV_VEC_THRESHOLD (8) -- below
that, environments are untouched (plain cons chain, exactly as before),
to avoid paying construction cost where it can't pay for itself
(confirmed measured: building the vector unconditionally cost naive
recursion like `fib` ~13% for no benefit, since it never nests anywhere
near that deep; gating it behind a threshold got that down to ~5-10%).

The tests here specifically target the ways a naive implementation of
this could go wrong: `_lexaddr_vec`/`_depth` staying in sync across
`extend()` *and* Phase 2's separate `_extend_direct()`, across the global
frame being replaced in place (not via extend) by a fresh top-level
`define`, across the threshold boundary itself, and across call/cc and
amb/choose's own control flow -- neither creates a new frame the "normal"
way, but both can re-enter or hold onto environments this machinery is
responsible for.
"""
import calysto_scheme.scheme as scheme


def _eval(src):
    result = scheme.execute_string_rm(src)
    assert not scheme.exception_q(result), scheme.get_traceback_string(result)
    return result


def test_repeated_top_level_redefinition():
    """set-first-frame! replaces the global frame in place (add-binding
    builds a new frame object; set-first-frame! swaps it in via
    set-car!) -- must keep updating _lexaddr_vec's innermost entry every
    time this happens, not just the first."""
    _eval("(define rtlr-x 1)")
    _eval("(define rtlr-x 2)")
    assert _eval("(define rtlr-x 3) rtlr-x") == 3


def test_many_sequential_new_top_level_defines():
    """Each of these is a fresh, not-yet-bound name -- a fresh
    add-binding/set-first-frame! call each time, not just a rebind."""
    src = "\n".join("(define mstd-v%d %d)" % (i, i) for i in range(10))
    src += "\n(+ %s)" % " ".join("mstd-v%d" % i for i in range(10))
    assert _eval(src) == sum(range(10))


def test_deep_let_crosses_trie_and_threshold_boundaries():
    """40 levels of nesting: crosses both _ENV_VEC_THRESHOLD (8) and the
    persistent vector's own 32-way branching boundary, so this exercises
    tail-buffer -> trie folding and at least one internal trie-height
    growth, not just the tail buffer alone. Checks a near (depth 0), a
    mid (depth 17), and a far (innermost) reference all resolve
    correctly in the same environment."""
    n = 40
    lines = ["(define (dltb-test)"]
    for i in range(n):
        lines.append("(let ((dltb-f%d %d))" % (i, i))
    lines.append("(+ dltb-f0 dltb-f%d dltb-f17)" % (n - 1))
    lines.append(")" * n)
    lines.append(")")
    lines.append("(dltb-test)")
    assert _eval("\n".join(lines)) == 0 + (n - 1) + 17


def test_threshold_boundary_exact():
    """Depths 7, 8, 9, 10 straddle _ENV_VEC_THRESHOLD (8) exactly --
    pins the crossover isn't off by one in either direction."""
    for depth in (7, 8, 9, 10):
        lines = ["(define (tbe-test-%d)" % depth]
        for i in range(depth):
            lines.append("(let ((tbe-f%d %d))" % (i, i))
        lines.append("(+ tbe-f0 tbe-f%d)" % (depth - 1))
        lines.append(")" * depth)
        lines.append(")")
        lines.append("(tbe-test-%d)" % depth)
        assert _eval("\n".join(lines)) == depth - 1


def test_closure_environment_survives_later_unrelated_redefinition():
    """A closure's captured environment (and its _lexaddr_vec/_depth)
    must be independent of later, unrelated top-level defines mutating
    the global frame via set-first-frame! -- confirms per-environment
    tracking, not something accidentally shared or invalidated."""
    _eval("(define (cesl-adder n) (lambda (x) (+ x n)))")
    _eval("(define cesl-add5 (cesl-adder 5))")
    _eval("(define cesl-unrelated 999)")
    assert _eval("(cesl-add5 10)") == 15


def test_amb_choose_backtracking_with_deep_lexical_nesting():
    """choose/require backtracking (its own fail-continuation-based
    control flow, independent of call/cc) combined with lexically
    addressed variables nested past _ENV_VEC_THRESHOLD -- both the
    backtracking search itself and a deep reference (acbd-f9) must
    resolve correctly together."""
    lines = ["(define (acbd-test)"]
    for i in range(10):
        lines.append("(let ((acbd-f%d %d))" % (i, i))
    lines.append("(let ((a (choose 1 2 3)))")
    lines.append("(let ((b (choose 1 2 3)))")
    lines.append("(require (= (+ a b acbd-f0) 5))")
    lines.append("(+ (* 100 a) (* 10 b) acbd-f9))))")
    lines.append(")" * 10)
    lines.append("(acbd-test)")
    result = _eval("\n".join(lines))
    a, b = (result // 100) % 10, (result // 10) % 10
    assert a in (1, 2, 3) and b in (1, 2, 3)
    assert a + b == 5  # acbd-f0 is 0
    assert result % 10 == 9  # acbd-f9


def test_callcc_through_deep_lexical_nesting():
    """A lexically addressed reference used both before and after
    invoking a call/cc-captured continuation, nested well past
    _ENV_VEC_THRESHOLD -- confirms the persistent-vector-backed
    environment resolves correctly across a continuation invocation, not
    just ordinary sequential evaluation."""
    lines = ["(define (ccdn-test)"]
    for i in range(10):
        lines.append("(let ((ccdn-f%d %d))" % (i, i))
    lines.append("(+ ccdn-f0 ccdn-f9 (call/cc (lambda (k) (k 100) 999))))")
    lines.append(")" * 10)
    lines.append("(ccdn-test)")
    assert _eval("\n".join(lines)) == 0 + 9 + 100
