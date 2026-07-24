"""
Random small-Scheme-program generator for the differential fuzz test
(test_jit_fuzz.py). Pure Python, no dependency on calysto_scheme -- the
same seed always produces the same list of case sources, in this process
or a fresh subprocess, so results can be regenerated for reporting without
having to round-trip them through the interpreter.

Deliberately narrow in scope: every construct used here is one
_eval_direct / _JitCompiler / _phase2_safe_walk explicitly claims to
handle (see test_jit_tag_parity.py) -- arithmetic, comparisons, `if`,
self/mutual recursion, closures returned from closures, a parameter
called as a function, list ops, `begin` (deliberately, since `begin`
is the one documented, intentional gap between _eval_direct and
_JitCompiler -- see this file's `begin_body` case and
test_jit_tag_parity.py's module docstring), and `let`/`or`/`and`/`cond`/
named-`let` (which all desugar to a literal-lambda IIFE operator --
`((lambda (v ...) body) e ...)` -- per JIT-IIFE-GAP.md; see this file's
`let_wrapped_*`/`nested_let`/`or_and`/`cond`/`named_let` cases). The goal
isn't broad language coverage, it's many random *shapes* of the specific
patterns the three walkers' bug history (README-PERFORMANCE.md's Phases
6-9) shows they can silently disagree about.

Every generated case is self-contained (its own uniquely-numbered
function name(s), no shared mutable state) and constructed to always
terminate quickly: recursive cases strictly decrease a bounded counter
toward a base case, and argument magnitudes are kept small.
"""
import random

_NARY_OPS = ["+", "-", "*"]
_CMP_OPS = ["<", ">", "<=", ">=", "="]
_NUM_UNARY_OPS = ["not", "zero?", "even?", "odd?", "abs"]
_NUM_LITERALS = [0, 1, -1, 2, -2, 3, 5, -5, 10, -10, 100,
                 0.0, -0.0, 1.5, -1.5, 2.5, -2.5]


def _lit_src(v):
    if v is True:
        return "#t"
    if v is False:
        return "#f"
    return repr(v)


def _gen_num_expr(rng, params, depth):
    """A pure numeric-or-boolean expression, using only params/literals
    and the ~9 primitives _JitCompiler inlines as raw Python
    operators/templates (_NARY/_CMP/_UNARY) -- the exact surface most
    likely to silently diverge between the inlined-operator JIT path and
    the general dispatch _eval_direct/the trampoline use instead."""
    if depth <= 0 or rng.random() < 0.35:
        if params and rng.random() < 0.6:
            return rng.choice(params)
        return _lit_src(rng.choice(_NUM_LITERALS))
    choice = rng.random()
    if choice < 0.10:
        # (- ) is a genuine arity error (see _JitCompiler._app's comment
        # on why `-` has no zero-arg identity element); occasionally
        # probe it directly rather than only ever generating well-formed
        # calls.
        return "(-)"
    if choice < 0.45:
        op = rng.choice(_NARY_OPS)
        n = rng.choice([1, 2, 2, 3])
        args = [_gen_num_expr(rng, params, depth - 1) for _ in range(n)]
        return f"({op} {' '.join(args)})"
    if choice < 0.65:
        op = rng.choice(_NUM_UNARY_OPS)
        return f"({op} {_gen_num_expr(rng, params, depth - 1)})"
    if choice < 0.85:
        test = _gen_bool_expr(rng, params, depth - 1)
        then_ = _gen_num_expr(rng, params, depth - 1)
        else_ = _gen_num_expr(rng, params, depth - 1)
        return f"(if {test} {then_} {else_})"
    # `begin` wrapping a discarded pure expression then a value -- probes
    # the documented begin_aexp gap: _JitCompiler must decline to compile
    # any function whose body contains one of these, anywhere, and fall
    # back to Phase 2/the trampoline instead, silently and correctly.
    junk = _gen_num_expr(rng, params, depth - 1)
    val = _gen_num_expr(rng, params, depth - 1)
    return f"(begin {junk} {val})"


def _gen_bool_expr(rng, params, depth):
    if depth <= 0 or rng.random() < 0.3:
        return rng.choice(["#t", "#f"])
    a = _gen_num_expr(rng, params, depth - 1)
    b = _gen_num_expr(rng, params, depth - 1)
    op = rng.choice(_CMP_OPS)
    return f"({op} {a} {b})"


def _rand_arg(rng, lo=0, hi=12):
    return rng.randint(lo, hi)


def _case_simple_rec(rng, idx):
    """Non-tail self-recursion (the `fib` shape) -- the most common
    recursive pattern, and the one Phase 7's self-call identity binding
    specifically optimizes."""
    fn = f"f_{idx}"
    base = rng.choice([0, 1])
    combine = rng.choice(_NARY_OPS)
    body_extra = _gen_num_expr(rng, [], 1)
    src = (
        f"(define ({fn} n)\n"
        f"  (if (<= n {base}) {body_extra}\n"
        f"      ({combine} ({fn} (- n 1)) ({fn} (- n 2)))))\n"
        f"({fn} {_rand_arg(rng, 0, 12)})"
    )
    return src


def _case_tail_rec(rng, idx):
    """Accumulator-style tail loop -- exercises Phase 4's tail-loop
    flattening (self-recursive tail call -> parameter reassignment +
    Python `continue`, both in _eval_direct and _JitCompiler.tail_stmts).

    The accumulator update is deliberately restricted to a linear
    combination of `acc` and `n` (never `acc` multiplied against itself
    or another acc-derived expression) -- with up to 200 iterations,
    letting `_gen_num_expr`'s general `*` case multiply the carried `acc`
    by itself compounds into doubly-exponential bignum growth (repeated
    squaring), which stalled a real fuzz run for minutes on one single
    generated case before this was found and fixed. `n` itself is safe
    to multiply freely since it strictly decreases every iteration and
    is never fed back into itself."""
    fn = f"f_{idx}"
    op = rng.choice(["+", "-"])
    other = rng.choice(["n", _gen_num_expr(rng, ["n"], 1)])
    step = f"({op} acc {other})"
    src = (
        f"(define ({fn} n acc)\n"
        f"  (if (<= n 0) acc\n"
        f"      ({fn} (- n 1) {step})))\n"
        f"({fn} {_rand_arg(rng, 0, 200)} {_lit_src(rng.choice(_NUM_LITERALS))})"
    )
    return src


def _case_mutual_rec(rng, idx):
    """Mutual recursion -- per README-PERFORMANCE.md, this shape never
    successfully JIT-compiles (each function's compile attempt needs the
    other already resolved), so it's always retried through _eval_direct
    every call. Good coverage for the "never amortized, always hot"
    _eval_direct path specifically."""
    fe, fo = f"feven_{idx}", f"fodd_{idx}"
    src = (
        f"(define ({fe} n) (if (<= n 0) #t ({fo} (- n 1))))\n"
        f"(define ({fo} n) (if (<= n 0) #f ({fe} (- n 1))))\n"
        f"({fe} {_rand_arg(rng, 0, 30)})"
    )
    return src


def _case_closure_factory(rng, idx):
    """A function returning a freshly-built closure over one of its own
    parameters (the `make-adder` shape) -- Phase 5/_JitCompiler._lambda's
    reconstructed-frame machinery."""
    mk = f"make_{idx}"
    op = rng.choice(_NARY_OPS)
    k = _rand_arg(rng, -10, 10)
    x = _rand_arg(rng, -10, 10)
    src = (
        f"(define ({mk} k) (lambda (x) ({op} x k)))\n"
        f"(({mk} {k}) {x})"
    )
    return src


def _case_param_as_op(rng, idx):
    """A parameter called as a function (the `apply-twice` shape) --
    unreachable by the JIT/Phase 2 fast paths since Phase 8 (see
    JIT-OVERVIEW.md's Phase 6/8 discussion), but must still be *correct*,
    always falling all the way back."""
    ap, inc = f"apply2_{idx}", f"inc_{idx}"
    op = rng.choice(_NARY_OPS)
    k = _rand_arg(rng, -5, 5)
    x = _rand_arg(rng, -20, 20)
    src = (
        f"(define ({ap} f x) (f (f x)))\n"
        f"(define ({inc} x) ({op} x {k}))\n"
        f"({ap} {inc} {x})"
    )
    return src


def _case_list_ops(rng, idx):
    """Self-recursive list traversal (car/cdr/null?/pair? -- the
    _UNARY-inlined list predicates/accessors) over a freshly-built,
    guaranteed-proper list literal, so car/cdr never hit an empty list."""
    fn = f"sum_{idx}"
    n = rng.randint(0, 6)
    items = [_lit_src(rng.choice(_NUM_LITERALS)) for _ in range(n)]
    src = (
        f"(define ({fn} lst)\n"
        f"  (if (null? lst) 0\n"
        f"      (+ (car lst) ({fn} (cdr lst)))))\n"
        f"({fn} (list {' '.join(items)}))"
    )
    return src


def _case_begin_body(rng, idx):
    """A self-recursive function whose body is a top-level `begin` of
    several pure (discarded) sub-expressions before the tail `if` --
    structurally the exact shape _JitCompiler.expr has no case for at
    all, so every case of this shape must always, silently fall back to
    Phase 2/the trampoline. See test_jit_tag_parity.py."""
    fn = f"f_{idx}"
    junk1 = _gen_num_expr(rng, ["n"], 2)
    junk2 = _gen_num_expr(rng, ["n"], 2)
    base = rng.choice([0, 1])
    src = (
        f"(define ({fn} n)\n"
        f"  (begin\n"
        f"    {junk1}\n"
        f"    {junk2}\n"
        f"    (if (<= n {base}) n\n"
        f"        (+ 1 ({fn} (- n 1))))))\n"
        f"({fn} {_rand_arg(rng, 0, 20)})"
    )
    return src


def _case_arith_leaf(rng, idx):
    """No function definitions at all -- just a single raw top-level
    expression, for broad structural coverage of inlined-operator fold
    order/negative-zero/arity-edge behavior without any recursion at
    all."""
    return _gen_num_expr(rng, [], 4)


def _case_let_wrapped_rec(rng, idx):
    """Non-tail self-recursion whose base-case test is wrapped in a
    `let` -- i.e. the literal-lambda IIFE-operator shape any `let`
    desugars to, `((lambda (v) body) e)` (JIT-IIFE-GAP.md). Stresses
    _phase2_safe_walk_call's IIFE-operator branch: `<=`/the combining op,
    referenced from inside the let's body, are lexical-address operators
    at depth > 0 relative to the let's own synthetic frame -- exactly the
    shape that silently resolved to the wrong enclosing frame before that
    branch pushed a real placeholder frame onto env (confirmed directly
    against JIT-IIFE-GAP.md's mi-loop case before this fix)."""
    fn = f"g_{idx}"
    base = rng.choice([0, 1])
    combine = rng.choice(_NARY_OPS)
    src = (
        f"(define ({fn} n)\n"
        f"  (let ((done (<= n {base})))\n"
        f"    (if done n\n"
        f"        ({combine} ({fn} (- n 1)) ({fn} (- n 2))))))\n"
        f"({fn} {_rand_arg(rng, 0, 12)})"
    )
    return src


def _case_let_wrapped_tail_rec(rng, idx):
    """Tail-recursive accumulator loop whose per-iteration step is
    wrapped in a `let` -- the JIT-IIFE-GAP.md mi-loop shape directly: a
    plain, set!-free `let` inside a hot self-recursive loop, previously
    permanently excluded from Phase 2/JIT (not just slowed down) because
    _phase2_safe_walk_call could not resolve a literal-lambda operator at
    all."""
    fn = f"g_{idx}"
    op = rng.choice(["+", "-"])
    other = rng.choice(["n", _gen_num_expr(rng, ["n"], 1)])
    step = f"({op} acc {other})"
    src = (
        f"(define ({fn} n acc)\n"
        f"  (let ((done (<= n 0)))\n"
        f"    (if done acc\n"
        f"        ({fn} (- n 1) {step}))))\n"
        f"({fn} {_rand_arg(rng, 0, 200)} {_lit_src(rng.choice(_NUM_LITERALS))})"
    )
    return src


def _case_nested_let_rec(rng, idx):
    """Self-recursion wrapped in two levels of nested `let` -- stresses
    the IIFE-operator branch across more than one synthetic frame: a
    reference from the innermost let's body to a primitive is at lexical
    depth >= 2 relative to its own frame, which only resolves correctly
    if *every* enclosing synthetic frame was pushed with the right
    shape, not just the innermost one."""
    fn = f"g_{idx}"
    base = rng.choice([0, 1])
    combine = rng.choice(_NARY_OPS)
    src = (
        f"(define ({fn} n)\n"
        f"  (let ((a (- n 0)))\n"
        f"    (let ((done (<= a {base})))\n"
        f"      (if done a\n"
        f"          ({combine} ({fn} (- a 1)) ({fn} (- a 2)))))))\n"
        f"({fn} {_rand_arg(rng, 0, 12)})"
    )
    return src


def _case_or_and_rec(rng, idx):
    """Self-recursion whose base-case test goes through `or`/`and`,
    which also desugar through a `let` IIFE (JIT-IIFE-GAP.md) -- same
    static-safety gap as a plain `let`, reached through a different
    macro.

    Both test variants must still reduce to a monotonic `n <= base`
    threshold -- true for every n from some point downward, never true
    then false again as n keeps decreasing. An earlier version used
    `(and (<= n base) (>= n 0))`, a finite *window* of base cases
    (single-point when base=0) that the `(- n 2)` recursive step can
    jump clean over on an unlucky branch, landing just past it on the
    negative side where the window is false again -- and since every
    branch of this non-memoized double recursion is explored, one such
    branch is enough to recurse forever. This is invisible under
    fast/phase2-only timing (RecursionError caps it there), but the
    classic trampoline this case exists to exercise under "slow" mode
    doesn't grow the Python call stack for Scheme recursion at all, so
    it just hung -- caught by test_jit_fuzz.py's slow-mode subprocess
    stalling well past its timeout instead of a clean failure."""
    fn = f"g_{idx}"
    base = rng.choice([0, 1])
    combine = rng.choice(_NARY_OPS)
    test = rng.choice([
        f"(or (< n {base}) (= n {base}))",
        f"(and (<= n {base}) #t)",
    ])
    src = (
        f"(define ({fn} n)\n"
        f"  (if {test} n\n"
        f"      ({combine} ({fn} (- n 1)) ({fn} (- n 2)))))\n"
        f"({fn} {_rand_arg(rng, 0, 12)})"
    )
    return src


def _case_cond_rec(rng, idx):
    """Self-recursion whose base case is a `cond`, which also bottoms
    out in a `let` internally (JIT-IIFE-GAP.md)."""
    fn = f"g_{idx}"
    base = rng.choice([0, 1])
    combine = rng.choice(_NARY_OPS)
    src = (
        f"(define ({fn} n)\n"
        f"  (cond ((<= n {base}) n)\n"
        f"        (else ({combine} ({fn} (- n 1)) ({fn} (- n 2))))))\n"
        f"({fn} {_rand_arg(rng, 0, 12)})"
    )
    return src


def _case_named_let(rng, idx):
    """A named-let accumulator loop -- desugars through letrec/set!
    (JIT-IIFE-GAP.md), so its own outer wrapper stays excluded from
    Phase 2/JIT even after the literal-lambda-operator fix (a real set!
    still bars _is_direct_eval_safe directly). Included to confirm the
    fix doesn't change this shape's *result* -- only which of the three
    execution paths individual pieces of it end up using. `loop` is
    local to the let, so no top-level-name collision risk across cases
    despite not being uniquely numbered."""
    op = rng.choice(["+", "-"])
    other = rng.choice(["i", _gen_num_expr(rng, ["i"], 1)])
    step = f"({op} acc {other})"
    n = _rand_arg(rng, 0, 100)
    acc0 = _lit_src(rng.choice(_NUM_LITERALS))
    src = (
        f"(let loop ((i {n}) (acc {acc0}))\n"
        f"  (if (<= i 0) acc\n"
        f"      (loop (- i 1) {step})))"
    )
    return src


_CASE_KINDS = [
    _case_simple_rec,
    _case_tail_rec,
    _case_mutual_rec,
    _case_closure_factory,
    _case_param_as_op,
    _case_list_ops,
    _case_begin_body,
    _case_arith_leaf,
    _case_let_wrapped_rec,
    _case_let_wrapped_tail_rec,
    _case_nested_let_rec,
    _case_or_and_rec,
    _case_cond_rec,
    _case_named_let,
]


def gen_case(rng, idx):
    """Return (kind_name, source) for case number `idx`, deterministic
    given rng's prior state -- callers must share one rng across the
    whole batch, in index order, to get a reproducible sequence."""
    kind = rng.choice(_CASE_KINDS)
    src = kind(rng, idx)
    return kind.__name__, src


def gen_cases(seed, count):
    """The full, ordered list of (kind_name, source) for a fuzz batch.
    Regenerating with the same (seed, count) always reproduces the exact
    same cases -- this is the single source of truth both the runner
    subprocess and the reporting test process call into."""
    rng = random.Random(seed)
    return [gen_case(rng, i) for i in range(count)]
