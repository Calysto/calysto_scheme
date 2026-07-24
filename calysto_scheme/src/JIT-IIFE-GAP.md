# Why `let` (and everything built on it) can shut off the JIT

## Status: Fix A and Fix B have landed; Fix C was investigated and rejected

**Fix A and Fix B have both landed** (below). **Fix C was investigated in
depth and found unsafe as designed** — its central correctness argument
("this specific `set!` never needs undo-on-backtrack support") is false,
disproven with a reproducible counterexample. See Fix C's own section for
the full account. Don't re-attempt Fix C's original design without reading
that counterexample first.

**Fix A** (landed first) taught `_phase2_safe_walk_call` to resolve a
literal-lambda operator instead of giving up on it, so a function
containing a `let`/`or`/`and`/`cond`/`case` IIFE anywhere in its body could
reach Phase 2/JIT at all. Benchmarking that fix against the exact
motivating case here (`mi-loop`, below) found it didn't actually deliver a
speedup: the `let`'s operator was still, structurally, a literal
`lambda_aexp`, so the JIT still built a brand-new Scheme closure every time
it ran — and inside a hot self-recursive loop, JIT-compiled into a Python
`while True:`, that meant a real `compile()`/`exec()` call on *every tail
iteration*, not just once. Measured: `mi-loop` ran ~640× slower than the
`let`-free equivalent even after Fix A, because the thing actually costing
the time was never the classic-trampoline dispatch cost Fix A addressed —
it was this.

**Fix B** (landed second, superseding the "detect an IIFE, inline it"
design originally sketched below) fixes this at the root instead of
patching around it further: `let` — and everything built on it
(`or`/`and`/`cond`/`case`/`let*`/named-`let`/`letrec`/internal-`define`s,
all of which produce a `(let ...)` form at the parser level) — no longer
desugars to an IIFE at all. It parses directly to a native `let-aexp` AST
node, understood by the classic interpreter, Phase 2, and the JIT without
ever constructing an intermediate closure for the common (non-escaping)
case. Fixing this once at the parser, rather than teaching three separate
execution-tier walkers to pattern-match the IIFE shape after the fact,
means every macro built on `let` inherited the fix automatically, with
zero changes to any of those other transformers. See "Fix B" below for
what actually shipped (it differs in a few real ways from the original
sketch — a Plan-agent research pass found and fixed several correctness
gaps before implementation, documented there) and
`test_jit_native_let.py`/`_scheme_fuzz_gen.py`'s escaping-closure case for
the regression/fuzz coverage.

**Result**: `mi-loop` now runs at essentially the same speed as the
`let`-free equivalent — measured at 20–22 microseconds/pixel for *both*,
no longer distinguishable within noise, down from ~16,680
microseconds/pixel before Fix B (and ~14,770 before even Fix A). See Fix
B's "Landed" section for the full before/after table.

**Neither fix covers named-`let`'s own outer wrapper** (or plain
`letrec`/`letrec*`, or mutually-referential internal `define`s reaching
Phase 2/JIT *themselves*): the `set!` used to tie the recursive knot is
still there — now inside the native `let-aexp`'s body instead of an IIFE's
body, but `_phase2_safe_walk` still has no case for `assign_aexp` either
way, so the outer wrapper stays correctly excluded, confirmed unchanged
after Fix B landed. That's Fix C, still open — see below. The rest of this
document (mechanism, the original IIFE-based analysis, Fix B's original
"detect and inline" sketch) is preserved as originally written where it's
still an accurate historical record of what was tried and why the shipped
design ended up different.

---

## Summary

`let`, `or`, `and`, `cond`, `case`, `record-case`, and named-`let` all
desugar, at some point, into an application whose *operator* is a literal
`(lambda ...)` — an IIFE (immediately-invoked function expression):
`((lambda (v) body) e)`. Any function that contains one of these calls
*anywhere in its own body* was, before Fix A landed (see "Status" above),
permanently excluded from Phase 2/JIT, **not just slowed down** —
confirmed directly (see "Verified, directly" below, and re-verified
against the same example after the fix in Fix A's "Landed" section). This
was the dominant cost, and is now resolved for the non-`set!`-implicated
cases (`let`/`or`/`and`/`cond`/`case`); named-`let`'s own outer wrapper is
excluded for an independent reason (see "The relationship to `set!`"
below) and is unaffected by Fix A. A second, smaller cost stacks on top of
it for the specific closures that manage to still reach the JIT despite
that exclusion (e.g. a named-`let`'s self-recursive inner loop): those get
recompiled from scratch, at real `compile()`/`exec()` cost, on every
single call, because the JIT's compiled-function cache is keyed by Python
object identity and a closure created fresh every iteration never gets a
cache hit.

Both problems come from the same place `set!` support in the JIT came
from and got abandoned: the JIT's whole model is "resolve everything to a
concrete value once, then trust that value forever." `set!` breaks that
by changing a value *over time*; the IIFE cases break it by *the same
source-level lambda* being asked to work correctly with a *different*
captured value on every call. Named-`let` additionally implicates `set!`
literally, because `letrec` (which named-`let` desugars through) is
implemented with one.

---

## The mechanism, precisely

### 1. `let`/`or`/`and`/`cond`/`case` all produce an IIFE

`parser-cps.ss:719-734` (ordinary, non-named `let`):

```scheme
(define let-transformer^
  (lambda-macro (adatum handler fail k)
    ...
      (let* ((bindings (cadr^ adatum))
             (vars (map^ car^ bindings))
             (exps (map^ cadr^ bindings))
             (bodies (cddr^ adatum)))
        (k `((lambda ,vars ,@(at^ bodies)) ,@(at^ exps)))))))
```

`(let ((v e)) body)` becomes `((lambda (v) body) e)`. `or`/`and`/`cond`/
`case`/`record-case` all bottom out in a `let` somewhere in their own
expansion (see the earlier fix to these four macros), so this shape shows
up any time one of them appears in a hot loop, not just for an explicit
user-written `let`.

Named `let` goes through an extra layer — `parser-cps.ss:719-728`:

```scheme
;; named let
(k `(letrec ((,name (lambda ,vars ,@(at^ bodies)))) (,name ,@(at^ exps))))
```

and `letrec` (`parser-cps.ss:736-744`) is implemented with a real `set!`:

```scheme
(define* create-letrec-assignments^
  (lambda (vars procs k2)
    ...
      (k2 (cons `(,(car^ vars) 'undefined) bindings)
          (cons `(set! ,(car^ vars) ,(car^ procs)) assigns)))))
```

So `(let loop ((x 1)) body)` — confirmed directly via `(unparse (parse
'(let loop ((x 1)) body)))` — becomes:

```scheme
((lambda (loop)
   (set! loop (lambda (x) body))
   (loop 1))
 'undefined)
```

### 2. The JIT's *safety certification* can't resolve an IIFE operator, at all

`Scheme.py`'s `_resolve_operator` (~line 987) classifies an application's
operator expression before anything runs:

```python
def _resolve_operator(op_exp, env):
    if not isinstance(op_exp, cons):
        return ('unresolved', None)
    tag = op_exp.car
    if tag is symbol_lexical_address_aexp:
        return _resolve_lexical_address(op_exp.cdr.car, op_exp.cdr.cdr.car, env)
    if tag is symbol_var_aexp:
        return _resolve_var(op_exp.cdr.car, env)
    return ('unresolved', None)          # <-- lambda_aexp / app_aexp fall here
```

It only knows how to resolve a *named* operator (a variable or lexical
address). A literal `(lambda ...)` in operator position — or any other
computed operator — is unconditionally `'unresolved'`.

`_phase2_safe_walk_call` (~line 1005), which certifies "is it safe to
attempt Phase 2 for a function that makes this call," treats
`'unresolved'` as unsafe:

```python
kind, op = _resolve_operator(op_exp, env)
if kind != 'value':
    return False   # local param, computed operator, or unresolved — can't prove
```

So *any* function whose body contains an IIFE call — whether or not that
IIFE itself contains a `set!` — is certified `_is_phase2_safe() == False`.
Per `_is_phase2_safe`'s own docstring, that verdict is transitive: every
(direct or indirect) caller of that function is poisoned the same way.

### Verified, directly

```scheme
(define max-iter 50)
(define (mi-loop zx zy cx cy n)
  (let ((esc (> (+ (* zx zx) (* zy zy)) 4.0)))     ; plain let, no set! anywhere
    (if (>= n max-iter) n
        (if esc n
            (mi-loop (+ (- (* zx zx) (* zy zy)) cx) (+ (* 2.0 zx zy) cy) cx cy (+ n 1))))))
(define (mandelbrot-iterations cx cy) (mi-loop 0.0 0.0 cx cy 0))
(mandelbrot-iterations -0.5 0.5)
```

Instrumented directly against the running interpreter after one call:

```
proc[5] (is_direct_eval_safe): True
is_phase2_safe:                False
jit_lookup:                    None
```

`mi-loop`'s own body has no `set!` at all (`proc[5]` is `True`), yet
`_is_phase2_safe` is `False` and it was **never even attempted** for JIT
(`jit_lookup` is `None` after being called). It runs, permanently, on the
classic register-machine trampoline — the slowest tier in the system (the
Phase 1 baseline in `README-PERFORMANCE.md`'s own numbers, on the order of
tens-to-hundreds of times slower per call than JIT'd code) — for every
single one of its ~50 iterations, every single pixel. This is not "pays a
repeated compile cost"; it's "never gets a shot at Phase 2 or the JIT at
all."

**Re-verified after Fix A landed, same example, same instrumentation:**

```
proc[5] (is_direct_eval_safe): True
is_phase2_safe:                True
jit_lookup:                    <function _jit_fn at ...>
```

`mi-loop` now reaches Phase 2 and is JIT-compiled — pinned as
`test_mi_loop_plain_let_reaches_phase2_and_jit_and_is_correct` in
`test_jit_iife_operator.py`. `mandelbrot-iterations` (the caller) is no
longer poisoned either, confirming the transitivity direction also now
resolves cleanly in the safe case.

### The second, smaller effect: closures that *do* still reach the JIT

Named-`let`'s outer `(lambda (loop) (set! loop ...) (loop ...))` wrapper
has a direct `set!`, so it's excluded from Phase 2/JIT by
`_is_direct_eval_safe` on top of the `_is_phase2_safe` poisoning above —
its own call always falls to the classic trampoline. But the classic
trampoline's own generic "call a procedure" step (`apply_proc`,
`Scheme.py:590`) re-certifies `_is_phase2_safe` fresh for *every*
procedure it's asked to call — including the freshly-`set!`-assigned
inner `loop` closure. `loop`'s own body has no `set!` and is plain
self-recursion (which `_is_phase2_safe` special-cases as optimistically
safe), so `loop`, freshly created on *this* call, gets its own honest
`_is_phase2_safe() == True` and does reach Phase 2 → gets its first tail
call JIT-attempted via `_eval_direct`'s `app_aexp` case (`Scheme.py:1130`,
gated only by the closure's *own* `proc[5]`, not by `_is_phase2_safe`) →
successfully compiles.

Confirmed by patching `_jit_compile_proc` to record every closure it's
asked to compile, by formal-parameter list, across 5 calls to
`mandelbrot-iterations`:

```
{'(zx zy n)': 5}
```

Exactly one real compile of `loop`'s body per outer call — because `loop`
is a brand-new Python object every time, and `_jit_cache`/
`_phase2_safe_cache` are keyed by `id(proc)` (`_IdentityCache`,
`Scheme.py`), so a fresh identity is always a cache miss. `_jit_compile_proc`
(`Scheme.py:1225`) does a real `exec(compile(fn_src, ...))` on every one
of those misses. For a handful of calls this is cheap; at 3.84M inner
iterations (400×300 pixels × 80 max-iter) it's the dominant, unbounded
cost — this is the mechanism the original mandelbrot script investigation
found, correctly in its general shape, though it had attributed the cost
to the wrong closure (the outer `set!`-containing wrapper, which in fact
never even reaches this path) rather than to the inner self-recursive
loop specifically.

---

## The relationship to `set!`

There are two distinct things worth separating here, because they're easy
to conflate:

**1. A literal, but secondary, connection.** Named-`let` (and `do`,
mutually-referential internal `define`s, anything going through
`letrec`) is *implemented* with `set!` (`parser-cps.ss:736-744`). That
`set!` is why the *outer* letrec wrapper specifically can never be
JIT-attempted (`_is_direct_eval_safe` bars it directly, independent of the
IIFE-operator issue). But this is mostly moot in practice: the IIFE-operator
poisoning already excludes that wrapper's *enclosing* function regardless
of whether the letrec's own `set!` is present — a `set!`-free plain `let`
poisons its enclosing function exactly the same way, as demonstrated above.
So `set!` is *a* reason a piece of this is excluded, but not *the* reason
the overall pattern is slow. That said, this specific `set!` use — write
once, to a fresh cell, before any possible read — has a real
correctness argument for why it doesn't need the general mechanism's
undo-on-backtrack machinery at all; see Fix C below.

**2. The real, architectural connection.** Both problems are instances of
the same tension: `_jit_compile_proc`/`_capture`'s whole design freezes a
closure's free variables' *values* into the generated Python source once,
at compile time (`Scheme.py:1178-1196`'s own docstring is explicit about
this). That's sound only if those values can't legitimately differ across
uses of the compiled function:

- `set!` breaks the assumption by letting a *bound variable's value change
  over time* — the compiled code would need to notice a later mutation,
  which (per the abandoned investigation) additionally has to be
  *undoable* on `amb`/`choose` backtrack, not just visible.
- A closure created fresh every loop iteration (a `let`'s IIFE, a named-let's
  inner loop, `(lambda ...)` used as a value anywhere in a loop body)
  breaks the same assumption by needing the *same source-level lambda* to
  work correctly with a *different* captured value on every iteration —
  no backtracking involved, just ordinary parameter-passing semantics, but
  still a value that can't be baked in as a permanent Python literal.

Getting either right means moving part of the JIT from "freeze a value at
compile time" to "the compiled code must consult a value that can
legitimately differ across executions" — and doing that without
introducing a class of silently-stale-value bugs is exactly the risk
`README-PERFORMANCE.md`'s "Abandoned: `set!` support in Phase 2/JIT"
section weighed and declined to take on for `set!` specifically. The IIFE
case is a real cousin of that problem, not the same code path, but it
inherits the same "this needs real care, not a quick patch" character.

---

## What it would take to fix

These are two separable fixes with very different risk profiles.

### Fix A — teach `_phase2_safe_walk_call` to resolve a literal-lambda operator (addresses the *dominant* cost) — **Landed**

Right now `_resolve_operator` gives up immediately on anything that isn't
a `var_aexp`/`lexical_address_aexp`. It could additionally recognize: the
operator is *syntactically* a `lambda_aexp`, appearing directly in
operator position (exactly the `let`/`or`/`and`/`cond`/`case` shape) —
and instead of returning `'unresolved'`, recurse into that lambda's own
body with `_phase2_safe_walk_seq` the same way `_is_phase2_safe` already
does for an ordinary resolved closure, extending `env` with the lambda's
formals bound to "unknown-but-locally-scoped" the same way an ordinary
function parameter already is (this is not new machinery — a `let`-bound
variable and an ordinary function parameter are already handled
identically everywhere else in this walker).

This doesn't touch code generation at all — `_JitCompiler` already knows
how to compile an IIFE via `_jit_make_closure`/`_jit_call`
(`Scheme.py:1592-1610`, added in Phase 5). It only changes the *static
safety analysis* that currently refuses to even try. If it worked, `let`/
`or`/`and`/`cond`/`case` in a hot loop would behave like Phase 5's own
`make-adder` benchmark (a real, measured ~12× speedup, not "never
compiles") instead of "permanently stuck on the classic trampoline."

Caveats:
- Doesn't help named-`let`'s *outer* wrapper specifically — that one
  still has a direct `set!` and stays excluded by `_is_direct_eval_safe`
  regardless. What it *would* fix is every plain `let`/`or`/`and`/`cond`/
  `case` use, and it would stop those from poisoning whatever function
  encloses them — including a function that also happens to contain a
  named-`let`, whose *enclosing* function would then reach Phase 2 on its
  own merits instead of only getting there via named-let's incidental
  route through the classic trampoline's `apply_proc` re-check.
- Needs the recursive walk to still correctly reject a lambda whose body
  is itself unsafe (contains a `set!`, calls something unresolvable,
  etc.) — straightforward reuse of the existing walk, but needs a test
  for the case where it should still say no.
- Should be verified with this project's usual differential-fuzzing
  discipline (`tests/test_jit_fuzz.py`-style: compare trampoline / Phase 2
  / JIT results on many generated programs), since a wrong "yes" here is
  a silent-wrong-answer risk, not just a missed optimization — the same
  asymmetry the `set!` investigation flagged.

**Estimated effort/risk: moderate, contained.** It's a change to one
predicate (`_resolve_operator`/`_phase2_safe_walk_call`), not to code
generation, with an existing fallback (today's behavior) if the new case
is written conservatively.

**Landed.** Implemented in `_phase2_safe_walk_call` largely as scoped
above, with one correction found while verifying it against the `mi-loop`
example: "extending `env` with the lambda's formals bound to
unknown-but-locally-scoped" cannot mean *reusing `env` unmodified*, which
was the first thing tried. Lexical-address depths inside the IIFE body
are counted relative to the actual runtime frame stack, which gains one
new frame per level of IIFE nesting; leaving `env` unmodified silently
shifts every reference more than one frame away (e.g. a call to a
primitive like `>=` from inside the `let`, which is exactly what
`mi-loop` does) onto the wrong frame, and `_resolve_lexical_address`
either resolves the wrong binding or fails outright — in this case,
`_is_phase2_safe(mi-loop)` stayed `False` even with the new branch
present, for the new, wrong reason instead of the old one. The fix pushes
a *real* frame via `_extend_direct(env, formals, dummy_args)` before
recursing, with placeholder values (`False`) that are never actually
read: a depth-0 reference to one of these formals used as an operator is
still unconditionally unsafe (`'local'`), and used as a plain value is
still unconditionally safe (`_phase2_safe_walk`'s `lexical_address_aexp`
case) — only the frame's *shape* (formal names, and its presence at all,
for depth-counting) needs to be right. Confirmed correct on two levels of
nested `let` specifically because a single added frame wasn't enough to
catch this class of bug; one level alone still resolves depth-1
references correctly by coincidence.

Verified per the caveats above:
- The `mi-loop` example now reaches Phase 2 and JIT-compiles — see
  "Verified, directly"'s post-fix re-check earlier in this document.
- A body that's genuinely unsafe (calls its own formal in operator
  position, e.g.) is still correctly rejected —
  `test_unsafe_iife_body_is_still_rejected` in
  `test_jit_iife_operator.py`.
- Named-`let`'s outer wrapper is still correctly excluded (its literal
  `set!`) — `test_named_let_outer_wrapper_still_excluded_but_correct`.
- Differential fuzzing: six new IIFE-shaped generators (`let`, nested
  `let`, `let`-wrapped tail recursion, `or`/`and`, `cond`, named-`let`)
  added to `_scheme_fuzz_gen.py`, run through `test_jit_fuzz.py`'s
  existing fast/slow/phase2-only three-way comparison across several
  seeds and case counts (up to 1500 cases) with no mismatches. This also
  caught one unrelated fuzz-generator bug in the process: an
  `(and (<= n base) (>= n 0))` base-case test formed a window a `-2`
  recursive step could jump over, which hung specifically under the
  trampoline (real Scheme tail calls there don't grow the Python call
  stack, so there's no `RecursionError` to eventually cap it) — fixed by
  using a monotonic threshold test instead, consistent with every other
  case in that file.

### Fix B — inline non-escaping IIFEs directly as Python locals (addresses the *secondary*, repeated-compile cost) — **Landed (superseded by a native `let-aexp` AST node)**

Even with Fix A, a closure created fresh every iteration (`let`'s own
IIFE, named-`let`'s inner loop) still pays one real `compile()`/`exec()`
per fresh identity, because `_jit_cache` is keyed by object identity. The
deeper fix is to recognize when an IIFE's resulting closure *provably
never escapes* its own immediate call — never stored, returned, or passed
anywhere it could be invoked again later or from elsewhere — and compile
it as plain inline Python (`_j_v = <compiled test>`, then the body's
statements directly, in the *same* Python function `mandelbrot-iterations`
or `mi-loop` already compiles to) instead of constructing a proc tuple and
routing through `_jit_make_closure`/`_jit_call` at all. `let`, `or`,
`and`, `cond`, `case`, `record-case`'s specific desugarings all satisfy
this (none of them ever store the "rest" closure anywhere — this was
exactly what let the gensym fix drop their thunk-wrapping safely).
Named-`let`'s inner loop does *not* satisfy it as cleanly (it's
self-referential and, for very large bodies, may be worth keeping as a
real, independently-JIT'able closure rather than inlining) — this fix is
naturally scoped to the non-recursive control-flow macros first.

This is real interpreter code generation work, with real correctness
surface: escape analysis has to be conservative in the same "too
cautious costs speed, too permissive costs correctness" way everything
else in this JIT is, and it has to compose with Phase 4's tail-call
flattening (an inlined IIFE whose own tail position is a self-recursive
call back to the enclosing loop must still participate in the `while
True:` rewrite, not silently reintroduce a nested Python scope).

**Estimated effort/risk: substantially larger.** New static analysis
(escape analysis) plus new code-generation paths in `_JitCompiler`,
needing the same fuzzing-based verification discipline as Fix A, but with
more surface area for a subtle miss.

**Landed — but not as scoped above.** The plan above was "detect an IIFE
in `_JitCompiler`, decide per-call whether to inline it." What actually
shipped goes one level deeper, per explicit user direction after
benchmarking Fix A: **`let` no longer produces an IIFE at all.** Rather
than teaching `_eval_direct`/`_phase2_safe_walk`/`_JitCompiler` to each
recognize and special-case a *pattern* in the AST (an application whose
operator happens to be a literal lambda), `let`'s own parsing
(`parser-cps.ss`) was changed to emit a genuine, first-class `let-aexp`
node directly — the same kind of change as if `if` or `lambda` were core
forms instead of sugar, which is exactly what they already are. Since
`or`/`and`/`cond`/`case`/`let*`/named-`let`/`letrec`/internal-`define`s
all bottom out by *producing* a `(let ...)` s-expression in their own
macro expansions (not by desugaring some other, independent way), fixing
`let` once fixed all of them, automatically, with zero changes to any of
those other transformers.

**What shipped, by file:**

- `parser-cps.ss`: a new `let-aexp (vars val-aexps bodies info)` variant
  of `define-datatype aexpression`. Plain (unnamed) `let` is parsed
  directly in `aparse` — modeled line-for-line on how `lambda-no-defines?^`
  already parses a lambda body (parse the binding value-expressions
  against the current `senv`, extend `senv` with the new vars-frame, parse
  the body against that) — instead of going through macro expansion to
  build an IIFE. Named let is unchanged: it still expands through
  `letrec-transformer^` into `(letrec ((name (lambda ...))) (name ...))`,
  and `letrec` itself still just produces a plain, unnamed `(let ((name
  'undefined)) (set! name proc) body...)` — which *is* now parsed as a
  `let-aexp` too, `set!` and all, automatically and correctly (the safety
  walkers still see the `assign_aexp` inside its body and still exclude it
  from Phase 2/JIT, exactly as before — no accidental unlock, no
  regression, confirmed against the existing pinned
  `test_named_let_outer_wrapper_still_excluded_but_correct` test, unmodified,
  now exercising the new node instead of the old IIFE shape). A gap found
  during design and fixed before landing: `(define-syntax let ...)`
  genuinely overrides `let` today (confirmed empirically) — the new direct
  dispatch is guarded by an `eq?` check against the original
  `let-transformer^` in `macro-env`, falling through to ordinary macro
  expansion whenever `let` has been redefined, so this capability is
  provably unaffected, not just assumed fine. A second gap: `aunparse`
  (backing the documented `unparse` primitive, and used internally for
  `*tracing-on?*` output and unit-test diagnostics — the exact tool this
  document's own "Verified, directly" section used) had no case for the
  new tag; added.
- `interpreter-cps.ss`: one new `let-aexp` case in `m`'s CPS dispatch —
  evaluate `val-aexps` against the current env (`m*`, left-to-right,
  fail-threaded, the same helper `try-catch-handler` already composes this
  way), extend the env with one new frame (`extend`, same helper an
  ordinary function call already uses), evaluate `bodies` in that frame
  (`eval-sequence`, tail-preserving). No closure construction, no
  apply-dispatch layer — a genuine simplification even for the classic
  trampoline, not just a JIT-only concern. Confirmed this needs no special
  interaction with `amb`/`choose`/backtracking: `assign-aexp` is the only
  place that wraps `fail` with undo logic, because `set!` mutates an
  *existing*, potentially-shared binding cell — plain environment
  extension has no shared mutable state to undo, so a native `let-aexp` is
  structurally identical to an ordinary function call's frame extension in
  this respect. `test_all.ss`'s own `floors2` test (five levels of nested
  `let` binding to `(choose ...)`, with `(require ...)` between each) is a
  real, pre-existing, comprehensive exercise of exactly this and passed
  unmodified.
- `Scheme.py`: `_eval_direct` gained a `symbol_let_aexp` case mirroring
  `begin_aexp`'s existing inline tail-loop (no closure, no `apply_proc`).
  `_phase2_safe_walk` gained a case reusing Fix A's own "push a real
  placeholder frame via `_extend_direct` before recursing" discipline
  (Fix A's original literal-lambda-operator branch in
  `_phase2_safe_walk_call` is kept, unmodified, as a safety net for a user
  directly hand-writing `((lambda (x) ...) e)`, still valid application
  syntax — it's just no longer reached by macro-generated code). The JIT
  (`_JitCompiler`) does the actual work:
  - **Escape analysis**: a new `_let_body_has_escaping_closure` walker
    scans a `let`'s body (not its binding *values* — see below) for any of
    the four AST tags that create a first-class closure —
    `lambda_aexp`/`mu_lambda_aexp`/`trace_lambda_aexp`/`mu_trace_lambda_aexp`
    (the original sketch above only named the first two; the other two
    were found and added before landing). It deliberately does **not**
    stop at a nested `let-aexp` boundary — a closure escaping through a
    nested `let` must mark the *outer* `let` "real" too, not just the
    inner one. This gives a clean, provably-correct simplification found
    during design review: since the scan already recurses through nested
    `let`s, a "fast" (escape-free) `let` can never contain a "real"
    (escaping) `let` as a descendant, so the fallback path never needs to
    *compose* with the fast path's scope stack at all — it just defers the
    entire `let` to a real closure via the **already-existing, unmodified**
    `_lambda`/`_jit_make_closure`/`_jit_call` machinery (exactly the
    pre-Fix-B IIFE codegen shape), with no multi-level frame-chaining
    machinery needed. This was the single largest simplification made to
    the original plan.
  - **Fast path** (no escaping closure found): each binding value compiles
    to a fresh, uniquely-suffixed synthetic Python local (`_jc_let_%d`,
    following the existing `_capture_const` freshness convention, so
    there's never a shadowing collision by construction — real Scheme name
    shadowing is instead handled by scope-stack lookup order). In tail
    position, this is plain Python assignment statements followed by the
    body, with the *last* body expression compiled via a recursive
    `tail_stmts()` call so a self-recursive tail call inside the `let`'s
    body still becomes a `continue` in the enclosing `while True:` loop —
    exactly `mi-loop`'s own case. In non-tail (expression) position — e.g.
    `(+ 1 (let ((x 5)) (* x x)))`, which already compiled successfully
    before Fix B via the generic IIFE-`_jit_call` path — the whole thing
    reduces to a single Python expression via a walrus-operator tuple:
    `(_jc_let_0 := val0, ..., body_expr)[-1]` (verified directly to be
    valid, standard Python: tuple elements evaluate left-to-right, and a
    walrus assignment inside a tuple both assigns and yields its value; a
    trailing comma is added when there's exactly one element so a
    single-binding, single-body-expression `let` still produces a real
    tuple, not a parenthesized-expression indexing error). A gap found and
    fixed before landing: unlike the old codegen (which only ever captured
    a `let`'s body as an opaque constant, never tried to compile it), the
    fast path *does* try to inline-compile the body — so a
    `_TrampolineFallback` raised partway through (e.g. a body statement
    using a tag `expr()` doesn't support, like `begin_aexp`) is now caught
    **locally, at this `let`'s own compilation**, and treated as "use the
    deferred path for this `let`" instead of being allowed to abort the
    *entire enclosing function's* compile — preserving the invariant
    (true both before and after Fix B) that adding a `let` anywhere in a
    function's body never newly breaks that function's overall
    JIT-compilability.
  - **Reference resolution**: every call site that resolves a lexical
    reference against the closure's captured environment —
    `expr()`'s own `lexical_address_aexp`/`var_aexp` cases, `_var()`,
    `_is_self_ref` (used by `tail_stmts` to detect a self-recursive tail
    call), and `_is_unshadowed_primitive` (used by `_app` to decide
    whether `+`/`<`/`car`/... can inline as raw Python operators) — first
    checks the open fast-path scope stack before falling through to the
    existing "depth 0 = own parameter, depth > 0 = capture from `self._env`"
    logic (with the depth adjusted by the number of open scopes). Missing
    `_is_self_ref` specifically was flagged during design review as a
    silent-wrong-answer risk (a self-recursive tail call inside a `let`
    could be misclassified) — caught before implementation, not after. A
    further correctness requirement found *during* implementation, not in
    the original design: a `let`-bound variable's *value* can itself be an
    arbitrary runtime value, including a genuine closure that's created
    and consumed entirely within the same `let`'s body without ever
    escaping it — e.g. `(let ((f (lambda (x) (* x x)))) (f 5))`. The
    escape scan only looks at the *body* for a closure-creating node (it
    doesn't need to care about the binding *values* — nothing about
    binding a closure to a local variable makes it escape by itself), so
    this case correctly takes the fast path; but `_app`'s own "can this
    operator be proven to yield a plain Python callable at compile time"
    check (previously only recognizing a literal application/lambda, or a
    depth-0 own-parameter reference) also needed to recognize a
    fast-path-scoped operator reference at *any* depth within the open
    scopes, or `(f 5)` would compile to a bare Python call on a Scheme
    proc tuple and crash at runtime. Fixed and pinned directly (see
    `test_let_bound_lambda_called_internally_never_escapes`).
  - `(use-lexical-address #f)` — a live, user-toggleable runtime flag
    under which `aparse` produces name-based `var-aexp` nodes everywhere
    instead of lexical addresses — needed its own name-keyed,
    innermost-first parallel lookup against the scope stack (not just the
    depth-keyed one), or `let`-in-a-hot-loop under this mode would have
    silently stayed on the slow path instead of the fast one. Not in the
    original design; added and pinned
    (`test_use_lexical_address_false_still_correct`).

**Verified:**
- Full project test suite (442 tests as of landing) green throughout a
  staged rollout (AST+parser+classic-interpreter, then Phase 2, then JIT
  structural support on the deferred path only, then escape analysis + the
  fast path last) — each stage independently regenerated
  (`cd calysto_scheme/src && make`, the full `.ss` → `scheme.py` pipeline,
  since this touches `parser-cps.ss`/`interpreter-cps.ss`, not just
  `Scheme.py`) and tested before moving to the next.
- `test_jit_native_let.py`: fast path structurally confirmed in use (not
  just correct) for `mi-loop`'s own shape; non-tail expression position;
  three levels of nested `let`; a `let`-bound closure called internally
  (never escaping); an escaping closure via the deferred path; the
  "sticky real" nested-escape inheritance case; primitive-shadowing
  (`(let ((- +)(+ -)) (+ 1 2))`); `or`/`and`/`cond` reaching the fast path;
  `(use-lexical-address #f)`; backtracking through a native `let`.
- Differential fuzzing (`test_jit_fuzz.py`): a new
  `_case_let_escaping_closure_rec` generator (a self-recursive function
  whose body builds a `let`-bound closure that captures an *outer* `let`'s
  variable — the same "sticky real" shape) added alongside Fix A's
  existing `let`-shaped generators, run across 5 different seeds at 1200
  cases each (6000 generated programs total) with the existing
  fast/slow/phase2-only three-way comparison — no mismatches.
- Benchmark (the `mi-loop`/mandelbrot case from "Verified, directly",
  N=5000 pixels):

  | Variant | Before Fix A | After Fix A (before Fix B) | After Fix B |
  |---|---|---|---|
  | `let`-free `mi-loop` | — | — | 21.8 µs/pixel |
  | `let`-wrapped `mi-loop` | ~14,770 µs/pixel (forced trampoline) | ~16,680 µs/pixel | 20.4 µs/pixel |

  Fix B doesn't just improve on Fix A — it makes the `let`-wrapped version
  statistically indistinguishable from (and, in this run, marginally
  faster than) the hand-optimized `let`-free workaround this document's
  own `mandelbrot.ss` example originally had to resort to.

### Fix C — a restricted, non-undoable internal `set!` for tying the letrec knot (necessary, not sufficient, for named-`let`'s *own* wrapper and internal `define`s — see correction below) — **Investigated and rejected: unsafe as designed**

Fix A doesn't help named-`let`'s outer `(lambda (loop) (set! loop ...)
(loop ...))` wrapper reach Phase 2/JIT on its own merits — it's excluded
by `_is_direct_eval_safe` directly, because of the literal `set!`
`letrec`'s desugaring emits (`parser-cps.ss:746-751`). But that `set!` is
not an ordinary, arbitrary mutation: it always writes to a *brand-new*
binding cell (allocated moments earlier as `'undefined`), exactly once,
strictly before anything could possibly read it — the only reader is the
closure being tied to itself, which cannot run until after the `set!`
completes. Nothing outside this one `let`/`letrec` invocation ever
observes the placeholder, and every re-entry — including one driven by
`amb`/`choose` backtracking replaying the same code path — allocates a
*fresh* cell. There is no aliasing across a backtrack boundary for undo
to restore, unlike an ordinary `set!` on a variable that's shared or
observable from outside the mutating scope, which is what the fail-continuation
undo machinery in `interpreter-cps.ss`'s `assign-aexp` handling exists
for.

This generalizes past named-`let`: `letrec-transformer^` is also what
internal `define`s desugar through (`parser-cps.ss:646-652`,
`get-internal-defines^`/`create-letrec-bindings^`), so this is the exact
same tie-the-knot pattern behind Phase 5's separately-documented "closures
via internal `define`/`let` remain unsupported" gap. One restricted,
provably-undo-free assignment primitive would unlock named-`let`, plain
`letrec`/`letrec*`, *and* internal defines at once, since they all funnel
through this one mechanism.

The idea: introduce a second AST tag for this pattern — say
`internal_assign_aexp` — distinct from user-facing `set!`'s
`assign_aexp`, emitted only by `create-letrec-assignments^` (never
reachable from ordinary user syntax), and skip the undo-continuation setup
for it entirely.

**Correction, found while checking this more carefully: Fix C alone is
inert.** The wrapper closure has *two* independent reasons
`_is_phase2_safe` is `False`, not one — Fix C only removes the first.
The second: its body calls `loop` — its own formal parameter — in
operator position, and that shape is *separately* excluded regardless of
any `set!`, confirmed directly on the simplest possible case with no
`set!` anywhere at all:

```scheme
(define (apply-twice f x) (f (f x)))
```
```
proc[5]: True
is_phase2_safe: False
jit_lookup: None
```

This is Phase 6/8's own already-documented "local parameter used in
operator position" gap: `_resolve_operator` returns `('local', None)` for
a depth-0 lexical address (a parameter's value is only known at runtime),
and `_phase2_safe_walk_call` treats `'local'` exactly like
`'unresolved'` — unconditionally unsafe. The named-`let` wrapper's
`(loop ...)` call is precisely this shape from its own point of view
(`loop` is *its* parameter). So even with Fix C making the `set!` itself
invisible to `_is_direct_eval_safe`, `_is_phase2_safe(wrapper)` would
still be `False` on this second, completely independent ground, and
`apply_proc` would still never enter Phase 2 for it — Fix C's benefit is
entirely gated behind also resolving *this* shape.

Note this second gap is not simply "the same as Fix A" — Fix A as scoped
above only recognizes a *syntactically literal* lambda in operator
position; resolving `loop` here needs something Fix A doesn't have:
flow-sensitivity, i.e. recognizing that this *particular* local variable
was just given a concrete, known value by the `internal_assign_aexp`
statement immediately preceding this call, in the same body sequence.
That's a genuinely different (if kindred) piece of analysis from
recognizing a literal lambda operator, and it would need to be added
alongside Fix C — and, more generally, on top of `_resolve_operator`'s own
Phase-8-era conservatism about the plain `apply-twice` case above, which
isn't specific to `let`/`letrec` at all and is worth fixing in its own
right regardless of anything in this document.

This is real, from-scratch implementation work, not a check to relax.
Confirmed directly: `symbol_assign_aexp` appears exactly once in the whole
of `Scheme.py` — in `_is_direct_eval_safe`'s exclusion check — so there is
currently *zero* execution support for any `set!`, anywhere in Phase 2 or
the JIT (`_eval_direct` has no case for it; neither does
`_phase2_safe_walk`; neither does `_JitCompiler`). Making the new tag
actually run means:
- A case in `_eval_direct` and a case in `_phase2_safe_walk` treating
  `internal_assign_aexp` as safe iff its value-expression is safe (unlike
  the "else: return False" every unhandled tag falls into today).
- Real codegen in `_JitCompiler` — this is precisely the "handle
  `assign_aexp` with `nonlocal`" idea from `README-PERFORMANCE.md`'s
  abandoned-`set!` row (~700× potential, measured), just scoped to the one
  case that's provably undo-free by construction, sidestepping the
  `amb`/`choose` entanglement that killed the general version.
- The same fuzzing-based verification this project already leans on for
  JIT safety work — specifically, tests that combine `amb`/`choose` with
  `letrec`/named-`let`/internal-defines to confirm the "always a fresh
  cell, never aliased across a backtrack boundary" assumption actually
  holds, not just that it's plausible by inspection. Given the asymmetric
  risk (a wrong "yes" here is a silent-wrong-answer bug, same as
  everywhere else in this document), this verification isn't optional.

**Estimated effort/risk: moderate on its own terms** — narrower and safer
than the original general-`set!` effort, since it never has to solve
undo/backtracking at all — **but not sufficient by itself.** It needs a
companion change (resolving a parameter just fixed by an
`internal_assign_aexp` in the same body, and/or the plain
`apply-twice`-shaped gap generally) before it produces any observable
effect. Treat the pair as one unit of work, not two independent options.

**Investigated (via a dedicated design-review pass, the same discipline
used before Fix B) and rejected before any code was written.** The core
claim above — "this `set!` always writes to a brand-new cell, exactly
once, before anything could possibly read it, and every re-entry
(including one driven by `amb`/`choose` backtracking) allocates a *fresh*
cell, so there's no aliasing across a backtrack boundary for undo to
restore" — **is false.** It's true only for the narrow sub-claim "two
*separate* logical entries into the same `let`/`letrec` never share
storage." It was never true for the actual risk: a closure that *escapes*
one entry (stored somewhere reachable from outside, not just returned
normally) and is then *invoked after* a backtrack unwinds back through the
point where the tie-in `set!` ran.

Confirmed directly, reproducibly, against the live interpreter — no
hypothetical, no analogy:

```scheme
(define captured (vector #f))
(choose
  (begin
    (let loop ((i 0))
      (vector-set! captured 0 loop)
      (if (= i 0) 'first (loop (- i 1))))
    (require #f)                       ; force a full unwind, unconditionally,
    'unreached)                        ; *after* the named-let already
                                        ; completed normally
  ((vector-ref captured 0) 3))
```

Today (full undo, as it works before this investigation and unchanged by
it): this raises `RunTimeError: attempt to apply non-procedure 'undefined'`.
`loop` escapes via `vector-set!` (a plain mutation, never undo-wrapped —
only `assign-aexp` gets that treatment, confirmed as the only such site in
`interpreter-cps.ss`'s `m`), and when it's later invoked, its own
self-reference to `loop` resolves to `'undefined` — because the outer
`(require #f)`'s full backtrack correctly reverted the tie-in `set!` via
the *existing* undo machinery on its way back out, exactly as that
machinery is supposed to work. **If `internal_assign_aexp` had skipped
undo tracking, as Fix C's whole premise requires, this exact program would
instead silently keep running** — `loop`'s cell would never revert, the
stale self-reference would resolve, and `(loop 3)` would actually execute
— a real, verified, silent change in observable behavior, not a
theoretical risk. `(require #f)` doesn't even need to be lexically inside
the named-`let` for this to happen; the hazard comes from an *outer*
backtrack unwinding *through* the tie-in point, which can happen from
anywhere dynamically enclosing the call, regardless of what the
named-`let`'s own body does or doesn't contain.

This is exactly the finding `README-PERFORMANCE.md`'s "Abandoned: `set!`
support in Phase 2/JIT" section already made for the *general* case — "a
function can be running 'inside' an active backtracking search without any
`choose`/`amb` appearing anywhere in its own text ... the risk comes from
the *caller's* dynamic context, not the callee's code" — and already
declined to ship even partially verified. Restricting *which* assignments
get the new tag (Fix C's whole idea) changes nothing about *who might be
dynamically backtracking through them* at the moment they run. Phase 2 and
the JIT have no `fail`/undo-continuation parameter at all (confirmed:
`symbol_assign_aexp` appears exactly once in `Scheme.py`, in
`_is_direct_eval_safe`'s exclusion check) — there is no channel for either
of them to know "a live backtracking search might currently be in
progress," no matter how narrowly the mutation being considered is scoped.
`README-PERFORMANCE.md` already considered and rejected the natural
mitigation (checking the live fail-continuation register against a known
"no backtracking possible" baseline) as still not fully verifiable, for
the same underlying reason.

**This also isn't just a narrower-but-still-worthwhile win with an asterisk
attached.** A companion measurement (instrumenting `_jit_compile_proc` on
a hot named-`let`-based benchmark) found that named-`let`'s own wrapper
dispatch cost — the piece Fix C targets — is only about half of the total
remaining overhead. The other half comes from an entirely different,
`set!`-unrelated mechanism: the tied-in inner closure is a fresh Python
object every single outer call, so `_jit_cache` never gets a hit and it
pays a real `compile()`/`exec()` cost every time, exactly the "second,
smaller effect" this document's own "Verified, directly" section already
described. Fix C, even if it were safe, would do nothing about that half.

**If a genuinely sound version of this is ever wanted**, the only path
found that actually closes the gap is a *whole-program*, not per-function,
gate: track whether `choose`/`amb` has *ever actually executed* anywhere
in the running process (a global flag, bumped inside `choose-aexp`'s
evaluator, mirroring the existing `_binding_write_epoch` pattern already
used for cache invalidation elsewhere in `Scheme.py`), and only treat
`internal_assign_aexp` as safe while that flag is still false —
unconditionally declining it, and invalidating any cache entries that used
the optimization, the instant it flips true. This removes the precondition
the counterexample above depends on (a live/possible backtracking
context), but at real cost: **zero benefit for any program or
REPL/notebook session that uses `amb`/`choose` anywhere at all**, even in
code unrelated to the named-`let`/`letrec` being considered — a real cost
given this project treats `amb`/`choose` as a supported, documented
feature (`test_all.ss`'s own `floors2` test exercises it extensively), not
a corner case. Building and, especially, *verifying* that gate (correctly
extending across `load`/interactive `eval` of new top-level forms after
compiles have already happened) is itself a nontrivial, adversarial-testing-
requiring project — not something to bolt on casually to recover roughly
half of an already-modest remaining win.

**Two regression tests worth keeping regardless of what's decided about
Fix C** (both already run against the live interpreter as part of this
investigation, both pin *today's* correct, fully-undo-protected behavior):
the named-`let` example above (must keep raising `RunTimeError`, not
silently start succeeding, for as long as full undo is kept), and its
`letrec` analogue (a `letrec` binding whose *value expression* itself
contains `choose`, escaped via a mutable structure — pins the more general
aliasing hazard, not just named-let's narrower self-tie shape).

**Recommendation: do not implement Fix C as designed.** Named-`let`/
`letrec`/internal-defines' *enclosing* function already gets the full
benefit of Fix A+B today; only the wrapper's own dispatch remains
unaddressed, worth roughly 2× on the specific case measured here, not an
order of magnitude — and reaching even that requires either accepting a
proven, reproducible silent-behavior-change under `amb`/`choose`, or
building materially more infrastructure (the whole-program gate above)
than this section originally scoped, for a payoff now known to be smaller
than assumed. The separate, `set!`-unrelated recompilation-avoidance
opportunity noted above (a closure built from an already-JIT-compiled body
shouldn't need a fresh `compile()`/`exec()` on every call just because its
free variables differ) is a more promising, better-isolated place to look
for further speedup, if one is wanted — untouched by any of the soundness
problems here.

### Recommendation

Fix A is the higher-leverage, lowest-risk piece — worth doing on its own
even without the others, since it turns "never reaches Phase 2 at all"
into "reaches Phase 2 and the existing JIT pipeline," which is already a
large win by itself (see Phase 5's own measured numbers for exactly this
call shape). **Landed** — see "Fix A" above for the implementation and its
verification.

Fix B, as actually shipped (a native `let-aexp` AST node, not the
"detect-and-inline the IIFE pattern" design originally sketched in this
section), turned out to be both the *necessary* fix and, once reframed at
the parser level instead of the JIT-compiler level, no riskier than doing
it the originally-scoped way — arguably less risky, since fixing `let`
once at its source means `or`/`and`/`cond`/`case`/`let*`/named-`let`/
`letrec`/internal-defines all inherit the fix for free, rather than each
needing independent verification that the JIT's IIFE-detection pattern
still matches whatever shape their macro expansion happens to produce.
**Landed** — see "Fix B" above for the full account, including the
several real correctness gaps a design-review pass found and fixed before
implementation (an original design flaw, not an implementation slip:
patch-the-symptom approaches like the original Fix B sketch tend to
accumulate exactly this kind of gap, one per call site that turns out to
need updating — the root-cause fix instead needed the gaps found once,
structurally, not per call site).

Fix C turned out to be worse than "not a standalone follow-up" — its core
safety argument is false, disproven with a reproducible counterexample
(see "Fix C" above). **Rejected as designed; do not implement.** The
*enclosing* function around a named-`let`/`letrec`/internal-define already
benefits fully from Fix A+B, per the caveat noted in Fix A's own section
above (an enclosing function that also happens to contain a named-`let`
reaches Phase 2 independently of named-let's own, narrower path through
it) — what's NOT reachable is named-`let`'s/`letrec`'s *own* outer
wrapper, and closing that gap the way Fix C proposed means accepting a
proven silent-behavior-change under `amb`/`choose`. A sound version would
need a whole-program "has backtracking ever run" gate (sketched in Fix C's
section above), which is real, verifiable-but-nontrivial infrastructure
for a payoff now measured at roughly 2× on the specific case checked, not
an order of magnitude — about half of the remaining cost turns out to be a
separate, `set!`-unrelated recompilation issue (a fresh closure identity
every call means a fresh `compile()`/`exec()` every call) that Fix C
wouldn't touch even if it were safe. This document's remaining open items
are: that recompilation-avoidance idea (untouched by any soundness
problem, worth its own separate investigation), and the whole-program gate
above, if named-`let`'s own wrapper reaching Phase 2/JIT is ever judged
worth that infrastructure cost.
