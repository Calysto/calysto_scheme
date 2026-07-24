# Why `let` (and everything built on it) can shut off the JIT

## Status: Fix A has landed

`_phase2_safe_walk_call` now resolves a literal-lambda operator instead of
giving up on it — see "Fix A" below for what shipped and
`test_jit_iife_operator.py`/`_scheme_fuzz_gen.py`'s IIFE-shaped cases for
the regression/fuzz coverage. **This closes the dominant cost described
below for `let`/`or`/`and`/`cond`/`case`**: a function whose body contains
one of these no longer gets permanently excluded from Phase 2/JIT the way
this document originally found — the `mi-loop` example in "Verified,
directly" now reaches Phase 2 and JIT-compiles.

**Fix A does not cover named-`let`** (or plain `letrec`/`letrec*`, or
mutually-referential internal `define`s): its outer
`(lambda (loop) (set! loop ...) (loop ...))` wrapper still contains a
literal `set!`, which `_phase2_safe_walk` has no case for regardless of
how its operator resolves — confirmed still excluded after Fix A landed.
That gap, and the second, smaller repeated-compile cost described below,
are exactly Fix B and Fix C, both still open. The rest of this document
is preserved as originally written (including the now-historical
"Verified, directly" numbers) since it's still the accurate account of
what those two remaining fixes require and why.

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

### Fix B — inline non-escaping IIFEs directly as Python locals (addresses the *secondary*, repeated-compile cost)

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

### Fix C — a restricted, non-undoable internal `set!` for tying the letrec knot (necessary, not sufficient, for named-`let`'s *own* wrapper and internal `define`s — see correction below)

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

### Recommendation

Fix A is the higher-leverage, lowest-risk piece — worth doing on its own
even without the others, since it turns "never reaches Phase 2 at all"
into "reaches Phase 2 and the existing JIT pipeline," which is already a
large win by itself (see Phase 5's own measured numbers for exactly this
call shape). **Landed** — see "Fix A" above for the implementation and its
verification.

Fix C is *not* a standalone follow-up the way it first looked — traced
through directly, it produces zero observable benefit until it's paired
with resolving the "own parameter, just assigned, called as operator"
shape (found while checking this). That combination is worth doing
together, specifically for named-`let`/`letrec`/internal-defines, and is
still narrower and lower-risk than reviving general `set!` support. Fix A
having landed removes one of its two preconditions — the *enclosing*
function around a named-`let`/`letrec`/internal-define now benefits from
Fix A on its own merits already, per the caveat noted in Fix A's own
section above (an enclosing function that also happens to contain a
named-`let` now reaches Phase 2 independently of named-let's own,
narrower path through it). What's left, unchanged: Fix C plus the
"own parameter, just assigned, called as operator" resolution, still
needed together before named-`let`'s *own* outer wrapper (as opposed to
its enclosing function) can reach Phase 2/JIT.

Fix B is the deepest and riskiest of the three, and only obviously worth
its cost now that Fix A is in and its residual repeated-compile cost
(the second, smaller effect described above — a fresh closure identity
per loop iteration paying a real `compile()`/`exec()` every time) can
actually be measured on real workloads instead of estimated.
