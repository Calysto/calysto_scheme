# Calysto Scheme — Python Performance Optimizations

This document describes the performance optimizations made to the
Python interpreter (`calysto_scheme/scheme.py`) and their implementation
details. All changes live in `Scheme.py` and `translate_rm.py`; the
generated `scheme.py` is rebuilt with:

```
python translate_rm.py source-rm.ss ../scheme.py
```

## Benchmark

`(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))`
`(fib 20)` — 21,891 recursive calls, no memoization.

| State | Time | vs original |
|-------|------|-------------|
| Original | 7,400 ms | 1× |
| After Phase 1 (trampoline optimizations) | 1,540 ms | **4.8×** |
| After Phase 2 (direct eval fast path) | 82 ms | **91×** |
| After Phase 3 (JIT to Python) | 1.6 ms | **~4,500×** |
| After Phase 7 (drop self-recursion indirection) | 1.2 ms | **~6,200×** |
| Python native | 1.0 ms | — |

The JIT brings Scheme `fib(20)` to within **~1.2× of native Python** —
i.e. close to parity, not the "~4×" this doc previously (and
incorrectly) reported. See the "Benchmark-harness correctness bug"
section below: every "measured" number in this document prior to that
fix was silently computed by a benchmarking macro that **ran the
timed expression twice** for any Phase-2/JIT-eligible closure whose
body also called a primitive outside a small ~83-entry allow-list
(`current-time` itself, needed to compute elapsed time, is one such
primitive) — inflating every one of those numbers by roughly 2×
without inflating the separately-measured "Python native" baseline.
`fib(20)`'s honest **single-execution** numbers, measured with the
fixed harness: 2.8 ms before Phase 7 → 1.2 ms after (**~2.3×**,
consistent with the larger-scale Phase 7 measurements below), against
1.0 ms native Python.

### Cross-version comparison

The table above compares phases within a single session. This one instead
runs the same benchmarks, unmodified, against the actual tagged releases —
`v1.4.8` (2018, pre-optimization), `v2.0.1` (Phase 1–3: trampoline +
direct-eval + JIT, but not yet Phase 4), and 2.0.2 (adds Phase 4
tail-call flattening plus Phase 5/6 closure and HOF JIT support). Each
version was checked out into its own `git worktree` and run via
`scripts/cross_version_bench.ss` (`python3 scheme.py
scripts/cross_version_bench.ss`).

That script, not a wall-clock `time python3 scheme.py ...` invocation, is
what makes the numbers comparable: every benchmark runs twice inside the
*same already-running interpreter*, timed with the `current-time`
primitive rather than the OS — a small **warmup** input first (discarded),
then the real **measured** input timed after warmup. This keeps two
things out of the reported number that would otherwise distort it:
Python/interpreter start-up (never measured at all, since timing starts
after the process is already up), and one-time JIT-compile cost (paid
during warmup, not during the measured run, so what's reported is
steady-state speed — the number a long-running program would actually
see).

| Benchmark | v1.4.8 | v2.0.1 | v2.0.2 |
|---|---|---|---|
| `fib(20)` — 21,891 calls | 3.6582s | 0.0056s | 0.0056s |
| tail loop, 3,000 iters | 0.5892s | 0.0022s | 0.0004s |
| tail loop, 6,000 iters | 1.1813s | **crash — `RecursionError`** | 0.0006s |
| mutual recursion (`even?`/`odd?`), depth 2,000 | 0.2620s | 0.0825s | 0.0752s |

**Caveat, added after the fact:** `scripts/cross_version_bench.ss`'s
`elapsed` macro used the same `(let ((_bench_start (current-time))) ?exp
...)` pattern later found to silently double-execute the measured
expression for any shape that reaches Phase 2/JIT (see "Benchmark-harness
correctness bug" below) — every column here except v1.4.8 (which predates
Phase 2/JIT, so was never vulnerable) most likely carries close to a 2×
inflation on the `fib(20)` and tail-loop rows. This wasn't caught or
corrected retroactively (would require re-running v2.0.1/v2.0.2 from
their own tags in a fresh `git worktree`, not done here) — treat the
*shape* of these findings (v2.0.1's crash, the mutual-recursion gap) as
solid, but not the precise multipliers.

Two things this surfaces that the phase-by-phase fib table doesn't:

- **v2.0.1 has a live correctness regression, not just missing speed.**
  Its Phase 1–3 JIT/direct-eval path uses real Python recursion for tail
  calls with no flattening, so any tail-recursive loop over ~5,000
  iterations crashes with `RecursionError` (confirmed directly above at
  6,000 iterations) — v1.4.8's plain trampoline never had this problem,
  since it never grows the Python call stack for a tail call. 2.0.2's
  Phase 4 (see below) restores that correctness guarantee while keeping
  the Phase 1–3 speed.
- **Mutual recursion barely improves across any version** (0.26s → 0.08s →
  0.08s). `even?`/`odd?` calling each other in tail position never gets
  JIT-compiled at all: when `even?` is first invoked, `odd?` hasn't been
  JIT'd yet, so `even?`'s own compile attempt can't resolve `odd?` to a
  compiled Python function and raises `_TrampolineFallback`.

  **Correction (found while auditing `_jit_lookup` for the JIT cleanup
  work — see "Phase 9" below):** this document previously claimed the
  failed attempt gets "permanently cached as 'don't JIT'... never
  retried." That's wrong. `_jit_lookup` deliberately reports a cached
  failed-compile verdict the same as "not yet attempted"
  (`return None if value is _CACHE_MISS or value is False else value`),
  so every call site (`_eval_direct`, `_jit_call`) re-invokes
  `_jit_compile_proc` on **every single call** to `even?`/`odd?`, forever
  — a wasted, repeated compile attempt each time, not a one-time miss.
  This is why the pattern is stuck near Phase-1 trampoline speed even on
  2.0.2 (the retry cost adds on top of the trampoline cost, not instead of
  it) — a known gap, not yet fixed. Collapsing this into a true
  one-time-failure cache was considered during the Phase 9 cleanup and
  deliberately deferred — see Phase 9's notes below for why.

### Best case

The table above deliberately caps its inputs at sizes every version can
run: v2.0.1 crashes on tail loops past ~5,000 iterations, and v1.4.8 is
slow enough that matching 2.0.2's larger inputs would take
minutes-to-hours. `scripts/best_case_bench.ss` (`python3 scheme.py
scripts/best_case_bench.ss`) instead scales the same three shapes up to
sizes that are only practical on 2.0.2, to show the ceiling the JIT and
Phase 4 tail-call flattening enable now that both speed and correctness
allow it. Same warmup/measured methodology as above; run on 2.0.2 only.

| Benchmark | 2.0.2 | 2.0.4 |
|---|---|---|
| `fib(37)` — ~78.2M calls | 20.0313s | 3.6685s |
| tail loop, 3,000,000 iters | 0.2867s | 0.1449s |
| mutual recursion, depth 50,000 | 1.8510s | 0.9111s |

**Caveat on the 2.0.2 column:** these numbers were captured with the same
`elapsed`-macro benchmark harness that turned out to have a measurement
bug (see "Benchmark-harness correctness bug" below) — for shapes that hit
Phase 2/JIT (all three rows here), the harness silently ran the timed
expression twice and reported the second (retried) run's time, inflating
every number by very close to 2×. 2.0.4's column above is measured with
the *fixed* harness (`scripts/best_case_bench.ss`, current version), so
this table is no longer apples-to-apples: roughly half of the 2.0.2 →
2.0.4 drop in the tail-loop and mutual-recursion rows (which Phase 7
doesn't touch at all) is just the harness fix, not a real 2.0.2 → 2.0.4
speedup. Re-running 2.0.2 with the fixed harness (via `git worktree`,
same method as the cross-version table above) would be needed for a
trustworthy 2.0.2-vs-2.0.4 comparison; not done here.

The `fib(37)` row *is* a real, additional Phase 7 speedup on top of that
harness-fix effect — see the honest before/after (both measured with the
fixed harness, same commit range) in the table below.

These and other shapes (closures allocated per call, nested closures, HOF
with a parameter in operator position, `map` over a JIT'd closure,
`set!`-based loops) are covered by the broader benchmark suite at
`scripts/benchmark.py` — run with `python3 scripts/benchmark.py`. Only the
naive-recursion (`fib`) shape moves under Phase 7; the rest are flat
before/after (both measured with the harness fix already applied), within
noise — confirming Phase 7 only affects self-recursive, non-tail calls:

| Benchmark (`scripts/benchmark.py`) | Before Phase 7 | After Phase 7 |
|---|---|---|
| 1. `fib(30)`, naive recursion | 0.3463s | 0.125s |
| 2. self tail-recursion, 3M iters | 0.1461s | 0.1477s |
| 3. mutual tail-recursion, 50K | 0.9263s | 0.9356s |
| 4. closures allocated per call, 20K | 0.8726s | 0.8382s |
| 5. nested closures, 5K | 0.6223s | 0.596s |
| 6. HOF, parameter in operator position, 5K | 0.1887s | 0.1822s |
| 7. `map` + closure over a 20K-elt list | 0.2404s | 0.2343s |
| 8. `set!`-based loop, 20K iters | 1.199s | 1.1964s |

(Rows 4–8 above are re-measured with the fixed harness but are otherwise
unaffected by Phase 7, included for completeness — item 8 in particular
was never doubled in the first place, since `set!` already forces that
closure onto the slow trampoline before Phase 2 is ever attempted.)

---

## Architecture background

The interpreter is a **register machine** compiled to Python from Scheme
source via a CPS (continuation-passing style) pipeline:

```
reader-cps.ss + environments-cps.ss + parser-cps.ss
  + interpreter-cps.ss + unifier-cps.ss
        ↓ compile-ds.ss
    source-ds.ss   (direct style)
        ↓ compile-rm.ss
    source-rm.ss   (register machine)
        ↓ translate_rm.py
    scheme.py      (Python)
```

Execution uses a **trampoline loop**:

```python
def trampoline():
    while pc:
        pc()          # each step sets pc to the next step
    return final_reg
```

Every Scheme operation is a step: it reads from ~170 global registers,
performs work, writes results back, and sets `pc` to the next step.
Continuations (closures over the current register state) are heap-allocated
and chained to represent the call stack.

---

## Phase 1 — Trampoline optimizations

### 1. Tuples for continuations and closures

**File:** `Scheme.py`

**Before:** `make_cont(*args)` returned `List(symbol_continuation, *args)` —
a linked cons-cell list, constructed by converting Python args to a cons
chain and back on every dispatch.

**After:** `make_cont(*args)` returns `(symbol_continuation,) + args` — a
plain Python tuple. Apply dispatch becomes:

```python
def apply_cont():   k_reg[1](*k_reg[2:])
def apply_proc():   proc_reg[1](*proc_reg[2:])
```

Continuations, closures, handlers, and fail stacks all use tuples.
Eliminates `Apply()` / `list_to_vector` overhead on every trampoline step.

**Speedup:** ~10× faster per dispatch step.

---

### 2. Dict-cached environment frames

**File:** `Scheme.py`

Scheme environments are chains of frames; each frame maps variable names
to mutable binding cells. Variable lookup previously did a linear walk of
the cons-list of variable names in each frame — O(N) per frame.

**Before (`search_frame`):** iterate over the cons-list of variable names,
comparing each to the target, then index into the bindings vector.

**After (`make_frame`):** single-pass construction that builds both the
bindings vector and a `{Symbol → binding_cell}` dict simultaneously.
`search_frame` does an O(1) dict lookup:

```python
def make_frame(variables, values, docstrings):
    bindings, cache = [], {}
    while isinstance(vars_cur, cons):
        b = cons(vals_cur.car, docs_cur.car)
        bindings.append(b)
        cache[vars_cur.car] = b
        vars_cur, vals_cur, docs_cur = vars_cur.cdr, vals_cur.cdr, docs_cur.cdr
    frame = List(Vector(bindings), variables)
    frame._search_cache = cache
    return frame
```

The dict stores references to the **same mutable binding cells**, so
`set!` semantics are fully preserved.

**Speedup:** ~50× faster variable lookup for frames with many bindings.

---

### 3. Inline constant `list-ref` in the code generator

**File:** `translate_rm.py`

`(list-ref x 2)` with a literal index is compiled to `x.cdr.cdr.car`
instead of calling the `list_ref()` Python function:

```python
elif expr[0] == "list-ref" and len(expr) == 3:
    pos_expr = expr[2]
    if isinstance(pos_expr, str) and pos_expr.isdigit():
        n = int(pos_expr)
        result = "(%s)" % self.process_app(expr[1])
        for _ in range(n):
            result = "(%s).cdr" % result
        return "%s.car" % result
```

**Speedup:** ~5.6× faster for constant-index list access in the hot interpreter
inner loop.

---

### 4. Simplified `true_q`

**File:** `translate_rm.py`

**Before:** `(False if ((%s) is False) else True)`

**After:** `(%s is not False)`

**Speedup:** ~30% faster for every boolean test.

---

## Phase 2 — Direct eval fast path

### Overview

The trampoline exists because Python lacks tail-call optimization.
Every Scheme function call, even `(< n 2)`, went through ~15–20 trampoline
steps, each involving multiple global register writes and a heap-allocated
continuation tuple.

The key insight: `call/cc` is **not used internally** by the interpreter
itself — it is only a user-facing feature. Therefore, user-defined functions
that contain no `set!` can be evaluated with a direct recursive Python
interpreter that uses Python's own call stack, completely bypassing the
trampoline for the duration of the call.

---

### Safety analysis (`_is_direct_eval_safe`)

At closure-creation time (`closure()`), the body AST is walked to check
for `assign_aexp` (the AST node for `set!`). Functions with no `set!` are
tagged `safe = True` as element `[5]` of the procedure tuple:

```python
def closure(formals, bodies, env):
    safe = _is_direct_eval_safe(bodies)
    return make_proc(b_proc_1_d, bodies, formals, env, safe)
```

Inner lambdas are not recursed into — they receive their own safety tag when
defined.

If `_TrampolineFallback` is raised at any point during direct evaluation
(e.g., if a call to `call/cc` or an unhandled proc is encountered), `apply_proc`
restores the original register state and falls through to the trampoline path,
which re-executes the function correctly. Because `set!` closures are excluded
from the fast path, there are no double-execution side effects.

---

### `_eval_direct` — recursive AST interpreter

Handles the most common AST node types. The `if-aexp` branch uses a
`while True` loop for tail-call optimization instead of recursion:

```python
def _eval_direct(exp, env):
    while True:
        tag = exp.car
        if tag is symbol_lit_aexp:
            return exp.cdr.car
        elif tag is symbol_lexical_address_aexp:
            d, off = exp.cdr.car, exp.cdr.cdr.car
            return binding_value(
                vector_ref(frame_bindings(list_ref(frames(env), d)), off))
        elif tag is symbol_var_aexp:
            b = search_env(env, exp.cdr.car)
            if b is False: raise _TrampolineFallback()
            return binding_value(b)
        elif tag is symbol_if_aexp:
            test = _eval_direct(exp.cdr.car, env)
            exp = exp.cdr.cdr.car if test is not False else exp.cdr.cdr.cdr.car
        elif tag is symbol_lambda_aexp:
            return closure(exp.cdr.car, exp.cdr.cdr.car, env)
        elif tag is symbol_begin_aexp:
            return _eval_sequence_direct(exp.cdr, env)
        elif tag is symbol_app_aexp:
            # ... (see apply logic below)
```

---

### `_fast_prim_map` — direct primitive dispatch (83 entries)

Common primitives are stored in the environment as Scheme procedure tuples
`(symbol_procedure, b_proc_XX_d)`. Going through the register-machine wrapper
would defeat the purpose of the fast path.

Instead, `_FAST_PRIM_SPECS` (a module-level dict, hoisted out of
`_build_fast_prim_map` during the Phase 9 cleanup so it can be inspected/
tested directly — see below) maps each primitive's Scheme name to a Python
callable taking a plain Python list of args; `_build_fast_prim_map` resolves
each name **lazily, on first use**, to the matching `b_proc_XX_d` function
found in the live toplevel environment:

```python
'+':  lambda args: plus(*args),
'<':  lambda args: LessThan(*args),
'car': lambda args: args[0].car,
'length': lambda args: length(args[0]),
'map': _fast_prim_direct_map,   # uses _apply_direct internally
# ... ~80 total entries covering arithmetic, predicates,
#     list/vector/string ops, char ops, HOF (map, for-each)
```

The `args` parameter is already a **Python list** from `_eval_direct`'s
arg accumulation loop — no cons-list construction needed.

---

### `_extend_direct` — environment extension without cons lists

For user closure calls, the new environment frame is built directly from
the Python args list, bypassing `List(*args)`, `make_empty_docstrings`,
and the `extend` + `make_frame` function call chain:

```python
def _extend_direct(env, formals, args_list):
    bindings, cache = [], {}
    vars_cur = formals
    for val in args_list:
        b = cons(val, "")
        bindings.append(b)
        cache[vars_cur.car] = b
        vars_cur = vars_cur.cdr
    frame = cons(Vector(bindings), cons(formals, symbol_emptylist))
    frame._search_cache = cache
    return cons(symbol_environment, cons(frame, env.cdr))
```

---

## Phase 3 — JIT compilation to Python

### Overview

Phase 2 still pays per-call overhead: environment extension, AST dispatch,
and Python function call frames for each recursive step. Phase 3 eliminates
this entirely for safe functions by compiling their Scheme AST to a **real
Python function** using `compile()` + `exec()`.

The compiled function runs at native Python speed with no Scheme interpreter
overhead.

---

### How it works

The Scheme compiler already annotates the AST with variable names inside
`lexical-address-aexp` nodes:

```
(lexical-address-aexp depth offset name info)
```

`depth=0` means a local parameter; `depth>0` means a free variable from
an outer frame. The name is stored at `exp.cdr.cdr.cdr.car`. This means
no name-reconstruction is needed — we read names directly from the AST.

`_jit_compile_proc(proc)` walks the AST and emits Python source strings:

| AST node | Generated Python |
|----------|-----------------|
| `lit-aexp(v)` | `repr(v)` |
| `lexical-address-aexp(0, _, name)` | `_j_name` (local param) |
| `lexical-address-aexp(d, off, name)` | `_j_name` (captured free var) |
| `if-aexp(test, then, else)` | `(then if (test) is not False else else)` |
| `app-aexp(+, [a, b])` | `(a + b)` (inlined) |
| `app-aexp(f, args)` | `f(args...)` |

For `fib`, the generated source is:

```python
def _jit_fn(_j_n):
    return (_j_n if ((_j_n < 2)) is not False else
            (_j_fib((_j_n - 1)) + _j_fib((_j_n - 2))))
```

---

### Inlined operators

Arithmetic and comparison operators are emitted as Python operators directly,
avoiding any function call:

```python
_NARY  = {'+': '+', '-': '-', '*': '*'}   # n-ary, left-assoc
_CMP   = {'<': '<', '>': '>', '<=': '<=', '>=': '>=', '=': '=='}
_UNARY = {
    'not': '({0} is False)',  'zero?': '({0} == 0)',
    'even?': '({0} % 2 == 0)', 'odd?': '({0} % 2 != 0)',
    'car': '({0}).car',  'cdr': '({0}).cdr',
    'null?': '({0} is _j__empty)',  'pair?': 'isinstance({0}, _j__cons)',
    'abs': 'abs({0})',
}
```

`_is_unshadowed_primitive` guards against inlining a name that's *already*
been redefined by the time a function using it is compiled (see
`test_primitive_redefinition.py`). Redefining a primitive *after* a
function that inlines it has already been compiled used to be a separate,
unguarded gap -- since fixed as a side effect of the JIT cache's
epoch-based invalidation (see "JIT cache" below and
`_jit_lookup`/`_is_unshadowed_primitive`'s docstrings in Scheme.py):
recompiling on a stale epoch re-runs this check against the fresh
environment too. See `test_jit_cache_invalidation.py`'s
`test_redefining_an_inlined_operator_after_first_compile_is_observed`.

---

### Free variable capture

Free variables (depth > 0) are looked up in the closure's captured
environment at JIT compile time and stored in the `exec()` namespace:

- **Self-recursive calls**: detected by identity (`val is self._proc`);
  compiled to a direct call by the generated function's own def name
  (`_jit_fn`) rather than a captured free variable — see Phase 7 below
  (superseding an earlier forward-reference-cell implementation).
- **Already-JIT-compiled siblings**: their compiled Python function is
  captured directly.
- **`_fast_prim_map` primitives**: wrapped as `lambda *a, _d=direct: _d(list(a))`
  to adapt the list interface to positional args.
- **Local parameters in operator position**: refused at compile time
  (`_TrampolineFallback`) — can't know at compile time whether the arg
  will be a Python callable or a Scheme proc tuple.

---

### JIT cache with GC-safe identity check

The cache maps `id(proc)` to `(proc, compiled_fn)`. On lookup, the stored
proc reference is checked for identity (`cached_proc is proc`) to detect
`id()` reuse after garbage collection:

```python
def _jit_lookup(proc):
    entry = _jit_cache.get(id(proc))
    if entry is None:
        return None
    cached_proc, result = entry
    if cached_proc is not proc:
        del _jit_cache[id(proc)]   # stale entry — id reused after GC
        return None
    return result if result is not False else None
```

---

### Fallback safety

If JIT compilation fails for any reason (`_TrampolineFallback` or any
exception), `_jit_cache[id(proc)] = (proc, False)` is stored and the
function silently falls back to Phase 2's `_eval_sequence_direct`. No
user-visible difference.

---

## Phase 4 — Tail-call flattening (fixes the crash, not just speed)

### The bug

Ordinary tail-recursive Scheme loops — the idiomatic way to iterate, e.g.
`(define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1))))` — used to
crash with `RecursionError` past ~4,900–5,000 iterations. Neither the Phase 2
direct-eval interpreter nor the Phase 3 JIT did anything special for a
function call in **tail position**: both compiled/evaluated it as an
ordinary call that recurses through the Python call stack, so a tail loop of
N iterations consumed N Python stack frames — a correctness bug, not just a
missed optimization, since this is exactly the pattern real Scheme code
relies on to loop at all.

### The fix

**Phase 3 (JIT, `Scheme.py`):** `_JitCompiler` gained a `tail_stmts()` method
used only for a compiled function's tail-position expression (through `if`
branches). It detects **self-recursive** tail calls by identity
(`_is_self_ref`, using the same `val is self._self` identity check used
elsewhere for self-recursion, see Phase 7) and compiles them to a
parameter reassignment + `continue` instead of a call:

```python
def _jit_fn(_j_n, _j_acc):
    while True:
        if ((_j_n == 0)) is not False:
            return _j_acc
        else:
            _j_n, _j_acc = (_j_n - 1), (_j_acc + 1)
            continue
```

The `while True:` wrapper is only emitted when a self-tail-call was actually
found (`jc._used_loop`); ordinary functions like `fib` are unaffected since
their recursive calls aren't in tail position.

**Phase 2 (`_eval_direct`, `Scheme.py`):** the `app_aexp` branch previously
did `return _eval_sequence_direct(op[2], new_env)` for a tail call — a real
Python call. It now reassigns `exp`/`env` to the callee's body/environment
and `continue`s the enclosing `while True:` loop instead, for **any**
tail-called user closure, not just self-recursive ones. `begin_aexp` was
changed the same way (its tail expression loops instead of recursing into
`_eval_sequence_direct`).

### What this does and doesn't cover

- Self-recursive tail loops (by far the common case — `loop`/named-let-style
  iteration) now run at JIT speed with **O(1) Python stack**, tested up to
  50,000,000 iterations in ~2.4s (within ~10% of a hand-written Python
  `while` loop).
- Mutual tail recursion (`is-even?`/`is-odd?` calling each other) is also
  covered in practice: compiling either function's JIT requires resolving
  the other, which isn't compiled yet, so JIT compilation for **both**
  fails and they permanently fall back to Phase 2 — which, with this fix,
  flattens the mutual tail-call chain in the interpreter instead of
  recursing. Verified to 1,000,000 iterations, both cold and pre-warmed.
- Not covered: a tail-call cycle between two functions that **each**
  independently JIT-compile successfully without referencing each other at
  compile time (a narrow, contrived case — genuine two-function cycles hit
  the circular-dependency case above and safely fall back). This residual
  gap would require a general cross-function trampoline, out of scope here.
- Verified with the full test suite (`pytest tests/test_all.py`, 246 tests)
  and by forcibly disabling JIT to exercise the Phase 2 fallback path in
  isolation — both pass.

---

## Phase 5 — JIT support for `lambda_aexp` (closures/HOF)

### The gap

`_JitCompiler.expr()` had no case for `lambda_aexp` — any function whose body
directly evaluates to a `lambda` (the common factory/curry pattern, e.g.
`(define (make-adder k) (lambda (x) (+ x k)))`) failed to JIT-compile
*at all* and paid Phase 2's interpreted cost on every call, forever.

### The fix

JIT'd Python functions represent a Scheme function's parameters as plain
Python locals (`_j_k`, ...); a real Scheme closure needs a proper
environment frame (a chain of `cons(Vector(bindings), formals)` frames with
a `_search_cache`) so the classic interpreter — or a future JIT of the
*inner* lambda — can look up captured variables by lexical address. So
`_JitCompiler` doesn't try to compile the inner lambda's body inline;
instead, at the point the `lambda_aexp` executes, it reconstructs the exact
frame the outer function's own parameters would occupy under normal
(non-JIT) evaluation and hands it to the ordinary `closure()` constructor:

```python
def _jit_make_closure(formals, bodies, parent_env, outer_formals, outer_values):
    frame_env = _extend_direct(parent_env, outer_formals, outer_values)
    return closure(formals, bodies, frame_env)
```

This reuses existing building blocks (`_extend_direct`, `closure()`)
unchanged, so the result is a completely ordinary Scheme proc tuple —
independently eligible for its own JIT compilation later, exactly like any
other closure. Guarded by a compile-time check that the outer function's own
formals are a plain (non-variadic) list matching its parameter count 1:1,
since the frame is rebuilt positionally.

### The bug this surfaced, and its fix

Making `lambda_aexp` produce a value exposed a second, more general problem:
an application whose *operator* is itself a computed expression —
`((make-adder n) 0)`, or an immediately-invoked lambda like
`((lambda (x) ...) e)` (how `let`/`or`/`and` commonly desugar) — could now
evaluate to a Scheme proc *tuple* at runtime instead of a plain Python
callable, and the existing "general call" codegen (`op_src(args...)`) tried
to call it directly: `TypeError: 'tuple' object is not callable`. This was
caught by the full test suite (`choose`/`require`/backtracking code in
`test_all.ss` hit it, intermittently — `choose` is randomized, so the repro
wasn't always reliable). Confirmed absent before this phase's change by
running the suite against the prior commit.

Fixed with a small runtime dispatcher, used only when the operator is itself
an `app_aexp` or `lambda_aexp` (a plain variable/lexical-address operator
reference was already guaranteed callable via the existing `_capture` logic,
so that path is untouched):

```python
def _jit_call(op, args):
    if isinstance(op, tuple) and op[0] is symbol_procedure:
        return _apply_direct(op, args, None)
    return op(*args)
```

### What this does and doesn't cover

- Functions that directly return a closure over their own parameters
  (`make-adder`-style) now JIT-compile. Measured on
  `(define (run-loop n acc) (if (= n 0) acc (run-loop (- n 1) (+ acc ((make-adder n) 0)))))`,
  4,500 iterations: 0.58s → 0.23s wall-clock (subtracting ~0.2s fixed
  startup: ~0.38s → ~0.03s of actual work, roughly **12×**).
- IIFE patterns (`let`, `or`, `and` desugaring) and calls through a computed
  operator now also work correctly via the same `_jit_call` dispatch,
  verified directly (`f`, `g`, `loop` in the test script below all
  successfully JIT-compiled; only the value differs from before, not the
  requirement that they be *called from within already-JIT-attempting
  code* to trigger compilation at all — same pre-existing trigger rule as
  everywhere else in the JIT).
- Not covered (unchanged from before, and not part of this fix): closures
  created via an *internal* `define`/`let` inside a larger function body —
  `_JitCompiler` still has no support for internal lexical bindings at all,
  so a function like the earlier `trial-hof` benchmark
  (`(define add (make-adder k))` as an internal define, then calling `add`)
  still falls back to Phase 2 entirely. That's a separate, larger piece of
  work (teaching the compiler about local bindings generally).
- A local *parameter* used in operator position (e.g.
  `(define (apply-twice f x) (f (f x)))`) bailed at compile time when this
  phase shipped — lifted in **Phase 6**, below.
- Verified with the full test suite (10+ repeated runs, since `choose`'s
  randomization made the bug intermittent) and targeted closure/IIFE/HOF
  scripts, checked directly against `_jit_cache` to confirm JIT compilation
  actually occurred rather than a silent, correct-by-luck Phase 2 fallback.

---

## Phase 6 — JIT support for a parameter used in operator position

### The gap

`_app()` refused to JIT-compile any function that called one of its own
*parameters* as if it were a function — e.g.
`(define (apply-twice f x) (f (f x)))` — because at compile time there was
no way to know whether `f`'s value would be a native Python callable or a
Scheme proc tuple. Phase 5's `_jit_call` dispatcher (built for
`lambda_aexp` support) already resolves exactly that ambiguity at runtime,
so this restriction could simply be folded into the same dispatch instead
of bailing: the depth-0 (local parameter) operator case now routes through
`_jit_call` alongside the nested-call and immediately-invoked-lambda cases
from Phase 5.

### The bug this surfaced

Fixing this exposed a second gap, in `_jit_call` itself (introduced in
Phase 5, latent until this change gave it a reason to matter): it checked
`_fast_prim_map` and otherwise called `_apply_direct`, which interprets the
callee via Phase 2 but **never attempts to JIT-compile it** — unlike
`_eval_direct`'s own app_aexp dispatch, which always gives an uncompiled
proc a shot at JIT first. The practical effect: any function reached
*only* through `_jit_call` (e.g. a function passed solely as a parameter,
never called by name anywhere else) would silently and permanently stay
at Phase 2 speed, and — worse — could cascade: a caller capturing that
never-compiled function as a free variable would itself fail to JIT too
(`_capture` only accepts an already-compiled or primitive proc). Caught by
benchmarking, not by the test suite: correctness was never at risk (this
was a missed-optimization bug, not a wrong-answer one), but the very first
before/after measurement showed almost no improvement, which didn't match
the fix's own logic — tracing why led straight to `_jit_call` never having
attempted compilation at all. Fixed by mirroring `_eval_direct`'s
attempt-then-fallback logic inside `_jit_call`.

### Measured impact

`(define (apply-twice f x) (f (f x)))` called by a non-tail-recursive
driver (`drive`, `fib`-shaped, so Phase 4's tail-loop optimization can't
mask the difference) for 4,800 calls: 0.39s → 0.19s wall-clock. Subtracting
~0.2s fixed startup, that's ~0.19s of real work → immeasurably small,
consistent with the other JIT-speed measurements in this document (roughly
the same "AST-walk vs compiled-Python" gap as everywhere else, not a new
one) — confirmed via `_jit_cache` that `apply-twice`, `add1`, and `drive`
all actually compiled, not merely ran fast by chance.

### What this does and doesn't cover

- A parameter called directly in operator position (custom comparators,
  predicates, callbacks invoked by hand rather than via `map`/`for-each`)
  is now JIT-eligible.
  **No longer current as of Phase 8, below — see its own "what this
  doesn't cover" section.** `_is_phase2_safe` (added in Phase 8, after
  this phase shipped) replaced `apply_proc`'s old, shallow gate (just
  `proc_reg[1] is b_proc_1_d and proc_reg[5]`, all this phase needed)
  with a static check that a closure's *entire reachable call graph* is
  provably safe, not just its own body. It can't resolve a "local
  parameter used as operator" call (or an IIFE, or any other computed-
  operator call — exactly the three shapes `_jit_call` was built for) to
  a concrete value at certification time, so it's classified unsafe —
  and that classification poisons every caller, transitively, all the
  way to the top level. Since JIT compilation is only ever attempted
  from inside an already-Phase-2-certified execution, `apply-twice` (and
  anything that calls it, even indirectly) no longer reaches Phase 2 at
  all today, so it never gets a chance to JIT-compile — confirmed
  directly in a later audit: `_is_phase2_safe(apply-twice)` is `False`,
  and re-running this section's own `drive` benchmark shows `apply-twice`
  never appears in `_jit_cache`. Deliberately left unfixed, not an
  oversight: see Phase 8's own note below on why a sound fix needs new
  analysis, not a quick patch.
- **Related, separate, still-open gap found along the way (not fixed
  here):** `_apply_direct` — used by `_fast_prim_map`'s `map`/`for-each`
  wrappers (`_fast_prim_direct_map`, `_fast_prim_direct_for_each`, etc.),
  not just `_jit_call` — has the exact same "never attempts JIT
  compilation" limitation. A
  function invoked *only* through `map`/`for-each` (e.g. a `lambda` passed
  inline) stays at Phase 2 speed indefinitely, regardless of this fix.
  Confirmed directly: timing `(for-each (lambda (n) (apply-twice add1 n)) (range N))`
  showed no improvement, because the outer lambda is always dispatched via
  `_apply_direct`. The same fix applied to `_jit_call` here (attempt
  `_jit_compile_proc` before falling back) would apply directly to
  `_apply_direct` too — a small, separate, mechanically similar follow-on.
  **Also no longer current as of Phase 8, for the identical reason as the
  bullet above — confirmed directly in a later audit:** `map`/`for-each`
  are excluded from `_phase2_safe_walk_call`'s certification (see Phase
  8's own "what this doesn't cover" section, `map`/`for-each` bullet), so
  any function that calls them directly is uncertified, and that poisons
  every (transitive) caller the same way `apply-twice` does. Traced
  `_fast_prim_direct_map`/`_fast_prim_direct_for_each` directly: neither
  is ever invoked by a normal nested-call program today (`_is_phase2_safe`
  is `False` for the calling function, so Phase 2 is never entered, so
  these fast-prim wrappers are never reached — the classic trampoline's
  own native `map`/`for-each` handle the call instead). So this "small,
  separate, mechanically similar follow-on" would have zero measurable
  effect on its own now — the code path it would improve isn't exercised
  by anything, the same as `_jit_call`'s fix for `apply-twice`. It only
  becomes worth doing together with whatever eventually restores
  reachability (the call-site certification idea in Phase 8's notes),
  not in isolation.
- **A separate `_apply_direct` issue, found and closed after Phase 8
  shipped: its *safety* gate, not the JIT-compilation-attempt gap above.**
  `apply_proc` only starts a live Phase-2 attempt after the full
  `_is_phase2_safe(proc)` certification (Phase 8); `_apply_direct` used to
  gate the same `_eval_sequence_direct` entry point with only `proc[5]`
  (`_is_direct_eval_safe`) — a shallow, single-closure "no `set!` in this
  body" check with no visibility into what the closure calls. A closure
  with `proc[5]` true but `_is_phase2_safe` false (e.g. it calls a
  *different* closure that itself uses `set!`) could get past that
  weaker gate and start executing, only to hit `_TrampolineFallback`
  partway through. Confirmed this was already unreachable via any real
  program, for the identical transitive-poisoning reason as the
  `map`/`for-each` unreachability just above — but it was a real gap in
  `_apply_direct`'s own contract regardless, one a future JIT change
  could have silently made reachable. Fixed by requiring
  `_is_phase2_safe(proc)` in `_apply_direct` too, matching `apply_proc`
  exactly. No behavior change for any currently-reachable path (the
  path was dead code before and after); see
  `tests/test_apply_direct_proc5_gap.py`.
- Verified with the full test suite (8+ repeated runs) and targeted
  scripts combining this with Phase 5's closure support, checked against
  `_jit_cache` to confirm real compilation rather than a fast-by-luck
  fallback.

---

## Phase 7 — Drop the indirection cell for self-recursive (non-tail) calls

### The gap

Phase 4 already special-cases self-recursive **tail** calls (compiled to a
parameter reassignment + `continue`, see above). But a self-recursive call
in **non-tail** position — the common case for tree recursion like `fib`,
where `(+ (fib (- n 1)) (fib (- n 2)))` can't be a tail call because both
results feed into `+` — still went through `_capture`'s generic
free-variable path, which treated "this call refers to the function's own
proc tuple" as just another captured value:

```python
if val is self._self:
    sref = self._sref            # sref[0] patched to the real fn after exec()
    self._free[m] = lambda *a, _r=sref: _r[0](*a)
```

Every recursive `fib` call paid for an extra Python function call (the
wrapper lambda) and a tuple indirection (`_r[0]`) on top of the real
JIT'd call — profiling `fib(30)` showed this wrapper alone accounted for
roughly a third of total runtime.

### The fix

The generated function is always emitted under the literal name `_jit_fn`
(see `_jit_compile_proc`: `'def _jit_fn(' + ps + '):'`). A bare reference
to that name inside the function's own body resolves at call time via a
normal Python global lookup into the `exec()` namespace — which already
holds the function under that name by the time any call actually happens,
since `def` binds it immediately when the `exec()` runs, before the
function is ever invoked. So a self-reference can just emit the token
`_jit_fn` directly, with no forward-reference cell and no wrapper:

```python
if val is self._self:
    return '_jit_fn'   # generated code calls itself by its own def name
```

checked in both places a variable can resolve to the enclosing proc
(`expr()`'s `lexical_address_aexp` case, and `_var()`). This made the
forward-reference cell (`self_ref = [None]` / `self._sref` /
`self_ref[0] = fn`) entirely dead, so it was removed along with it.

For `fib`, the generated source is unchanged in shape, just without the
indirection:

```python
def _jit_fn(_j_n):
    return (_j_n if ((_j_n < 2)) is not False else
            (_jit_fn((_j_n - 1)) + _jit_fn((_j_n - 2))))
```

### Measured impact

Both numbers below hold the same shape (self-recursive, non-tail) across
three different scales; tail-recursive, mutual-recursive, closure, HOF,
`map`, and `set!` benchmarks are flat before/after (see tables above) —
consistent with the fix only touching the self-recursive non-tail path.

All numbers below are measured with the harness fix described in
"Benchmark-harness correctness bug" (below) already applied to both the
"before" and "after" run, so — unlike an earlier version of this
document — these are honest, single-execution numbers, not doubled ones:

| Benchmark | Before | After | Speedup |
|---|---|---|---|
| `fib(20)` — 21,891 calls | 2.8ms | 1.2ms | **~2.3×** |
| `fib(30)` (`scripts/benchmark.py` #1) | 0.3463s | 0.125s | **~2.8×** |
| `fib(37)` — ~78.2M calls (`scripts/best_case_bench.py`) | 9.9s | 3.6352s | **~2.7×** |

Also reflected in `time python scheme.py fib.ss` for `(fib 35)`: **3.95s →
~1.55s** wall-clock (this one *is* a real single execution — bare
top-level calls aren't subject to the harness bug, see below), bringing
it to within ~1.1–1.2× of a hand-written recursive `fib.py` (~1.29s on
the same machine) instead of ~3× slower than it.

### What this does and doesn't cover

- Any self-recursive, non-tail call now compiles to a direct call by
  name — no change in *which* functions are JIT-eligible, only how a
  self-call within an already-JIT-eligible function is compiled.
- Semantics are unchanged from before this fix: a self-recursive JIT'd
  call was already bound to the *specific compiled closure instance* at
  compile time (via the now-removed `sref` cell), not re-resolved
  dynamically against the current value of the function's name in the
  environment — e.g. a `set!` that reassigns the function's name after
  the closure was compiled was never observed by its own recursive calls,
  before or after this change. This matches the existing design intent
  documented in Phase 3/4, just implemented without the extra indirection.
- Verified with the full test suite (`pytest tests/test_all.py`, 271
  tests) and `scripts/benchmark.py` / `scripts/best_case_bench.py`
  end-to-end, both before (via `git stash`) and after the change, on the
  same machine, for a clean paired comparison.

---

## Benchmark-harness correctness bug (found while validating Phase 7)

### The symptom

Re-measuring `fib(20)` to sanity-check Phase 7's numbers gave a wall-clock
`time python scheme.py fib.ss` result (`(fib 35)`: ~1.55s total, close to
hand-written `fib.py`'s ~1.29s) that flatly contradicted this document's
own `elapsed`-macro-based numbers, which put the same JIT'd `fib` at
~4× native Python. Both couldn't be right for the same code.

### Root cause

`scripts/benchmark.ss` (and `best_case_bench.ss`, `cross_version_bench.ss`)
all time a benchmark with this pattern:

```scheme
(define-syntax elapsed
  [(elapsed ?exp)
   (let ((_bench_start (current-time)))
     ?exp
     (round4 (- (current-time) _bench_start)))])
```

`(let ...)` desugars to a closure call (an immediately-invoked lambda).
Phase 2's `apply_proc` wraps every closure call in a try/except: attempt
`_eval_direct`/JIT first, and if it raises `_TrampolineFallback`
*anywhere in the closure's body*, discard that attempt entirely and
re-run the **whole closure call** from scratch via the slow trampoline
(see Phase 2's "Safety analysis" above — this is the documented,
intentional fallback mechanism). `current-time` is not one of the ~83
primitives in `_fast_prim_map`, so the *second* statement in this `let`'s
body — computing elapsed time, which calls `current-time` again — always
raises `_TrampolineFallback`. By that point `?exp` (the thing actually
being measured) has already run to completion once, successfully, via
Phase 2/JIT. The fallback then re-runs the entire `let` body, including
`?exp`, a second time via the trampoline — and only the *second* run's
duration ends up in the reported time, since the timer (`_bench_start`)
is itself inside the same retried body and gets reset.

Net effect: every "measured" number in this document that exercised
Phase 2/JIT was the time to run the benchmark **twice**, silently, while
the separately-measured "Python native" baseline was never doubled —
making the JIT look roughly 2× slower relative to native Python than it
actually is.

Confirmed directly, not just inferred, with an instrumented call counter
(a closure that increments a vector slot on every call — `vector-set!`
isn't `set!`, so it doesn't disable Phase 2 the way `(set! ...)` would):

```scheme
(let ((start (current-time)))
  (fib 10)                    ; 177 calls for fib(10), naive recursion
  (printf "done~%"))          ; not in _fast_prim_map -> triggers fallback
;; counter reads 354 afterward, not 177 -- fib(10) ran twice.
```

Bare top-level statements and a top-level `(begin ...)` are **not**
vulnerable — confirmed the same way, counter reads 177 in both cases.
Neither is a closure call, so neither goes through `apply_proc`'s
catch-and-retry at all; only an actual closure/`let`/lambda invocation is.

### Why this is more than a benchmarking nuisance

The trigger has nothing to do with timing code specifically. **Any**
Phase-2/JIT-eligible closure (any closure not containing `set!` — see
`_is_direct_eval_safe`) whose body does real work and *then* calls
**any** primitive outside the small `_fast_prim_map` allow-list — which
includes `display`, `printf`, `current-time`, `float`, string/vector
constructors not in the list, host interop calls, and more — will have
its **entire body silently re-executed** the moment that later call is
reached. For a pure function this just wastes time (as here). For a
function with real side effects — writing output, mutating a vector or
box, a `python-exec` call, appending to a file — those side effects
happen **twice**, silently, with no error and no test-suite signal
(`pytest tests/test_all.py` passes throughout at the time this was
written, since none of its assertions happened to be sensitive to this
shape — two regression tests were added in Phase 8, below).
This is a genuine interpreter correctness gap, not just a measurement
artifact — at the time this section was written it was tracked as an
open row in "Potential further improvements" rather than fixed
immediately, since a proper fix needs the same level of scrutiny the
abandoned `set!`/JIT work below got, to avoid trading a performance bug
for a silent-wrong-answers one. It was fixed shortly after, once that
scrutiny was done — see Phase 8, immediately below.

### The fix applied here

Scoped narrowly to the benchmark scripts, to make this document's numbers
trustworthy again — not a fix to the interpreter itself. `elapsed` in
`scripts/benchmark.ss` and `scripts/best_case_bench.ss` was rewritten to
sequence with `begin` and a top-level `set!` instead of a `let`:

```scheme
(define _bench_start 0)
(define-syntax elapsed
  [(elapsed ?exp)
   (begin
     (set! _bench_start (current-time))
     ?exp
     (round4 (- (current-time) _bench_start)))])
```

`begin` is plain sequencing, not a closure call, so it's never wrapped in
`apply_proc`'s catch-and-retry — confirmed with the same instrumented
counter (reads 177, not 354, with this version). `scripts/cross_version_bench.ss`
was intentionally left as-is: it's checked out fresh from each historical
tag via `git worktree` for the "Cross-version comparison" table above, so
editing today's copy wouldn't change what actually ran to produce those
numbers; see the caveat added to that table's section instead.

Every number in this document that could be affected (the top summary
table, "Best case", the `scripts/benchmark.py` table, and Phase 7's
"Measured impact") was re-measured with the fixed harness and corrected
in place, rather than left as a mix of doubled and honest figures.

---

## Phase 8 — Fix the underlying double-execution bug, not just its benchmark symptom

### The bug, restated

The previous section fixed the *symptom* (the benchmark harness) but
left the actual interpreter behavior unchanged: `apply_proc` attempts
Phase 2 (`_eval_sequence_direct`) for the whole of any `set!`-free
closure body, and if `_TrampolineFallback` is raised *anywhere* in that
body — a call to a primitive outside `_fast_prim_map`, or to a closure
that itself contains `set!` — the old code discarded the attempt and
re-ran the **entire body** via the slow trampoline, including whatever
side effects already ran before the failure. This isn't limited to a
single closure's own statements: `_eval_direct` inlines calls to other
safe-but-not-yet-JIT'd closures into the same Python call chain (the
"reuse this Python frame" mechanism from Phase 4), so the one shared
catch point at the outermost `apply_proc` can sit *several logical
closures* away from where the failure actually happens. Confirmed with
an instrumented vector-counter closure `O` that does its own work, then
calls a nested closure `H` which does its own work and then fails:
before this fix, `O`'s counter read 2 and `H`'s read 3 for what should
have been exactly one call each — not just doubling, but compounding,
since a retry can itself retry.

This is a real correctness bug, not a performance one: any Phase-2-
eligible closure that does real work (I/O, mutation, anything with an
observable side effect) and then calls something outside the small
`_fast_prim_map` allow-list, or a `set!`-using helper, could silently
re-run that work.

### The fix

`_is_phase2_safe(proc)` (`Scheme.py`, near `_is_direct_eval_safe`): a
static, side-effect-free walk that mirrors `_eval_direct`'s own dispatch
— same AST node cases, same call-inlining behavior for nested closures —
but only *classifies*, the same way a failed JIT compile attempt
(`_jit_compile_proc`) never executes anything either. It transitively
certifies that every call reachable from a closure's body (without
crossing an inner-lambda boundary) resolves, statically, to either a
known-pure `_fast_prim_map` primitive or another closure that is itself
already certified safe. Self- and mutually-recursive references are
handled by optimistically assuming a proc already being checked is safe
(sound: the overall walk still ANDs in every other reachable expression,
so real unsafety anywhere in a cycle is still found); only the outermost
call's verdict is cached, by identity, mirroring `_jit_cache`'s pattern
including its GC-id-reuse guard.

`apply_proc` now requires `_is_phase2_safe(proc_reg)` before attempting
Phase 2 at all — not just the pre-existing `set!`-free check:

```python
if (isinstance(proc_reg, tuple) and len(proc_reg) == 6
        and proc_reg[1] is b_proc_1_d and proc_reg[5]
        and _is_phase2_safe(proc_reg)):
    ...
```

Deliberately conservative: anything the walk can't prove safe — a call
through a parameter or other computed/dynamic operator (e.g.
`((make-adder n) 0)`, or a parameter used in operator position), a call
to a `set!`-using closure, an unresolved/forward-referenced name,
`map`/`for-each`'s callback argument (its safety depends on a runtime
value the walk doesn't try to verify) — is marked unsafe, so that
closure's Phase-2 attempt is never even started; it runs via the
always-correct slow trampoline instead. Being too conservative only
costs speed; the risk this closes off (silently re-executing side
effects) only existed on the "too permissive" side, so every tie is
broken toward unsafe, matching the same asymmetric-risk principle
documented for the abandoned `set!`/JIT idea below.

The old silent "catch `_TrampolineFallback`, discard, retry everything"
behavior around the body evaluation itself is gone. The arity check
(which runs *before* any body statement executes, so nothing has
happened yet) is still safely retriable. But `_eval_sequence_direct`'s
own call is no longer wrapped in a catch: if a closure `_is_phase2_safe`
already certified somehow still raises `_TrampolineFallback`, that means
the checker has a soundness gap, and the exception is now left to
propagate — a loud, reportable crash instead of a silent wrong answer.

### Verified

- Two new regression tests in `calysto_scheme/modules/test_all.ss`
  (`jit-edge-cases` group, cases 62–63), using a vector-based call
  counter (not `set!`, so the counting closure itself stays Phase-2/JIT-
  eligible) to make double-execution directly observable as a wrong
  number rather than something that has to be inferred from timing.
  Confirmed both **fail** against the pre-fix interpreter (177→354, and
  the compounding (1,1)→(2,3) case) before merging the fix, then pass
  after — run automatically by `pytest tests/` in CI.
- Full suite: 282/282 passing (271 pre-existing + the 2 new above +
  Phase 7's own additions along the way).
- The original, unfixed `scripts/benchmark.ss`-style `elapsed` macro
  (the `let`-based one from the previous section, not the `begin`-based
  fix) now also produces the correct, non-doubled count when re-tested
  directly — `apply_proc` simply declines to attempt Phase 2 for that
  closure at all now (it calls `current-time`, not in
  `_fast_prim_map`), so it's slow but correct, independent of the
  benchmark-script-level fix.
- Broader stress tests, all correct: `map` with a side-effecting
  callback that itself hits an unmapped primitive; `sort` with a custom
  comparator; a closure doing its own work and then calling a `set!`-
  using helper.

### Measured cost

Isolated, paired before/after (same machine, clean process per run, not
sequential-benchmark-suite numbers, which proved noisy enough in Phase 7
to be misleading — see that section):

| Shape | Before | After | Ratio |
|---|---|---|---|
| `fib(35)` wall-clock (naive/tree recursion) | 1.55s | 1.57s | unchanged |
| tail loops, mutual recursion, `map`, `set!` loops | — | — | unchanged |
| Phase 5: closures allocated per call, `((make-adder n) 0)`, 20K iters | 1.09s | 2.02s | **~1.85× slower** |
| Phase 5: nested closures, depth-2 capture, 5K iters | 0.67s | 0.74s | ~1.1× slower |
| Phase 6: HOF, parameter used as operator, 5K iters | 0.43s | 0.77s | **~1.8× slower** |

The regression is real and specific: it hits exactly the shapes that
route through a statically-unresolvable operator (Phase 5's computed-
closure-factory pattern, Phase 6's HOF pattern) — those closures no
longer get *any* Phase-2/JIT treatment at their own `apply_proc` entry
point, falling back to the slow trampoline for their own iteration
mechanism. That trampoline is still Phase 1-optimized (tuples for
continuations, dict-cached environment frames), not 2018-era slow, which
is why the regression is ~1.1–1.85×, not the 90×+ gap those phases
originally closed. Naive/tree recursion, tail loops, and mutual
recursion — the shapes most code actually hits — are unaffected.

### What this does and doesn't cover

- Closes the double-execution bug for every shape this document's
  benchmark suite and stress tests exercise, using a conservative,
  easy-to-reason-about static check rather than a more invasive runtime
  mechanism.
- Does not attempt to recover Phase 5/6's lost speed for
  dynamic-dispatch shapes — specifically, Phase 6's "parameter called in
  operator position" support (`apply-twice`-shaped functions) becomes
  unreachable via normal top-level execution as a side effect of this
  phase, confirmed directly in a later audit (`_is_phase2_safe
  (apply-twice)` is `False`; see Phase 6's own "what this does and
  doesn't cover" section above, updated to point here). Phase 5's other
  half — a function directly *returning* a closure over its own
  parameters — is unaffected and remains reachable, since creating a
  closure has no call to resolve and so never trips this same
  certification gap. A sound way to recover the lost half would be
  e.g. certifying a dynamically-resolved callee *at the call site, at
  the moment its actual value becomes available*, and running just that
  one call via a synchronous, non-retrying path, rather than gating the
  whole closure upfront — was considered and looks promising, but
  requires reasoning carefully about `call/cc`/`amb`/`choose` interaction
  (the same class of subtlety that made the `set!`/JIT idea below not
  worth shipping half-verified) and is deliberately left as future work
  rather than bundled into a correctness fix.
- `map`/`for-each` are conservatively treated as always phase2-unsafe
  (not attempting to verify their callback argument specifically), even
  though many real calls (e.g. a directly-written `lambda` literal
  argument) could in principle be proven safe. Same trade-off: simpler
  and easier to verify now, at a measurable but bounded cost (item 7 in
  the `scripts/benchmark.py` table is unaffected in practice, since
  `map`'s own trampoline-driven iteration is already reasonably fast —
  see Phase 1).

---

## Phase 9 — Reduce duplication across the JIT/Phase-2 machinery (no behavior change)

### Motivation

Not a performance phase — a cleanup one. By Phase 8, `Scheme.py` had grown
**three** independently hand-maintained recursive walks over the same
annotated-AST node tags (`lit`, `lexical-address`, `var`, `if`, `lambda`,
`begin`, `app`): `_eval_direct` (executes), `_JitCompiler` (compiles to
Python source), and `_is_phase2_safe`/`_phase2_safe_walk` (statically
classifies safety). This is exactly the shape of duplication that caused
real bugs earlier in this document — Phase 6 found `_jit_call` missing a
JIT-attempt step that `_eval_direct` already had; Phase 8 itself exists
because `_is_phase2_safe` didn't exist yet and nothing checked whether
Phase 2's optimistic fallback matched real `_eval_direct` behavior. A
dedicated audit (four ranked, independently-verified changes, each
regenerated and re-tested before the next) addressed the most
mechanically-safe duplication without touching the three walkers'
actual per-tag semantics, which stayed deliberately un-merged — see "What
this does and doesn't cover" below for why.

### 1. Unified the two identity caches

`_jit_cache`/`_jit_lookup` and `_phase2_safe_cache`/`_phase2_safe_lookup`
were two hand-copied `dict[id(x)] -> (x, verdict)` implementations, each
with its own copy of the GC-id-reuse guard (Python can reuse a
garbage-collected object's `id()`, so a lookup must check the cached key
`is` the object asked about, not just that `id()` matches). Both now wrap
one shared `_IdentityCache` class. The two caches' *behavior* was
deliberately **not** unified — see the mutual-recursion correction above:
`_jit_lookup` still reports a cached failed-compile verdict as "not yet
attempted" (so JIT compilation is retried every call), while
`_phase2_safe_lookup` still reports a cached `False` as final. Collapsing
that difference is a real behavior/performance change, not a dedup, and
was explicitly left alone.

### 2. Unified operator-resolution logic

Four places independently re-derived "does this operator expression
statically resolve to a value, given `depth`/`offset` or a `var-aexp`
name" — `_JitCompiler.expr()`'s `lexical-address-aexp` case, `_var()`,
`_is_self_ref()`, and `_phase2_safe_walk_call()`. Extracted into
`_resolve_lexical_address(depth, offset, env)`, `_resolve_var(sym, env)`,
and a combining `_resolve_operator(op_exp, env)`, each returning a tagged
`('local', None)` / `('unresolved', None)` / `('value', v)` result and
never raising — callers keep applying their own existing policy on top
(e.g. `expr()` raises `_TrampolineFallback` on `'unresolved'`;
`_is_self_ref()` just returns `False`). `_is_self_ref()` shrank from ~20
lines to `return kind == 'value' and val is self._self`.
`_phase2_safe_walk_call`'s `lit-aexp`-as-operator special case (which no
JIT-side caller has ever needed) was kept local rather than folded into
the shared helper, to avoid silently expanding what the JIT accepts.

Added `tests/test_jit_self_recursion.py`: two pinned regression tests for
the single most safety-critical invariant this logic guards (Phase 7's
identity-based self-recursion binding) — including one that redefines a
JIT'd function's name after compilation and confirms the *original*
compiled closure's own recursive calls are unaffected.

### 3. Primitive tables: hoisted, not merged

`_fast_prim_map`'s source dict (renamed `_FAST_PRIM_SPECS`) was local to
`_build_fast_prim_map`; hoisted to module level (along with its four
helper closures, renamed `_fast_prim_direct_map` /
`_fast_prim_direct_for_each` / `_fast_prim_set_car` / `_fast_prim_set_cdr`)
so it can be inspected and tested directly — no behavior change.
`_JitCompiler._NARY`/`_CMP`/`_UNARY` (the ~9-entry compile-time inlining
templates) were deliberately **not** merged into it: they serve a
different role (syntactic Python-operator inlining vs. generic runtime
dispatch), and forcing one data structure to serve both would conflate
"any callable" with "safely inlineable as a raw Python operator," a
narrower property. Added `tests/test_jit_prim_tables.py` instead, which
asserts every name `_NARY`/`_CMP`/`_UNARY` inlines is still a
`_fast_prim_map` entry — turning "must be kept in sync by hand" into an
automated check.

### 4. Expanded the differential-test harness — done first, as a safety net

`tests/test_phase2_safety.py` already ran `test_all.ss` twice (trampoline-
only vs. Phase 2+JIT) via `tests/_phase2_diff_runner.py`, diffing every
assertion's pass/fail result. Added a third mode, `"phase2only"`
(`_jit_compile_proc` monkeypatched to a no-op, so every Phase-2-eligible
closure runs through `_eval_direct` alone, JIT never engaging), and diff
all three modes pairwise. This isolates `_eval_direct`'s own correctness
from `_JitCompiler`'s specifically — the exact class of cross-walker
divergence this file's bug history (Phases 6–8) keeps finding — and was
deliberately done *before* the other three changes, so it could catch any
regression they introduced.

### What this does and doesn't cover

- Explicitly **not attempted**: merging `_eval_direct`, `_JitCompiler`,
  and `_phase2_safe_walk` into one shared AST-visitor abstraction. They
  differ in return type (value / Python-source-string / bool), side-effect
  profile (real execution / none / none), and control-flow shape (Phase
  4's tail-position `while True` loop exists in `_eval_direct` and,
  separately, in `_JitCompiler.tail_stmts()`, with no analog in
  `_phase2_safe_walk` at all). A generic visitor would have to thread
  three incompatible shapes through one piece of plumbing — likely as
  risky as the duplication it would remove, in code with a specific
  documented history of exactly this class of bug. The flat `if`/`elif`
  chain in each walker is itself a debugging asset today: auditing "did a
  new AST case get added to all three" is a `grep` and an eyeball diff;
  a handler-dispatch abstraction would make that harder, not easier.
- Also not attempted: narrowing `_jit_compile_proc`'s broad
  `except Exception` (it's exactly what guarantees any compile failure
  falls back silently and safely — no correctness upside to narrowing
  it), and reconciling `_phase2_safe_walk`'s deliberate
  `mu-lambda-aexp` divergence from `_is_direct_eval_safe` (already an
  intentional asymmetry guarding against a case `_eval_direct` can't even
  handle — see Phase 8's discussion above).
- A separate idea investigated in parallel — moving the JIT compiler's
  *implementation* into hand-written Scheme (in `interpreter-cps.ss`),
  flowing through the existing `cps → ds → rm → translate_rm.py` pipeline
  like the rest of the interpreter — was spiked and confirmed technically
  *possible* (the pipeline can carry new hand-written CPS Scheme through
  correctly), but produces register-machine/trampolined Python at the
  output, not readable code, which undermines the elegance goal. Not
  pursued.

### Verified

- Full suite: 292 → 297 passing (5 new tests: 3 in the expanded
  `test_phase2_safety.py`, 2 in `test_jit_self_recursion.py`, 1 in
  `test_jit_prim_tables.py`), regenerating `../scheme.py` from `Scheme.py`
  and re-running after each of the four changes individually, not just at
  the end.
- `scripts/benchmark.py` and `scripts/best_case_bench.py`: every row flat
  across all four changes, within normal run-to-run noise (~2–5%, no
  systematic direction) — consistent with a behavior-preserving refactor.

### Addendum — AST field-accessor functions: tried, measured, reverted

A natural next abstraction was considered: `_eval_direct`, `_JitCompiler`,
and `_phase2_safe_walk*` all pull fields out of aexp cons-nodes with raw
chains like `exp.cdr.cdr.cdr.car`, each re-deriving (and re-commenting)
the same layout inline. Wrapping each tag's field extraction in a small
named accessor (`_if_parts(exp) -> (test, then, else)`, etc.) and an
`_iter_aexp_list` generator to replace the repeated arg-list
`while isinstance(cur, cons): ...` loops looked like a clean win —
implemented and initially verified (297/297 tests passing) — but
benchmarking caught a real, reproducible regression before it shipped:

| Benchmark | Before | After accessors | Why |
|---|---|---|---|
| `fib(30)`, tail loops (compile once, run native) | flat | flat | one-time cost, amortized over unlimited later calls |
| Mutual recursion (`even?`/`odd?`, never JIT-compiles) | 0.924s | 1.10s (**+19%**) | `_jit_compile_proc` retries the *entire* failed compile attempt on every call (see the mutual-recursion correction above) — every accessor call's overhead is paid every single call, with no amortization at all |
| Fresh closure allocated every iteration | 1.711s | 1.81s (**+6%**) | a *new* closure every call defeats both `_jit_cache` and `_phase2_safe_cache` — same "no amortization" problem, smaller because only one (successful) compile attempt is paid per call, not a failed one plus a full `_eval_direct` re-execution |

Root cause, confirmed with an isolated microbenchmark (not just inferred):
a plain attribute chain (`exp.cdr.cdr.car`) took ~101 ns; the same
extraction via a one-line accessor function returning a tuple took ~139 ns
(**+38%**, from Python's function-call and tuple-pack/unpack overhead);
the arg-list generator took ~355 ns against a raw `while`-loop's ~227 ns
(**+56%**, from the `yield`/`next()` suspend-resume protocol). Individually
tiny, but multiplied across every AST node visited in a hot, uncached
retry loop, these add up to exactly the regression measured above. A
non-generator, list-returning variant of the arg-list helper was also
measured (~9% over raw, versus the generator's ~56%) as a cheaper
middle ground — not used, since *any* non-zero per-call cost was ruled
out for this code, not just minimized.

**Resolution:** reverted every accessor/generator call in `_eval_direct`,
`_JitCompiler`, and `_phase2_safe_walk*` back to raw inline field access
— confirmed by benchmark to fully recover all three regressed rows to
baseline. The accessor *functions* themselves were removed (they had zero
remaining callers), replaced with a single comment block (directly above
`_is_direct_eval_safe` in `Scheme.py`) documenting each aexp tag's
`(tag field1 field2 ... info)` cons-cell layout once, in prose, with zero
runtime cost — the readability goal without the speed cost. Full test
suite still 297/297 after reverting; every benchmark row back within
normal noise of the Phase 8 baseline.

The general lesson, consistent with this file's existing design
principle: in `_eval_direct` and `_JitCompiler` specifically, *any*
closure that either never successfully JIT-compiles or is freshly
allocated on every call gets zero benefit from either cache, so these two
code paths should be treated as always-hot and always-uncached when
weighing a readability abstraction against its runtime cost — unlike
`_phase2_safe_cache`-gated or `_jit_cache`-gated code reached only through
a closure that's called many times *after* its first (cached) attempt,
where the same kind of wrapper is normally free.

---

## What real Scheme implementations do

For reference, production Scheme systems use similar but deeper techniques:

- **Chez Scheme / Racket**: compile directly to native machine code; the register machine *is* the CPU; continuations are stack-allocated when escape analysis proves they don't escape.
- **CHICKEN Scheme**: CPS-compiles to C (same architecture as Calysto Scheme), but uses the C call stack directly ("Cheney on the M.T.A.") — GC is triggered when the C stack fills, copying live frames to the heap and continuing.
- **Guile**: compiles to a register-based bytecode VM with JIT.

The Phase 2 direct-eval fast path is the Python analogue of what CHICKEN does:
use the host language's call stack for ordinary function calls, and fall back
to the explicit continuation machinery when needed.

The Phase 3 JIT is analogous to what Guile and modern JavaScript engines do:
compile hot functions to a lower-level representation (native code / Python
bytecode) that runs without interpreter overhead.

---

## Potential further improvements

Measured (not estimated) on this machine, see methodology after the table.
"PyPy" aside, all numbers below were obtained by exercising the actual code
paths in `scheme.py` — not projected from the fib table.

| Idea | Measured gain | Effort |
|------|--------------|--------|
| ~~JIT: tail-call via `while True` loop for self-recursive tail calls~~ | **Done — see Phase 4 above.** Turned a crash (`RecursionError` past ~4,900–5,000 iterations) into O(1)-stack execution; 50,000,000 iterations now run in ~2.4s, within ~10% of hand-written Python | ~~Medium~~ |
| ~~JIT: handle `assign_aexp` with `nonlocal` (enables `set!` in JIT)~~ | **Abandoned — see below.** ~700× potential measured, but `set!` in this interpreter is entangled with `amb`/`choose` backtracking in a way that can't be made safe without real risk of silent wrong answers | ~~Medium~~ |
| ~~JIT: handle `lambda_aexp` (compile HOF-returning functions)~~ | **Done — see Phase 5 above.** `make-adder`-shaped functions (body directly returns a closure over its own parameters) now JIT-compile; measured **~12×** on a 4,500-iteration benchmark once fixed startup cost is subtracted. Closures via internal `define`/`let` remain unsupported (separate, larger gap) | ~~Medium~~ |
| ~~Replace `GLOBALS['x'] = val` with `global x; x = val` in generated code~~ | **Done.** Isolated microbenchmark suggested ~1.2× (20%); real end-to-end gain on the trampoline path is much smaller, **~2%** (34.4s → 33.7s on a 600K-iteration `set!` benchmark, consistent across repeats) — register writes turned out to be a small slice of per-step cost next to environment/continuation allocation. **Bonus:** the change surfaced and fixed a latent correctness bug (see below) | ~~Low~~ |
| ~~JIT: allow a parameter used in operator position~~ | **Done — see Phase 6 above.** `(define (apply-twice f x) (f (f x)))`-shaped functions now JIT-compile; ~0.39s → ~0.19s on a 4,800-call benchmark (work time drops to immeasurably small, consistent with other JIT gains here). Surfaced and fixed a related missed-optimization bug in `_jit_call` itself | ~~Low~~ |
| ~~JIT: drop the forward-ref-cell indirection for self-recursive (non-tail) calls~~ | **Done — see Phase 7 above.** `fib`-shaped naive recursion **~2.3–2.8×** across three scales (`fib(20)`, `fib(30)`, `fib(37)`); tail/mutual-recursion/closure/HOF/`map`/`set!` benchmarks unaffected, as expected | ~~Low~~ |
| ~~Interpreter: Phase 2's fallback re-executes a closure's entire body, including already-completed side effects, instead of resuming or staying on the trampoline from the point of failure~~ | **Done — see Phase 8 above.** `_is_phase2_safe` gates `apply_proc`'s Phase-2 attempt on a static, transitive proof of safety instead of discovering failure mid-execution; the old silent retry is gone (a soundness gap in the checker would now crash loudly instead). Costs Phase 5/6's speedup for dynamic-dispatch shapes (~1.1–1.85× slower, measured); naive/tail/mutual recursion unaffected. Recovering that speed safely is tracked as a separate follow-up, not bundled with the correctness fix | ~~Medium–High~~ |
| Run on PyPy | 5–20× additional on top of existing gains (measured ~2.5×–10× on real workloads) — a user/deployment decision, not pursued further here | Zero code changes |

**Bonus finding, not in the original list:** `(use-stack-trace #f)` — an
*already-shipped*, zero-code-change toggle — gives **~10–13%** wall-clock and
**~25–45%** memory reduction on the trampoline (Phase 1) path, because the
default `*use-stack-trace* = #t` (`interpreter-cps.ss:372`) wraps every
non-JIT/non-Phase-2 call's continuation in an extra pop-frame
(`b_cont2_63_d`/`b_cont2_62_d` in `scheme.py`), adding a heap-allocated
continuation *and* an extra trampoline step per call. It has **no effect**
on the JIT/Phase-2 crash above — confirmed by testing directly (see below).
Worth defaulting off, or documenting as a perf knob for hot trampoline-bound
code (functions using `set!`, `call/cc`, etc.).

### Abandoned: `set!` support in Phase 2/JIT

Investigated but deliberately **not implemented**, despite a real measured
~700× potential (`_is_direct_eval_safe` in `Scheme.py` currently bars *any*
function containing `set!` from Phase 2 and Phase 3 entirely, forcing the
full trampoline).

The blocker: `set!` in this interpreter isn't a plain mutation. Every
assignment pushes a fail-continuation that **undoes** it on backtrack
(`interpreter-cps.ss`'s `assign-aexp` handling — `(lambda-fail () (set-binding-value! binding old-value) (fail))`), because `set!` must be
undoable across `amb`/`choose` backtracking. Confirmed this is live,
observable behavior today: a `choose`/`require` search that calls a
`set!`-mutating helper on each attempt ends with the mutation counter
reflecting only the *winning* branch, not the cumulative count across all
backtracked attempts — proving the undo genuinely fires.

The problem for a fast path: a function can be running "inside" an active
backtracking search without any `choose`/`amb` appearing anywhere in its own
text — the risk comes from the *caller's* dynamic context, not the callee's
code, so a static, per-function safety check can't rule it out. A more
robust option was floated (check the live `fail_reg` register — already
threaded through the whole interpreter, including into `apply_proc`'s
Phase-2 attempt — against the one well-known "no backtracking possible"
base value, rather than hand-instrumenting every `choose`/`amb` entry/exit
point), but even that still needed verifying it holds through nested
Phase 2/JIT calls and closures, not just the direct case tested. Given the
two failure modes are asymmetric — too conservative just costs a missed
optimization, too permissive means **silent wrong answers** under
backtracking — and there's no way to fully rule out the second without
substantially more implementation and verification work than the tail-call
and `lambda_aexp` fixes required, this was judged not worth the risk.
Closed out here rather than shipped partially-verified.

### Methodology

All Scheme timings run via `python scheme.py <file>.ss`; ~0.19–0.2s of that
is fixed interpreter startup, subtracted where noted. Recursion depth for
non-tail and tail self-recursive Scheme calls is capped at ~4,900–5,000
today (see the tail-call finding), so benchmarks that can't use `map`/
`for-each` (which loop in Python, not Scheme) are capped at n≈4,500–4,900.

- **Tail-call crash boundary:** `(define (loop n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1))))`
  called as `(loop N 0)` — `N=4900` returns; `N=5000` raises
  `RecursionError`, reproducibly, with or without `use-stack-trace`. Despite
  `sys.setrecursionlimit(10000)` (`scheme.py:37`), only about half the
  budget is left for the recursive loop once CLI/REPL/load frames are
  accounted for — and each JIT-compiled tail call keeps consuming a real
  Python stack frame (no tail-call flattening happens in `_JitCompiler` or
  `_eval_direct` today, only `if`-branches loop in Python).
- **`set!`/JIT-nonlocal estimate:** same `loop`, but with `(set! n n)` added
  in the body — this taints the whole closure `unsafe` (`_is_direct_eval_safe`
  in `Scheme.py`), forcing the full register-machine trampoline for every
  call. `(loop 4900 0)`: 0.46s measured vs ~0.20s startup ⇒ ~53 µs/call,
  vs. ~0.073 µs/call for JIT'd `fib` (1.6ms / 21,891 calls from the table
  above) ⇒ ~726×. A native Python equivalent of the same loop runs in
  0.0002s (4900 calls) for reference.
- **`lambda_aexp`/HOF estimate:** `(define (make-adder k) (lambda (x) (+ x k)))`
  vs. an equivalent using only inline arithmetic, both invoked 4,500 times
  from a driver loop. Closure version: 1.13s (≈0.94s work ⇒ ~209 µs/call,
  permanently stuck in Phase 2 because `_JitCompiler.expr()` has no case for
  `lambda_aexp`). No-closure version: 0.19s (≈0s measurable work — JIT'd
  after its first call). Native Python: 0.0022s for the same 4,500 calls.
  The "~400×" above is the ratio of the closure version to native Python;
  it's a lower bound on the JIT gain since a real JIT wouldn't fully close
  the gap to hand-written Python either.
- **Register-write microbenchmark:** 3,000,000 iterations of an 8-register
  trampoline step (`GLOBALS['x'] = ...` ×8) vs. the equivalent with
  `global x; x = ...`, in isolation from the rest of the interpreter. The
  real-world gain from actually making this change (`translate_rm.py`) was
  much smaller than this predicted — see the table.
- **`global x` implementation note:** `translate_rm.py`'s codegen buffers
  each generated function's body, tracks which module-level registers it
  assigns via `set!` (`check_global`), and prepends one `global ...`
  declaration instead of using `GLOBALS['name'] = ...` per write. Doing this
  surfaced a **latent correctness bug**: the generator's `locals` tracking
  never included a function's own formal parameters, so `(set! param ...)`
  on a parameter was silently emitting a module-level global write — Python
  allowed a same-named global dict entry and local parameter to coexist
  without conflict, so the parameter's local value was silently never
  updated by the `set!`. Declaring `global param` for a name that's also a
  parameter is a Python `SyntaxError`, which is what surfaced it. Fixed by
  seeding `locals` with the function's own parameter names before
  processing its body (`process_function_definition` in `translate_rm.py`).
  Full test suite (246 tests) passes after the fix.
