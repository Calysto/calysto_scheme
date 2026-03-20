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
| Python native | 0.48 ms | — |

The JIT brings Scheme `fib(20)` to within **3.5× of native Python**.

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

Instead, a dict is built **lazily on first use** mapping each `b_proc_XX_d`
function to a Python callable that takes a plain Python list of args:

```python
'+':  lambda args: plus(*args),
'<':  lambda args: LessThan(*args),
'car': lambda args: args[0].car,
'length': lambda args: length(args[0]),
'map': _direct_map,   # uses _apply_direct internally
# ... 83 total entries covering arithmetic, predicates,
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

---

### Free variable capture

Free variables (depth > 0) are looked up in the closure's captured
environment at JIT compile time and stored in the `exec()` namespace:

- **Self-recursive calls**: detected by identity (`val is self._proc`);
  a forward-reference cell `_self_ref = [None]` is injected and patched
  after compilation.
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

| Idea | Expected gain | Effort |
|------|--------------|--------|
| JIT: handle `lambda_aexp` (compile HOF-returning functions) | significant for HOF-heavy code | Medium |
| JIT: handle `assign_aexp` with `nonlocal` declarations | enables `set!` in JIT | Medium |
| JIT: tail-call via `while True` loop for self-recursive tail calls | eliminates Python stack frames | Medium |
| Run on PyPy | 5–20× additional on top of existing gains | Zero code changes |
| Replace `GLOBALS['x'] = val` with `global x; x = val` in generated code | 1.5–3× for trampoline path | Low |
