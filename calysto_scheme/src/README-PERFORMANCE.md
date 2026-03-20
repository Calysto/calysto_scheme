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

| State | Time | Calls | vs original |
|-------|------|-------|-------------|
| Original | 7.4 s | 43 M | 1× |
| After Phase 1 (trampoline optimizations) | 1.54 s | 6.4 M | **4.8×** |
| After Phase 2 (direct eval fast path) | 0.081 s | ~330 K | **91×** |

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

Handles the five most common AST node types. The `if-aexp` branch uses a
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
            # loop — no Python stack frame consumed for the branch
        elif tag is symbol_app_aexp:
            op = _eval_direct(exp.cdr.car, env)
            args = []
            cur = exp.cdr.cdr.car
            while isinstance(cur, cons):
                args.append(_eval_direct(cur.car, env))
                cur = cur.cdr
            # ... apply (see below, inlined for speed)
        else:
            raise _TrampolineFallback()
```

---

### `_fast_prim_map` — direct primitive dispatch

Common primitives (`+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=`,
`not`, `car`, `cdr`, `cons`, `pair?`, `null?`, `zero?`) are stored in the
environment as Scheme procedure tuples `(symbol_procedure, b_proc_XX_d)`.
Going through the register-machine wrapper (`b_proc_XX_d`) would defeat
the purpose of the fast path.

Instead, a dict is built **lazily on first use** by looking up each known
symbol in the live `toplevel_env` and mapping its `b_proc_XX_d` function
to the underlying Python callable:

```python
def _build_fast_prim_map():
    name_to_direct = {
        '+':  lambda args: plus(*args),
        '-':  lambda args: minus(*args),
        '<':  lambda args: LessThan(*args),
        '=':  lambda args: numeric_equal(*args),
        'car': lambda args: args[0].car,
        'cons': lambda args: cons(args[0], args[1]),
        # ... etc.
    }
    for sym_name, direct_fn in name_to_direct.items():
        b = search_env(toplevel_env, make_symbol(sym_name))
        if b is not False:
            proc = binding_value(b)
            if isinstance(proc, tuple) and proc[0] is symbol_procedure:
                result[proc[1]] = direct_fn
    return result
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

### Fast path dispatch in `apply_proc`

```python
def apply_proc():
    if (isinstance(proc_reg, tuple) and len(proc_reg) == 6
            and proc_reg[1] is b_proc_1_d and proc_reg[5]):
        bodies, formals, cenv = proc_reg[2], proc_reg[3], proc_reg[4]
        _args, _k2, _fail = args_reg, k2_reg, fail_reg
        try:
            new_env = _extend_direct(cenv, formals, list(iter(_args)) if ... else ...)
            result = _eval_sequence_direct(bodies, new_env)
            GLOBALS['value2_reg'] = _fail
            GLOBALS['value1_reg'] = result
            GLOBALS['k_reg'] = _k2
            GLOBALS['pc'] = apply_cont2
            return
        except _TrampolineFallback:
            GLOBALS['args_reg'] = _args   # restore for trampoline fallback
            GLOBALS['k2_reg']   = _k2
            GLOBALS['fail_reg'] = _fail
    proc_reg[1](*proc_reg[2:])            # trampoline path
```

---

### Call count comparison for `(fib 20)`

| Function | Before (trampoline) | After (direct eval) |
|----------|--------------------|--------------------|
| `trampoline` steps | ~6.4 M | ~330 K |
| `m()` (main evaluator) | 273 K | ~0 (for fib body) |
| `make_cont2` (alloc) | 558 K | ~0 (for fib body) |
| `_eval_direct` | — | 252 K |
| primitive calls via `_fast_prim_map` | — | 88 K |
| `_extend_direct` / `make_frame` | 43 K | 22 K |

---

## What real Scheme implementations do

For reference, production Scheme systems use similar but deeper techniques:

- **Chez Scheme / Racket**: compile directly to native machine code; the register machine *is* the CPU; continuations are stack-allocated when escape analysis proves they don't escape.
- **CHICKEN Scheme**: CPS-compiles to C (same architecture as Calysto Scheme), but uses the C call stack directly ("Cheney on the M.T.A.") — GC is triggered when the C stack fills, copying live frames to the heap and continuing.
- **Guile**: compiles to a register-based bytecode VM with JIT.

The direct-eval fast path in Calysto Scheme is the Python analogue of what CHICKEN does: use the host language's call stack for ordinary function calls, and only fall back to the explicit continuation machinery when needed.

---

## Potential further improvements

| Idea | Expected gain | Effort |
|------|--------------|--------|
| Replace `GLOBALS['x'] = val` with `global x; x = val` in generated code | 1.5–3× | Low |
| Extend `_fast_prim_map` with more primitives (`map`, `length`, `append`, …) | 10–30% | Low |
| Handle `lambda_aexp` in `_eval_direct` (HOF-heavy code) | varies | Medium |
| Run on PyPy | 5–20× additional | Zero code changes |
