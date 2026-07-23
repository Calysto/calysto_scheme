# How Calysto Scheme Got Fast: A Plain-English Guide to the JIT

This document explains what was added to Calysto Scheme to make it run
much faster, and how it works.

## The problem: interpreters are slow

Calysto Scheme is an **interpreter** — a program that reads Scheme code and
carries out its instructions step by step, on the fly, rather than turning
it into a standalone program first. That's convenient (you can run code
immediately, no separate "build" step), but it's slow, because the
interpreter re-does a lot of bookkeeping every single time a piece of code
runs — even if it's the exact same function being called for the millionth
time in a loop.

As a concrete example used throughout this document, here's the classic
"naive Fibonacci" function, which calls itself twice for almost every
number it computes — a good stress test because it makes a *lot* of
function calls:

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

Computing `(fib 20)` makes 21,891 calls to `fib`. Before any of the work
described here, that took **7.4 seconds**. Native, hand-written Python
computing the same thing takes about **1 millisecond** — roughly 7,400×
faster. Closing that gap, safely, is what this whole effort was about.

The final result: `(fib 20)` now takes about **1.2 milliseconds** — within
**1.2×** of hand-written Python. The rest of this document explains how.

## The big idea: stop interpreting, start compiling

Rather than one big change, this was a series of layered improvements,
each attacking a different source of slowness. The two biggest wins came
from a shift in *strategy*, not just tuning: instead of always
interpreting Scheme code step-by-step, the system learns to recognize
"safe," well-behaved functions and either (a) run them using Python's own
machinery directly, or (b) translate them into an actual Python function
one time and reuse that translation on every future call. That second
technique — translating code to a faster form *while the program is
running*, rather than ahead of time — is called **JIT compilation**
("Just-In-Time" compilation), and it's the same basic idea used by
modern JavaScript engines in web browsers and by production Scheme
implementations to get near-native speed out of a dynamic language.

Everything below builds toward that.

## Background: how the interpreter normally runs code

Before the optimizations, every single operation — even something as
simple as comparing two numbers — went through a **trampoline loop**:

```python
def trampoline():
    while pc:
        pc()          # each step figures out what to do next
    return final_reg
```

Think of `pc` as "the next tiny step to do." Each step reads some
"registers" (about 170 global slots holding the current state), does a
small piece of work, and decides what the next step is. This structure
exists because Scheme supports advanced control-flow features (like
`call/cc`, which lets a program save and later resume "where it was")
that Python itself can't do directly — the trampoline is what makes that
possible. But it means even trivial work is very indirect: a single
function call could take 15–20 of these tiny steps, each one allocating
memory for bookkeeping along the way.

The optimizations below fall into three groups:

1. **Make the trampoline itself faster**, without changing how it works.
2. **Skip the trampoline entirely** for functions that don't need its
   advanced features, letting Python's own function-call machinery do
   the work instead.
3. **Compile those functions to real Python code**, removing interpreter
   overhead altogether.

## Phase 1: Making the slow path less slow

Before any structural change, the trampoline itself was tuned:

- **Continuations and closures became plain Python tuples** instead of
  linked-list-like structures, cutting out repeated conversions on every
  step (~10× faster per step).
- **Variable lookup got a dictionary cache.** Previously, looking up a
  variable meant scanning a list of names one at a time. Now each set of
  bindings also builds a `name → value` dictionary alongside it, so
  lookup is instant instead of scanning (~50× faster for lookups in
  frames with many variables).
- **A few common patterns were special-cased** in the code generator,
  like turning `(list-ref x 2)` directly into `x.cdr.cdr.car` instead of
  calling a general-purpose function.

None of this changed what the interpreter *could* do — it's the same
trampoline, just leaner. This alone was a **4.8× speedup**.

## Phase 2: Skip the trampoline for "safe" functions

This was the first big structural change. The key realization: the
trampoline's whole reason for existing is to support `call/cc` (and a
few related features) — but the interpreter *itself* never uses `call/cc`
internally to run ordinary code, only user Scheme code does, and only if
it explicitly calls it.

So: when a function is defined, it's checked once for whether its body
ever uses `set!` (Scheme's mutation operator — more on why this matters
below). If it doesn't, that function is tagged "safe," and calls to it
can use a much more direct evaluation path (called `_eval_direct`) that
walks the function's structure using **Python's own function calls and
recursion**, instead of going through the trampoline's 15–20 tiny steps
per call. If anything unexpected comes up mid-evaluation (like an actual
call to `call/cc`), the attempt is safely abandoned and the call re-runs
through the reliable, general-purpose trampoline instead — so this fast
path can never produce a wrong answer, only occasionally decline to help.

Common built-in operations (arithmetic, comparisons, list operations,
`map`, and about 80 others) also got a fast, direct dispatch table so
they don't have to go through general-purpose machinery either.

This phase alone brought `(fib 20)` from 1.54 seconds down to **82
milliseconds** — about **19× faster** on top of Phase 1 (91× faster than
the original).

## Phase 3: The JIT — compiling Scheme functions into real Python functions

Phase 2 still re-walks a function's structure every time it's called.
Phase 3 removes that too, for eligible functions, by actually
**generating Python source code** for the function's body, the first
time it's called, and using Python's own `compile()`/`exec()` to turn
that source into a real, callable Python function. Every call after that
first one just calls the compiled function directly — no interpreting at
all.

For our `fib` example, the JIT generates this Python function on `fib`'s
first call:

```python
def _jit_fn(_j_n):
    return (_j_n if ((_j_n < 2)) is not False else
            (_jit_fn((_j_n - 1)) + _jit_fn((_j_n - 2))))
```

That's it — a completely ordinary, fast Python function, indistinguishable
(performance-wise) from one a person would have hand-written.

A few things make this possible:

- **Names come for free.** The Scheme compiler already tags every
  variable reference in the code with information about where it lives
  (a local parameter vs. a variable captured from an enclosing scope), so
  the JIT doesn't have to guess — it just reads that information off the
  existing structure.
- **Arithmetic and comparisons are inlined as real Python operators**
  (`+`, `<`, etc.) instead of function calls, so `(+ a b)` becomes just
  `a + b` in the generated code (if not shadowed or redefined).
- **Calls to other JIT-compiled functions call each other directly**,
  and a function calling *itself* (as `fib` does) refers to itself by its
  own generated name (`_jit_fn`) rather than through any extra
  indirection — this alone was worth another ~2.3–2.8× once it was added
  (see "Phase 7" below).
- **If compilation fails for any reason** — an unsupported pattern, a
  call the compiler can't resolve — the attempt is abandoned quietly and
  the function falls back to Phase 2's interpreted-but-still-fast path.
  Again: failure only costs speed, never correctness.
- **A cache remembers each function's compiled version** (keyed by the
  function's identity) so compilation only happens once per function, not
  once per call.

This phase alone brought `(fib 20)` from 82 milliseconds down to **1.6
milliseconds** — about **51× faster** on top of Phase 2 (~4,500× faster
than the original).

## Later refinements

Once the core JIT existed, several follow-up phases closed gaps and fixed
subtle bugs it exposed:

- **Phase 4 — fixed a real crash, not just a speed problem.** Ordinary
  loops written as tail-recursive Scheme functions (the normal way to
  loop in Scheme) used to *crash* once they exceeded ~5,000 iterations,
  because each "loop iteration" was secretly consuming a Python stack
  frame. The fix teaches both the JIT and the direct-eval path to
  recognize a function calling itself as its very last action and turn
  that into an ordinary loop (a `while` loop with reassigned variables)
  instead of a new function call. This makes such loops able to run
  essentially forever using constant memory — tested up to 50 million
  iterations.
- **Phases 5 & 6 — taught the JIT to handle more patterns**: functions
  that return a new function (closures, e.g. a "make an adder" factory),
  and functions that call one of their own parameters as if it were a
  function (used for custom comparators, callbacks, etc.). Both of these
  are very common patterns in real Scheme code and previously fell back
  to the slower path unconditionally. (Update: Phase 8, below, later
  made the second half of this — calling a parameter as a function —
  unreachable in ordinary programs again, as a deliberate, understood
  side effect of closing a correctness gap, not a regression that
  slipped through unnoticed. The first half, returning a new function,
  is unaffected.)
- **Phase 7 — removed one last layer of indirection** for a function
  calling itself (the most common recursive pattern, as in `fib`), nearly
  tripling speed for that specific shape.
- **Phase 8 — fixed a genuine correctness bug**, not just a performance
  one: in certain circumstances, the "safe" fast path could re-run a
  function's entire body a second time silently, which is harmless for
  a pure calculation but dangerous for any function with a visible side
  effect (printing something, changing a stored value, writing a file).
  This was fixed by teaching the interpreter to *prove upfront*, before
  ever starting, that every part of a function's body is provably safe to
  attempt this way — rather than discovering a problem partway through
  and needing to "undo" and retry. The proof can't see far enough to
  vouch for a call whose target isn't known until the moment it runs
  (a parameter used as a function, as in Phase 6 above), so that one
  narrow pattern quietly lost its speedup again as the price of the fix
  — always correct either way, just not always as fast as Phase 6 once
  made it.
- **Phase 9 — cleanup**, consolidating duplicated logic without changing
  behavior, to make the code easier to maintain and audit going forward.

## What was deliberately *not* done

Not every possible speedup was pursued. Notably, functions that use
`set!` (Scheme's variable-mutation operator) still don't get the fast
paths at all, even though doing so was measured to offer a roughly
**700× further speedup** for functions that use it. The reason: in this
interpreter, `set!` is tied into Scheme's backtracking search features
(`amb`/`choose`), where an assignment might need to be silently *undone*
later if a search path fails. Detecting, with total confidence, whether
a given function call is ever at risk of this happening turned out to
require information that isn't available just by looking at the
function's own code — it depends on how it's being called. Getting this
wrong wouldn't just be slow, it would produce **silently incorrect
answers** in edge cases — a much worse outcome than staying slow. That
risk was judged not worth taking without a much deeper investigation, so
the idea was investigated, measured, and consciously shelved rather than
shipped half-verified.

This caution shows up throughout the project: every fast path is
designed so that a failure or an unsupported pattern can only ever cost
*speed*, by falling back to the slower-but-always-correct trampoline —
never *correctness*.

## How this is tested

Because the fast paths (`_eval_direct` and the JIT compiler) and the
always-correct trampoline are two independently-maintained implementations
of the same Scheme semantics, the main risk isn't a crash — it's the two
quietly disagreeing on an answer. The test suite (`tests/test_all.py`
running `test_all.ss`'s `jit-edge-cases` group) is run three different
ways in `tests/test_phase2_safety.py`: once with the trampoline forced
for everything, once with the JIT forced off (Phase 2's direct-eval
interpreter alone), and once normally (Phase 2 + JIT together). Every
test case's result has to match across all three runs — a mismatch means
one of the fast paths computed something different from the trampoline,
which is treated as a bug regardless of which one is "wrong."

Beyond ordinary functions, that suite specifically stress-tests the two
control-flow features called out above as deliberately excluded from the
fast paths — `call/cc` and `choose`/`require` backtracking — in
combination with functions that *are* fast-path-eligible:

- **`call/cc` escaping from inside a JIT-compiled function**, both from a
  flattened tail loop (Phase 4) and from the middle of ordinary
  non-tail recursion (like `fib`), including cases with a side-effect
  counter to confirm the trampoline fallback that `call/cc` triggers
  doesn't silently redo work that already happened.
- **`choose`/`require` backtracking searches that repeatedly call a
  JIT-compiled helper** — including a self-recursive one — across many
  branches the search tries and later abandons, confirming a compiled
  function keeps returning correct answers no matter how erratically
  backtracking control flow calls into it.
- **Real multi-shot `call/cc` generators** (a continuation captured once
  and resumed several separate times, driving a JIT-compiled helper —
  including a self-recursive one — on each resumption), built using
  `callback`, a special form that hands a Scheme procedure to a fresh,
  isolated re-entry point (the same mechanism used to pass Scheme
  procedures to host callbacks, like a `sort` comparator) so each
  resumption gets its own clean continuation boundary instead of
  colliding with the interpreter's single shared top-level continuation.

## Net result

| Stage | Time for `(fib 20)` | Speedup vs. original |
|---|---|---|
| Original interpreter | 7,400 ms | 1× |
| Phase 1 (trampoline tuning) | 1,540 ms | 4.8× |
| Phase 2 (skip the trampoline) | 82 ms | 91× |
| Phase 3 (JIT to real Python) | 1.6 ms | ~4,500× |
| Phase 7 (drop self-call indirection) | 1.2 ms | ~6,200× |
| Hand-written Python (for comparison) | 1.0 ms | — |

The Scheme interpreter now runs this kind of code within about **20% of
native Python speed** — a dynamic, interpreted Scheme system getting
essentially compiled-language performance for the patterns real programs
actually use, while still falling back safely to the original,
fully-correct interpreter for anything it can't prove safe to speed up.
