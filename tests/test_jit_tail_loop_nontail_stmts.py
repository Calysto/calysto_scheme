"""
Regression test for a bug found while designing the fix for
test_jit_lambda_shared_frame.py: _jit_compile_proc (Scheme.py) flattens a
self-recursive tail call into a `while True:` loop that reassigns
parameters instead of recursing (see README-PERFORMANCE.md's Phase 4).
But the function body's *non-tail* statements -- anything before the
final, tail-position expression -- were emitted only once, before the
loop, not inside it.

Each loop iteration represents a fresh logical call (that's the whole
point of the flattening: reusing one Python stack frame across
arbitrarily many tail-recursive calls), so non-tail statements must
re-run on every iteration, exactly as they would if the call were a
genuine, unflattened recursive call. Emitting them only once meant any
side-effecting non-tail statement (a vector-set!, an accumulation, a
counter) silently only ever ran for the *first* iteration's argument
values and never again -- while the final iteration's *tail* expression
(compiled separately, correctly placed inside the loop) still ran every
time. A tail-recursive loop that fills a vector by index across its
iterations, for instance, ended up leaving every position but the last
one it visited at whatever value it started with, instead of the value
that iteration actually computed.

Confirmed by reproducing it before this fix existed: a JIT-compiled loop
that calls (vector-set! v n n) before its recursive step, run over indices
4,3,2,1,0, produced #5(0 0 0 0 4) -- only the very first iteration's
vector-set! took effect. The classic trampoline (the reference semantics)
produces the correct #5(0 1 2 3 4).

Fixed by moving stmt_lines (the compiled non-tail statements) inside the
while-loop, alongside tail_lines, instead of before it.
"""
import calysto_scheme.scheme as scheme


def test_nontail_statement_reruns_on_every_tail_loop_iteration():
    src = """
    (define (fill-vec! v n)
      (vector-set! v n n)
      (if (= n 0)
          v
          (fill-vec! v (- n 1))))
    (define (warmup-fill-vec v n) (fill-vec! v n))
    (warmup-fill-vec (make-vector 5) 0)
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("fill-vec!")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: fill-vec! should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("(warmup-fill-vec (make-vector 5) 4)")
    values = list(result)
    assert values == [0, 1, 2, 3, 4], (
        "the tail-recursive loop's non-tail vector-set! did not re-run on "
        f"every iteration -- got {values!r}, expected [0, 1, 2, 3, 4] "
        "(the bug's signature is every-position-but-the-last staying at "
        "its initial value, e.g. [0, 0, 0, 0, 4])"
    )


def test_matches_the_plain_trampoline_baseline():
    """Sanity check / control, mirroring the pattern used throughout this
    suite: the same program with fill-vec-trampoline! tainted by a
    harmless self-assigning set! (forcing it entirely onto the always-
    correct classic trampoline, no Phase 2/JIT involved) must produce the
    same answer -- confirms the JIT result above is diagnosing a real gap
    against the reference semantics, not disagreeing with them."""
    src = """
    (define (fill-vec-trampoline! v n)
      (set! n n)
      (vector-set! v n n)
      (if (= n 0)
          v
          (fill-vec-trampoline! v (- n 1))))
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("fill-vec-trampoline!")))
    assert proc[5] is False, "sanity check: the set! should mark this proc unsafe for Phase 2"

    result = scheme.execute_string_rm("(fill-vec-trampoline! (make-vector 5) 4)")
    assert list(result) == [0, 1, 2, 3, 4]
