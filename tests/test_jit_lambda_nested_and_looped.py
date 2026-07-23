"""
Tier 2 of the _JitCompiler._lambda audit (see test_jit_lambda_shared_frame
.py and test_jit_tail_loop_nontail_stmts.py, both of which found real bugs
in this exact mechanism -- frame reconstruction for closures created
inside JIT-compiled code). Given that bug density, this file goes looking
for more, systematically, in the directions the two known bugs suggested:

  1. Closures built on *different* iterations of a self-recursive tail
     loop, accumulated into a list -- do they each get their own
     independent frame (per iteration), rather than either all sharing
     one stale frame (the class of bug test_jit_lambda_shared_frame.py
     found) or all collapsing to the loop's final values (the classic
     "closures over a loop variable" late-binding bug)?
  2. Three levels of nested lambdas, with the outermost function
     independently JIT-compiled via a warmup call -- does depth > 1
     lexical addressing resolve correctly through a *chain* of
     reconstructed frames, not just a single one?
  3. A lambda created conditionally (one of two different closures,
     depending on an if-branch) inside a tail loop -- does the lazy,
     walrus-initialized shared frame (see _lambda's docstring) handle
     branching correctly within one iteration, and still reset properly
     across iterations regardless of which branch fired?
  4. Mutual recursion combined with returning a closure -- confirms this
     stays *correct* (mutual recursion's inability to ever JIT-compile is
     an existing, accepted, performance-only limitation -- see
     _jit_lookup's "README-PERFORMANCE.md's mutual-recursion discussion"
     -- not a correctness concern this file is re-litigating).
  5. Sibling closures (a getter/setter pair) built on *each* of several
     tail-loop iterations -- combines the exact shape of the bug
     test_jit_lambda_shared_frame.py already found (siblings from one
     logical call must share a frame) with independence across
     iterations (test 1 above), simultaneously. This is the one test in
     this file that actually discriminates the fixed code from the
     pre-fix code -- confirmed by checking out Scheme.py from just before
     the _lambda fix and re-running it, which gives [3, 2, 1] (the
     setter's mutation invisible to its own sibling getter) instead of
     the correct [103, 102, 101]. Tests 1-4 all pass unchanged against
     that same pre-fix code, because none of them happen to create more
     than one sibling closure within a single logical call/iteration --
     included anyway as guard tests for these related-but-distinct
     shapes, not as regression tests for the already-fixed bug itself.

Each test uses a nested-call warmup to force the real JIT-compiled path
(a top-level call alone never uses a compiled function -- see
_jit_lookup's callers) and asserts JIT compilation actually happened, so
a future change that silently stops compiling these shapes (falling back
to the always-correct but unaudited-for-this-purpose Phase 2 path) would
be caught too.
"""
import calysto_scheme.scheme as scheme


def _scheme_list_to_pylist(result):
    values = []
    cur = result
    while isinstance(cur, scheme.cons):
        values.append(cur.car)
        cur = cur.cdr
    return values


def test_closures_built_on_different_tail_loop_iterations_are_independent():
    src = """
    (define (build-closures n acc)
      (if (= n 0)
          acc
          (build-closures (- n 1) (cons (lambda () n) acc))))
    (define (warmup n acc) (build-closures n acc))
    (warmup 1 '())
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("build-closures")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: build-closures should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("""
    (define bcl-closures (warmup 5 '()))
    (map (lambda (f) (f)) bcl-closures)
    """)
    values = _scheme_list_to_pylist(result)
    assert values == [1, 2, 3, 4, 5], (
        f"got {values!r}, expected [1, 2, 3, 4, 5] -- each closure should "
        "capture its own iteration's n, not share one stale frame (would "
        "show up as all-the-same-value) or all collapse to the loop's "
        "final value (classic late-binding-over-a-loop-variable bug)"
    )


def test_three_levels_of_nested_lambdas_resolve_depth_correctly():
    src = """
    (define (level1 a)
      (lambda (b)
        (lambda (c)
          (lambda (d) (+ a b c d)))))
    (define (warmup a) (level1 a))
    (warmup 1)
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(scheme.search_env(scheme.toplevel_env, scheme.make_symbol("level1")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: level1 should have JIT-compiled on warmup"
    )

    # warmup's own nested call to level1 uses level1's already-compiled
    # function, so the returned 3-deep closure chain was built by
    # _lambda's frame-reconstruction machinery, not plain interpretation.
    result = scheme.execute_string_rm("(((( warmup 10) 20) 30) 40)")
    assert result == 100, (
        f"got {result!r}, expected 100 (10+20+30+40) -- the innermost "
        "lambda references a variable from each of the three enclosing "
        "levels (depth 3, 2, 1), each resolved through a chain of "
        "reconstructed frames"
    )


def test_conditionally_created_closure_inside_a_tail_loop():
    src = """
    (define (build n acc)
      (if (= n 0)
          acc
          (build (- n 1)
                 (cons (if (even? n) (lambda () (* n 100)) (lambda () (* n 1000)))
                       acc))))
    (define (warmup-build n acc) (build n acc))
    (warmup-build 1 '())
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(scheme.search_env(scheme.toplevel_env, scheme.make_symbol("build")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: build should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("""
    (define branch-closures (warmup-build 5 '()))
    (map (lambda (f) (f)) branch-closures)
    """)
    values = _scheme_list_to_pylist(result)
    assert values == [1000, 200, 3000, 400, 5000], (
        f"got {values!r}, expected [1000, 200, 3000, 400, 5000] "
        "(odd n * 1000, even n * 100, each iteration's own n) -- "
        "the lazily-shared frame (see _lambda's docstring) must build "
        "correctly regardless of which if-branch creates it first, and "
        "reset correctly across iterations even when different branches "
        "fire on different iterations"
    )


def test_mutual_recursion_combined_with_a_returned_closure_stays_correct():
    """Mutual recursion's inability to ever JIT-compile is an existing,
    documented, performance-only limitation (see _jit_lookup's docstring)
    -- this only checks that combining it with _lambda doesn't also break
    correctness, not that it becomes JIT-eligible."""
    src = """
    (define (is-even? n) (if (= n 0) #t (is-odd? (- n 1))))
    (define (is-odd? n) (if (= n 0) #f (is-even? (- n 1))))
    (define (make-checker n)
      (lambda () (if (is-even? n) 'even 'odd)))
    (define (warmup-checker n) (make-checker n))
    (warmup-checker 1)
    """
    scheme.execute_string_rm(src)

    even_result = scheme.execute_string_rm("((warmup-checker 4))")
    odd_result = scheme.execute_string_rm("((warmup-checker 7))")
    assert str(even_result) == "even", f"got {even_result!r}, expected 'even"
    assert str(odd_result) == "odd", f"got {odd_result!r}, expected 'odd"


def test_sibling_pairs_built_on_each_tail_loop_iteration_are_both_shared_and_independent():
    """The one test in this file that actually fails against the pre-fix
    code (see module docstring) -- each iteration builds its own
    getter/setter pair (must share a frame with each other, the already-
    fixed bug) while staying independent from every other iteration's
    pair (must NOT share a frame with siblings from a *different*
    iteration)."""
    src = """
    (define (build n acc)
      (if (= n 0)
          acc
          (build (- n 1)
                 (cons (cons (lambda (x) (set! n (+ n x)) n) (lambda () n))
                       acc))))
    (define (warmup-pairs n acc) (build n acc))
    (warmup-pairs 1 '())
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(scheme.search_env(scheme.toplevel_env, scheme.make_symbol("build")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: build should have JIT-compiled on warmup"
    )

    result = scheme.execute_string_rm("""
    (define sp-pairs (warmup-pairs 3 '()))
    (define sp-results '())
    (for-each
      (lambda (pair)
        (define setter (car pair))
        (define getter (cdr pair))
        (setter 100)
        (set! sp-results (cons (getter) sp-results)))
      sp-pairs)
    sp-results
    """)
    values = _scheme_list_to_pylist(result)
    assert values == [103, 102, 101], (
        f"got {values!r}, expected [103, 102, 101] (each pair's own "
        "starting n + 100, distinct per pair) -- either a sibling getter "
        "isn't seeing its own sibling setter's mutation (pre-fix symptom: "
        "[3, 2, 1], the setter's set! having no visible effect), or "
        "different iterations' pairs are bleeding into each other"
    )
