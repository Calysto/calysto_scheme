"""
Regression test found by applying the same audit method used elsewhere in
this suite (test_phase2_safe_cache_invalidation.py, test_jit_cache_
invalidation.py) to _JitCompiler._lambda (Scheme.py).

_lambda compiles a lambda_aexp encountered as a *value* -- e.g. a function
that returns a closure, like (define (make-adder k) (lambda (x) (+ x k))).
Since JIT'd code represents the enclosing function's own parameters as
plain Python locals (not a real environment frame), _lambda has to
reconstruct, at runtime, the frame those parameters would occupy under
normal (non-JIT) evaluation, so the returned closure can be looked up by
lexical address like any other Scheme proc.

_lambda is invoked once per lambda_aexp *occurrence* in the source. The
previous implementation had each occurrence independently rebuild its own
copy of that frame. That's wrong whenever a single call to the enclosing
function creates *more than one* closure -- e.g. (list (lambda ...)
(lambda ...)) -- because normal evaluation builds the frame exactly once
per call and every closure created during that call shares it. Two
independently-rebuilt frames hold two separate copies of the same
starting values, so a set! inside one sibling closure (safe for Phase 2/
JIT to compile as long as it's not the *enclosing* function's own body
that contains it -- see _is_direct_eval_safe, which stops at lambda
boundaries) silently stopped being visible to any other sibling closure
that captured the same variable -- breaking ordinary Scheme closure-
sharing semantics, silently (a wrong answer, not a crash).

Confirmed by reproducing it before this fix existed, using a nested call
to force the JIT-compiled path (a top-level call never uses it -- see
_jit_lookup's callers): a setter/getter pair sharing one captured `n`,
where calling the setter did not change what the getter saw (10 instead
of the correct 15). Fixed by building the frame at most once per logical
call (each self-recursive tail-loop iteration, or once for an ordinary
call) and sharing it across every lambda_aexp compiled from the same
enclosing function -- see _lambda's own docstring for the full design.
"""
import calysto_scheme.scheme as scheme


def test_sibling_closures_from_the_same_call_share_a_mutable_binding():
    src = """
    (define (make-pair n)
      (list (lambda (x) (set! n (+ n x)) n)
            (lambda () n)))
    (define (warmup-make-pair y) (make-pair y))
    (warmup-make-pair 1)
    (define (call-make-pair y) (make-pair y))
    (call-make-pair 1)
    """
    scheme.execute_string_rm(src)
    # make-pair itself (which contains the two lambda_aexps) is what needs
    # to be JIT-compiled for this test to mean anything -- call-make-pair
    # is only ever called at the top level here, which never uses a
    # compiled function directly (see _jit_lookup's callers: only a
    # *nested* call checks it), but that's fine: call-make-pair is still
    # Phase-2-certified (it just calls make-pair by name), so its own
    # nested call to make-pair correctly finds and uses make-pair's
    # compiled function regardless of whether call-make-pair itself ever
    # compiles.
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("make-pair")))
    assert scheme._jit_lookup(proc) is not None, (
        "sanity check: make-pair should have JIT-compiled on warmup, "
        "exercising _lambda's frame-reconstruction path (not the plain "
        "_eval_direct path a top-level call alone would use)"
    )

    result = scheme.execute_string_rm("""
    (define pair (call-make-pair 10))
    (define setter (car pair))
    (define getter (cadr pair))
    (setter 5)
    (getter)
    """)
    assert result == 15, (
        "the setter and getter closures, both created from the same call "
        f"to make-pair, did not share their captured n -- got {result!r}, "
        "expected 15 (10 + 5, via the setter's mutation being visible to "
        "the getter; the stale/unshared answer would be 10)"
    )


def test_matches_the_plain_trampoline_baseline():
    """Sanity check / control: the same program with make-pair-trampoline
    tainted by a harmless self-assigning set! in its own body (forcing it
    entirely onto the classic trampoline, no Phase 2/JIT involved) must
    produce the same answer -- confirms the JIT result above is
    diagnosing a real gap against the reference semantics, not
    disagreeing with them."""
    src = """
    (define (make-pair-trampoline n)
      (set! n n)
      (list (lambda (x) (set! n (+ n x)) n)
            (lambda () n)))
    """
    scheme.execute_string_rm(src)
    proc = scheme.binding_value(
        scheme.search_env(scheme.toplevel_env, scheme.make_symbol("make-pair-trampoline")))
    assert proc[5] is False, "sanity check: the set! should mark this proc unsafe for Phase 2"

    result = scheme.execute_string_rm("""
    (define pair2 (make-pair-trampoline 10))
    (define setter2 (car pair2))
    (define getter2 (cadr pair2))
    (setter2 5)
    (getter2)
    """)
    assert result == 15
