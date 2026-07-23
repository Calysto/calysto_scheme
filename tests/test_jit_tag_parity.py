"""
Guard tests applying the same audit method as
test_fastprim_no_trampoline_fallback.py to a different asymmetry: the set
of annotated-AST tags (aexp node types) each fast-path walker recognizes
by name, rather than the set of primitives each treats as safe.

The AST the classic trampoline supports is large -- 25 aexp tags exist as
of this writing (define-aexp, try-catch-aexp, raise-aexp, callback-aexp,
choose-aexp, ...). Phase 2's own semantic reference, _eval_direct,
whitelists only 7 of them (lit/lexical-address/var/if/lambda/begin/app)
and bails via _TrampolineFallback on everything else -- see the "canonical
reference" comment block above _is_direct_eval_safe in Scheme.py.
_JitCompiler.expr/tail_stmts whitelist a further-narrowed 6 (the same 7
minus begin_aexp). _phase2_safe_walk whitelists the same 7 as
_eval_direct.

Three walkers, three pairwise asymmetries are possible, with very
different risk:

  - _JitCompiler recognizing a tag _eval_direct itself doesn't: dangerous.
    _eval_direct is the thing being shadowed -- if _eval_direct bails on a
    tag, that tag's semantics were never validated for Phase 2 at all, so
    the JIT compiling it anyway would be generating Python source for
    behavior nobody checked. This is the same failure shape as the
    already-fixed mu_lambda_aexp gap (see _phase2_safe_walk's comment: it
    deliberately does NOT certify mu_lambda_aexp as safe despite
    _is_direct_eval_safe grouping it with lambda_aexp, specifically
    because _eval_direct has no case for it).

  - _phase2_safe_walk recognizing (i.e. returning True for) a tag
    _eval_direct itself doesn't: dangerous, and historically the *worse*
    of the two -- this is precisely the "soundness gap" _is_phase2_safe's
    own docstring describes and the "Benchmark-harness correctness bug"
    in README-PERFORMANCE.md that motivated adding _is_phase2_safe at
    all. Unlike a failed JIT compile (which just declines and falls back,
    no side effects yet performed), a wrongly-certified proc has already
    committed apply_proc to a *live* Phase-2 attempt by the time
    _eval_direct hits its own unhandled-tag bail -- so if any body
    statement before that point had a visible side effect, retrying the
    whole call from the top (the old, buggy fallback) would silently redo
    it. _is_phase2_safe exists specifically so this can never be
    attempted in the first place; a tag-set gap here would reopen exactly
    that hole.

  - _eval_direct recognizing a tag one of the other two doesn't (today,
    only _JitCompiler is missing one: begin_aexp, e.g. a nested
    `(begin ...)` produced by desugaring inside an if-branch): safe.
    _JitCompiler.expr's fallback for any unhandled tag is a blanket
    `else: raise _TrampolineFallback()`, so encountering it just
    abandons compilation for that closure -- costs speed, falls back to
    Phase 2/the trampoline, never produces a wrong answer. (No such gap
    exists for _phase2_safe_walk: its tag set is currently identical to
    _eval_direct's.)

test_jit_compiler_tag_set_is_a_subset_of_eval_directs and
test_phase2_safe_walk_tag_set_is_a_subset_of_eval_directs each guard one
of the two dangerous directions above. test_jit_compiler_cleanly_bails_on_
every_other_tag and test_phase2_safe_walk_cleanly_rejects_every_other_tag
behaviorally confirm the safe direction really is safe for every tag its
walker doesn't claim to handle -- a _TrampolineFallback (not a crash or a
silently wrong value) for the JIT compiler, a plain `False` (not a crash
or a wrongly-optimistic `True`) for _phase2_safe_walk -- including all 19
tags the classic interpreter supports that neither fast path ever
attempts.
"""
import inspect
import re

import calysto_scheme.scheme as scheme

_TAG_DISPATCH_RE = re.compile(r'\btag is (symbol_\w+_aexp)\b')
_TAG_NAME_RE = re.compile(r'\Asymbol_\w+_aexp\Z')


def _tags_dispatched_by(func):
    """The set of aexp tags (by module-attribute name) a walker function's
    source explicitly checks for, found the same way the walkers
    themselves dispatch: a literal `tag is symbol_..._aexp` comparison."""
    return set(_TAG_DISPATCH_RE.findall(inspect.getsource(func)))


_ALL_KNOWN_TAGS = {name for name in dir(scheme) if _TAG_NAME_RE.match(name)}
_EVAL_DIRECT_TAGS = _tags_dispatched_by(scheme._eval_direct)
_JIT_TAGS = (_tags_dispatched_by(scheme._JitCompiler.expr)
             | _tags_dispatched_by(scheme._JitCompiler.tail_stmts))
_PHASE2_SAFE_WALK_TAGS = _tags_dispatched_by(scheme._phase2_safe_walk)


def test_eval_direct_and_jit_and_phase2_safe_walk_tag_sets_are_sane():
    # Sanity check on the regex/introspection approach itself: if any of
    # these sets came back empty or suspiciously small, the rest of this
    # file's assertions would be vacuous rather than meaningful.
    assert _EVAL_DIRECT_TAGS == {
        'symbol_lit_aexp', 'symbol_lexical_address_aexp', 'symbol_var_aexp',
        'symbol_if_aexp', 'symbol_lambda_aexp', 'symbol_begin_aexp',
        'symbol_app_aexp',
    }
    assert _JIT_TAGS == _EVAL_DIRECT_TAGS - {'symbol_begin_aexp'}
    assert _PHASE2_SAFE_WALK_TAGS == _EVAL_DIRECT_TAGS


def test_jit_compiler_tag_set_is_a_subset_of_eval_directs():
    extra = _JIT_TAGS - _EVAL_DIRECT_TAGS
    assert not extra, (
        f"_JitCompiler recognizes {sorted(extra)}, which _eval_direct "
        "(Phase 2's own semantic reference for what a closure is even "
        "allowed to run as) does not handle at all. Anything _eval_direct "
        "itself bails on is unverified semantics for Phase 2 -- the JIT "
        "must never claim to compile it. If this is a deliberate, "
        "correct expansion (shared new support added to both), extend "
        "_eval_direct first and then update this test's expectations."
    )


def test_phase2_safe_walk_tag_set_is_a_subset_of_eval_directs():
    extra = _PHASE2_SAFE_WALK_TAGS - _EVAL_DIRECT_TAGS
    assert not extra, (
        f"_phase2_safe_walk recognizes (certifies safe) {sorted(extra)}, "
        "which _eval_direct does not handle at all. A proc containing "
        "this tag could be certified _is_phase2_safe == True, apply_proc "
        "would then start a live Phase-2 attempt trusting that "
        "certification, and _eval_direct would raise _TrampolineFallback "
        "partway through -- exactly the soundness gap _is_phase2_safe's "
        "docstring and README-PERFORMANCE.md's 'Benchmark-harness "
        "correctness bug' section describe, silently re-executing any "
        "side effects already performed earlier in the body on retry. "
        "If this is a deliberate, correct expansion (shared new support "
        "added to both), extend _eval_direct first and then update this "
        "test's expectations."
    )


def _minimal_node(tag):
    """A cons cell tagged `tag` with a stub .cdr. Safe because every
    bail-out branch in expr()/tail_stmts() (the final `else: raise
    _TrampolineFallback()`) fires on the tag check alone, before ever
    looking at a node's other fields."""
    return scheme.cons(tag, scheme.symbol_emptylist)


def _new_compiler():
    # self_proc's only use for a bailed-out tag would be an identity
    # check (`val is self._self`), never reached here -- any object works.
    return scheme._JitCompiler(object(), [], scheme.toplevel_env, {})


def test_jit_compiler_cleanly_bails_on_every_tag_it_does_not_recognize():
    unrecognized = sorted(_ALL_KNOWN_TAGS - _JIT_TAGS)
    assert unrecognized, "sanity check: there should be unrecognized tags to probe"
    crashed, silent = [], []
    for tag_name in unrecognized:
        tag = getattr(scheme, tag_name)
        jc = _new_compiler()
        try:
            jc.expr(_minimal_node(tag))
            silent.append(tag_name)
        except scheme._TrampolineFallback:
            pass  # expected: a clean, safe bail
        except Exception:
            crashed.append(tag_name)
    assert not silent, (
        f"_JitCompiler.expr() silently returned a value for unrecognized "
        f"tag(s) {silent} instead of bailing -- it must never compile a "
        "tag it doesn't actually implement"
    )
    assert not crashed, (
        f"_JitCompiler.expr() raised something other than "
        f"_TrampolineFallback for unrecognized tag(s) {crashed}. "
        "_jit_compile_proc's own blanket `except Exception` (Scheme.py) "
        "would still catch this and fall back safely, so it's not an "
        "immediate correctness risk -- but it means an accidental crash "
        "(e.g. an AttributeError from a genuine bug elsewhere in expr()) "
        "would be silently indistinguishable from an intentional bail, "
        "masked by that same catch-all. The bail-out path should raise "
        "_TrampolineFallback specifically, not fall into it by accident."
    )


def test_phase2_safe_walk_cleanly_rejects_every_tag_it_does_not_recognize():
    # Unlike _JitCompiler.expr, _phase2_safe_walk never raises on purpose
    # -- it's a pure classifier that returns a bool. "Clean" here means
    # returning False (unproven -> unsafe), never True (which would be
    # the soundness gap described in this file's module docstring) and
    # never letting an exception escape uncaught (which _is_phase2_safe's
    # own `except Exception: safe = False` would mask, turning a real bug
    # in the walker into an indistinguishable "safe bail").
    unrecognized = sorted(_ALL_KNOWN_TAGS - _PHASE2_SAFE_WALK_TAGS)
    assert unrecognized, "sanity check: there should be unrecognized tags to probe"
    crashed, wrongly_true = [], []
    for tag_name in unrecognized:
        tag = getattr(scheme, tag_name)
        try:
            verdict = scheme._phase2_safe_walk(
                _minimal_node(tag), scheme.toplevel_env, set())
            if verdict is not False:
                wrongly_true.append(tag_name)
        except Exception:
            crashed.append(tag_name)
    assert not wrongly_true, (
        f"_phase2_safe_walk() returned a non-False verdict for "
        f"unrecognized tag(s) {wrongly_true} instead of rejecting them -- "
        "it must never certify a tag it doesn't actually implement as "
        "safe to run through Phase 2."
    )
    assert not crashed, (
        f"_phase2_safe_walk() raised an exception for unrecognized "
        f"tag(s) {crashed} instead of cleanly returning False. "
        "_is_phase2_safe's own blanket `except Exception: safe = False` "
        "would still catch this and treat the whole proc as unsafe, so "
        "it's not an immediate correctness risk -- but it means an "
        "accidental crash (e.g. an AttributeError from a genuine bug "
        "elsewhere in the walker) would be silently indistinguishable "
        "from an intentional, considered rejection, masked by that same "
        "catch-all. The rejection path should be an ordinary `return "
        "False`, not an escaped exception."
    )
