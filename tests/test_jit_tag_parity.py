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
minus begin_aexp).

Two different asymmetries are possible here, with very different risk:

  - _JitCompiler recognizing a tag _eval_direct itself doesn't: dangerous.
    _eval_direct is the thing being shadowed -- if _eval_direct bails on a
    tag, that tag's semantics were never validated for Phase 2 at all, so
    the JIT compiling it anyway would be generating Python source for
    behavior nobody checked. This is the same failure shape as the
    already-fixed mu_lambda_aexp gap (see _phase2_safe_walk's comment: it
    deliberately does NOT certify mu_lambda_aexp as safe despite
    _is_direct_eval_safe grouping it with lambda_aexp, specifically
    because _eval_direct has no case for it).

  - _eval_direct recognizing a tag _JitCompiler doesn't (currently just
    begin_aexp, e.g. a nested `(begin ...)` produced by desugaring inside
    an if-branch): safe. _JitCompiler.expr's fallback for any unhandled
    tag is a blanket `else: raise _TrampolineFallback()`, so encountering
    it just abandons compilation for that closure -- costs speed, falls
    back to Phase 2/the trampoline, never produces a wrong answer.

test_jit_compiler_tag_set_is_a_subset_of_eval_directs guards the first
(dangerous) direction. test_jit_compiler_cleanly_bails_on_every_other_tag
behaviorally confirms the second direction really is a clean bail (a
_TrampolineFallback, not a crash or a silently wrong value) for every tag
_JitCompiler doesn't claim to handle -- including all 19 the classic
interpreter supports that neither fast path ever attempts.
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


def test_eval_direct_and_jit_tag_sets_are_sane():
    # Sanity check on the regex/introspection approach itself: if either
    # set came back empty or suspiciously small, the rest of this file's
    # assertions would be vacuous rather than meaningful.
    assert _EVAL_DIRECT_TAGS == {
        'symbol_lit_aexp', 'symbol_lexical_address_aexp', 'symbol_var_aexp',
        'symbol_if_aexp', 'symbol_lambda_aexp', 'symbol_begin_aexp',
        'symbol_app_aexp',
    }
    assert _JIT_TAGS == _EVAL_DIRECT_TAGS - {'symbol_begin_aexp'}


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
