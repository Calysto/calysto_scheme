"""
Consistency check between the JIT compiler's inlining templates and the
generic fast-primitive dispatch table (both in Scheme.py).

_JitCompiler._NARY/_CMP/_UNARY inline a small, hand-picked subset (~9
entries) of Scheme primitives as raw Python operators/expression templates
at compile time. _FAST_PRIM_SPECS (consumed lazily by
_build_fast_prim_map) is the much larger (~80-entry) generic name -> Python
callable table used everywhere else Phase 2 dispatches a primitive
directly. These are two different roles -- compile-time codegen template
vs. runtime dispatch table -- and are deliberately kept as separate data
structures rather than merged (see the JIT cleanup plan). But every name
the JIT inlines *should* also be one of the generically-fast-dispatchable
primitives; if a rename or removal in _FAST_PRIM_SPECS ever left a stale
name behind in _NARY/_CMP/_UNARY, that name would still get *inlined* by
the JIT (compiled directly as a Python operator) even after silently no
longer being fast-dispatchable elsewhere, with no runtime error to signal
the drift, since the JIT never looks it up in _FAST_PRIM_SPECS to inline
it in the first place. This test only catches a hand-editing slip, since
this codebase does not distinguish "renamed a Python helper" errors any
other way for this pair of tables.
"""
import calysto_scheme.scheme as scheme


def _resolves_to_fast_prim(name):
    binding = scheme.search_env(scheme.toplevel_env, scheme.make_symbol(name))
    if binding is False:
        return False
    proc = scheme.binding_value(binding)
    if not (isinstance(proc, tuple) and proc[0] is scheme.symbol_procedure):
        return False
    if scheme._fast_prim_map is None:
        scheme._fast_prim_map = scheme._build_fast_prim_map()
    return proc[1] in scheme._fast_prim_map


def test_jit_inlined_primitives_are_a_subset_of_fast_prim_specs():
    inlined_names = (
        set(scheme._JitCompiler._NARY)
        | set(scheme._JitCompiler._CMP)
        | set(scheme._JitCompiler._UNARY)
    )
    assert inlined_names, "sanity check: the inlining tables aren't empty"

    not_fast_dispatchable = sorted(
        name for name in inlined_names if not _resolves_to_fast_prim(name)
    )
    assert not not_fast_dispatchable, (
        "these names are inlined directly by _JitCompiler but no longer "
        f"resolve to a _fast_prim_map entry: {not_fast_dispatchable} -- "
        "likely a stale entry left in _NARY/_CMP/_UNARY after a rename or "
        "removal in _FAST_PRIM_SPECS"
    )
