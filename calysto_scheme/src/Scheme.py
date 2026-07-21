#############################################################
# Scheme.py
# These are native implementations of functions to allow
# the register machine translation to run in Python

from __future__ import division, print_function

from collections.abc import Iterable
import inspect
import fractions
import functools
import operator
import random as pyrandom
import types
import math
import time
import sys
import os
import io

try:
    import yasi
    yasi.IF_LIKE = [] ## removed "if" so indents then-part and else-part the same
    opts = yasi.parse_args([])
    opts.dialect = "scheme"
except:
    yasi = None


# To trick some systems into believing input is interactive:
sys.ps1 = "In : "
sys.ps2 = "...: "

PY3 = sys.version_info[0] == 3

# Increase recursion limit for direct-eval fast path (deep Scheme recursion)
sys.setrecursionlimit(max(10000, sys.getrecursionlimit()))

__version__ = "2.1.1"

#############################################################
# Python implementation notes:
#
# Each symbol is a singleton for easy comparison reasons:
# Symbol("x") is Symbol("x")
#
# Python's list is used as Scheme's vector.
#
# The List() class is used for Scheme's con-cell based lists.
#
# Lists implement iter, so you can use Python's iter tools
# (such as [x for x in List(1, 2, 3)])
#
# A couple of functions are O(2n) because they have a
# reverse. Should be fixed to be O(n).
#############################################################

## Global symbols:

# Set to a dictionary-like object for global-shared namespace:
ENVIRONMENT = {}
# Python environment:
if "str" in dir(__builtins__):
    ENVIRONMENT.update({key: getattr(__builtins__, key)
                        for key in dir(__builtins__)})
# IPython environment:
if "keys" in dir(__builtins__):
    ENVIRONMENT.update(__builtins__)
ENVIRONMENT["DEBUG"] = False

GLOBALS = globals()

class DebugException(Exception):
    """
    Exception for use in GUI
    """
    def __init__(self, data):
        super(DebugException, self).__init__()
        self.data = data

class Char(object):
    def __init__(self, c):
        self.char = c
    def __eq__(self, other):
        return isinstance(other, Char) and self.char == other.char
    def __lt__(self, other):
        return isinstance(other, Char) and self.char < other.char
    def __gt__(self, other):
        return isinstance(other, Char) and self.char > other.char
    def __str__(self):
        if self.char == " ":
            return "#\\space"
        elif self.char == "\n":
            return "#\\newline"
        return "#\\%s" % self.char
    def __repr__(self):
        return str(self)

class Symbol(object):
    def __init__(self, name):
        self.name = name
        self.hash = hash(name)

    def __repr__(self):
        return "%s" % self.name

    def __eq__(self, other):
        return isinstance(other, Symbol) and self.hash == other.hash

    def __hash__(self):
        return hash(self.name)

    def __iter__(self):
        # So that EmptyList will be treated as []
        return self

    def next(self):
        # So that EmptyList will be treated as []
        raise StopIteration

    def __next__(self):
        # So that EmptyList will be treated as []
        raise StopIteration

    def __len__(self):
        # So that EmptyList will be treated as []
        return 0

    def __getattr__(self, attr):
        if attr == "name":
            return self.__getattribute__("name")
        elif hasattr(self.name, attr):
            return getattr(self.name, attr)
        else:
            raise AttributeError("no such attribute '%s' on '%s'" % (attr, self.name))

SYMBOLS = {}
CHARS = {}

def make_symbol(string):
    if not (string in SYMBOLS):
        SYMBOLS[string] = Symbol(string)
    return SYMBOLS[string]

def make_char(c):
    if not (c in CHARS):
        CHARS[c] = Char(c)
    return CHARS[c]

void_value = make_symbol("<void>")

def make_initial_env_extended(names, procs, docstrings):
    ## If you wish to extend the environment to
    ## include native values, do so here:
    return make_initial_environment(names, procs, docstrings)

### Lists:

class cons(object):
    # build a cons cell
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        # Written to not deal with exact same
        # atoms, so will work with unpickled objs
        if isinstance(self.car, Symbol):
            if self.car.name == "procedure":
                return "#<procedure>"
            elif self.car.name == "environment":
                return "#<environment>"
        retval = io.StringIO()
        retval.write("(")
        current = self
        while isinstance(current, cons):
            if retval.tell() != 1:
                retval.write(" ")
            retval.write(make_safe(current.car))
            current = current.cdr
        if not (isinstance(current, Symbol) and current.name == "()"):
            retval.write(" . " + make_safe(current))
        retval.write(")")
        return retval.getvalue()

    def __call__(self, *args, **kwargs):
        if self.car is symbol_procedure:
            return dlr_func(self)(*args, **kwargs)
        else:
            raise Exception("not a procedure")

    def __iter__(self):
        cp = cons(self.car, self.cdr)
        cp.current = cp
        return cp

    def __len__(self):
        if isinstance(self.cdr, cons):
            current = self
            count = 0
            while isinstance(current, cons):
                count += 1
                current = current.cdr
            if null_q(current):
                return count
            else:
                raise AttributeError("list is not a proper list")
        else:
            return 1

    def next(self): # Python 2
        if not isinstance(self.current, cons):
            raise StopIteration
        else:
            retval = self.current.car
            self.current = self.current.cdr
            return retval

    def __next__(self): # Python 3
        if not isinstance(self.current, cons):
            raise StopIteration
        else:
            retval = self.current.car
            self.current = self.current.cdr
            return retval

    def __getitem__(self, pos):
        if pos < 0:
            raise Exception("negative index not allowed")
        current = self
        for i in range(pos):
            current = current.cdr
        return current.car

def List(*args):
    # Scheme list
    retval = symbol_emptylist
    for i in range(len(args)):
        arg = args[len(args) - i - 1]
        retval = cons(arg, retval)
    return retval

def reverse(lyst):
    retval = symbol_emptylist
    current = lyst
    while isinstance(current, cons):
        retval = cons(current.car, retval)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    return retval

def length(lyst):
    current = lyst
    count = 0
    while isinstance(current, cons):
        current = current.cdr
        count += 1
    if current != symbol_emptylist:
        raise Exception("not a proper list")
    return count

def Map(f, lyst, lyst2=None):
    if lyst2 is None:
        stack = symbol_emptylist
        current = lyst
        while isinstance(current, cons):
            stack = cons(f(current.car), stack)
            current = current.cdr
        if current != symbol_emptylist:
            raise Exception("not a proper list")
        retval = symbol_emptylist
        current = stack
        while isinstance(current, cons):
            retval = cons(current.car, retval)
            current = current.cdr
        return retval
    else:
        stack = symbol_emptylist
        current1 = lyst
        current2 = lyst2
        while isinstance(current1, cons) and isinstance(current2, cons):
            stack = cons(f(current1.car, current2.car), stack)
            current1 = current1.cdr
            current2 = current2.cdr
        if current1 != symbol_emptylist:
            raise Exception("not a proper list")
        if current2 != symbol_emptylist:
            raise Exception("not a proper list")
        retval = symbol_emptylist
        current = stack
        while isinstance(current, cons):
            retval = cons(current.car, retval)
            current = current.cdr
        return retval

def for_each(f, lyst):
    current = lyst
    while isinstance(current, cons):
        f(current.car)
        current = current.cdr
    if current != symbol_emptylist:
        raise Exception("not a proper list")

def make_comparison_function(procedure):
    def compare(carl, cadrl):
        save_k2_reg = k2_reg
        GLOBALS["proc_reg"] = procedure
        GLOBALS["args_reg"] = List(carl, cadrl)
        GLOBALS["handler_reg"] = REP_handler
        GLOBALS["k2_reg"] = REP_k
        GLOBALS["pc"] = apply_proc
        retval = trampoline()
        GLOBALS["k2_reg"] = save_k2_reg
        return retval
    return compare

def apply_comparison(p, carl, cadrl):
    return p(carl, cadrl)

def random(number):
    if isinstance(number, float):
        return pyrandom.random() * number
    elif isinstance(number, int):
        return pyrandom.randint(0, number - 1)
    else:
        raise Exception("random function received invalid value: %s" % number)

def sort_native():
    state = {
        'args_reg': GLOBALS['args_reg'],
        'env2_reg': GLOBALS['env2_reg'],
        'info_reg': GLOBALS['info_reg'],
        'handler_reg': GLOBALS['handler_reg'],
        'fail_reg': GLOBALS['fail_reg'],
        'k2_reg': GLOBALS['k2_reg'],
    }
    pred = GLOBALS['args_reg'].car
    elements = GLOBALS['args_reg'].cdr.car
    try:
        python_pred = dlr_func(pred)
        result = sort(python_pred, elements)
        GLOBALS.update(state)
        GLOBALS['value1_reg'] = result
        GLOBALS['value2_reg'] = GLOBALS['fail_reg']
        GLOBALS['k_reg'] = GLOBALS['k2_reg']
        GLOBALS['pc'] = apply_cont2
    except Exception as exc:
        GLOBALS.update(state)
        GLOBALS['msg_reg'] = str(exc)
        GLOBALS['pc'] = runtime_error

def sort(p, arg):
    l = list_to_vector(arg)
    if procedure_q(p):
        f = make_comparison_function(p)
    else:
        f = p
    def cmp(a, b):
        result = f(a, b)
        if exception_q(result):
            raise Exception(cadadr(result)) ## FIXME: get_exception_message
        if result:
            return -1
        elif equal_q(a, b):
            return 0
        else:
            return 1
    l.sort(key=functools.cmp_to_key(cmp))
    return vector_to_list(l)

def append(*objs):
    retval = objs[-1]
    # FIXME: rewrite without reversed
    for obj in reversed(objs[:-1]):
        # FIXME: rewrite without reverse
        current = reverse(obj)
        while isinstance(current, cons):
            retval = cons(current.car, retval)
            current = current.cdr
    return retval

def car(lyst):
    return lyst.car

def cdr(lyst):
    return lyst.cdr

def caar(lyst):
    return lyst.car.car

def cadr(lyst):
    return lyst.cdr.car

def cddr(lyst):
    return lyst.cdr.cdr

def cdar(lyst):
    return lyst.car.cdr

def caddr(lyst):
    return lyst.cdr.cdr.car

def cadar(lyst):
    return lyst.car.cdr.car

def cdddr(lyst):
    return lyst.cdr.cdr.cdr

def cadddr(lyst):
    return lyst.cdr.cdr.cdr.car

def cddddr(lyst):
    return lyst.cdr.cdr.cdr.cdr

def caaaar(lyst):
    return lyst.car.car.car.car

def caaadr(lyst):
    return lyst.cdr.car.car.car

def caaar(lyst):
    return lyst.car.car.car

def caadar(lyst):
    return lyst.car.cdr.car.car

def caaddr(lyst):
    return lyst.cdr.cdr.car.car

def caadr(lyst):
    return lyst.cdr.car.car

def cadaar(lyst):
    return lyst.car.car.cdr.car

def cadadr(lyst):
    return lyst.cdr.car.cdr.car

def caddar(lyst):
    return lyst.car.cdr.cdr.car

def cdaaar(lyst):
    return lyst.car.car.car.cdr

def cdaadr(lyst):
    return lyst.cdr.car.car.cdr

def cdaar(lyst):
    return lyst.car.car.cdr

def cdadar(lyst):
    return lyst.car.cdr.car.cdr

def cdaddr(lyst):
    return lyst.cdr.cdr.car.cdr

def cdadr(lyst):
    return lyst.cdr.car.cdr

def cddaar(lyst):
    return lyst.car.car.cdr.cdr

def cddadr(lyst):
    return lyst.cdr.car.cdr.cdr

def cddar(lyst):
    return lyst.car.cdr.cdr

def cdddar(lyst):
    return lyst.car.cdr.cdr.cdr

def set_car_b(cell, item):
    cell.car = item
    return void_value

def set_cdr_b(cell, item):
    cell.cdr = item
    return void_value

def list_tail(lyst, pos):
    if pos < 0:
        raise Exception("invalid list-ref position: " + pos)
    current = lyst
    while pos != 0:
        current = current.cdr
        pos = pos - 1
    return current

def list_head(lyst, pos):
    stack = symbol_emptylist
    current = lyst
    for i in range(pos):
        stack = cons(current.car, stack)
        current = current.cdr
    retval = symbol_emptylist
    for i in range(pos):
        retval = cons(stack.car, retval)
        stack = stack.cdr
    return retval

def list_ref(lyst, pos):
    if pos < 0:
        raise Exception("invalid list-ref position: " + pos)
    current = lyst
    while pos != 0:
        current = current.cdr
        pos = pos - 1
    return current.car

def vector_length(vec):
    return len(vec)

### Native make- functions:

def make_proc(*args):
    return (symbol_procedure,) + args

def make_macro(*args):
    return (symbol_macro_transformer,) + args

def make_cont(*args):
    return (symbol_continuation,) + args

def make_cont2(*args):
    return (symbol_continuation2,) + args

def make_cont3(*args):
    return (symbol_continuation3,) + args

def make_cont4(*args):
    return (symbol_continuation4,) + args

def make_fail(*args):
    return (symbol_fail_continuation,) + args

def make_handler(*args):
    return (symbol_handler,) + args

def make_handler2(*args):
    return (symbol_handler2,) + args

### Native apply dispatch functions (tuple-based):

def apply_cont():
    k_reg[1](*k_reg[2:])

def apply_cont2():
    k_reg[1](*k_reg[2:])

def apply_cont3():
    k_reg[1](*k_reg[2:])

def apply_cont4():
    k_reg[1](*k_reg[2:])

def apply_fail():
    fail_reg[1](*fail_reg[2:])

def apply_handler():
    handler_reg[1](*handler_reg[2:])

def apply_handler2():
    handler_reg[1](*handler_reg[2:])

def apply_proc():
    if (isinstance(proc_reg, tuple) and len(proc_reg) == 6
            and proc_reg[1] is b_proc_1_d and proc_reg[5]
            and _is_phase2_safe(proc_reg)):
        bodies, formals, cenv = proc_reg[2], proc_reg[3], proc_reg[4]
        _args = args_reg
        _k2   = k2_reg
        _fail = fail_reg
        # Arity check happens before any body statement runs, so nothing
        # has executed yet -- safe to fall back to the trampoline here.
        try:
            n = length(formals)
            if n != length(_args):
                raise _TrampolineFallback()
            new_env = extend(cenv, formals, _args, make_empty_docstrings(n))
        except _TrampolineFallback:
            GLOBALS['args_reg'] = _args
            GLOBALS['k2_reg']   = _k2
            GLOBALS['fail_reg'] = _fail
            proc_reg[1](*proc_reg[2:])
            return
        # This closure was certified by _is_phase2_safe as fully
        # representable by Phase 2, so _eval_sequence_direct must run to
        # completion. If it somehow still raises _TrampolineFallback,
        # that means _is_phase2_safe has a soundness gap. The old
        # behavior here was to silently discard this attempt and re-run
        # the whole closure body via the trampoline -- which re-executes
        # any side effects (mutation, I/O, ...) from statements that
        # already completed before the failure (see
        # README-PERFORMANCE.md's "Benchmark-harness correctness bug"
        # section). Letting the exception propagate instead turns a
        # silent wrong answer into a loud, reportable failure -- BUT
        # trampoline()'s own top-level `except Exception` catches any
        # propagating Python exception (including this one) and converts
        # it into an ordinary Scheme exception object, dispatched through
        # the normal handler_reg chain, not a process crash. Some Scheme-
        # level handlers -- notably run-unit-test-cases in
        # interpreter-cps.ss, which re-evaluates a failed assertion's
        # sub-expressions to build a diagnostic report -- can then
        # immediately re-invoke the very same (wrongly-certified) proc,
        # which would hit the identical failure again, forever, since the
        # wrong verdict stays cached. Invalidating the cache entry here,
        # before re-raising, means any later attempt on this exact proc
        # (including such an immediate diagnostic re-evaluation) takes
        # the always-correct slow path instead of repeating the same
        # failure -- the first failure still propagates and stays fully
        # visible, but it can't loop.
        try:
            result = _eval_sequence_direct(bodies, new_env)
        except _TrampolineFallback:
            _phase2_safe_cache.set(proc_reg, False)
            raise
        GLOBALS['value2_reg'] = _fail
        GLOBALS['value1_reg'] = result
        GLOBALS['k_reg'] = _k2
        GLOBALS['pc'] = apply_cont2
        return
    proc_reg[1](*proc_reg[2:])

def apply_macro():
    macro_reg[1](*macro_reg[2:])

### Direct eval fast path for user closures:

class _TrampolineFallback(Exception):
    pass

## ===== Annotated-AST (aexp) node layouts: canonical reference =====
##
## _eval_direct, _JitCompiler, and _phase2_safe_walk* below are three
## independent walks over the same annotated-AST node shapes (each tagged
## by a `symbol_*_aexp` singleton, matched by `is`). All three access a
## node's fields as raw cons-cell chains (`exp.cdr.car`, etc.) rather than
## through helper accessor functions: a documented attempt at wrapping
## each tag's field extraction in a small accessor function measured a
## real 6-19% slowdown (see README-PERFORMANCE.md's Phase 9 addendum),
## concentrated in exactly the code paths that have no per-closure caching
## to amortize it (a closure that never successfully JIT-compiles, so its
## whole AST is re-walked from scratch on every single call; or a fresh
## closure allocated every call, so neither the JIT cache nor
## _phase2_safe_cache ever gets a hit) -- with no offsetting benefit where
## a closure compiles once and runs at native speed forever after. Given
## that, the field layout below is documented here, once, as the single
## place to look up a tag's shape, rather than as a function each walker
## would pay to call:
##
##   lit-aexp:              (tag datum info)
##                            datum = exp.cdr.car
##   lexical-address-aexp:  (tag depth offset name info)
##                            depth=0 -> local parameter (JIT only);
##                            depth>0 -> free var, offset into the frame
##                            depth-1 levels out in the captured env
##                            depth  = exp.cdr.car
##                            offset = exp.cdr.cdr.car
##                            name   = exp.cdr.cdr.cdr.car
##   var-aexp:               (tag id info)
##                            id = exp.cdr.car
##   if-aexp:                (tag test then else info)
##                            test = exp.cdr.car
##                            then = exp.cdr.cdr.car
##                            else = exp.cdr.cdr.cdr.car
##   lambda-aexp:            (tag formals bodies info)
##                            formals = exp.cdr.car
##                            bodies  = exp.cdr.cdr.car
##   app-aexp:               (tag operator operands info)
##                            operator = exp.cdr.car
##                            operands = exp.cdr.cdr.car
##   begin-aexp:             (tag exp1 exp2 ... expN)
##                            exp.cdr is itself a proper cons-list of body
##                            expressions -- no trailing info field, unlike
##                            the tags above.

def _is_direct_eval_safe(bodies):
    """True iff bodies contain no assign_aexp (set!) at any depth.
    Stops recursion at inner lambda boundaries (they get their own analysis)."""
    def _safe(exp):
        if not isinstance(exp, cons):
            return True
        tag = exp.car
        if tag is symbol_assign_aexp:
            return False
        # Inner lambdas are separate closures — don't recurse into them
        if tag is symbol_lambda_aexp or tag is symbol_mu_lambda_aexp:
            return True
        cur = exp.cdr
        while isinstance(cur, cons):
            if not _safe(cur.car):
                return False
            cur = cur.cdr
        return True
    cur = bodies
    while isinstance(cur, cons):
        if not _safe(cur.car):
            return False
        cur = cur.cdr
    return True

_CACHE_MISS = object()   # sentinel distinct from any real cached value (incl. False/None)

class _IdentityCache:
    """Maps id(key) -> (key, value). A lookup whose stored key isn't `is`
    the object asked about is a miss, not a hit for the wrong object --
    guards against Python reusing a garbage-collected object's id()."""
    __slots__ = ("_entries",)

    def __init__(self):
        self._entries = {}

    def get(self, key):
        entry = self._entries.get(id(key))
        if entry is None:
            return _CACHE_MISS
        cached_key, value = entry
        if cached_key is not key:
            del self._entries[id(key)]   # stale -- id() reused after GC
            return _CACHE_MISS
        return value

    def set(self, key, value):
        self._entries[id(key)] = (key, value)

_phase2_safe_cache = _IdentityCache()   # proc -> True/False

def _phase2_safe_lookup(proc):
    """Return the cached verdict for proc, or None if not yet computed."""
    value = _phase2_safe_cache.get(proc)
    return None if value is _CACHE_MISS else value

def _is_phase2_safe(proc, _visiting=None):
    """Conservatively certify that invoking `proc` via apply_proc's Phase-2
    fast path is guaranteed to run to completion without ever raising
    _TrampolineFallback -- i.e. that it is safe to START a live Phase-2
    attempt for it at all.

    This exists because a live Phase-2 attempt that fails partway through
    a multi-statement body does not fail cleanly: apply_proc's old
    fallback re-ran the closure from the top on any mid-body
    _TrampolineFallback, silently re-executing whatever side effects
    already happened before the failure (see README-PERFORMANCE.md's
    "Benchmark-harness correctness bug" section, where this was
    discovered). The fix is to never start an attempt that isn't already
    proven to succeed.

    Mirrors _eval_direct's own dispatch (same AST node cases, same
    "reuse this Python frame" call-inlining behavior for nested closure
    calls) but only classifies -- it never executes anything and has no
    side effects, the same way a failed JIT compile attempt
    (_jit_compile_proc) never executes anything either.

    Deliberately conservative: anything this can't prove safe (a call
    through a parameter or other computed/dynamic operator, a call to a
    closure that itself contains set!, an unresolved/forward-referenced
    name, map/for-each's callback argument, ...) is treated as unsafe.
    Being too conservative only costs speed (apply_proc falls back to the
    always-correct slow trampoline for that closure); the risk this
    guards against -- silently re-executing side effects -- only exists
    on the "too permissive" side, so ties are broken toward unsafe.

    Computed lazily, on first use (a closure's referenced names must
    already be bound in its captured environment -- mirrors the JIT
    compiler's own lazy, on-first-call compile+cache pattern, including
    the same one-call "miss" for mutually recursive functions whose
    partner isn't certified yet). Cached by identity in
    _phase2_safe_cache, with the same GC-id-reuse guard _jit_cache uses.
    Self/mutually-recursive references are handled by optimistically
    assuming a proc already being checked (present in `_visiting`) is
    safe -- sound because the overall verdict still walks every other
    reachable expression and ANDs them together, so any real unsafety
    anywhere in the cycle is still found and still fails the whole
    checked closure; only the outermost call's verdict is cached, so a
    cycle can't cache a premature/partial answer.
    """
    if not (isinstance(proc, tuple) and len(proc) == 6
            and proc[1] is b_proc_1_d and proc[5]):
        return False
    cached = _phase2_safe_lookup(proc)
    if cached is not None:
        return cached
    outermost = _visiting is None
    if _visiting is None:
        _visiting = set()
    pid = id(proc)
    if pid in _visiting:
        return True
    _visiting.add(pid)
    try:
        safe = _phase2_safe_walk_seq(proc[2], proc[4], _visiting)
    except Exception:
        safe = False
    finally:
        _visiting.discard(pid)
    if outermost:
        _phase2_safe_cache.set(proc, safe)
    return safe

def _phase2_safe_walk_seq(bodies, env, visiting):
    cur = bodies
    while isinstance(cur, cons):
        if not _phase2_safe_walk(cur.car, env, visiting):
            return False
        cur = cur.cdr
    return True

def _phase2_safe_walk(exp, env, visiting):
    """True iff evaluating exp in env via Phase 2 can never raise
    _TrampolineFallback. See _is_phase2_safe's docstring."""
    if not isinstance(exp, cons):
        return True
    tag = exp.car
    if tag is symbol_lit_aexp:
        return True
    elif tag is symbol_lexical_address_aexp:
        return True   # compiler-verified; always resolves
    elif tag is symbol_var_aexp:
        return search_env(env, exp.cdr.car) is not False
    elif tag is symbol_if_aexp:
        return (_phase2_safe_walk(exp.cdr.car, env, visiting) and
                _phase2_safe_walk(exp.cdr.cdr.car, env, visiting) and
                _phase2_safe_walk(exp.cdr.cdr.cdr.car, env, visiting))
    elif tag is symbol_lambda_aexp:
        # Creating a closure has no side effects and doesn't call
        # anything; its own body gets its own independent, lazy check
        # if/when IT is ever itself invoked via apply_proc.
        return True
        # NOTE: symbol_mu_lambda_aexp (a lambda with dotted/rest formals,
        # e.g. (lambda (a . rest) ...)) is deliberately NOT included above
        # despite being grouped with symbol_lambda_aexp in
        # _is_direct_eval_safe's lambda-boundary check. _eval_direct has
        # no case for symbol_mu_lambda_aexp at all -- it falls through to
        # `else: raise _TrampolineFallback()` -- so treating it as safe
        # here would certify a closure Phase 2 cannot actually evaluate.
        # Falls through to the final `else: return False` below.
    elif tag is symbol_begin_aexp:
        return _phase2_safe_walk_seq(exp.cdr, env, visiting)
    elif tag is symbol_app_aexp:
        op_exp = exp.cdr.car
        cur = exp.cdr.cdr.car
        while isinstance(cur, cons):
            if not _phase2_safe_walk(cur.car, env, visiting):
                return False
            cur = cur.cdr
        return _phase2_safe_walk_call(op_exp, env, visiting)
    else:
        return False

def _resolve_lexical_address(depth, offset, env):
    """Classify a lexical-address reference against env, without executing
    or raising: ('local', None) for a depth-0 local parameter (a
    runtime-supplied value -- can't resolve statically); ('value', v) if
    the outer-frame binding resolves; ('unresolved', None) if the frame
    walk fails (e.g. env doesn't actually have that many enclosing
    frames yet -- a forward reference)."""
    if depth == 0:
        return ('local', None)
    try:
        frm = list_ref(frames(env), depth - 1)
        return ('value', binding_value(vector_ref(frame_bindings(frm), offset)))
    except Exception:
        return ('unresolved', None)

def _resolve_var(sym, env):
    """Classify a var-aexp reference by name against env, without executing
    or raising: ('value', v) if bound, ('unresolved', None) if not."""
    b = search_env(env, sym)
    if b is False:
        return ('unresolved', None)
    return ('value', binding_value(b))

def _resolve_operator(op_exp, env):
    """Statically classify an application's operator expression against
    env: ('local', None) for a depth-0 lexical address (a runtime-supplied
    parameter); ('unresolved', None) for anything else that can't be
    resolved without executing (a computed/nested operator such as
    app-aexp/lambda-aexp, an unbound name, a failed frame lookup); or
    ('value', v) when a concrete value is found. Never raises or executes
    anything -- callers apply their own policy (propagate as a compile
    failure, treat as "not provably safe", etc.) on top of the tag."""
    if not isinstance(op_exp, cons):
        return ('unresolved', None)
    tag = op_exp.car
    if tag is symbol_lexical_address_aexp:
        return _resolve_lexical_address(op_exp.cdr.car, op_exp.cdr.cdr.car, env)
    if tag is symbol_var_aexp:
        return _resolve_var(op_exp.cdr.car, env)
    return ('unresolved', None)

def _phase2_safe_walk_call(op_exp, env, visiting):
    """Statically resolve op_exp, if possible, and certify that calling it
    is safe: a known-pure _fast_prim_map primitive (excluding map/
    for-each, whose safety additionally depends on their callback
    argument -- not verified here, so conservatively treated as unsafe),
    or another closure that is itself (transitively) phase2-safe.
    Anything unresolvable at this point -- a local parameter or other
    computed expression used in operator position, an unbound name --
    can't be proven safe, so is treated as unsafe."""
    global _fast_prim_map
    # A literal in operator position has no analog in the JIT compiler
    # (which never accepts one either), so this stays a local special case
    # rather than folding into _resolve_operator's shared contract.
    if isinstance(op_exp, cons) and op_exp.car is symbol_lit_aexp:
        op = op_exp.cdr.car
    else:
        kind, op = _resolve_operator(op_exp, env)
        if kind != 'value':
            return False   # local param, computed operator, or unresolved — can't prove
    if isinstance(op, tuple) and op[0] is symbol_procedure:
        fn = op[1]
        if _fast_prim_map is None:
            _fast_prim_map = _build_fast_prim_map()
        if fn in _fast_prim_map and fn not in _fast_prim_hof_fns:
            return True
        return _is_phase2_safe(op, visiting)
    return False   # dlr proc / host interop / anything else — can't prove

def _extend_direct(env, formals, args_list):
    """Extend environment from a Python list of arg values — no cons list construction."""
    bindings = []
    cache = {}
    vars_cur = formals
    for val in args_list:
        b = cons(val, "")
        bindings.append(b)
        cache[vars_cur.car] = b
        vars_cur = vars_cur.cdr
    frame = cons(Vector(bindings), cons(formals, symbol_emptylist))
    frame._search_cache = cache
    return cons(symbol_environment, cons(frame, env.cdr))

def _eval_direct(exp, env):
    """Direct recursive AST interpreter. Raises _TrampolineFallback for unhandled cases.

    Uses raw cons-cell field access (exp.cdr.car, etc.) rather than a
    helper accessor function per tag -- see the "canonical reference"
    comment block above _is_direct_eval_safe for each tag's layout. This
    function runs on every single AST node evaluated, with no caching at
    all, so an extra Python function call per field access is measurable
    here -- see README-PERFORMANCE.md's Phase 9 addendum."""
    global _fast_prim_map
    while True:
        tag = exp.car
        if tag is symbol_lit_aexp:
            return exp.cdr.car
        elif tag is symbol_lexical_address_aexp:
            d   = exp.cdr.car
            off = exp.cdr.cdr.car
            return binding_value(
                vector_ref(frame_bindings(list_ref(frames(env), d)), off))
        elif tag is symbol_var_aexp:
            b = search_env(env, exp.cdr.car)
            if b is False:
                raise _TrampolineFallback()
            return binding_value(b)
        elif tag is symbol_if_aexp:
            test = _eval_direct(exp.cdr.car, env)
            if test is not False:
                exp = exp.cdr.cdr.car       # tail: loop with then-branch
            else:
                exp = exp.cdr.cdr.cdr.car   # tail: loop with else-branch
        elif tag is symbol_lambda_aexp:
            return closure(exp.cdr.car, exp.cdr.cdr.car, env)
        elif tag is symbol_begin_aexp:
            # Inline sequence: all but last for effect, last is tail (loop, no recursion).
            cur = exp.cdr
            while cur.cdr is not symbol_emptylist:
                _eval_direct(cur.car, env)
                cur = cur.cdr
            exp = cur.car
        elif tag is symbol_app_aexp:
            op   = _eval_direct(exp.cdr.car, env)
            args = []
            cur  = exp.cdr.cdr.car
            while isinstance(cur, cons):
                args.append(_eval_direct(cur.car, env))
                cur = cur.cdr
            # Inline _apply_direct for speed:
            if isinstance(op, tuple) and op[0] is symbol_procedure:
                fn = op[1]
                if _fast_prim_map is None:
                    _fast_prim_map = _build_fast_prim_map()
                direct = _fast_prim_map.get(fn)
                if direct is not None:
                    return direct(args)
                if len(op) == 6 and fn is b_proc_1_d and op[5]:
                    pid = id(op)
                    jit_fn = _jit_lookup(op)
                    if jit_fn is None:
                        _jit_compile_proc(op)
                        jit_fn = _jit_lookup(op)
                    if jit_fn:
                        return jit_fn(*args)
                    # Tail call: reuse this Python frame instead of recursing —
                    # this is what makes deep tail-recursive Scheme loops run in
                    # O(1) Python stack instead of hitting RecursionError.
                    new_env = _extend_direct(op[4], op[3], args)
                    bodies = op[2]
                    while bodies.cdr is not symbol_emptylist:
                        _eval_direct(bodies.car, new_env)
                        bodies = bodies.cdr
                    exp = bodies.car
                    env = new_env
                    continue
                raise _TrampolineFallback()
            elif dlr_proc_q(op):
                return dlr_apply(op, List(*args))
            raise _TrampolineFallback()
        else:
            raise _TrampolineFallback()

## ===== JIT: compile safe Scheme closures to Python functions =====

def _jit_mangle(sym):
    """Scheme name → valid Python identifier (prefixed _j_ to avoid collisions)."""
    s = sym.name if isinstance(sym, Symbol) else str(sym)
    s = (s.replace('->', '_to_').replace('-', '_').replace('?', '_q')
          .replace('!', '_b').replace('+', '_add').replace('*', '_mul')
          .replace('/', '_div').replace('<', '_lt').replace('>', '_gt')
          .replace('=', '_eq'))
    if s and s[0].isdigit():
        s = '_' + s
    return '_j_' + s

_jit_cache = _IdentityCache()   # proc -> compiled_fn or False

def _jit_lookup(proc):
    """Return the cached compiled function for proc, or None if not yet
    compiled OR if a previous compile attempt failed -- a cached failure
    is deliberately reported the same as "not yet attempted" (not a
    permanent verdict), so a call site retries compilation on every call.
    That retry cost is real (see README-PERFORMANCE.md's mutual-recursion
    discussion) but preserved here unchanged from the prior implementation,
    since collapsing it into a permanent failure cache is a behavior change,
    not a refactor."""
    value = _jit_cache.get(proc)
    if value is _CACHE_MISS or value is False:
        return None
    return value

def _jit_compile_proc(proc):
    """Try to JIT-compile a safe Scheme closure.
    Sets _jit_cache[proc] and returns the compiled fn or None."""
    formals, bodies, cenv = proc[3], proc[2], proc[4]
    params = []
    cur = formals
    while isinstance(cur, cons):
        params.append(cur.car.name)
        cur = cur.cdr
    free = {}           # name → value captured into the exec() namespace
    try:
        jc = _JitCompiler(proc, params, cenv, free)
        body_list = []
        cur = bodies
        while isinstance(cur, cons):
            body_list.append(cur.car)
            cur = cur.cdr
        stmt_lines = ['    ' + jc.expr(e) for e in body_list[:-1]]
        tail_lines = jc.tail_stmts(body_list[-1], '    ')
        ps = ', '.join(jc._params)
        lines = ['def _jit_fn(' + ps + '):']
        lines += stmt_lines
        if jc._used_loop:
            lines.append('    while True:')
            lines += ['    ' + ln for ln in tail_lines]
        else:
            lines += tail_lines
        fn_src = '\n'.join(lines)
        ns = dict(free)
        ns['__builtins__'] = __builtins__
        exec(compile(fn_src, '<scheme-jit>', 'exec'), ns)
        fn = ns['_jit_fn']
        _jit_cache.set(proc, fn)
        return fn
    except _TrampolineFallback:
        _jit_cache.set(proc, False)
        return None
    except Exception:
        _jit_cache.set(proc, False)
        return None

def _jit_make_closure(formals, bodies, parent_env, outer_formals, outer_values):
    """Materialize a genuine Scheme closure for a lambda_aexp encountered
    inside JIT'd code, by reconstructing — from live values — the frame the
    closure would have captured had the enclosing function been evaluated
    normally instead of JIT-compiled. Reuses the same frame/closure builders
    the rest of the direct-eval path uses, so the result is a completely
    ordinary Scheme proc tuple."""
    frame_env = _extend_direct(parent_env, outer_formals, outer_values)
    return closure(formals, bodies, frame_env)

def _jit_call(op, args):
    """Call `op` (a Python arg list) whether it's a native Python callable
    (an already-JIT'd function, a wrapped primitive, or a self-reference) or
    a Scheme proc tuple — e.g. one produced by _jit_make_closure, or an
    un-JIT'd closure reached through a computed operator expression such as
    ((make-adder n) 0), an immediately-invoked lambda, or a parameter used
    in operator position (e.g. (define (apply-twice f x) (f (f x))))."""
    if isinstance(op, tuple) and op[0] is symbol_procedure:
        fn = op[1]
        if len(op) == 6 and fn is b_proc_1_d and op[5]:
            # Mirror _eval_direct's app_aexp dispatch: give this proc a
            # chance to JIT-compile instead of always falling to Phase 2
            # via _apply_direct — otherwise a function reached *only*
            # through _jit_call (e.g. always passed as a parameter, never
            # called by name) would never get its own JIT attempt.
            jit_fn = _jit_lookup(op)
            if jit_fn is None:
                _jit_compile_proc(op)
                jit_fn = _jit_lookup(op)
            if jit_fn:
                return jit_fn(*args)
        return _apply_direct(op, args, None)
    return op(*args)

class _JitCompiler:
    """Walks Scheme annotated-AST nodes and emits Python source strings."""

    # Arithmetic: n-ary left-associative (matches Scheme semantics)
    _NARY  = {'+': '+', '-': '-', '*': '*'}
    # Binary comparisons
    _CMP   = {'<': '<', '>': '>', '<=': '<=', '>=': '>=', '=': '=='}
    # Unary — {0} substituted with the single compiled argument string
    _UNARY = {
        'not':   '({0} is False)',
        'zero?': '({0} == 0)',
        'even?': '({0} % 2 == 0)',
        'odd?':  '({0} % 2 != 0)',
        'car':   '({0}).car',
        'cdr':   '({0}).cdr',
        'abs':   'abs({0})',
        'null?': '({0} is _j__empty)',
        'pair?': 'isinstance({0}, _j__cons)',
    }

    def __init__(self, self_proc, params, env, free):
        self._self   = self_proc
        self._params = [_jit_mangle(p) for p in params]
        self._pset   = set(self._params)
        self._env    = env
        self._free   = free
        self._used_loop = False   # set True if a self-recursive tail call was compiled
        self._const_count = 0     # counter for generated constant-capture names

    def expr(self, exp):
        # Raw cons-cell field access throughout -- see the "canonical
        # reference" comment block above _is_direct_eval_safe for each
        # tag's layout. _jit_compile_proc retries this whole walk from
        # scratch on every call for a closure that never successfully
        # compiles (e.g. mutually-recursive functions, see
        # README-PERFORMANCE.md), so any per-field-access overhead here is
        # paid every call, not just once per closure.
        if not isinstance(exp, cons):
            raise _TrampolineFallback()
        tag = exp.car

        if tag is symbol_lit_aexp:
            val = exp.cdr.car
            if isinstance(val, (int, float, bool, str)):
                return repr(val)
            raise _TrampolineFallback()

        elif tag is symbol_lexical_address_aexp:
            # layout: (tag depth offset name info)
            # depth=0 → local parameter; depth>0 → free var in captured env
            depth  = exp.cdr.car
            offset = exp.cdr.cdr.car
            name   = exp.cdr.cdr.cdr.car
            m = _jit_mangle(name)
            if depth == 0:
                return m   # local parameter — already in function signature
            # Outer frame: look up actual value and capture into free.
            # Guarded by `m not in self._free` so an already-captured free
            # variable referenced again later in the same body skips the
            # frame walk entirely, not just the capture.
            if m not in self._free:
                kind, val = _resolve_lexical_address(depth, offset, self._env)
                if kind != 'value':
                    raise _TrampolineFallback()
                if val is self._self:
                    # Self-recursive (non-tail): call the generated function
                    # by its own def name directly, instead of paying for a
                    # wrapper-lambda indirection on every recursive call.
                    return '_jit_fn'
                self._capture(m, val)
            return m

        elif tag is symbol_var_aexp:
            return self._var(exp.cdr.car)

        elif tag is symbol_if_aexp:
            # layout: (tag test then else info)
            test = self.expr(exp.cdr.car)
            then = self.expr(exp.cdr.cdr.car)
            els  = self.expr(exp.cdr.cdr.cdr.car)
            return f'({then} if ({test}) is not False else {els})'

        elif tag is symbol_app_aexp:
            # layout: (tag op args-list info)
            return self._app(exp.cdr.car, exp.cdr.cdr.car)

        elif tag is symbol_lambda_aexp:
            # layout: (tag formals bodies info)
            return self._lambda(exp.cdr.car, exp.cdr.cdr.car)

        else:
            raise _TrampolineFallback()

    def _capture_const(self, val):
        """Stash an arbitrary Scheme object (AST fragment, environment, ...)
        into the exec() namespace under a fresh name. Distinct '_jc_const_'
        prefix so it can never collide with a _jit_mangle()'d Scheme name."""
        name = '_jc_const_%d' % self._const_count
        self._const_count += 1
        self._free[name] = val
        return name

    def _lambda(self, formals, bodies):
        """Compile a lambda_aexp encountered as a value (e.g. a function
        whose body directly returns a closure, like
        (define (make-adder k) (lambda (x) (+ x k)))).

        JIT'd code represents this function's own parameters as plain
        Python locals, but a real Scheme closure needs a proper environment
        frame (so the classic interpreter, or a future JIT of the inner
        lambda, can look it up by lexical address). So instead of compiling
        the inner lambda's body inline, reconstruct — at runtime — the exact
        frame this function's own parameters would occupy under normal
        (non-JIT) evaluation, chain it onto the real captured environment
        (self._env), and hand the result to the ordinary closure()
        constructor. The inner lambda is then a completely normal Scheme
        proc, independently eligible for its own JIT compilation later.

        Only safe when this function's own formals are a plain (non-variadic)
        list matching self._params 1:1 — _jit_make_closure rebuilds the frame
        positionally from self._params's live values, so a rest parameter
        (silently dropped from self._params — see _jit_compile_proc) would
        misalign the frame.
        """
        cur = self._self[3]
        count = 0
        while isinstance(cur, cons):
            count += 1
            cur = cur.cdr
        if cur is not symbol_emptylist or count != len(self._params):
            raise _TrampolineFallback()
        self._free['_jit_make_closure'] = _jit_make_closure
        fm = self._capture_const(formals)
        bd = self._capture_const(bodies)
        ce = self._capture_const(self._env)
        of = self._capture_const(self._self[3])
        values = ", ".join(self._params)
        return f'_jit_make_closure({fm}, {bd}, {ce}, {of}, [{values}])'

    def _sym_name(self, op_exp):
        """Extract the Scheme symbol name from a lex-addr or var operator node."""
        if not isinstance(op_exp, cons):
            return None
        if op_exp.car is symbol_lexical_address_aexp:
            s = op_exp.cdr.cdr.cdr.car
            return s.name if isinstance(s, Symbol) else None
        if op_exp.car is symbol_var_aexp:
            s = op_exp.cdr.car
            return s.name if isinstance(s, Symbol) else None
        return None

    def _app(self, op_exp, arg_list):
        # Raw while-loop, not _iter_aexp_list -- see the note on expr()
        # above; a generator's per-element suspend/resume cost is also
        # measurable on this same retried-every-call path.
        args = []
        cur = arg_list
        while isinstance(cur, cons):
            args.append(self.expr(cur.car))
            cur = cur.cdr
        n   = len(args)
        sym = self._sym_name(op_exp)

        if sym:
            # n-ary arithmetic (left-assoc)
            if sym in self._NARY:
                op = self._NARY[sym]
                if n == 0:
                    return '0' if sym == '+' else '1'
                if n == 1 and sym == '-':
                    return f'(-{args[0]})'
                return '(' + f' {op} '.join(args) + ')'
            # binary comparisons
            if sym in self._CMP and n == 2:
                return f'({args[0]} {self._CMP[sym]} {args[1]})'
            # unary with inline template
            if sym in self._UNARY and n == 1:
                if sym == 'null?':
                    self._free['_j__empty'] = symbol_emptylist
                elif sym == 'pair?':
                    self._free['_j__cons'] = cons
                return self._UNARY[sym].format(args[0])

        # Operator shapes that can't be proven, at compile time, to yield a
        # plain Python callable:
        #  - a local parameter (depth=0) — its value is whatever the caller
        #    passed: could be a native callable or a Scheme proc tuple
        #    (e.g. (define (apply-twice f x) (f (f x))))
        #  - a nested call, e.g. ((make-adder n) 0)
        #  - an immediately-invoked lambda (how `let`/`or`/`and` commonly
        #    desugar), e.g. ((lambda (x) ...) e)
        # self.expr() on any of these can yield a Scheme proc *tuple* (via
        # _jit_make_closure, or simply an un-JIT'd proc passed through as an
        # argument), not a plain Python callable, so a bare op_src(...) call
        # could try to call a tuple. Dispatch through a runtime helper that
        # handles both.
        if isinstance(op_exp, cons) and (
                op_exp.car in (symbol_app_aexp, symbol_lambda_aexp) or
                (op_exp.car is symbol_lexical_address_aexp and op_exp.cdr.car == 0)):
            op_src = self.expr(op_exp)
            self._free['_jit_call'] = _jit_call
            return f'_jit_call({op_src}, [{", ".join(args)}])'

        # General call: compile operator as expression and emit a call
        op_src = self.expr(op_exp)
        return f'{op_src}({", ".join(args)})'

    def _var(self, sym):
        """Handle var-aexp: local param or captured free variable."""
        m = _jit_mangle(sym)
        if m in self._pset or m in self._free:
            return m
        kind, val = _resolve_var(sym, self._env)
        if kind != 'value':
            raise _TrampolineFallback()
        if val is self._self:
            # Self-recursive (non-tail): call the generated function by its
            # own def name directly — see the lexical_address_aexp case above.
            return '_jit_fn'
        self._capture(m, val)
        return m

    def _capture(self, m, val):
        """Add a free-variable value to self._free, or raise _TrampolineFallback."""
        if isinstance(val, tuple) and val[0] is symbol_procedure:
            pid = id(val)
            jit_fn = _jit_lookup(val)
            if jit_fn is not None:
                self._free[m] = jit_fn
                return
            # Wrap a fast_prim_map entry (list interface → positional interface)
            if _fast_prim_map is not None:
                direct = _fast_prim_map.get(val[1])
                if direct is not None:
                    self._free[m] = lambda *a, _d=direct: _d(list(a))
                    return
            raise _TrampolineFallback()
        # Plain value (number, string, bool…) — capture directly
        if isinstance(val, (int, float, bool, str)):
            self._free[m] = val
            return
        raise _TrampolineFallback()

    def _is_self_ref(self, op_exp):
        """True iff op_exp resolves (by identity) to the proc being compiled —
        i.e. this application is a self-recursive call."""
        kind, val = _resolve_operator(op_exp, self._env)
        return kind == 'value' and val is self._self

    def tail_stmts(self, exp, indent):
        """Compile exp in tail position to a list of already-indented Python
        statement lines. A self-recursive tail call becomes a parameter
        reassignment + `continue` instead of a return/call, so the enclosing
        `while True:` loop (see _jit_compile_proc) reuses the same Python
        frame across arbitrarily many tail iterations."""
        if not isinstance(exp, cons):
            raise _TrampolineFallback()
        tag = exp.car
        if tag is symbol_if_aexp:
            test = self.expr(exp.cdr.car)
            lines = [indent + f'if ({test}) is not False:']
            lines += self.tail_stmts(exp.cdr.cdr.car, indent + '    ')
            lines.append(indent + 'else:')
            lines += self.tail_stmts(exp.cdr.cdr.cdr.car, indent + '    ')
            return lines
        if tag is symbol_app_aexp and self._is_self_ref(exp.cdr.car):
            arg_srcs = []
            cur = exp.cdr.cdr.car
            while isinstance(cur, cons):
                arg_srcs.append(self.expr(cur.car))
                cur = cur.cdr
            if len(arg_srcs) != len(self._params):
                raise _TrampolineFallback()   # arity mismatch — be safe, don't JIT
            self._used_loop = True
            if not self._params:
                return [indent + 'continue']
            targets = ', '.join(self._params)
            values  = ', '.join(arg_srcs)
            return [indent + f'{targets} = {values}', indent + 'continue']
        return [indent + 'return ' + self.expr(exp)]

## Fast prim map: proc[1] function -> Python callable.
## Built lazily on first use from the live toplevel environment.
_fast_prim_map = None

## Subset of _fast_prim_map whose safety depends on a caller-supplied
## callback argument (map, for-each), not just the primitive's own
## identity. _is_phase2_safe treats these as unsafe rather than trying to
## also verify the callback argument -- see its docstring.
_fast_prim_hof_fns = set()

def _fast_prim_direct_map(args):
    f, lst = args[0], args[1]
    items = []
    while isinstance(lst, cons):
        items.append(_apply_direct(f, [lst.car], None))
        lst = lst.cdr
    out = symbol_emptylist
    for v in reversed(items):
        out = cons(v, out)
    return out

def _fast_prim_direct_for_each(args):
    f, lst = args[0], args[1]
    while isinstance(lst, cons):
        _apply_direct(f, [lst.car], None)
        lst = lst.cdr
    return void_value

def _fast_prim_set_car(args):
    args[0].car = args[1]
    return void_value

def _fast_prim_set_cdr(args):
    args[0].cdr = args[1]
    return void_value

## Source of truth for Phase 2's fast primitive dispatch: Scheme name ->
## a Python callable taking a plain Python list of args. Module-level (not
## built lazily inside _build_fast_prim_map) so it can be inspected/tested
## directly -- e.g. to check that _JitCompiler's _NARY/_CMP/_UNARY inlining
## templates (which cover a syntactic subset of these) never reference a
## name that isn't also fast-dispatchable here.
_FAST_PRIM_SPECS = {
    # Arithmetic
    '+':     lambda args: plus(*args),
    '-':     lambda args: minus(*args),
    '*':     lambda args: multiply(*args),
    '/':     lambda args: divide(*args),
    # Numeric comparisons
    '<':     lambda args: LessThan(*args),
    '>':     lambda args: GreaterThan(*args),
    '<=':    lambda args: LessThanEqual(*args),
    '>=':    lambda args: GreaterThanEqual(*args),
    '=':     lambda args: numeric_equal(*args),
    # Logic
    'not':   lambda args: (args[0] is False),
    # Numeric predicates / math
    'zero?':     lambda args: (args[0] == 0),
    'even?':     lambda args: even_q(args[0]),
    'odd?':      lambda args: odd_q(args[0]),
    'abs':       lambda args: abs(args[0]),
    'min':       lambda args: min(*args),
    'max':       lambda args: max(*args),
    'modulo':    lambda args: modulo(args[0], args[1]),
    'remainder': lambda args: remainder(args[0], args[1]),
    'quotient':  lambda args: quotient(args[0], args[1]),
    'expt':      lambda args: expt_native(args[0], args[1]),
    'sqrt':      lambda args: sqrt(args[0]),
    'round':     lambda args: round(args[0]),
    # Type predicates
    'null?':      lambda args: (args[0] is symbol_emptylist),
    'pair?':      lambda args: pair_q(args[0]),
    'number?':    lambda args: number_q(args[0]),
    'string?':    lambda args: string_q(args[0]),
    'symbol?':    lambda args: symbol_q(args[0]),
    'char?':      lambda args: char_q(args[0]),
    'boolean?':   lambda args: boolean_q(args[0]),
    'vector?':    lambda args: vector_q(args[0]),
    'list?':      lambda args: list_q(args[0]),
    'procedure?': lambda args: procedure_q(args[0]),
    # Equality
    'eq?':    lambda args: eq_q(args[0], args[1]),
    'eqv?':   lambda args: eqv_q(args[0], args[1]),
    'equal?': lambda args: equal_q(args[0], args[1]),
    # Pair / list construction and access
    'car':    lambda args: args[0].car,
    'cdr':    lambda args: args[0].cdr,
    'cons':   lambda args: cons(args[0], args[1]),
    'set-car!': _fast_prim_set_car,
    'set-cdr!': _fast_prim_set_cdr,
    'caar':   lambda args: args[0].car.car,
    'cadr':   lambda args: args[0].cdr.car,
    'cdar':   lambda args: args[0].car.cdr,
    'cddr':   lambda args: args[0].cdr.cdr,
    'cadar':  lambda args: args[0].car.cdr.car,
    'caddr':  lambda args: args[0].cdr.cdr.car,
    'cdddr':  lambda args: args[0].cdr.cdr.cdr,
    'cadddr': lambda args: args[0].cdr.cdr.cdr.car,
    # List operations
    'list':      lambda args: List(*args),
    'length':    lambda args: length(args[0]),
    'reverse':   lambda args: reverse(args[0]),
    'append':    lambda args: append(*args),
    'list-ref':  lambda args: list_ref(args[0], args[1]),
    # Search
    'memq':   lambda args: memq(args[0], args[1]),
    'memv':   lambda args: memv(args[0], args[1]),
    'member': lambda args: member(args[0], args[1]),
    'assq':   lambda args: assq(args[0], args[1]),
    'assv':   lambda args: assv(args[0], args[1]),
    # Vector operations
    'make-vector':   lambda args: make_vector(args[0]),
    'vector-ref':    lambda args: vector_ref(args[0], args[1]),
    'vector-set!':   lambda args: (vector_set_b(args[0], args[1], args[2]), void_value)[1],
    'vector-length': lambda args: vector_length(args[0]),
    'vector->list':  lambda args: vector_to_list(args[0]),
    'list->vector':  lambda args: list_to_vector(args[0]),
    # String operations
    'string-length':  lambda args: string_length(args[0]),
    'string-ref':     lambda args: string_ref(args[0], args[1]),
    'string-append':  lambda args: string_append(*args),
    'substring':      lambda args: substring(args[0], args[1], args[2]),
    'string->list':   lambda args: string_to_list(args[0]),
    'list->string':   lambda args: list_to_string(args[0]),
    'string->number': lambda args: string_to_number(args[0]),
    'number->string': lambda args: number_to_string(args[0]),
    'string=?':       lambda args: string_is__q(args[0], args[1]),
    'string<?':       lambda args: stringLessThan_q(args[0], args[1]),
    # Symbol / char operations
    'symbol->string':  lambda args: symbol_to_string(args[0]),
    'string->symbol':  lambda args: string_to_symbol(args[0]),
    'char->integer':   lambda args: char_to_integer(args[0]),
    'integer->char':   lambda args: integer_to_char(args[0]),
    'char-alphabetic?': lambda args: char_alphabetic_q(args[0]),
    'char-numeric?':    lambda args: char_numeric_q(args[0]),
    'char-whitespace?': lambda args: char_whitespace_q(args[0]),
    # Higher-order (propagate _TrampolineFallback if proc not directly evaluable)
    'map':      _fast_prim_direct_map,
    'for-each': _fast_prim_direct_for_each,
}

def _build_fast_prim_map():
    """Populate the fast prim map by resolving known symbol names in toplevel_env."""
    result = {}
    for sym_name, direct_fn in _FAST_PRIM_SPECS.items():
        b = search_env(toplevel_env, make_symbol(sym_name))
        if b is not False:
            proc = binding_value(b)
            if isinstance(proc, tuple) and proc[0] is symbol_procedure:
                result[proc[1]] = direct_fn
                if sym_name in ('map', 'for-each'):
                    _fast_prim_hof_fns.add(proc[1])
    return result

def _apply_direct(proc, args, env):
    global _fast_prim_map
    if dlr_proc_q(proc):
        return dlr_apply(proc, List(*args))
    if isinstance(proc, tuple) and proc[0] is symbol_procedure:
        fn = proc[1]
        if _fast_prim_map is None:
            _fast_prim_map = _build_fast_prim_map()
        direct = _fast_prim_map.get(fn)
        if direct is not None:
            return direct(args)      # args is a Python list — no cons needed
        if len(proc) == 6 and fn is b_proc_1_d and proc[5]:
            bodies, formals, cenv = proc[2], proc[3], proc[4]
            new_env = extend(cenv, formals, List(*args), make_empty_docstrings(len(args)))
            return _eval_sequence_direct(bodies, new_env)
    raise _TrampolineFallback()

def _eval_sequence_direct(bodies, env):
    cur = bodies
    while True:
        exp = cur.car
        if cur.cdr is symbol_emptylist:
            return _eval_direct(exp, env)   # tail expression: TCO via _eval_direct loop
        _eval_direct(exp, env)              # intermediate expr: eval for side effects
        cur = cur.cdr

### Native b_proc_1_d and closure (direct-eval-aware):

def b_proc_1_d(bodies, formals, env, _safe=False):
    # _safe is used by apply_proc fast path, ignored here (trampoline path)
    formals_and_args = process_formals_and_args(formals, args_reg, info_reg, handler_reg, fail_reg)
    new_formals = (formals_and_args).car
    new_args = (formals_and_args).cdr
    if (numeric_equal(length(new_args), length(new_formals)) is not False):
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['env_reg'] = extend(env, new_formals, new_args, make_empty_docstrings(length(new_args)))
        GLOBALS['exps_reg'] = bodies
        GLOBALS['pc'] = eval_sequence
    else:
        GLOBALS['msg_reg'] = "incorrect number of arguments in application"
        GLOBALS['pc'] = runtime_error

def closure(formals, bodies, env):
    safe = _is_direct_eval_safe(bodies)
    return make_proc(b_proc_1_d, bodies, formals, env, safe)

### Native frame functions (dict-cached for O(1) variable lookup):

def make_frame(variables, values, docstrings):
    # Single pass: build bindings list and search cache simultaneously
    bindings = []
    cache = {}
    vars_cur, vals_cur, docs_cur = variables, values, docstrings
    while isinstance(vars_cur, cons):
        b = cons(vals_cur.car, docs_cur.car)  # make_binding inlined
        bindings.append(b)
        cache[vars_cur.car] = b
        vars_cur = vars_cur.cdr
        vals_cur = vals_cur.cdr
        docs_cur = docs_cur.cdr
    bindings_vector = Vector(bindings)
    frame = List(bindings_vector, variables)
    frame._search_cache = cache
    return frame

def add_binding(new_var, new_binding, frame):
    vars = (frame).cdr.car
    old_bindings = (frame).car  # Vector
    new_bindings = Vector(list(old_bindings) + [new_binding])
    new_frame = List(new_bindings, append(vars, List(new_var)))
    cache = getattr(frame, '_search_cache', None)
    if cache is not None:
        new_cache = dict(cache)
        new_cache[new_var] = new_binding
        new_frame._search_cache = new_cache
    return new_frame

def continuation_object_q(x):
    return (isinstance(x, tuple) and len(x) > 0 and
            x[0] in (symbol_continuation, symbol_continuation2,
                     symbol_continuation3, symbol_continuation4))

_empty_docstrings_cache = {}
def make_empty_docstrings(n):
    if n not in _empty_docstrings_cache:
        result = symbol_emptylist
        for _ in range(n):
            result = cons("", result)
        _empty_docstrings_cache[n] = result
    return _empty_docstrings_cache[n]

def process_args(args, params, info, handler, fail):
    return args

def process_formals(params, info, handler, fail):
    # Fast path: if all params are plain Symbols, return unchanged (avoids Map allocation)
    cur = params
    while isinstance(cur, cons):
        if not isinstance(cur.car, Symbol):
            return Map(get_symbol, params)
        cur = cur.cdr
    return params

def process_formals_and_args(params, args, info, handler, fail):
    return cons(process_formals(params, info, handler, fail), args)

### Native other functions:

def positive_q(value):
    return 0 < value

def length_one_q(ls):
    return isinstance(ls, cons) and (ls.cdr is symbol_emptylist)

def length_two_q(ls):
    return (isinstance(ls, cons) and
            isinstance(ls.cdr, cons) and
            (ls.cdr.cdr is symbol_emptylist))

def length_at_least_q(n, ls):
    count = 0
    current = ls
    while count < n:
        if not pair_q(current):
            return False
        current = current.cdr
        count += 1
    return True

def all_numeric_q(ls):
    for item in ls:
        if not number_q(item):
            return False
    return True

def list_native(iterable):
    return list(iterable)

def expt_native(base, power):
    return math.pow(base, power)

### Questions:

def even_q(n):
    return n % 2 == 0

def odd_q(n):
    return n % 2 == 1

def eq_q(o1, o2):
    if (isinstance(o1, float) and isinstance(o2, float)):
        return o1 == o2
    elif (isinstance(o1, int) and isinstance(o2, int)):
        return o1 == o2
    return o1 is o2

def char_q(item):
    return isinstance(item, Char)

def string_q(item):
    if PY3:
        return isinstance(item, str)
    else:
        return isinstance(item, (str, unicode))

def char_whitespace_q(c):
    return c.char in [' ', '\t', '\n', '\r']

def char_alphabetic_q(c):
    return str.isalpha(c.char)

def char_numeric_q(c):
    return str.isdigit(c.char)

def char_is__q(c1, c2):
    return c1 == c2

def number_q(item):
    return isinstance(item, (int, float, fractions.Fraction, complex))

def integer_q(item):
    return isinstance(item, int)

def null_q(item):
    return item is symbol_emptylist

def boolean_q(item):
    return isinstance(item, bool)

def true_q(item):
    return False if (item is False) else True

def list_q(item):
    ## return proper_list?
    current = item
    while isinstance(current, cons):
        current = current.cdr
    return current is symbol_emptylist

def procedure_q(item):
    return isinstance(item, tuple) and len(item) > 0 and item[0] is symbol_procedure

def symbol_q(item):
    return ((isinstance(item, Symbol) or association_q(item))
            and (not (null_q(item))))

def vector_q(item):
    return isinstance(item, list)

def pair_q(item):
    return isinstance(item, cons)

def iterator_q(item):
    # return true if an iter that implementation doesn't
    # know how to handle. Python knows how to handle all
    # of the iters, but IronPython can import other
    # things.
    return hasattr(item, "MoveNext")

def get_iterator(generator):
    # Not used in Python version
    return iter(generator)

def get_type(obj):
    return type(obj)

### Math and applications:

def fraction_repr(self):
    if self.denominator == 1:
        return str(self.numerator)
    return "%s/%s" % (self.numerator, self.denominator)

fractions.Fraction.__repr__ = fraction_repr
fractions.Fraction.__str__ = fraction_repr

def modulo(a, b):
    return a % b

def quotient(a, b):
    return int(a / b)

def remainder(a, b):
    return a % b

def sqrt(number):
    return math.sqrt(number)

def plus(*args):
    return functools.reduce(operator.add, args, 0)

def minus(*args):
    if len(args) == 0:
        return 0
    elif len(args) == 1:
        return -args[0]
    else:
        return functools.reduce(operator.sub, args[1:], args[0])

def multiply(*args):
    return functools.reduce(operator.mul, args, 1)

def divide(*args):
    try:
        if len(args) == 0:
            return 1
        elif len(args) == 1:
            return fractions.Fraction(1, args[0])
        else:
            current = fractions.Fraction(args[0], args[1])
            for arg in args[2:]:
                current = fractions.Fraction(current, arg)
            return current
    except:
        return functools.reduce(operator.truediv, args)

def numeric_equal(o1, o2):
    return o1 == o2

def equal_q(o1, o2):
    if boolean_q(o1) or boolean_q(o2):
        return boolean_q(o1) and boolean_q(o2) and o1 is o2
    return o1 == o2

def LessThan(a, b):
    return a < b

def LessThanEqual(a, b):
    return a <= b

def GreaterThanEqual(a, b):
    return a >= b

def GreaterThan(a, b):
    return a > b

def memq(item, lyst):
    current = lyst
    while isinstance(current, cons):
        if current.car is item:
            return current
        current = current.cdr
    return False

### Converters:

def char_to_integer(c):
    return ord(c.char)

def integer_to_char(i):
    return make_char(chr(i))

def number_to_string(number):
    return str(number)

def string_to_integer(s):
    return int(s)

def string_to_symbol(string):
    return make_symbol(string)

def list_to_string(lyst):
    # only on list of chars
    retval = io.StringIO()
    current = lyst
    while isinstance(current, cons):
        retval.write(current.car.char)
        current = current.cdr
    return retval.getvalue()

def list_to_vector(lyst):
    # this works because cons implements iter
    return Vector(lyst)

def vector_to_list(vector):
    return List(*vector)

def vector_ref(vector, position):
    return vector[position]

def char_to_string(c):
    return c.char

def string_to_list(st):
    return List(*[make_char(c) for c in st])

def symbol_to_string(symbol):
    return symbol.name

def string_to_decimal(s):
    return float(s)

def string_to_rational(s):
    try:
        return fractions.Fraction(s)
    except:
        return False

def string_to_number(s):
    if "/" in s:
        return string_to_rational(s)
    elif "." in s:
        return string_to_decimal(s)
    else:
        return string_to_integer(s)

def truncate_to_integer(number):
    return int(number)

### Strings:

def string_append(*args):
    return "".join([str(arg) for arg in args])

def string_ref(string, pos):
    return make_char(string[pos])

def string(*chars):
    retval = io.StringIO()
    for c in chars:
        if isinstance(c, Char):
            retval.write(c.char)
        else:
            raise Exception("invalid argument to string: '%s' is not a character" % c)
    return retval.getvalue()

def string_split(string, delim):
    return List(*string.split(delim.char))

def member(item, lyst):
    current = lyst
    while isinstance(current, cons):
        if item == current.car:
            return True
        current = current.cdr
    return False

def string_is__q(s1, s2):
    return s1 == s2

def string_length(s):
    return len(s)

def stringLessThan_q(s1, s2):
    return s1 < s2

def substring(s, start, stop):
    return s[start:stop]

### Functions:

def Apply(f, lyst):
    if lyst is symbol_emptylist:
        return f()
    else:
        return f(*list_to_vector(lyst))

### Annotated expression support:

def list_q_or_length_hat(lyst):
    current = lyst
    count = 0
    while not null_q_hat(current):
        if not pair_q_hat(current):
            return False
        count += 1
        current = cdr_hat(current)
    return count

def tagged_list_hat(keyword, op, length):
    def tagged_list(asexp):
        len_q = list_q_or_length_hat(asexp)
        car_hat_item = car_hat(asexp)
        return (len_q is not False and
                op(len_q, length) and
                symbol_q_hat(car_hat_item) and
                eq_q_hat(car_hat_item, keyword))
    return tagged_list

def tagged_list_or_hat(keyword1, keyword2, op, length):
    def tagged_list(asexp):
        len_q = list_q_or_length_hat(asexp)
        car_hat_item = car_hat(asexp)
        return (len_q is not False and
                op(len_q, length) and
                symbol_q_hat(car_hat_item) and
                (eq_q_hat(car_hat_item, keyword1) or
                 eq_q_hat(car_hat_item, keyword2)))
    return tagged_list

def tagged2_list_hat(keyword, op, length):
    def tagged2_list(asexp):
        len_q = list_q_or_length_hat(asexp)
        return (len_q is not False and
                op(len_q, length) and
                symbol_q_hat(car_hat(asexp)) and
                eq_q_hat(cadr_hat(asexp), keyword))
    return tagged2_list

### Misc:

def error(function, message):
    raise Exception("Exception in %s: %s" % (function, message))

def display(item):
    print(item, end="")

def printf(formatting, *items):
    print(format(formatting, *items), end="")

def newline():
    print()

def trampoline():
    global pc, exception_reg
    if ENVIRONMENT.get("DEBUG", False) == True:
        while pc:
            pc()
    else:
        while pc:
            try:
                pc()
            except DebugException:
                raise
            except KeyboardInterrupt:
                exception_reg = make_exception("KeyboardInterrupt", "Keyboard interrupt", symbol_none, symbol_none, symbol_none)
                pc = apply_handler2
            except Exception as e:
                exception_reg = make_exception("Unhandled %s" % e.__class__.__name__, str(e), symbol_none, symbol_none, symbol_none)
                pc = apply_handler2
    return final_reg

class Box():
    """
    A Box object.
    """
    def __init__(self, item):
        self.item = item

    def __str__(self):
        return "#&%s" % self.item

    def __repr__(self):
        return "#&%s" % self.item

def box_q(item):
    return isinstance(item, Box)

def box(item):
    return Box(item)

def unbox(item):
    return item.item

def ready_to_eval(text):
    if yasi:
        data = yasi.indent_code(text + "\n(", opts) ## where does next expression go?
        if data["indented_code"][-1] == "(":
            return (True, "")
        else:
            return (False, "... " + data["indented_code"][-1][:-1])
    elif text:
        lines = text.split("\n")
        if len(lines) > 0 and lines[-1].strip() == "":
            return (True, "") ## force it
        ## else, only if valid parse
        return (try_parse(text), "... ")
    return (True, "")

# native:
def read_multiline(prompt):
    retval = io.StringIO()
    while True:
        try:
            if retval.tell() != 0:
                retval.write("\n")
            if PY3:
                retval.write(input(prompt)) ## Python 3
            else:
                retval.write(raw_input(prompt)) ## Python 2
        except EOFError:
            return "(exit)"
        except:
            return ""
        s = retval.getvalue()
        ready, prompt = ready_to_eval(s)
        if ready:
            return s
        retval.write(" ")

def format(formatting, *lyst):
    args = list_to_vector(lyst)
    retval = io.StringIO()
    i = 0
    count = 0
    while i < len(formatting):
        if formatting[i] == '\\':
            i += 1
        elif formatting[i] == "~":
            if formatting[i+1] == 's' and count < len(args):
                i += 1
                retval.write(make_safe(args[count]))
                count += 1
            elif formatting[i+1] == 'a' and count < len(args):
                i += 1
                retval.write(str(args[count]))
                count += 1
            elif formatting[i+1] == '%':
                i += 1
                retval.write("\n")
            else:
                retval.write(formatting[i]) # unknown ~X
        else:
            retval.write(formatting[i])
        i += 1
    return retval.getvalue()

def pretty_print(thing):
    print(thing)

def make_safe(item):
    if procedure_q(item):
        return "#<procedure>"
    elif environment_q(item):
        return "#<environment>"
    elif string_q(item):
        # Unlike Python, Scheme's strings must start with "
        return '"%s"' % item.replace('"', '\\"')
    elif boolean_q(item):
        return "#t" if item else "#f"
    else:
        return repr(item)

def search_frame(frame, variable):
    if isinstance(frame, cons):
        cache = getattr(frame, '_search_cache', None)
        if cache is not None:
            return cache.get(variable, False)
        bindings = frame.car
        variables = frame.cdr.car
        i = 0
        while not null_q(variables):
            if eq_q(variables.car, variable):
                return bindings[i]
            variables = variables.cdr
            i += 1
        return False
    else:
        raise Exception("invalid frame")

def read_content(filename):
    return open(filename).read()

def file_exists_q(path):
    return os.path.isfile(path)

def get_current_time():
    return time.time()

def current_directory(*path):
    if len(path) == 1:
        path = os.path.expanduser(path[0])
        os.chdir(path)
    return os.getcwd()

def Range(*args):
    return List(*range(*args))

def assv(x, ls):
    while isinstance(ls, cons):
        if x is ls.car.car:
            return ls.car
        ls = ls.cdr
    return False

def memv(item, ls):
    current = ls
    while isinstance(current, cons):
        if (eqv_q(item, current.car)):
            return current
        current = current.cdr
    return False

def make_vector(size):
    return Vector([0] * size)

class Vector(list):
    def __repr__(self):
        return "#%d(%s)" % (len(self), " ".join(map(repr, self)))

def vector_native(*ls):
    return Vector(ls)

def vector_set_b(vec, pos, value):
    vec[pos] = value

def eqv_q(a, b):
    if number_q(a) and number_q(b):
        # but be same type, and value
        return type(a) == type(b) and a == b
    elif char_q(a) and char_q(b):
        return a.char == b.char
    else:
        return eq_q(a, b)

def atom_q(item):
    return number_q(item) or symbol_q(item) or string_q(item)

def iter_q(item):
    return isinstance(item, Iterable)

def assq(x, ls):
    while not null_q(ls):
        if eq_q(x, ls.car.car):
            return ls.car
        ls = ls.cdr
    return False

### External env interface:

def import_native(libraries, environment):
    env = {}
    for library in libraries:
        if PY3:
            exec("import %s" % library, env)
        else:
            exec ("import %s" % library) in env
    ENVIRONMENT.update(env)
    return List(*[make_symbol(name) for name in env.keys() if not name.startswith("_")])

def import_as_native(library, name, environment):
    env = {}
    if name == make_symbol("*") or name == "*":
        if PY3:
            exec("from %s import *" % library, env)
        else:
            exec ("from %s import *" % library) in env
    else:
        if PY3:
            exec("import %s as %s" % (library, name), env)
        else:
            exec ("import %s as %s" % (library, name)) in env
    ENVIRONMENT.update(env)
    return List(*[make_symbol(name) for name in env.keys() if not name.startswith("_")])

def import_from_native(library, name_list, environment):
    env = {}
    if PY3:
        exec("from %s import %s" % (library, ", ".join([str(name) for name in name_list])), env)
    else:
        exec ("from %s import %s" % (library, ", ".join([str(name) for name in name_list]))) in env
    ENVIRONMENT.update(env)
    return List(*[make_symbol(name) for name in env.keys() if not name.startswith("_")])

def dlr_proc_q(item):
    return (callable(item) and not isinstance(item, cons)) or hasattr(item, "MoveNext")

def dlr_env_contains(item):
    return item.name in ENVIRONMENT

def set_global_value_b(variable, value):
    ENVIRONMENT[variable.name] = value

def dlr_env_lookup(variable):
    if variable.name in ENVIRONMENT:
        return ENVIRONMENT[variable.name]
    else:
        raise Exception("no such global variable: '%s'" % variable.name)

def dlr_object_contains(obj, components):
    # components: (math sqrt)
    retval = obj
    for component in components.cdr:
        if hasattr(retval, component.name):
            retval = getattr(retval, component.name)
        else:
            return False
    return True

def get_external_member(obj, components):
    # components: (math sqrt)
    retval = obj
    for component in components.cdr:
        if hasattr(retval, component.name):
            retval = getattr(retval, component.name)
        else:
            return void_value
    return retval

def dlr_apply(f, args):
    largs = list_to_vector(args)
    fargs = []
    fkwargs = {}

    # args can be: (), (1, 2, 3), (dict, dict), (dict)
    # if last is a dict, use it for kwargs

    if len(largs) > 0:
        if isinstance(largs[-1], dict):
            fkwargs = largs[-1]
            fargs = largs[:-1]
        else:
            fargs = largs

    return f(*fargs, **fkwargs)

def dlr_func(schemeProc):
    def f(*args):
        GLOBALS["proc_reg"] = schemeProc
        GLOBALS["args_reg"] = List(*args)
        GLOBALS["handler_reg"] = REP_handler
        GLOBALS["k2_reg"] = REP_k
        GLOBALS["pc"] = apply_proc
        return trampoline()
    return f

def set_global_docstring_b(variable, docstring):
    pass

def get_external_members(obj):
    return List(*[make_symbol(x) for x in  dir(obj)])

def callback(schemeProc):
    def cb(*args):
        GLOBALS["proc_reg"] = schemeProc
        GLOBALS["args_reg"] = List(*args)
        GLOBALS["handler_reg"] = REP_handler
        GLOBALS["k2_reg"] = REP_k
        GLOBALS["pc"] = apply_proc
        return trampoline()
    return cb

def set_external_member_b(obj, components, value):
    for component in components[1:-1]:
        obj = getattr(obj, component.name)
    setattr(obj, components[-1].name, value)

def apply_star(external_function, args):
    return external_function(*args)

def next_item(iter_item):
    try:
        return next(iter_item)
    except StopIteration:
        return symbol_emptylist

def load_native(filename):
    result = execute_rm('(load "%s")' % filename, "stdin")
    if true_q(exception_q(result)):
        handle_exception(result)
        return False # continue?
    return True # continue?

## for dict or indexed-based objects:

def getitem_native(thing, item):
    ## FIXME:
    ## if environment_q(thing):
    if isinstance(thing, dict):
        if symbol_q(item):
            item = symbol_to_string(item)
        return thing[item]
    else:
        return thing.__getitem__(item)

def setitem_native(thing, item, value):
    if isinstance(thing, dict):
        if symbol_q(item):
            item = symbol_to_string(item)
        thing[item] = value
    else:
        thing.__setitem__(item, value)

def hasitem_native(thing, item):
    if isinstance(thing, dict):
        if symbol_q(item):
            item = symbol_to_string(item)
        return item in thing
    else:
        try:
            thing.__getitem__(item)
            return True
        except:
            return False

def getattr_native(thing, item):
    return getattr(thing, item)

def setattr_native(thing, item, value):
    setattr(thing, item, value)

def hasattr_native(thing, item):
    return hasattr(thing, item)

def dict_to_keys(dictionary):
    return vector_to_list(list(dictionary.keys()))

def dict_to_values(dictionary):
    return vector_to_list(list(dictionary.values()))

def vlist():
    return list()

def vlist_append_native(vec, item):
    vec.append(item)

def vlist_length_native(vec):
    return len(vec)

def vlist_ref_native(vec, index):
    return vec[index]

def python_eval(arg):
    return eval(arg, ENVIRONMENT)

def python_exec(arg):
    if PY3:
        exec(arg, ENVIRONMENT)
    else:
        exec (arg) in ENVIRONMENT

def highlight_expression(exp):
    info = symbol_undefined
    info = rac(exp)
    if true_q(not((info) is (symbol_none))):
        if GLOBALS.get("TRACE_GUI", False):
            GLOBALS["TRACE_GUI_COUNT"] += 1
            if GLOBALS["TRACE_GUI_COUNT"] % 2 == 1:
                raise DebugException([get_start_line(info), get_start_char(info), get_end_line(info), get_end_char(info)])
        else:
            printf("call: ~s~%", aunparse(exp))

def string_startswith_q(string, s):
    return string.startswith(s)

def host_environment_native():
    return "python"

def format_float(total, right, value):
    return ('%%%s.%sf' % (total, right)) % value

symbol_emptylist = make_symbol("()") # will be redefined; ok
path, filename = os.path.split(__file__)
SCHEMEPATH = List(".", os.path.join(path, "modules"))

# end of Scheme.py
#############################################################
