####################################################
## Scheme in Python
##
## Jim Marshall
## Doug Blank
####################################################


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

__version__ = "2.1.7"

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

### Tuple layouts, documented once here rather than at each call site:
###
### Every invokable tuple (any make_proc(...)/make_cont(...)/make_fail(...)/
### make_handler(...)/make_macro(...) result, and k_reg/fail_reg/handler_reg/
### proc_reg/macro_reg generally) is (tag, fn, *args): index 0 is the tag
### symbol, index 1 is always the dispatch function, and everything from
### index 2 on are the arguments to call it with (`reg[1](*reg[2:])`, see
### apply_cont/apply_fail/apply_handler/apply_proc/apply_macro below).
###
### The one shape Phase 2/JIT special-cases -- an ordinary Scheme closure,
### built by closure()/make_proc(b_proc_1_d, bodies, formals, env, safe) --
### is always exactly 6 long, i.e. (tag, fn, bodies, formals, env, safe).
### Named below for use in code that runs once per closure (JIT compile
### time, cached after) rather than once per call:
_PROC_FN      = 1  # dispatch function (b_proc_1_d for an ordinary closure)
_PROC_BODIES  = 2
_PROC_FORMALS = 3
_PROC_ENV     = 4  # captured/definition environment ("cenv")
_PROC_SAFE    = 5  # precomputed _is_direct_eval_safe() verdict
### The once-per-*call* paths (apply_proc, _eval_direct, _apply_direct,
### _jit_call, _check_call_arity, ...) deliberately keep bare integer
### literals instead of these names -- see the comment on _eval_direct
### about extra-overhead-per-node being measurable on those paths.

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
    # proc_reg[1]=fn, [5]=safe; [2..4]=bodies/formals/cenv below.
    # See _PROC_* constants above make_proc (bare literals here on purpose).
    # _staruse_jit_star (Scheme-level *use-jit*, default #t) is a live
    # runtime toggle, checked here rather than folded into
    # _is_phase2_safe/_phase2_safe_cache -- caching a False verdict while
    # the flag happened to be off would wrongly keep this proc off
    # Phase 2 forever, even after the flag is switched back on.
    if (isinstance(proc_reg, tuple) and len(proc_reg) == 6
            and proc_reg[1] is b_proc_1_d and proc_reg[5]
            and _staruse_jit_star
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
            # epoch=None: a genuine soundness gap observed by execution,
            # not just a stale snapshot -- permanent, see
            # _phase2_safe_lookup's docstring.
            _phase2_safe_cache.set(proc_reg, (False, None))
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

class _SchemeRuntimeError(Exception):
    """Raised by Phase 2/JIT fast-path code (nested closure calls,
    car/cdr, ...) to signal a genuine Scheme-level runtime error with a
    proper, language-level message. apply_proc's own top-level entry into
    Phase 2 already validates arity before starting, and the classic
    register-machine dispatch (b_proc_55_d/b_proc_56_d for car/cdr, etc.)
    already checks argument count/type before ever touching the raw
    Python value -- but every *nested* call from within already-running
    Phase 2/JIT code (a JIT'd function calling another closure, _jit_call,
    _apply_direct, a fast-prim dispatch) bypasses those checks and would
    otherwise let a raw Python TypeError/AttributeError/IndexError escape
    with implementation-internal names (_jit_fn, _j_b, ...) in the
    message. trampoline() converts this into the same "RunTimeError"
    condition type the classic dispatch produces, instead of the
    catch-all "Unhandled <ClassName>" every other unanticipated Python
    exception gets -- see tests/test_fastpath_error_handling.py."""
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

## _binding_write_epoch / set_binding_value_b: see _phase2_safe_lookup's
## docstring for why this exists. set_binding_value_b is normally
## generated from source-rm.ss's `set-binding-value!` (just `(set-car!
## binding value)`) -- overridden here (added to translate_rm.py's
## to_ignore list) so it can bump a counter, since it is the one place
## every existing-binding mutation in the whole interpreter funnels
## through: (set! ...), a top-level (define ...) that overwrites an
## already-bound name, and amb/choose's own undo-on-backtrack. Bumping
## unconditionally on all of these (rather than only "the ones that
## matter") is deliberate -- see _phase2_safe_lookup.
_binding_write_epoch = 0

def set_binding_value_b(binding, value):
    global _binding_write_epoch
    _binding_write_epoch += 1
    set_car_b(binding, value)

_phase2_safe_cache = _IdentityCache()   # proc -> (verdict, epoch-or-None)

def _phase2_safe_lookup(proc):
    """Return the cached verdict for proc, or None if not yet computed --
    or if it was computed before some binding it depended on was
    reassigned since (a stale snapshot; see below).

    _is_phase2_safe's certification resolves names (e.g. an operator like
    `+` in _phase2_safe_walk_call) against the live environment once, then
    caches the resulting verdict by proc identity forever. That's a
    snapshot: a later (set! + something-not-fast-prim-safe) can make an
    already-cached "safe" verdict wrong in retrospect, without anything
    about `proc` itself changing. Left unchecked, apply_proc would still
    trust the stale verdict, start a live Phase-2 attempt, and hit
    exactly the "soundness gap" scenario _is_phase2_safe's own docstring
    warns about -- except the gap isn't a bug in the analysis, just a
    snapshot gone stale, and apply_proc has no way to tell the difference:
    either way it treats the mid-body _TrampolineFallback as unrecoverable
    and propagates a loud, internal-looking error instead of running the
    closure correctly (confirmed by reproducing it before this epoch
    check existed -- see tests/test_phase2_safe_cache_invalidation.py).

    Coarse-grained on purpose: ANY binding write anywhere invalidates
    every cached verdict, not just ones that actually depended on what
    changed, because a cached verdict doesn't record which specific names
    it resolved during certification -- there's nothing narrower to
    compare against without adding that bookkeeping. Correct, and cheap
    (one integer compare per lookup); a wrongly-invalidated entry only
    costs a re-walk, and since any closure containing a set! is already
    excluded from Phase 2 entirely by _is_direct_eval_safe, an epoch bump
    can only happen alongside a call that was already on the slow
    (trampoline) path, never inside a Phase-2/JIT hot loop itself.

    epoch is None for apply_proc's own "genuine soundness gap" verdict
    (a mismatch observed directly by execution, not just a snapshot risk)
    -- permanent, and deliberately exempt from this staleness check, so
    an unrelated later write can't un-blacklist a proc already proven to
    have a real bug in its certification."""
    entry = _phase2_safe_cache.get(proc)
    if entry is _CACHE_MISS:
        return None
    verdict, epoch = entry
    if epoch is not None and epoch != _binding_write_epoch:
        return None
    return verdict

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
    # proc[1]=fn, proc[5]=safe -- runs on every apply_proc call (cheap
    # guard before the cache lookup below), so kept as bare literals.
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
        # Only reached on a cache miss (once per unique closure identity,
        # then cached forever below), so named constants cost nothing here.
        safe = _phase2_safe_walk_seq(proc[_PROC_BODIES], proc[_PROC_ENV], _visiting)
    except Exception:
        safe = False
    finally:
        _visiting.discard(pid)
    if outermost:
        _phase2_safe_cache.set(proc, (safe, _binding_write_epoch))
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
    another closure that is itself (transitively) phase2-safe, or a
    literal-lambda IIFE operator (the let/or/and/cond/case/named-let
    desugaring shape) whose own body is itself (transitively) phase2-safe.
    Anything else unresolvable at this point -- a local parameter or other
    computed expression used in operator position, an unbound name --
    can't be proven safe, so is treated as unsafe."""
    global _fast_prim_map
    # A literal in operator position has no analog in the JIT compiler
    # (which never accepts one either), so this stays a local special case
    # rather than folding into _resolve_operator's shared contract.
    if isinstance(op_exp, cons) and op_exp.car is symbol_lit_aexp:
        op = op_exp.cdr.car
    elif isinstance(op_exp, cons) and op_exp.car is symbol_lambda_aexp:
        # An IIFE operator -- ((lambda (v ...) body ...) e ...), exactly
        # how let/or/and/cond/case/named-let all desugar. Both real
        # executors already handle this shape fine: _eval_direct's own
        # symbol_lambda_aexp case (above) just builds a closure that the
        # surrounding app_aexp case then calls like any other proc, and
        # _JitCompiler generates code for it via _jit_call. The only gap
        # is this static walk refusing to even try, so recurse into the
        # lambda's own body instead of bailing out.
        #
        # A real frame for the formals -- not just `env` unmodified --
        # has to be pushed first: nested lexical-address depths inside
        # the body are counted relative to the *actual* runtime frame
        # stack, which includes one frame for this lambda's own formals.
        # Skipping this would silently shift every deeper reference off
        # by one frame and make _resolve_lexical_address resolve the
        # wrong binding entirely (confirmed directly: without this, a
        # reference to a global like `>=` from inside a `let` nested in
        # a function body resolved to the wrong frame and was wrongly
        # declared unsafe -- see the mi-loop case in
        # tests/test_jit_iife_operator.py). The values bound don't
        # matter -- a depth-0 reference to one of them used as an
        # operator is still unconditionally unsafe ('local', below)
        # regardless of what's in the cell, and a depth-0 reference used
        # as a plain value is unconditionally safe regardless too
        # (_phase2_safe_walk's lexical_address_aexp case) -- only the
        # frame's *shape* (its formal names, for by-name shadowing, and
        # its presence, for depth-counting) has to be right.
        formals = op_exp.cdr.car
        dummy_args = []
        cur = formals
        while isinstance(cur, cons):
            dummy_args.append(False)
            cur = cur.cdr
        inner_env = _extend_direct(env, formals, dummy_args)
        return _phase2_safe_walk_seq(op_exp.cdr.cdr.car, inner_env, visiting)
    else:
        kind, op = _resolve_operator(op_exp, env)
        if kind != 'value':
            return False   # local param, computed operator, or unresolved — can't prove
    if isinstance(op, tuple) and op[0] is symbol_procedure:
        fn = op[_PROC_FN]
        if _fast_prim_map is None:
            _fast_prim_map = _build_fast_prim_map()
        if fn in _fast_prim_map and fn not in _fast_prim_hof_fns:
            return True
        return _is_phase2_safe(op, visiting)
    return False   # dlr proc / host interop / anything else — can't prove

def _check_call_arity(op, args):
    """Raise _SchemeRuntimeError (not a raw Python exception) if args'
    length doesn't match op's formal parameter count. Mirrors b_proc_1_d's
    own arity check (numeric_equal(length(new_args), length(new_formals)))
    -- every one of Phase 2/JIT's *nested*-call dispatch paths (this
    function's own callers: _eval_direct's app_aexp case, _jit_call,
    _apply_direct) bypasses b_proc_1_d entirely and calls straight into
    _extend_direct/extend/a JIT-compiled function, none of which validate
    argument count themselves.

    Only ever called with an op already confirmed to be a plain
    b_proc_1_d closure (fn is b_proc_1_d), so formals is always a proper,
    fixed-length list: a closure with rest/dotted formals compiles to a
    different AST tag (mu_lambda_aexp) and a different proc shape
    entirely, and is excluded from Phase 2/JIT altogether -- confirmed
    directly: _is_phase2_safe's own first check is `proc[1] is
    b_proc_1_d`, which such a closure never satisfies. So a plain length
    comparison is always correct here; no variadic-arity handling needed."""
    n = 0
    cur = op[3]  # formals (_PROC_FORMALS); runs on every nested call, kept as a literal
    while isinstance(cur, cons):
        n += 1
        cur = cur.cdr
    if n != len(args):
        raise _SchemeRuntimeError("incorrect number of arguments in application")

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
            # Inline _apply_direct for speed. op[1]=fn, and below:
            # op[5]=safe, op[4]=cenv, op[3]=formals, op[2]=bodies
            # (see _PROC_* above make_proc) -- runs per AST node, so
            # kept as bare literals rather than named-global lookups.
            if isinstance(op, tuple) and op[0] is symbol_procedure:
                fn = op[1]
                if _fast_prim_map is None:
                    _fast_prim_map = _build_fast_prim_map()
                direct = _fast_prim_map.get(fn)
                if direct is not None:
                    return direct(args)
                if len(op) == 6 and fn is b_proc_1_d and op[5]:
                    _check_call_arity(op, args)
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

_jit_cache = _IdentityCache()   # proc -> (compiled_fn-or-False, epoch)

def _jit_lookup(proc):
    """Return the cached compiled function for proc, or None if not yet
    compiled, if a previous compile attempt failed, OR if it was compiled
    before some binding a free-variable capture inside it depended on was
    reassigned since (a stale snapshot).

    _JitCompiler/_capture freeze every free variable's *value* into the
    compiled function's Python closure once, at compile time -- an
    already-JIT-compiled helper closure captured directly by its compiled
    Python function, a primitive wrapped from _fast_prim_map, or a plain
    scalar. Unlike Phase 2's _eval_direct, which re-resolves every name
    fresh on every call, none of that ever got re-checked once compiled: a
    later (set! + something-else) or (set! limit 20) would leave any
    already-compiled function silently using the original value forever
    (confirmed by reproducing it before this epoch check existed -- see
    tests/test_jit_cache_invalidation.py). Since a stale compiled function
    is evicted and recompiled *as a whole*, not just its individual
    captures, this also closes -- as a side effect, not by original
    design -- _is_unshadowed_primitive's own previously-documented
    residual risk for inlined arithmetic/comparison operators (`+`, `<`,
    ...): recompiling re-runs that check against the fresh environment
    too, so it correctly declines to re-inline a name that's since been
    shadowed. See test_primitive_redefinition.py's original inlined-
    operator case, and test_jit_cache_invalidation.py's
    redefined-after-first-compile variant of it.

    Reuses _binding_write_epoch (see _phase2_safe_lookup's docstring for
    the full reasoning -- set_binding_value_b bumps it on every existing-
    binding mutation anywhere in the interpreter) rather than a separate
    counter: one mutation invalidates both caches' stale entries in a
    single cheap comparison, and there's no bookkeeping anywhere of which
    specific names a given compile captured to invalidate more narrowly.
    Costlier here than for _phase2_safe_cache -- a Phase 2 re-walk is
    cheap, a JIT recompile is a real compile()/exec() -- but a stale
    compile can only happen for a name that's actually still being
    reassigned somewhere (via set!, so on the slow trampoline path: any
    closure containing set! is excluded from Phase 2/JIT entirely by
    _is_direct_eval_safe), never from inside the hot compiled loop itself.

    A cached failure (fn is False) is, as before, always reported the
    same as "not yet attempted" regardless of epoch (see
    README-PERFORMANCE.md's mutual-recursion discussion) -- retried on
    every call already, so there's nothing additional to invalidate."""
    entry = _jit_cache.get(proc)
    if entry is _CACHE_MISS:
        return None
    fn, epoch = entry
    if fn is False:
        return None
    if epoch != _binding_write_epoch:
        return None
    return fn

def _jit_compile_proc(proc):
    """Try to JIT-compile a safe Scheme closure.
    Sets _jit_cache[proc] and returns the compiled fn or None."""
    # Runs once per unique closure identity (result cached in _jit_cache
    # by every caller), so named constants cost nothing here.
    formals, bodies, cenv = proc[_PROC_FORMALS], proc[_PROC_BODIES], proc[_PROC_ENV]
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
        if jc._used_loop:
            # Non-tail statements must re-run on every logical call, i.e.
            # every loop iteration -- not just the first -- since each
            # iteration is a fresh (tail-)recursive call reusing this same
            # Python frame (see README-PERFORMANCE.md's Phase 4 tail-loop
            # flattening). Emitting them once, before the loop, silently
            # skipped their side effects on every call after the first --
            # see tests/test_jit_tail_loop_nontail_stmts.py.
            lines.append('    while True:')
            if jc._frame_var is not None:
                # Reset once per iteration/logical call -- see _lambda's
                # docstring -- so a later call can't reuse a stale frame
                # a previous iteration built.
                lines.append('        ' + jc._frame_var + ' = None')
            lines += ['    ' + ln for ln in stmt_lines]
            lines += ['    ' + ln for ln in tail_lines]
        else:
            if jc._frame_var is not None:
                lines.append('    ' + jc._frame_var + ' = None')
            lines += stmt_lines
            lines += tail_lines
        fn_src = '\n'.join(lines)
        ns = dict(free)
        ns['__builtins__'] = __builtins__
        exec(compile(fn_src, '<scheme-jit>', 'exec'), ns)
        fn = ns['_jit_fn']
        _jit_cache.set(proc, (fn, _binding_write_epoch))
        return fn
    except _TrampolineFallback:
        _jit_cache.set(proc, (False, _binding_write_epoch))
        return None
    except Exception:
        _jit_cache.set(proc, (False, _binding_write_epoch))
        return None

def _jit_make_frame(parent_env, outer_formals, outer_values):
    """Reconstruct — from live values — the environment frame the
    enclosing JIT'd function's own parameters would occupy under normal
    (non-JIT) evaluation, chained onto its real captured environment
    (parent_env). Split out from _jit_make_closure (which used to build
    this itself, once per lambda_aexp) so _JitCompiler._lambda can build
    it *once per logical call* and share it across every lambda_aexp
    compiled from the same enclosing function body -- see _lambda's
    docstring."""
    return _extend_direct(parent_env, outer_formals, outer_values)

def _jit_make_closure(formals, bodies, frame_env):
    """Materialize a genuine Scheme closure for a lambda_aexp encountered
    inside JIT'd code, using a frame already reconstructed by
    _jit_make_frame (shared across sibling lambda_aexps from the same
    call -- see _lambda). Reuses the ordinary closure() constructor, so
    the result is a completely normal Scheme proc tuple."""
    return closure(formals, bodies, frame_env)

def _jit_call(op, args):
    """Call `op` (a Python arg list) whether it's a native Python callable
    (an already-JIT'd function, a wrapped primitive, or a self-reference) or
    a Scheme proc tuple — e.g. one produced by _jit_make_closure, or an
    un-JIT'd closure reached through a computed operator expression such as
    ((make-adder n) 0), an immediately-invoked lambda, or a parameter used
    in operator position (e.g. (define (apply-twice f x) (f (f x))))."""
    # Runs on every call routed through here (e.g. each map/for-each
    # element), so op[1]=fn / op[5]=safe stay bare literals -- see
    # _PROC_* above make_proc.
    if isinstance(op, tuple) and op[0] is symbol_procedure:
        fn = op[1]
        # _staruse_jit_star: see apply_proc's comment. Falls through to
        # _apply_direct below when off, which is gated the same way.
        if len(op) == 6 and fn is b_proc_1_d and op[5] and _staruse_jit_star:
            _check_call_arity(op, args)
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
        # Must match odd_q's real definition (`n % 2 == 1`) exactly, not
        # `!= 0` -- the two agree for every integer (n % 2 is always 0 or
        # 1 there) but silently diverge for a non-integer argument, e.g.
        # 2.5 % 2 == 0.5: `!= 0` is True (wrongly "odd"), `== 1` is False
        # (correct) -- found by tests/test_jit_fuzz.py's differential
        # fuzzer, see tests/test_jit_odd_float.py.
        'odd?':  '({0} % 2 == 1)',
        'car':   '_j_safe_car({0})',
        'cdr':   '_j_safe_cdr({0})',
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
        self._frame_var = None    # local var name holding this call's reconstructed
                                   # outer frame, once _lambda first needs one -- see _lambda

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
        list matching self._params 1:1 — the reconstructed frame is built
        positionally from self._params's live values, so a rest parameter
        (silently dropped from self._params — see _jit_compile_proc) would
        misalign the frame.

        The frame itself is built at most *once per logical call* and
        shared by every lambda_aexp compiled by this same _JitCompiler
        instance (self._frame_var names the local Python variable holding
        it, lazily initialized here via a walrus expression so it works
        regardless of which lambda happens to run first across conditional
        branches) -- matching normal (non-JIT) evaluation, where every
        closure created during one call to the enclosing function is built
        against the exact same environment frame, so mutating a variable
        captured by one (via a set! inside a *different*, separately
        analyzed inner closure -- this function's own body can't contain
        set! at all, see _is_direct_eval_safe) is visible through any
        sibling closure that also captured it. Building an independent
        frame per lambda_aexp instead (the previous behavior) silently
        broke that sharing -- see tests/test_jit_lambda_shared_frame.py.
        _jit_compile_proc resets self._frame_var's variable to None once
        per logical call (each while-loop iteration for a self-recursive
        tail loop, or once for an ordinary call), so a later call doesn't
        reuse a previous call's frame.
        """
        # _lambda runs once per closure at JIT-source-generation time
        # (result cached), so named constants are free here.
        cur = self._self[_PROC_FORMALS]
        count = 0
        while isinstance(cur, cons):
            count += 1
            cur = cur.cdr
        if cur is not symbol_emptylist or count != len(self._params):
            raise _TrampolineFallback()
        self._free['_jit_make_closure'] = _jit_make_closure
        self._free['_jit_make_frame'] = _jit_make_frame
        if self._frame_var is None:
            self._frame_var = '_jit_frame'
        fm = self._capture_const(formals)
        bd = self._capture_const(bodies)
        ce = self._capture_const(self._env)
        of = self._capture_const(self._self[_PROC_FORMALS])
        values = ", ".join(self._params)
        fv = self._frame_var
        frame_expr = (f'({fv} if {fv} is not None else '
                       f'({fv} := _jit_make_frame({ce}, {of}, [{values}])))')
        return f'_jit_make_closure({fm}, {bd}, {frame_expr})'

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

        if sym and (sym in self._NARY or sym in self._CMP or sym in self._UNARY) \
                and not self._is_unshadowed_primitive(sym, op_exp):
            # sym has the right *spelling* for one of the tables below, but
            # doesn't currently resolve to the genuine, unmodified
            # primitive that inlining it as a raw Python operator/template
            # assumes -- see _is_unshadowed_primitive. Treat it as an
            # ordinary call instead of inlining a stale operator; falls
            # through to the general call path at the bottom of this
            # method, same as any other name that isn't one of these ~9.
            sym = None

        if sym:
            # n-ary arithmetic (left-assoc)
            if sym in self._NARY:
                op = self._NARY[sym]
                if n == 0:
                    # + and * have identity elements (0 and 1, matching
                    # plus()/multiply() with zero args) -- but `-` requires
                    # at least one argument (matching minus()/the classic
                    # dispatch's own arity check), so (- ) is a genuine
                    # arity error, not "0". Emitting a literal '1' for it
                    # here would silently produce a wrong answer instead of
                    # an error -- fall through to the general call path
                    # below instead, which correctly reaches the
                    # arity-checked fast-prim dispatch.
                    if sym == '+':
                        return '0'
                    if sym == '*':
                        return '1'
                elif n == 1 and sym == '-':
                    return f'(-{args[0]})'
                elif sym == '+':
                    # plus() folds from an explicit 0
                    # (functools.reduce(operator.add, args, 0)), which the
                    # classic dispatch and _fast_prim_map both go through
                    # for this same call -- for ordinary numbers 0 + x ==
                    # x, but IEEE-754 negative zero is the one case where
                    # they differ (0.0 + -0.0 == 0.0, positive, while -0.0
                    # alone stays negative). Matching the exact fold order
                    # here (0 + a + b + ...) keeps JIT-inlined + identical
                    # to plus() in that edge case instead of silently
                    # flipping a sign bit -- confirmed this diverges
                    # without the explicit 0 -- see
                    # tests/test_jit_plus_negative_zero.py.
                    return '(0 + ' + ' + '.join(args) + ')'
                else:
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
                elif sym == 'car':
                    self._free['_j_safe_car'] = _jit_safe_car
                elif sym == 'cdr':
                    self._free['_j_safe_cdr'] = _jit_safe_cdr
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

    def _is_unshadowed_primitive(self, sym, op_exp):
        """True iff op_exp, resolved against this closure's own captured
        environment, still points to the genuine, unmodified primitive
        that _NARY/_CMP/_UNARY assume when inlining `sym` as a raw Python
        operator/expression template. Without this check, a program that
        already redefines `+`, `<`, `car`, ... (e.g. `(set! + my-own-add)`)
        *before* a function using it is ever compiled would have that
        first compile attempt bake in the wrong, stale-at-birth semantics
        -- see tests/test_primitive_redefinition.py. A redefinition
        happening *after* a function has already been compiled is a
        separate case, not this function's job: _jit_lookup's epoch check
        (see its docstring) evicts and recompiles the whole function when
        that happens, which re-runs this exact check against the fresh
        environment -- see test_jit_cache_invalidation.py.

        Reuses _resolve_operator (the same static-resolution helper
        _is_self_ref and _phase2_safe_walk_call use) rather than
        re-deriving env-lookup logic here. Deliberately conservative: a
        local parameter (kind == 'local', e.g. `+` shadowed as one of
        this very function's own formals), an unresolved/computed
        operator, or a resolved value that isn't exactly the primitive
        _fast_prim_map has on file for `sym` all return False -- inlining
        is skipped and the call falls through to the general call path,
        which correctly dispatches to whatever `sym` actually currently
        is. Ties are broken toward "don't inline", the same asymmetric
        risk tradeoff used throughout Phase 2/JIT: too conservative only
        costs a missed optimization, too permissive risks a silently
        wrong answer.
        """
        global _fast_prim_map
        kind, val = _resolve_operator(op_exp, self._env)
        if kind != 'value':
            return False
        if not (isinstance(val, tuple) and val[0] is symbol_procedure):
            return False
        if _fast_prim_map is None:
            _fast_prim_map = _build_fast_prim_map()
        entry = _fast_prim_map.get(val[_PROC_FN])
        return entry is not None and getattr(entry, '_fast_prim_name', None) == sym

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
                direct = _fast_prim_map.get(val[_PROC_FN])
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

def _jit_safe_car(x):
    """Type-checked car, matching the classic dispatch's own check
    (b_proc_55_d in the generated scheme.py) instead of a raw `.car`
    attribute access, which raises a bare Python AttributeError -- with no
    Scheme-level type -- on a non-pair. See _SchemeRuntimeError. Takes the
    scalar argument directly (not a Python-list `args`) so _JitCompiler's
    _UNARY inlining can call it with zero extra allocation."""
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("car called on non-pair ~s", x))
    return x.car

def _jit_safe_cdr(x):
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cdr called on non-pair ~s", x))
    return x.cdr

def _fast_prim_car(args):
    return _jit_safe_car(args[0])

def _fast_prim_cdr(args):
    return _jit_safe_cdr(args[0])

## ----- Type/bounds-checked fast-prim wrappers -----
##
## Every _FAST_PRIM_SPECS entry below this point that isn't a trivial
## predicate/equality check touches its argument's internal representation
## directly (a raw `.car`/`.cdr` chain, a Python len()/index, an isinstance
## check assumed to already hold). The classic register-machine dispatch
## (the b_proc_XX_d functions in the generated scheme.py, compiled from
## interpreter-cps.ss) validates argument *type* before ever doing that --
## these wrappers replicate the exact same check and message each one
## does, found by reading each primitive's own b_proc_XX_d and confirmed
## against its actual behavior (not just its check condition -- e.g. caar
## only validates its *own* pair-ness, not a deeper level, so a value like
## (cons 1 2) still raises a raw AttributeError even at the classic
## baseline; this file matches that exactly, not a stricter, self-invented
## check, since the goal is parity with the reference behavior, not a new
## one). See tests/test_fastprim_type_checks.py.
def _fast_prim_caar(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("caar called on non-pair ~s", x))
    return x.car.car

def _fast_prim_cdar(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cdar called on non-pair ~s", x))
    return x.car.cdr

def _fast_prim_cadar(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cadar called on non-pair ~s", x))
    return x.car.cdr.car

def _fast_prim_cdddr(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cdddr called on non-pair ~s", x))
    return x.cdr.cdr.cdr

def _fast_prim_cddr(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cddr called on non-pair ~s", x))
    return x.cdr.cdr

def _fast_prim_cadddr(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("cadddr called on non-pair ~s", x))
    return x.cdr.cdr.cdr.car

def _fast_prim_cadr(args):
    x = args[0]
    if not length_at_least_q(2, x):
        raise _SchemeRuntimeError(format("cadr called on incorrect list structure ~s", x))
    return x.cdr.car

def _fast_prim_caddr(args):
    x = args[0]
    if not length_at_least_q(3, x):
        raise _SchemeRuntimeError(format("caddr called on incorrect list structure ~s", x))
    return x.cdr.cdr.car

def _fast_prim_length(args):
    x = args[0]
    n = 0
    while x is not symbol_emptylist:
        if not isinstance(x, cons):
            raise _SchemeRuntimeError(format("length called on improper list ~s", args[0]))
        n += 1
        x = x.cdr
    return n

def _fast_prim_symbol_to_string(args):
    x = args[0]
    if not isinstance(x, Symbol):
        raise _SchemeRuntimeError(format("symbol->string called on non-symbol item ~s", x))
    return symbol_to_string(x)

def _fast_prim_round(args):
    # round-prim (interpreter-cps.ss) checks arity and type together with
    # one shared message, not two separate ones -- matched here rather
    # than relying on the generic _arity_checked_prim wrapper (which
    # would use the standard "incorrect number of arguments to round"
    # instead), so 'round' is deliberately left out of _FAST_PRIM_ARITY.
    if not (len(args) == 1 and number_q(args[0])):
        raise _SchemeRuntimeError("round requires exactly one number")
    x = args[0]
    return round(x)

def _fast_prim_set_car_checked(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("set-car! called on non-pair ~s", x))
    x.car = args[1]
    return void_value

def _fast_prim_set_cdr_checked(args):
    x = args[0]
    if not isinstance(x, cons):
        raise _SchemeRuntimeError(format("set-cdr! called on non-pair ~s", x))
    x.cdr = args[1]
    return void_value

def _fast_prim_vector_length(args):
    x = args[0]
    if not vector_q(x):
        raise _SchemeRuntimeError(format("vector-length called on incorrect vector structure ~s", x))
    return vector_length(x)

def _fast_prim_vector_to_list(args):
    x = args[0]
    if not vector_q(x):
        raise _SchemeRuntimeError(format("vector->list called on incorrect vector structure ~s", x))
    return vector_to_list(x)

def _fast_prim_list_to_vector(args):
    x = args[0]
    if not list_q(x):
        raise _SchemeRuntimeError(format("list->vector called on incorrect list structure ~s", x))
    return list_to_vector(x)

def _fast_prim_string_length(args):
    x = args[0]
    if not string_q(x):
        raise _SchemeRuntimeError("string-length called on non-string argument")
    return string_length(x)

def _fast_prim_string_to_list(args):
    x = args[0]
    if not string_q(x):
        raise _SchemeRuntimeError(format("string->list called on non-string item ~s", x))
    return string_to_list(x)

def _fast_prim_list_to_string(args):
    x = args[0]
    if not list_q(x):
        raise _SchemeRuntimeError(format("list->string called on incorrect list structure ~s", x))
    return list_to_string(x)

def _fast_prim_string_to_symbol(args):
    x = args[0]
    if not string_q(x):
        raise _SchemeRuntimeError(format("string->symbol called on non-string item ~s", x))
    return string_to_symbol(x)

def _fast_prim_member(args):
    item, lyst = args[0], args[1]
    x = lyst
    while x is not symbol_emptylist:
        if not isinstance(x, cons):
            raise _SchemeRuntimeError(format("member called on improper list ~s", lyst))
        if not (equal_q(item, x.car) is False):
            return x
        x = x.cdr
    return False

def _fast_prim_numeric_equal(args):
    if not all_numeric_q(args):
        raise _SchemeRuntimeError("attempt to apply = on non-numeric argument")
    result = True
    for i in range(len(args) - 1):
        if not numeric_equal(args[i], args[i + 1]):
            result = False
            break
    return result

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
    '=':     _fast_prim_numeric_equal,
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
    'round':     _fast_prim_round,
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
    'car':    _fast_prim_car,
    'cdr':    _fast_prim_cdr,
    'cons':   lambda args: cons(args[0], args[1]),
    'set-car!': _fast_prim_set_car_checked,
    'set-cdr!': _fast_prim_set_cdr_checked,
    'caar':   _fast_prim_caar,
    'cadr':   _fast_prim_cadr,
    'cdar':   _fast_prim_cdar,
    'cddr':   _fast_prim_cddr,
    'cadar':  _fast_prim_cadar,
    'caddr':  _fast_prim_caddr,
    'cdddr':  _fast_prim_cdddr,
    'cadddr': _fast_prim_cadddr,
    # List operations
    'list':      lambda args: List(*args),
    'length':    _fast_prim_length,
    'reverse':   lambda args: reverse(args[0]),
    # append(*objs) (native, below) indexes objs[-1] unconditionally, so it
    # can't handle zero arguments itself even though 0-arg (append) is
    # valid (returns '()') at the classic dispatch -- special-cased here
    # rather than changing the native append(), which is also used
    # elsewhere. See tests/test_fastprim_arity.py.
    'append':    lambda args: symbol_emptylist if not args else append(*args),
    'list-ref':  lambda args: list_ref(args[0], args[1]),
    # Search
    'memq':   lambda args: memq(args[0], args[1]),
    'memv':   lambda args: memv(args[0], args[1]),
    'member': _fast_prim_member,
    'assq':   lambda args: assq(args[0], args[1]),
    'assv':   lambda args: assv(args[0], args[1]),
    # Vector operations
    'make-vector':   lambda args: make_vector(args[0]),
    'vector-ref':    lambda args: vector_ref(args[0], args[1]),
    'vector-set!':   lambda args: (vector_set_b(args[0], args[1], args[2]), void_value)[1],
    'vector-length': _fast_prim_vector_length,
    'vector->list':  _fast_prim_vector_to_list,
    'list->vector':  _fast_prim_list_to_vector,
    # String operations
    'string-length':  _fast_prim_string_length,
    'string-ref':     lambda args: string_ref(args[0], args[1]),
    'string-append':  lambda args: string_append(*args),
    'substring':      lambda args: substring(args[0], args[1], args[2] if len(args) > 2 else string_length(args[0])),
    'string->list':   _fast_prim_string_to_list,
    'list->string':   _fast_prim_list_to_string,
    'string->number': lambda args: string_to_number(args[0]),
    'number->string': lambda args: number_to_string(args[0], args[1] if len(args) > 1 else 10),
    'string=?':       lambda args: string_is__q(args[0], args[1]),
    'string<?':       lambda args: stringLessThan_q(args[0], args[1]),
    # Symbol / char operations
    'symbol->string':  _fast_prim_symbol_to_string,
    'string->symbol':  _fast_prim_string_to_symbol,
    'char->integer':   lambda args: char_to_integer(args[0]),
    'integer->char':   lambda args: integer_to_char(args[0]),
    'char-alphabetic?': lambda args: char_alphabetic_q(args[0]),
    'char-numeric?':    lambda args: char_numeric_q(args[0]),
    'char-whitespace?': lambda args: char_whitespace_q(args[0]),
    # Higher-order (propagate _TrampolineFallback if proc not directly evaluable)
    'map':      _fast_prim_direct_map,
    'for-each': _fast_prim_direct_for_each,
}

## (min, max) argument-count bounds for every _FAST_PRIM_SPECS entry, so
## that the exact same wrong-arity call that b_proc_1_d cleanly rejects
## for a user closure (see _check_call_arity) also gets a clean
## "incorrect number of arguments to X" RunTimeError here, instead of a
## raw IndexError/TypeError from indexing straight into `args` -- none of
## the _FAST_PRIM_SPECS callables above validate their own argument count.
##
## Derived empirically from each primitive's own classic dispatch
## (b_proc_XX_d in the generated scheme.py -- always correct, since a
## direct top-level call never goes through Phase 2 at all): the boundary
## between "returns a value" and "some error" at the reference/baseline
## level, probed across argument counts 0-6 with type-appropriate dummy
## values (see the audit that found this gap). max=None means the
## classic dispatch itself places no upper bound (fully variadic, or
## silently ignores extra args the same way the fast lambda already
## does) -- not asserting an upper bound there preserves exactly that
## existing (baseline-matching) behavior rather than inventing a new,
## stricter one. See tests/test_fastprim_arity.py.
_FAST_PRIM_ARITY = {
    '+': (0, None), '-': (1, None), '*': (0, None), '/': (0, None),
    '<': (2, None), '>': (2, None), '<=': (2, None), '>=': (2, None), '=': (2, None),
    'not': (1, 1),
    # zero?/expt were (1, None)/(2, None) -- too permissive, matching the
    # classic dispatch's own former looseness (length-at-least?, not an
    # exact check): a fast-prim call with extra args just silently
    # ignored them (zero?'s wrapper only ever reads args[0]; expt's only
    # args[0]/args[1]) instead of raising an arity error, on every path
    # -- see tests/test_arity_tightening.py.
    'zero?': (1, 1), 'even?': (1, 1), 'odd?': (1, 1), 'abs': (1, 1),
    'min': (1, None), 'max': (1, None),
    'modulo': (2, 2), 'remainder': (2, 2), 'quotient': (2, 2),
    'expt': (2, 2), 'sqrt': (1, 1),
    # 'round' deliberately absent -- see _fast_prim_round's own comment.
    'null?': (1, 1), 'pair?': (1, 1), 'number?': (1, 1), 'string?': (1, 1),
    'symbol?': (1, 1), 'char?': (1, 1), 'boolean?': (1, 1), 'vector?': (1, 1),
    'list?': (1, 1), 'procedure?': (1, 1),
    'eq?': (2, 2), 'eqv?': (2, 2), 'equal?': (2, 2),
    'car': (1, 1), 'cdr': (1, 1), 'cons': (2, 2),
    'set-car!': (2, 2), 'set-cdr!': (2, 2),
    'caar': (1, 1), 'cadr': (1, 1), 'cdar': (1, 1), 'cddr': (1, 1),
    'cadar': (1, 1), 'caddr': (1, 1), 'cdddr': (1, 1), 'cadddr': (1, 1),
    'list': (0, None), 'length': (1, 1), 'reverse': (1, 1), 'append': (0, None),
    'list-ref': (2, 2),
    # memv/assv were (2, None) -- same too-permissive pattern as zero?/
    # expt above (their wrappers only ever read args[0]/args[1]).
    'memq': (2, 2), 'memv': (2, 2), 'member': (2, 2), 'assq': (2, 2), 'assv': (2, 2),
    'make-vector': (1, 1), 'vector-ref': (2, 2), 'vector-set!': (3, 3),
    'vector-length': (1, 1), 'vector->list': (1, 1), 'list->vector': (1, 1),
    'string-length': (1, 1), 'string-ref': (2, 2), 'string-append': (2, None),
    'substring': (2, 3), 'string->list': (1, 1), 'list->string': (1, 1),
    'string->number': (1, 1), 'number->string': (1, 2),
    'string=?': (2, 2), 'string<?': (2, 2),
    'symbol->string': (1, 1), 'string->symbol': (1, 1),
    'char->integer': (1, 1), 'integer->char': (1, 1),
    'char-alphabetic?': (1, 1), 'char-numeric?': (1, 1), 'char-whitespace?': (1, 1),
    'map': (2, None), 'for-each': (2, None),
}

def _arity_checked_prim(name, min_n, max_n, fn):
    def wrapper(args):
        n = len(args)
        if n < min_n or (max_n is not None and n > max_n):
            raise _SchemeRuntimeError("incorrect number of arguments to %s" % name)
        return fn(args)
    # _JitCompiler._is_unshadowed_primitive (Bug 2's redefinition-safety
    # check) identifies a _fast_prim_map entry by *which primitive name it
    # was built for*, not by comparing it to _FAST_PRIM_SPECS[sym] by
    # identity -- that identity no longer holds now that every entry is
    # wrapped here rather than stored as-is. Stashing the name on the
    # wrapper is what lets that check keep working post-wrapping.
    wrapper._fast_prim_name = name
    return wrapper

def _build_fast_prim_map():
    """Populate the fast prim map by resolving known symbol names in
    toplevel_env, once, lazily, on first use -- cached in the module-level
    _fast_prim_map for the life of the process (see its callers).

    Keyed by proc[1], the identity of the underlying Python dispatch
    function -- unique per *primitive* (e.g. b_proc_55_d for car), but
    NOT unique per user-defined closure: every ordinary Scheme closure
    shares the exact same dispatch function, b_proc_1_d (see closure() /
    b_proc_1_d itself). If a fast-path name (e.g. `+`) has already been
    redefined to a user closure by the time this first runs, proc[1] for
    that name resolves to b_proc_1_d too -- registering it would map
    b_proc_1_d itself to a primitive's handler, and since b_proc_1_d is
    shared by *every* closure in the program, every later Phase-2/JIT
    closure call would then incorrectly match this one entry and get
    silently replaced by the primitive's behavior, regardless of what
    that closure's own code says. The `proc[1] is not b_proc_1_d` guard
    below is what prevents this: such a name is simply left out of the
    fast-path table (that specific name loses its fast-path optimization
    for the rest of the process -- costs speed, not correctness -- while
    every other, non-redefined name is unaffected), rather than ever
    letting b_proc_1_d become a key. See
    tests/test_primitive_redefinition.py."""
    # Runs once total: this map is built lazily on first use and cached
    # at module level (_fast_prim_map) for the life of the process, so
    # named constants cost nothing here.
    result = {}
    for sym_name, direct_fn in _FAST_PRIM_SPECS.items():
        b = search_env(toplevel_env, make_symbol(sym_name))
        if b is not False:
            proc = binding_value(b)
            if (isinstance(proc, tuple) and proc[0] is symbol_procedure
                    and proc[_PROC_FN] is not b_proc_1_d):
                min_n, max_n = _FAST_PRIM_ARITY.get(sym_name, (0, None))
                result[proc[_PROC_FN]] = _arity_checked_prim(sym_name, min_n, max_n, direct_fn)
                if sym_name in ('map', 'for-each'):
                    _fast_prim_hof_fns.add(proc[_PROC_FN])
    return result

def _apply_direct(proc, args, env):
    global _fast_prim_map
    if dlr_proc_q(proc):
        return dlr_apply(proc, List(*args))
    # Runs on every map/for-each element and every dlr callback, so
    # proc[1]=fn / proc[5]=safe / proc[2..4]=bodies,formals,cenv below
    # stay bare literals -- see _PROC_* above make_proc. proc[5] alone only
    # proves THIS closure's own body has no set! -- it says nothing about
    # whether everything it calls is safe to run without ever hitting
    # _TrampolineFallback mid-body (a call to a closure that itself uses
    # set!, for instance). apply_proc requires the full transitive
    # _is_phase2_safe(proc) certification before it will ever start
    # _eval_sequence_direct; this must too, for the same reason -- see
    # tests/test_apply_direct_proc5_gap.py.
    if isinstance(proc, tuple) and proc[0] is symbol_procedure:
        fn = proc[1]
        if _fast_prim_map is None:
            _fast_prim_map = _build_fast_prim_map()
        direct = _fast_prim_map.get(fn)
        if direct is not None:
            return direct(args)      # args is a Python list — no cons needed
        # See apply_proc's comment on _staruse_jit_star for why this is a
        # separate live check rather than folded into _is_phase2_safe.
        if (len(proc) == 6 and fn is b_proc_1_d and proc[5]
                and _staruse_jit_star and _is_phase2_safe(proc)):
            _check_call_arity(proc, args)
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

## N-ary chained comparison (a < b < c < ..., not just pairwise), matching
## _fast_prim_numeric_equal's own already-correct pattern for `=`. The
## classic dispatch's own arity check (b_proc_95_d..b_proc_99_d in the
## generated scheme.py) already only requires "at least 2" args -- the
## bug was these being fixed 2-parameter functions, so e.g.
## Apply(LessThan, args_reg) crashed with a raw Python TypeError the
## moment the arity check let 3+ args through. `=` had this same crash
## in the classic dispatch (Apply(numeric_equal, args_reg)) despite
## _fast_prim_numeric_equal already handling N-ary correctly on the
## Phase 2/JIT path, meaning (= a b c) silently gave a correct answer or
## crashed depending on invisible internal state (whether the calling
## closure happened to be Phase-2-certified) -- see
## tests/test_nary_comparisons.py.
##
## `a, b, *rest` (not a single `*args`) rather than the more obvious
## `all(args[i] OP args[i+1] for i in range(len(args)-1))` on the whole
## tuple: measured ~1.5x overhead for the overwhelmingly common 2-arg
## case with this shape, vs. ~5x with the single-*args version -- these
## are also used in hot internal bookkeeping (e.g. numeric_equal in
## b_proc_1_d's own per-call arity check, on every classic-trampoline
## function call; not on the Phase 2/JIT hot path, which uses
## _check_call_arity's own plain integer comparison instead), so the
## fast path for the by-far-most-common 2-arg call matters. The N-ary
## case (`rest` non-empty) only ever happens for an actual N-ary Scheme
## call and pays the tuple-rebuild + loop cost once, not per comparison.
def numeric_equal(o1, o2, *rest):
    if not rest:
        return o1 == o2
    args = (o1, o2) + rest
    return all(args[i] == args[i + 1] for i in range(len(args) - 1))

def equal_q(o1, o2):
    if boolean_q(o1) or boolean_q(o2):
        return boolean_q(o1) and boolean_q(o2) and o1 is o2
    return o1 == o2

def LessThan(a, b, *rest):
    if not rest:
        return a < b
    args = (a, b) + rest
    return all(args[i] < args[i + 1] for i in range(len(args) - 1))

def LessThanEqual(a, b, *rest):
    if not rest:
        return a <= b
    args = (a, b) + rest
    return all(args[i] <= args[i + 1] for i in range(len(args) - 1))

def GreaterThanEqual(a, b, *rest):
    if not rest:
        return a >= b
    args = (a, b) + rest
    return all(args[i] >= args[i + 1] for i in range(len(args) - 1))

def GreaterThan(a, b, *rest):
    if not rest:
        return a > b
    args = (a, b) + rest
    return all(args[i] > args[i + 1] for i in range(len(args) - 1))

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

## radix defaults to 10 (str(number), handling floats/fractions/complex/
## anything else str() knows about) -- previously the only mode, silently
## accepting but ignoring an explicit radix argument (see
## tests/test_number_to_string_radix.py). A non-10 radix follows R7RS:
## only defined for an exact integer, so anything else is a clean
## _SchemeRuntimeError rather than a nonsensical string. '{:x}'.format(n)
## (a str *method* call), not the bare format(n, 'x') builtin -- format
## is shadowed module-wide by the Scheme-level ~s/~a formatter above.
def number_to_string(number, radix=10):
    if radix == 10:
        return str(number)
    if not isinstance(number, int) or isinstance(number, bool):
        raise _SchemeRuntimeError(
            format("number->string: a radix other than 10 requires an exact integer, got ~s", number))
    if radix == 2:
        return '{:b}'.format(number)
    if radix == 8:
        return '{:o}'.format(number)
    if radix == 16:
        return '{:x}'.format(number)
    raise _SchemeRuntimeError(
        format("number->string: unsupported radix ~s (must be 2, 8, 10, or 16)", radix))

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
            except _SchemeRuntimeError as e:
                exception_reg = make_exception("RunTimeError", str(e), symbol_none, symbol_none, symbol_none)
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

symbol_emptylist = make_symbol("()")
symbol_lit_aexp = make_symbol("lit-aexp")
symbol_var_aexp = make_symbol("var-aexp")
symbol_lexical_address_aexp = make_symbol("lexical-address-aexp")
symbol_if_aexp = make_symbol("if-aexp")
symbol_help_aexp = make_symbol("help-aexp")
symbol_association_aexp = make_symbol("association-aexp")
symbol_assign_aexp = make_symbol("assign-aexp")
symbol_func_aexp = make_symbol("func-aexp")
symbol_callback_aexp = make_symbol("callback-aexp")
symbol_define_aexp = make_symbol("define-aexp")
symbol_define_b_aexp = make_symbol("define!-aexp")
symbol_define_syntax_aexp = make_symbol("define-syntax-aexp")
symbol_define_syntax_transformer_aexp = make_symbol("define-syntax-transformer-aexp")
symbol_define_tests_aexp = make_symbol("define-tests-aexp")
symbol_run_tests_aexp = make_symbol("run-tests-aexp")
symbol_begin_aexp = make_symbol("begin-aexp")
symbol_lambda_aexp = make_symbol("lambda-aexp")
symbol_mu_lambda_aexp = make_symbol("mu-lambda-aexp")
symbol_trace_lambda_aexp = make_symbol("trace-lambda-aexp")
symbol_mu_trace_lambda_aexp = make_symbol("mu-trace-lambda-aexp")
symbol_app_aexp = make_symbol("app-aexp")
symbol_try_catch_aexp = make_symbol("try-catch-aexp")
symbol_try_finally_aexp = make_symbol("try-finally-aexp")
symbol_try_catch_finally_aexp = make_symbol("try-catch-finally-aexp")
symbol_raise_aexp = make_symbol("raise-aexp")
symbol_choose_aexp = make_symbol("choose-aexp")
symbol_undefined = make_symbol("undefined")
symbol_continuation = make_symbol("continuation")
symbol_none = make_symbol("none")
symbol_quasiquote = make_symbol("quasiquote")
symbol_lambda_no_defines = make_symbol("lambda-no-defines")
symbol_letrec = make_symbol("letrec")
symbol_trace_lambda_no_defines = make_symbol("trace-lambda-no-defines")
symbol_let = make_symbol("let")
symbol_cond = make_symbol("cond")
symbol_else = make_symbol("else")
symbol_eq_q = make_symbol("eq?")
symbol_quote = make_symbol("quote")
symbol_memq = make_symbol("memq")
symbol_car = make_symbol("car")
symbol_Apply = make_symbol("apply")
symbol_lambda = make_symbol("lambda")
symbol_cdr = make_symbol("cdr")
symbol_define = make_symbol("define")
symbol_args = make_symbol("args")
symbol_if = make_symbol("if")
symbol_numeric_equal = make_symbol("=")
symbol_length = make_symbol("length")
symbol_error = make_symbol("error")
symbol_not = make_symbol("not")
symbol_cases = make_symbol("cases")
symbol_append = make_symbol("append")
symbol_list_to_vector = make_symbol("list->vector")
symbol_cons = make_symbol("cons")
symbol_List = make_symbol("list")
symbol_unit = make_symbol("unit")
symbol_composite = make_symbol("composite")
symbol_continuation2 = make_symbol("continuation2")
symbol_set_b = make_symbol("set!")
symbol_x = make_symbol("x")
symbol_and = make_symbol("and")
symbol_pair_q = make_symbol("pair?")
symbol_begin = make_symbol("begin")
symbol_end_marker = make_symbol("end-marker")
symbol_ok = make_symbol("ok")
symbol_continuation3 = make_symbol("continuation3")
symbol_continuation4 = make_symbol("continuation4")
symbol_dot = make_symbol("dot")
symbol_fail_continuation = make_symbol("fail-continuation")
symbol_handler = make_symbol("handler")
symbol_exception = make_symbol("exception")
symbol_handler2 = make_symbol("handler2")
symbol_procedure = make_symbol("procedure")
symbol_macro_transformer = make_symbol("macro-transformer")
symbol_or = make_symbol("or")
symbol__is_to_ = make_symbol("=>")
symbol_goto = make_symbol("goto")
symbol_start_state = make_symbol("start-state")
symbol_shift = make_symbol("shift")
symbol_replace = make_symbol("replace")
symbol_drop = make_symbol("drop")
symbol_token_start_state = make_symbol("token-start-state")
symbol_emit = make_symbol("emit")
symbol_apply_action = make_symbol("apply-action")
symbol_integer = make_symbol("integer")
symbol_decimal = make_symbol("decimal")
symbol_rational = make_symbol("rational")
symbol_identifier = make_symbol("identifier")
symbol_boolean = make_symbol("boolean")
symbol_character = make_symbol("character")
symbol_named_character = make_symbol("named-character")
symbol_string = make_symbol("string")
symbol_comment_state = make_symbol("comment-state")
symbol_lparen = make_symbol("lparen")
symbol_lbracket = make_symbol("lbracket")
symbol_rparen = make_symbol("rparen")
symbol_rbracket = make_symbol("rbracket")
symbol_apostrophe = make_symbol("apostrophe")
symbol_backquote = make_symbol("backquote")
symbol_comma_state = make_symbol("comma-state")
symbol_hash_prefix_state = make_symbol("hash-prefix-state")
symbol_string_state = make_symbol("string-state")
symbol_identifier_state = make_symbol("identifier-state")
symbol_signed_state = make_symbol("signed-state")
symbol_decimal_point_state = make_symbol("decimal-point-state")
symbol_whole_number_state = make_symbol("whole-number-state")
symbol_comma_at = make_symbol("comma-at")
symbol_comma = make_symbol("comma")
symbol_character_state = make_symbol("character-state")
symbol_lvector = make_symbol("lvector")
symbol_alphabetic_character_state = make_symbol("alphabetic-character-state")
symbol_named_character_state = make_symbol("named-character-state")
symbol_string_escape_state = make_symbol("string-escape-state")
symbol_signed_decimal_point_state = make_symbol("signed-decimal-point-state")
symbol_fractional_number_state = make_symbol("fractional-number-state")
symbol_rational_number_state = make_symbol("rational-number-state")
symbol_suffix_state = make_symbol("suffix-state")
symbol_rational_number_state_star = make_symbol("rational-number-state*")
symbol_signed_exponent_state = make_symbol("signed-exponent-state")
symbol_exponent_state = make_symbol("exponent-state")
symbol_apply_state = make_symbol("apply-state")
symbol_unquote = make_symbol("unquote")
symbol_unquote_splicing = make_symbol("unquote-splicing")
symbol_environment = make_symbol("environment")
symbol_func = make_symbol("func")
symbol_define_b = make_symbol("define!")
symbol_let_star = make_symbol("let*")
symbol_case = make_symbol("case")
symbol_record_case = make_symbol("record-case")
symbol_try = make_symbol("try")
symbol_catch = make_symbol("catch")
symbol_finally = make_symbol("finally")
symbol_raise = make_symbol("raise")
symbol_define_syntax = make_symbol("define-syntax")
symbol_choose = make_symbol("choose")
symbol_define_datatype = make_symbol("define-datatype")
symbol_trace_lambda = make_symbol("trace-lambda")
symbol_λ = make_symbol("λ")
symbol_pattern_macro = make_symbol("pattern-macro")
symbol_callback = make_symbol("callback")
symbol_aunparse = make_symbol("aunparse")
symbol_goodbye = make_symbol("goodbye")
symbol_exception_object = make_symbol("exception-object")
symbol_dotdotdot = make_symbol("...")
symbol_application = make_symbol("application")
symbol_unknown = make_symbol("unknown")
symbol_macro_generated_exp = make_symbol("macro-generated-exp")
symbol_colon = make_symbol(":")
symbol_b_procedure_d = make_symbol("<procedure>")
symbol_b_environment_d = make_symbol("<environment>")
symbol_b_exception_d = make_symbol("<exception>")
symbol_Map = make_symbol("map")
symbol_p = make_symbol("%")
symbol_multiply = make_symbol("*")
symbol_plus = make_symbol("+")
symbol_minus = make_symbol("-")
symbol_divide = make_symbol("/")
symbol___ = make_symbol("//")
symbol_LessThan = make_symbol("<")
symbol_LessThanEqual = make_symbol("<=")
symbol_GreaterThan = make_symbol(">")
symbol_GreaterThanEqual = make_symbol(">=")
symbol_SCHEMEPATH = make_symbol("SCHEMEPATH")
symbol_abort = make_symbol("abort")
symbol_abs = make_symbol("abs")
symbol_assert = make_symbol("assert")
symbol_assq = make_symbol("assq")
symbol_assv = make_symbol("assv")
symbol_atom_q = make_symbol("atom?")
symbol_boolean_q = make_symbol("boolean?")
symbol_box = make_symbol("box")
symbol_box_q = make_symbol("box?")
symbol_caaaar = make_symbol("caaaar")
symbol_caaadr = make_symbol("caaadr")
symbol_caaar = make_symbol("caaar")
symbol_caadar = make_symbol("caadar")
symbol_caaddr = make_symbol("caaddr")
symbol_caadr = make_symbol("caadr")
symbol_caar = make_symbol("caar")
symbol_cadaar = make_symbol("cadaar")
symbol_cadadr = make_symbol("cadadr")
symbol_cadar = make_symbol("cadar")
symbol_caddar = make_symbol("caddar")
symbol_cadddr = make_symbol("cadddr")
symbol_caddr = make_symbol("caddr")
symbol_cadr = make_symbol("cadr")
symbol_call_with_current_continuation = make_symbol("call-with-current-continuation")
symbol_call_cc = make_symbol("call/cc")
symbol_cd = make_symbol("cd")
symbol_cdaaar = make_symbol("cdaaar")
symbol_cdaadr = make_symbol("cdaadr")
symbol_cdaar = make_symbol("cdaar")
symbol_cdadar = make_symbol("cdadar")
symbol_cdaddr = make_symbol("cdaddr")
symbol_cdadr = make_symbol("cdadr")
symbol_cdar = make_symbol("cdar")
symbol_cddaar = make_symbol("cddaar")
symbol_cddadr = make_symbol("cddadr")
symbol_cddar = make_symbol("cddar")
symbol_cdddar = make_symbol("cdddar")
symbol_cddddr = make_symbol("cddddr")
symbol_cdddr = make_symbol("cdddr")
symbol_cddr = make_symbol("cddr")
symbol_char_to_integer = make_symbol("char->integer")
symbol_char_to_string = make_symbol("char->string")
symbol_char_alphabetic_q = make_symbol("char-alphabetic?")
symbol_char_numeric_q = make_symbol("char-numeric?")
symbol_char_whitespace_q = make_symbol("char-whitespace?")
symbol_char_is__q = make_symbol("char=?")
symbol_char_q = make_symbol("char?")
symbol_clear_unit_tests = make_symbol("clear-unit-tests")
symbol_current_directory = make_symbol("current-directory")
symbol_current_environment = make_symbol("current-environment")
symbol_current_time = make_symbol("current-time")
symbol_cut = make_symbol("cut")
symbol_dict = make_symbol("dict")
symbol_dir = make_symbol("dir")
symbol_display = make_symbol("display")
symbol_div = make_symbol("div")
symbol_equal_q = make_symbol("equal?")
symbol_eqv_q = make_symbol("eqv?")
symbol_eval = make_symbol("eval")
symbol_eval_ast = make_symbol("eval-ast")
symbol_even_q = make_symbol("even?")
symbol_exit = make_symbol("exit")
symbol_expt = make_symbol("expt")
symbol_float = make_symbol("float")
symbol_for_each = make_symbol("for-each")
symbol_format = make_symbol("format")
symbol_get_attr = make_symbol("get-attr")
symbol_get_completions = make_symbol("get-completions")
symbol_get_item = make_symbol("get-item")
symbol_get_stack_trace = make_symbol("get-stack-trace")
symbol_get_exception_message = make_symbol("get-exception-message")
symbol_globals = make_symbol("globals")
symbol_has_attr_q = make_symbol("has-attr?")
symbol_has_item_q = make_symbol("has-item?")
symbol_host_environment = make_symbol("host-environment")
symbol_import = make_symbol("import")
symbol_import_as = make_symbol("import-as")
symbol_import_from = make_symbol("import-from")
symbol_int_ = make_symbol("int")
symbol_integer_to_char = make_symbol("integer->char")
symbol_iter_q = make_symbol("iter?")
symbol_list_to_string = make_symbol("list->string")
symbol_list_ref = make_symbol("list-ref")
symbol_list_q = make_symbol("list?")
symbol_load = make_symbol("load")
symbol_load_as = make_symbol("load-as")
symbol_macros = make_symbol("macros")
symbol_make_set = make_symbol("make-set")
symbol_make_vector = make_symbol("make-vector")
symbol_max = make_symbol("max")
symbol_member = make_symbol("member")
symbol_memv = make_symbol("memv")
symbol_min = make_symbol("min")
symbol_mod = make_symbol("mod")
symbol_modulo = make_symbol("modulo")
symbol_newline = make_symbol("newline")
symbol_null_q = make_symbol("null?")
symbol_number_to_string = make_symbol("number->string")
symbol_number_q = make_symbol("number?")
symbol_odd_q = make_symbol("odd?")
symbol_parse = make_symbol("parse")
symbol_parse_string = make_symbol("parse-string")
symbol_print = make_symbol("print")
symbol_printf = make_symbol("printf")
symbol_procedure_q = make_symbol("procedure?")
symbol_property = make_symbol("property")
symbol_python_eval = make_symbol("python-eval")
symbol_python_exec = make_symbol("python-exec")
symbol_quit = make_symbol("quit")
symbol_quotient = make_symbol("quotient")
symbol_rac = make_symbol("rac")
symbol_random = make_symbol("random")
symbol_Range = make_symbol("range")
symbol_rdc = make_symbol("rdc")
symbol_read_string = make_symbol("read-string")
symbol_remainder = make_symbol("remainder")
symbol_require = make_symbol("require")
symbol_reset_toplevel_env = make_symbol("reset-toplevel-env")
symbol_reverse = make_symbol("reverse")
symbol_round = make_symbol("round")
symbol_set_attr_b = make_symbol("set-attr!")
symbol_set_car_b = make_symbol("set-car!")
symbol_set_cdr_b = make_symbol("set-cdr!")
symbol_set_item_b = make_symbol("set-item!")
symbol_snoc = make_symbol("snoc")
symbol_sort = make_symbol("sort")
symbol_sqrt = make_symbol("sqrt")
symbol_string_to_list = make_symbol("string->list")
symbol_string_to_number = make_symbol("string->number")
symbol_string_to_symbol = make_symbol("string->symbol")
symbol_string_append = make_symbol("string-append")
symbol_string_join = make_symbol("string-join")
symbol_string_length = make_symbol("string-length")
symbol_string_ref = make_symbol("string-ref")
symbol_string_split = make_symbol("string-split")
symbol_stringLessThan_q = make_symbol("string<?")
symbol_string_is__q = make_symbol("string=?")
symbol_string_q = make_symbol("string?")
symbol_substring = make_symbol("substring")
symbol_symbol_to_string = make_symbol("symbol->string")
symbol_symbol_q = make_symbol("symbol?")
symbol_typeof = make_symbol("typeof")
symbol_unbox = make_symbol("unbox")
symbol_unparse = make_symbol("unparse")
symbol_unparse_procedure = make_symbol("unparse-procedure")
symbol_use_jit = make_symbol("use-jit")
symbol_use_lexical_address = make_symbol("use-lexical-address")
symbol_use_stack_trace = make_symbol("use-stack-trace")
symbol_use_tracing = make_symbol("use-tracing")
symbol_vector = make_symbol("vector")
symbol_vector_to_list = make_symbol("vector->list")
symbol_vector_length = make_symbol("vector-length")
symbol_vector_ref = make_symbol("vector-ref")
symbol_vector_set_b = make_symbol("vector-set!")
symbol_vector_q = make_symbol("vector?")
symbol_void = make_symbol("void")
symbol_zero_q = make_symbol("zero?")
symbol_get_symbol = make_symbol("get-symbol")
symbol_empty = make_symbol("empty")
symbol_instantiate_hat = make_symbol("instantiate^")
symbol_substitution = make_symbol("substitution")
symbol_apply_sub_hat = make_symbol("apply-sub^")
symbol_atom = make_symbol("atom")
symbol_pair = make_symbol("pair")
symbol_help = make_symbol("help")
symbol_define_tests = make_symbol("define-tests")
symbol_run_tests = make_symbol("run-tests")
symbol_b__q_q_q_d = make_symbol("<???>")
symbol_b_fail_d = make_symbol("<fail>")
symbol_b_handler_d = make_symbol("<handler>")
symbol_b_void_d = make_symbol("<void>")
symbol_exiting = make_symbol("exiting")
symbol_the = make_symbol("the")
symbol_interpreter = make_symbol("interpreter")
symbol_b_typecolonenvironment_d = make_symbol("<type:environment>")
symbol_b_typecolonexception_d = make_symbol("<type:exception>")
symbol_b_typecolonprocedure_d = make_symbol("<type:procedure>")
symbol_b_typecolonnumber_d = make_symbol("<type:number>")
symbol_b_typecolonsymbol_d = make_symbol("<type:symbol>")
symbol_b_typecolonpair_d = make_symbol("<type:pair>")
symbol_b_typecolonstring_d = make_symbol("<type:string>")
symbol_b_typecolonnull_d = make_symbol("<type:null>")
symbol_b_typecolonboolean_d = make_symbol("<type:boolean>")
symbol_b_typecolonchar_d = make_symbol("<type:char>")
symbol_b_typecolonvector_d = make_symbol("<type:vector>")
symbol_b_typecolonbox_d = make_symbol("<type:box>")
symbol_b_typecolonunknown_d = make_symbol("<type:unknown>")

def lit_aexp(*args):
    args = List(*args)
    return cons(symbol_lit_aexp, args)

def var_aexp(*args):
    args = List(*args)
    return cons(symbol_var_aexp, args)

def lexical_address_aexp(*args):
    args = List(*args)
    return cons(symbol_lexical_address_aexp, args)

def if_aexp(*args):
    args = List(*args)
    return cons(symbol_if_aexp, args)

def help_aexp(*args):
    args = List(*args)
    return cons(symbol_help_aexp, args)

def association_aexp(*args):
    args = List(*args)
    return cons(symbol_association_aexp, args)

def assign_aexp(*args):
    args = List(*args)
    return cons(symbol_assign_aexp, args)

def func_aexp(*args):
    args = List(*args)
    return cons(symbol_func_aexp, args)

def callback_aexp(*args):
    args = List(*args)
    return cons(symbol_callback_aexp, args)

def define_aexp(*args):
    args = List(*args)
    return cons(symbol_define_aexp, args)

def define_b_aexp(*args):
    args = List(*args)
    return cons(symbol_define_b_aexp, args)

def define_syntax_aexp(*args):
    args = List(*args)
    return cons(symbol_define_syntax_aexp, args)

def define_syntax_transformer_aexp(*args):
    args = List(*args)
    return cons(symbol_define_syntax_transformer_aexp, args)

def define_tests_aexp(*args):
    args = List(*args)
    return cons(symbol_define_tests_aexp, args)

def run_tests_aexp(*args):
    args = List(*args)
    return cons(symbol_run_tests_aexp, args)

def begin_aexp(*args):
    args = List(*args)
    return cons(symbol_begin_aexp, args)

def lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_lambda_aexp, args)

def mu_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_mu_lambda_aexp, args)

def trace_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_trace_lambda_aexp, args)

def mu_trace_lambda_aexp(*args):
    args = List(*args)
    return cons(symbol_mu_trace_lambda_aexp, args)

def app_aexp(*args):
    args = List(*args)
    return cons(symbol_app_aexp, args)

def try_catch_aexp(*args):
    args = List(*args)
    return cons(symbol_try_catch_aexp, args)

def try_finally_aexp(*args):
    args = List(*args)
    return cons(symbol_try_finally_aexp, args)

def try_catch_finally_aexp(*args):
    args = List(*args)
    return cons(symbol_try_catch_finally_aexp, args)

def raise_aexp(*args):
    args = List(*args)
    return cons(symbol_raise_aexp, args)

def choose_aexp(*args):
    args = List(*args)
    return cons(symbol_choose_aexp, args)

pc = symbol_undefined
aclauses_reg = symbol_undefined
action_reg = symbol_undefined
adatum_list_reg = symbol_undefined
adatum_reg = symbol_undefined
ap1_reg = symbol_undefined
ap2_reg = symbol_undefined
ap_reg = symbol_undefined
apair1_reg = symbol_undefined
apair2_reg = symbol_undefined
args_reg = symbol_undefined
assertions_reg = symbol_undefined
associations_reg = symbol_undefined
avar_reg = symbol_undefined
ax_reg = symbol_undefined
bindings_reg = symbol_undefined
bodies_reg = symbol_undefined
buffer_reg = symbol_undefined
cdrs_reg = symbol_undefined
char_reg = symbol_undefined
chars_reg = symbol_undefined
clauses_reg = symbol_undefined
components_reg = symbol_undefined
contours_reg = symbol_undefined
datum_reg = symbol_undefined
depth_reg = symbol_undefined
dk_reg = symbol_undefined
elements_reg = symbol_undefined
env2_reg = symbol_undefined
env_reg = symbol_undefined
exception_reg = symbol_undefined
exp_reg = symbol_undefined
expected_terminator_reg = symbol_undefined
exps_reg = symbol_undefined
fail_reg = symbol_undefined
fields_reg = symbol_undefined
filename_reg = symbol_undefined
filenames_reg = symbol_undefined
final_reg = symbol_undefined
frames_reg = symbol_undefined
generator_reg = symbol_undefined
gk_reg = symbol_undefined
handler_reg = symbol_undefined
i_reg = symbol_undefined
id_reg = symbol_undefined
info_reg = symbol_undefined
input_reg = symbol_undefined
items_reg = symbol_undefined
iterator_reg = symbol_undefined
k2_reg = symbol_undefined
k_reg = symbol_undefined
keyword_reg = symbol_undefined
line_reg = symbol_undefined
list1_reg = symbol_undefined
list2_reg = symbol_undefined
lists_reg = symbol_undefined
ls1_reg = symbol_undefined
ls2_reg = symbol_undefined
ls_reg = symbol_undefined
lst_reg = symbol_undefined
macro_reg = symbol_undefined
module_reg = symbol_undefined
msg_reg = symbol_undefined
name_reg = symbol_undefined
nums_reg = symbol_undefined
offset_reg = symbol_undefined
p1_reg = symbol_undefined
p2_reg = symbol_undefined
pair1_reg = symbol_undefined
pair2_reg = symbol_undefined
path_reg = symbol_undefined
paths_reg = symbol_undefined
pattern_reg = symbol_undefined
pred_reg = symbol_undefined
proc_reg = symbol_undefined
procs_reg = symbol_undefined
right_reg = symbol_undefined
s_reg = symbol_undefined
senv_reg = symbol_undefined
sep_reg = symbol_undefined
sexps_reg = symbol_undefined
sk_reg = symbol_undefined
src_reg = symbol_undefined
start_time_reg = symbol_undefined
sum_reg = symbol_undefined
test_name_reg = symbol_undefined
test_reg = symbol_undefined
tests_reg = symbol_undefined
token_type_reg = symbol_undefined
tokens_reg = symbol_undefined
v1_reg = symbol_undefined
v2_reg = symbol_undefined
value1_reg = symbol_undefined
value2_reg = symbol_undefined
value3_reg = symbol_undefined
value4_reg = symbol_undefined
value_reg = symbol_undefined
var_info_reg = symbol_undefined
var_reg = symbol_undefined
variant_reg = symbol_undefined
variants_reg = symbol_undefined
vars_reg = symbol_undefined
verbose_reg = symbol_undefined
where_reg = symbol_undefined
wrong_reg = symbol_undefined
x_reg = symbol_undefined
y_reg = symbol_undefined
temp_2 = symbol_undefined
temp_3 = symbol_undefined
temp_4 = symbol_undefined
temp_1 = symbol_undefined
def b_cont_1_d(chars, fail, k):
    global k_reg, pc, value1_reg, value2_reg, value3_reg
    value3_reg = fail
    value2_reg = chars
    value1_reg = value_reg
    k_reg = k
    pc = apply_cont3

def b_cont_2_d(v1, info, k):
    global k_reg, pc, value_reg
    value_reg = List(pair_tag, v1, value_reg, info)
    k_reg = k
    pc = apply_cont

def b_cont_3_d(x, info, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_2_d, value_reg, info, k)
    info_reg = symbol_none
    x_reg = (x).cdr
    pc = annotate_cps

def b_cont_4_d(v1, k):
    global k_reg, pc, value_reg
    value_reg = cons(v1, value_reg)
    k_reg = k
    pc = apply_cont

def b_cont_5_d(x, k):
    global k_reg, pc, x_reg
    k_reg = make_cont(b_cont_4_d, value_reg, k)
    x_reg = (x).cdr
    pc = unannotate_cps

def b_cont_6_d(k):
    global k_reg, pc, value_reg
    value_reg = list_to_vector(value_reg)
    k_reg = k
    pc = apply_cont

def b_cont_7_d(x, k):
    global k_reg, pc, x_reg
    k_reg = make_cont(b_cont_4_d, value_reg, k)
    x_reg = (x).cdr.cdr.car
    pc = unannotate_cps

def b_cont_8_d(end, tokens_left, fail, k):
    global k_reg, pc, value1_reg, value2_reg, value3_reg, value4_reg
    value4_reg = fail
    value3_reg = tokens_left
    value2_reg = end
    value1_reg = value_reg
    k_reg = k
    pc = apply_cont4

def b_cont_9_d(end, tokens, fail, k):
    global k_reg, pc, value1_reg, value2_reg, value3_reg, value4_reg
    value4_reg = fail
    value3_reg = rest_of(tokens)
    value2_reg = end
    value1_reg = value_reg
    k_reg = k
    pc = apply_cont4

def b_cont_10_d(src, start, tokens, handler, fail, k):
    global fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont4(b_cont4_3_d, src, start, value_reg, k)
    fail_reg = fail
    handler_reg = handler
    src_reg = src
    tokens_reg = rest_of(tokens)
    pc = read_sexp

def b_cont_11_d():
    global final_reg, pc
    final_reg = value_reg
    pc = pc_halt_signal

def b_cont_12_d(adatum, senv, info, handler, fail, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    name = untag_atom_hat(cadr_hat(adatum))
    formals_list = (value_reg if (list_q(value_reg)) and (not(association_q(value_reg))) else cons(last(value_reg), head(value_reg)))
    k_reg = make_cont2(b_cont2_9_d, name, value_reg, info, k)
    fail_reg = fail
    handler_reg = handler
    senv_reg = cons(formals_list, senv)
    adatum_list_reg = cdddr_hat(adatum)
    pc = aparse_all

def b_cont_13_d(adatum, senv, info, handler, fail, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    formals_list = (value_reg if (list_q(value_reg)) and (not(association_q(value_reg))) else cons(last(value_reg), head(value_reg)))
    k_reg = make_cont2(b_cont2_18_d, value_reg, info, k)
    fail_reg = fail
    handler_reg = handler
    senv_reg = cons(formals_list, senv)
    adatum_list_reg = cddr_hat(adatum)
    pc = aparse_all

def b_cont_14_d(senv, info, handler, fail, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = k
    fail_reg = fail
    handler_reg = handler
    senv_reg = senv
    adatum_reg = replace_info(value_reg, info)
    pc = aparse

def b_cont_15_d(senv, info, handler, fail, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_14_d, senv, info, handler, fail, k)
    info_reg = symbol_none
    x_reg = value_reg
    pc = annotate_cps

def b_cont_16_d(aclauses, name, info, fail, k):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail
    value1_reg = define_syntax_aexp(name, value_reg, aclauses, info)
    k_reg = k
    pc = apply_cont2

def b_cont_17_d(adatum, senv, info, handler, fail, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    if (original_source_info_q(adatum) is not False):
        k_reg = k
        fail_reg = fail
        handler_reg = handler
        senv_reg = senv
        adatum_reg = replace_info(value_reg, snoc(symbol_quasiquote, info))
        pc = aparse
    else:
        k_reg = k
        fail_reg = fail
        handler_reg = handler
        senv_reg = senv
        adatum_reg = replace_info(value_reg, info)
        pc = aparse

def b_cont_18_d(adatum, senv, info, handler, fail, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_17_d, adatum, senv, info, handler, fail, k)
    info_reg = symbol_none
    x_reg = value_reg
    pc = annotate_cps

def b_cont_19_d(info, fail, k):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail
    value1_reg = lit_aexp(value_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont_20_d(info, fail, k):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail
    value1_reg = lit_aexp((value_reg).cdr.car, info)
    k_reg = k
    pc = apply_cont2

def b_cont_21_d(tests, fail, k):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail
    value1_reg = cons(value_reg, tests)
    k_reg = k
    pc = apply_cont2

def b_cont_22_d(msg, info, handler, fail):
    global exception_reg, fail_reg, handler_reg, pc
    fail_reg = fail
    exception_reg = make_exception("ParseError", format("~a ~a", msg, value_reg), get_srcfile(info), get_start_line(info), get_start_char(info))
    handler_reg = handler
    pc = apply_handler2

def b_cont_23_d(bodies2, formals, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_lambda_no_defines), append(List(formals), List(append(List(symbol_letrec), append(List(value_reg), at_hat(bodies2))))))
    k_reg = k
    pc = apply_cont

def b_cont_24_d(bodies2, name, formals, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_trace_lambda_no_defines), append(List(name), append(List(formals), List(append(List(symbol_letrec), append(List(value_reg), at_hat(bodies2)))))))
    k_reg = k
    pc = apply_cont

def b_cont_25_d(adatum, bodies, handler, fail, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, msg_reg, pc, value1_reg, value2_reg
    if (value_reg is not False):
        fail_reg = fail
        handler_reg = handler
        adatum_reg = adatum
        msg_reg = "misplaced define in"
        pc = aparse_error
    else:
        value2_reg = bodies
        value1_reg = symbol_emptylist
        k_reg = k
        pc = apply_cont2

def b_cont_26_d(defines, handler, fail, k):
    return get_define_var_and_exp_hat((defines).car, handler, fail, make_cont2(b_cont2_45_d, value_reg, k))

def b_cont_27_d(bindings, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_let), append(List(List(car_hat(bindings))), List(value_reg)))
    k_reg = k
    pc = apply_cont

def b_cont_28_d(exp, r, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_let), append(List(List(append(List(r), List(exp)))), List(append(List(symbol_cond), value_reg))))
    k_reg = k
    pc = apply_cont

def b_cont_29_d(clauses, var, k):
    global k_reg, pc, value_reg
    clause = car_hat(clauses)
    if (eq_q_hat(car_hat(clause), symbol_else) is not False):
        value_reg = cons(clause, value_reg)
        k_reg = k
        pc = apply_cont
    else:
        if (symbol_q_hat(car_hat(clause)) is not False):
            value_reg = cons(append(List(append(List(symbol_eq_q), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            k_reg = k
            pc = apply_cont
        else:
            value_reg = cons(append(List(append(List(symbol_memq), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            k_reg = k
            pc = apply_cont

def b_cont_30_d(clauses, var, k):
    global k_reg, pc, value_reg
    clause = car_hat(clauses)
    if (eq_q_hat(car_hat(clause), symbol_else) is not False):
        value_reg = cons(append(List(symbol_else), at_hat(cdr_hat(clause))), value_reg)
        k_reg = k
        pc = apply_cont
    else:
        if (symbol_q_hat(car_hat(clause)) is not False):
            value_reg = cons(append(List(append(List(symbol_eq_q), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause))))), List(append(List(symbol_cdr), List(var))))))), value_reg)
            k_reg = k
            pc = apply_cont
        else:
            value_reg = cons(append(List(append(List(symbol_memq), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause))))), List(append(List(symbol_cdr), List(var))))))), value_reg)
            k_reg = k
            pc = apply_cont

def b_cont_31_d(fields, name, k2):
    global k_reg, pc, value1_reg, value2_reg
    constructor_def = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(symbol_args), List(append(List(symbol_if), append(List(append(List(symbol_numeric_equal), append(List(append(List(symbol_length), List(symbol_args))), List(length_hat(fields))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), List("wrong number of arguments")))))))))))))
    value2_reg = constructor_def
    value1_reg = name
    k_reg = k2
    pc = apply_cont2

def b_cont_32_d(cdrs, fields, name, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_if), append(List(append(List(cadar_hat(fields)), List(append(List(symbol_car), List(cdrs))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), append(List("~a is not of type ~a"), append(List(append(List(symbol_car), List(cdrs))), List(append(List(symbol_quote), List(cadar_hat(fields))))))))))))
    k_reg = k
    pc = apply_cont

def b_cont_33_d(exp, r, type_name, type_tester_name, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_let), append(List(List(append(List(r), List(exp)))), List(append(List(symbol_if), append(List(append(List(symbol_not), List(append(List(type_tester_name), List(r))))), append(List(append(List(symbol_error), append(List(append(List(symbol_quote), List(symbol_cases))), append(List("~a is not a valid ~a"), append(List(r), List(append(List(symbol_quote), List(type_name)))))))), List(append(List(symbol_cond), value_reg))))))))
    k_reg = k
    pc = apply_cont

def b_cont_34_d(adatum, macro_keyword, fail, k):
    global k_reg, pc, value1_reg, value2_reg
    if (has_source_info_q(value_reg) is not False):
        value2_reg = fail
        value1_reg = value_reg
        k_reg = k
        pc = apply_cont2
    else:
        info = get_source_info(adatum)
        if (original_source_info_q(adatum) is not False):
            value2_reg = fail
            value1_reg = replace_info(value_reg, snoc(macro_keyword, info))
            k_reg = k
            pc = apply_cont2
        else:
            value2_reg = fail
            value1_reg = replace_info(value_reg, info)
            k_reg = k
            pc = apply_cont2

def b_cont_35_d(adatum, macro_keyword, fail, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_34_d, adatum, macro_keyword, fail, k)
    info_reg = symbol_none
    x_reg = value_reg
    pc = annotate_cps

def b_cont_36_d(aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k):
    global aclauses_reg, adatum_reg, ap_reg, clauses_reg, fail_reg, handler_reg, k2_reg, k_reg, pattern_reg, pc, s_reg
    if (value_reg is not False):
        k2_reg = make_cont2(b_cont2_52_d, fail, k)
        ap_reg = right_apattern
        s_reg = value_reg
        pattern_reg = right_pattern
        pc = instantiate_hat
    else:
        k_reg = k
        fail_reg = fail
        handler_reg = handler
        adatum_reg = adatum
        aclauses_reg = cdr_hat(aclauses)
        clauses_reg = (clauses).cdr
        pc = process_macro_clauses_hat

def b_cont_37_d(aclauses, adatum, clauses, left_apattern, left_pattern, right_apattern, right_pattern, handler, fail, k):
    global ap1_reg, ap2_reg, k_reg, p1_reg, p2_reg, pc
    k_reg = make_cont(b_cont_36_d, aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k)
    ap2_reg = adatum
    ap1_reg = left_apattern
    p2_reg = value_reg
    p1_reg = left_pattern
    pc = unify_patterns_hat

def b_cont_38_d(v1, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_append), append(List(v1), List(value_reg)))
    k_reg = k
    pc = apply_cont

def b_cont_39_d(ax, depth, k):
    global ax_reg, depth_reg, k_reg, pc
    k_reg = make_cont(b_cont_38_d, value_reg, k)
    depth_reg = depth
    ax_reg = cdr_hat(ax)
    pc = qq_expand_cps

def b_cont_40_d(k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_list_to_vector), List(value_reg))
    k_reg = k
    pc = apply_cont

def b_cont_41_d(depth, k):
    global ax_reg, depth_reg, k_reg, pc
    k_reg = make_cont(b_cont_40_d, k)
    depth_reg = depth
    ax_reg = value_reg
    pc = qq_expand_cps

def b_cont_42_d(k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))
    k_reg = k
    pc = apply_cont

def b_cont_43_d(ax, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))
    k_reg = k
    pc = apply_cont

def b_cont_44_d(k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_List), List(value_reg))
    k_reg = k
    pc = apply_cont

def b_cont_45_d(v1, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_List), List(append(List(symbol_append), append(List(v1), List(value_reg)))))
    k_reg = k
    pc = apply_cont

def b_cont_46_d(ax, depth, k):
    global ax_reg, depth_reg, k_reg, pc
    k_reg = make_cont(b_cont_45_d, value_reg, k)
    depth_reg = depth
    ax_reg = cdr_hat(ax)
    pc = qq_expand_cps

def b_cont_47_d(k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))))
    k_reg = k
    pc = apply_cont

def b_cont_48_d(ax, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))))
    k_reg = k
    pc = apply_cont

def b_cont_49_d(proc, env, info, handler, fail, k2):
    global args_reg, env2_reg, fail_reg, handler_reg, info_reg, k2_reg, pc, proc_reg
    k2_reg = make_cont2(b_cont2_64_d, k2)
    fail_reg = fail
    handler_reg = handler
    info_reg = info
    env2_reg = env
    args_reg = List(value_reg)
    proc_reg = proc
    pc = apply_proc

def b_cont_50_d(handler, fail, k2):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_92_d, handler, k2)
    fail_reg = fail
    handler_reg = handler
    senv_reg = initial_contours(toplevel_env)
    adatum_reg = value_reg
    pc = aparse

def b_cont_51_d(args, handler, fail, k2):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_93_d, args, handler, k2)
    fail_reg = fail
    handler_reg = handler
    senv_reg = initial_contours((args).cdr.car)
    adatum_reg = value_reg
    pc = aparse

def b_cont_52_d(handler, fail, k2):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = k2
    fail_reg = fail
    handler_reg = handler
    senv_reg = initial_contours(toplevel_env)
    adatum_reg = value_reg
    pc = aparse

def b_cont_53_d(fail, k2):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail
    value1_reg = value_reg
    k_reg = k2
    pc = apply_cont2

def b_cont_54_d(x, y, k):
    global k_reg, pc, value_reg, x_reg, y_reg
    if (value_reg is not False):
        k_reg = k
        y_reg = (y).cdr
        x_reg = (x).cdr
        pc = equal_objects_q
    else:
        value_reg = False
        k_reg = k
        pc = apply_cont

def b_cont_55_d(i, v1, v2, k):
    global i_reg, k_reg, pc, v1_reg, v2_reg, value_reg
    if (value_reg is not False):
        k_reg = k
        i_reg = (i) - (1)
        v2_reg = v2
        v1_reg = v1
        pc = equal_vectors_q
    else:
        value_reg = False
        k_reg = k
        pc = apply_cont

def b_cont_56_d(ls, x, y, info, handler, fail, k):
    global fail_reg, handler_reg, info_reg, k_reg, ls_reg, pc, value1_reg, value2_reg, x_reg, y_reg
    if (value_reg is not False):
        value2_reg = fail
        value1_reg = y
        k_reg = k
        pc = apply_cont2
    else:
        k_reg = k
        fail_reg = fail
        handler_reg = handler
        info_reg = info
        ls_reg = ls
        y_reg = (y).cdr
        x_reg = x
        pc = member_loop

def b_cont_57_d(pattern, var, k):
    global k_reg, pattern_reg, pc, value_reg, var_reg
    if (value_reg is not False):
        value_reg = True
        k_reg = k
        pc = apply_cont
    else:
        k_reg = k
        pattern_reg = (pattern).cdr
        var_reg = var
        pc = occurs_q

def b_cont_58_d(ap2, p1, p2, k):
    global k_reg, pc, value_reg
    if (value_reg is not False):
        value_reg = False
        k_reg = k
        pc = apply_cont
    else:
        value_reg = make_sub(symbol_unit, p1, p2, ap2)
        k_reg = k
        pc = apply_cont

def b_cont_59_d(s_car, k):
    global k_reg, pc, value_reg
    if (not(value_reg) is not False):
        value_reg = False
        k_reg = k
        pc = apply_cont
    else:
        value_reg = make_sub(symbol_composite, s_car, value_reg)
        k_reg = k
        pc = apply_cont

def b_cont_60_d(apair1, apair2, pair1, pair2, k):
    global ap_reg, k2_reg, k_reg, pattern_reg, pc, s_reg, value_reg
    if (not(value_reg) is not False):
        value_reg = False
        k_reg = k
        pc = apply_cont
    else:
        k2_reg = make_cont2(b_cont2_121_d, apair2, pair2, value_reg, k)
        ap_reg = cdr_hat(apair1)
        s_reg = value_reg
        pattern_reg = (pair1).cdr
        pc = instantiate_hat

def b_cont2_1_d(token, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(token, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_2_d():
    global final_reg, pc
    final_reg = value1_reg
    pc = pc_halt_signal

def b_cont2_3_d(k):
    global k_reg, pc, value1_reg
    value1_reg = binding_value(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_4_d(k):
    global k_reg, pc, value1_reg
    value1_reg = dlr_env_lookup(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_5_d(v1, info, k):
    global k_reg, pc, value1_reg
    value1_reg = app_aexp(v1, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_6_d(adatum, senv, info, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_5_d, value1_reg, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_list_reg = cdr_hat(adatum)
    pc = aparse_all

def b_cont2_7_d(info, k):
    global k_reg, pc, value1_reg
    value1_reg = raise_aexp(value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_8_d(info, k):
    global k_reg, pc, value1_reg
    value1_reg = choose_aexp(value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_9_d(name, formals, info, k):
    global k_reg, pc, value1_reg
    if ((list_q(formals)) and (not(association_q(formals))) is not False):
        value1_reg = trace_lambda_aexp(name, formals, value1_reg, info)
        k_reg = k
        pc = apply_cont2
    else:
        value1_reg = mu_trace_lambda_aexp(name, head(formals), last(formals), value1_reg, info)
        k_reg = k
        pc = apply_cont2

def b_cont2_10_d(cexps, cvar, body, info, k):
    global k_reg, pc, value1_reg
    value1_reg = try_catch_finally_aexp(body, cvar, cexps, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_11_d(adatum, cvar, senv, body, info, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_10_d, value1_reg, cvar, body, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_list_reg = try_catch_finally_exps_hat(adatum)
    pc = aparse_all

def b_cont2_12_d(adatum, senv, info, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    cvar = catch_var_hat(adatum)
    k_reg = make_cont2(b_cont2_11_d, adatum, cvar, senv, value1_reg, info, handler, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = cons(List(cvar), senv)
    adatum_list_reg = catch_exps_hat(adatum)
    pc = aparse_all

def b_cont2_13_d(cvar, body, info, k):
    global k_reg, pc, value1_reg
    value1_reg = try_catch_aexp(body, cvar, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_14_d(adatum, senv, info, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    cvar = catch_var_hat(adatum)
    k_reg = make_cont2(b_cont2_13_d, cvar, value1_reg, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = cons(List(cvar), senv)
    adatum_list_reg = catch_exps_hat(adatum)
    pc = aparse_all

def b_cont2_15_d(body, info, k):
    global k_reg, pc, value1_reg
    value1_reg = try_finally_aexp(body, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_16_d(adatum, senv, info, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_15_d, value1_reg, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_list_reg = try_finally_exps_hat(adatum)
    pc = aparse_all

def b_cont2_17_d(info, k):
    global k_reg, pc, value1_reg
    value1_reg = begin_aexp(value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_18_d(formals, info, k):
    global k_reg, pc, value1_reg
    if ((list_q(formals)) and (not(association_q(formals))) is not False):
        value1_reg = lambda_aexp(formals, value1_reg, info)
        k_reg = k
        pc = apply_cont2
    else:
        value1_reg = mu_lambda_aexp(head(formals), last(formals), value1_reg, info)
        k_reg = k
        pc = apply_cont2

def b_cont2_19_d(name, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_tests_aexp(name, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_20_d(k):
    global k_reg, pc, value1_reg
    value1_reg = run_tests_aexp(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_21_d(adatum, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_b_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_22_d(adatum, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_b_aexp(define_var_hat(adatum), "", value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_23_d(name, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_syntax_transformer_aexp(name, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_24_d(info, k):
    global k_reg, pc, value1_reg
    value1_reg = callback_aexp(value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_25_d(adatum, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_26_d(adatum, info, k):
    global k_reg, pc, value1_reg
    value1_reg = define_aexp(define_var_hat(adatum), "", value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_27_d(adatum, info, k):
    global k_reg, pc, value1_reg
    var_info = get_source_info(cadr_hat(adatum))
    value1_reg = association_aexp(untag_atom_hat(car_hat(adatum)), value1_reg, var_info, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_28_d(info, k):
    global k_reg, pc, value1_reg
    value1_reg = func_aexp(value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_29_d(adatum, info, k):
    global k_reg, pc, value1_reg
    var_info = get_source_info(cadr_hat(adatum))
    value1_reg = assign_aexp(untag_atom_hat(cadr_hat(adatum)), value1_reg, var_info, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_30_d(v1, info, k):
    global k_reg, pc, value1_reg
    value1_reg = if_aexp(v1, value1_reg, lit_aexp(False, symbol_none), info)
    k_reg = k
    pc = apply_cont2

def b_cont2_31_d(adatum, senv, info, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_30_d, value1_reg, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_reg = caddr_hat(adatum)
    pc = aparse

def b_cont2_32_d(v1, v2, info, k):
    global k_reg, pc, value1_reg
    value1_reg = if_aexp(v1, v2, value1_reg, info)
    k_reg = k
    pc = apply_cont2

def b_cont2_33_d(adatum, senv, v1, info, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_32_d, v1, value1_reg, info, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_reg = cadddr_hat(adatum)
    pc = aparse

def b_cont2_34_d(adatum, senv, info, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_33_d, adatum, senv, value1_reg, info, handler, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_reg = caddr_hat(adatum)
    pc = aparse

def b_cont2_35_d(senv, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_reg = value1_reg
    pc = aparse

def b_cont2_36_d(args, k):
    global k_reg, pc, x_reg
    k_reg = make_cont(b_cont_21_d, value1_reg, value2_reg, k)
    x_reg = car_hat(args)
    pc = unannotate_cps

def b_cont2_37_d(args, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(List(untag_atom_hat(car_hat(args))), value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_38_d(a, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(a, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_39_d(adatum_list, senv, handler, k):
    global adatum_list_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_38_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    adatum_list_reg = cdr_hat(adatum_list)
    pc = aparse_all

def b_cont2_40_d(v1, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(v1, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_41_d(senv, src, tokens_left, handler, k):
    global fail_reg, handler_reg, k_reg, pc, senv_reg, src_reg, tokens_reg
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    senv_reg = senv
    src_reg = src
    tokens_reg = tokens_left
    pc = aparse_sexps

def b_cont2_42_d(formals, handler, fail, k):
    global k_reg, pc, value_reg
    if (((value1_reg) is symbol_emptylist) is not False):
        value_reg = append(List(symbol_lambda_no_defines), append(List(formals), at_hat(value2_reg)))
        k_reg = k
        pc = apply_cont
    else:
        return create_letrec_bindings_hat(value1_reg, handler, fail, make_cont(b_cont_23_d, value2_reg, formals, k))

def b_cont2_43_d(name, formals, handler, fail, k):
    global k_reg, pc, value_reg
    if (((value1_reg) is symbol_emptylist) is not False):
        value_reg = append(List(symbol_trace_lambda_no_defines), append(List(name), append(List(formals), at_hat(value2_reg))))
        k_reg = k
        pc = apply_cont
    else:
        return create_letrec_bindings_hat(value1_reg, handler, fail, make_cont(b_cont_24_d, value2_reg, name, formals, k))

def b_cont2_44_d(bodies, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(car_hat(bodies), value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_45_d(bindings, k):
    global k_reg, pc, value_reg
    value_reg = cons(append(List(value1_reg), List(value2_reg)), bindings)
    k_reg = k
    pc = apply_cont

def b_cont2_46_d(bodies, k):
    global k_reg, pc, value_reg
    value_reg = append(List(symbol_let), append(List(value1_reg), append(value2_reg, at_hat(bodies))))
    k_reg = k
    pc = apply_cont

def b_cont2_47_d(procs, vars, k2):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = cons(append(List(symbol_set_b), append(List(car_hat(vars)), List(car_hat(procs)))), value2_reg)
    value1_reg = cons(append(List(car_hat(vars)), List(append(List(symbol_quote), List(symbol_undefined)))), value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_48_d(type_tester_name, k):
    global k_reg, pc, value_reg
    tester_def = append(List(symbol_define), append(List(type_tester_name), List(append(List(symbol_lambda), append(List(List(symbol_x)), List(append(List(symbol_and), append(List(append(List(symbol_pair_q), List(symbol_x))), List(append(List(symbol_not), List(append(List(symbol_not), List(append(List(symbol_memq), append(List(append(List(symbol_car), List(symbol_x))), List(append(List(symbol_quote), List(value1_reg))))))))))))))))))
    value_reg = append(List(symbol_begin), append(List(tester_def), value2_reg))
    k_reg = k
    pc = apply_cont

def b_cont2_49_d(def_, name, k2):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = cons(def_, value2_reg)
    value1_reg = cons(name, value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_50_d(variants, k2):
    global k2_reg, pc, variants_reg
    k2_reg = make_cont2(b_cont2_49_d, value2_reg, value1_reg, k2)
    variants_reg = cdr_hat(variants)
    pc = make_dd_variant_constructors_hat

def b_cont2_51_d(macro_keyword, k):
    global k_reg, pc, value1_reg
    value1_reg = replace_info(value1_reg, snoc(macro_keyword, get_source_info(value1_reg)))
    k_reg = k
    pc = apply_cont2

def b_cont2_52_d(fail, k):
    global k_reg, pc, value1_reg, value2_reg
    value1_reg = value2_reg
    value2_reg = fail
    k_reg = k
    pc = apply_cont2

def b_cont2_53_d():
    global _starlast_fail_star, final_reg, pc
    _starlast_fail_star = value2_reg
    final_reg = value1_reg
    pc = pc_halt_signal

def b_cont2_54_d():
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = REP_k
    fail_reg = value2_reg
    handler_reg = REP_handler
    env_reg = toplevel_env
    exp_reg = value1_reg
    pc = m

def b_cont2_55_d():
    global final_reg, pc
    final_reg = True
    pc = pc_halt_signal

def b_cont2_56_d():
    global fail_reg, handler_reg, k_reg, pc, senv_reg, src_reg, tokens_reg
    k_reg = make_cont2(b_cont2_55_d)
    fail_reg = value2_reg
    handler_reg = try_parse_handler
    senv_reg = initial_contours(toplevel_env)
    src_reg = "stdin"
    tokens_reg = value1_reg
    pc = aparse_sexps

def b_cont2_57_d(exp, k):
    global k_reg, pc
    handle_debug_info(exp, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_58_d(exp, k):
    global k_reg, pc
    pop_stack_trace_b(exp)
    k_reg = k
    pc = apply_cont2

def b_cont2_59_d(args, exp, env, info, handler, k):
    global args_reg, env2_reg, fail_reg, handler_reg, info_reg, k2_reg, k_reg, msg_reg, pc, proc_reg, value1_reg
    if (_staruse_stack_trace_star is not False):
        push_stack_trace_b(exp)
    if (dlr_proc_q(value1_reg) is not False):
        result = dlr_apply(value1_reg, args)
        if (_staruse_stack_trace_star is not False):
            pop_stack_trace_b(exp)
        value1_reg = result
        k_reg = k
        pc = apply_cont2
    else:
        if (procedure_object_q(value1_reg) is not False):
            if (_staruse_stack_trace_star is not False):
                k2_reg = make_cont2(b_cont2_58_d, exp, k)
                fail_reg = value2_reg
                handler_reg = handler
                info_reg = info
                env2_reg = env
                args_reg = args
                proc_reg = value1_reg
                pc = apply_proc
            else:
                k2_reg = k
                fail_reg = value2_reg
                handler_reg = handler
                info_reg = info
                env2_reg = env
                args_reg = args
                proc_reg = value1_reg
                pc = apply_proc
        else:
            fail_reg = value2_reg
            handler_reg = handler
            info_reg = info
            msg_reg = format("attempt to apply non-procedure '~a'", value1_reg)
            pc = runtime_error

def b_cont2_60_d(exp, operator, env, info, handler, k):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_59_d, value1_reg, exp, env, info, handler, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    exp_reg = operator
    pc = m

def b_cont2_61_d(v, k):
    global k_reg, pc, value1_reg
    value1_reg = v
    k_reg = k
    pc = apply_cont2

def b_cont2_62_d(fexps, env, handler, k):
    global env_reg, exps_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_61_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    exps_reg = fexps
    pc = eval_sequence

def b_cont2_63_d(info, handler):
    global exception_reg, fail_reg, handler_reg, info_reg, msg_reg, pc
    col = get_start_char(info)
    line = get_start_line(info)
    src = get_srcfile(info)
    if (exception_object_q(value1_reg) is not False):
        fail_reg = value2_reg
        exception_reg = value1_reg
        handler_reg = handler
        pc = apply_handler2
    else:
        if (string_q(value1_reg) is not False):
            fail_reg = value2_reg
            exception_reg = make_exception("Exception", value1_reg, src, line, col)
            handler_reg = handler
            pc = apply_handler2
        else:
            if ((list_q(value1_reg)) and (valid_exception_type_q((value1_reg).car)) and (string_q((value1_reg).cdr.car)) is not False):
                fail_reg = value2_reg
                exception_reg = make_exception((value1_reg).car, (value1_reg).cdr.car, src, line, col)
                handler_reg = handler
                pc = apply_handler2
            else:
                fail_reg = value2_reg
                handler_reg = handler
                info_reg = info
                msg_reg = "bad exception type"
                pc = runtime_error

def b_cont2_64_d(k2):
    global k_reg, pc, value_reg
    value_reg = value1_reg
    k_reg = k2
    pc = apply_cont

def b_cont2_65_d(macro_transformer, k):
    global k_reg, pc, value1_reg
    set_binding_value_b(value1_reg, macro_transformer)
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_66_d(name, env, info, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, pc, var_reg
    macro_transformer = make_macro(b_macro_14_d, value1_reg, env, info)
    k_reg = make_cont2(b_cont2_65_d, macro_transformer, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = macro_env
    var_reg = name
    pc = lookup_binding_in_first_frame

def b_cont2_67_d(docstring, var, k):
    global k_reg, pc, value1_reg
    if (procedure_object_q(value1_reg) is not False):
        set_global_value_b(var, dlr_func(value1_reg))
    else:
        set_global_value_b(var, value1_reg)
    set_global_docstring_b(var, docstring)
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_68_d(aclauses, clauses, k):
    global k_reg, pc, value1_reg
    set_binding_value_b(value1_reg, make_pattern_macro_hat(clauses, aclauses))
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_69_d(rhs_value, k):
    global k_reg, pc, value1_reg, value2_reg
    old_value = binding_value(value1_reg)
    set_binding_value_b(value1_reg, rhs_value)
    new_fail = make_fail(b_fail_2_d, value1_reg, old_value, value2_reg)
    value2_reg = new_fail
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_70_d(rhs_value, k):
    global k_reg, pc, value1_reg, value2_reg
    old_value = dlr_env_lookup(value1_reg)
    set_global_value_b(value1_reg, rhs_value)
    new_fail = make_fail(b_fail_3_d, old_value, value1_reg, value2_reg)
    value2_reg = new_fail
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_71_d(var, var_info, env, handler, k):
    global dk_reg, env_reg, fail_reg, gk_reg, handler_reg, pc, sk_reg, var_info_reg, var_reg
    sk_reg = make_cont2(b_cont2_69_d, value1_reg, k)
    dk_reg = make_cont3(b_cont3_4_d, value1_reg, k)
    gk_reg = make_cont2(b_cont2_70_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    var_info_reg = var_info
    env_reg = env
    var_reg = var
    pc = lookup_variable

def b_cont2_72_d(docstring, rhs_value, k):
    global k_reg, pc, value1_reg
    set_binding_value_b(value1_reg, rhs_value)
    set_binding_docstring_b(value1_reg, docstring)
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_73_d(docstring, var, env, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, pc, var_reg
    k_reg = make_cont2(b_cont2_72_d, docstring, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    var_reg = var
    pc = lookup_binding_in_first_frame

def b_cont2_74_d(k):
    global k_reg, pc, value1_reg
    value1_reg = binding_docstring(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_75_d(k):
    global k_reg, pc, value1_reg
    value1_reg = help(dlr_env_lookup(value1_reg))
    k_reg = k
    pc = apply_cont2

def b_cont2_76_d(var, k):
    global k_reg, pc, value1_reg
    value1_reg = association(var, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_77_d(k):
    global k_reg, pc, value1_reg
    value1_reg = callback(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_78_d(else_exp, then_exp, env, handler, k):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    if (value1_reg is not False):
        k_reg = k
        fail_reg = value2_reg
        handler_reg = handler
        env_reg = env
        exp_reg = then_exp
        pc = m
    else:
        k_reg = k
        fail_reg = value2_reg
        handler_reg = handler
        env_reg = env
        exp_reg = else_exp
        pc = m

def b_cont2_79_d(k):
    global k_reg, pc, value1_reg
    value1_reg = dlr_func(value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_80_d(start_time, tests, handler, k):
    global fail_reg, handler_reg, k_reg, pc, right_reg, start_time_reg, tests_reg, wrong_reg
    wrong2 = (value1_reg).cdr.car
    right2 = (value1_reg).car
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    wrong_reg = wrong2
    right_reg = right2
    start_time_reg = start_time
    tests_reg = (tests).cdr
    pc = run_unit_tests

def b_cont2_81_d(right, test_name, wrong, env, handler, k):
    global assertions_reg, env_reg, fail_reg, handler_reg, k_reg, pc, right_reg, test_name_reg, verbose_reg, wrong_reg
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    wrong_reg = wrong
    right_reg = right
    verbose_reg = True
    assertions_reg = value1_reg
    test_name_reg = test_name
    pc = run_unit_test_cases

def b_cont2_82_d(matched_exps, k):
    global k_reg, pc, value1_reg
    value1_reg = append(matched_exps, value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_83_d(assertions, nums, test_name, handler, k):
    global assertions_reg, fail_reg, handler_reg, k_reg, nums_reg, pc, test_name_reg
    k_reg = make_cont2(b_cont2_82_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    assertions_reg = (assertions).cdr
    nums_reg = (nums).cdr
    test_name_reg = test_name
    pc = filter_assertions

def b_cont2_84_d(assertions, msg, proc_exp, result_val, right, test_exp, test_name, traceback, verbose, wrong, env, handler, k):
    global assertions_reg, env_reg, fail_reg, handler_reg, k_reg, pc, right_reg, test_name_reg, verbose_reg, wrong_reg
    if (verbose is not False):
        printf("~a\n", traceback)
        printf("  Procedure    : ~a\n", proc_exp)
        printf("       src     : ~a\n", test_exp)
        printf("       src eval: ~a\n", value1_reg)
        printf("       result  : ~a\n", result_val)
    make_test_callback(test_name, msg, False, traceback, proc_exp, test_exp, result_val)
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    wrong_reg = (wrong) + (1)
    right_reg = right
    verbose_reg = verbose
    assertions_reg = (assertions).cdr
    test_name_reg = test_name
    pc = run_unit_test_cases

def b_cont2_85_d(assertions, msg, proc_exp, right, test_aexp, test_exp, test_name, traceback, verbose, where, wrong, env, handler, k):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_84_d, assertions, msg, proc_exp, value1_reg, right, test_exp, test_name, traceback, verbose, wrong, env, handler, k)
    fail_reg = value2_reg
    handler_reg = make_handler2(b_handler2_4_d, assertions, msg, right, test_name, verbose, where, wrong, env, handler, k)
    env_reg = env
    exp_reg = test_aexp
    pc = m

def b_cont2_86_d(assertions, right, test_name, verbose, wrong, env, handler, k):
    global assertions_reg, env_reg, fail_reg, handler_reg, k_reg, pc, right_reg, test_name_reg, verbose_reg, wrong_reg
    make_test_callback(test_name, "test", True, "", "", "", "")
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    wrong_reg = wrong
    right_reg = (right) + (1)
    verbose_reg = verbose
    assertions_reg = (assertions).cdr
    test_name_reg = test_name
    pc = run_unit_test_cases

def b_cont2_87_d(exps, env, handler, k):
    global env_reg, exps_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    exps_reg = (exps).cdr
    pc = m_star

def b_cont2_88_d(exps, env, handler, k):
    global env_reg, exps_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    exps_reg = (exps).cdr
    pc = eval_sequence

def b_cont2_89_d(e, handler):
    global exception_reg, fail_reg, handler_reg, pc
    fail_reg = value2_reg
    exception_reg = e
    handler_reg = handler
    pc = apply_handler2

def b_cont2_90_d(trace_depth, k2):
    global k_reg, pc
    trace_depth = (trace_depth) - (1)
    printf("~areturn: ~s~%", make_trace_depth_string(trace_depth), value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_91_d(items, sep, k2):
    global k_reg, pc, value1_reg
    value1_reg = string_append(format("~a", (items).car), sep, value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_92_d(handler, k2):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = k2
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = toplevel_env
    exp_reg = value1_reg
    pc = m

def b_cont2_93_d(args, handler, k2):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = k2
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = (args).cdr.car
    exp_reg = value1_reg
    pc = m

def b_cont2_94_d(handler, k2):
    global fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont4(b_cont4_11_d, handler, k2)
    fail_reg = value2_reg
    handler_reg = handler
    src_reg = "stdin"
    tokens_reg = value1_reg
    pc = read_sexp

def b_cont2_95_d(handler, k2):
    global fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont4(b_cont4_12_d, handler, k2)
    fail_reg = value2_reg
    handler_reg = handler
    src_reg = "stdin"
    tokens_reg = value1_reg
    pc = read_sexp

def b_cont2_96_d(k):
    global k_reg, load_stack, pc, value1_reg
    if (((load_stack) is symbol_emptylist) is not False):
        printf("WARNING: empty load-stack encountered!\n")
    else:
        load_stack = (load_stack).cdr
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont2_97_d(filename, env2, handler, k):
    global env2_reg, fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont2(b_cont2_96_d, k)
    fail_reg = value2_reg
    handler_reg = handler
    env2_reg = env2
    src_reg = filename
    tokens_reg = value1_reg
    pc = read_and_eval_asexps

def b_cont2_98_d(src, tokens_left, env2, handler, k):
    global env2_reg, fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    if (token_type_q(first(tokens_left), symbol_end_marker) is not False):
        k_reg = k
        pc = apply_cont2
    else:
        k_reg = k
        fail_reg = value2_reg
        handler_reg = handler
        env2_reg = env2
        src_reg = src
        tokens_reg = tokens_left
        pc = read_and_eval_asexps

def b_cont2_99_d(src, tokens_left, env2, handler, k):
    global env_reg, exp_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_98_d, src, tokens_left, env2, handler, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env2
    exp_reg = value1_reg
    pc = m

def b_cont2_100_d(filenames, env2, info, handler, k):
    global env2_reg, fail_reg, filenames_reg, handler_reg, info_reg, k_reg, pc
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    info_reg = info
    env2_reg = env2
    filenames_reg = (filenames).cdr
    pc = load_files

def b_cont2_101_d(args, info, handler, k2):
    global fail_reg, handler_reg, info_reg, k_reg, msg_reg, pc, value1_reg
    if ((value1_reg) is (True) is not False):
        value1_reg = symbol_ok
        k_reg = k2
        pc = apply_cont2
    else:
        if (numeric_equal(length(args), 3) is not False):
            fail_reg = value2_reg
            handler_reg = handler
            info_reg = info
            msg_reg = ""
            pc = assertion_error
        else:
            fail_reg = value2_reg
            handler_reg = handler
            info_reg = info
            msg_reg = (args).cdr.cdr.cdr.car
            pc = assertion_error

def b_cont2_102_d(lst, k2):
    global k_reg, pc, value1_reg
    if (member((lst).car, value1_reg) is not False):
        k_reg = k2
        pc = apply_cont2
    else:
        value1_reg = cons((lst).car, value1_reg)
        k_reg = k2
        pc = apply_cont2

def b_cont2_103_d(filename, info, handler, k2):
    global env2_reg, fail_reg, filename_reg, handler_reg, info_reg, k_reg, paths_reg, pc
    module = make_toplevel_env()
    set_binding_value_b(value1_reg, module)
    k_reg = k2
    fail_reg = value2_reg
    handler_reg = handler
    info_reg = info
    env2_reg = module
    filename_reg = filename
    paths_reg = SCHEMEPATH
    pc = find_file_and_load

def b_cont2_104_d(ls1, k2):
    global k_reg, pc, value1_reg
    value1_reg = cons((ls1).car, value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_105_d(lists, k2):
    global fail_reg, k2_reg, ls1_reg, ls2_reg, pc
    k2_reg = k2
    fail_reg = value2_reg
    ls2_reg = value1_reg
    ls1_reg = (lists).car
    pc = append2

def b_cont2_106_d(iterator, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, iterator_reg, k_reg, pc, proc_reg
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    iterator_reg = iterator
    proc_reg = proc
    pc = iterate_continue

def b_cont2_107_d(iterator, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, iterator_reg, k_reg, pc, proc_reg
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    iterator_reg = iterator
    proc_reg = proc
    pc = iterate_collect_continue

def b_cont2_108_d(list1, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, list1_reg, pc, proc_reg
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    list1_reg = (list1).cdr
    proc_reg = proc
    pc = map1

def b_cont2_109_d(list1, proc, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(dlr_apply(proc, List((list1).car)), value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_110_d(list1, list2, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, list1_reg, list2_reg, pc, proc_reg
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    list2_reg = (list2).cdr
    list1_reg = (list1).cdr
    proc_reg = proc
    pc = map2

def b_cont2_111_d(list1, list2, proc, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(dlr_apply(proc, List((list1).car, (list2).car)), value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_112_d(lists, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, lists_reg, pc, proc_reg
    k_reg = make_cont2(b_cont2_40_d, value1_reg, k)
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    lists_reg = Map(cdr, lists)
    proc_reg = proc
    pc = mapN

def b_cont2_113_d(lists, proc, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(dlr_apply(proc, Map(car, lists)), value1_reg)
    k_reg = k
    pc = apply_cont2

def b_cont2_114_d(arg_list, proc, env, handler, k):
    global env_reg, fail_reg, handler_reg, k_reg, lists_reg, pc, proc_reg
    k_reg = k
    fail_reg = value2_reg
    handler_reg = handler
    env_reg = env
    lists_reg = Map(cdr, arg_list)
    proc_reg = proc
    pc = for_each_primitive

def b_cont2_115_d(k2):
    global k_reg, pc, value1_reg
    value1_reg = apply_native(dict, List(value1_reg))
    k_reg = k2
    pc = apply_cont2

def b_cont2_116_d(associations, k2):
    global k_reg, pc, value1_reg
    value = ((associations).car).cdr.cdr.car
    key = to_string(((associations).car).car)
    value1_reg = cons(List(key, value), value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_117_d(elements, pred, env2, info, handler, k2):
    global elements_reg, env2_reg, fail_reg, handler_reg, info_reg, k2_reg, pc, proc_reg, x_reg
    k2_reg = k2
    fail_reg = value2_reg
    handler_reg = handler
    info_reg = info
    env2_reg = env2
    elements_reg = value1_reg
    x_reg = (elements).car
    proc_reg = pred
    pc = insert_element

def b_cont2_118_d(elements, k2):
    global k_reg, pc, value1_reg
    value1_reg = cons((elements).car, value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_119_d(elements, proc, x, env2, info, handler, k2):
    global elements_reg, env2_reg, fail_reg, handler_reg, info_reg, k2_reg, k_reg, pc, proc_reg, value1_reg, x_reg
    if (value1_reg is not False):
        value1_reg = cons(x, elements)
        k_reg = k2
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_118_d, elements, k2)
        fail_reg = value2_reg
        handler_reg = handler
        info_reg = info
        env2_reg = env2
        elements_reg = (elements).cdr
        x_reg = x
        proc_reg = proc
        pc = insert_element

def b_cont2_120_d(new_acdr1, new_cdr1, s_car, k):
    global ap1_reg, ap2_reg, k_reg, p1_reg, p2_reg, pc
    k_reg = make_cont(b_cont_59_d, s_car, k)
    ap2_reg = value2_reg
    ap1_reg = new_acdr1
    p2_reg = value1_reg
    p1_reg = new_cdr1
    pc = unify_patterns_hat

def b_cont2_121_d(apair2, pair2, s_car, k):
    global ap_reg, k2_reg, pattern_reg, pc, s_reg
    k2_reg = make_cont2(b_cont2_120_d, value2_reg, value1_reg, s_car, k)
    ap_reg = cdr_hat(apair2)
    s_reg = s_car
    pattern_reg = (pair2).cdr
    pc = instantiate_hat

def b_cont2_122_d(a, aa, ap, k2):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = cons_hat(aa, value2_reg, get_source_info(ap))
    value1_reg = cons(a, value1_reg)
    k_reg = k2
    pc = apply_cont2

def b_cont2_123_d(ap, pattern, s, k2):
    global ap_reg, k2_reg, pattern_reg, pc, s_reg
    k2_reg = make_cont2(b_cont2_122_d, value1_reg, value2_reg, ap, k2)
    ap_reg = cdr_hat(ap)
    s_reg = s
    pattern_reg = (pattern).cdr
    pc = instantiate_hat

def b_cont2_124_d(s2, k2):
    global ap_reg, k2_reg, pattern_reg, pc, s_reg
    k2_reg = k2
    ap_reg = value2_reg
    s_reg = s2
    pattern_reg = value1_reg
    pc = instantiate_hat

def b_cont3_1_d(src, handler, k):
    global chars_reg, fail_reg, handler_reg, k_reg, pc, src_reg, value1_reg, value2_reg
    if (token_type_q(value1_reg, symbol_end_marker) is not False):
        value2_reg = value3_reg
        value1_reg = List(value1_reg)
        k_reg = k
        pc = apply_cont2
    else:
        k_reg = make_cont2(b_cont2_1_d, value1_reg, k)
        fail_reg = value3_reg
        handler_reg = handler
        src_reg = src
        chars_reg = value2_reg
        pc = scan_input_loop

def b_cont3_2_d():
    global final_reg, pc
    final_reg = value1_reg
    pc = pc_halt_signal

def b_cont3_3_d(k):
    global k_reg, pc, value1_reg, value2_reg
    value1_reg = get_external_member(value1_reg, value2_reg)
    value2_reg = value3_reg
    k_reg = k
    pc = apply_cont2

def b_cont3_4_d(rhs_value, k):
    global k_reg, pc, value1_reg, value2_reg
    old_value = get_external_member(value1_reg, value2_reg)
    set_external_member_b(value1_reg, value2_reg, rhs_value)
    new_fail = make_fail(b_fail_4_d, value2_reg, value1_reg, old_value, value3_reg)
    value2_reg = new_fail
    value1_reg = void_value
    k_reg = k
    pc = apply_cont2

def b_cont3_5_d(k):
    global k_reg, pc, value1_reg, value2_reg
    value1_reg = help(get_external_member(value1_reg, value2_reg))
    value2_reg = value3_reg
    k_reg = k
    pc = apply_cont2

def b_cont4_1_d(src, start, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    info_reg = make_info(src, start, value2_reg)
    x_reg = value1_reg
    pc = annotate_cps

def b_cont4_2_d(src, start, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    info_reg = make_info(src, start, value2_reg)
    x_reg = list_to_vector(value1_reg)
    pc = annotate_cps

def b_cont4_3_d(src, start, v, k):
    global info_reg, k_reg, pc, x_reg
    k_reg = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    info_reg = make_info(src, start, value2_reg)
    x_reg = List(v, value1_reg)
    pc = annotate_cps

def b_cont4_4_d(sexp1, k):
    global k_reg, pc, value1_reg
    value1_reg = cons(sexp1, value1_reg)
    k_reg = k
    pc = apply_cont4

def b_cont4_5_d(src, handler, k):
    global fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont4(b_cont4_4_d, value1_reg, k)
    fail_reg = value4_reg
    handler_reg = handler
    src_reg = src
    tokens_reg = value3_reg
    pc = read_vector_sequence

def b_cont4_6_d(expected_terminator, sexp1, src, handler, k):
    global expected_terminator_reg, fail_reg, handler_reg, k_reg, pc, sexps_reg, src_reg, tokens_reg
    k_reg = k
    fail_reg = value4_reg
    handler_reg = handler
    src_reg = src
    expected_terminator_reg = expected_terminator
    tokens_reg = value3_reg
    sexps_reg = cons(sexp1, value1_reg)
    pc = close_sexp_sequence

def b_cont4_7_d(expected_terminator, src, handler, k):
    global expected_terminator_reg, fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    if (token_type_q(first(value3_reg), symbol_dot) is not False):
        k_reg = make_cont4(b_cont4_6_d, expected_terminator, value1_reg, src, handler, k)
        fail_reg = value4_reg
        handler_reg = handler
        src_reg = src
        tokens_reg = rest_of(value3_reg)
        pc = read_sexp
    else:
        k_reg = make_cont4(b_cont4_4_d, value1_reg, k)
        fail_reg = value4_reg
        handler_reg = handler
        src_reg = src
        expected_terminator_reg = expected_terminator
        tokens_reg = value3_reg
        pc = read_sexp_sequence

def b_cont4_8_d():
    global final_reg, pc
    final_reg = value1_reg
    pc = pc_halt_signal

def b_cont4_9_d(senv, src, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_41_d, senv, src, value3_reg, handler, k)
    fail_reg = value4_reg
    handler_reg = handler
    senv_reg = senv
    adatum_reg = value1_reg
    pc = aparse

def b_cont4_10_d():
    global _startokens_left_star, adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    _startokens_left_star = value3_reg
    k_reg = make_cont2(b_cont2_54_d)
    fail_reg = value4_reg
    handler_reg = REP_handler
    senv_reg = initial_contours(toplevel_env)
    adatum_reg = value1_reg
    pc = aparse

def b_cont4_11_d(handler, k2):
    global adatum_reg, fail_reg, handler_reg, k_reg, msg_reg, pc, senv_reg, src_reg, tokens_reg
    if (token_type_q(first(value3_reg), symbol_end_marker) is not False):
        k_reg = k2
        fail_reg = value4_reg
        handler_reg = handler
        senv_reg = initial_contours(toplevel_env)
        adatum_reg = value1_reg
        pc = aparse
    else:
        fail_reg = value4_reg
        handler_reg = handler
        src_reg = "stdin"
        tokens_reg = value3_reg
        msg_reg = "tokens left over"
        pc = read_error

def b_cont4_12_d(handler, k2):
    global fail_reg, handler_reg, k_reg, msg_reg, pc, src_reg, tokens_reg, value2_reg
    if (token_type_q(first(value3_reg), symbol_end_marker) is not False):
        value2_reg = value4_reg
        k_reg = k2
        pc = apply_cont2
    else:
        fail_reg = value4_reg
        handler_reg = handler
        src_reg = "stdin"
        tokens_reg = value3_reg
        msg_reg = "tokens left over"
        pc = read_error

def b_cont4_13_d(src, env2, handler, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, pc, senv_reg
    k_reg = make_cont2(b_cont2_99_d, src, value3_reg, env2, handler, k)
    fail_reg = value4_reg
    handler_reg = handler
    senv_reg = initial_contours(env2)
    adatum_reg = value1_reg
    pc = aparse

def b_fail_1_d():
    global final_reg, pc
    final_reg = "no more choices"
    pc = pc_halt_signal

def b_fail_2_d(binding, old_value, fail):
    global fail_reg, pc
    set_binding_value_b(binding, old_value)
    fail_reg = fail
    pc = apply_fail

def b_fail_3_d(old_value, var, fail):
    global fail_reg, pc
    set_global_value_b(var, old_value)
    fail_reg = fail
    pc = apply_fail

def b_fail_4_d(components, dlr_obj, old_value, fail):
    global fail_reg, pc
    set_external_member_b(dlr_obj, components, old_value)
    fail_reg = fail
    pc = apply_fail

def b_fail_5_d(exps, env, handler, fail, k):
    global env_reg, exps_reg, fail_reg, handler_reg, k_reg, pc
    k_reg = k
    fail_reg = fail
    handler_reg = handler
    env_reg = env
    exps_reg = (exps).cdr
    pc = eval_choices

def b_handler_1_d():
    global final_reg, pc
    final_reg = List(symbol_exception, exception_reg)
    pc = pc_halt_signal

def b_handler2_1_d():
    global final_reg, pc
    final_reg = List(symbol_exception, exception_reg)
    pc = pc_halt_signal

def b_handler2_2_d():
    global _starlast_fail_star, final_reg, pc
    _starlast_fail_star = fail_reg
    final_reg = List(symbol_exception, exception_reg)
    pc = pc_halt_signal

def b_handler2_3_d():
    global final_reg, pc
    final_reg = False
    pc = pc_halt_signal

def b_handler2_4_d(assertions, msg, right, test_name, verbose, where, wrong, env, handler, k):
    global assertions_reg, env_reg, handler_reg, k_reg, msg_reg, pc, right_reg, test_name_reg, verbose_reg, where_reg, wrong_reg
    k_reg = k
    handler_reg = handler
    env_reg = env
    wrong_reg = wrong
    right_reg = right
    verbose_reg = verbose
    assertions_reg = assertions
    test_name_reg = test_name
    where_reg = where
    msg_reg = msg
    pc = report_unit_test_diagnostic_fallback

def b_handler2_5_d(assertions, right, test_name, verbose, wrong, env, handler, k):
    global assertions_reg, env_reg, exp_reg, handler_reg, k_reg, msg_reg, pc, right_reg, test_name_reg, verbose_reg, where_reg, wrong_reg
    msg = get_exception_message(exception_reg)
    where = get_exception_info(exception_reg)
    assert_exp = (assertions).car
    assert_shaped_q = (pair_q(assert_exp)) and (pair_q(cdr_hat(assert_exp))) and (pair_q((cdr_hat(assert_exp)).cdr)) and (pair_q((cdr_hat(assert_exp)).cdr.cdr))
    if (not(assert_shaped_q) is not False):
        k_reg = k
        handler_reg = handler
        env_reg = env
        wrong_reg = wrong
        right_reg = right
        verbose_reg = verbose
        assertions_reg = assertions
        test_name_reg = test_name
        where_reg = where
        msg_reg = msg
        pc = report_unit_test_diagnostic_fallback
    else:
        proc_exp = aunparse((cdr_hat(assert_exp)).car)
        test_aexp = (cdr_hat(assert_exp)).cdr.car
        test_exp = aunparse(test_aexp)
        result_exp = (cdr_hat(assert_exp)).cdr.cdr.car
        traceback = get_traceback_string(List(symbol_exception, exception_reg))
        if (GreaterThan(string_length(msg), 0) is not False):
            if ((where) is (symbol_none) is not False):
                printf("  Error: ~a \"~a\"\n", test_name, msg)
            else:
                printf("  Error: ~a \"~a\" at ~a\n", test_name, msg, where)
        else:
            if ((where) is (symbol_none) is not False):
                printf("  Error: ~a\n", test_name)
            else:
                printf("  Error: ~a at ~a\n", test_name, where)
        initialize_stack_trace_b()
        k_reg = make_cont2(b_cont2_85_d, assertions, msg, proc_exp, right, test_aexp, test_exp, test_name, traceback, verbose, where, wrong, env, handler, k)
        handler_reg = make_handler2(b_handler2_4_d, assertions, msg, right, test_name, verbose, where, wrong, env, handler, k)
        env_reg = env
        exp_reg = result_exp
        pc = m

def b_handler2_6_d(cexps, cvar, env, handler, k):
    global env_reg, exps_reg, handler_reg, k_reg, pc
    new_env = extend(env, List(cvar), List(exception_reg), List("try-catch handler"))
    k_reg = k
    handler_reg = handler
    env_reg = new_env
    exps_reg = cexps
    pc = eval_sequence

def b_handler2_7_d(fexps, env, handler):
    global env_reg, exps_reg, handler_reg, k_reg, pc
    k_reg = make_cont2(b_cont2_89_d, exception_reg, handler)
    handler_reg = handler
    env_reg = env
    exps_reg = fexps
    pc = eval_sequence

def b_handler2_8_d(cexps, cvar, fexps, env, handler, k):
    global env_reg, exps_reg, handler_reg, k_reg, pc
    new_env = extend(env, List(cvar), List(exception_reg), List("try-catch-finally handler"))
    catch_handler = try_finally_handler(fexps, env, handler)
    k_reg = make_cont2(b_cont2_62_d, fexps, env, handler, k)
    handler_reg = catch_handler
    env_reg = new_env
    exps_reg = cexps
    pc = eval_sequence

def b_proc_2_d(bodies, formals, runt, env):
    global env_reg, exps_reg, k_reg, msg_reg, pc
    new_args = args_reg
    new_formals = formals
    if (GreaterThanEqual(length(new_args), length(new_formals)) is not False):
        new_env = extend(env, cons(runt, new_formals), cons(list_tail(new_args, length(new_formals)), list_head(new_args, length(new_formals))), make_empty_docstrings((1) + (length(new_formals))))
        k_reg = k2_reg
        env_reg = new_env
        exps_reg = bodies
        pc = eval_sequence
    else:
        msg_reg = "not enough arguments in application"
        pc = runtime_error

def b_proc_3_d(bodies, name, trace_depth, formals, env):
    global env_reg, exps_reg, k_reg, msg_reg, pc
    formals_and_args = process_formals_and_args(formals, args_reg, info_reg, handler_reg, fail_reg)
    new_formals = (formals_and_args).car
    new_args = (formals_and_args).cdr
    if (numeric_equal(length(new_args), length(new_formals)) is not False):
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, new_args))
        trace_depth = (trace_depth) + (1)
        k_reg = make_cont2(b_cont2_90_d, trace_depth, k2_reg)
        env_reg = extend(env, new_formals, new_args, make_empty_docstrings(length(new_formals)))
        exps_reg = bodies
        pc = eval_sequence
    else:
        msg_reg = "incorrect number of arguments in application"
        pc = runtime_error

def b_proc_4_d(bodies, name, trace_depth, formals, runt, env):
    global env_reg, exps_reg, k_reg, msg_reg, pc
    new_args = args_reg
    new_formals = formals
    if (GreaterThanEqual(length(args_reg), length(new_formals)) is not False):
        new_env = extend(env, cons(runt, new_formals), cons(list_tail(new_args, length(new_formals)), list_head(new_args, length(new_formals))), make_empty_docstrings((1) + (length(new_formals))))
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, new_args))
        trace_depth = (trace_depth) + (1)
        k_reg = make_cont2(b_cont2_90_d, trace_depth, k2_reg)
        env_reg = new_env
        exps_reg = bodies
        pc = eval_sequence
    else:
        msg_reg = "not enough arguments in application"
        pc = runtime_error

def b_proc_5_d():
    global k_reg, pc, unit_test_table, value1_reg, value2_reg
    unit_test_table = dict()
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_6_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_7_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_exactly_q(1, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to zero?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = numeric_equal((args_reg).car, 0)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_8_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = python_eval((args_reg).car)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_9_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = python_exec((args_reg).car)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_10_d():
    global final_reg, pc
    final_reg = end_of_session
    pc = pc_halt_signal

def b_proc_11_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_exactly_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to expt"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = expt_native((args_reg).car, (args_reg).cdr.car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_12_d():
    global items_reg, msg_reg, pc, sep_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of args to string-join; should be two"
        pc = runtime_error
    else:
        if (not(string_q((args_reg).car)) is not False):
            msg_reg = "first arg to string-join must be a string"
            pc = runtime_error
        else:
            if (not(list_q((args_reg).cdr.car)) is not False):
                msg_reg = "second arg to string-join must be a list"
                pc = runtime_error
            else:
                items_reg = (args_reg).cdr.car
                sep_reg = (args_reg).car
                pc = string_join

def b_proc_13_d():
    global k_reg, msg_reg, pc, x_reg
    if (length_one_q(args_reg) is not False):
        k_reg = make_cont(b_cont_50_d, handler_reg, fail_reg, k2_reg)
        x_reg = (args_reg).car
        pc = annotate_cps
    else:
        if (length_two_q(args_reg) is not False):
            k_reg = make_cont(b_cont_51_d, args_reg, handler_reg, fail_reg, k2_reg)
            x_reg = (args_reg).car
            pc = annotate_cps
        else:
            msg_reg = "incorrect number of arguments to eval"
            pc = runtime_error

def b_proc_14_d():
    global env_reg, exp_reg, k_reg, msg_reg, pc
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to eval-ast"
        pc = runtime_error
    else:
        if (not(list_q((args_reg).car)) is not False):
            msg_reg = "eval-ast called on non-abstract syntax tree argument"
            pc = runtime_error
        else:
            k_reg = k2_reg
            env_reg = toplevel_env
            exp_reg = (args_reg).car
            pc = m

def b_proc_15_d():
    global k_reg, pc, x_reg
    k_reg = make_cont(b_cont_52_d, handler_reg, fail_reg, k2_reg)
    x_reg = (args_reg).car
    pc = annotate_cps

def b_proc_16_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string-length"
        pc = runtime_error
    else:
        if (not(string_q((args_reg).car)) is not False):
            msg_reg = "string-length called on non-string argument"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(string_length, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_17_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string-ref"
        pc = runtime_error
    else:
        if (not(string_q((args_reg).car)) is not False):
            msg_reg = "string-ref called with non-string first argument"
            pc = runtime_error
        else:
            if (not(number_q((args_reg).cdr.car)) is not False):
                msg_reg = "string-ref called with non-numberic second argument"
                pc = runtime_error
            else:
                value2_reg = fail_reg
                value1_reg = Apply(string_ref, args_reg)
                k_reg = k2_reg
                pc = apply_cont2

def b_proc_18_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = aunparse((args_reg).car)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_19_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = aunparse((((args_reg).car).cdr.cdr.car).car)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_20_d():
    global input_reg, k_reg, pc, src_reg
    k_reg = make_cont2(b_cont2_94_d, handler_reg, k2_reg)
    src_reg = "stdin"
    input_reg = (args_reg).car
    pc = scan_input

def b_proc_21_d():
    global input_reg, k_reg, pc, src_reg
    k_reg = make_cont2(b_cont2_95_d, handler_reg, k2_reg)
    src_reg = "stdin"
    input_reg = (args_reg).car
    pc = scan_input

def b_proc_22_d():
    global args_reg, k_reg, pc, proc_reg, value1_reg, value2_reg
    proc_args = (args_reg).cdr.car
    proc = (args_reg).car
    if (dlr_proc_q(proc) is not False):
        value2_reg = fail_reg
        value1_reg = dlr_apply(proc, proc_args)
        k_reg = k2_reg
        pc = apply_cont2
    else:
        args_reg = proc_args
        proc_reg = proc
        pc = apply_proc

def b_proc_23_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to sqrt"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(sqrt, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_24_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to odd?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = odd_q((args_reg).car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_25_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to even?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = even_q((args_reg).car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_26_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to quotient"
        pc = runtime_error
    else:
        if (member(0, (args_reg).cdr) is not False):
            msg_reg = "division by zero"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(quotient, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_27_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to remainder"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(remainder, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_28_d():
    global k_reg, pc, value1_reg, value2_reg
    for_each(safe_print, args_reg)
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_29_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(string, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_30_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (length_three_q(args_reg) is not False):
        value2_reg = fail_reg
        value1_reg = substring((args_reg).car, (args_reg).cdr.car, (args_reg).cdr.cdr.car)
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (length_two_q(args_reg) is not False):
            value2_reg = fail_reg
            value1_reg = substring((args_reg).car, (args_reg).cdr.car, string_length((args_reg).car))
            k_reg = k2_reg
            pc = apply_cont2
        else:
            msg_reg = "incorrect number of arguments to substring"
            pc = runtime_error

def b_proc_31_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_between_q(1, 2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to number->string"
        pc = runtime_error
    else:
        if (length_at_least_q(2, args_reg) is not False):
            value2_reg = fail_reg
            value1_reg = number_to_string((args_reg).car, (args_reg).cdr.car)
            k_reg = k2_reg
            pc = apply_cont2
        else:
            value2_reg = fail_reg
            value1_reg = number_to_string((args_reg).car)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_32_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_exactly_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to assv"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = assv((args_reg).car, (args_reg).cdr.car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_33_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_exactly_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to memv"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = memv((args_reg).car, (args_reg).cdr.car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_34_d():
    global _starneed_newline_star, k_reg, pc, value1_reg, value2_reg
    s = format("~a", (args_reg).car)
    _starneed_newline_star = true_q(not(ends_with_newline_q(s)))
    display(s)
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_35_d():
    global _starneed_newline_star, k_reg, pc, value1_reg, value2_reg
    _starneed_newline_star = False
    newline()
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_36_d():
    global env2_reg, filenames_reg, k_reg, msg_reg, pc
    if (not(length_at_least_q(1, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to load"
        pc = runtime_error
    else:
        k_reg = k2_reg
        env2_reg = toplevel_env
        filenames_reg = args_reg
        pc = load_files

def b_proc_37_d():
    global ls_reg, msg_reg, pc, sum_reg, x_reg
    if (length_one_q(args_reg) is not False):
        ls_reg = (args_reg).car
        sum_reg = 0
        x_reg = (args_reg).car
        pc = length_loop
    else:
        msg_reg = "incorrect number of arguments to length"
        pc = runtime_error

def b_proc_38_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to symbol?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(symbol_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_39_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to number?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(number_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_40_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to boolean?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(boolean_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_41_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(string_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_42_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(char_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_43_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char=?"
        pc = runtime_error
    else:
        if ((not(char_q((args_reg).car))) or (not(char_q((args_reg).cdr.car))) is not False):
            msg_reg = "char=? requires arguments of type char"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(char_is__q, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_44_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char-whitespace?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(char_whitespace_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_45_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char->integer"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(char_to_integer, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_46_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to integer->char"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(integer_to_char, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_47_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char-alphabetic?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(char_alphabetic_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_48_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char-numeric?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(char_numeric_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_49_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to null?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(null_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_50_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to box?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(box_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_51_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to pair?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(pair_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_52_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to box"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(box, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_53_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to unbox"
        pc = runtime_error
    else:
        if (not(box_q((args_reg).car)) is not False):
            msg_reg = format("unbox called on non-box ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(unbox, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_54_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cons"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(cons, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_55_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to car"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("car called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(car, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_56_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_57_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cadr"
        pc = runtime_error
    else:
        if (not(length_at_least_q(2, (args_reg).car)) is not False):
            msg_reg = format("cadr called on incorrect list structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_58_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caddr"
        pc = runtime_error
    else:
        if (not(length_at_least_q(3, (args_reg).car)) is not False):
            msg_reg = format("caddr called on incorrect list structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_59_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caaaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caaaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caaaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_60_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caaadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caaadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caaadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_61_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_62_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caadar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caadar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caadar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_63_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caaddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caaddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caaddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_64_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_65_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_66_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cadaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cadaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cadaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_67_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cadadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cadadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cadadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_68_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cadar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cadar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cadar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_69_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to caddar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("caddar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(caddar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_70_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cadddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cadddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cadddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_71_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdaaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdaaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdaaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_72_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdaadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdaadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdaadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_73_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_74_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdadar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdadar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdadar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_75_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdaddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdaddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdaddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_76_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_77_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_78_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cddaar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cddaar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cddaar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_79_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cddadr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cddadr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cddadr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_80_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cddar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cddar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cddar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_81_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdddar"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdddar called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdddar, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_82_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cddddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cddddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cddddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_83_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cdddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cdddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cdddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_84_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to cddr"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("cddr called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(cddr, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_85_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = args_reg
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_86_d():
    global args_reg, k2_reg, msg_reg, pc, proc_reg
    if (not((numeric_equal(length(args_reg), 3)) or (numeric_equal(length(args_reg), 4))) is not False):
        msg_reg = "incorrect number of arguments to assert"
        pc = runtime_error
    else:
        if (not(procedure_object_q((args_reg).car)) is not False):
            msg_reg = "assertion predicate is not a procedure"
            pc = runtime_error
        else:
            expected_result = (args_reg).cdr.cdr.car
            expression_result = (args_reg).cdr.car
            proc = (args_reg).car
            k2_reg = make_cont2(b_cont2_101_d, args_reg, info_reg, handler_reg, k2_reg)
            args_reg = List(expression_result, expected_result)
            proc_reg = proc
            pc = apply_proc

def b_proc_87_d():
    global lst_reg, msg_reg, pc
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to set"
        pc = runtime_error
    else:
        lst_reg = (args_reg).car
        pc = make_set

def b_proc_88_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(plus, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_89_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (((args_reg) is symbol_emptylist) is not False):
        msg_reg = "incorrect number of arguments to -"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(minus, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_90_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(multiply, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_91_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((GreaterThan(length(args_reg), 1)) and (member(0, (args_reg).cdr)) is not False):
        msg_reg = "division by zero"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(divide, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_92_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to modulo"
        pc = runtime_error
    else:
        if (numeric_equal((args_reg).cdr.car, 0) is not False):
            msg_reg = "modulo by zero"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(modulo, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_93_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(1, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to min"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(min, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_94_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(1, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to max"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(max, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_95_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to <"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(LessThan, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_96_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to >"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(GreaterThan, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_97_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to <="
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(LessThanEqual, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_98_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to >="
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(GreaterThanEqual, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_99_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to ="
        pc = runtime_error
    else:
        if (not(all_numeric_q(args_reg)) is not False):
            msg_reg = "attempt to apply = on non-numeric argument"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(numeric_equal, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_100_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to abs"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(abs, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_101_d():
    global k_reg, msg_reg, pc, x_reg, y_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to equal?"
        pc = runtime_error
    else:
        k_reg = make_cont(b_cont_53_d, fail_reg, k2_reg)
        y_reg = (args_reg).cdr.car
        x_reg = (args_reg).car
        pc = equal_objects_q

def b_proc_102_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to eq?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(eq_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_103_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to memq"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(memq, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_104_d():
    global k_reg, ls_reg, msg_reg, pc, x_reg, y_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to member"
        pc = runtime_error
    else:
        k_reg = k2_reg
        ls_reg = (args_reg).cdr.car
        y_reg = (args_reg).cdr.car
        x_reg = (args_reg).car
        pc = member_loop

def b_proc_105_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to random"
        pc = runtime_error
    else:
        if (not(positive_q((args_reg).car)) is not False):
            msg_reg = "argument to random must be positive"
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(random, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_106_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((((args_reg) is symbol_emptylist)) or (length_at_least_q(4, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to range"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(Range, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_107_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(snoc, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_108_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(rac, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_109_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(rdc, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_110_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to set-car!"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("set-car! called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(set_car_b, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_111_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to set-cdr!"
        pc = runtime_error
    else:
        if (not(pair_q((args_reg).car)) is not False):
            msg_reg = format("set-cdr! called on non-pair ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(set_cdr_b, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_112_d():
    global env_reg, k_reg, msg_reg, pc, var_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to load-as"
        pc = runtime_error
    else:
        module_name = (args_reg).cdr.car
        filename = (args_reg).car
        k_reg = make_cont2(b_cont2_103_d, filename, info_reg, handler_reg, k2_reg)
        env_reg = env2_reg
        var_reg = module_name
        pc = lookup_binding_in_first_frame

def b_proc_113_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = (_starstack_trace_star).car
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_114_d(k):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = (args_reg).car
    k_reg = k
    pc = apply_cont2

def b_proc_115_d():
    global args_reg, k_reg, msg_reg, pc, proc_reg, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to call/cc"
        pc = runtime_error
    else:
        proc = (args_reg).car
        if (not(procedure_object_q(proc)) is not False):
            msg_reg = "call/cc called with non-procedure"
            pc = runtime_error
        else:
            fake_k = make_proc(b_proc_114_d, k2_reg)
            if (dlr_proc_q(proc) is not False):
                value2_reg = fail_reg
                value1_reg = dlr_apply(proc, List(fake_k))
                k_reg = k2_reg
                pc = apply_cont2
            else:
                args_reg = List(fake_k)
                proc_reg = proc
                pc = apply_proc

def b_proc_116_d():
    global k_reg, pc, value1_reg, value2_reg
    if (((args_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = void_value
        k_reg = REP_k
        pc = apply_cont2
    else:
        value2_reg = fail_reg
        value1_reg = (args_reg).car
        k_reg = REP_k
        pc = apply_cont2

def b_proc_117_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to require"
        pc = runtime_error
    else:
        if (true_q((args_reg).car) is not False):
            value2_reg = fail_reg
            value1_reg = symbol_ok
            k_reg = k2_reg
            pc = apply_cont2
        else:
            pc = apply_fail

def b_proc_118_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = REP_fail
    value1_reg = args_reg
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_119_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to reverse"
        pc = runtime_error
    else:
        if (not(list_q(args_reg)) is not False):
            msg_reg = format("reverse called on incorrect list structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(reverse, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_120_d():
    global lists_reg, pc
    lists_reg = args_reg
    pc = append_all

def b_proc_121_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string->number"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(string_to_number, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_122_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string=?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(string_is__q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_123_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to list->vector"
        pc = runtime_error
    else:
        if (not(list_q((args_reg).car)) is not False):
            msg_reg = format("list->vector called on incorrect list structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(list_to_vector, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_124_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to list->string"
        pc = runtime_error
    else:
        if (not(list_q((args_reg).car)) is not False):
            msg_reg = format("list->string called on incorrect list structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            if (not(all_char_q((args_reg).car)) is not False):
                msg_reg = format("list->string called on non-char list ~s", (args_reg).car)
                pc = runtime_error
            else:
                value2_reg = fail_reg
                value1_reg = Apply(list_to_string, args_reg)
                k_reg = k2_reg
                pc = apply_cont2

def b_proc_125_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to char->string"
        pc = runtime_error
    else:
        if (not(char_q((args_reg).car)) is not False):
            msg_reg = format("char->string called on non-char item ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(char_to_string, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_126_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string->list"
        pc = runtime_error
    else:
        if (not(string_q((args_reg).car)) is not False):
            msg_reg = format("string->list called on non-string item ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(string_to_list, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_127_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string->symbol"
        pc = runtime_error
    else:
        if (not(string_q((args_reg).car)) is not False):
            msg_reg = format("string->symbol called on non-string item ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(string_to_symbol, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_128_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to symbol->string"
        pc = runtime_error
    else:
        if (not(symbol_q((args_reg).car)) is not False):
            msg_reg = format("symbol->string called on non-symbol item ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(symbol_to_string, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_129_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to vector->list"
        pc = runtime_error
    else:
        if (not(vector_q((args_reg).car)) is not False):
            msg_reg = format("vector->list called on incorrect vector structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(vector_to_list, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_130_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to vector-length"
        pc = runtime_error
    else:
        if (not(vector_q((args_reg).car)) is not False):
            msg_reg = format("vector-length called on incorrect vector structure ~s", (args_reg).car)
            pc = runtime_error
        else:
            value2_reg = fail_reg
            value1_reg = Apply(vector_length, args_reg)
            k_reg = k2_reg
            pc = apply_cont2

def b_proc_131_d():
    global lst_reg, pc
    lst_reg = sort(symbolLessThan_q, get_completions(args_reg, env2_reg))
    pc = make_set

def b_proc_132_d():
    global lst_reg, pc
    lst_reg = directory(args_reg, env2_reg)
    pc = make_set

def b_proc_133_d():
    global lst_reg, pc
    lst_reg = sort(symbolLessThan_q, get_variables_from_frames(frames(macro_env)))
    pc = make_set

def b_proc_134_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = get_current_time()
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_135_d():
    global args_reg, env_reg, k_reg, pc, proc_reg
    k_reg = k2_reg
    env_reg = env2_reg
    proc_reg = (args_reg).car
    args_reg = (args_reg).cdr
    pc = map_primitive

def b_proc_136_d():
    global env_reg, k_reg, lists_reg, pc, proc_reg
    k_reg = k2_reg
    env_reg = env2_reg
    lists_reg = (args_reg).cdr
    proc_reg = (args_reg).car
    pc = for_each_primitive

def b_proc_137_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (LessThan(length(args_reg), 1) is not False):
        msg_reg = "incorrect number of arguments to format"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(format, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_138_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = env2_reg
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_139_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = import_native(args_reg, env2_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_140_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = import_as_native((args_reg).car, (args_reg).cdr.car, env2_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_141_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = import_from_native((args_reg).car, (args_reg).cdr, env2_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_142_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to not"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = not(true_q((args_reg).car))
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_143_d():
    global k_reg, pc, value1_reg, value2_reg
    Apply(printf, args_reg)
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_144_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(vector_native, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_145_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_three_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to vector-set!"
        pc = runtime_error
    else:
        vector_set_b((args_reg).car, (args_reg).cdr.car, (args_reg).cdr.cdr.car)
        value2_reg = fail_reg
        value1_reg = void_value
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_146_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to vector-ref"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(vector_ref, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_147_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to make-vector"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(make_vector, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_148_d():
    global msg_reg, pc
    if (not(length_at_least_q(1, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to 'error' (should at least 1)"
        pc = runtime_error
    else:
        location = format("Error in '~a': ", (args_reg).car)
        message = string_append(location, Apply(format, (args_reg).cdr))
        msg_reg = message
        pc = runtime_error

def b_proc_149_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to list-ref"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(list_ref, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_150_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (((args_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = current_directory()
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (length_one_q(args_reg) is not False):
            if (string_q((args_reg).car) is not False):
                value2_reg = fail_reg
                value1_reg = current_directory((args_reg).car)
                k_reg = k2_reg
                pc = apply_cont2
            else:
                msg_reg = "directory must be a string"
                pc = runtime_error
        else:
            msg_reg = "incorrect number of arguments to current-directory"
            pc = runtime_error

def b_proc_151_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((length_one_q(args_reg)) and (number_q((args_reg).car)) is not False):
        value2_reg = fail_reg
        value1_reg = round((args_reg).car)
        k_reg = k2_reg
        pc = apply_cont2
    else:
        msg_reg = "round requires exactly one number"
        pc = runtime_error

def b_proc_152_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((length_one_q(args_reg)) and (boolean_q((args_reg).car)) is not False):
        set_use_stack_trace_b((args_reg).car)
        value2_reg = fail_reg
        value1_reg = void_value
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (((args_reg) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = _staruse_stack_trace_star
            k_reg = k2_reg
            pc = apply_cont2
        else:
            msg_reg = "use-stack-trace requires exactly one boolean or nothing"
            pc = runtime_error

def b_proc_153_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((length_one_q(args_reg)) and (boolean_q((args_reg).car)) is not False):
        set_use_jit_b((args_reg).car)
        value2_reg = fail_reg
        value1_reg = void_value
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (((args_reg) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = _staruse_jit_star
            k_reg = k2_reg
            pc = apply_cont2
        else:
            msg_reg = "use-jit requires exactly one boolean or nothing"
            pc = runtime_error

def b_proc_154_d():
    global _startracing_on_q_star, k_reg, msg_reg, pc, value1_reg, value2_reg
    if ((length_one_q(args_reg)) and (boolean_q((args_reg).car)) is not False):
        _startracing_on_q_star = true_q((args_reg).car)
        value2_reg = fail_reg
        value1_reg = void_value
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (((args_reg) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = _startracing_on_q_star
            k_reg = k2_reg
            pc = apply_cont2
        else:
            msg_reg = "use-tracing requires exactly one boolean or nothing"
            pc = runtime_error

def b_proc_155_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to eqv?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(eqv_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_156_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to vector?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(vector_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_157_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to atom?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(atom_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_158_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to iter?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(iter_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_159_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(getitem_native, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_160_d():
    global k_reg, pc, value1_reg, value2_reg
    Apply(setitem_native, args_reg)
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_161_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(hasitem_native, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_162_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(getattr_native, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_163_d():
    global k_reg, pc, value1_reg, value2_reg
    Apply(setattr_native, args_reg)
    value2_reg = fail_reg
    value1_reg = void_value
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_164_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(hasattr_native, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_165_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to list?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(list_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_166_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to procedure?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = procedure_object_q((args_reg).car)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_167_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string<?"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(stringLessThan_q, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_168_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to float"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(float, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_169_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(((args_reg) is symbol_emptylist)) is not False):
        msg_reg = "incorrect number of arguments to globals"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(globals, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_170_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to int"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(truncate_to_integer, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_171_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to assq"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(assq, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_172_d():
    global associations_reg, k2_reg, k_reg, pc, value1_reg, value2_reg
    if (((args_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = apply_native(dict, List(symbol_emptylist))
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_115_d, k2_reg)
        associations_reg = (args_reg).car
        pc = make_dict_tuples

def b_proc_173_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to property"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(property, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_174_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to rational"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(divide, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_175_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(((args_reg) is symbol_emptylist)) is not False):
        msg_reg = "incorrect number of arguments to reset-toplevel-env"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(reset_toplevel_env, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_176_d():
    global msg_reg, pc
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to sort"
        pc = runtime_error
    else:
        pc = sort_native

def b_proc_177_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_at_least_q(2, args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string-append"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(string_append, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_178_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_two_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to string-split"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(string_split, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_179_d():
    global k_reg, msg_reg, pc, value1_reg, value2_reg
    if (not(length_one_q(args_reg)) is not False):
        msg_reg = "incorrect number of arguments to typeof"
        pc = runtime_error
    else:
        value2_reg = fail_reg
        value1_reg = Apply(type, args_reg)
        k_reg = k2_reg
        pc = apply_cont2

def b_proc_180_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = Apply(use_lexical_address, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_181_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = host_environment_native()
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_182_d():
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = get_exception_message((args_reg).car)
    k_reg = k2_reg
    pc = apply_cont2

def b_proc_183_d(external_function_object):
    global k_reg, pc, value1_reg, value2_reg
    value2_reg = fail_reg
    value1_reg = apply_star(external_function_object, args_reg)
    k_reg = k2_reg
    pc = apply_cont2

def b_macro_1_d():
    global adatum_reg, msg_reg, pc
    if (LessThan(length_hat(datum_reg), 3) is not False):
        adatum_reg = datum_reg
        msg_reg = "bad lambda expression:"
        pc = aparse_error
    else:
        bodies = cddr_hat(datum_reg)
        formals = cadr_hat(datum_reg)
        return get_internal_defines_hat(bodies, datum_reg, handler_reg, fail_reg, make_cont2(b_cont2_42_d, formals, handler_reg, fail_reg, k_reg))

def b_macro_2_d():
    global adatum_reg, msg_reg, pc
    if (LessThan(length_hat(datum_reg), 4) is not False):
        adatum_reg = datum_reg
        msg_reg = "bad trace-lambda expression:"
        pc = aparse_error
    else:
        bodies = cdddr_hat(datum_reg)
        formals = caddr_hat(datum_reg)
        name = cadr_hat(datum_reg)
        return get_internal_defines_hat(bodies, datum_reg, handler_reg, fail_reg, make_cont2(b_cont2_43_d, name, formals, handler_reg, fail_reg, k_reg))

def b_macro_3_d():
    global pc, value_reg
    if (symbol_q_hat(cadr_hat(datum_reg)) is not False):
        name = cadr_hat(datum_reg)
        bindings = caddr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cdddr_hat(datum_reg)
        value_reg = append(List(symbol_letrec), append(List(List(append(List(name), List(append(List(symbol_lambda), append(List(vars), at_hat(bodies))))))), List(append(List(name), at_hat(exps)))))
        pc = apply_cont
    else:
        bindings = cadr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cddr_hat(datum_reg)
        value_reg = append(List(append(List(symbol_lambda), append(List(vars), at_hat(bodies)))), at_hat(exps))
        pc = apply_cont

def b_macro_4_d():
    global k2_reg, pc, procs_reg, vars_reg
    decls = cadr_hat(datum_reg)
    vars = map_hat(car_hat, decls)
    procs = map_hat(cadr_hat, decls)
    bodies = cddr_hat(datum_reg)
    k2_reg = make_cont2(b_cont2_46_d, bodies, k_reg)
    procs_reg = procs
    vars_reg = vars
    pc = create_letrec_assignments_hat

def b_macro_5_d():
    global pc, value_reg
    bodies = cddr_hat(datum_reg)
    formals = cdadr_hat(datum_reg)
    name = caadr_hat(datum_reg)
    value_reg = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(formals), at_hat(bodies))))))
    pc = apply_cont

def b_macro_6_d():
    global pc, value_reg
    exps = cdr_hat(datum_reg)
    if (null_q_hat(exps) is not False):
        value_reg = True
        pc = apply_cont
    else:
        if (null_q_hat(cdr_hat(exps)) is not False):
            value_reg = car_hat(exps)
            pc = apply_cont
        else:
            value_reg = append(List(symbol_if), append(List(car_hat(exps)), append(List(append(List(symbol_and), at_hat(cdr_hat(exps)))), List(False))))
            pc = apply_cont

def b_macro_7_d():
    global pc, value_reg
    exps = cdr_hat(datum_reg)
    if (null_q_hat(exps) is not False):
        value_reg = False
        pc = apply_cont
    else:
        if (null_q_hat(cdr_hat(exps)) is not False):
            value_reg = car_hat(exps)
            pc = apply_cont
        else:
            g = gensym_hat("or-bool")
            value_reg = append(List(symbol_let), append(List(List(append(List(g), List(car_hat(exps))))), List(append(List(symbol_if), append(List(g), append(List(g), List(append(List(symbol_or), at_hat(cdr_hat(exps))))))))))
            pc = apply_cont

def b_macro_8_d():
    global adatum_reg, msg_reg, pc, value_reg
    clauses = cdr_hat(datum_reg)
    if (null_q_hat(clauses) is not False):
        adatum_reg = datum_reg
        msg_reg = "empty (cond) expression"
        pc = amacro_error
    else:
        other_clauses = cdr_hat(clauses)
        first_clause = car_hat(clauses)
        if ((null_q_hat(first_clause)) or (not(list_q_hat(first_clause))) is not False):
            adatum_reg = first_clause
            msg_reg = "improper cond clause"
            pc = amacro_error
        else:
            then_exps = cdr_hat(first_clause)
            test_exp = car_hat(first_clause)
            if (eq_q_hat(test_exp, symbol_else) is not False):
                if (null_q_hat(then_exps) is not False):
                    adatum_reg = first_clause
                    msg_reg = "improper else clause"
                    pc = amacro_error
                else:
                    if (null_q_hat(cdr_hat(then_exps)) is not False):
                        value_reg = car_hat(then_exps)
                        pc = apply_cont
                    else:
                        value_reg = append(List(symbol_begin), at_hat(then_exps))
                        pc = apply_cont
            else:
                if (null_q_hat(then_exps) is not False):
                    g = gensym_hat("cond-bool")
                    if (null_q_hat(other_clauses) is not False):
                        value_reg = append(List(symbol_let), append(List(List(append(List(g), List(test_exp)))), List(append(List(symbol_if), append(List(g), List(g))))))
                        pc = apply_cont
                    else:
                        value_reg = append(List(symbol_let), append(List(List(append(List(g), List(test_exp)))), List(append(List(symbol_if), append(List(g), append(List(g), List(append(List(symbol_cond), at_hat(other_clauses)))))))))
                        pc = apply_cont
                else:
                    if (eq_q_hat(car_hat(then_exps), symbol__is_to_) is not False):
                        if (null_q_hat(cdr_hat(then_exps)) is not False):
                            adatum_reg = first_clause
                            msg_reg = "improper => clause"
                            pc = amacro_error
                        else:
                            if (null_q_hat(other_clauses) is not False):
                                g = gensym_hat("cond-bool")
                                value_reg = append(List(symbol_let), append(List(List(append(List(g), List(test_exp)))), List(append(List(symbol_if), append(List(g), List(append(List(cadr_hat(then_exps)), List(g))))))))
                                pc = apply_cont
                            else:
                                g = gensym_hat("cond-bool")
                                value_reg = append(List(symbol_let), append(List(List(append(List(g), List(test_exp)))), List(append(List(symbol_if), append(List(g), append(List(append(List(cadr_hat(then_exps)), List(g))), List(append(List(symbol_cond), at_hat(other_clauses)))))))))
                                pc = apply_cont
                    else:
                        if (null_q_hat(other_clauses) is not False):
                            if (null_q_hat(cdr_hat(then_exps)) is not False):
                                value_reg = append(List(symbol_if), append(List(test_exp), List(car_hat(then_exps))))
                                pc = apply_cont
                            else:
                                value_reg = append(List(symbol_if), append(List(test_exp), List(append(List(symbol_begin), at_hat(then_exps)))))
                                pc = apply_cont
                        else:
                            if (null_q_hat(cdr_hat(then_exps)) is not False):
                                value_reg = append(List(symbol_if), append(List(test_exp), append(List(car_hat(then_exps)), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                pc = apply_cont
                            else:
                                value_reg = append(List(symbol_if), append(List(test_exp), append(List(append(List(symbol_begin), at_hat(then_exps))), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                pc = apply_cont

def b_macro_9_d():
    global bindings_reg, bodies_reg, pc
    bodies = cddr_hat(datum_reg)
    bindings = cadr_hat(datum_reg)
    bodies_reg = bodies
    bindings_reg = bindings
    pc = nest_let_star_bindings_hat

def b_macro_10_d():
    global clauses_reg, k_reg, pc, var_reg
    r = gensym_hat("case")
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    k_reg = make_cont(b_cont_28_d, exp, r, k_reg)
    clauses_reg = clauses
    var_reg = r
    pc = case_clauses_to_cond_clauses_hat

def b_macro_11_d():
    global clauses_reg, k_reg, pc, var_reg
    r = gensym_hat("record-case")
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    k_reg = make_cont(b_cont_28_d, exp, r, k_reg)
    clauses_reg = clauses
    var_reg = r
    pc = record_case_clauses_to_cond_clauses_hat

def b_macro_12_d():
    global adatum_reg, k2_reg, msg_reg, pc, variants_reg
    datatype_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(datatype_name), "?"))
    if (not(eq_q_hat(caddr_hat(datum_reg), type_tester_name)) is not False):
        adatum_reg = caddr_hat(datum_reg)
        msg_reg = format("datatype tester predicate not named ~a", type_tester_name)
        pc = amacro_error
    else:
        variants = cdddr_hat(datum_reg)
        k2_reg = make_cont2(b_cont2_48_d, type_tester_name, k_reg)
        variants_reg = variants
        pc = make_dd_variant_constructors_hat

def b_macro_13_d():
    global clauses_reg, k_reg, pc, var_reg
    type_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(type_name), "?"))
    exp = caddr_hat(datum_reg)
    clauses = cdddr_hat(datum_reg)
    r = gensym_hat("cases")
    k_reg = make_cont(b_cont_33_d, exp, r, type_name, type_tester_name, k_reg)
    clauses_reg = clauses
    var_reg = r
    pc = record_case_clauses_to_cond_clauses_hat

def b_macro_14_d(proc, env, info):
    global k_reg, pc, x_reg
    k_reg = make_cont(b_cont_49_d, proc, env, info, handler_reg, fail_reg, k_reg)
    x_reg = datum_reg
    pc = unannotate_cps

def next_avail(n):
    return string_ref(chars_to_scan, n)

def remaining(n):
    return (1) + (n)

def initialize_scan_counters():
    global last_scan_char, last_scan_line, last_scan_position, scan_char, scan_line, scan_position
    scan_line = 1
    scan_char = 1
    scan_position = 1
    last_scan_line = scan_line
    last_scan_char = scan_char
    last_scan_position = scan_position

def increment_scan_counters(chars):
    global last_scan_char, last_scan_line, last_scan_position, scan_char, scan_line, scan_position
    last_scan_line = scan_line
    last_scan_char = scan_char
    last_scan_position = scan_position
    if (char_is__q(next_avail(chars), make_char('\n')) is not False):
        scan_line = (1) + (scan_line)
        scan_char = 1
    else:
        scan_char = (1) + (scan_char)
    scan_position = (1) + (scan_position)

def mark_token_start():
    global token_start_char, token_start_line, token_start_position
    token_start_line = scan_line
    token_start_char = scan_char
    token_start_position = scan_position

def scan_input():
    global chars_reg, chars_to_scan, pc
    initialize_scan_counters()
    chars_to_scan = string_append(input_reg, string(make_char('\0')))
    chars_reg = 0
    pc = scan_input_loop

def scan_input_loop():
    global action_reg, buffer_reg, k_reg, pc
    k_reg = make_cont3(b_cont3_1_d, src_reg, handler_reg, k_reg)
    buffer_reg = symbol_emptylist
    action_reg = List(symbol_goto, symbol_start_state)
    pc = apply_action

def apply_action():
    global action_reg, buffer_reg, chars_reg, k_reg, pc, token_type_reg
    if (((action_reg).car) is (symbol_shift) is not False):
        next = ((action_reg)).cdr.car
        increment_scan_counters(chars_reg)
        buffer_reg = cons(next_avail(chars_reg), buffer_reg)
        chars_reg = remaining(chars_reg)
        action_reg = next
        pc = apply_action
    else:
        if (((action_reg).car) is (symbol_replace) is not False):
            next = (((action_reg)).cdr).cdr.car
            new_char = ((action_reg)).cdr.car
            increment_scan_counters(chars_reg)
            chars_reg = remaining(chars_reg)
            buffer_reg = cons(new_char, buffer_reg)
            action_reg = next
            pc = apply_action
        else:
            if (((action_reg).car) is (symbol_drop) is not False):
                next = ((action_reg)).cdr.car
                increment_scan_counters(chars_reg)
                chars_reg = remaining(chars_reg)
                action_reg = next
                pc = apply_action
            else:
                if (((action_reg).car) is (symbol_goto) is not False):
                    state = ((action_reg)).cdr.car
                    if ((state) is (symbol_token_start_state) is not False):
                        mark_token_start()
                    action = apply_state(state, next_avail(chars_reg))
                    if ((action) is (symbol_error) is not False):
                        pc = unexpected_char_error
                    else:
                        action_reg = action
                        pc = apply_action
                else:
                    if (((action_reg).car) is (symbol_emit) is not False):
                        token_type = ((action_reg)).cdr.car
                        k_reg = make_cont(b_cont_1_d, chars_reg, fail_reg, k_reg)
                        token_type_reg = token_type
                        pc = convert_buffer_to_token
                    else:
                        raise Exception("symbol_apply_action: " + format("invalid action: ~a", *[action_reg]))

def scan_error():
    global exception_reg, pc
    exception_reg = make_exception("ScanError", msg_reg, src_reg, line_reg, char_reg)
    pc = apply_handler2

def unexpected_char_error():
    global char_reg, line_reg, msg_reg, pc
    c = next_avail(chars_reg)
    if (char_is__q(c, make_char('\0')) is not False):
        char_reg = scan_char
        line_reg = scan_line
        msg_reg = "unexpected end of input"
        pc = scan_error
    else:
        char_reg = scan_char
        line_reg = scan_line
        msg_reg = format("unexpected character '~a' encountered", c)
        pc = scan_error

def convert_buffer_to_token():
    global char_reg, line_reg, msg_reg, pc, value_reg
    buffer = reverse(buffer_reg)
    if ((token_type_reg) is (symbol_end_marker) is not False):
        value_reg = make_token1(symbol_end_marker)
        pc = apply_cont
    else:
        if ((token_type_reg) is (symbol_integer) is not False):
            value_reg = make_token2(symbol_integer, list_to_string(buffer))
            pc = apply_cont
        else:
            if ((token_type_reg) is (symbol_decimal) is not False):
                value_reg = make_token2(symbol_decimal, list_to_string(buffer))
                pc = apply_cont
            else:
                if ((token_type_reg) is (symbol_rational) is not False):
                    value_reg = make_token2(symbol_rational, list_to_string(buffer))
                    pc = apply_cont
                else:
                    if ((token_type_reg) is (symbol_identifier) is not False):
                        value_reg = make_token2(symbol_identifier, string_to_symbol(list_to_string(buffer)))
                        pc = apply_cont
                    else:
                        if ((token_type_reg) is (symbol_boolean) is not False):
                            value_reg = make_token2(symbol_boolean, (char_is__q((buffer).car, make_char('t'))) or (char_is__q((buffer).car, make_char('T'))))
                            pc = apply_cont
                        else:
                            if ((token_type_reg) is (symbol_character) is not False):
                                value_reg = make_token2(symbol_character, (buffer).car)
                                pc = apply_cont
                            else:
                                if ((token_type_reg) is (symbol_named_character) is not False):
                                    name = list_to_string(buffer)
                                    if (string_is__q(name, "nul") is not False):
                                        value_reg = make_token2(symbol_character, make_char('\0'))
                                        pc = apply_cont
                                    else:
                                        if (string_is__q(name, "space") is not False):
                                            value_reg = make_token2(symbol_character, make_char(' '))
                                            pc = apply_cont
                                        else:
                                            if (string_is__q(name, "tab") is not False):
                                                value_reg = make_token2(symbol_character, make_char('\t'))
                                                pc = apply_cont
                                            else:
                                                if (string_is__q(name, "newline") is not False):
                                                    value_reg = make_token2(symbol_character, make_char('\n'))
                                                    pc = apply_cont
                                                else:
                                                    if (string_is__q(name, "linefeed") is not False):
                                                        value_reg = make_token2(symbol_character, make_char('\n'))
                                                        pc = apply_cont
                                                    else:
                                                        if (string_is__q(name, "backspace") is not False):
                                                            value_reg = make_token2(symbol_character, make_char('\b'))
                                                            pc = apply_cont
                                                        else:
                                                            if (string_is__q(name, "return") is not False):
                                                                value_reg = make_token2(symbol_character, make_char('\r'))
                                                                pc = apply_cont
                                                            else:
                                                                if (string_is__q(name, "page") is not False):
                                                                    value_reg = make_token2(symbol_character, make_char(u"\u000C"))
                                                                    pc = apply_cont
                                                                else:
                                                                    char_reg = token_start_char
                                                                    line_reg = token_start_line
                                                                    msg_reg = format("invalid character name #\\~a", name)
                                                                    pc = scan_error
                                else:
                                    if ((token_type_reg) is (symbol_string) is not False):
                                        value_reg = make_token2(symbol_string, list_to_string(buffer))
                                        pc = apply_cont
                                    else:
                                        value_reg = make_token1(token_type_reg)
                                        pc = apply_cont

def make_token1(token_type):
    end = List(last_scan_line, last_scan_char, last_scan_position)
    start = List(token_start_line, token_start_char, token_start_position)
    if ((token_type) is (symbol_end_marker) is not False):
        return List(token_type, end, end)
    else:
        return List(token_type, start, end)

def make_token2(token_type, token_info):
    return List(token_type, token_info, List(token_start_line, token_start_char, token_start_position), List(last_scan_line, last_scan_char, last_scan_position))

def token_type_q(token, class_):
    return ((token).car) is (class_)

def get_token_start(token):
    return rac(rdc(token))

def get_token_end(token):
    return rac(token)

def get_token_start_line(token):
    return (get_token_start(token)).car

def get_token_start_char(token):
    return (get_token_start(token)).cdr.car

def get_token_start_pos(token):
    return (get_token_start(token)).cdr.cdr.car

def rac(ls):
    if ((((ls).cdr) is symbol_emptylist) is not False):
        return (ls).car
    else:
        current = (ls).cdr
        while pair_q((current).cdr):
            current = (current).cdr
        return (current).car

def rdc(ls):
    if ((((ls).cdr) is symbol_emptylist) is not False):
        return List()
    else:
        retval = List((ls).car)
        front = retval
        current = (ls).cdr
        while pair_q((current).cdr):
            set_cdr_b(retval, List((current).car))
            retval = (retval).cdr
            current = (current).cdr
        return front

def snoc(x, ls):
    if (((ls) is symbol_emptylist) is not False):
        return List(x)
    else:
        retval = List((ls).car)
        front = retval
        current = (ls).cdr
        while pair_q(current):
            set_cdr_b(retval, List((current).car))
            retval = (retval).cdr
            current = (current).cdr
        set_cdr_b(retval, List(x))
        return front

def char_delimiter_q(c):
    return (char_whitespace_q(c)) or (char_is__q(c, make_char("'"))) or (char_is__q(c, make_char('('))) or (char_is__q(c, make_char('['))) or (char_is__q(c, make_char(')'))) or (char_is__q(c, make_char(']'))) or (char_is__q(c, make_char('"'))) or (char_is__q(c, make_char(';'))) or (char_is__q(c, make_char('#'))) or (char_is__q(c, make_char('\0')))

def char_initial_q(c):
    return (char_alphabetic_q(c)) or (char_is__q(c, make_char('!'))) or (char_is__q(c, make_char('$'))) or (char_is__q(c, make_char('%'))) or (char_is__q(c, make_char('&'))) or (char_is__q(c, make_char('*'))) or (char_is__q(c, make_char('/'))) or (char_is__q(c, make_char(':'))) or (char_is__q(c, make_char('<'))) or (char_is__q(c, make_char('='))) or (char_is__q(c, make_char('>'))) or (char_is__q(c, make_char('?'))) or (char_is__q(c, make_char('^'))) or (char_is__q(c, make_char('_'))) or (char_is__q(c, make_char('~')))

def char_special_subsequent_q(c):
    return (char_is__q(c, make_char('+'))) or (char_is__q(c, make_char('-'))) or (char_is__q(c, make_char('@'))) or (char_is__q(c, make_char('.')))

def char_subsequent_q(c):
    return (char_initial_q(c)) or (char_numeric_q(c)) or (char_special_subsequent_q(c))

def char_sign_q(c):
    return (char_is__q(c, make_char('+'))) or (char_is__q(c, make_char('-')))

def char_boolean_q(c):
    return (char_is__q(c, make_char('t'))) or (char_is__q(c, make_char('T'))) or (char_is__q(c, make_char('f'))) or (char_is__q(c, make_char('F')))

def apply_state(state, c):
    if ((state) is (symbol_start_state) is not False):
        if (char_whitespace_q(c) is not False):
            return List(symbol_drop, List(symbol_goto, symbol_start_state))
        else:
            if (char_is__q(c, make_char(';')) is not False):
                return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if (char_is__q(c, make_char('\0')) is not False):
                    return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                else:
                    return List(symbol_goto, symbol_token_start_state)
    else:
        if ((state) is (symbol_token_start_state) is not False):
            if (char_is__q(c, make_char('(')) is not False):
                return List(symbol_drop, List(symbol_emit, symbol_lparen))
            else:
                if (char_is__q(c, make_char('[')) is not False):
                    return List(symbol_drop, List(symbol_emit, symbol_lbracket))
                else:
                    if (char_is__q(c, make_char(')')) is not False):
                        return List(symbol_drop, List(symbol_emit, symbol_rparen))
                    else:
                        if (char_is__q(c, make_char(']')) is not False):
                            return List(symbol_drop, List(symbol_emit, symbol_rbracket))
                        else:
                            if (char_is__q(c, make_char("'")) is not False):
                                return List(symbol_drop, List(symbol_emit, symbol_apostrophe))
                            else:
                                if (char_is__q(c, make_char('`')) is not False):
                                    return List(symbol_drop, List(symbol_emit, symbol_backquote))
                                else:
                                    if (char_is__q(c, make_char(',')) is not False):
                                        return List(symbol_drop, List(symbol_goto, symbol_comma_state))
                                    else:
                                        if (char_is__q(c, make_char('#')) is not False):
                                            return List(symbol_drop, List(symbol_goto, symbol_hash_prefix_state))
                                        else:
                                            if (char_is__q(c, make_char('"')) is not False):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_state))
                                            else:
                                                if (char_initial_q(c) is not False):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if (char_sign_q(c) is not False):
                                                        return List(symbol_shift, List(symbol_goto, symbol_signed_state))
                                                    else:
                                                        if (char_is__q(c, make_char('.')) is not False):
                                                            return List(symbol_shift, List(symbol_goto, symbol_decimal_point_state))
                                                        else:
                                                            if (char_numeric_q(c) is not False):
                                                                return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                            else:
                                                                return symbol_error
        else:
            if ((state) is (symbol_comment_state) is not False):
                if (char_is__q(c, make_char('\n')) is not False):
                    return List(symbol_drop, List(symbol_goto, symbol_start_state))
                else:
                    if (char_is__q(c, make_char('\0')) is not False):
                        return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                    else:
                        return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if ((state) is (symbol_comma_state) is not False):
                    if (char_is__q(c, make_char('@')) is not False):
                        return List(symbol_drop, List(symbol_emit, symbol_comma_at))
                    else:
                        return List(symbol_emit, symbol_comma)
                else:
                    if ((state) is (symbol_hash_prefix_state) is not False):
                        if (char_boolean_q(c) is not False):
                            return List(symbol_shift, List(symbol_emit, symbol_boolean))
                        else:
                            if (char_is__q(c, make_char('\\')) is not False):
                                return List(symbol_drop, List(symbol_goto, symbol_character_state))
                            else:
                                if (char_is__q(c, make_char('(')) is not False):
                                    return List(symbol_drop, List(symbol_emit, symbol_lvector))
                                else:
                                    return symbol_error
                    else:
                        if ((state) is (symbol_character_state) is not False):
                            if (char_alphabetic_q(c) is not False):
                                return List(symbol_shift, List(symbol_goto, symbol_alphabetic_character_state))
                            else:
                                if (not(char_is__q(c, make_char('\0'))) is not False):
                                    return List(symbol_shift, List(symbol_emit, symbol_character))
                                else:
                                    return symbol_error
                        else:
                            if ((state) is (symbol_alphabetic_character_state) is not False):
                                if (char_alphabetic_q(c) is not False):
                                    return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    return List(symbol_emit, symbol_character)
                            else:
                                if ((state) is (symbol_named_character_state) is not False):
                                    if (char_delimiter_q(c) is not False):
                                        return List(symbol_emit, symbol_named_character)
                                    else:
                                        return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    if ((state) is (symbol_string_state) is not False):
                                        if (char_is__q(c, make_char('"')) is not False):
                                            return List(symbol_drop, List(symbol_emit, symbol_string))
                                        else:
                                            if (char_is__q(c, make_char('\\')) is not False):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_escape_state))
                                            else:
                                                if (not(char_is__q(c, make_char('\0'))) is not False):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    return symbol_error
                                    else:
                                        if ((state) is (symbol_string_escape_state) is not False):
                                            if (char_is__q(c, make_char('"')) is not False):
                                                return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                            else:
                                                if (char_is__q(c, make_char('\\')) is not False):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    if (char_is__q(c, make_char('b')) is not False):
                                                        return List(symbol_replace, make_char('\b'), List(symbol_goto, symbol_string_state))
                                                    else:
                                                        if (char_is__q(c, make_char('f')) is not False):
                                                            return List(symbol_replace, make_char(u"\u000C"), List(symbol_goto, symbol_string_state))
                                                        else:
                                                            if (char_is__q(c, make_char('n')) is not False):
                                                                return List(symbol_replace, make_char('\n'), List(symbol_goto, symbol_string_state))
                                                            else:
                                                                if (char_is__q(c, make_char('t')) is not False):
                                                                    return List(symbol_replace, make_char('\t'), List(symbol_goto, symbol_string_state))
                                                                else:
                                                                    if (char_is__q(c, make_char('r')) is not False):
                                                                        return List(symbol_replace, make_char('\r'), List(symbol_goto, symbol_string_state))
                                                                    else:
                                                                        return symbol_error
                                        else:
                                            if ((state) is (symbol_identifier_state) is not False):
                                                if (char_subsequent_q(c) is not False):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if (char_delimiter_q(c) is not False):
                                                        return List(symbol_emit, symbol_identifier)
                                                    else:
                                                        return symbol_error
                                            else:
                                                if ((state) is (symbol_signed_state) is not False):
                                                    if (char_numeric_q(c) is not False):
                                                        return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                    else:
                                                        if (char_is__q(c, make_char('.')) is not False):
                                                            return List(symbol_shift, List(symbol_goto, symbol_signed_decimal_point_state))
                                                        else:
                                                            if (char_delimiter_q(c) is not False):
                                                                return List(symbol_emit, symbol_identifier)
                                                            else:
                                                                if (char_subsequent_q(c) is not False):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                else:
                                                    if ((state) is (symbol_decimal_point_state) is not False):
                                                        if (char_numeric_q(c) is not False):
                                                            return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                        else:
                                                            if (char_delimiter_q(c) is not False):
                                                                return List(symbol_emit, symbol_dot)
                                                            else:
                                                                if (char_subsequent_q(c) is not False):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                    else:
                                                        if ((state) is (symbol_signed_decimal_point_state) is not False):
                                                            if (char_numeric_q(c) is not False):
                                                                return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                            else:
                                                                if (char_delimiter_q(c) is not False):
                                                                    return List(symbol_emit, symbol_identifier)
                                                                else:
                                                                    if (char_subsequent_q(c) is not False):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                    else:
                                                                        return symbol_error
                                                        else:
                                                            if ((state) is (symbol_whole_number_state) is not False):
                                                                if (char_numeric_q(c) is not False):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                                else:
                                                                    if (char_is__q(c, make_char('.')) is not False):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if (char_is__q(c, make_char('/')) is not False):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state))
                                                                        else:
                                                                            if ((char_is__q(c, make_char('e'))) or (char_is__q(c, make_char('E'))) is not False):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                            else:
                                                                                if (char_delimiter_q(c) is not False):
                                                                                    return List(symbol_emit, symbol_integer)
                                                                                else:
                                                                                    if (char_subsequent_q(c) is not False):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                            else:
                                                                if ((state) is (symbol_fractional_number_state) is not False):
                                                                    if (char_numeric_q(c) is not False):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if ((char_is__q(c, make_char('e'))) or (char_is__q(c, make_char('E'))) is not False):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                        else:
                                                                            if (char_delimiter_q(c) is not False):
                                                                                return List(symbol_emit, symbol_decimal)
                                                                            else:
                                                                                if (char_subsequent_q(c) is not False):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                else:
                                                                    if ((state) is (symbol_rational_number_state) is not False):
                                                                        if (char_numeric_q(c) is not False):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                        else:
                                                                            if (char_delimiter_q(c) is not False):
                                                                                return List(symbol_emit, symbol_identifier)
                                                                            else:
                                                                                if (char_subsequent_q(c) is not False):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                    else:
                                                                        if ((state) is (symbol_rational_number_state_star) is not False):
                                                                            if (char_numeric_q(c) is not False):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                            else:
                                                                                if (char_delimiter_q(c) is not False):
                                                                                    return List(symbol_emit, symbol_rational)
                                                                                else:
                                                                                    if (char_subsequent_q(c) is not False):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                                        else:
                                                                            if ((state) is (symbol_suffix_state) is not False):
                                                                                if (char_sign_q(c) is not False):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_signed_exponent_state))
                                                                                else:
                                                                                    if (char_numeric_q(c) is not False):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if (char_delimiter_q(c) is not False):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if (char_subsequent_q(c) is not False):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                            else:
                                                                                if ((state) is (symbol_signed_exponent_state) is not False):
                                                                                    if (char_numeric_q(c) is not False):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if (char_delimiter_q(c) is not False):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if (char_subsequent_q(c) is not False):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                                else:
                                                                                    if ((state) is (symbol_exponent_state) is not False):
                                                                                        if (char_numeric_q(c) is not False):
                                                                                            return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                        else:
                                                                                            if (char_delimiter_q(c) is not False):
                                                                                                return List(symbol_emit, symbol_decimal)
                                                                                            else:
                                                                                                if (char_subsequent_q(c) is not False):
                                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                                else:
                                                                                                    return symbol_error
                                                                                    else:
                                                                                        raise Exception("symbol_apply_state: " + format("invalid state: ~a", *[state]))

def aatom_q(x):
    return (pair_q(x)) and (((x).car) is (atom_tag))

def apair_q(x):
    return (pair_q(x)) and (((x).car) is (pair_tag))

def annotated_q(x):
    return (pair_q(x)) and ((((x).car) is (atom_tag)) or (((x).car) is (pair_tag)))

def untag_atom_hat(aatom):
    return (aatom).cdr.car

def atom_q_hat(asexp):
    return ((asexp).car) is (atom_tag)

def pair_q_hat(asexp):
    return ((asexp).car) is (pair_tag)

def null_q_hat(asexp):
    return (atom_q_hat(asexp)) and (((untag_atom_hat(asexp)) is symbol_emptylist))

def symbol_q_hat(asexp):
    return (atom_q_hat(asexp)) and (symbol_q(untag_atom_hat(asexp)))

def string_q_hat(asexp):
    return (atom_q_hat(asexp)) and (string_q(untag_atom_hat(asexp)))

def vector_q_hat(asexp):
    return (atom_q_hat(asexp)) and (vector_q(untag_atom_hat(asexp)))

def car_hat(asexp):
    return (asexp).cdr.car

def cdr_hat(asexp):
    return (asexp).cdr.cdr.car

def cadr_hat(asexp):
    return car_hat(cdr_hat(asexp))

def cdar_hat(asexp):
    return cdr_hat(car_hat(asexp))

def caar_hat(asexp):
    return car_hat(car_hat(asexp))

def cddr_hat(asexp):
    return cdr_hat(cdr_hat(asexp))

def cdddr_hat(asexp):
    return cdr_hat(cdr_hat(cdr_hat(asexp)))

def caddr_hat(asexp):
    return car_hat(cdr_hat(cdr_hat(asexp)))

def cdadr_hat(asexp):
    return cdr_hat(car_hat(cdr_hat(asexp)))

def cadar_hat(asexp):
    return car_hat(cdr_hat(car_hat(asexp)))

def caadr_hat(asexp):
    return car_hat(car_hat(cdr_hat(asexp)))

def cadddr_hat(asexp):
    return car_hat(cdr_hat(cdr_hat(cdr_hat(asexp))))

def eq_q_hat(asexp, sym):
    return ((asexp).cdr.car) is (sym)

def vector_to_list_hat(asexp):
    return vector_to_list((asexp).cdr.car)

def symbol_to_string_hat(asexp):
    return symbol_to_string((asexp).cdr.car)

def list_q_hat(asexp):
    return (null_q_hat(asexp)) or ((pair_q_hat(asexp)) and (list_q_hat((asexp).cdr.cdr.car)))

def at_hat(alist):
    if (null_q_hat(alist) is not False):
        return symbol_emptylist
    else:
        return cons(car_hat(alist), at_hat(cdr_hat(alist)))

def length_hat(asexp):
    if (null_q_hat(asexp) is not False):
        return 0
    else:
        return (1) + (length_hat(cdr_hat(asexp)))

def cons_hat(a, b, info):
    return List(pair_tag, a, b, info)

def map_hat(f_hat, asexp):
    if (null_q_hat(asexp) is not False):
        return make_null_hat()
    else:
        return cons_hat(f_hat(car_hat(asexp)), map_hat(f_hat, cdr_hat(asexp)), symbol_none)

def make_null_hat():
    return List(atom_tag, symbol_emptylist, symbol_none)

def list_hat(x):
    return cons_hat(x, make_null_hat(), symbol_none)

def annotate_cps():
    global info_reg, k_reg, pc, value_reg, x_reg
    if (not(_starreader_generates_annotated_sexps_q_star) is not False):
        value_reg = x_reg
        pc = apply_cont
    else:
        if (annotated_q(x_reg) is not False):
            value_reg = x_reg
            pc = apply_cont
        else:
            if (pair_q(x_reg) is not False):
                k_reg = make_cont(b_cont_3_d, x_reg, info_reg, k_reg)
                info_reg = symbol_none
                x_reg = (x_reg).car
                pc = annotate_cps
            else:
                value_reg = List(atom_tag, x_reg, info_reg)
                pc = apply_cont

def unannotate_cps():
    global k_reg, pc, value_reg, x_reg
    if (aatom_q(x_reg) is not False):
        x_reg = (x_reg).cdr.car
        pc = unannotate_cps
    else:
        if (apair_q(x_reg) is not False):
            k_reg = make_cont(b_cont_7_d, x_reg, k_reg)
            x_reg = (x_reg).cdr.car
            pc = unannotate_cps
        else:
            if (pair_q(x_reg) is not False):
                k_reg = make_cont(b_cont_5_d, x_reg, k_reg)
                x_reg = (x_reg).car
                pc = unannotate_cps
            else:
                if (vector_q(x_reg) is not False):
                    k_reg = make_cont(b_cont_6_d, k_reg)
                    x_reg = vector_to_list(x_reg)
                    pc = unannotate_cps
                else:
                    value_reg = x_reg
                    pc = apply_cont

def filename_cache(filename):
    if (hasitem_native(_starfilename_dict_star, filename) is not False):
        return getitem_native(_starfilename_dict_star, filename)
    else:
        index = vlist_length_native(_starfilename_vector_star)
        vlist_append_native(_starfilename_vector_star, filename)
        setitem_native(_starfilename_dict_star, filename, index)
        return index

def get_filename_from_index(index):
    return vlist_ref_native(_starfilename_vector_star, index)

def make_info(src, start, end):
    return cons(filename_cache(src), append(start, end))

def replace_info(asexp, new_info):
    if (atom_q_hat(asexp) is not False):
        return List(atom_tag, (asexp).cdr.car, new_info)
    else:
        return List(pair_tag, (asexp).cdr.car, (asexp).cdr.cdr.car, new_info)

def get_srcfile(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return get_filename_from_index((info).car)

def get_start_line(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return (info).cdr.car

def get_start_char(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return (info).cdr.cdr.car

def get_start_pos(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return (info).cdr.cdr.cdr.car

def get_end_line(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return ((info).cdr.cdr.cdr.cdr).car

def get_end_char(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return ((info).cdr.cdr.cdr.cdr).cdr.car

def get_end_pos(info):
    if ((info) is (symbol_none) is not False):
        return symbol_none
    else:
        return ((info).cdr.cdr.cdr.cdr).cdr.cdr.car

def get_source_info(asexp):
    return rac(asexp)

def source_info_q(x):
    return ((x) is (symbol_none)) or (list_q(x))

def has_source_info_q(asexp):
    return not((get_source_info(asexp)) is (symbol_none))

def original_source_info_q(asexp):
    return (has_source_info_q(asexp)) and (numeric_equal(length(get_source_info(asexp)), 7))

def macro_derived_source_info_q(asexp):
    return (has_source_info_q(asexp)) and (numeric_equal(length(get_source_info(asexp)), 8))

def first(x):
    return (x).car

def rest_of(x):
    return (x).cdr

def unexpected_token_error():
    global msg_reg, pc
    token = first(tokens_reg)
    if (token_type_q(token, symbol_end_marker) is not False):
        msg_reg = "unexpected end of input"
        pc = read_error
    else:
        msg_reg = format("unexpected '~a' encountered", (token).car)
        pc = read_error

def read_error():
    global exception_reg, pc
    token = first(tokens_reg)
    exception_reg = make_exception("ReadError", msg_reg, src_reg, get_token_start_line(token), get_token_start_char(token))
    pc = apply_handler2

def read_sexp():
    global expected_terminator_reg, info_reg, k_reg, keyword_reg, msg_reg, pc, tokens_reg, x_reg
    end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    temp_1 = first(tokens_reg)
    if (((temp_1).car) is (symbol_integer) is not False):
        str = ((temp_1)).cdr.car
        k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
        info_reg = make_info(src_reg, start, end)
        x_reg = string_to_integer(str)
        pc = annotate_cps
    else:
        if (((temp_1).car) is (symbol_decimal) is not False):
            str = ((temp_1)).cdr.car
            k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
            info_reg = make_info(src_reg, start, end)
            x_reg = string_to_decimal(str)
            pc = annotate_cps
        else:
            if (((temp_1).car) is (symbol_rational) is not False):
                str = ((temp_1)).cdr.car
                num = string_to_rational(str)
                if (true_q(num) is not False):
                    k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    info_reg = make_info(src_reg, start, end)
                    x_reg = num
                    pc = annotate_cps
                else:
                    msg_reg = format("cannot represent ~a", str)
                    pc = read_error
            else:
                if (((temp_1).car) is (symbol_boolean) is not False):
                    bool = ((temp_1)).cdr.car
                    k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    info_reg = make_info(src_reg, start, end)
                    x_reg = bool
                    pc = annotate_cps
                else:
                    if (((temp_1).car) is (symbol_character) is not False):
                        char = ((temp_1)).cdr.car
                        k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                        info_reg = make_info(src_reg, start, end)
                        x_reg = char
                        pc = annotate_cps
                    else:
                        if (((temp_1).car) is (symbol_string) is not False):
                            str = ((temp_1)).cdr.car
                            k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                            info_reg = make_info(src_reg, start, end)
                            x_reg = str
                            pc = annotate_cps
                        else:
                            if (((temp_1).car) is (symbol_identifier) is not False):
                                id = ((temp_1)).cdr.car
                                k_reg = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                                info_reg = make_info(src_reg, start, end)
                                x_reg = id
                                pc = annotate_cps
                            else:
                                if (((temp_1).car) is (symbol_apostrophe) is not False):
                                    keyword_reg = symbol_quote
                                    pc = read_abbreviation
                                else:
                                    if (((temp_1).car) is (symbol_backquote) is not False):
                                        keyword_reg = symbol_quasiquote
                                        pc = read_abbreviation
                                    else:
                                        if (((temp_1).car) is (symbol_comma) is not False):
                                            keyword_reg = symbol_unquote
                                            pc = read_abbreviation
                                        else:
                                            if (((temp_1).car) is (symbol_comma_at) is not False):
                                                keyword_reg = symbol_unquote_splicing
                                                pc = read_abbreviation
                                            else:
                                                if (((temp_1).car) is (symbol_lparen) is not False):
                                                    tokens = rest_of(tokens_reg)
                                                    k_reg = make_cont4(b_cont4_1_d, src_reg, start, k_reg)
                                                    expected_terminator_reg = symbol_rparen
                                                    tokens_reg = tokens
                                                    pc = read_sexp_sequence
                                                else:
                                                    if (((temp_1).car) is (symbol_lbracket) is not False):
                                                        tokens = rest_of(tokens_reg)
                                                        k_reg = make_cont4(b_cont4_1_d, src_reg, start, k_reg)
                                                        expected_terminator_reg = symbol_rbracket
                                                        tokens_reg = tokens
                                                        pc = read_sexp_sequence
                                                    else:
                                                        if (((temp_1).car) is (symbol_lvector) is not False):
                                                            k_reg = make_cont4(b_cont4_2_d, src_reg, start, k_reg)
                                                            tokens_reg = rest_of(tokens_reg)
                                                            pc = read_vector_sequence
                                                        else:
                                                            pc = unexpected_token_error

def read_abbreviation():
    global info_reg, k_reg, pc, x_reg
    keyword_end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    k_reg = make_cont(b_cont_10_d, src_reg, start, tokens_reg, handler_reg, fail_reg, k_reg)
    info_reg = make_info(src_reg, start, keyword_end)
    x_reg = keyword_reg
    pc = annotate_cps

def read_vector_sequence():
    global expected_terminator_reg, k_reg, msg_reg, pc, sexps_reg
    temp_1 = first(tokens_reg)
    if (((temp_1).car) is (symbol_rparen) is not False):
        expected_terminator_reg = symbol_rparen
        sexps_reg = symbol_emptylist
        pc = close_sexp_sequence
    else:
        if (((temp_1).car) is (symbol_dot) is not False):
            msg_reg = "unexpected dot (.)"
            pc = read_error
        else:
            k_reg = make_cont4(b_cont4_5_d, src_reg, handler_reg, k_reg)
            pc = read_sexp

def read_sexp_sequence():
    global k_reg, msg_reg, pc, sexps_reg
    temp_1 = first(tokens_reg)
    if (memq((temp_1).car, List(symbol_rparen, symbol_rbracket)) is not False):
        sexps_reg = symbol_emptylist
        pc = close_sexp_sequence
    else:
        if (((temp_1).car) is (symbol_dot) is not False):
            msg_reg = "unexpected dot (.)"
            pc = read_error
        else:
            k_reg = make_cont4(b_cont4_7_d, expected_terminator_reg, src_reg, handler_reg, k_reg)
            pc = read_sexp

def close_sexp_sequence():
    global msg_reg, pc, value1_reg, value2_reg, value3_reg, value4_reg
    end = get_token_end(first(tokens_reg))
    temp_1 = first(tokens_reg)
    if (memq((temp_1).car, List(symbol_rparen, symbol_rbracket)) is not False):
        if (token_type_q(first(tokens_reg), expected_terminator_reg) is not False):
            value4_reg = fail_reg
            value3_reg = rest_of(tokens_reg)
            value2_reg = end
            value1_reg = sexps_reg
            pc = apply_cont4
        else:
            if ((expected_terminator_reg) is (symbol_rparen) is not False):
                msg_reg = "parenthesized list terminated by bracket"
                pc = read_error
            else:
                if ((expected_terminator_reg) is (symbol_rbracket) is not False):
                    msg_reg = "bracketed list terminated by parenthesis"
                    pc = read_error
    else:
        pc = unexpected_token_error

def make_binding(value, docstring):
    return cons(value, docstring)

def binding_value(binding):
    return (binding).car

def binding_docstring(binding):
    return (binding).cdr

def set_binding_docstring_b(binding, docstring):
    set_cdr_b(binding, docstring)

def empty_frame_q(frame):
    return (((frame).cdr.car) is symbol_emptylist)

def frame_bindings(frame):
    return (frame).car

def environment_q(x):
    return (pair_q(x)) and (((x).car) is (symbol_environment))

def make_empty_environment():
    return List(symbol_environment, make_frame(symbol_emptylist, symbol_emptylist, symbol_emptylist))

def make_initial_environment(vars, vals, docstrings):
    return List(symbol_environment, make_frame(vars, vals, docstrings))

def first_frame(env):
    return (env).cdr.car

def first_frame_vars(env):
    return (first_frame(env)).cdr.car

def initial_contours(env):
    return (first_frame(env)).cdr

def frames(env):
    return (env).cdr

def set_first_frame_b(env, new_frame):
    set_car_b((env).cdr, new_frame)

def extend(env, variables, values, docstrings):
    return cons(symbol_environment, cons(make_frame(variables, values, docstrings), (env).cdr))

def search_env(env, variable):
    return search_frames((env).cdr, variable)

def search_frames(frames, variable):
    if (((frames) is symbol_emptylist) is not False):
        return False
    else:
        binding = search_frame((frames).car, variable)
        if (binding is not False):
            return binding
        else:
            return search_frames((frames).cdr, variable)

def in_first_frame_q(var, env):
    return true_q(memq(var, first_frame_vars(env)))

def get_first_frame_value(var, env):
    return binding_value(search_frame(first_frame(env), var))

def lookup_value_by_lexical_address():
    global pc, value1_reg, value2_reg
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    value2_reg = fail_reg
    value1_reg = binding_value(vector_ref(bindings, offset_reg))
    pc = apply_cont2

def lookup_binding_by_lexical_address():
    global pc, value1_reg, value2_reg
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    value2_reg = fail_reg
    value1_reg = vector_ref(bindings, offset_reg)
    pc = apply_cont2

def lookup_value():
    global dk_reg, gk_reg, pc, sk_reg
    sk_reg = make_cont2(b_cont2_3_d, k_reg)
    dk_reg = make_cont3(b_cont3_3_d, k_reg)
    gk_reg = make_cont2(b_cont2_4_d, k_reg)
    pc = lookup_variable

def lookup_variable():
    global components_reg, info_reg, k_reg, module_reg, msg_reg, path_reg, pc, value1_reg, value2_reg, value3_reg
    binding = search_env(env_reg, var_reg)
    if (binding is not False):
        value2_reg = fail_reg
        value1_reg = binding
        k_reg = sk_reg
        pc = apply_cont2
    else:
        components = split_variable(var_reg)
        if (((((components).cdr) is symbol_emptylist)) and (dlr_env_contains((components).car)) is not False):
            value2_reg = fail_reg
            value1_reg = (components).car
            k_reg = gk_reg
            pc = apply_cont2
        else:
            if ((not((((components).cdr) is symbol_emptylist))) and (dlr_env_contains((components).car)) and (dlr_object_contains(dlr_env_lookup((components).car), components)) is not False):
                value3_reg = fail_reg
                value2_reg = components
                value1_reg = dlr_env_lookup((components).car)
                k_reg = dk_reg
                pc = apply_cont3
            else:
                if ((((components).cdr) is symbol_emptylist) is not False):
                    info_reg = var_info_reg
                    msg_reg = format("unbound variable '~a'", var_reg)
                    pc = runtime_error
                else:
                    module_reg = env_reg
                    path_reg = ""
                    components_reg = components
                    pc = lookup_variable_components

def lookup_variable_components():
    global components_reg, info_reg, k_reg, module_reg, msg_reg, path_reg, pc, value1_reg, value2_reg, value3_reg
    var = (components_reg).car
    binding = search_env(module_reg, var)
    if (binding is not False):
        if ((((components_reg).cdr) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = binding
            k_reg = sk_reg
            pc = apply_cont2
        else:
            new_path = (format("~a", var) if string_is__q(path_reg, "") else format("~a.~a", path_reg, var))
            value = binding_value(binding)
            if (environment_q(value) is not False):
                module_reg = value
                path_reg = new_path
                components_reg = (components_reg).cdr
                pc = lookup_variable_components
            else:
                if (dlr_object_contains(value, components_reg) is not False):
                    value3_reg = fail_reg
                    value2_reg = components_reg
                    value1_reg = value
                    k_reg = dk_reg
                    pc = apply_cont3
                else:
                    info_reg = var_info_reg
                    msg_reg = format("'~a' is not a module", new_path)
                    pc = runtime_error
    else:
        if (string_is__q(path_reg, "") is not False):
            info_reg = var_info_reg
            msg_reg = format("undefined item in '~a'", var)
            pc = runtime_error
        else:
            info_reg = var_info_reg
            msg_reg = format("unbound variable '~a' in module '~a'", var, path_reg)
            pc = runtime_error

def lookup_binding_in_first_frame():
    global pc, value1_reg, value2_reg
    frame = first_frame(env_reg)
    binding = search_frame(frame, var_reg)
    if (binding is not False):
        value2_reg = fail_reg
        value1_reg = binding
        pc = apply_cont2
    else:
        new_binding = make_binding(symbol_undefined, "")
        new_frame = add_binding(var_reg, new_binding, frame)
        set_first_frame_b(env_reg, new_frame)
        value2_reg = fail_reg
        value1_reg = new_binding
        pc = apply_cont2

def split_variable(var):
    strings = string_split(symbol_to_string(var), make_char('.'))
    if (member("", strings) is not False):
        return symbol_emptylist
    else:
        return Map(string_to_symbol, strings)

def id_q(exp):
    return (symbol_q(exp)) or (association_q(exp))

def head(formals):
    if (symbol_q(formals) is not False):
        return symbol_emptylist
    else:
        if (association_q(formals) is not False):
            return symbol_emptylist
        else:
            if (pair_q((formals).cdr) is not False):
                return cons((formals).car, head((formals).cdr))
            else:
                return List((formals).car)

def last(formals):
    if (symbol_q(formals) is not False):
        return formals
    else:
        if (association_q(formals) is not False):
            return formals
        else:
            if (pair_q((formals).cdr) is not False):
                return last((formals).cdr)
            else:
                return (formals).cdr

def anything_q(datum):
    return True

def application_q_hat(asexp):
    return (list_q_hat(asexp)) and (not(null_q_hat(asexp))) and (not(reserved_keyword_q(untag_atom_hat(car_hat(asexp)))))

def reserved_keyword_q(x):
    return (symbol_q(x)) and (not((memq(x, get_reserved_keywords())) is (False)))

def get_reserved_keywords():
    return List(symbol_quote, symbol_func, symbol_define_b, symbol_quasiquote, symbol_lambda, symbol_if, symbol_set_b, symbol_define, symbol_begin, symbol_cond, symbol_and, symbol_or, symbol_let, symbol_let_star, symbol_letrec, symbol_case, symbol_record_case, symbol_try, symbol_catch, symbol_finally, symbol_raise, symbol_define_syntax, symbol_choose, symbol_define_datatype, symbol_cases, symbol_trace_lambda)

def mit_style_define_q_hat(asexp):
    return (define_q_hat(asexp)) and (pair_q_hat(cadr_hat(asexp)))

def literal_q(datum):
    return (number_q(datum)) or (boolean_q(datum)) or (((datum) is symbol_emptylist)) or (char_q(datum)) or (string_q(datum))

def literal_q_hat(asexp):
    return (((asexp).car) is (atom_tag)) and ((number_q(untag_atom_hat(asexp))) or (boolean_q(untag_atom_hat(asexp))) or (((untag_atom_hat(asexp)) is symbol_emptylist)) or (char_q(untag_atom_hat(asexp))) or (string_q(untag_atom_hat(asexp))))

def syntactic_sugar_q_hat(asexp):
    return (pair_q_hat(asexp)) and (symbol_q_hat(car_hat(asexp))) and (in_first_frame_q(untag_atom_hat(car_hat(asexp)), macro_env))

def list_of_test_groups_q_hat(x):
    return (null_q_hat(x)) or ((pair_q_hat(x)) and (atom_q_hat(car_hat(x))) and ((integer_q(untag_atom_hat(car_hat(x)))) or (string_q(untag_atom_hat(car_hat(x))))) and (list_of_test_groups_q_hat(cdr_hat(x))))

def define_var_hat(x):
    return untag_atom_hat(cadr_hat(x))

def define_docstring_hat(x):
    return untag_atom_hat(caddr_hat(x))

def try_body_hat(x):
    return cadr_hat(x)

def catch_var_hat(x):
    return untag_atom_hat(cadr_hat(caddr_hat(x)))

def catch_exps_hat(x):
    return cddr_hat(caddr_hat(x))

def try_finally_exps_hat(x):
    return cdr_hat(caddr_hat(x))

def try_catch_finally_exps_hat(x):
    return cdr_hat(cadddr_hat(x))

def aparse():
    global adatum_list_reg, adatum_reg, args_reg, ax_reg, datum_reg, depth_reg, id_reg, info_reg, k_reg, macro_reg, msg_reg, pc, value1_reg, value2_reg, x_reg
    info = get_source_info(adatum_reg)
    if (literal_q_hat(adatum_reg) is not False):
        value2_reg = fail_reg
        value1_reg = lit_aexp(untag_atom_hat(adatum_reg), info)
        pc = apply_cont2
    else:
        if (symbol_q_hat(adatum_reg) is not False):
            if (_staruse_lexical_address_star is not False):
                info_reg = info
                depth_reg = 0
                id_reg = untag_atom_hat(adatum_reg)
                pc = get_lexical_address
            else:
                value2_reg = fail_reg
                value1_reg = var_aexp(untag_atom_hat(adatum_reg), info)
                pc = apply_cont2
        else:
            if (vector_q_hat(adatum_reg) is not False):
                k_reg = make_cont(b_cont_19_d, info, fail_reg, k_reg)
                x_reg = adatum_reg
                pc = unannotate_cps
            else:
                if (quote_q_hat(adatum_reg) is not False):
                    k_reg = make_cont(b_cont_20_d, info, fail_reg, k_reg)
                    x_reg = adatum_reg
                    pc = unannotate_cps
                else:
                    if (quasiquote_q_hat(adatum_reg) is not False):
                        k_reg = make_cont(b_cont_18_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                        depth_reg = 0
                        ax_reg = cadr_hat(adatum_reg)
                        pc = qq_expand_cps
                    else:
                        if (unquote_q_hat(adatum_reg) is not False):
                            msg_reg = "misplaced"
                            pc = aparse_error
                        else:
                            if (unquote_splicing_q_hat(adatum_reg) is not False):
                                msg_reg = "misplaced"
                                pc = aparse_error
                            else:
                                if (syntactic_sugar_q_hat(adatum_reg) is not False):
                                    k_reg = make_cont2(b_cont2_35_d, senv_reg, handler_reg, k_reg)
                                    pc = expand_once_hat
                                else:
                                    if (if_then_q_hat(adatum_reg) is not False):
                                        k_reg = make_cont2(b_cont2_31_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                        adatum_reg = cadr_hat(adatum_reg)
                                        pc = aparse
                                    else:
                                        if (if_else_q_hat(adatum_reg) is not False):
                                            k_reg = make_cont2(b_cont2_34_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                            adatum_reg = cadr_hat(adatum_reg)
                                            pc = aparse
                                        else:
                                            if (help_q_hat(adatum_reg) is not False):
                                                var_info = get_source_info(cadr_hat(adatum_reg))
                                                value2_reg = fail_reg
                                                value1_reg = help_aexp(untag_atom_hat(cadr_hat(adatum_reg)), var_info, info)
                                                pc = apply_cont2
                                            else:
                                                if (assignment_q_hat(adatum_reg) is not False):
                                                    k_reg = make_cont2(b_cont2_29_d, adatum_reg, info, k_reg)
                                                    adatum_reg = caddr_hat(adatum_reg)
                                                    pc = aparse
                                                else:
                                                    if (association_q_hat(adatum_reg) is not False):
                                                        k_reg = make_cont2(b_cont2_27_d, adatum_reg, info, k_reg)
                                                        adatum_reg = caddr_hat(adatum_reg)
                                                        pc = aparse
                                                    else:
                                                        if (func_q_hat(adatum_reg) is not False):
                                                            k_reg = make_cont2(b_cont2_28_d, info, k_reg)
                                                            adatum_reg = cadr_hat(adatum_reg)
                                                            pc = aparse
                                                        else:
                                                            if (callback_q_hat(adatum_reg) is not False):
                                                                k_reg = make_cont2(b_cont2_24_d, info, k_reg)
                                                                adatum_reg = cadr_hat(adatum_reg)
                                                                pc = aparse
                                                            else:
                                                                if (define_q_hat(adatum_reg) is not False):
                                                                    if (mit_style_define_q_hat(adatum_reg) is not False):
                                                                        k_reg = make_cont(b_cont_15_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                        datum_reg = adatum_reg
                                                                        macro_reg = mit_define_transformer_hat
                                                                        pc = apply_macro
                                                                    else:
                                                                        if ((numeric_equal(length_hat(adatum_reg), 3)) and (symbol_q_hat(cadr_hat(adatum_reg))) is not False):
                                                                            k_reg = make_cont2(b_cont2_26_d, adatum_reg, info, k_reg)
                                                                            adatum_reg = caddr_hat(adatum_reg)
                                                                            pc = aparse
                                                                        else:
                                                                            if ((numeric_equal(length_hat(adatum_reg), 4)) and (symbol_q_hat(cadr_hat(adatum_reg))) and (string_q_hat(caddr_hat(adatum_reg))) is not False):
                                                                                k_reg = make_cont2(b_cont2_25_d, adatum_reg, info, k_reg)
                                                                                adatum_reg = cadddr_hat(adatum_reg)
                                                                                pc = aparse
                                                                            else:
                                                                                msg_reg = "bad concrete syntax:"
                                                                                pc = aparse_error
                                                                else:
                                                                    if (define_b_q_hat(adatum_reg) is not False):
                                                                        if (mit_style_define_q_hat(adatum_reg) is not False):
                                                                            k_reg = make_cont(b_cont_15_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                            datum_reg = adatum_reg
                                                                            macro_reg = mit_define_transformer_hat
                                                                            pc = apply_macro
                                                                        else:
                                                                            if (numeric_equal(length_hat(adatum_reg), 3) is not False):
                                                                                k_reg = make_cont2(b_cont2_22_d, adatum_reg, info, k_reg)
                                                                                adatum_reg = caddr_hat(adatum_reg)
                                                                                pc = aparse
                                                                            else:
                                                                                if ((numeric_equal(length_hat(adatum_reg), 4)) and (string_q_hat(caddr_hat(adatum_reg))) is not False):
                                                                                    k_reg = make_cont2(b_cont2_21_d, adatum_reg, info, k_reg)
                                                                                    adatum_reg = cadddr_hat(adatum_reg)
                                                                                    pc = aparse
                                                                                else:
                                                                                    msg_reg = "bad concrete syntax:"
                                                                                    pc = aparse_error
                                                                    else:
                                                                        if (define_syntax_q_hat(adatum_reg) is not False):
                                                                            name = define_var_hat(adatum_reg)
                                                                            if (lambda_q_hat(caddr_hat(adatum_reg)) is not False):
                                                                                k_reg = make_cont2(b_cont2_23_d, name, info, k_reg)
                                                                                adatum_reg = caddr_hat(adatum_reg)
                                                                                pc = aparse
                                                                            else:
                                                                                aclauses = cddr_hat(adatum_reg)
                                                                                k_reg = make_cont(b_cont_16_d, aclauses, name, info, fail_reg, k_reg)
                                                                                x_reg = aclauses
                                                                                pc = unannotate_cps
                                                                        else:
                                                                            if (define_tests_q_hat(adatum_reg) is not False):
                                                                                aclauses = cddr_hat(adatum_reg)
                                                                                name = define_var_hat(adatum_reg)
                                                                                k_reg = make_cont2(b_cont2_19_d, name, info, k_reg)
                                                                                adatum_list_reg = aclauses
                                                                                pc = aparse_all
                                                                            else:
                                                                                if (run_tests_q_hat(adatum_reg) is not False):
                                                                                    args = cdr_hat(adatum_reg)
                                                                                    if (null_q_hat(args) is not False):
                                                                                        value2_reg = fail_reg
                                                                                        value1_reg = run_tests_aexp(symbol_emptylist)
                                                                                        pc = apply_cont2
                                                                                    else:
                                                                                        if ((symbol_q_hat(car_hat(args))) and (list_of_test_groups_q_hat(cdr_hat(args))) is not False):
                                                                                            k_reg = make_cont2(b_cont2_20_d, k_reg)
                                                                                            args_reg = list_hat(args)
                                                                                            pc = aparse_unit_tests
                                                                                        else:
                                                                                            k_reg = make_cont2(b_cont2_20_d, k_reg)
                                                                                            args_reg = args
                                                                                            pc = aparse_unit_tests
                                                                                else:
                                                                                    if (begin_q_hat(adatum_reg) is not False):
                                                                                        if (null_q_hat(cdr_hat(adatum_reg)) is not False):
                                                                                            msg_reg = "bad concrete syntax:"
                                                                                            pc = aparse_error
                                                                                        else:
                                                                                            if (null_q_hat(cddr_hat(adatum_reg)) is not False):
                                                                                                adatum_reg = cadr_hat(adatum_reg)
                                                                                                pc = aparse
                                                                                            else:
                                                                                                k_reg = make_cont2(b_cont2_17_d, info, k_reg)
                                                                                                adatum_list_reg = cdr_hat(adatum_reg)
                                                                                                pc = aparse_all
                                                                                    else:
                                                                                        if (lambda_no_defines_q_hat(adatum_reg) is not False):
                                                                                            k_reg = make_cont(b_cont_13_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                            x_reg = cadr_hat(adatum_reg)
                                                                                            pc = unannotate_cps
                                                                                        else:
                                                                                            if (trace_lambda_no_defines_q_hat(adatum_reg) is not False):
                                                                                                k_reg = make_cont(b_cont_12_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                                x_reg = caddr_hat(adatum_reg)
                                                                                                pc = unannotate_cps
                                                                                            else:
                                                                                                if (try_q_hat(adatum_reg) is not False):
                                                                                                    if ((numeric_equal(length_hat(adatum_reg), 3)) and (catch_q_hat(caddr_hat(adatum_reg))) is not False):
                                                                                                        k_reg = make_cont2(b_cont2_14_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                        adatum_reg = try_body_hat(adatum_reg)
                                                                                                        pc = aparse
                                                                                                    else:
                                                                                                        if ((numeric_equal(length_hat(adatum_reg), 3)) and (finally_q_hat(caddr_hat(adatum_reg))) is not False):
                                                                                                            k_reg = make_cont2(b_cont2_16_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                            adatum_reg = try_body_hat(adatum_reg)
                                                                                                            pc = aparse
                                                                                                        else:
                                                                                                            if ((numeric_equal(length_hat(adatum_reg), 4)) and (catch_q_hat(caddr_hat(adatum_reg))) and (finally_q_hat(cadddr_hat(adatum_reg))) is not False):
                                                                                                                k_reg = make_cont2(b_cont2_12_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                adatum_reg = try_body_hat(adatum_reg)
                                                                                                                pc = aparse
                                                                                                            else:
                                                                                                                k_reg = make_cont2(b_cont2_6_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                adatum_reg = car_hat(adatum_reg)
                                                                                                                pc = aparse
                                                                                                else:
                                                                                                    if (raise_q_hat(adatum_reg) is not False):
                                                                                                        k_reg = make_cont2(b_cont2_7_d, info, k_reg)
                                                                                                        adatum_reg = cadr_hat(adatum_reg)
                                                                                                        pc = aparse
                                                                                                    else:
                                                                                                        if (choose_q_hat(adatum_reg) is not False):
                                                                                                            k_reg = make_cont2(b_cont2_8_d, info, k_reg)
                                                                                                            adatum_list_reg = cdr_hat(adatum_reg)
                                                                                                            pc = aparse_all
                                                                                                        else:
                                                                                                            if (application_q_hat(adatum_reg) is not False):
                                                                                                                k_reg = make_cont2(b_cont2_6_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                adatum_reg = car_hat(adatum_reg)
                                                                                                                pc = aparse
                                                                                                            else:
                                                                                                                msg_reg = "bad concrete syntax:"
                                                                                                                pc = aparse_error

def aparse_unit_tests():
    global adatum_reg, args_reg, k_reg, msg_reg, pc, value1_reg, value2_reg
    if (null_q_hat(args_reg) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        if (symbol_q_hat(car_hat(args_reg)) is not False):
            k_reg = make_cont2(b_cont2_37_d, args_reg, k_reg)
            args_reg = cdr_hat(args_reg)
            pc = aparse_unit_tests
        else:
            if ((list_q_hat(car_hat(args_reg))) and (not(null_q_hat(car_hat(args_reg)))) and (symbol_q_hat(caar_hat(args_reg))) and (list_of_test_groups_q_hat(cdar_hat(args_reg))) is not False):
                k_reg = make_cont2(b_cont2_36_d, args_reg, k_reg)
                args_reg = cdr_hat(args_reg)
                pc = aparse_unit_tests
            else:
                adatum_reg = car_hat(args_reg)
                msg_reg = "bad unit test syntax:"
                pc = aparse_error

def aparse_all():
    global adatum_reg, k_reg, pc, value1_reg, value2_reg
    if (null_q_hat(adatum_list_reg) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        k_reg = make_cont2(b_cont2_39_d, adatum_list_reg, senv_reg, handler_reg, k_reg)
        adatum_reg = car_hat(adatum_list_reg)
        pc = aparse

def aparse_error():
    global k_reg, pc, x_reg
    info = get_source_info(adatum_reg)
    k_reg = make_cont(b_cont_22_d, msg_reg, info, handler_reg, fail_reg)
    x_reg = adatum_reg
    pc = unannotate_cps

def aparse_sexps():
    global k_reg, pc, value1_reg, value2_reg
    if (token_type_q(first(tokens_reg), symbol_end_marker) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        k_reg = make_cont4(b_cont4_9_d, senv_reg, src_reg, handler_reg, k_reg)
        pc = read_sexp

def get_lexical_address():
    global contours_reg, depth_reg, offset_reg, pc, senv_reg, value1_reg, value2_reg
    if (((senv_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = var_aexp(id_reg, info_reg)
        pc = apply_cont2
    else:
        if (memq(id_reg, (senv_reg).car) is not False):
            offset_reg = 0
            contours_reg = (senv_reg).car
            pc = get_lexical_address_offset
        else:
            depth_reg = (depth_reg) + (1)
            senv_reg = (senv_reg).cdr
            pc = get_lexical_address

def get_lexical_address_offset():
    global contours_reg, offset_reg, pc, value1_reg, value2_reg
    if (((contours_reg).car) is (id_reg) is not False):
        value2_reg = fail_reg
        value1_reg = lexical_address_aexp(depth_reg, offset_reg, id_reg, info_reg)
        pc = apply_cont2
    else:
        offset_reg = (offset_reg) + (1)
        contours_reg = (contours_reg).cdr
        pc = get_lexical_address_offset

def get_internal_defines_hat(bodies, adatum, handler, fail, k):
    global adatum_reg, fail_reg, handler_reg, msg_reg, pc
    if (null_q_hat(bodies) is not False):
        fail_reg = fail
        handler_reg = handler
        adatum_reg = adatum
        msg_reg = "no body expressions found for"
        pc = aparse_error
    else:
        if (define_q_hat(car_hat(bodies)) is not False):
            return get_internal_defines_hat(cdr_hat(bodies), adatum, handler, fail, make_cont2(b_cont2_44_d, bodies, k))
        else:
            return any_internal_defines_q_hat(cdr_hat(bodies), make_cont(b_cont_25_d, adatum, bodies, handler, fail, k))

def any_internal_defines_q_hat(exps, k):
    global k_reg, pc, value_reg
    if (null_q_hat(exps) is not False):
        value_reg = False
        k_reg = k
        pc = apply_cont
    else:
        if (define_q_hat(car_hat(exps)) is not False):
            value_reg = True
            k_reg = k
            pc = apply_cont
        else:
            return any_internal_defines_q_hat(cdr_hat(exps), k)

def create_letrec_bindings_hat(defines, handler, fail, k):
    global k_reg, pc, value_reg
    if (((defines) is symbol_emptylist) is not False):
        value_reg = symbol_emptylist
        k_reg = k
        pc = apply_cont
    else:
        return create_letrec_bindings_hat((defines).cdr, handler, fail, make_cont(b_cont_26_d, defines, handler, fail, k))

def get_define_var_and_exp_hat(adatum, handler, fail, k):
    global adatum_reg, fail_reg, handler_reg, k_reg, msg_reg, pc, value1_reg, value2_reg
    if (mit_style_define_q_hat(adatum) is not False):
        bodies = cddr_hat(adatum)
        formals = cdadr_hat(adatum)
        name = caadr_hat(adatum)
        value2_reg = append(List(symbol_lambda), append(List(formals), at_hat(bodies)))
        value1_reg = name
        k_reg = k
        pc = apply_cont2
    else:
        if (numeric_equal(length_hat(adatum), 3) is not False):
            exp = caddr_hat(adatum)
            name = define_var_hat(adatum)
            value2_reg = exp
            value1_reg = name
            k_reg = k
            pc = apply_cont2
        else:
            if ((numeric_equal(length_hat(adatum), 4)) and (string_q_hat(caddr_hat(adatum))) is not False):
                exp = cadddr_hat(adatum)
                name = define_var_hat(adatum)
                value2_reg = exp
                value1_reg = name
                k_reg = k
                pc = apply_cont2
            else:
                fail_reg = fail
                handler_reg = handler
                adatum_reg = adatum
                msg_reg = "bad concrete syntax:"
                pc = aparse_error

def create_letrec_assignments_hat():
    global k2_reg, k_reg, pc, procs_reg, value1_reg, value2_reg, vars_reg
    if (null_q_hat(vars_reg) is not False):
        value2_reg = symbol_emptylist
        value1_reg = symbol_emptylist
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_47_d, procs_reg, vars_reg, k2_reg)
        procs_reg = cdr_hat(procs_reg)
        vars_reg = cdr_hat(vars_reg)
        pc = create_letrec_assignments_hat

def gensym_hat(prefix):
    global _stargensym_counter_star
    _stargensym_counter_star = (_stargensym_counter_star) + (1)
    return string_to_symbol(string_append("%%", prefix, "-", number_to_string(_stargensym_counter_star)))

def amacro_error():
    global exception_reg, pc
    info = get_source_info(adatum_reg)
    exception_reg = make_exception("MacroError", msg_reg, get_start_line(info), get_srcfile(info), get_start_char(info))
    pc = apply_handler2

def nest_let_star_bindings_hat():
    global bindings_reg, k_reg, pc, value_reg
    if ((null_q_hat(bindings_reg)) or (null_q_hat(cdr_hat(bindings_reg))) is not False):
        value_reg = append(List(symbol_let), append(List(bindings_reg), at_hat(bodies_reg)))
        pc = apply_cont
    else:
        k_reg = make_cont(b_cont_27_d, bindings_reg, k_reg)
        bindings_reg = cdr_hat(bindings_reg)
        pc = nest_let_star_bindings_hat

def case_clauses_to_cond_clauses_hat():
    global clauses_reg, k_reg, pc, value_reg
    if (null_q_hat(clauses_reg) is not False):
        value_reg = symbol_emptylist
        pc = apply_cont
    else:
        k_reg = make_cont(b_cont_29_d, clauses_reg, var_reg, k_reg)
        clauses_reg = cdr_hat(clauses_reg)
        pc = case_clauses_to_cond_clauses_hat

def record_case_clauses_to_cond_clauses_hat():
    global clauses_reg, k_reg, pc, value_reg
    if (null_q_hat(clauses_reg) is not False):
        value_reg = symbol_emptylist
        pc = apply_cont
    else:
        k_reg = make_cont(b_cont_30_d, clauses_reg, var_reg, k_reg)
        clauses_reg = cdr_hat(clauses_reg)
        pc = record_case_clauses_to_cond_clauses_hat

def make_dd_variant_constructors_hat():
    global k2_reg, k_reg, pc, value1_reg, value2_reg, variant_reg
    if (null_q_hat(variants_reg) is not False):
        value2_reg = symbol_emptylist
        value1_reg = symbol_emptylist
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_50_d, variants_reg, k2_reg)
        variant_reg = car_hat(variants_reg)
        pc = make_dd_variant_constructor_hat

def make_dd_variant_constructor_hat():
    global cdrs_reg, fields_reg, k_reg, name_reg, pc
    fields = cdr_hat(variant_reg)
    name = car_hat(variant_reg)
    k_reg = make_cont(b_cont_31_d, fields, name, k2_reg)
    cdrs_reg = symbol_args
    fields_reg = fields
    name_reg = name
    pc = verify_dd_constructor_fields_hat

def verify_dd_constructor_fields_hat():
    global cdrs_reg, fields_reg, k_reg, pc, value_reg
    if (null_q_hat(fields_reg) is not False):
        value_reg = append(List(symbol_cons), append(List(append(List(symbol_quote), List(name_reg))), List(symbol_args)))
        pc = apply_cont
    else:
        k_reg = make_cont(b_cont_32_d, cdrs_reg, fields_reg, name_reg, k_reg)
        cdrs_reg = append(List(symbol_cdr), List(cdrs_reg))
        fields_reg = cdr_hat(fields_reg)
        pc = verify_dd_constructor_fields_hat

def make_macro_env_hat():
    return make_initial_environment(List(symbol_lambda, symbol_λ, symbol_trace_lambda, symbol_and, symbol_or, symbol_cond, symbol_let, symbol_letrec, symbol_let_star, symbol_case, symbol_record_case, symbol_define_datatype, symbol_cases), List(lambda_transformer_hat, lambda_transformer_hat, trace_lambda_transformer_hat, and_transformer_hat, or_transformer_hat, cond_transformer_hat, let_transformer_hat, letrec_transformer_hat, let_star_transformer_hat, case_transformer_hat, record_case_transformer_hat, define_datatype_transformer_hat, cases_transformer_hat), List(string_append("(lambda ...) - lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(lambda ...) - lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(trace-lambda name ...) - trace-lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (trace-lambda name (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(and ...) - short-circuiting `and` macro\n", "\n", "Example:\n", "    In  [1]: (and)\n", "    Out [1]: #t\n", "    In  [2]: (and #t #f)\n", "    Out [2]: #f\n"), string_append("(or ...) - short-circuiting `or` macro", "\n", "Example:\n", "    In  [1]: (or)\n", "    Out [1]: #f\n", "    In  [2]: (or #t #f)\n", "    Out [2]: #t\n"), string_append("(cond (TEST RETURN)...) - conditional evaluation macro", "\n", "Example:\n", "    In  [1]: (cond ((= 1 2) 3)(else 4))\n", "    Out [1]: 4\n"), string_append("(let ((VAR VALUE)...)...) - local variable macro", "\n", "Example:\n", "    In  [1]: (let ((x 3)) x)\n", "    Out [1]: 3\n"), string_append("(letrec ((VAR VALUE)...)...) - recursive local variable macro", "\n", "Example:\n", "    In  [*]: (letrec ((loop (lambda () (loop)))) (loop))\n"), string_append("(let* ((VAR VALUE)...)...) - cascading local variable macro", "\n", "Example:\n", "    In  [1]: (let* ((a 1)(b a)(c b)) c)\n", "    Out [1]: 1\n"), string_append("(case THING (ITEM RETURN)...)) - case macro", "\n", "Example:\n", "    In  [1]: (case 1 (1 2)(3 4))\n", "    Out [1]: 2\n"), string_append("(record-case ) - record-case macro for define-datatype", "\n", "Example:\n", "    In  [1]: (record-case ddtype (subtype (part...) return)...)\n"), string_append("(define-datatype NAME NAME? (TYPE (PART TEST))...) - defines new datatypes and support functions (macro)", "\n", "Example:\n", "    In  [1]: (define-datatype e e?)\n", "    In  [1]: (e? 1)\n", "    Out [1]: #f\n"), string_append("(cases ...) - cases macro for a more flexible case", "\n", "Example:\n", "    In  [1]: (cases 1 ((1 2) 3))\n", "    Out [1]: 3\n")))

def make_pattern_macro_hat(clauses, aclauses):
    return List(symbol_pattern_macro, clauses, aclauses)

def pattern_macro_q(x):
    return (pair_q(x)) and (((x).car) is (symbol_pattern_macro))

def macro_clauses(macro):
    return (macro).cdr.car

def macro_aclauses(macro):
    return (macro).cdr.cdr.car

def define_syntax_clause_q(x):
    return (list_q(x)) and (numeric_equal(length(x), 2)) and (pattern_q((x).car)) and (pattern_q((x).cdr.car))

def define_syntax_clause_q_hat(x):
    return (list_q_hat(x)) and (numeric_equal(length_hat(x), 2)) and (apattern_q(car_hat(x))) and (apattern_q(cadr_hat(x)))

def apattern_q(x):
    return (aatom_q(x)) or ((apair_q(x)) and (apattern_q((x).cdr.car)) and (apattern_q((x).cdr.cdr.car)))

def list_of_define_syntax_clauses_q_hat(alist):
    return (null_q_hat(alist)) or ((define_syntax_clause_q_hat(car_hat(alist))) and (list_of_define_syntax_clauses_q_hat(cdr_hat(alist))))

def expand_once_hat():
    global aclauses_reg, clauses_reg, datum_reg, k_reg, macro_reg, pc
    macro_keyword = untag_atom_hat(car_hat(adatum_reg))
    macro = get_first_frame_value(macro_keyword, macro_env)
    if (pattern_macro_q(macro) is not False):
        k_reg = make_cont2(b_cont2_51_d, macro_keyword, k_reg)
        aclauses_reg = macro_aclauses(macro)
        clauses_reg = macro_clauses(macro)
        pc = process_macro_clauses_hat
    else:
        k_reg = make_cont(b_cont_35_d, adatum_reg, macro_keyword, fail_reg, k_reg)
        datum_reg = adatum_reg
        macro_reg = macro
        pc = apply_macro

def process_macro_clauses_hat():
    global k_reg, msg_reg, pc, x_reg
    if (((clauses_reg) is symbol_emptylist) is not False):
        msg_reg = "no matching clause found for"
        pc = aparse_error
    else:
        right_apattern = cadar_hat(aclauses_reg)
        left_apattern = caar_hat(aclauses_reg)
        right_pattern = (clauses_reg).car.cdr.car
        left_pattern = (clauses_reg).car.car
        k_reg = make_cont(b_cont_37_d, aclauses_reg, adatum_reg, clauses_reg, left_apattern, left_pattern, right_apattern, right_pattern, handler_reg, fail_reg, k_reg)
        x_reg = adatum_reg
        pc = unannotate_cps

def qq_expand_cps():
    global ax_reg, depth_reg, info_reg, k_reg, pc, value_reg, x_reg
    if (quasiquote_q_hat(ax_reg) is not False):
        k_reg = make_cont(b_cont_42_d, k_reg)
        depth_reg = (depth_reg) + (1)
        ax_reg = cdr_hat(ax_reg)
        pc = qq_expand_cps
    else:
        if ((unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg)) is not False):
            if (GreaterThan(depth_reg, 0) is not False):
                k_reg = make_cont(b_cont_43_d, ax_reg, k_reg)
                depth_reg = (depth_reg) - (1)
                ax_reg = cdr_hat(ax_reg)
                pc = qq_expand_cps
            else:
                if ((unquote_q_hat(ax_reg)) and (not(null_q_hat(cdr_hat(ax_reg)))) and (null_q_hat(cddr_hat(ax_reg))) is not False):
                    value_reg = cadr_hat(ax_reg)
                    pc = apply_cont
                else:
                    value_reg = append(List(symbol_quote), List(ax_reg))
                    pc = apply_cont
        else:
            if (vector_q_hat(ax_reg) is not False):
                k_reg = make_cont(b_cont_41_d, depth_reg, k_reg)
                info_reg = symbol_none
                x_reg = vector_to_list_hat(ax_reg)
                pc = annotate_cps
            else:
                if (not(pair_q_hat(ax_reg)) is not False):
                    value_reg = append(List(symbol_quote), List(ax_reg))
                    pc = apply_cont
                else:
                    if (null_q_hat(cdr_hat(ax_reg)) is not False):
                        ax_reg = car_hat(ax_reg)
                        pc = qq_expand_list_cps
                    else:
                        k_reg = make_cont(b_cont_39_d, ax_reg, depth_reg, k_reg)
                        ax_reg = car_hat(ax_reg)
                        pc = qq_expand_list_cps

def qq_expand_list_cps():
    global ax_reg, depth_reg, k_reg, pc, value_reg
    if (quasiquote_q_hat(ax_reg) is not False):
        k_reg = make_cont(b_cont_47_d, k_reg)
        depth_reg = (depth_reg) + (1)
        ax_reg = cdr_hat(ax_reg)
        pc = qq_expand_cps
    else:
        if ((unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg)) is not False):
            if (GreaterThan(depth_reg, 0) is not False):
                k_reg = make_cont(b_cont_48_d, ax_reg, k_reg)
                depth_reg = (depth_reg) - (1)
                ax_reg = cdr_hat(ax_reg)
                pc = qq_expand_cps
            else:
                if (unquote_q_hat(ax_reg) is not False):
                    value_reg = append(List(symbol_List), cdr_hat(ax_reg))
                    pc = apply_cont
                else:
                    if (null_q_hat(cddr_hat(ax_reg)) is not False):
                        value_reg = cadr_hat(ax_reg)
                        pc = apply_cont
                    else:
                        value_reg = append(List(symbol_append), cdr_hat(ax_reg))
                        pc = apply_cont
        else:
            if (vector_q_hat(ax_reg) is not False):
                k_reg = make_cont(b_cont_44_d, k_reg)
                pc = qq_expand_cps
            else:
                if (not(pair_q_hat(ax_reg)) is not False):
                    value_reg = append(List(symbol_quote), List(List(ax_reg)))
                    pc = apply_cont
                else:
                    if (null_q_hat(cdr_hat(ax_reg)) is not False):
                        k_reg = make_cont(b_cont_44_d, k_reg)
                        ax_reg = car_hat(ax_reg)
                        pc = qq_expand_list_cps
                    else:
                        k_reg = make_cont(b_cont_46_d, ax_reg, depth_reg, k_reg)
                        ax_reg = car_hat(ax_reg)
                        pc = qq_expand_list_cps

def aunparse(aexp):
    if (((aexp).car) is (symbol_lit_aexp) is not False):
        datum = ((aexp)).cdr.car
        if (((datum) is symbol_emptylist) is not False):
            return List(symbol_quote, symbol_emptylist)
        else:
            if (literal_q(datum) is not False):
                return datum
            else:
                if (vector_q(datum) is not False):
                    return datum
                else:
                    return append(List(symbol_quote), List(datum))
    else:
        if (((aexp).car) is (symbol_var_aexp) is not False):
            id = ((aexp)).cdr.car
            return id
        else:
            if (((aexp).car) is (symbol_lexical_address_aexp) is not False):
                id = ((((aexp)).cdr).cdr).cdr.car
                return id
            else:
                if (((aexp).car) is (symbol_if_aexp) is not False):
                    else_aexp = ((((aexp)).cdr).cdr).cdr.car
                    then_aexp = (((aexp)).cdr).cdr.car
                    test_aexp = ((aexp)).cdr.car
                    return append(List(symbol_if), append(List(aunparse(test_aexp)), append(List(aunparse(then_aexp)), List(aunparse(else_aexp)))))
                else:
                    if (((aexp).car) is (symbol_assign_aexp) is not False):
                        rhs_exp = (((aexp)).cdr).cdr.car
                        var = ((aexp)).cdr.car
                        return append(List(symbol_set_b), append(List(var), List(aunparse(rhs_exp))))
                    else:
                        if (((aexp).car) is (symbol_func_aexp) is not False):
                            exp = ((aexp)).cdr.car
                            return append(List(symbol_func), List(aunparse(exp)))
                        else:
                            if (((aexp).car) is (symbol_callback_aexp) is not False):
                                exp = ((aexp)).cdr.car
                                return append(List(symbol_callback), List(aunparse(exp)))
                            else:
                                if (((aexp).car) is (symbol_define_aexp) is not False):
                                    rhs_exp = ((((aexp)).cdr).cdr).cdr.car
                                    docstring = (((aexp)).cdr).cdr.car
                                    id = ((aexp)).cdr.car
                                    if (string_is__q(docstring, "") is not False):
                                        return append(List(symbol_define), append(List(id), List(aunparse(rhs_exp))))
                                    else:
                                        return append(List(symbol_define), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                else:
                                    if (((aexp).car) is (symbol_define_b_aexp) is not False):
                                        rhs_exp = ((((aexp)).cdr).cdr).cdr.car
                                        docstring = (((aexp)).cdr).cdr.car
                                        id = ((aexp)).cdr.car
                                        if (string_is__q(docstring, "") is not False):
                                            return append(List(symbol_define_b), append(List(id), List(aunparse(rhs_exp))))
                                        else:
                                            return append(List(symbol_define_b), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                    else:
                                        if (((aexp).car) is (symbol_define_syntax_aexp) is not False):
                                            clauses = (((aexp)).cdr).cdr.car
                                            name = ((aexp)).cdr.car
                                            return append(List(symbol_define_syntax), append(List(name), clauses))
                                        else:
                                            if (((aexp).car) is (symbol_begin_aexp) is not False):
                                                exps = ((aexp)).cdr.car
                                                return append(List(symbol_begin), Map(aunparse, exps))
                                            else:
                                                if (((aexp).car) is (symbol_lambda_aexp) is not False):
                                                    bodies = (((aexp)).cdr).cdr.car
                                                    formals = ((aexp)).cdr.car
                                                    return append(List(symbol_lambda), append(List(formals), Map(aunparse, bodies)))
                                                else:
                                                    if (((aexp).car) is (symbol_mu_lambda_aexp) is not False):
                                                        bodies = ((((aexp)).cdr).cdr).cdr.car
                                                        runt = (((aexp)).cdr).cdr.car
                                                        formals = ((aexp)).cdr.car
                                                        return append(List(symbol_lambda), append(List(append(formals, runt)), Map(aunparse, bodies)))
                                                    else:
                                                        if (((aexp).car) is (symbol_app_aexp) is not False):
                                                            operands = (((aexp)).cdr).cdr.car
                                                            operator = ((aexp)).cdr.car
                                                            return append(List(aunparse(operator)), Map(aunparse, operands))
                                                        else:
                                                            if (((aexp).car) is (symbol_try_catch_aexp) is not False):
                                                                catch_exps = ((((aexp)).cdr).cdr).cdr.car
                                                                catch_var = (((aexp)).cdr).cdr.car
                                                                body = ((aexp)).cdr.car
                                                                return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps))))))
                                                            else:
                                                                if (((aexp).car) is (symbol_try_finally_aexp) is not False):
                                                                    finally_exps = (((aexp)).cdr).cdr.car
                                                                    body = ((aexp)).cdr.car
                                                                    return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_finally), Map(aunparse, finally_exps)))))
                                                                else:
                                                                    if (((aexp).car) is (symbol_try_catch_finally_aexp) is not False):
                                                                        finally_exps = (((((aexp)).cdr).cdr).cdr).cdr.car
                                                                        catch_exps = ((((aexp)).cdr).cdr).cdr.car
                                                                        catch_var = (((aexp)).cdr).cdr.car
                                                                        body = ((aexp)).cdr.car
                                                                        return append(List(symbol_try), append(List(aunparse(body)), append(List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps)))), List(append(List(symbol_finally), Map(aunparse, finally_exps))))))
                                                                    else:
                                                                        if (((aexp).car) is (symbol_raise_aexp) is not False):
                                                                            exp = ((aexp)).cdr.car
                                                                            return append(List(symbol_raise), List(aunparse(exp)))
                                                                        else:
                                                                            if (((aexp).car) is (symbol_choose_aexp) is not False):
                                                                                exps = ((aexp)).cdr.car
                                                                                return append(List(symbol_choose), Map(aunparse, exps))
                                                                            else:
                                                                                raise Exception("symbol_aunparse: " + format("bad abstract syntax: ~s", *[aexp]))

def exception_q(x):
    return (pair_q(x)) and (((x).car) is (symbol_exception))

def make_test_callback(group_name, case_name, result, traceback, proc_exp, test_exp, result_val):
    return void_value

def path_join(path, filename):
    if (((path) is symbol_emptylist) is not False):
        return filename
    else:
        return path_join((path).cdr, string_append((path).car, "/", filename))

def use_lexical_address(*args):
    global _staruse_lexical_address_star
    args = List(*args)
    if (((args) is symbol_emptylist) is not False):
        return _staruse_lexical_address_star
    else:
        _staruse_lexical_address_star = true_q((args).car)
        return void_value

def handle_exception(exc):
    display(get_traceback_string(exc))
    return void_value

def get_traceback_string(exc):
    if ((list_q((exc).cdr.car)) and (numeric_equal(length((exc).cdr.car), 7)) is not False):
        retval = ""
        stack = ((((((((exc).cdr.car)).cdr).cdr).cdr).cdr).cdr).cdr.car
        src_col = (((((((exc).cdr.car)).cdr).cdr).cdr).cdr).cdr.car
        src_line = ((((((exc).cdr.car)).cdr).cdr).cdr).cdr.car
        src_file = (((((exc).cdr.car)).cdr).cdr).cdr.car
        message = ((((exc).cdr.car)).cdr).cdr.car
        error_type = (((exc).cdr.car)).cdr.car
        retval = string_append(retval, format("~%Traceback (most recent call last):~%"))
        while not(((stack) is symbol_emptylist)):
            retval = string_append(retval, format_exception_line((stack).car))
            stack = (stack).cdr
        if (not((src_file) is (symbol_none)) is not False):
            retval = string_append(retval, format("  File \"~a\", line ~a, col ~a~%", src_file, src_line, src_col))
        return string_append(retval, format("~a: ~a~%", error_type, message))
    else:
        retval = format("~%Traceback (most recent call last):~%")
        return string_append(retval, format("Raised Exception: ~a~%", (exc).cdr.car))

def get_exception_values(exc):
    if ((list_q((exc).cdr.car)) and (GreaterThan(length((exc).cdr.car), 2)) is not False):
        message = ((((exc).cdr.car)).cdr).cdr.car
        error_type = (((exc).cdr.car)).cdr.car
        return list_to_vector(List(error_type, message))
    else:
        return list_to_vector(List("UnhandledException", (exc).cdr.car))

def format_exception_line(line):
    if (list_q(line) is not False):
        column_number = (line).cdr.cdr.car
        line_number = (line).cdr.car
        filename = (line).car
        if (numeric_equal(length(line), 3) is not False):
            return format("  File \"~a\", line ~a, col ~a~%", filename, line_number, column_number)
        else:
            return format("  File \"~a\", line ~a, col ~a, in '~a'~%", filename, line_number, column_number, (line).cdr.cdr.cdr.car)
    else:
        return format("  Source \"~a\"~%", line)

def read_eval_print_loop_rm():
    input_ = read_multiline("==> ")
    result = execute_rm(input_, "stdin")
    while not(end_of_session_q(result)):
        if (exception_q(result) is not False):
            handle_exception(result)
        else:
            if (not(void_q(result)) is not False):
                if (_starneed_newline_star is not False):
                    newline()
                safe_print(result)
        input_ = read_multiline("==> ")
        result = execute_rm(input_, "stdin")
    return symbol_goodbye

def execute_string_top(input_, source):
    return execute_rm(input_, source)

def execute_string_rm(input_):
    return execute_rm(input_, "stdin")

def execute_file_rm(filename):
    return execute_rm(read_content(filename), filename)

def execute_rm(input_, src):
    global _startokens_left_star, fail_reg, handler_reg, input_reg, k_reg, load_stack, pc, src_reg
    load_stack = symbol_emptylist
    initialize_execute_b()
    k_reg = REP_k
    fail_reg = _starlast_fail_star
    handler_reg = REP_handler
    src_reg = src
    input_reg = input_
    pc = scan_input
    result = trampoline()
    if (exception_q(result) is not False):
        return result
    else:
        _startokens_left_star = result
        if (token_type_q(first(_startokens_left_star), symbol_end_marker) is not False):
            return void_value
        else:
            return execute_loop_rm(src)

def execute_loop_rm(src):
    execute_next_expression_rm(src)
    result = trampoline()
    if ((exception_q(result)) or (end_of_session_q(result)) or (token_type_q(first(_startokens_left_star), symbol_end_marker)) is not False):
        return result
    else:
        return execute_loop_rm(src)

def execute_next_expression_rm(src):
    global fail_reg, handler_reg, k_reg, pc, src_reg, tokens_reg
    k_reg = make_cont4(b_cont4_10_d)
    fail_reg = _starlast_fail_star
    handler_reg = REP_handler
    src_reg = src
    tokens_reg = _startokens_left_star
    pc = read_sexp

def try_parse(input_):
    global fail_reg, handler_reg, input_reg, k_reg, load_stack, pc, src_reg
    load_stack = symbol_emptylist
    k_reg = make_cont2(b_cont2_56_d)
    fail_reg = _starlast_fail_star
    handler_reg = try_parse_handler
    src_reg = "stdin"
    input_reg = input_
    pc = scan_input
    return trampoline()

def initialize_globals():
    global _starfilename_dict_star, _starfilename_vector_star, _starlast_fail_star, load_stack, macro_env, toplevel_env, unit_test_table
    _starfilename_dict_star = dict()
    _starfilename_vector_star = vlist()
    filename_cache("stdin")
    toplevel_env = make_toplevel_env()
    macro_env = make_macro_env_hat()
    unit_test_table = dict()
    load_stack = symbol_emptylist
    initialize_execute_b()
    _starlast_fail_star = REP_fail

def make_debugging_k(exp, k):
    return make_cont2(b_cont2_57_d, exp, k)

def handle_debug_info(exp, result):
    printf("~s => ~a~%", aunparse(exp), make_safe(result))

def get_use_stack_trace():
    return _staruse_stack_trace_star

def set_use_stack_trace_b(value):
    global _staruse_stack_trace_star
    _staruse_stack_trace_star = true_q(value)

def get_use_jit():
    return _staruse_jit_star

def set_use_jit_b(value):
    global _staruse_jit_star
    _staruse_jit_star = true_q(value)

def initialize_stack_trace_b():
    set_car_b(_starstack_trace_star, symbol_emptylist)

def initialize_execute_b():
    global _closure_depth, _trace_pause
    _closure_depth = 0
    _trace_pause = False
    initialize_stack_trace_b()

def push_stack_trace_b(exp):
    set_car_b(_starstack_trace_star, cons(exp, (_starstack_trace_star).car))

def pop_stack_trace_b(exp):
    if (not((((_starstack_trace_star).car) is symbol_emptylist)) is not False):
        set_car_b(_starstack_trace_star, ((_starstack_trace_star).car).cdr)

def m():
    global depth_reg, dk_reg, env_reg, exp_reg, exps_reg, frames_reg, gk_reg, handler_reg, info_reg, k_reg, msg_reg, offset_reg, pc, right_reg, sk_reg, start_time_reg, tests_reg, value1_reg, value2_reg, var_info_reg, var_reg, wrong_reg
    if (_startracing_on_q_star is not False):
        highlight_expression(exp_reg)
    k = (make_debugging_k(exp_reg, k_reg) if _startracing_on_q_star else k_reg)
    if (((exp_reg).car) is (symbol_lit_aexp) is not False):
        datum = ((exp_reg)).cdr.car
        value2_reg = fail_reg
        value1_reg = datum
        k_reg = k
        pc = apply_cont2
    else:
        if (((exp_reg).car) is (symbol_var_aexp) is not False):
            info = (((exp_reg)).cdr).cdr.car
            id = ((exp_reg)).cdr.car
            k_reg = k
            var_info_reg = info
            var_reg = id
            pc = lookup_value
        else:
            if (((exp_reg).car) is (symbol_lexical_address_aexp) is not False):
                offset = (((exp_reg)).cdr).cdr.car
                depth = ((exp_reg)).cdr.car
                k_reg = k
                frames_reg = frames(env_reg)
                offset_reg = offset
                depth_reg = depth
                pc = lookup_value_by_lexical_address
            else:
                if (((exp_reg).car) is (symbol_func_aexp) is not False):
                    exp = ((exp_reg)).cdr.car
                    k_reg = make_cont2(b_cont2_79_d, k)
                    exp_reg = exp
                    pc = m
                else:
                    if (((exp_reg).car) is (symbol_callback_aexp) is not False):
                        exp = ((exp_reg)).cdr.car
                        k_reg = make_cont2(b_cont2_77_d, k)
                        exp_reg = exp
                        pc = m
                    else:
                        if (((exp_reg).car) is (symbol_if_aexp) is not False):
                            else_exp = ((((exp_reg)).cdr).cdr).cdr.car
                            then_exp = (((exp_reg)).cdr).cdr.car
                            test_exp = ((exp_reg)).cdr.car
                            k_reg = make_cont2(b_cont2_78_d, else_exp, then_exp, env_reg, handler_reg, k)
                            exp_reg = test_exp
                            pc = m
                        else:
                            if (((exp_reg).car) is (symbol_help_aexp) is not False):
                                var_info = (((exp_reg)).cdr).cdr.car
                                var = ((exp_reg)).cdr.car
                                sk_reg = make_cont2(b_cont2_74_d, k)
                                dk_reg = make_cont3(b_cont3_5_d, k)
                                gk_reg = make_cont2(b_cont2_75_d, k)
                                var_info_reg = var_info
                                var_reg = var
                                pc = lookup_variable
                            else:
                                if (((exp_reg).car) is (symbol_association_aexp) is not False):
                                    exp = (((exp_reg)).cdr).cdr.car
                                    var = ((exp_reg)).cdr.car
                                    k_reg = make_cont2(b_cont2_76_d, var, k)
                                    exp_reg = exp
                                    pc = m
                                else:
                                    if (((exp_reg).car) is (symbol_assign_aexp) is not False):
                                        var_info = ((((exp_reg)).cdr).cdr).cdr.car
                                        rhs_exp = (((exp_reg)).cdr).cdr.car
                                        var = ((exp_reg)).cdr.car
                                        k_reg = make_cont2(b_cont2_71_d, var, var_info, env_reg, handler_reg, k)
                                        exp_reg = rhs_exp
                                        pc = m
                                    else:
                                        if (((exp_reg).car) is (symbol_define_aexp) is not False):
                                            rhs_exp = ((((exp_reg)).cdr).cdr).cdr.car
                                            docstring = (((exp_reg)).cdr).cdr.car
                                            var = ((exp_reg)).cdr.car
                                            k_reg = make_cont2(b_cont2_73_d, docstring, var, env_reg, handler_reg, k)
                                            exp_reg = rhs_exp
                                            pc = m
                                        else:
                                            if (((exp_reg).car) is (symbol_define_b_aexp) is not False):
                                                rhs_exp = ((((exp_reg)).cdr).cdr).cdr.car
                                                docstring = (((exp_reg)).cdr).cdr.car
                                                var = ((exp_reg)).cdr.car
                                                k_reg = make_cont2(b_cont2_67_d, docstring, var, k)
                                                exp_reg = rhs_exp
                                                pc = m
                                            else:
                                                if (((exp_reg).car) is (symbol_define_syntax_aexp) is not False):
                                                    aclauses = ((((exp_reg)).cdr).cdr).cdr.car
                                                    clauses = (((exp_reg)).cdr).cdr.car
                                                    name = ((exp_reg)).cdr.car
                                                    k_reg = make_cont2(b_cont2_68_d, aclauses, clauses, k)
                                                    env_reg = macro_env
                                                    var_reg = name
                                                    pc = lookup_binding_in_first_frame
                                                else:
                                                    if (((exp_reg).car) is (symbol_define_syntax_transformer_aexp) is not False):
                                                        info = ((((exp_reg)).cdr).cdr).cdr.car
                                                        rhs_exp = (((exp_reg)).cdr).cdr.car
                                                        name = ((exp_reg)).cdr.car
                                                        k_reg = make_cont2(b_cont2_66_d, name, env_reg, info, handler_reg, k)
                                                        exp_reg = rhs_exp
                                                        pc = m
                                                    else:
                                                        if (((exp_reg).car) is (symbol_define_tests_aexp) is not False):
                                                            info = ((((exp_reg)).cdr).cdr).cdr.car
                                                            aclauses = (((exp_reg)).cdr).cdr.car
                                                            name = ((exp_reg)).cdr.car
                                                            if (hasitem_native(unit_test_table, name) is not False):
                                                                info_reg = info
                                                                msg_reg = format("duplicate unit test group name '~a'; did you forget to (clear-unit-tests)?", name)
                                                                pc = runtime_error
                                                            else:
                                                                setitem_native(unit_test_table, name, List(aclauses, env_reg))
                                                                value2_reg = fail_reg
                                                                value1_reg = void_value
                                                                k_reg = k
                                                                pc = apply_cont2
                                                        else:
                                                            if (((exp_reg).car) is (symbol_run_tests_aexp) is not False):
                                                                tests = ((exp_reg)).cdr.car
                                                                if (((tests) is symbol_emptylist) is not False):
                                                                    k_reg = k
                                                                    wrong_reg = 0
                                                                    right_reg = 0
                                                                    start_time_reg = get_current_time()
                                                                    tests_reg = Map(List, dict_to_keys(unit_test_table))
                                                                    pc = run_unit_tests
                                                                else:
                                                                    k_reg = k
                                                                    wrong_reg = 0
                                                                    right_reg = 0
                                                                    start_time_reg = get_current_time()
                                                                    tests_reg = tests
                                                                    pc = run_unit_tests
                                                            else:
                                                                if (((exp_reg).car) is (symbol_begin_aexp) is not False):
                                                                    exps = ((exp_reg)).cdr.car
                                                                    k_reg = k
                                                                    exps_reg = exps
                                                                    pc = eval_sequence
                                                                else:
                                                                    if (((exp_reg).car) is (symbol_lambda_aexp) is not False):
                                                                        bodies = (((exp_reg)).cdr).cdr.car
                                                                        formals = ((exp_reg)).cdr.car
                                                                        value2_reg = fail_reg
                                                                        value1_reg = closure(formals, bodies, env_reg)
                                                                        k_reg = k
                                                                        pc = apply_cont2
                                                                    else:
                                                                        if (((exp_reg).car) is (symbol_mu_lambda_aexp) is not False):
                                                                            bodies = ((((exp_reg)).cdr).cdr).cdr.car
                                                                            runt = (((exp_reg)).cdr).cdr.car
                                                                            formals = ((exp_reg)).cdr.car
                                                                            value2_reg = fail_reg
                                                                            value1_reg = mu_closure(formals, get_symbol(runt), bodies, env_reg)
                                                                            k_reg = k
                                                                            pc = apply_cont2
                                                                        else:
                                                                            if (((exp_reg).car) is (symbol_trace_lambda_aexp) is not False):
                                                                                bodies = ((((exp_reg)).cdr).cdr).cdr.car
                                                                                formals = (((exp_reg)).cdr).cdr.car
                                                                                name = ((exp_reg)).cdr.car
                                                                                value2_reg = fail_reg
                                                                                value1_reg = trace_closure(name, formals, bodies, env_reg)
                                                                                k_reg = k
                                                                                pc = apply_cont2
                                                                            else:
                                                                                if (((exp_reg).car) is (symbol_mu_trace_lambda_aexp) is not False):
                                                                                    bodies = (((((exp_reg)).cdr).cdr).cdr).cdr.car
                                                                                    runt = ((((exp_reg)).cdr).cdr).cdr.car
                                                                                    formals = (((exp_reg)).cdr).cdr.car
                                                                                    name = ((exp_reg)).cdr.car
                                                                                    value2_reg = fail_reg
                                                                                    value1_reg = mu_trace_closure(name, formals, get_symbol(runt), bodies, env_reg)
                                                                                    k_reg = k
                                                                                    pc = apply_cont2
                                                                                else:
                                                                                    if (((exp_reg).car) is (symbol_try_catch_aexp) is not False):
                                                                                        cexps = ((((exp_reg)).cdr).cdr).cdr.car
                                                                                        cvar = (((exp_reg)).cdr).cdr.car
                                                                                        body = ((exp_reg)).cdr.car
                                                                                        new_handler = try_catch_handler(cvar, cexps, env_reg, handler_reg, k)
                                                                                        k_reg = k
                                                                                        handler_reg = new_handler
                                                                                        exp_reg = body
                                                                                        pc = m
                                                                                    else:
                                                                                        if (((exp_reg).car) is (symbol_try_finally_aexp) is not False):
                                                                                            fexps = (((exp_reg)).cdr).cdr.car
                                                                                            body = ((exp_reg)).cdr.car
                                                                                            new_handler = try_finally_handler(fexps, env_reg, handler_reg)
                                                                                            k_reg = make_cont2(b_cont2_62_d, fexps, env_reg, handler_reg, k)
                                                                                            handler_reg = new_handler
                                                                                            exp_reg = body
                                                                                            pc = m
                                                                                        else:
                                                                                            if (((exp_reg).car) is (symbol_try_catch_finally_aexp) is not False):
                                                                                                fexps = (((((exp_reg)).cdr).cdr).cdr).cdr.car
                                                                                                cexps = ((((exp_reg)).cdr).cdr).cdr.car
                                                                                                cvar = (((exp_reg)).cdr).cdr.car
                                                                                                body = ((exp_reg)).cdr.car
                                                                                                new_handler = try_catch_finally_handler(cvar, cexps, fexps, env_reg, handler_reg, k)
                                                                                                k_reg = make_cont2(b_cont2_62_d, fexps, env_reg, handler_reg, k)
                                                                                                handler_reg = new_handler
                                                                                                exp_reg = body
                                                                                                pc = m
                                                                                            else:
                                                                                                if (((exp_reg).car) is (symbol_raise_aexp) is not False):
                                                                                                    info = (((exp_reg)).cdr).cdr.car
                                                                                                    exp = ((exp_reg)).cdr.car
                                                                                                    k_reg = make_cont2(b_cont2_63_d, info, handler_reg)
                                                                                                    exp_reg = exp
                                                                                                    pc = m
                                                                                                else:
                                                                                                    if (((exp_reg).car) is (symbol_choose_aexp) is not False):
                                                                                                        exps = ((exp_reg)).cdr.car
                                                                                                        k_reg = k
                                                                                                        exps_reg = exps
                                                                                                        pc = eval_choices
                                                                                                    else:
                                                                                                        if (((exp_reg).car) is (symbol_app_aexp) is not False):
                                                                                                            info = ((((exp_reg)).cdr).cdr).cdr.car
                                                                                                            operands = (((exp_reg)).cdr).cdr.car
                                                                                                            operator = ((exp_reg)).cdr.car
                                                                                                            k_reg = make_cont2(b_cont2_60_d, exp_reg, operator, env_reg, info, handler_reg, k)
                                                                                                            exps_reg = operands
                                                                                                            pc = m_star
                                                                                                        else:
                                                                                                            info_reg = info
                                                                                                            msg_reg = format("unknown abstract syntax type: ~a", (exp_reg).car)
                                                                                                            pc = runtime_error

def run_unit_tests():
    global k_reg, pc, test_reg, value1_reg, value2_reg
    if (((tests_reg) is symbol_emptylist) is not False):
        total = Apply(plus, Map(length, Map(car, dict_to_values(unit_test_table))))
        printf("=================\n")
        printf("Testing completed!\n")
        printf("  Time : ~a seconds~%", format_float(4, 2, (get_current_time()) - (start_time_reg)))
        printf("  Total tests defined: ~s ~%", total)
        printf("  Total tests tested : ~s ~%", (right_reg) + (wrong_reg))
        printf("                Right: ~s ~%", right_reg)
        printf("                Wrong: ~s ~%", wrong_reg)
        value2_reg = fail_reg
        value1_reg = void_value
        pc = apply_cont2
    else:
        k_reg = make_cont2(b_cont2_80_d, start_time_reg, tests_reg, handler_reg, k_reg)
        test_reg = (tests_reg).car
        pc = run_unit_test

def run_unit_test():
    global assertions_reg, env_reg, info_reg, k_reg, msg_reg, nums_reg, pc, test_name_reg, verbose_reg
    test_name = (test_reg).car
    nums = (test_reg).cdr
    entry = getitem_native(unit_test_table, test_name)
    if ((entry) is (False) is not False):
        info_reg = symbol_none
        msg_reg = format("test group '~a' not found", test_name)
        pc = runtime_error
    else:
        assertions = (entry).car
        env = (entry).cdr.car
        printf("Testing group '~a'...\n", test_name)
        if (((nums) is symbol_emptylist) is not False):
            env_reg = env
            verbose_reg = False
            assertions_reg = assertions
            test_name_reg = test_name
            pc = run_unit_test_cases
        else:
            k_reg = make_cont2(b_cont2_81_d, right_reg, test_name, wrong_reg, env, handler_reg, k_reg)
            assertions_reg = assertions
            nums_reg = nums
            test_name_reg = test_name
            pc = filter_assertions

def filter_assertions():
    global pc, value1_reg, value2_reg
    if (((nums_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        if (number_q((nums_reg).car) is not False):
            case_name = format("case ~a", (nums_reg).car)
        else:
            case_name = (nums_reg).car
        return lookup_assertions(test_name_reg, case_name, assertions_reg, symbol_emptylist, handler_reg, fail_reg, make_cont2(b_cont2_83_d, assertions_reg, nums_reg, test_name_reg, handler_reg, k_reg))

def lookup_assertions(test_name, case_name, assertions, accum, handler, fail, k):
    global fail_reg, handler_reg, info_reg, k_reg, msg_reg, pc, value1_reg, value2_reg
    if (((assertions) is symbol_emptylist) is not False):
        if (((accum) is symbol_emptylist) is not False):
            fail_reg = fail
            handler_reg = handler
            info_reg = symbol_none
            msg_reg = format("~a unit test '~a' not found", test_name, case_name)
            pc = runtime_error
        else:
            value2_reg = fail
            value1_reg = accum
            k_reg = k
            pc = apply_cont2
    else:
        assertion = (assertions).car
        app_aexp_args = (assertion).cdr.cdr.car
        if (numeric_equal(length(app_aexp_args), 4) is not False):
            lit_aexp_datum = ((app_aexp_args).cdr.cdr.cdr.car).cdr.car
            if ((string_q(lit_aexp_datum)) and (((string_startswith_q(case_name, "case ")) and (string_is__q(lit_aexp_datum, case_name))) or ((not(string_startswith_q(case_name, "case "))) and (string_startswith_q(lit_aexp_datum, case_name)))) is not False):
                return lookup_assertions(test_name, case_name, (assertions).cdr, cons(assertion, accum), handler, fail, k)
            else:
                return lookup_assertions(test_name, case_name, (assertions).cdr, accum, handler, fail, k)

def valid_exception_type_q(exception_type):
    return (string_q(exception_type)) and ((string_is__q(exception_type, "AssertionError")) or (string_is__q(exception_type, "Exception")) or (string_is__q(exception_type, "KeyboardInterrupt")) or (string_is__q(exception_type, "MacroError")) or (string_is__q(exception_type, "ParseError")) or (string_is__q(exception_type, "ReadError")) or (string_is__q(exception_type, "RunTimeError")) or (string_is__q(exception_type, "ScanError")) or (string_is__q(exception_type, "UnhandledException")))

def report_unit_test_diagnostic_fallback():
    global assertions_reg, pc, wrong_reg
    if (GreaterThan(string_length(msg_reg), 0) is not False):
        if ((where_reg) is (symbol_none) is not False):
            printf("  Error: ~a \"~a\"\n", test_name_reg, msg_reg)
        else:
            printf("  Error: ~a \"~a\" at ~a\n", test_name_reg, msg_reg, where_reg)
    else:
        if ((where_reg) is (symbol_none) is not False):
            printf("  Error: ~a\n", test_name_reg)
        else:
            printf("  Error: ~a at ~a\n", test_name_reg, where_reg)
    make_test_callback(test_name_reg, msg_reg, False, "", "", "", "")
    wrong_reg = (wrong_reg) + (1)
    assertions_reg = (assertions_reg).cdr
    pc = run_unit_test_cases

def run_unit_test_cases():
    global exp_reg, handler_reg, k_reg, pc, value1_reg, value2_reg
    if (((assertions_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = List(right_reg, wrong_reg)
        pc = apply_cont2
    else:
        test_case_handler = make_handler2(b_handler2_5_d, assertions_reg, right_reg, test_name_reg, verbose_reg, wrong_reg, env_reg, handler_reg, k_reg)
        initialize_stack_trace_b()
        k_reg = make_cont2(b_cont2_86_d, assertions_reg, right_reg, test_name_reg, verbose_reg, wrong_reg, env_reg, handler_reg, k_reg)
        handler_reg = test_case_handler
        exp_reg = (assertions_reg).car
        pc = m

def get_exception_info(exception):
    column = ((((((exception)).cdr).cdr).cdr).cdr).cdr.car
    line = (((((exception)).cdr).cdr).cdr).cdr.car
    source = ((((exception)).cdr).cdr).cdr.car
    if ((source) is (symbol_none) is not False):
        return symbol_none
    else:
        return format("line ~a, column ~a of ~a", line, column, source)

def make_exception(exception_type, message, source, line, column):
    return List(symbol_exception_object, exception_type, message, source, line, column, make_stack_trace())

def get_exception_message(exception):
    return (((exception)).cdr).cdr.car

def make_stack_trace():
    trace = (_starstack_trace_star).car
    return reverse(Map(format_stack_trace, trace))

def get_procedure_name(aexp):
    if (macro_derived_source_info_q(aexp) is not False):
        return rac(get_source_info(aexp))
    else:
        if (((aexp).car) is (symbol_app_aexp) is not False):
            operator = ((aexp)).cdr.car
            if (((operator).car) is (symbol_lexical_address_aexp) is not False):
                id = ((((operator)).cdr).cdr).cdr.car
                return id
            else:
                if (((operator).car) is (symbol_var_aexp) is not False):
                    id = ((operator)).cdr.car
                    return id
                else:
                    if (((operator).car) is (symbol_lambda_aexp) is not False):
                        formals = ((operator)).cdr.car
                        return append(List(symbol_lambda), append(List(formals), List(symbol_dotdotdot)))
                    else:
                        if (((operator).car) is (symbol_mu_lambda_aexp) is not False):
                            runt = (((operator)).cdr).cdr.car
                            formals = ((operator)).cdr.car
                            return append(List(symbol_lambda), append(List(append(formals, runt)), List(symbol_dotdotdot)))
                        else:
                            if (((operator).car) is (symbol_trace_lambda_aexp) is not False):
                                name = ((operator)).cdr.car
                                return name
                            else:
                                if (((operator).car) is (symbol_mu_trace_lambda_aexp) is not False):
                                    name = ((operator)).cdr.car
                                    return name
                                else:
                                    return symbol_application
        else:
            return symbol_unknown

def format_stack_trace(exp):
    info = rac(exp)
    if ((info) is (symbol_none) is not False):
        return symbol_macro_generated_exp
    else:
        return List(get_srcfile(info), get_start_line(info), get_start_char(info), get_procedure_name(exp))

def runtime_error():
    global exception_reg, pc
    if ((info_reg) is (symbol_none) is not False):
        exception_reg = make_exception("RunTimeError", msg_reg, symbol_none, symbol_none, symbol_none)
        pc = apply_handler2
    else:
        char_number = get_start_char(info_reg)
        line_number = get_start_line(info_reg)
        src = get_srcfile(info_reg)
        exception_reg = make_exception("RunTimeError", msg_reg, src, line_number, char_number)
        pc = apply_handler2

def assertion_error():
    global exception_reg, pc
    if ((info_reg) is (symbol_none) is not False):
        exception_reg = make_exception("AssertionError", msg_reg, symbol_none, symbol_none, symbol_none)
        pc = apply_handler2
    else:
        char_number = get_start_char(info_reg)
        line_number = get_start_line(info_reg)
        src = get_srcfile(info_reg)
        exception_reg = make_exception("AssertionError", msg_reg, src, line_number, char_number)
        pc = apply_handler2

def m_star():
    global exp_reg, k_reg, pc, value1_reg, value2_reg
    if (((exps_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        k_reg = make_cont2(b_cont2_87_d, exps_reg, env_reg, handler_reg, k_reg)
        exp_reg = (exps_reg).car
        pc = m

def eval_sequence():
    global exp_reg, k_reg, pc
    if ((((exps_reg).cdr) is symbol_emptylist) is not False):
        exp_reg = (exps_reg).car
        pc = m
    else:
        k_reg = make_cont2(b_cont2_88_d, exps_reg, env_reg, handler_reg, k_reg)
        exp_reg = (exps_reg).car
        pc = m

def try_catch_handler(cvar, cexps, env, handler, k):
    return make_handler2(b_handler2_6_d, cexps, cvar, env, handler, k)

def try_finally_handler(fexps, env, handler):
    return make_handler2(b_handler2_7_d, fexps, env, handler)

def try_catch_finally_handler(cvar, cexps, fexps, env, handler, k):
    return make_handler2(b_handler2_8_d, cexps, cvar, fexps, env, handler, k)

def eval_choices():
    global exp_reg, fail_reg, pc
    if (((exps_reg) is symbol_emptylist) is not False):
        pc = apply_fail
    else:
        new_fail = make_fail(b_fail_5_d, exps_reg, env_reg, handler_reg, fail_reg, k_reg)
        fail_reg = new_fail
        exp_reg = (exps_reg).car
        pc = m

def association(var, value):
    return List(var, symbol_colon, value)

def mu_closure(formals, runt, bodies, env):
    return make_proc(b_proc_2_d, bodies, formals, runt, env)

def make_trace_depth_string(level):
    if (numeric_equal(level, 0) is not False):
        return ""
    else:
        return string_append(" |", make_trace_depth_string((level) - (1)))

def trace_closure(name, formals, bodies, env):
    trace_depth = 0
    return make_proc(b_proc_3_d, bodies, name, trace_depth, formals, env)

def mu_trace_closure(name, formals, runt, bodies, env):
    trace_depth = 0
    return make_proc(b_proc_4_d, bodies, name, trace_depth, formals, runt, env)

def length_three_q(ls):
    return (not(((ls) is symbol_emptylist))) and (not((((ls).cdr) is symbol_emptylist))) and (not((((ls).cdr.cdr) is symbol_emptylist))) and ((((ls).cdr.cdr.cdr) is symbol_emptylist))

def length_exactly_q(n, ls):
    return (length_at_least_q(n, ls)) and (not(length_at_least_q((n) + (1), ls)))

def length_between_q(lo, hi, ls):
    return (length_at_least_q(lo, ls)) and (not(length_at_least_q((hi) + (1), ls)))

def all_char_q(ls):
    return (((ls) is symbol_emptylist)) or ((char_q((ls).car)) and (all_char_q((ls).cdr)))

def void_q(x):
    return (x) is (void_value)

def end_of_session_q(x):
    return (x) is (end_of_session)

def string_join():
    global items_reg, k2_reg, k_reg, pc, value1_reg, value2_reg
    if (((items_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = ""
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if ((((items_reg).cdr) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = format("~a", (items_reg).car)
            k_reg = k2_reg
            pc = apply_cont2
        else:
            k2_reg = make_cont2(b_cont2_91_d, items_reg, sep_reg, k2_reg)
            items_reg = (items_reg).cdr
            pc = string_join

def safe_print(arg):
    global _starneed_newline_star
    _starneed_newline_star = False
    pretty_print(make_safe(arg))

def exception_object_q(x):
    return (list_q(x)) and (numeric_equal(length(x), 7)) and (((x).car) is (symbol_exception_object)) and (valid_exception_type_q((x).cdr.car)) and (string_q((x).cdr.car))

def procedure_object_q(x):
    return (procedure_q(x)) or ((pair_q(x)) and (((x).car) is (symbol_procedure)))

def environment_object_q(x):
    return (pair_q(x)) and (((x).car) is (symbol_environment))

def ends_with_newline_q(s):
    len = string_length(s)
    return (GreaterThan(len, 0)) and (equal_q(substring(s, (len) - (1), len), "\n"))

def load_file():
    global input_reg, k_reg, load_stack, msg_reg, pc, src_reg, value1_reg, value2_reg
    if (member(filename_reg, load_stack) is not False):
        printf("skipping recursive load of ~a~%", filename_reg)
        value2_reg = fail_reg
        value1_reg = void_value
        pc = apply_cont2
    else:
        if (not(string_q(filename_reg)) is not False):
            msg_reg = format("filename '~a' is not a string", filename_reg)
            pc = runtime_error
        else:
            if (not(file_exists_q(filename_reg)) is not False):
                msg_reg = format("attempted to load nonexistent file '~a'", filename_reg)
                pc = runtime_error
            else:
                load_stack = cons(filename_reg, load_stack)
                k_reg = make_cont2(b_cont2_97_d, filename_reg, env2_reg, handler_reg, k_reg)
                src_reg = filename_reg
                input_reg = read_content(filename_reg)
                pc = scan_input

def read_and_eval_asexps():
    global k_reg, pc, value1_reg, value2_reg
    if (token_type_q(first(tokens_reg), symbol_end_marker) is not False):
        value2_reg = fail_reg
        value1_reg = void_value
        pc = apply_cont2
    else:
        k_reg = make_cont4(b_cont4_13_d, src_reg, env2_reg, handler_reg, k_reg)
        pc = read_sexp

def load_files():
    global filename_reg, k_reg, paths_reg, pc, value1_reg, value2_reg
    if (((filenames_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = void_value
        pc = apply_cont2
    else:
        k_reg = make_cont2(b_cont2_100_d, filenames_reg, env2_reg, info_reg, handler_reg, k_reg)
        filename_reg = (filenames_reg).car
        paths_reg = SCHEMEPATH
        pc = find_file_and_load

def find_file_and_load():
    global filename_reg, msg_reg, paths_reg, pc
    if (string_startswith_q(filename_reg, "/") is not False):
        pc = load_file
    else:
        if (((paths_reg) is symbol_emptylist) is not False):
            msg_reg = format("attempted to load nonexistent file '~a'", filename_reg)
            pc = runtime_error
        else:
            path = path_join(List((paths_reg).car), filename_reg)
            if (file_exists_q(path) is not False):
                filename_reg = path
                pc = load_file
            else:
                paths_reg = (paths_reg).cdr
                pc = find_file_and_load

def length_loop():
    global k_reg, msg_reg, pc, sum_reg, value1_reg, value2_reg, x_reg
    if (((x_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = sum_reg
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (not(pair_q(x_reg)) is not False):
            msg_reg = format("length called on improper list ~s", ls_reg)
            pc = runtime_error
        else:
            sum_reg = (sum_reg) + (1)
            x_reg = (x_reg).cdr
            pc = length_loop

def make_set():
    global k2_reg, k_reg, lst_reg, pc, value1_reg, value2_reg
    if (((lst_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = lst_reg
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_102_d, lst_reg, k2_reg)
        lst_reg = (lst_reg).cdr
        pc = make_set

def equal_objects_q():
    global i_reg, k_reg, pc, v1_reg, v2_reg, value_reg, x_reg, y_reg
    if (((((x_reg) is symbol_emptylist)) and (((y_reg) is symbol_emptylist))) or ((boolean_q(x_reg)) and (boolean_q(y_reg)) and (((x_reg) and (y_reg)) or ((not(x_reg)) and (not(y_reg))))) or ((symbol_q(x_reg)) and (symbol_q(y_reg)) and ((x_reg) is (y_reg))) or ((number_q(x_reg)) and (number_q(y_reg)) and (numeric_equal(x_reg, y_reg))) or ((char_q(x_reg)) and (char_q(y_reg)) and (char_is__q(x_reg, y_reg))) or (((x_reg) is (void_value)) and ((y_reg) is (void_value))) or ((string_q(x_reg)) and (string_q(y_reg)) and (string_is__q(x_reg, y_reg))) is not False):
        value_reg = True
        pc = apply_cont
    else:
        if ((pair_q(x_reg)) and (pair_q(y_reg)) is not False):
            k_reg = make_cont(b_cont_54_d, x_reg, y_reg, k_reg)
            y_reg = (y_reg).car
            x_reg = (x_reg).car
            pc = equal_objects_q
        else:
            if ((vector_q(x_reg)) and (vector_q(y_reg)) and (numeric_equal(vector_length(x_reg), vector_length(y_reg))) is not False):
                i_reg = (vector_length(x_reg)) - (1)
                v2_reg = y_reg
                v1_reg = x_reg
                pc = equal_vectors_q
            else:
                if ((box_q(x_reg)) and (box_q(y_reg)) is not False):
                    y_reg = unbox(y_reg)
                    x_reg = unbox(x_reg)
                    pc = equal_objects_q
                else:
                    value_reg = False
                    pc = apply_cont

def equal_vectors_q():
    global k_reg, pc, value_reg, x_reg, y_reg
    if (LessThan(i_reg, 0) is not False):
        value_reg = True
        pc = apply_cont
    else:
        k_reg = make_cont(b_cont_55_d, i_reg, v1_reg, v2_reg, k_reg)
        y_reg = vector_ref(v2_reg, i_reg)
        x_reg = vector_ref(v1_reg, i_reg)
        pc = equal_objects_q

def member_loop():
    global k_reg, msg_reg, pc, value1_reg, value2_reg, y_reg
    if (((y_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = False
        pc = apply_cont2
    else:
        if (not(pair_q(y_reg)) is not False):
            msg_reg = format("member called on improper list ~s", ls_reg)
            pc = runtime_error
        else:
            k_reg = make_cont(b_cont_56_d, ls_reg, x_reg, y_reg, info_reg, handler_reg, fail_reg, k_reg)
            y_reg = (y_reg).car
            pc = equal_objects_q

def append2():
    global k2_reg, k_reg, ls1_reg, pc, value1_reg, value2_reg
    if (((ls1_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = ls2_reg
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_104_d, ls1_reg, k2_reg)
        ls1_reg = (ls1_reg).cdr
        pc = append2

def append_all():
    global k2_reg, k_reg, lists_reg, msg_reg, pc, value1_reg, value2_reg
    if (((lists_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if ((((lists_reg).cdr) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = (lists_reg).car
            k_reg = k2_reg
            pc = apply_cont2
        else:
            if (not(list_q((lists_reg).car)) is not False):
                msg_reg = format("append called on incorrect list structure ~s", (lists_reg).car)
                pc = runtime_error
            else:
                k2_reg = make_cont2(b_cont2_105_d, lists_reg, k2_reg)
                lists_reg = (lists_reg).cdr
                pc = append_all

def get_completions(args, env):
    if (((args) is symbol_emptylist) is not False):
        return append(get_variables_from_frames(frames(env)), get_variables_from_frames(frames(macro_env)))
    else:
        if (environment_q((args).car) is not False):
            return append(get_variables_from_frames(frames((args).car)), get_variables_from_frames(frames(macro_env)))
        else:
            return get_external_members((args).car)

def directory(args, env):
    if ((((args) is symbol_emptylist)) or (environment_q((args).car)) is not False):
        return sort(symbolLessThan_q, (get_variables_from_frames(frames(env)) if ((args) is symbol_emptylist) else get_variables_from_frames(frames((args).car))))
    else:
        return get_external_members((args).car)

def get_variables_from_frame(frame):
    return (frame).cdr.car

def get_variables_from_frames(frames):
    return flatten(Map(get_variables_from_frame, frames))

def symbolLessThan_q(a, b):
    b_string = symbol_to_string(b)
    a_string = symbol_to_string(a)
    return stringLessThan_q(a_string, b_string)

def flatten(lists):
    if (((lists) is symbol_emptylist) is not False):
        return symbol_emptylist
    else:
        if (list_q((lists).car) is not False):
            return append(flatten((lists).car), flatten((lists).cdr))
        else:
            return cons((lists).car, flatten((lists).cdr))

def map_primitive():
    global generator_reg, list1_reg, list2_reg, lists_reg, pc
    if (iterator_q((args_reg).car) is not False):
        generator_reg = (args_reg).car
        pc = iterate_collect
    else:
        list_args = listify(args_reg)
        len = length(args_reg)
        if (numeric_equal(len, 1) is not False):
            list1_reg = (list_args).car
            pc = map1
        else:
            if (numeric_equal(len, 2) is not False):
                list2_reg = (list_args).cdr.car
                list1_reg = (list_args).car
                pc = map2
            else:
                lists_reg = list_args
                pc = mapN

def listify(arg_list):
    if (((arg_list) is symbol_emptylist) is not False):
        return symbol_emptylist
    else:
        if (list_q((arg_list).car) is not False):
            return cons((arg_list).car, listify((arg_list).cdr))
        else:
            if (vector_q((arg_list).car) is not False):
                return cons(vector_to_list((arg_list).car), listify((arg_list).cdr))
            else:
                if (string_q((arg_list).car) is not False):
                    return cons(string_to_list((arg_list).car), listify((arg_list).cdr))
                else:
                    if (iter_q((arg_list).car) is not False):
                        return cons(vector_to_list(list_native((arg_list).car)), listify((arg_list).cdr))
                    else:
                        raise Exception("symbol_Map: " + format("cannot use object type '~a' in map", *[get_type((arg_list).car)]))

def iterate():
    global iterator_reg, pc
    iterator = get_iterator(generator_reg)
    iterator_reg = iterator
    pc = iterate_continue

def iterate_continue():
    global args_reg, env2_reg, info_reg, k2_reg, pc, value1_reg, value2_reg
    item = next_item(iterator_reg)
    if (((item) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_106_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        info_reg = symbol_none
        env2_reg = env_reg
        args_reg = List(item)
        pc = apply_proc

def iterate_collect():
    global iterator_reg, pc
    iterator = get_iterator(generator_reg)
    iterator_reg = iterator
    pc = iterate_collect_continue

def iterate_collect_continue():
    global args_reg, env2_reg, info_reg, k2_reg, pc, value1_reg, value2_reg
    item = next_item(iterator_reg)
    if (((item) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_107_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        info_reg = symbol_none
        env2_reg = env_reg
        args_reg = List(item)
        pc = apply_proc

def map1():
    global args_reg, env2_reg, info_reg, k2_reg, k_reg, list1_reg, pc, value1_reg, value2_reg
    if (((list1_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        if (dlr_proc_q(proc_reg) is not False):
            k_reg = make_cont2(b_cont2_109_d, list1_reg, proc_reg, k_reg)
            list1_reg = (list1_reg).cdr
            pc = map1
        else:
            k2_reg = make_cont2(b_cont2_108_d, list1_reg, proc_reg, env_reg, handler_reg, k_reg)
            info_reg = symbol_none
            env2_reg = env_reg
            args_reg = List((list1_reg).car)
            pc = apply_proc

def map2():
    global args_reg, env2_reg, info_reg, k2_reg, k_reg, list1_reg, list2_reg, pc, value1_reg, value2_reg
    if (((list1_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        if (dlr_proc_q(proc_reg) is not False):
            k_reg = make_cont2(b_cont2_111_d, list1_reg, list2_reg, proc_reg, k_reg)
            list2_reg = (list2_reg).cdr
            list1_reg = (list1_reg).cdr
            pc = map2
        else:
            k2_reg = make_cont2(b_cont2_110_d, list1_reg, list2_reg, proc_reg, env_reg, handler_reg, k_reg)
            info_reg = symbol_none
            env2_reg = env_reg
            args_reg = List((list1_reg).car, (list2_reg).car)
            pc = apply_proc

def mapN():
    global args_reg, env2_reg, info_reg, k2_reg, k_reg, lists_reg, pc, value1_reg, value2_reg
    if ((((lists_reg).car) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        pc = apply_cont2
    else:
        if (dlr_proc_q(proc_reg) is not False):
            k_reg = make_cont2(b_cont2_113_d, lists_reg, proc_reg, k_reg)
            lists_reg = Map(cdr, lists_reg)
            pc = mapN
        else:
            k2_reg = make_cont2(b_cont2_112_d, lists_reg, proc_reg, env_reg, handler_reg, k_reg)
            info_reg = symbol_none
            env2_reg = env_reg
            args_reg = Map(car, lists_reg)
            pc = apply_proc

def for_each_primitive():
    global args_reg, env2_reg, generator_reg, info_reg, k2_reg, lists_reg, pc, value1_reg, value2_reg
    if (iterator_q((lists_reg).car) is not False):
        generator_reg = (lists_reg).car
        pc = iterate
    else:
        arg_list = listify(lists_reg)
        if ((((arg_list).car) is symbol_emptylist) is not False):
            value2_reg = fail_reg
            value1_reg = void_value
            pc = apply_cont2
        else:
            if (dlr_proc_q(proc_reg) is not False):
                dlr_apply(proc_reg, Map(car, arg_list))
                lists_reg = Map(cdr, arg_list)
                pc = for_each_primitive
            else:
                k2_reg = make_cont2(b_cont2_114_d, arg_list, proc_reg, env_reg, handler_reg, k_reg)
                info_reg = symbol_none
                env2_reg = env_reg
                args_reg = Map(car, arg_list)
                pc = apply_proc

def make_dict_tuples():
    global associations_reg, k2_reg, k_reg, pc, value1_reg, value2_reg
    if (((associations_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = symbol_emptylist
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_116_d, associations_reg, k2_reg)
        associations_reg = (associations_reg).cdr
        pc = make_dict_tuples

def to_string(obj):
    if (symbol_q(obj) is not False):
        return symbol_to_string(obj)
    else:
        return obj

def apply_native(proc, args):
    if (dlr_proc_q(proc) is not False):
        return dlr_apply(proc, args)
    else:
        return Apply(proc, args)

def insert_element():
    global args_reg, k2_reg, k_reg, pc, value1_reg, value2_reg
    if (((elements_reg) is symbol_emptylist) is not False):
        value2_reg = fail_reg
        value1_reg = List(x_reg)
        k_reg = k2_reg
        pc = apply_cont2
    else:
        k2_reg = make_cont2(b_cont2_119_d, elements_reg, proc_reg, x_reg, env2_reg, info_reg, handler_reg, k2_reg)
        args_reg = List(x_reg, (elements_reg).car)
        pc = apply_proc

def make_toplevel_env():
    primitives = List(List(symbol_p, modulo_prim, "(% arg0 arg1): modulo procedure for two arguments (aliases mod and modulo)"), List(symbol_multiply, times_prim, "(* ...): multiplication procedure; multiplies all arguments"), List(symbol_plus, plus_prim, "(+ ...): addition procedure; adds all arguments"), List(symbol_minus, minus_prim, "(- ...): subtraction procedure; subtracts all arguments"), List(symbol_divide, divide_prim, "(/ ...): division procedure; divides all arguments"), List(symbol___, quotient_prim, "(// arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases div and quotient)"), List(symbol_LessThan, lt_prim, "(< arg0 arg1): less-than procedure for two arguments"), List(symbol_LessThanEqual, lt_or_eq_prim, "(<= arg0 arg1): less-than or equal procedure for two arguments"), List(symbol_numeric_equal, equal_sign_prim, "(= arg0 arg1): numeric equality procedure for two arguments"), List(symbol_GreaterThan, gt_prim, "(> arg0 arg1): greater-than procedure for two arguments"), List(symbol_GreaterThanEqual, gt_or_eq_prim, "(>= arg0 arg1): greater-than or equal procedure for two arguments"), List(symbol_SCHEMEPATH, SCHEMEPATH, "List of search directories used with (load NAME)"), List(symbol_abort, abort_prim, "(abort) : aborts processing and returns to top level"), List(symbol_abs, abs_prim, "(abs value): absolute value procedure"), List(symbol_append, append_prim, "(append ...): append lists together into a single list"), List(symbol_Apply, apply_prim, "(apply PROCEDURE '(args...)): apply the PROCEDURE to the args"), List(symbol_assert, assert_prim, "(assert OPERATOR EXPRESSION ANSWER): assert that (OPERATOR EXPRESSION ANSWER) is #t"), List(symbol_assq, assq_prim, "(assq ...): "), List(symbol_assv, assv_prim, "(assv KEY ((ITEM VALUE) ...)): look for KEY in ITEMs; return matching (ITEM VALUE) or #f if not found"), List(symbol_atom_q, atom_q_prim, "(atom? ITEM): return #t if ITEM is a atom, #f otherwise"), List(symbol_boolean_q, boolean_q_prim, "(boolean? ITEM): return #t if ITEM is a boolean value"), List(symbol_box, box_prim, "(box ITEM): return a new box containing ITEM"), List(symbol_box_q, box_q_prim, "(box? ITEM): return #t if ITEM is a boxed value"), List(symbol_caaaar, caaaar_prim, "caaaar ...): "), List(symbol_caaadr, caaadr_prim, "(caaadr ...): "), List(symbol_caaar, caaar_prim, "(caaar ...): "), List(symbol_caadar, caadar_prim, "(caadar ...): "), List(symbol_caaddr, caaddr_prim, "(caaddr ...): "), List(symbol_caadr, caadr_prim, "(caadr ...): "), List(symbol_caar, caar_prim, "(caar ...): "), List(symbol_cadaar, cadaar_prim, "(cadaar ...): "), List(symbol_cadadr, cadadr_prim, "(cadadr ...): "), List(symbol_cadar, cadar_prim, "(cadar ...): "), List(symbol_caddar, caddar_prim, "(caddar ...): "), List(symbol_cadddr, cadddr_prim, "(cadddr ...): "), List(symbol_caddr, caddr_prim, "(caddr ITEM): return the (car (cdr (cdr ITEM)))"), List(symbol_cadr, cadr_prim, "(cadr ITEM): return the (car (cdr ITEM))"), List(symbol_call_with_current_continuation, call_cc_prim, "(call-with-current-continuation ...): "), List(symbol_call_cc, call_cc_prim, "(call/cc ...): "), List(symbol_car, car_prim, "(car LIST) returns the first element of LIST"), List(symbol_cd, current_directory_prim, "(cd [PATH]): get the current directory, or set it if PATH is given (alias current-directory)"), List(symbol_cdaaar, cdaaar_prim, "(cdaaar ...): "), List(symbol_cdaadr, cdaadr_prim, "(cdaadr ...): "), List(symbol_cdaar, cdaar_prim, "(cdaar ...): "), List(symbol_cdadar, cdadar_prim, "(cdadar ...): "), List(symbol_cdaddr, cdaddr_prim, "(cdaddr ...): "), List(symbol_cdadr, cdadr_prim, "(cdadr ...): "), List(symbol_cdar, cdar_prim, "(cdar ...): "), List(symbol_cddaar, cddaar_prim, "(cddaar ...): "), List(symbol_cddadr, cddadr_prim, "(cddadr ...): "), List(symbol_cddar, cddar_prim, "(cddar ...): "), List(symbol_cdddar, cdddar_prim, "(cdddar ...): "), List(symbol_cddddr, cddddr_prim, "(cddddr ...): "), List(symbol_cdddr, cdddr_prim, "(cdddr ...): "), List(symbol_cddr, cddr_prim, "(cddr ...): "), List(symbol_cdr, cdr_prim, "(cdr LIST) returns rest of LIST after (car LIST)"), List(symbol_char_to_integer, char_to_integer_prim, "(char->integer CHAR): return associated number of CHAR "), List(symbol_char_to_string, char_to_string_prim, "(char->string CHAR): "), List(symbol_char_alphabetic_q, char_alphabetic_q_prim, "(char-alphabetic? CHAR): return #t if CHAR is an alphabetic character, #f otherwise"), List(symbol_char_numeric_q, char_numeric_q_prim, "(char-numeric? CHAR): return #t if CHAR is a whitespace character, #f otherwise"), List(symbol_char_whitespace_q, char_whitespace_q_prim, "(char-whitespace? CHAR): return #t if CHAR is a whitespace character, #f otherwise"), List(symbol_char_is__q, char_is__q_prim, "(char=? CHAR1 CHAR2): return #t if CHAR1 has the same values as CHAR2, #f otherwise"), List(symbol_char_q, char_q_prim, "(char? ITEM): return #t if ITEM is a character, #f otherwise"), List(symbol_clear_unit_tests, clear_unit_tests_prim, "(clear-unit-tests): clear old unit tests. Usually run before define-tests"), List(symbol_cons, cons_prim, "(cons ITEM1 ITEM2): return a list with ITEM1 as car and ITEM2 as cdr (ITEM2 is typically a list)"), List(symbol_current_directory, current_directory_prim, "(current-directory [PATH]): get the current directory, or set it if PATH is given (alias cd)"), List(symbol_current_environment, current_environment_prim, "(current-environment): returns the current environment"), List(symbol_current_time, current_time_prim, "(current-time): returns the current time as number of seconds since 1970-1-1"), List(symbol_cut, cut_prim, "(cut ARGS...): return to toplevel with ARGS"), List(symbol_dict, dict_prim, "(dict ...): "), List(symbol_dir, dir_prim, "(dir [ITEM]): return items in environment, or, if ITEM is given, the items in module"), List(symbol_display, display_prim, "(display ITEM): display the ITEM as output"), List(symbol_div, quotient_prim, "(div arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and quotient)"), List(symbol_eq_q, eq_q_prim, "(eq? ITEM1 ITEM2): return #t if ITEM1 is eq to ITEM2, #f otherwise"), List(symbol_equal_q, equal_q_prim, "(equal? ITEM1 ITEM2): return #t if ITEM1 is equal to ITEM2, #f otherwise"), List(symbol_eqv_q, eqv_q_prim, "(eqv? ITEM1 ITEM2): return #t if ITEM1 and ITEM2 have the same value"), List(symbol_error, error_prim, "(error NAME MESSAGE): create an exception in NAME with MESSAGE"), List(symbol_eval, eval_prim, "(eval LIST): evaluates the LIST as a Scheme expression"), List(symbol_eval_ast, eval_ast_prim, "(eval-ast AST): evaluates the Abstract Syntax Tree as a Scheme expression (see parse and parse-string)"), List(symbol_even_q, even_q_prim, "(even? NUMBER): returns #t if NUMBER is odd, #f otherwise"), List(symbol_exit, exit_prim, "(exit): Exit the interpreter"), List(symbol_expt, expt_prim, "(expt BASE POWER): raise a base number to a power"), List(symbol_float, float_prim, "(float NUMBER): return NUMBER as a floating point value"), List(symbol_for_each, for_each_prim, "(for-each PROCEDURE LIST): apply PROCEDURE to each item in LIST, but don't return results"), List(symbol_format, format_prim, "(format STRING ITEM ...): format the string with ITEMS as arguments"), List(symbol_get_attr, getattr_prim, "(get-attr THING ATTR): get the ATTRIBUTE from the THING"), List(symbol_get_completions, get_completions_prim, "(get-completions ...): returns completions for TAB"), List(symbol_get_item, getitem_prim, "(get-item THING ITEM): get the ITEM from the THING (dict or vector)"), List(symbol_get_stack_trace, get_stack_trace_prim, "(get-stack-trace): return the current stack trace"), List(symbol_get_exception_message, get_exception_message_prim, "(get-exception-message EXCEPTION): get the message from the exception"), List(symbol_globals, globals_prim, "(globals): get global environment"), List(symbol_has_attr_q, hasattr_prim, "(has-attr? THING ATTR): does the THING have this attribute?"), List(symbol_has_item_q, hasitem_prim, "(has-item? THING ITEM): does the THING (dict or vector) have this ITEM?"), List(symbol_host_environment, host_environment_prim, "(host-environment): get the host environment (\"python\" or \"scheme\")"), List(symbol_import, import_prim, "(import MODULE...): import host-system modules; MODULEs are strings"), List(symbol_import_as, import_as_prim, "(import-as MODULE NAME): import a host-system module; MODULE is a string, and NAME is a symbol or string. Use * for NAME to import into toplevel environment"), List(symbol_import_from, import_from_prim, "(import-from MODULE NAME...): import from host-system module; MODULE is a string, and NAME is a symbol or string"), List(symbol_int_, int_prim, "(int NUMBER): return NUMBER as an integer"), List(symbol_integer_to_char, integer_to_char_prim, "(integer->char INTEGER): return the assocated character of INTEGER"), List(symbol_iter_q, iter_q_prim, "(iter? ITEM): return #t if ITEM is a iterator, #f otherwise"), List(symbol_length, length_prim, "(length LIST): returns the number of elements in top level of LIST"), List(symbol_List, list_prim, "(list ITEM ...): returns a list composed of all of the items"), List(symbol_list_to_string, list_to_string_prim, "(list->string LIST): returns the LIST as a string"), List(symbol_list_to_vector, list_to_vector_prim, "(list->vector LIST): returns the LIST as a vector"), List(symbol_list_ref, list_ref_prim, "(list-ref LIST INDEX): returns the item in LIST at INDEX (zero-based)"), List(symbol_list_q, list_q_prim, "(list? ITEM): return #t if ITEM is a list, #f otherwise"), List(symbol_load, load_prim, "(load FILENAME...): loads the given FILENAMEs"), List(symbol_load_as, load_as_prim, "(load-as FILENAME MODULE-NAME): load the filename, putting items in MODULE-NAME namespace"), List(symbol_macros, macros_prim, "(macros): return the names of the macros"), List(symbol_make_set, make_set_prim, "(make-set LIST): returns a list of unique items from LIST"), List(symbol_make_vector, make_vector_prim, "(make-vector LENGTH): returns a vector of length LENGTH"), List(symbol_Map, map_prim, "(map PROCEDURE LIST...): apply PROCEDURE to each element of LIST, and return return results"), List(symbol_max, max_prim, "(max ...): returns the maximum value from the list of values"), List(symbol_member, member_prim, "(member ITEM LIST): return LIST if ITEM in top level of LIST"), List(symbol_memq, memq_prim, "(memq ...): "), List(symbol_memv, memv_prim, "(memv ...): "), List(symbol_min, min_prim, "(min ...): returns the minimum value from the list of values"), List(symbol_mod, modulo_prim, "(mod arg0 arg1): modulo procedure for two arguments (aliases % and modulo)"), List(symbol_modulo, modulo_prim, "(modulo arg0 arg1): modulo procedure for two arguments (aliases mod and %)"), List(symbol_newline, newline_prim, "(newline): displays a new line in output"), List(symbol_not, not_prim, "(not ITEM): returns the boolean not of ITEM; ITEM is only #t when #t, otherwise #f"), List(symbol_null_q, null_q_prim, "(null? ITEM): return #t if ITEM is empty list, #f otherwise"), List(symbol_number_to_string, number_to_string_prim, "(number->string NUMBER): return NUMBER as a string"), List(symbol_number_q, number_q_prim, "(number? ITEM): return #t if ITEM is a number, #f otherwise"), List(symbol_odd_q, odd_q_prim, "(odd? NUMBER): returns #t if NUMBER is even, #f otherwise"), List(symbol_pair_q, pair_q_prim, "(pair? ITEM): "), List(symbol_parse, parse_prim, "(parse LIST): parse a list; returns Abstract Syntax Tree (AST)"), List(symbol_parse_string, parse_string_prim, "(parse-string STRING): parse a string; returns Abstract Syntax Tree (AST)"), List(symbol_print, print_prim, "(print ITEM): "), List(symbol_printf, printf_prim, "(printf FORMAT ARGS...): "), List(symbol_procedure_q, procedure_q_prim, "(procedure? ITEM): return #t if ITEM is a procedure, #f otherwise"), List(symbol_property, property_prim, "(property ...): "), List(symbol_python_eval, python_eval_prim, "(python-eval PYTHON-EXPRESSION [globals [locals]]): return the result of evaluating PYTHON-EXPRESSION string"), List(symbol_python_exec, python_exec_prim, "(python-exec PYTHON-STATEMENTS [globals [locals]]): return the result of evaluating PYTHON-STATEMENTS string"), List(symbol_quit, exit_prim, "(quit): Exit the interpreter"), List(symbol_quotient, quotient_prim, "(quotient arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and div)"), List(symbol_rac, rac_prim, "(rac LIST): return the last item of LIST"), List(symbol_random, random_prim, "(random N): return a random number in the range [0, N)"), List(symbol_Range, range_prim, "(range END), (range START END), or (RANGE START END STEP): (all integers)"), List(symbol_rational, rational_prim, "(rational NUMERATOR DENOMINTAOR): return a rational number"), List(symbol_rdc, rdc_prim, "(rdc LIST): return everything but last item in LIST"), List(symbol_read_string, read_string_prim, "(read-string ...): "), List(symbol_remainder, remainder_prim, "(remainder NUMBER1 NUMBER2): returns the remainder after dividing NUMBER1 by NUMBER2"), List(symbol_require, require_prim, "(require ...): "), List(symbol_reset_toplevel_env, reset_toplevel_env_prim, "(reset-toplevel-env): reset the toplevel environment"), List(symbol_reverse, reverse_prim, "(reverse LIST): "), List(symbol_round, round_prim, "(round NUMBER): round NUMBER to the nearest integer (may return float)"), List(symbol_set_attr_b, setattr_prim, "(setattr THING ATTR VALUE): sets THING.ITEM with VALUE"), List(symbol_set_car_b, set_car_b_prim, "(set-car! LIST ITEM): set the car of LIST to be ITEM"), List(symbol_set_cdr_b, set_cdr_b_prim, "(set-cdr! LIST ITEM): set the car of LIST to be ITEM (which is typically a list)"), List(symbol_set_item_b, setitem_prim, "(setitem THING ITEM VALUE): sets THING[ITEM] with VALUE"), List(symbol_snoc, snoc_prim, "(snoc ITEM LIST): cons the ITEM onto the end of LIST"), List(symbol_sort, sort_prim, "(sort PROCEDURE LIST): sort the list using PROCEDURE to compare items"), List(symbol_sqrt, sqrt_prim, "(sqrt NUMBER): return the square root of NUMBER"), List(symbol_string, string_prim, "(string ITEM): returns ITEM as a string"), List(symbol_string_to_list, string_to_list_prim, "(string->list STRING): string STRING as a list of characters"), List(symbol_string_to_number, string_to_number_prim, "(string->number STRING): return STRING as a number"), List(symbol_string_to_symbol, string_to_symbol_prim, "(string->symbol STRING): return STRING as a symbol"), List(symbol_string_append, string_append_prim, "(string-append STRING1 STRING2): append two strings together"), List(symbol_string_join, string_join_prim, "(string-join \", \" '(1 2 3)): gives \"1, 2, 3\""), List(symbol_string_length, string_length_prim, "(string-length STRING): returns the length of a string"), List(symbol_string_ref, string_ref_prim, "(string-ref STRING INDEX): return the character of STRING at position INDEX"), List(symbol_string_split, string_split_prim, "(string-split STRING CHAR): return a list with substrings of STRING where split by CHAR"), List(symbol_stringLessThan_q, stringLessThan_q_prim, "(string<? STRING1 STRING2): compare two strings to see if STRING1 is less than STRING2"), List(symbol_string_is__q, string_is__q_prim, "(string=? STRING1 STRING2): return #t if STRING1 is the same as STRING2, #f otherwise"), List(symbol_string_q, string_q_prim, "(string? ITEM): return #t if ITEM is a string, #f otherwise"), List(symbol_substring, substring_prim, "(substring STRING START [END]): return the substring of STRING starting with position START and ending before END. If END is not provided, it defaults to the length of the STRING"), List(symbol_symbol_to_string, symbol_to_string_prim, "(symbol->string SYMBOL): return SYMBOL as a string"), List(symbol_symbol_q, symbol_q_prim, "(symbol? ITEM): return #t if ITEM is a symbol, #f otherwise"), List(symbol_typeof, typeof_prim, "(typeof ITEM): returns type of ITEM"), List(symbol_unbox, unbox_prim, "(unbox BOX): return the contents of BOX"), List(symbol_unparse, unparse_prim, "(unparse AST): "), List(symbol_unparse_procedure, unparse_procedure_prim, "(unparse-procedure ...): "), List(symbol_use_jit, use_jit_prim, "(use-jit [BOOLEAN]): get Phase-2/JIT usage setting, or set it on/off if BOOLEAN is given"), List(symbol_use_lexical_address, use_lexical_address_prim, "(use-lexical-address [BOOLEAN]): get lexical-address setting, or set it on/off if BOOLEAN is given"), List(symbol_use_stack_trace, use_stack_trace_prim, "(use-stack-trace BOOLEAN): set stack-trace usage on/off"), List(symbol_use_tracing, use_tracing_prim, "(use-tracing [BOOLEAN]): get tracing setting, or set it on/off if BOOLEAN is given"), List(symbol_vector, vector_prim, "(vector [ITEMS]...): return ITEMs as a vector"), List(symbol_vector_to_list, vector_to_list_prim, "(vector->list VECTOR): return VECTOR as a list"), List(symbol_vector_length, vector_length_prim, "(vector-length VECTOR): returns length of VECTOR"), List(symbol_vector_ref, vector_ref_prim, "(vector-ref VECTOR INDEX): "), List(symbol_vector_set_b, vector_set_b_prim, "(vector-set! VECTOR INDEX VALUE): sets the item at INDEX of VECTOR"), List(symbol_vector_q, vector_q_prim, "(vector? ITEM): return #t if ITEM is a vector, #f otherwise"), List(symbol_void, void_prim, "(void): The null value symbol"), List(symbol_zero_q, zero_q_prim, "(zero? NUMBER): return #t if NUMBER is equal to zero, #f otherwise"))
    return make_initial_env_extended(Map(car, primitives), Map(cadr, primitives), Map(caddr, primitives))

def reset_toplevel_env():
    global toplevel_env
    toplevel_env = make_toplevel_env()
    return void_value

def make_external_proc(external_function_object):
    return make_proc(b_proc_183_d, external_function_object)

def get_values_for_params(params, associations, used, info, handler, fail):
    if (((params) is symbol_emptylist) is not False):
        if ((not(((associations) is symbol_emptylist))) and (association_q((associations).car)) and (((associations).car.car) is (symbol_multiply)) is not False):
            return List(get_value((associations).car))
        else:
            return symbol_emptylist
    else:
        return cons(get_value_from_associations((params).car, associations, info, handler, fail), get_values_for_params((params).cdr, associations, cons((params).car, used), info, handler, fail))

def get_value_from_associations(param, associations, info, handler, fail):
    global fail_reg, handler_reg, info_reg, msg_reg, pc
    symbol = get_symbol(param)
    value = assq(symbol, associations)
    if (value is not False):
        return get_value(value)
    else:
        if (association_q(param) is not False):
            return get_value(param)
        else:
            fail_reg = fail
            handler_reg = handler
            info_reg = info
            msg_reg = format("missing parameter: ~a", param)
            pc = runtime_error

def get_arg_associations(args, params, must_be_association, info, handler, fail):
    global fail_reg, handler_reg, info_reg, msg_reg, pc
    if (((args) is symbol_emptylist) is not False):
        return symbol_emptylist
    else:
        if (association_q((args).car) is not False):
            return cons((args).car, get_arg_associations((args).cdr, params, True, info, handler, fail))
        else:
            if (must_be_association is not False):
                fail_reg = fail
                handler_reg = handler
                info_reg = info
                msg_reg = format("non-keyword arg following keyword arg: ~a", (args).car)
                pc = runtime_error
            else:
                if (((params) is symbol_emptylist) is not False):
                    return List(association(symbol_multiply, args))
                else:
                    return cons(association(get_symbol((params).car), (args).car), get_arg_associations((args).cdr, (params).cdr, False, info, handler, fail))

def get_symbol(item):
    if (association_q(item) is not False):
        return (item).car
    else:
        if (symbol_q(item) is not False):
            return item
        else:
            raise Exception("symbol_get_symbol: " + format("invalid symbol ~a", *[item]))

def get_value(item):
    if (association_q(item) is not False):
        return (item).cdr.cdr.car
    else:
        return item

def association_q(x):
    return (list_q(x)) and (numeric_equal(length(x), 3)) and (((x).cdr.car) is (symbol_colon))

def make_associations(dict):
    if (((dict) is symbol_emptylist) is not False):
        return symbol_emptylist
    else:
        value = (dict).car.cdr.cdr.car
        keyword = (dict).car.car
        return cons(association(keyword, value), make_associations((dict).cdr))

def pattern_q(x):
    return (((x) is symbol_emptylist)) or (number_q(x)) or (boolean_q(x)) or (symbol_q(x)) or ((pair_q(x)) and (pattern_q((x).car)) and (pattern_q((x).cdr)))

def pattern_variable_q(x):
    return (symbol_q(x)) and (equal_q("?", substring(symbol_to_string(x), 0, 1)))

def constant_q(x):
    return (not(pattern_variable_q(x))) and (not(pair_q(x)))

def occurs_q():
    global k_reg, pattern_reg, pc, value_reg
    if (constant_q(pattern_reg) is not False):
        value_reg = False
        pc = apply_cont
    else:
        if (pattern_variable_q(pattern_reg) is not False):
            value_reg = equal_q(var_reg, pattern_reg)
            pc = apply_cont
        else:
            k_reg = make_cont(b_cont_57_d, pattern_reg, var_reg, k_reg)
            pattern_reg = (pattern_reg).car
            pc = occurs_q

def unify_patterns_hat():
    global ap1_reg, ap2_reg, apair1_reg, apair2_reg, k_reg, p1_reg, p2_reg, pair1_reg, pair2_reg, pattern_reg, pc, temp_1, temp_2, temp_3, temp_4, value_reg, var_reg
    if (pattern_variable_q(p1_reg) is not False):
        if (pattern_variable_q(p2_reg) is not False):
            value_reg = make_sub(symbol_unit, p1_reg, p2_reg, ap2_reg)
            pc = apply_cont
        else:
            k_reg = make_cont(b_cont_58_d, ap2_reg, p1_reg, p2_reg, k_reg)
            pattern_reg = p2_reg
            var_reg = p1_reg
            pc = occurs_q
    else:
        if (pattern_variable_q(p2_reg) is not False):
            temp_1 = p2_reg
            temp_2 = p1_reg
            temp_3 = ap2_reg
            temp_4 = ap1_reg
            p1_reg = temp_1
            p2_reg = temp_2
            ap1_reg = temp_3
            ap2_reg = temp_4
            pc = unify_patterns_hat
        else:
            if ((constant_q(p1_reg)) and (constant_q(p2_reg)) and (equal_q(p1_reg, p2_reg)) is not False):
                value_reg = make_sub(symbol_empty)
                pc = apply_cont
            else:
                if ((pair_q(p1_reg)) and (pair_q(p2_reg)) is not False):
                    apair2_reg = ap2_reg
                    apair1_reg = ap1_reg
                    pair2_reg = p2_reg
                    pair1_reg = p1_reg
                    pc = unify_pairs_hat
                else:
                    value_reg = False
                    pc = apply_cont

def unify_pairs_hat():
    global ap1_reg, ap2_reg, k_reg, p1_reg, p2_reg, pc
    k_reg = make_cont(b_cont_60_d, apair1_reg, apair2_reg, pair1_reg, pair2_reg, k_reg)
    ap2_reg = car_hat(apair2_reg)
    ap1_reg = car_hat(apair1_reg)
    p2_reg = (pair2_reg).car
    p1_reg = (pair1_reg).car
    pc = unify_patterns_hat

def instantiate_hat():
    global ap_reg, avar_reg, k2_reg, k_reg, pattern_reg, pc, value1_reg, value2_reg, var_reg
    if (constant_q(pattern_reg) is not False):
        value2_reg = ap_reg
        value1_reg = pattern_reg
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (pattern_variable_q(pattern_reg) is not False):
            avar_reg = ap_reg
            var_reg = pattern_reg
            pc = apply_sub_hat
        else:
            if (pair_q(pattern_reg) is not False):
                k2_reg = make_cont2(b_cont2_123_d, ap_reg, pattern_reg, s_reg, k2_reg)
                ap_reg = car_hat(ap_reg)
                pattern_reg = (pattern_reg).car
                pc = instantiate_hat
            else:
                raise Exception("symbol_instantiate_hat: " + format("bad pattern: ~a", *[pattern_reg]))

def make_sub(*args):
    args = List(*args)
    return cons(symbol_substitution, args)

def apply_sub_hat():
    global k2_reg, k_reg, pc, s_reg, value1_reg, value2_reg
    temp_1 = (s_reg).cdr
    if (((temp_1).car) is (symbol_empty) is not False):
        value2_reg = avar_reg
        value1_reg = var_reg
        k_reg = k2_reg
        pc = apply_cont2
    else:
        if (((temp_1).car) is (symbol_unit) is not False):
            new_apattern = ((((temp_1)).cdr).cdr).cdr.car
            new_pattern = (((temp_1)).cdr).cdr.car
            new_var = ((temp_1)).cdr.car
            if (equal_q(var_reg, new_var) is not False):
                value2_reg = new_apattern
                value1_reg = new_pattern
                k_reg = k2_reg
                pc = apply_cont2
            else:
                value2_reg = avar_reg
                value1_reg = var_reg
                k_reg = k2_reg
                pc = apply_cont2
        else:
            if (((temp_1).car) is (symbol_composite) is not False):
                s2 = (((temp_1)).cdr).cdr.car
                s1 = ((temp_1)).cdr.car
                k2_reg = make_cont2(b_cont2_124_d, s2, k2_reg)
                s_reg = s1
                pc = apply_sub_hat
            else:
                raise Exception("symbol_apply_sub_hat: " + format("bad substitution: ~a", *[s_reg]))

chars_to_scan = symbol_undefined
scan_line = symbol_undefined
scan_char = symbol_undefined
scan_position = symbol_undefined
last_scan_line = symbol_undefined
last_scan_char = symbol_undefined
last_scan_position = symbol_undefined
token_start_line = symbol_undefined
token_start_char = symbol_undefined
token_start_position = symbol_undefined
atom_tag = box(symbol_atom)
pair_tag = box(symbol_pair)
_starreader_generates_annotated_sexps_q_star = True
_starfilename_dict_star = dict()
_starfilename_vector_star = vlist()
_staruse_lexical_address_star = True
quote_q_hat = tagged_list_hat(symbol_quote, numeric_equal, 2)
quasiquote_q_hat = tagged_list_hat(symbol_quasiquote, numeric_equal, 2)
unquote_q_hat = tagged_list_hat(symbol_unquote, GreaterThanEqual, 2)
unquote_splicing_q_hat = tagged_list_hat(symbol_unquote_splicing, GreaterThanEqual, 2)
if_then_q_hat = tagged_list_hat(symbol_if, numeric_equal, 3)
if_else_q_hat = tagged_list_hat(symbol_if, numeric_equal, 4)
help_q_hat = tagged_list_hat(symbol_help, numeric_equal, 2)
association_q_hat = tagged2_list_hat(symbol_colon, numeric_equal, 3)
assignment_q_hat = tagged_list_hat(symbol_set_b, numeric_equal, 3)
func_q_hat = tagged_list_hat(symbol_func, numeric_equal, 2)
callback_q_hat = tagged_list_hat(symbol_callback, numeric_equal, 2)
define_q_hat = tagged_list_hat(symbol_define, GreaterThanEqual, 3)
define_b_q_hat = tagged_list_hat(symbol_define_b, GreaterThanEqual, 3)
define_syntax_q_hat = tagged_list_hat(symbol_define_syntax, GreaterThanEqual, 3)
begin_q_hat = tagged_list_hat(symbol_begin, GreaterThanEqual, 2)
lambda_q_hat = tagged_list_or_hat(symbol_lambda, symbol_λ, GreaterThanEqual, 3)
lambda_no_defines_q_hat = tagged_list_hat(symbol_lambda_no_defines, GreaterThanEqual, 3)
trace_lambda_no_defines_q_hat = tagged_list_hat(symbol_trace_lambda_no_defines, GreaterThanEqual, 4)
raise_q_hat = tagged_list_hat(symbol_raise, numeric_equal, 2)
choose_q_hat = tagged_list_hat(symbol_choose, GreaterThanEqual, 1)
try_q_hat = tagged_list_hat(symbol_try, GreaterThanEqual, 2)
catch_q_hat = tagged_list_hat(symbol_catch, GreaterThanEqual, 3)
finally_q_hat = tagged_list_hat(symbol_finally, GreaterThanEqual, 2)
define_tests_q_hat = tagged_list_hat(symbol_define_tests, GreaterThanEqual, 2)
run_tests_q_hat = tagged_list_hat(symbol_run_tests, GreaterThanEqual, 1)
lambda_transformer_hat = make_macro(b_macro_1_d)
trace_lambda_transformer_hat = make_macro(b_macro_2_d)
let_transformer_hat = make_macro(b_macro_3_d)
letrec_transformer_hat = make_macro(b_macro_4_d)
mit_define_transformer_hat = make_macro(b_macro_5_d)
and_transformer_hat = make_macro(b_macro_6_d)
_stargensym_counter_star = 0
or_transformer_hat = make_macro(b_macro_7_d)
cond_transformer_hat = make_macro(b_macro_8_d)
let_star_transformer_hat = make_macro(b_macro_9_d)
case_transformer_hat = make_macro(b_macro_10_d)
record_case_transformer_hat = make_macro(b_macro_11_d)
define_datatype_transformer_hat = make_macro(b_macro_12_d)
cases_transformer_hat = make_macro(b_macro_13_d)
macro_env = symbol_undefined
REP_k = make_cont2(b_cont2_53_d)
REP_handler = make_handler2(b_handler2_2_d)
REP_fail = make_fail(b_fail_1_d)
_starlast_fail_star = REP_fail
_startokens_left_star = symbol_undefined
try_parse_handler = make_handler2(b_handler2_3_d)
unit_test_table = symbol_undefined
_startracing_on_q_star = False
_starstack_trace_star = List(symbol_emptylist)
_staruse_stack_trace_star = False
_staruse_jit_star = True
clear_unit_tests_prim = make_proc(b_proc_5_d)
void_prim = make_proc(b_proc_6_d)
zero_q_prim = make_proc(b_proc_7_d)
python_eval_prim = make_proc(b_proc_8_d)
python_exec_prim = make_proc(b_proc_9_d)
exit_prim = make_proc(b_proc_10_d)
expt_prim = make_proc(b_proc_11_d)
end_of_session = List(symbol_exiting, symbol_the, symbol_interpreter)
string_join_prim = make_proc(b_proc_12_d)
eval_prim = make_proc(b_proc_13_d)
eval_ast_prim = make_proc(b_proc_14_d)
parse_prim = make_proc(b_proc_15_d)
string_length_prim = make_proc(b_proc_16_d)
string_ref_prim = make_proc(b_proc_17_d)
unparse_prim = make_proc(b_proc_18_d)
unparse_procedure_prim = make_proc(b_proc_19_d)
parse_string_prim = make_proc(b_proc_20_d)
read_string_prim = make_proc(b_proc_21_d)
apply_prim = make_proc(b_proc_22_d)
sqrt_prim = make_proc(b_proc_23_d)
odd_q_prim = make_proc(b_proc_24_d)
even_q_prim = make_proc(b_proc_25_d)
quotient_prim = make_proc(b_proc_26_d)
remainder_prim = make_proc(b_proc_27_d)
print_prim = make_proc(b_proc_28_d)
string_prim = make_proc(b_proc_29_d)
substring_prim = make_proc(b_proc_30_d)
number_to_string_prim = make_proc(b_proc_31_d)
assv_prim = make_proc(b_proc_32_d)
memv_prim = make_proc(b_proc_33_d)
display_prim = make_proc(b_proc_34_d)
newline_prim = make_proc(b_proc_35_d)
_starneed_newline_star = False
load_prim = make_proc(b_proc_36_d)
load_stack = symbol_emptylist
length_prim = make_proc(b_proc_37_d)
symbol_q_prim = make_proc(b_proc_38_d)
number_q_prim = make_proc(b_proc_39_d)
boolean_q_prim = make_proc(b_proc_40_d)
string_q_prim = make_proc(b_proc_41_d)
char_q_prim = make_proc(b_proc_42_d)
char_is__q_prim = make_proc(b_proc_43_d)
char_whitespace_q_prim = make_proc(b_proc_44_d)
char_to_integer_prim = make_proc(b_proc_45_d)
integer_to_char_prim = make_proc(b_proc_46_d)
char_alphabetic_q_prim = make_proc(b_proc_47_d)
char_numeric_q_prim = make_proc(b_proc_48_d)
null_q_prim = make_proc(b_proc_49_d)
box_q_prim = make_proc(b_proc_50_d)
pair_q_prim = make_proc(b_proc_51_d)
box_prim = make_proc(b_proc_52_d)
unbox_prim = make_proc(b_proc_53_d)
cons_prim = make_proc(b_proc_54_d)
car_prim = make_proc(b_proc_55_d)
cdr_prim = make_proc(b_proc_56_d)
cadr_prim = make_proc(b_proc_57_d)
caddr_prim = make_proc(b_proc_58_d)
caaaar_prim = make_proc(b_proc_59_d)
caaadr_prim = make_proc(b_proc_60_d)
caaar_prim = make_proc(b_proc_61_d)
caadar_prim = make_proc(b_proc_62_d)
caaddr_prim = make_proc(b_proc_63_d)
caadr_prim = make_proc(b_proc_64_d)
caar_prim = make_proc(b_proc_65_d)
cadaar_prim = make_proc(b_proc_66_d)
cadadr_prim = make_proc(b_proc_67_d)
cadar_prim = make_proc(b_proc_68_d)
caddar_prim = make_proc(b_proc_69_d)
cadddr_prim = make_proc(b_proc_70_d)
cdaaar_prim = make_proc(b_proc_71_d)
cdaadr_prim = make_proc(b_proc_72_d)
cdaar_prim = make_proc(b_proc_73_d)
cdadar_prim = make_proc(b_proc_74_d)
cdaddr_prim = make_proc(b_proc_75_d)
cdadr_prim = make_proc(b_proc_76_d)
cdar_prim = make_proc(b_proc_77_d)
cddaar_prim = make_proc(b_proc_78_d)
cddadr_prim = make_proc(b_proc_79_d)
cddar_prim = make_proc(b_proc_80_d)
cdddar_prim = make_proc(b_proc_81_d)
cddddr_prim = make_proc(b_proc_82_d)
cdddr_prim = make_proc(b_proc_83_d)
cddr_prim = make_proc(b_proc_84_d)
list_prim = make_proc(b_proc_85_d)
assert_prim = make_proc(b_proc_86_d)
make_set_prim = make_proc(b_proc_87_d)
plus_prim = make_proc(b_proc_88_d)
minus_prim = make_proc(b_proc_89_d)
times_prim = make_proc(b_proc_90_d)
divide_prim = make_proc(b_proc_91_d)
modulo_prim = make_proc(b_proc_92_d)
min_prim = make_proc(b_proc_93_d)
max_prim = make_proc(b_proc_94_d)
lt_prim = make_proc(b_proc_95_d)
gt_prim = make_proc(b_proc_96_d)
lt_or_eq_prim = make_proc(b_proc_97_d)
gt_or_eq_prim = make_proc(b_proc_98_d)
equal_sign_prim = make_proc(b_proc_99_d)
abs_prim = make_proc(b_proc_100_d)
equal_q_prim = make_proc(b_proc_101_d)
eq_q_prim = make_proc(b_proc_102_d)
memq_prim = make_proc(b_proc_103_d)
member_prim = make_proc(b_proc_104_d)
random_prim = make_proc(b_proc_105_d)
range_prim = make_proc(b_proc_106_d)
snoc_prim = make_proc(b_proc_107_d)
rac_prim = make_proc(b_proc_108_d)
rdc_prim = make_proc(b_proc_109_d)
set_car_b_prim = make_proc(b_proc_110_d)
set_cdr_b_prim = make_proc(b_proc_111_d)
load_as_prim = make_proc(b_proc_112_d)
get_stack_trace_prim = make_proc(b_proc_113_d)
call_cc_prim = make_proc(b_proc_115_d)
abort_prim = make_proc(b_proc_116_d)
require_prim = make_proc(b_proc_117_d)
cut_prim = make_proc(b_proc_118_d)
reverse_prim = make_proc(b_proc_119_d)
append_prim = make_proc(b_proc_120_d)
string_to_number_prim = make_proc(b_proc_121_d)
string_is__q_prim = make_proc(b_proc_122_d)
list_to_vector_prim = make_proc(b_proc_123_d)
list_to_string_prim = make_proc(b_proc_124_d)
char_to_string_prim = make_proc(b_proc_125_d)
string_to_list_prim = make_proc(b_proc_126_d)
string_to_symbol_prim = make_proc(b_proc_127_d)
symbol_to_string_prim = make_proc(b_proc_128_d)
vector_to_list_prim = make_proc(b_proc_129_d)
vector_length_prim = make_proc(b_proc_130_d)
get_completions_prim = make_proc(b_proc_131_d)
dir_prim = make_proc(b_proc_132_d)
macros_prim = make_proc(b_proc_133_d)
current_time_prim = make_proc(b_proc_134_d)
map_prim = make_proc(b_proc_135_d)
for_each_prim = make_proc(b_proc_136_d)
format_prim = make_proc(b_proc_137_d)
current_environment_prim = make_proc(b_proc_138_d)
import_prim = make_proc(b_proc_139_d)
import_as_prim = make_proc(b_proc_140_d)
import_from_prim = make_proc(b_proc_141_d)
not_prim = make_proc(b_proc_142_d)
printf_prim = make_proc(b_proc_143_d)
vector_prim = make_proc(b_proc_144_d)
vector_set_b_prim = make_proc(b_proc_145_d)
vector_ref_prim = make_proc(b_proc_146_d)
make_vector_prim = make_proc(b_proc_147_d)
error_prim = make_proc(b_proc_148_d)
list_ref_prim = make_proc(b_proc_149_d)
current_directory_prim = make_proc(b_proc_150_d)
round_prim = make_proc(b_proc_151_d)
use_stack_trace_prim = make_proc(b_proc_152_d)
use_jit_prim = make_proc(b_proc_153_d)
use_tracing_prim = make_proc(b_proc_154_d)
eqv_q_prim = make_proc(b_proc_155_d)
vector_q_prim = make_proc(b_proc_156_d)
atom_q_prim = make_proc(b_proc_157_d)
iter_q_prim = make_proc(b_proc_158_d)
getitem_prim = make_proc(b_proc_159_d)
setitem_prim = make_proc(b_proc_160_d)
hasitem_prim = make_proc(b_proc_161_d)
getattr_prim = make_proc(b_proc_162_d)
setattr_prim = make_proc(b_proc_163_d)
hasattr_prim = make_proc(b_proc_164_d)
list_q_prim = make_proc(b_proc_165_d)
procedure_q_prim = make_proc(b_proc_166_d)
stringLessThan_q_prim = make_proc(b_proc_167_d)
float_prim = make_proc(b_proc_168_d)
globals_prim = make_proc(b_proc_169_d)
int_prim = make_proc(b_proc_170_d)
assq_prim = make_proc(b_proc_171_d)
dict_prim = make_proc(b_proc_172_d)
property_prim = make_proc(b_proc_173_d)
rational_prim = make_proc(b_proc_174_d)
reset_toplevel_env_prim = make_proc(b_proc_175_d)
sort_prim = make_proc(b_proc_176_d)
string_append_prim = make_proc(b_proc_177_d)
string_split_prim = make_proc(b_proc_178_d)
typeof_prim = make_proc(b_proc_179_d)
use_lexical_address_prim = make_proc(b_proc_180_d)
host_environment_prim = make_proc(b_proc_181_d)
get_exception_message_prim = make_proc(b_proc_182_d)
toplevel_env = symbol_undefined
pc_halt_signal = False
def run(setup, *args):
    args = List(*args)
    Apply(setup, args)
    return trampoline()

def start():
    global macro_env, toplevel_env, unit_test_table
    toplevel_env = make_toplevel_env()
    macro_env = make_macro_env_hat()
    unit_test_table = dict()
    return read_eval_print_loop_rm()

def restart():
    printf("Restarting...\n")
    return read_eval_print_loop_rm()

initialize_globals()

def main():
    print('Calysto Scheme, version 2.1.7')
    print('----------------------------')
    import sys
    for filename in sys.argv[1:]:
        if filename.startswith('-'): continue
        if load_native(filename): continue
        break
    if '-i' in sys.argv[1:] or sys.argv[1:] == []:
        print('Use (exit) to exit')
        read_eval_print_loop_rm()
        print()
if __name__ == '__main__':
    main()
