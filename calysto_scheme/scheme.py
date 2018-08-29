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

from collections import Iterable
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

PY3 = sys.version_info[0] == 3

__version__ = "1.4.0"

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

def pivot (p, l):
    if null_q(l):
        return make_symbol("done")
    elif null_q(cdr(l)):
        return make_symbol("done")
    result = apply_comparison(p, car(l), cadr(l))
    if result:
        return pivot(p, cdr(l))
    else:
        return car(l)

def make_comparison_function(procedure):
    def compare(carl, cadrl):
        GLOBALS["save_k2_reg"] = k2_reg
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

## usage: (partition 4 '(6 4 2 1 7) () ()) -> returns partitions
def partition (p, piv, l, p1, p2):
    if (null_q(l)):
        return List(p1, p2)
    result = apply_comparison(p, car(l), piv)
    if (result):
        return partition(p, piv, cdr(l), cons(car(l), p1), p2)
    else:
        return partition(p, piv, cdr(l), p1, cons(car(l), p2))

def sort_native(args, env2, info, handler, fail):
    p = car(args)
    l = cadr(args)
    if procedure_q(p):
        f = make_comparison_function(p)
    else:
        f = p
    return sort(f, l)

def sort(f, l):
    piv = pivot(f, l)
    if (piv is make_symbol("done")): return l
    parts = partition(f, piv, l, symbol_emptylist, symbol_emptylist)
    return append(sort(f, car(parts)),
                  sort(f, cadr(parts)))

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
    return List(symbol_procedure, *args)

def make_macro(*args):
    return List(symbol_macro_transformer, *args)

def make_cont(*args):
    return List(symbol_continuation, *args)

def make_cont2(*args):
    return List(symbol_continuation2, *args)

def make_cont3(*args):
    return List(symbol_continuation3, *args)

def make_cont4(*args):
    return List(symbol_continuation4, *args)

def make_fail(*args):
    return List(symbol_fail_continuation, *args)

def make_handler(*args):
    return List(symbol_handler, *args)

def make_handler2(*args):
    return List(symbol_handler2, *args)

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
    length = len(list(ls))
    return length >= n

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
    return pair_q(item) and (car(item) is symbol_procedure)

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
    return f(*list_to_vector(lyst))

### Annotated expression support:

def tagged_list_hat(keyword, op, length):
    def tagged_list(asexp):
        return (list_q_hat(asexp) and
                op(length_hat(asexp), length) and
                symbol_q_hat(car_hat(asexp)) and
                eq_q_hat(car_hat(asexp), keyword))
    return tagged_list

def tagged_list_or_hat(keyword1, keyword2, op, length):
    def tagged_list(asexp):
        return (list_q_hat(asexp) and
                op(length_hat(asexp), length) and
                symbol_q_hat(car_hat(asexp)) and
                (eq_q_hat(car_hat(asexp), keyword1) or
                 eq_q_hat(car_hat(asexp), keyword2)))
    return tagged_list

def tagged2_list_hat(keyword, op, length):
    def tagged2_list(asexp):
        return (list_q_hat(asexp) and
                op(length_hat(asexp), length) and
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
                exception_reg = make_exception("UnhandledException", str(e), symbol_none, symbol_none, symbol_none)
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
    if text:
        lines = text.split("\n")
        if len(lines) > 0 and lines[-1].strip() == "":
            return True ## force it
        ## else, only if valid parse
        return try_parse(text)
    return True

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
            prompt = "... "
        except EOFError:
            return "(exit)"
        except:
            return ""
        if ready_to_eval(retval):
            return retval.getvalue()
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
        bindings = car(frame)
        variables = cadr(frame)
        i = 0
        while not null_q(variables):
            if eq_q(car(variables), variable):
                return bindings[i]
            variables = cdr(variables)
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
        if x is caar(ls):
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
        if eq_q(x, caar(ls)):
            return car(ls)
        ls = cdr(ls)
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
    for component in cdr(components):
        if hasattr(retval, component.name):
            retval = getattr(retval, component.name)
        else:
            return False
    return True

def get_external_member(obj, components):
    # components: (math sqrt)
    retval = obj
    for component in cdr(components):
        if hasattr(retval, component.name):
            retval = getattr(retval, component.name)
        else:
            return void_value
    return retval

def dlr_apply(f, args):
    largs = list_to_vector(args)
    fargs = []
    fkwargs = {}
    for larg in largs:
        if association_q(larg):
            sym = symbol_to_string(car(larg))
            if sym == "*":
                fargs = caddr(larg)
            elif sym == "**":
                fkwargs = caddr(larg)
            else:
                fkwargs[sym] = caddr(larg)
        else:
            fargs.append(larg)
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
    result = execute_rm('(load "%s")' % filename, symbol_stdin)
    if true_q(exception_q(result)):
        handle_exception(result)
        return False # continue?
    return True # continue?

def getitem_native(dictionary, item):
    if item in dictionary:
        return dictionary[item]
    elif string_q(item) and hasattr(dictionary, item):
        return getattr(dictionary, item)
    else:
        return False

def setitem_native(dictionary, item, value):
    try:
        dictionary[item] = value
    except:
        setattr(dictionary, item, value)
    return value

def hasitem_native(dictionary, key):
    return key in dictionary

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
symbol_else = make_symbol("else")
symbol_eq_q = make_symbol("eq?")
symbol_quote = make_symbol("quote")
symbol_memq = make_symbol("memq")
symbol_define = make_symbol("define")
symbol_lambda = make_symbol("lambda")
symbol_args = make_symbol("args")
symbol_if = make_symbol("if")
symbol_numeric_equal = make_symbol("=")
symbol_length = make_symbol("length")
symbol_error = make_symbol("error")
symbol_car = make_symbol("car")
symbol_append = make_symbol("append")
symbol_list_to_vector = make_symbol("list->vector")
symbol_cons = make_symbol("cons")
symbol_List = make_symbol("list")
symbol_unit = make_symbol("unit")
symbol_composite = make_symbol("composite")
symbol_continuation2 = make_symbol("continuation2")
symbol_set_b = make_symbol("set!")
symbol_r = make_symbol("r")
symbol_cond = make_symbol("cond")
symbol_else_code = make_symbol("else-code")
symbol_Apply = make_symbol("apply")
symbol_cdr = make_symbol("cdr")
symbol_x = make_symbol("x")
symbol_and = make_symbol("and")
symbol_pair_q = make_symbol("pair?")
symbol_not = make_symbol("not")
symbol_begin = make_symbol("begin")
symbol_cases = make_symbol("cases")
symbol_stdin = make_symbol("stdin")
symbol_exception = make_symbol("exception")
symbol_end_marker = make_symbol("end-marker")
symbol_ok = make_symbol("ok")
symbol_continuation3 = make_symbol("continuation3")
symbol_continuation4 = make_symbol("continuation4")
symbol_dot = make_symbol("dot")
symbol_fail_continuation = make_symbol("fail-continuation")
symbol_handler = make_symbol("handler")
symbol_handler2 = make_symbol("handler2")
symbol_procedure = make_symbol("procedure")
symbol_macro_transformer = make_symbol("macro-transformer")
symbol_bool = make_symbol("bool")
symbol_or = make_symbol("or")
symbol__is_to_ = make_symbol("=>")
symbol_th = make_symbol("th")
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
symbol_dotdotdot = make_symbol("...")
symbol_application = make_symbol("application")
symbol_unknown = make_symbol("unknown")
symbol_macro_generated_exp = make_symbol("macro-generated-exp")
symbol_colon = make_symbol(":")
symbol_b_procedure_d = make_symbol("<procedure>")
symbol_b_environment_d = make_symbol("<environment>")
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
symbol_for_each = make_symbol("for-each")
symbol_format = make_symbol("format")
symbol_get = make_symbol("get")
symbol_get_completions = make_symbol("get-completions")
symbol_get_stack_trace = make_symbol("get-stack-trace")
symbol_hasitem = make_symbol("hasitem")
symbol_host_environment = make_symbol("host-environment")
symbol_import = make_symbol("import")
symbol_import_as = make_symbol("import-as")
symbol_import_from = make_symbol("import-from")
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
symbol_reverse = make_symbol("reverse")
symbol_round = make_symbol("round")
symbol_set_car_b = make_symbol("set-car!")
symbol_set_cdr_b = make_symbol("set-cdr!")
symbol_snoc = make_symbol("snoc")
symbol_sqrt = make_symbol("sqrt")
symbol_string_to_list = make_symbol("string->list")
symbol_string_to_number = make_symbol("string->number")
symbol_string_to_symbol = make_symbol("string->symbol")
symbol_string_length = make_symbol("string-length")
symbol_string_ref = make_symbol("string-ref")
symbol_stringLessThan_q = make_symbol("string<?")
symbol_string_is__q = make_symbol("string=?")
symbol_string_q = make_symbol("string?")
symbol_string_join = make_symbol("string-join")
symbol_substring = make_symbol("substring")
symbol_symbol_to_string = make_symbol("symbol->string")
symbol_symbol_q = make_symbol("symbol?")
symbol_unbox = make_symbol("unbox")
symbol_unparse = make_symbol("unparse")
symbol_unparse_procedure = make_symbol("unparse-procedure")
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
symbol_assq = make_symbol("assq")
symbol_dict = make_symbol("dict")
symbol_float = make_symbol("float")
symbol_getitem = make_symbol("getitem")
symbol_globals = make_symbol("globals")
symbol_int_ = make_symbol("int")
symbol_property = make_symbol("property")
symbol_reset_toplevel_env = make_symbol("reset-toplevel-env")
symbol_setitem = make_symbol("setitem")
symbol_sort = make_symbol("sort")
symbol_string_append = make_symbol("string-append")
symbol_string_split = make_symbol("string-split")
symbol_typeof = make_symbol("typeof")
symbol_use_lexical_address = make_symbol("use-lexical-address")
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
wrong_reg = symbol_undefined
x_reg = symbol_undefined
y_reg = symbol_undefined
temp_2 = symbol_undefined
temp_3 = symbol_undefined
temp_4 = symbol_undefined
temp_1 = symbol_undefined
def apply_cont():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont_1_d(chars, fail, k):
    GLOBALS['value3_reg'] = fail
    GLOBALS['value2_reg'] = chars
    GLOBALS['value1_reg'] = value_reg
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont3

def b_cont_2_d(v1, info, k):
    GLOBALS['value_reg'] = List(pair_tag, v1, value_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_3_d(x, info, k):
    GLOBALS['k_reg'] = make_cont(b_cont_2_d, value_reg, info, k)
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['x_reg'] = cdr(x)
    GLOBALS['pc'] = annotate_cps

def b_cont_4_d(v1, k):
    GLOBALS['value_reg'] = cons(v1, value_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_5_d(x, k):
    GLOBALS['k_reg'] = make_cont(b_cont_4_d, value_reg, k)
    GLOBALS['x_reg'] = cdr(x)
    GLOBALS['pc'] = unannotate_cps

def b_cont_6_d(k):
    GLOBALS['value_reg'] = list_to_vector(value_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_7_d(x, k):
    GLOBALS['k_reg'] = make_cont(b_cont_4_d, value_reg, k)
    GLOBALS['x_reg'] = caddr(x)
    GLOBALS['pc'] = unannotate_cps

def b_cont_8_d(end, tokens_left, fail, k):
    GLOBALS['value4_reg'] = fail
    GLOBALS['value3_reg'] = tokens_left
    GLOBALS['value2_reg'] = end
    GLOBALS['value1_reg'] = value_reg
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont4

def b_cont_9_d(end, tokens, fail, k):
    GLOBALS['value4_reg'] = fail
    GLOBALS['value3_reg'] = rest_of(tokens)
    GLOBALS['value2_reg'] = end
    GLOBALS['value1_reg'] = value_reg
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont4

def b_cont_10_d(src, start, tokens, handler, fail, k):
    GLOBALS['k_reg'] = make_cont4(b_cont4_3_d, src, start, value_reg, k)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['src_reg'] = src
    GLOBALS['tokens_reg'] = rest_of(tokens)
    GLOBALS['pc'] = read_sexp

def b_cont_11_d():
    GLOBALS['final_reg'] = value_reg
    GLOBALS['pc'] = pc_halt_signal

def b_cont_12_d(adatum, senv, info, handler, fail, k):
    formals_list = symbol_undefined
    name = symbol_undefined
    name = untag_atom_hat(cadr_hat(adatum))
    formals_list = (value_reg if (list_q(value_reg)) and (not(association_q(value_reg))) else cons(last(value_reg), head(value_reg)))
    GLOBALS['k_reg'] = make_cont2(b_cont2_9_d, name, value_reg, info, k)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = cons(formals_list, senv)
    GLOBALS['adatum_list_reg'] = cdddr_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont_13_d(adatum, senv, info, handler, fail, k):
    formals_list = symbol_undefined
    formals_list = (value_reg if (list_q(value_reg)) and (not(association_q(value_reg))) else cons(last(value_reg), head(value_reg)))
    GLOBALS['k_reg'] = make_cont2(b_cont2_18_d, value_reg, info, k)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = cons(formals_list, senv)
    GLOBALS['adatum_list_reg'] = cddr_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont_14_d(senv, info, handler, fail, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = replace_info(value_reg, info)
    GLOBALS['pc'] = aparse

def b_cont_15_d(senv, info, handler, fail, k):
    GLOBALS['k_reg'] = make_cont(b_cont_14_d, senv, info, handler, fail, k)
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['x_reg'] = value_reg
    GLOBALS['pc'] = annotate_cps

def b_cont_16_d(aclauses, name, info, fail, k):
    GLOBALS['value2_reg'] = fail
    GLOBALS['value1_reg'] = define_syntax_aexp(name, value_reg, aclauses, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont_17_d(adatum, senv, info, handler, fail, k):
    if true_q(original_source_info_q(adatum)):
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['senv_reg'] = senv
        GLOBALS['adatum_reg'] = replace_info(value_reg, snoc(symbol_quasiquote, info))
        GLOBALS['pc'] = aparse
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['senv_reg'] = senv
        GLOBALS['adatum_reg'] = replace_info(value_reg, info)
        GLOBALS['pc'] = aparse

def b_cont_18_d(adatum, senv, info, handler, fail, k):
    GLOBALS['k_reg'] = make_cont(b_cont_17_d, adatum, senv, info, handler, fail, k)
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['x_reg'] = value_reg
    GLOBALS['pc'] = annotate_cps

def b_cont_19_d(info, fail, k):
    GLOBALS['value2_reg'] = fail
    GLOBALS['value1_reg'] = lit_aexp(value_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont_20_d(info, fail, k):
    GLOBALS['value2_reg'] = fail
    GLOBALS['value1_reg'] = lit_aexp(cadr(value_reg), info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont_21_d(tests, fail, k):
    GLOBALS['value2_reg'] = fail
    GLOBALS['value1_reg'] = cons(value_reg, tests)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont_22_d(msg, info, handler, fail):
    GLOBALS['fail_reg'] = fail
    GLOBALS['exception_reg'] = make_exception("ParseError", format("~a ~a", msg, value_reg), get_srcfile(info), get_start_line(info), get_start_char(info))
    GLOBALS['handler_reg'] = handler
    GLOBALS['pc'] = apply_handler2

def b_cont_23_d(bodies2, formals, k):
    GLOBALS['value_reg'] = append(List(symbol_lambda_no_defines), append(List(formals), List(append(List(symbol_letrec), append(List(value_reg), at_hat(bodies2))))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_24_d(bodies2, name, formals, k):
    GLOBALS['value_reg'] = append(List(symbol_trace_lambda_no_defines), append(List(name), append(List(formals), List(append(List(symbol_letrec), append(List(value_reg), at_hat(bodies2)))))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_25_d(adatum, bodies, handler, fail, k):
    if true_q(value_reg):
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['adatum_reg'] = adatum
        GLOBALS['msg_reg'] = "misplaced define in"
        GLOBALS['pc'] = aparse_error
    else:
        GLOBALS['value2_reg'] = bodies
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2

def b_cont_26_d(defines, handler, fail, k):
    return get_define_var_and_exp_hat(car(defines), handler, fail, make_cont2(b_cont2_45_d, value_reg, k))

def b_cont_27_d(bindings, k):
    GLOBALS['value_reg'] = append(List(symbol_let), append(List(List(car_hat(bindings))), List(value_reg)))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_28_d(clauses, var, k):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if true_q(eq_q_hat(car_hat(clause), symbol_else)):
        GLOBALS['value_reg'] = cons(clause, value_reg)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(symbol_q_hat(car_hat(clause))):
            GLOBALS['value_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont
        else:
            GLOBALS['value_reg'] = cons(append(List(append(List(symbol_memq), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), at_hat(cdr_hat(clause))), value_reg)
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont

def b_cont_29_d(fields, name, k2):
    constructor_def = symbol_undefined
    constructor_def = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(symbol_args), List(append(List(symbol_if), append(List(append(List(symbol_numeric_equal), append(List(append(List(symbol_length), List(symbol_args))), List(length_hat(fields))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), List("wrong number of arguments")))))))))))))
    GLOBALS['value2_reg'] = constructor_def
    GLOBALS['value1_reg'] = name
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont_30_d(cdrs, fields, name, k):
    GLOBALS['value_reg'] = append(List(symbol_if), append(List(append(List(cadar_hat(fields)), List(append(List(symbol_car), List(cdrs))))), append(List(value_reg), List(append(List(symbol_error), append(List(append(List(symbol_quote), List(name))), append(List("~a is not of type ~a"), append(List(append(List(symbol_car), List(cdrs))), List(append(List(symbol_quote), List(cadar_hat(fields))))))))))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_31_d(adatum, macro_keyword, fail, k):
    if true_q(has_source_info_q(value_reg)):
        GLOBALS['value2_reg'] = fail
        GLOBALS['value1_reg'] = value_reg
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        info = symbol_undefined
        info = get_source_info(adatum)
        if true_q(original_source_info_q(adatum)):
            GLOBALS['value2_reg'] = fail
            GLOBALS['value1_reg'] = replace_info(value_reg, snoc(macro_keyword, info))
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont2
        else:
            GLOBALS['value2_reg'] = fail
            GLOBALS['value1_reg'] = replace_info(value_reg, info)
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont2

def b_cont_32_d(adatum, macro_keyword, fail, k):
    GLOBALS['k_reg'] = make_cont(b_cont_31_d, adatum, macro_keyword, fail, k)
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['x_reg'] = value_reg
    GLOBALS['pc'] = annotate_cps

def b_cont_33_d(aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k):
    if true_q(value_reg):
        GLOBALS['k2_reg'] = make_cont2(b_cont2_56_d, fail, k)
        GLOBALS['ap_reg'] = right_apattern
        GLOBALS['s_reg'] = value_reg
        GLOBALS['pattern_reg'] = right_pattern
        GLOBALS['pc'] = instantiate_hat
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['adatum_reg'] = adatum
        GLOBALS['aclauses_reg'] = cdr_hat(aclauses)
        GLOBALS['clauses_reg'] = cdr(clauses)
        GLOBALS['pc'] = process_macro_clauses_hat

def b_cont_34_d(aclauses, adatum, clauses, left_apattern, left_pattern, right_apattern, right_pattern, handler, fail, k):
    GLOBALS['k_reg'] = make_cont(b_cont_33_d, aclauses, adatum, clauses, right_apattern, right_pattern, handler, fail, k)
    GLOBALS['ap2_reg'] = adatum
    GLOBALS['ap1_reg'] = left_apattern
    GLOBALS['p2_reg'] = value_reg
    GLOBALS['p1_reg'] = left_pattern
    GLOBALS['pc'] = unify_patterns_hat

def b_cont_35_d(v1, k):
    GLOBALS['value_reg'] = append(List(symbol_append), append(List(v1), List(value_reg)))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_36_d(ax, depth, k):
    GLOBALS['k_reg'] = make_cont(b_cont_35_d, value_reg, k)
    GLOBALS['depth_reg'] = depth
    GLOBALS['ax_reg'] = cdr_hat(ax)
    GLOBALS['pc'] = qq_expand_cps

def b_cont_37_d(k):
    GLOBALS['value_reg'] = append(List(symbol_list_to_vector), List(value_reg))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_38_d(depth, k):
    GLOBALS['k_reg'] = make_cont(b_cont_37_d, k)
    GLOBALS['depth_reg'] = depth
    GLOBALS['ax_reg'] = value_reg
    GLOBALS['pc'] = qq_expand_cps

def b_cont_39_d(k):
    GLOBALS['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_40_d(ax, k):
    GLOBALS['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_41_d(k):
    GLOBALS['value_reg'] = append(List(symbol_List), List(value_reg))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_42_d(v1, k):
    GLOBALS['value_reg'] = append(List(symbol_List), List(append(List(symbol_append), append(List(v1), List(value_reg)))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_43_d(ax, depth, k):
    GLOBALS['k_reg'] = make_cont(b_cont_42_d, value_reg, k)
    GLOBALS['depth_reg'] = depth
    GLOBALS['ax_reg'] = cdr_hat(ax)
    GLOBALS['pc'] = qq_expand_cps

def b_cont_44_d(k):
    GLOBALS['value_reg'] = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(symbol_quasiquote))), List(value_reg)))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_45_d(ax, k):
    GLOBALS['value_reg'] = append(List(symbol_List), List(append(List(symbol_cons), append(List(append(List(symbol_quote), List(car_hat(ax)))), List(value_reg)))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont_46_d(proc, env, info, handler, fail, k2):
    GLOBALS['k2_reg'] = make_cont2(b_cont2_68_d, k2)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['info_reg'] = info
    GLOBALS['env2_reg'] = env
    GLOBALS['args_reg'] = List(value_reg)
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = apply_proc

def b_cont_47_d(handler, fail, k2):
    GLOBALS['k_reg'] = make_cont2(b_cont2_95_d, handler, k2)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = initial_contours(toplevel_env)
    GLOBALS['adatum_reg'] = value_reg
    GLOBALS['pc'] = aparse

def b_cont_48_d(args, handler, fail, k2):
    GLOBALS['k_reg'] = make_cont2(b_cont2_96_d, args, handler, k2)
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = initial_contours(cadr(args))
    GLOBALS['adatum_reg'] = value_reg
    GLOBALS['pc'] = aparse

def b_cont_49_d(handler, fail, k2):
    GLOBALS['k_reg'] = k2
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = initial_contours(toplevel_env)
    GLOBALS['adatum_reg'] = value_reg
    GLOBALS['pc'] = aparse

def b_cont_50_d(fail, k2):
    GLOBALS['value2_reg'] = fail
    GLOBALS['value1_reg'] = value_reg
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont_51_d(x, y, k):
    if true_q(value_reg):
        GLOBALS['k_reg'] = k
        GLOBALS['y_reg'] = cdr(y)
        GLOBALS['x_reg'] = cdr(x)
        GLOBALS['pc'] = equal_objects_q
    else:
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont

def b_cont_52_d(i, v1, v2, k):
    if true_q(value_reg):
        GLOBALS['k_reg'] = k
        GLOBALS['i_reg'] = (i) - (1)
        GLOBALS['v2_reg'] = v2
        GLOBALS['v1_reg'] = v1
        GLOBALS['pc'] = equal_vectors_q
    else:
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont

def b_cont_53_d(ls, x, y, info, handler, fail, k):
    if true_q(value_reg):
        GLOBALS['value2_reg'] = fail
        GLOBALS['value1_reg'] = y
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['info_reg'] = info
        GLOBALS['ls_reg'] = ls
        GLOBALS['y_reg'] = cdr(y)
        GLOBALS['x_reg'] = x
        GLOBALS['pc'] = member_loop

def b_cont_54_d(pattern, var, k):
    if true_q(value_reg):
        GLOBALS['value_reg'] = True
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['pattern_reg'] = cdr(pattern)
        GLOBALS['var_reg'] = var
        GLOBALS['pc'] = occurs_q

def b_cont_55_d(ap2, p1, p2, k):
    if true_q(value_reg):
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['value_reg'] = make_sub(symbol_unit, p1, p2, ap2)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont

def b_cont_56_d(s_car, k):
    if true_q(not(value_reg)):
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['value_reg'] = make_sub(symbol_composite, s_car, value_reg)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont

def b_cont_57_d(apair1, apair2, pair1, pair2, k):
    if true_q(not(value_reg)):
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_123_d, apair2, pair2, value_reg, k)
        GLOBALS['ap_reg'] = cdr_hat(apair1)
        GLOBALS['s_reg'] = value_reg
        GLOBALS['pattern_reg'] = cdr(pair1)
        GLOBALS['pc'] = instantiate_hat

def apply_cont2():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont2_1_d(token, k):
    GLOBALS['value1_reg'] = cons(token, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_2_d():
    GLOBALS['final_reg'] = value1_reg
    GLOBALS['pc'] = pc_halt_signal

def b_cont2_3_d(k):
    GLOBALS['value1_reg'] = binding_value(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_4_d(k):
    GLOBALS['value1_reg'] = dlr_env_lookup(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_5_d(v1, info, k):
    GLOBALS['value1_reg'] = app_aexp(v1, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_6_d(adatum, senv, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_5_d, value1_reg, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_list_reg'] = cdr_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont2_7_d(info, k):
    GLOBALS['value1_reg'] = raise_aexp(value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_8_d(info, k):
    GLOBALS['value1_reg'] = choose_aexp(value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_9_d(name, formals, info, k):
    if true_q((list_q(formals)) and (not(association_q(formals)))):
        GLOBALS['value1_reg'] = trace_lambda_aexp(name, formals, value1_reg, info)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['value1_reg'] = mu_trace_lambda_aexp(name, head(formals), last(formals), value1_reg, info)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2

def b_cont2_10_d(body, info, k):
    GLOBALS['value1_reg'] = try_finally_aexp(body, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_11_d(adatum, senv, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_10_d, value1_reg, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_list_reg'] = try_finally_exps_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont2_12_d(cexps, cvar, body, info, k):
    GLOBALS['value1_reg'] = try_catch_finally_aexp(body, cvar, cexps, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_13_d(adatum, cvar, senv, body, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_12_d, value1_reg, cvar, body, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_list_reg'] = try_catch_finally_exps_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont2_14_d(adatum, senv, info, handler, k):
    cvar = symbol_undefined
    cvar = catch_var_hat(adatum)
    GLOBALS['k_reg'] = make_cont2(b_cont2_13_d, adatum, cvar, senv, value1_reg, info, handler, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = cons(List(cvar), senv)
    GLOBALS['adatum_list_reg'] = catch_exps_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont2_15_d(cvar, body, info, k):
    GLOBALS['value1_reg'] = try_catch_aexp(body, cvar, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_16_d(adatum, senv, info, handler, k):
    cvar = symbol_undefined
    cvar = catch_var_hat(adatum)
    GLOBALS['k_reg'] = make_cont2(b_cont2_15_d, cvar, value1_reg, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = cons(List(cvar), senv)
    GLOBALS['adatum_list_reg'] = catch_exps_hat(adatum)
    GLOBALS['pc'] = aparse_all

def b_cont2_17_d(info, k):
    GLOBALS['value1_reg'] = begin_aexp(value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_18_d(formals, info, k):
    if true_q((list_q(formals)) and (not(association_q(formals)))):
        GLOBALS['value1_reg'] = lambda_aexp(formals, value1_reg, info)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['value1_reg'] = mu_lambda_aexp(head(formals), last(formals), value1_reg, info)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2

def b_cont2_19_d(name, info, k):
    GLOBALS['value1_reg'] = define_tests_aexp(name, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_20_d(k):
    GLOBALS['value1_reg'] = run_tests_aexp(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_21_d(adatum, info, k):
    GLOBALS['value1_reg'] = define_b_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_22_d(adatum, info, k):
    GLOBALS['value1_reg'] = define_b_aexp(define_var_hat(adatum), "", value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_23_d(name, info, k):
    GLOBALS['value1_reg'] = define_syntax_transformer_aexp(name, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_24_d(info, k):
    GLOBALS['value1_reg'] = callback_aexp(value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_25_d(adatum, info, k):
    GLOBALS['value1_reg'] = define_aexp(define_var_hat(adatum), define_docstring_hat(adatum), value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_26_d(adatum, info, k):
    GLOBALS['value1_reg'] = define_aexp(define_var_hat(adatum), "", value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_27_d(adatum, info, k):
    var_info = symbol_undefined
    var_info = get_source_info(cadr_hat(adatum))
    GLOBALS['value1_reg'] = association_aexp(untag_atom_hat(car_hat(adatum)), value1_reg, var_info, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_28_d(info, k):
    GLOBALS['value1_reg'] = func_aexp(value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_29_d(adatum, info, k):
    var_info = symbol_undefined
    var_info = get_source_info(cadr_hat(adatum))
    GLOBALS['value1_reg'] = assign_aexp(untag_atom_hat(cadr_hat(adatum)), value1_reg, var_info, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_30_d(v1, info, k):
    GLOBALS['value1_reg'] = if_aexp(v1, value1_reg, lit_aexp(False, symbol_none), info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_31_d(adatum, senv, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_30_d, value1_reg, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = caddr_hat(adatum)
    GLOBALS['pc'] = aparse

def b_cont2_32_d(v1, v2, info, k):
    GLOBALS['value1_reg'] = if_aexp(v1, v2, value1_reg, info)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_33_d(adatum, senv, v1, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_32_d, v1, value1_reg, info, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = cadddr_hat(adatum)
    GLOBALS['pc'] = aparse

def b_cont2_34_d(adatum, senv, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_33_d, adatum, senv, value1_reg, info, handler, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = caddr_hat(adatum)
    GLOBALS['pc'] = aparse

def b_cont2_35_d(senv, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = value1_reg
    GLOBALS['pc'] = aparse

def b_cont2_36_d(args, k):
    GLOBALS['k_reg'] = make_cont(b_cont_21_d, value1_reg, value2_reg, k)
    GLOBALS['x_reg'] = car_hat(args)
    GLOBALS['pc'] = unannotate_cps

def b_cont2_37_d(args, k):
    GLOBALS['value1_reg'] = cons(List(untag_atom_hat(car_hat(args))), value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_38_d(a, k):
    GLOBALS['value1_reg'] = cons(a, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_39_d(adatum_list, senv, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_38_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_list_reg'] = cdr_hat(adatum_list)
    GLOBALS['pc'] = aparse_all

def b_cont2_40_d(v1, k):
    GLOBALS['value1_reg'] = cons(v1, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_41_d(senv, src, tokens_left, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['src_reg'] = src
    GLOBALS['tokens_reg'] = tokens_left
    GLOBALS['pc'] = aparse_sexps

def b_cont2_42_d(formals, handler, fail, k):
    if true_q(null_q(value1_reg)):
        GLOBALS['value_reg'] = append(List(symbol_lambda_no_defines), append(List(formals), at_hat(value2_reg)))
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        return create_letrec_bindings_hat(value1_reg, handler, fail, make_cont(b_cont_23_d, value2_reg, formals, k))

def b_cont2_43_d(name, formals, handler, fail, k):
    if true_q(null_q(value1_reg)):
        GLOBALS['value_reg'] = append(List(symbol_trace_lambda_no_defines), append(List(name), append(List(formals), at_hat(value2_reg))))
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        return create_letrec_bindings_hat(value1_reg, handler, fail, make_cont(b_cont_24_d, value2_reg, name, formals, k))

def b_cont2_44_d(bodies, k):
    GLOBALS['value1_reg'] = cons(car_hat(bodies), value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_45_d(bindings, k):
    GLOBALS['value_reg'] = cons(append(List(value1_reg), List(value2_reg)), bindings)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont2_46_d(bodies, k):
    GLOBALS['value_reg'] = append(List(symbol_let), append(List(value1_reg), append(value2_reg, at_hat(bodies))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont2_47_d(procs, vars, k2):
    GLOBALS['value2_reg'] = cons(append(List(symbol_set_b), append(List(car_hat(vars)), List(car_hat(procs)))), value2_reg)
    GLOBALS['value1_reg'] = cons(append(List(car_hat(vars)), List(append(List(symbol_quote), List(symbol_undefined)))), value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_48_d(exp, k):
    GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_r), List(exp))), value1_reg)), List(append(List(symbol_cond), value2_reg))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont2_49_d(clauses, var, k2):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if true_q(eq_q_hat(car_hat(clause), symbol_else)):
        GLOBALS['value2_reg'] = cons(List(symbol_else, List(symbol_else_code)), value2_reg)
        GLOBALS['value1_reg'] = cons(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(symbol_q_hat(car_hat(clause))):
            name = symbol_undefined
            name = car_hat(clause)
            GLOBALS['value2_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), List(List(name))), value2_reg)
            GLOBALS['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
            GLOBALS['k_reg'] = k2
            GLOBALS['pc'] = apply_cont2
        else:
            name = symbol_undefined
            name = caar_hat(clause)
            GLOBALS['value2_reg'] = cons(append(List(append(List(symbol_memq), append(List(var), List(append(List(symbol_quote), List(car_hat(clause))))))), List(List(name))), value2_reg)
            GLOBALS['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
            GLOBALS['k_reg'] = k2
            GLOBALS['pc'] = apply_cont2

def b_cont2_50_d(clauses, var, k2):
    clause = symbol_undefined
    clause = car_hat(clauses)
    if true_q(eq_q_hat(car_hat(clause), symbol_else)):
        GLOBALS['value2_reg'] = cons(append(List(symbol_else), List(List(symbol_else_code))), value2_reg)
        GLOBALS['value1_reg'] = cons(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), at_hat(cdr_hat(clause)))))), value1_reg)
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(symbol_q_hat(car_hat(clause))):
            name = symbol_undefined
            name = car_hat(clause)
            GLOBALS['value2_reg'] = cons(append(List(append(List(symbol_eq_q), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(name), List(append(List(symbol_cdr), List(var))))))), value2_reg)
            GLOBALS['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg)
            GLOBALS['k_reg'] = k2
            GLOBALS['pc'] = apply_cont2
        else:
            name = symbol_undefined
            name = caar_hat(clause)
            GLOBALS['value2_reg'] = cons(append(List(append(List(symbol_memq), append(List(append(List(symbol_car), List(var))), List(append(List(symbol_quote), List(car_hat(clause))))))), List(append(List(symbol_Apply), append(List(name), List(append(List(symbol_cdr), List(var))))))), value2_reg)
            GLOBALS['value1_reg'] = cons(append(List(name), List(append(List(symbol_lambda), append(List(cadr_hat(clause)), at_hat(cddr_hat(clause)))))), value1_reg)
            GLOBALS['k_reg'] = k2
            GLOBALS['pc'] = apply_cont2

def b_cont2_51_d(type_tester_name, k):
    tester_def = symbol_undefined
    tester_def = append(List(symbol_define), append(List(type_tester_name), List(append(List(symbol_lambda), append(List(List(symbol_x)), List(append(List(symbol_and), append(List(append(List(symbol_pair_q), List(symbol_x))), List(append(List(symbol_not), List(append(List(symbol_not), List(append(List(symbol_memq), append(List(append(List(symbol_car), List(symbol_x))), List(append(List(symbol_quote), List(value1_reg))))))))))))))))))
    GLOBALS['value_reg'] = append(List(symbol_begin), append(List(tester_def), value2_reg))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont2_52_d(def_, name, k2):
    GLOBALS['value2_reg'] = cons(def_, value2_reg)
    GLOBALS['value1_reg'] = cons(name, value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_53_d(variants, k2):
    GLOBALS['k2_reg'] = make_cont2(b_cont2_52_d, value2_reg, value1_reg, k2)
    GLOBALS['variants_reg'] = cdr_hat(variants)
    GLOBALS['pc'] = make_dd_variant_constructors_hat

def b_cont2_54_d(exp, type_name, type_tester_name, k):
    GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_r), List(exp))), value1_reg)), List(append(List(symbol_if), append(List(append(List(symbol_not), List(append(List(type_tester_name), List(symbol_r))))), append(List(append(List(symbol_error), append(List(append(List(symbol_quote), List(symbol_cases))), append(List("~a is not a valid ~a"), append(List(symbol_r), List(append(List(symbol_quote), List(type_name)))))))), List(append(List(symbol_cond), value2_reg))))))))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont

def b_cont2_55_d(macro_keyword, k):
    GLOBALS['value1_reg'] = replace_info(value1_reg, snoc(macro_keyword, get_source_info(value1_reg)))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_56_d(fail, k):
    GLOBALS['value1_reg'] = value2_reg
    GLOBALS['value2_reg'] = fail
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_57_d():
    GLOBALS['_starlast_fail_star'] = value2_reg
    GLOBALS['final_reg'] = value1_reg
    GLOBALS['pc'] = pc_halt_signal

def b_cont2_58_d():
    GLOBALS['k_reg'] = REP_k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = REP_handler
    GLOBALS['env_reg'] = toplevel_env
    GLOBALS['exp_reg'] = value1_reg
    GLOBALS['pc'] = m

def b_cont2_59_d():
    GLOBALS['final_reg'] = True
    GLOBALS['pc'] = pc_halt_signal

def b_cont2_60_d():
    GLOBALS['k_reg'] = make_cont2(b_cont2_59_d)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = try_parse_handler
    GLOBALS['senv_reg'] = initial_contours(toplevel_env)
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['tokens_reg'] = value1_reg
    GLOBALS['pc'] = aparse_sexps

def b_cont2_61_d(exp, k):
    handle_debug_info(exp, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_62_d(exp, k):
    pop_stack_trace_b(exp)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_63_d(args, exp, env, info, handler, k):
    if true_q(_staruse_stack_trace_star):
        push_stack_trace_b(exp)
    if true_q(dlr_proc_q(value1_reg)):
        result = symbol_undefined
        result = dlr_apply(value1_reg, args)
        if true_q(_staruse_stack_trace_star):
            pop_stack_trace_b(exp)
        GLOBALS['value1_reg'] = result
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(procedure_object_q(value1_reg)):
            if true_q(_staruse_stack_trace_star):
                GLOBALS['k2_reg'] = make_cont2(b_cont2_62_d, exp, k)
                GLOBALS['fail_reg'] = value2_reg
                GLOBALS['handler_reg'] = handler
                GLOBALS['info_reg'] = info
                GLOBALS['env2_reg'] = env
                GLOBALS['args_reg'] = args
                GLOBALS['proc_reg'] = value1_reg
                GLOBALS['pc'] = apply_proc
            else:
                GLOBALS['k2_reg'] = k
                GLOBALS['fail_reg'] = value2_reg
                GLOBALS['handler_reg'] = handler
                GLOBALS['info_reg'] = info
                GLOBALS['env2_reg'] = env
                GLOBALS['args_reg'] = args
                GLOBALS['proc_reg'] = value1_reg
                GLOBALS['pc'] = apply_proc
        else:
            GLOBALS['fail_reg'] = value2_reg
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['msg_reg'] = format("attempt to apply non-procedure '~a'", value1_reg)
            GLOBALS['pc'] = runtime_error

def b_cont2_64_d(exp, operator, env, info, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_63_d, value1_reg, exp, env, info, handler, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exp_reg'] = operator
    GLOBALS['pc'] = m

def b_cont2_65_d(v, k):
    GLOBALS['value1_reg'] = v
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_66_d(fexps, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_65_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exps_reg'] = fexps
    GLOBALS['pc'] = eval_sequence

def b_cont2_67_d(handler):
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['exception_reg'] = value1_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['pc'] = apply_handler2

def b_cont2_68_d(k2):
    GLOBALS['value_reg'] = value1_reg
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont

def b_cont2_69_d(macro_transformer, k):
    set_binding_value_b(value1_reg, macro_transformer)
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_70_d(name, env, info, handler, k):
    macro_transformer = symbol_undefined
    macro_transformer = make_macro(b_macro_14_d, value1_reg, env, info)
    GLOBALS['k_reg'] = make_cont2(b_cont2_69_d, macro_transformer, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = macro_env
    GLOBALS['var_reg'] = name
    GLOBALS['pc'] = lookup_binding_in_first_frame

def b_cont2_71_d(docstring, var, k):
    if true_q(procedure_object_q(value1_reg)):
        set_global_value_b(var, dlr_func(value1_reg))
    else:
        set_global_value_b(var, value1_reg)
    set_global_docstring_b(var, docstring)
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_72_d(aclauses, clauses, k):
    set_binding_value_b(value1_reg, make_pattern_macro_hat(clauses, aclauses))
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_73_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = binding_value(value1_reg)
    set_binding_value_b(value1_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_2_d, value1_reg, old_value, value2_reg)
    GLOBALS['value2_reg'] = new_fail
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_74_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = dlr_env_lookup(value1_reg)
    set_global_value_b(value1_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_3_d, old_value, value1_reg, value2_reg)
    GLOBALS['value2_reg'] = new_fail
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_75_d(var, var_info, env, handler, k):
    GLOBALS['sk_reg'] = make_cont2(b_cont2_73_d, value1_reg, k)
    GLOBALS['dk_reg'] = make_cont3(b_cont3_4_d, value1_reg, k)
    GLOBALS['gk_reg'] = make_cont2(b_cont2_74_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['var_info_reg'] = var_info
    GLOBALS['env_reg'] = env
    GLOBALS['var_reg'] = var
    GLOBALS['pc'] = lookup_variable

def b_cont2_76_d(docstring, rhs_value, k):
    set_binding_value_b(value1_reg, rhs_value)
    set_binding_docstring_b(value1_reg, docstring)
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_77_d(docstring, var, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_76_d, docstring, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['var_reg'] = var
    GLOBALS['pc'] = lookup_binding_in_first_frame

def b_cont2_78_d(k):
    GLOBALS['value1_reg'] = binding_docstring(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_79_d(k):
    GLOBALS['value1_reg'] = help(dlr_env_lookup(value1_reg))
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_80_d(var, k):
    GLOBALS['value1_reg'] = association(var, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_81_d(k):
    GLOBALS['value1_reg'] = callback(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_82_d(else_exp, then_exp, env, handler, k):
    if true_q(value1_reg):
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = value2_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['env_reg'] = env
        GLOBALS['exp_reg'] = then_exp
        GLOBALS['pc'] = m
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = value2_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['env_reg'] = env
        GLOBALS['exp_reg'] = else_exp
        GLOBALS['pc'] = m

def b_cont2_83_d(k):
    GLOBALS['value1_reg'] = dlr_func(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_84_d(start_time, tests, handler, k):
    right2 = symbol_undefined
    wrong2 = symbol_undefined
    wrong2 = cadr(value1_reg)
    right2 = car(value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['wrong_reg'] = wrong2
    GLOBALS['right_reg'] = right2
    GLOBALS['start_time_reg'] = start_time
    GLOBALS['tests_reg'] = cdr(tests)
    GLOBALS['pc'] = run_unit_tests

def b_cont2_85_d(right, test_name, wrong, env, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['wrong_reg'] = wrong
    GLOBALS['right_reg'] = right
    GLOBALS['verbose_reg'] = True
    GLOBALS['assertions_reg'] = value1_reg
    GLOBALS['test_name_reg'] = test_name
    GLOBALS['pc'] = run_unit_test_cases

def b_cont2_86_d(matched_exps, k):
    GLOBALS['value1_reg'] = append(matched_exps, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_87_d(assertions, nums, test_name, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_86_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['assertions_reg'] = cdr(assertions)
    GLOBALS['nums_reg'] = cdr(nums)
    GLOBALS['test_name_reg'] = test_name
    GLOBALS['pc'] = filter_assertions

def b_cont2_88_d(assertions, e, proc_exp, right, test_exp, test_name, verbose, wrong, env, handler, k):
    if true_q(verbose):
        printf("~a\n", get_traceback_string(List(symbol_exception, e)))
        printf("  Procedure: ~a\n", aunparse(proc_exp))
        printf("           : ~a\n", aunparse(test_exp))
        printf("           : ~a\n", value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['wrong_reg'] = wrong
    GLOBALS['right_reg'] = (right) + (1)
    GLOBALS['verbose_reg'] = verbose
    GLOBALS['assertions_reg'] = cdr(assertions)
    GLOBALS['test_name_reg'] = test_name
    GLOBALS['pc'] = run_unit_test_cases

def b_cont2_89_d(assertions, right, test_name, verbose, wrong, env, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['wrong_reg'] = wrong
    GLOBALS['right_reg'] = (right) + (1)
    GLOBALS['verbose_reg'] = verbose
    GLOBALS['assertions_reg'] = cdr(assertions)
    GLOBALS['test_name_reg'] = test_name
    GLOBALS['pc'] = run_unit_test_cases

def b_cont2_90_d(exps, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exps_reg'] = cdr(exps)
    GLOBALS['pc'] = m_star

def b_cont2_91_d(exps, env, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exps_reg'] = cdr(exps)
    GLOBALS['pc'] = eval_sequence

def b_cont2_92_d(e, handler):
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['exception_reg'] = e
    GLOBALS['handler_reg'] = handler
    GLOBALS['pc'] = apply_handler2

def b_cont2_93_d(trace_depth, k2):
    GLOBALS['trace_depth'] = (trace_depth) - (1)
    printf("~areturn: ~s~%", make_trace_depth_string(trace_depth), value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_94_d(items, sep, k2):
    GLOBALS['value1_reg'] = string_append(format("~a", car(items)), sep, value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_95_d(handler, k2):
    GLOBALS['k_reg'] = k2
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = toplevel_env
    GLOBALS['exp_reg'] = value1_reg
    GLOBALS['pc'] = m

def b_cont2_96_d(args, handler, k2):
    GLOBALS['k_reg'] = k2
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = cadr(args)
    GLOBALS['exp_reg'] = value1_reg
    GLOBALS['pc'] = m

def b_cont2_97_d(handler, k2):
    GLOBALS['k_reg'] = make_cont4(b_cont4_11_d, handler, k2)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['tokens_reg'] = value1_reg
    GLOBALS['pc'] = read_sexp

def b_cont2_98_d(handler, k2):
    GLOBALS['k_reg'] = make_cont4(b_cont4_12_d, handler, k2)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['tokens_reg'] = value1_reg
    GLOBALS['pc'] = read_sexp

def b_cont2_99_d(k):
    if true_q(null_q(load_stack)):
        printf("WARNING: empty load-stack encountered!\n")
    else:
        GLOBALS['load_stack'] = cdr(load_stack)
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_100_d(filename, env2, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_99_d, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env2_reg'] = env2
    GLOBALS['src_reg'] = filename
    GLOBALS['tokens_reg'] = value1_reg
    GLOBALS['pc'] = read_and_eval_asexps

def b_cont2_101_d(src, tokens_left, env2, handler, k):
    if true_q(token_type_q(first(tokens_left), symbol_end_marker)):
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = k
        GLOBALS['fail_reg'] = value2_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['env2_reg'] = env2
        GLOBALS['src_reg'] = src
        GLOBALS['tokens_reg'] = tokens_left
        GLOBALS['pc'] = read_and_eval_asexps

def b_cont2_102_d(src, tokens_left, env2, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_101_d, src, tokens_left, env2, handler, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env2
    GLOBALS['exp_reg'] = value1_reg
    GLOBALS['pc'] = m

def b_cont2_103_d(filenames, env2, info, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['info_reg'] = info
    GLOBALS['env2_reg'] = env2
    GLOBALS['filenames_reg'] = cdr(filenames)
    GLOBALS['pc'] = load_files

def b_cont2_104_d(args, info, handler, k2):
    if true_q((value1_reg) is (True)):
        GLOBALS['value1_reg'] = symbol_ok
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(numeric_equal(length(args), 3)):
            GLOBALS['fail_reg'] = value2_reg
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['msg_reg'] = ""
            GLOBALS['pc'] = assertion_error
        else:
            GLOBALS['fail_reg'] = value2_reg
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['msg_reg'] = cadddr(args)
            GLOBALS['pc'] = assertion_error

def b_cont2_105_d(lst, k2):
    if true_q(member(car(lst), value1_reg)):
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['value1_reg'] = cons(car(lst), value1_reg)
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2

def b_cont2_106_d(filename, handler, k2):
    module = symbol_undefined
    module = make_toplevel_env()
    set_binding_value_b(value1_reg, module)
    GLOBALS['k_reg'] = k2
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['env2_reg'] = module
    GLOBALS['filename_reg'] = filename
    GLOBALS['paths_reg'] = SCHEMEPATH
    GLOBALS['pc'] = find_file_and_load

def b_cont2_107_d(args, sym, info, handler, k):
    if true_q(null_q(cdr(args))):
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(not(environment_q(value1_reg))):
            GLOBALS['fail_reg'] = value2_reg
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['msg_reg'] = format("invalid module '~a'", sym)
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['k_reg'] = k
            GLOBALS['fail_reg'] = value2_reg
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['env_reg'] = value1_reg
            GLOBALS['args_reg'] = cdr(args)
            GLOBALS['pc'] = get_primitive

def b_cont2_108_d(ls1, k2):
    GLOBALS['value1_reg'] = cons(car(ls1), value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_109_d(lists, k2):
    GLOBALS['k2_reg'] = k2
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['ls2_reg'] = value1_reg
    GLOBALS['ls1_reg'] = car(lists)
    GLOBALS['pc'] = append2

def b_cont2_110_d(iterator, proc, env, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['iterator_reg'] = iterator
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = iterate_continue

def b_cont2_111_d(iterator, proc, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['iterator_reg'] = iterator
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = iterate_collect_continue

def b_cont2_112_d(list1, proc, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['list1_reg'] = cdr(list1)
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = map1

def b_cont2_113_d(list1, proc, k):
    GLOBALS['value1_reg'] = cons(dlr_apply(proc, List(car(list1))), value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_114_d(list1, list2, proc, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['list2_reg'] = cdr(list2)
    GLOBALS['list1_reg'] = cdr(list1)
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = map2

def b_cont2_115_d(list1, list2, proc, k):
    GLOBALS['value1_reg'] = cons(dlr_apply(proc, List(car(list1), car(list2))), value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_116_d(lists, proc, env, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_40_d, value1_reg, k)
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['lists_reg'] = Map(cdr, lists)
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = mapN

def b_cont2_117_d(lists, proc, k):
    GLOBALS['value1_reg'] = cons(dlr_apply(proc, Map(car, lists)), value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont2_118_d(arg_list, proc, env, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value2_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['lists_reg'] = Map(cdr, arg_list)
    GLOBALS['proc_reg'] = proc
    GLOBALS['pc'] = for_each_primitive

def b_cont2_119_d(k2):
    GLOBALS['value1_reg'] = apply_native(dict, List(value1_reg))
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_120_d(args, k2):
    GLOBALS['value1_reg'] = cons(car(args), value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_121_d(args, k2):
    GLOBALS['value1_reg'] = cons(List(caar(args), caddar(args)), value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_122_d(new_acdr1, new_cdr1, s_car, k):
    GLOBALS['k_reg'] = make_cont(b_cont_56_d, s_car, k)
    GLOBALS['ap2_reg'] = value2_reg
    GLOBALS['ap1_reg'] = new_acdr1
    GLOBALS['p2_reg'] = value1_reg
    GLOBALS['p1_reg'] = new_cdr1
    GLOBALS['pc'] = unify_patterns_hat

def b_cont2_123_d(apair2, pair2, s_car, k):
    GLOBALS['k2_reg'] = make_cont2(b_cont2_122_d, value2_reg, value1_reg, s_car, k)
    GLOBALS['ap_reg'] = cdr_hat(apair2)
    GLOBALS['s_reg'] = s_car
    GLOBALS['pattern_reg'] = cdr(pair2)
    GLOBALS['pc'] = instantiate_hat

def b_cont2_124_d(a, aa, ap, k2):
    GLOBALS['value2_reg'] = cons_hat(aa, value2_reg, get_source_info(ap))
    GLOBALS['value1_reg'] = cons(a, value1_reg)
    GLOBALS['k_reg'] = k2
    GLOBALS['pc'] = apply_cont2

def b_cont2_125_d(ap, pattern, s, k2):
    GLOBALS['k2_reg'] = make_cont2(b_cont2_124_d, value1_reg, value2_reg, ap, k2)
    GLOBALS['ap_reg'] = cdr_hat(ap)
    GLOBALS['s_reg'] = s
    GLOBALS['pattern_reg'] = cdr(pattern)
    GLOBALS['pc'] = instantiate_hat

def b_cont2_126_d(s2, k2):
    GLOBALS['k2_reg'] = k2
    GLOBALS['ap_reg'] = value2_reg
    GLOBALS['s_reg'] = s2
    GLOBALS['pattern_reg'] = value1_reg
    GLOBALS['pc'] = instantiate_hat

def apply_cont3():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont3_1_d(src, handler, k):
    if true_q(token_type_q(value1_reg, symbol_end_marker)):
        GLOBALS['value2_reg'] = value3_reg
        GLOBALS['value1_reg'] = List(value1_reg)
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_1_d, value1_reg, k)
        GLOBALS['fail_reg'] = value3_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['src_reg'] = src
        GLOBALS['chars_reg'] = value2_reg
        GLOBALS['pc'] = scan_input_loop

def b_cont3_2_d():
    GLOBALS['final_reg'] = value1_reg
    GLOBALS['pc'] = pc_halt_signal

def b_cont3_3_d(k):
    GLOBALS['value1_reg'] = get_external_member(value1_reg, value2_reg)
    GLOBALS['value2_reg'] = value3_reg
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont3_4_d(rhs_value, k):
    old_value = symbol_undefined
    old_value = get_external_member(value1_reg, value2_reg)
    set_external_member_b(value1_reg, value2_reg, rhs_value)
    new_fail = symbol_undefined
    new_fail = make_fail(b_fail_4_d, value2_reg, value1_reg, old_value, value3_reg)
    GLOBALS['value2_reg'] = new_fail
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_cont3_5_d(k):
    GLOBALS['value1_reg'] = help(get_external_member(value1_reg, value2_reg))
    GLOBALS['value2_reg'] = value3_reg
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def apply_cont4():
    Apply(cadr(k_reg), cddr(k_reg))

def b_cont4_1_d(src, start, k):
    GLOBALS['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    GLOBALS['info_reg'] = make_info(src, start, value2_reg)
    GLOBALS['x_reg'] = value1_reg
    GLOBALS['pc'] = annotate_cps

def b_cont4_2_d(src, start, k):
    GLOBALS['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    GLOBALS['info_reg'] = make_info(src, start, value2_reg)
    GLOBALS['x_reg'] = list_to_vector(value1_reg)
    GLOBALS['pc'] = annotate_cps

def b_cont4_3_d(src, start, v, k):
    GLOBALS['k_reg'] = make_cont(b_cont_8_d, value2_reg, value3_reg, value4_reg, k)
    GLOBALS['info_reg'] = make_info(src, start, value2_reg)
    GLOBALS['x_reg'] = List(v, value1_reg)
    GLOBALS['pc'] = annotate_cps

def b_cont4_4_d(sexp1, k):
    GLOBALS['value1_reg'] = cons(sexp1, value1_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont4

def b_cont4_5_d(src, handler, k):
    GLOBALS['k_reg'] = make_cont4(b_cont4_4_d, value1_reg, k)
    GLOBALS['fail_reg'] = value4_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['src_reg'] = src
    GLOBALS['tokens_reg'] = value3_reg
    GLOBALS['pc'] = read_vector_sequence

def b_cont4_6_d(expected_terminator, sexp1, src, handler, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = value4_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['src_reg'] = src
    GLOBALS['expected_terminator_reg'] = expected_terminator
    GLOBALS['tokens_reg'] = value3_reg
    GLOBALS['sexps_reg'] = cons(sexp1, value1_reg)
    GLOBALS['pc'] = close_sexp_sequence

def b_cont4_7_d(expected_terminator, src, handler, k):
    if true_q(token_type_q(first(value3_reg), symbol_dot)):
        GLOBALS['k_reg'] = make_cont4(b_cont4_6_d, expected_terminator, value1_reg, src, handler, k)
        GLOBALS['fail_reg'] = value4_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['src_reg'] = src
        GLOBALS['tokens_reg'] = rest_of(value3_reg)
        GLOBALS['pc'] = read_sexp
    else:
        GLOBALS['k_reg'] = make_cont4(b_cont4_4_d, value1_reg, k)
        GLOBALS['fail_reg'] = value4_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['src_reg'] = src
        GLOBALS['expected_terminator_reg'] = expected_terminator
        GLOBALS['tokens_reg'] = value3_reg
        GLOBALS['pc'] = read_sexp_sequence

def b_cont4_8_d():
    GLOBALS['final_reg'] = value1_reg
    GLOBALS['pc'] = pc_halt_signal

def b_cont4_9_d(senv, src, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_41_d, senv, src, value3_reg, handler, k)
    GLOBALS['fail_reg'] = value4_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = senv
    GLOBALS['adatum_reg'] = value1_reg
    GLOBALS['pc'] = aparse

def b_cont4_10_d():
    GLOBALS['_startokens_left_star'] = value3_reg
    GLOBALS['k_reg'] = make_cont2(b_cont2_58_d)
    GLOBALS['fail_reg'] = value4_reg
    GLOBALS['handler_reg'] = REP_handler
    GLOBALS['senv_reg'] = initial_contours(toplevel_env)
    GLOBALS['adatum_reg'] = value1_reg
    GLOBALS['pc'] = aparse

def b_cont4_11_d(handler, k2):
    if true_q(token_type_q(first(value3_reg), symbol_end_marker)):
        GLOBALS['k_reg'] = k2
        GLOBALS['fail_reg'] = value4_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['senv_reg'] = initial_contours(toplevel_env)
        GLOBALS['adatum_reg'] = value1_reg
        GLOBALS['pc'] = aparse
    else:
        GLOBALS['fail_reg'] = value4_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['src_reg'] = symbol_stdin
        GLOBALS['tokens_reg'] = value3_reg
        GLOBALS['msg_reg'] = "tokens left over"
        GLOBALS['pc'] = read_error

def b_cont4_12_d(handler, k2):
    if true_q(token_type_q(first(value3_reg), symbol_end_marker)):
        GLOBALS['value2_reg'] = value4_reg
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['fail_reg'] = value4_reg
        GLOBALS['handler_reg'] = handler
        GLOBALS['src_reg'] = symbol_stdin
        GLOBALS['tokens_reg'] = value3_reg
        GLOBALS['msg_reg'] = "tokens left over"
        GLOBALS['pc'] = read_error

def b_cont4_13_d(src, env2, handler, k):
    GLOBALS['k_reg'] = make_cont2(b_cont2_102_d, src, value3_reg, env2, handler, k)
    GLOBALS['fail_reg'] = value4_reg
    GLOBALS['handler_reg'] = handler
    GLOBALS['senv_reg'] = initial_contours(env2)
    GLOBALS['adatum_reg'] = value1_reg
    GLOBALS['pc'] = aparse

def apply_fail():
    Apply(cadr(fail_reg), cddr(fail_reg))

def b_fail_1_d():
    GLOBALS['final_reg'] = "no more choices"
    GLOBALS['pc'] = pc_halt_signal

def b_fail_2_d(binding, old_value, fail):
    set_binding_value_b(binding, old_value)
    GLOBALS['fail_reg'] = fail
    GLOBALS['pc'] = apply_fail

def b_fail_3_d(old_value, var, fail):
    set_global_value_b(var, old_value)
    GLOBALS['fail_reg'] = fail
    GLOBALS['pc'] = apply_fail

def b_fail_4_d(components, dlr_obj, old_value, fail):
    set_external_member_b(dlr_obj, components, old_value)
    GLOBALS['fail_reg'] = fail
    GLOBALS['pc'] = apply_fail

def b_fail_5_d(exps, env, handler, fail, k):
    GLOBALS['k_reg'] = k
    GLOBALS['fail_reg'] = fail
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exps_reg'] = cdr(exps)
    GLOBALS['pc'] = eval_choices

def apply_handler():
    Apply(cadr(handler_reg), cddr(handler_reg))

def b_handler_1_d():
    GLOBALS['final_reg'] = List(symbol_exception, exception_reg)
    GLOBALS['pc'] = pc_halt_signal

def apply_handler2():
    Apply(cadr(handler_reg), cddr(handler_reg))

def b_handler2_1_d():
    GLOBALS['final_reg'] = List(symbol_exception, exception_reg)
    GLOBALS['pc'] = pc_halt_signal

def b_handler2_2_d():
    GLOBALS['_starlast_fail_star'] = fail_reg
    GLOBALS['final_reg'] = List(symbol_exception, exception_reg)
    GLOBALS['pc'] = pc_halt_signal

def b_handler2_3_d():
    GLOBALS['final_reg'] = False
    GLOBALS['pc'] = pc_halt_signal

def b_handler2_4_d(assertions, right, test_name, verbose, wrong, env, handler, k):
    msg = symbol_undefined
    where = symbol_undefined
    where = get_exception_info(exception_reg)
    msg = get_exception_message(exception_reg)
    if true_q(GreaterThan(string_length(msg), 0)):
        if true_q((where) is (symbol_none)):
            printf("  Error: ~a \"~a\"\n", test_name, msg)
        else:
            printf("  Error: ~a \"~a\" at ~a\n", test_name, msg, where)
    else:
        if true_q((where) is (symbol_none)):
            printf("  Error: ~a\n", test_name)
        else:
            printf("  Error: ~a at ~a\n", test_name, where)
    assert_exp = symbol_undefined
    proc_exp = symbol_undefined
    test_exp = symbol_undefined
    result_exp = symbol_undefined
    assert_exp = car(assertions)
    proc_exp = car(cdr_hat(assert_exp))
    test_exp = cadr(cdr_hat(assert_exp))
    result_exp = caddr(cdr_hat(assert_exp))
    initialize_stack_trace_b()
    GLOBALS['k_reg'] = make_cont2(b_cont2_88_d, assertions, exception_reg, proc_exp, right, test_exp, test_name, verbose, wrong, env, handler, k)
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exp_reg'] = result_exp
    GLOBALS['pc'] = m

def b_handler2_5_d(cexps, cvar, env, handler, k):
    new_env = symbol_undefined
    new_env = extend(env, List(cvar), List(exception_reg), List("try-catch handler"))
    GLOBALS['k_reg'] = k
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = new_env
    GLOBALS['exps_reg'] = cexps
    GLOBALS['pc'] = eval_sequence

def b_handler2_6_d(fexps, env, handler):
    GLOBALS['k_reg'] = make_cont2(b_cont2_92_d, exception_reg, handler)
    GLOBALS['handler_reg'] = handler
    GLOBALS['env_reg'] = env
    GLOBALS['exps_reg'] = fexps
    GLOBALS['pc'] = eval_sequence

def b_handler2_7_d(cexps, cvar, fexps, env, handler, k):
    new_env = symbol_undefined
    new_env = extend(env, List(cvar), List(exception_reg), List("try-catch-finally handler"))
    catch_handler = symbol_undefined
    catch_handler = try_finally_handler(fexps, env, handler)
    GLOBALS['k_reg'] = make_cont2(b_cont2_66_d, fexps, env, handler, k)
    GLOBALS['handler_reg'] = catch_handler
    GLOBALS['env_reg'] = new_env
    GLOBALS['exps_reg'] = cexps
    GLOBALS['pc'] = eval_sequence

def apply_proc():
    Apply(cadr(proc_reg), cddr(proc_reg))

def b_proc_1_d(bodies, formals, env):
    formals_and_args = symbol_undefined
    new_formals = symbol_undefined
    new_args = symbol_undefined
    formals_and_args = process_formals_and_args(formals, args_reg, info_reg, handler_reg, fail_reg)
    new_formals = car(formals_and_args)
    new_args = cdr(formals_and_args)
    if true_q(numeric_equal(length(new_args), length(new_formals))):
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['env_reg'] = extend(env, new_formals, new_args, make_empty_docstrings(length(new_args)))
        GLOBALS['exps_reg'] = bodies
        GLOBALS['pc'] = eval_sequence
    else:
        GLOBALS['msg_reg'] = "incorrect number of arguments in application"
        GLOBALS['pc'] = runtime_error

def b_proc_2_d(bodies, formals, runt, env):
    new_formals = symbol_undefined
    new_args = symbol_undefined
    new_args = args_reg
    new_formals = formals
    if true_q(GreaterThanEqual(length(new_args), length(new_formals))):
        new_env = symbol_undefined
        new_env = extend(env, cons(runt, new_formals), cons(list_tail(new_args, length(new_formals)), list_head(new_args, length(new_formals))), make_empty_docstrings((1) + (length(new_formals))))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['env_reg'] = new_env
        GLOBALS['exps_reg'] = bodies
        GLOBALS['pc'] = eval_sequence
    else:
        GLOBALS['msg_reg'] = "not enough arguments in application"
        GLOBALS['pc'] = runtime_error

def b_proc_3_d(bodies, name, trace_depth, formals, env):
    formals_and_args = symbol_undefined
    new_formals = symbol_undefined
    new_args = symbol_undefined
    formals_and_args = process_formals_and_args(formals, args_reg, info_reg, handler_reg, fail_reg)
    new_formals = car(formals_and_args)
    new_args = cdr(formals_and_args)
    if true_q(numeric_equal(length(new_args), length(new_formals))):
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, new_args))
        GLOBALS['trace_depth'] = (trace_depth) + (1)
        GLOBALS['k_reg'] = make_cont2(b_cont2_93_d, trace_depth, k2_reg)
        GLOBALS['env_reg'] = extend(env, new_formals, new_args, make_empty_docstrings(length(new_formals)))
        GLOBALS['exps_reg'] = bodies
        GLOBALS['pc'] = eval_sequence
    else:
        GLOBALS['msg_reg'] = "incorrect number of arguments in application"
        GLOBALS['pc'] = runtime_error

def b_proc_4_d(bodies, name, trace_depth, formals, runt, env):
    new_formals = symbol_undefined
    new_args = symbol_undefined
    new_args = args_reg
    new_formals = formals
    if true_q(GreaterThanEqual(length(args_reg), length(new_formals))):
        new_env = symbol_undefined
        new_env = extend(env, cons(runt, new_formals), cons(list_tail(new_args, length(new_formals)), list_head(new_args, length(new_formals))), make_empty_docstrings((1) + (length(new_formals))))
        printf("~acall: ~s~%", make_trace_depth_string(trace_depth), cons(name, new_args))
        GLOBALS['trace_depth'] = (trace_depth) + (1)
        GLOBALS['k_reg'] = make_cont2(b_cont2_93_d, trace_depth, k2_reg)
        GLOBALS['env_reg'] = new_env
        GLOBALS['exps_reg'] = bodies
        GLOBALS['pc'] = eval_sequence
    else:
        GLOBALS['msg_reg'] = "not enough arguments in application"
        GLOBALS['pc'] = runtime_error

def b_proc_5_d():
    GLOBALS['unit_test_table'] = dict()
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_6_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_7_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = numeric_equal(car(args_reg), 0)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_8_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = python_eval(car(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_9_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = python_exec(car(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_10_d():
    GLOBALS['final_reg'] = end_of_session
    GLOBALS['pc'] = pc_halt_signal

def b_proc_11_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = expt_native(car(args_reg), cadr(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_12_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of args to string-join; should be two"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(string_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "first arg to string-join must be a string"
            GLOBALS['pc'] = runtime_error
        else:
            if true_q(not(list_q(cadr(args_reg)))):
                GLOBALS['msg_reg'] = "second arg to string-join must be a list"
                GLOBALS['pc'] = runtime_error
            else:
                GLOBALS['items_reg'] = cadr(args_reg)
                GLOBALS['sep_reg'] = car(args_reg)
                GLOBALS['pc'] = string_join

def b_proc_13_d():
    if true_q(length_one_q(args_reg)):
        GLOBALS['k_reg'] = make_cont(b_cont_47_d, handler_reg, fail_reg, k2_reg)
        GLOBALS['info_reg'] = symbol_none
        GLOBALS['x_reg'] = car(args_reg)
        GLOBALS['pc'] = annotate_cps
    else:
        if true_q(length_two_q(args_reg)):
            GLOBALS['k_reg'] = make_cont(b_cont_48_d, args_reg, handler_reg, fail_reg, k2_reg)
            GLOBALS['info_reg'] = symbol_none
            GLOBALS['x_reg'] = car(args_reg)
            GLOBALS['pc'] = annotate_cps
        else:
            GLOBALS['msg_reg'] = "incorrect number of arguments to eval"
            GLOBALS['pc'] = runtime_error

def b_proc_14_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to eval-ast"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(list_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "eval-ast called on non-abstract syntax tree argument"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['env_reg'] = toplevel_env
            GLOBALS['exp_reg'] = car(args_reg)
            GLOBALS['pc'] = m

def b_proc_15_d():
    GLOBALS['k_reg'] = make_cont(b_cont_49_d, handler_reg, fail_reg, k2_reg)
    GLOBALS['info_reg'] = symbol_none
    GLOBALS['x_reg'] = car(args_reg)
    GLOBALS['pc'] = annotate_cps

def b_proc_16_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string-length"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(string_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "string-length called on non-string argument"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(string_length, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_17_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string-ref"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(string_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "string-ref called with non-string first argument"
            GLOBALS['pc'] = runtime_error
        else:
            if true_q(not(number_q(cadr(args_reg)))):
                GLOBALS['msg_reg'] = "string-ref called with non-numberic second argument"
                GLOBALS['pc'] = runtime_error
            else:
                GLOBALS['value2_reg'] = fail_reg
                GLOBALS['value1_reg'] = Apply(string_ref, args_reg)
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2

def b_proc_18_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = aunparse(car(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_19_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = aunparse(car(caddr(car(args_reg))))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_20_d():
    GLOBALS['k_reg'] = make_cont2(b_cont2_97_d, handler_reg, k2_reg)
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['input_reg'] = car(args_reg)
    GLOBALS['pc'] = scan_input

def b_proc_21_d():
    GLOBALS['k_reg'] = make_cont2(b_cont2_98_d, handler_reg, k2_reg)
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['input_reg'] = car(args_reg)
    GLOBALS['pc'] = scan_input

def b_proc_22_d():
    proc = symbol_undefined
    proc_args = symbol_undefined
    proc_args = cadr(args_reg)
    proc = car(args_reg)
    if true_q(dlr_proc_q(proc)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = dlr_apply(proc, proc_args)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['args_reg'] = proc_args
        GLOBALS['proc_reg'] = proc
        GLOBALS['pc'] = apply_proc

def b_proc_23_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to sqrt"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(sqrt, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_24_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to odd?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = odd_q(car(args_reg))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_25_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to even?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = even_q(car(args_reg))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_26_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to quotient"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(member(0, cdr(args_reg))):
            GLOBALS['msg_reg'] = "division by zero"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(quotient, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_27_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to remainder"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(remainder, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_28_d():
    for_each(safe_print, args_reg)
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_29_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(string, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_30_d():
    if true_q(numeric_equal(length(args_reg), 3)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = substring(car(args_reg), cadr(args_reg), caddr(args_reg))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = substring(car(args_reg), cadr(args_reg), string_length(car(args_reg)))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_31_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = number_to_string(car(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_32_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = assv(car(args_reg), cadr(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_33_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = memv(car(args_reg), cadr(args_reg))
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_34_d():
    s = symbol_undefined
    s = format("~a", car(args_reg))
    GLOBALS['_starneed_newline_star'] = true_q(not(ends_with_newline_q(s)))
    display(s)
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_35_d():
    GLOBALS['_starneed_newline_star'] = False
    newline()
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_36_d():
    if true_q(not(length_at_least_q(1, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to load"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['env2_reg'] = toplevel_env
        GLOBALS['filenames_reg'] = args_reg
        GLOBALS['pc'] = load_files

def b_proc_37_d():
    if true_q(length_one_q(args_reg)):
        GLOBALS['ls_reg'] = car(args_reg)
        GLOBALS['sum_reg'] = 0
        GLOBALS['x_reg'] = car(args_reg)
        GLOBALS['pc'] = length_loop
    else:
        GLOBALS['msg_reg'] = "incorrect number of arguments to length"
        GLOBALS['pc'] = runtime_error

def b_proc_38_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = format("incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument", args_reg)
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(symbol_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_39_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to number?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(number_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_40_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to boolean?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(boolean_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_41_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(string_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_42_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(char_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_43_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char=?"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q((not(char_q(car(args_reg)))) or (not(char_q(cadr(args_reg))))):
            GLOBALS['msg_reg'] = "char=? requires arguments of type char"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(char_is__q, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_44_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char-whitespace?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(char_whitespace_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_45_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char->integer"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(char_to_integer, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_46_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to integer->char"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(integer_to_char, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_47_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char-alphabetic?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(char_alphabetic_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_48_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char-numeric?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(char_numeric_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_49_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to null?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(null_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_50_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to box?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(box_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_51_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to pair?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(pair_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_52_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to box"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(box, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_53_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to unbox"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(box_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("unbox called on non-box ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(unbox, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_54_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cons"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(cons, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_55_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to car"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("car called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(car, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_56_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_57_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(length_at_least_q(2, car(args_reg)))):
            GLOBALS['msg_reg'] = format("cadr called on incorrect list structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_58_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(length_at_least_q(3, car(args_reg)))):
            GLOBALS['msg_reg'] = format("caddr called on incorrect list structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_59_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caaaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caaaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caaaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_60_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caaadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caaadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caaadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_61_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_62_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caadar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caadar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caadar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_63_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caaddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caaddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caaddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_64_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_65_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_66_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cadaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cadaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cadaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_67_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cadadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cadadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cadadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_68_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cadar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cadar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cadar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_69_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to caddar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("caddar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(caddar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_70_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cadddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cadddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cadddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_71_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdaaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdaaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdaaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_72_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdaadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdaadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdaadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_73_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_74_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdadar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdadar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdadar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_75_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdaddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdaddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdaddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_76_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_77_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_78_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cddaar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cddaar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cddaar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_79_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cddadr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cddadr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cddadr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_80_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cddar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cddar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cddar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_81_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdddar"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdddar called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdddar, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_82_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cddddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cddddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cddddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_83_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cdddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cdddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cdddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_84_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to cddr"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("cddr called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(cddr, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_85_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = args_reg
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_86_d():
    if true_q(not((numeric_equal(length(args_reg), 3)) or (numeric_equal(length(args_reg), 4)))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to assert"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(procedure_object_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "assertion predicate is not a procedure"
            GLOBALS['pc'] = runtime_error
        else:
            proc = symbol_undefined
            expression_result = symbol_undefined
            expected_result = symbol_undefined
            expected_result = caddr(args_reg)
            expression_result = cadr(args_reg)
            proc = car(args_reg)
            GLOBALS['k2_reg'] = make_cont2(b_cont2_104_d, args_reg, info_reg, handler_reg, k2_reg)
            GLOBALS['args_reg'] = List(expression_result, expected_result)
            GLOBALS['proc_reg'] = proc
            GLOBALS['pc'] = apply_proc

def b_proc_87_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to set"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['lst_reg'] = car(args_reg)
        GLOBALS['pc'] = make_set

def b_proc_88_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(plus, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_89_d():
    if true_q(null_q(args_reg)):
        GLOBALS['msg_reg'] = "incorrect number of arguments to -"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(minus, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_90_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(multiply, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_91_d():
    if true_q((GreaterThan(length(args_reg), 1)) and (member(0, cdr(args_reg)))):
        GLOBALS['msg_reg'] = "division by zero"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(divide, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_92_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to %"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(numeric_equal(cadr(args_reg), 0)):
            GLOBALS['msg_reg'] = "modulo by zero"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(modulo, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_93_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(min, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_94_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(max, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_95_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to <"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(LessThan, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_96_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to >"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(GreaterThan, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_97_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to <="
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(LessThanEqual, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_98_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to >="
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(GreaterThanEqual, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_99_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to ="
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(all_numeric_q(args_reg))):
            GLOBALS['msg_reg'] = "attempt to apply = on non-numeric argument"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(numeric_equal, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_100_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to abs"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(abs, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_101_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to equal?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_50_d, fail_reg, k2_reg)
        GLOBALS['y_reg'] = cadr(args_reg)
        GLOBALS['x_reg'] = car(args_reg)
        GLOBALS['pc'] = equal_objects_q

def b_proc_102_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to eq?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(eq_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_103_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to memq"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(memq, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_104_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to member"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['ls_reg'] = cadr(args_reg)
        GLOBALS['y_reg'] = cadr(args_reg)
        GLOBALS['x_reg'] = car(args_reg)
        GLOBALS['pc'] = member_loop

def b_proc_105_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to random"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(positive_q(car(args_reg)))):
            GLOBALS['msg_reg'] = "argument to random must be positive"
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(random, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_106_d():
    if true_q((null_q(args_reg)) or (length_at_least_q(4, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to range"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(Range, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_107_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(snoc, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_108_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(rac, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_109_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(rdc, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_110_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to set-car!"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("set-car! called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(set_car_b, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_111_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to set-cdr!"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(pair_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("set-cdr! called on non-pair ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(set_cdr_b, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_112_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to load-as"
        GLOBALS['pc'] = runtime_error
    else:
        filename = symbol_undefined
        module_name = symbol_undefined
        module_name = cadr(args_reg)
        filename = car(args_reg)
        GLOBALS['k_reg'] = make_cont2(b_cont2_106_d, filename, handler_reg, k2_reg)
        GLOBALS['env_reg'] = env2_reg
        GLOBALS['var_reg'] = module_name
        GLOBALS['pc'] = lookup_binding_in_first_frame

def b_proc_113_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = car(_starstack_trace_star)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_114_d():
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['env_reg'] = env2_reg
    GLOBALS['pc'] = get_primitive

def b_proc_115_d(k):
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = car(args_reg)
    GLOBALS['k_reg'] = k
    GLOBALS['pc'] = apply_cont2

def b_proc_116_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to call/cc"
        GLOBALS['pc'] = runtime_error
    else:
        proc = symbol_undefined
        proc = car(args_reg)
        if true_q(not(procedure_object_q(proc))):
            GLOBALS['msg_reg'] = "call/cc called with non-procedure"
            GLOBALS['pc'] = runtime_error
        else:
            fake_k = symbol_undefined
            fake_k = make_proc(b_proc_115_d, k2_reg)
            if true_q(dlr_proc_q(proc)):
                GLOBALS['value2_reg'] = fail_reg
                GLOBALS['value1_reg'] = dlr_apply(proc, List(fake_k))
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2
            else:
                GLOBALS['args_reg'] = List(fake_k)
                GLOBALS['proc_reg'] = proc
                GLOBALS['pc'] = apply_proc

def b_proc_117_d():
    if true_q(null_q(args_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['k_reg'] = REP_k
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = car(args_reg)
        GLOBALS['k_reg'] = REP_k
        GLOBALS['pc'] = apply_cont2

def b_proc_118_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to require"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(true_q(car(args_reg))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = symbol_ok
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2
        else:
            GLOBALS['pc'] = apply_fail

def b_proc_119_d():
    GLOBALS['value2_reg'] = REP_fail
    GLOBALS['value1_reg'] = args_reg
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_120_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to reverse"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(list_q(args_reg))):
            GLOBALS['msg_reg'] = format("reverse called on incorrect list structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(reverse, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_121_d():
    GLOBALS['lists_reg'] = args_reg
    GLOBALS['pc'] = append_all

def b_proc_122_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string->number"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(string_to_number, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_123_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string=?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(string_is__q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_124_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to list->vector"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(list_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("list->vector called on incorrect list structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(list_to_vector, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_125_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to list->string"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(list_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("list->string called on incorrect list structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            if true_q(not(all_char_q(car(args_reg)))):
                GLOBALS['msg_reg'] = format("list->string called on non-char list ~s", car(args_reg))
                GLOBALS['pc'] = runtime_error
            else:
                GLOBALS['value2_reg'] = fail_reg
                GLOBALS['value1_reg'] = Apply(list_to_string, args_reg)
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2

def b_proc_126_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to char->string"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(char_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("char->string called on non-char item ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(char_to_string, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_127_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string->list"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(string_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("string->list called on non-string item ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(string_to_list, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_128_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string->symbol"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(string_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("string->symbol called on non-string item ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(string_to_symbol, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_129_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to symbol->string"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(symbol_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("symbol->string called on non-symbol item ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(symbol_to_string, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_130_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to vector->list"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(vector_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("vector->list called on incorrect vector structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(vector_to_list, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_131_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to vector-length"
        GLOBALS['pc'] = runtime_error
    else:
        if true_q(not(vector_q(car(args_reg)))):
            GLOBALS['msg_reg'] = format("vector-length called on incorrect vector structure ~s", car(args_reg))
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = Apply(vector_length, args_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2

def b_proc_132_d():
    GLOBALS['lst_reg'] = sort(symbolLessThan_q, get_completions(args_reg, env2_reg))
    GLOBALS['pc'] = make_set

def b_proc_133_d():
    GLOBALS['lst_reg'] = directory(args_reg, env2_reg)
    GLOBALS['pc'] = make_set

def b_proc_134_d():
    GLOBALS['lst_reg'] = sort(symbolLessThan_q, get_variables_from_frames(frames(macro_env)))
    GLOBALS['pc'] = make_set

def b_proc_135_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = get_current_time()
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_136_d():
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['env_reg'] = env2_reg
    GLOBALS['proc_reg'] = car(args_reg)
    GLOBALS['args_reg'] = cdr(args_reg)
    GLOBALS['pc'] = map_primitive

def b_proc_137_d():
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['env_reg'] = env2_reg
    GLOBALS['lists_reg'] = cdr(args_reg)
    GLOBALS['proc_reg'] = car(args_reg)
    GLOBALS['pc'] = for_each_primitive

def b_proc_138_d():
    if true_q(LessThan(length(args_reg), 1)):
        GLOBALS['msg_reg'] = "incorrect number of arguments to format"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(format, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_139_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = env2_reg
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_140_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = import_native(args_reg, env2_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_141_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = import_as_native(car(args_reg), cadr(args_reg), env2_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_142_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = import_from_native(car(args_reg), cdr(args_reg), env2_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_143_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to not"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = not(true_q(car(args_reg)))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_144_d():
    Apply(printf, args_reg)
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_145_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(vector_native, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_146_d():
    vector_set_b(car(args_reg), cadr(args_reg), caddr(args_reg))
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = void_value
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_147_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(vector_ref, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_148_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(make_vector, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_149_d():
    if true_q(not(length_at_least_q(1, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to 'error' (should at least 1)"
        GLOBALS['pc'] = runtime_error
    else:
        location = symbol_undefined
        message = symbol_undefined
        location = format("Error in '~a': ", car(args_reg))
        message = string_append(location, Apply(format, cdr(args_reg)))
        GLOBALS['msg_reg'] = message
        GLOBALS['pc'] = runtime_error

def b_proc_150_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to list-ref"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(list_ref, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_151_d():
    if true_q(null_q(args_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = current_directory()
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(length_one_q(args_reg)):
            if true_q(string_q(car(args_reg))):
                GLOBALS['value2_reg'] = fail_reg
                GLOBALS['value1_reg'] = current_directory(car(args_reg))
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2
            else:
                GLOBALS['msg_reg'] = "directory must be a string"
                GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['msg_reg'] = "incorrect number of arguments to current-directory"
            GLOBALS['pc'] = runtime_error

def b_proc_152_d():
    if true_q((length_one_q(args_reg)) and (number_q(car(args_reg)))):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = round(car(args_reg))
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['msg_reg'] = "round requires exactly one number"
        GLOBALS['pc'] = runtime_error

def b_proc_153_d():
    if true_q((length_one_q(args_reg)) and (boolean_q(car(args_reg)))):
        set_use_stack_trace_b(car(args_reg))
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(null_q(args_reg)):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = _staruse_stack_trace_star
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2
        else:
            GLOBALS['msg_reg'] = "use-stack-trace requires exactly one boolean or nothing"
            GLOBALS['pc'] = runtime_error

def b_proc_154_d():
    if true_q((length_one_q(args_reg)) and (boolean_q(car(args_reg)))):
        GLOBALS['_startracing_on_q_star'] = true_q(car(args_reg))
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(null_q(args_reg)):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = _startracing_on_q_star
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2
        else:
            GLOBALS['msg_reg'] = "use-tracing requires exactly one boolean or nothing"
            GLOBALS['pc'] = runtime_error

def b_proc_155_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to eqv?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(eqv_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_156_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to vector?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(vector_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_157_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to atom?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(atom_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_158_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to iter?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(iter_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_159_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(getitem_native, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_160_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(setitem_native, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_161_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(hasitem_native, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_162_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to list?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(list_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_163_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to procedure?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(procedure_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_164_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string<?"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(stringLessThan_q, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_165_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to float"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(float, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_166_d():
    if true_q(not(null_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to globals"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(globals, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_167_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to int"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(truncate_to_integer, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_168_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to assq"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(assq, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_169_d():
    if true_q(null_q(args_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = dict()
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_119_d, k2_reg)
        GLOBALS['args_reg'] = car(args_reg)
        GLOBALS['pc'] = make_dict

def b_proc_170_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to property"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(property, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_171_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to rational"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(divide, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_172_d():
    if true_q(not(null_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to reset-toplevel-env"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(reset_toplevel_env, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_173_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to sort"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = sort_native(args_reg, env2_reg, info_reg, handler_reg, fail_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_174_d():
    if true_q(not(length_at_least_q(2, args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string-append"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(string_append, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_175_d():
    if true_q(not(length_two_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to string-split"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(string_split, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_176_d():
    if true_q(not(length_one_q(args_reg))):
        GLOBALS['msg_reg'] = "incorrect number of arguments to typeof"
        GLOBALS['pc'] = runtime_error
    else:
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = Apply(type, args_reg)
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2

def b_proc_177_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = Apply(use_lexical_address, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_178_d():
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = host_environment_native()
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def b_proc_179_d(external_function_object):
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = apply_star(external_function_object, args_reg)
    GLOBALS['k_reg'] = k2_reg
    GLOBALS['pc'] = apply_cont2

def apply_macro():
    Apply(cadr(macro_reg), cddr(macro_reg))

def b_macro_1_d():
    if true_q(LessThan(length_hat(datum_reg), 3)):
        GLOBALS['adatum_reg'] = datum_reg
        GLOBALS['msg_reg'] = "bad lambda expression:"
        GLOBALS['pc'] = aparse_error
    else:
        formals = symbol_undefined
        bodies = symbol_undefined
        bodies = cddr_hat(datum_reg)
        formals = cadr_hat(datum_reg)
        return get_internal_defines_hat(bodies, datum_reg, handler_reg, fail_reg, make_cont2(b_cont2_42_d, formals, handler_reg, fail_reg, k_reg))

def b_macro_2_d():
    if true_q(LessThan(length_hat(datum_reg), 4)):
        GLOBALS['adatum_reg'] = datum_reg
        GLOBALS['msg_reg'] = "bad trace-lambda expression:"
        GLOBALS['pc'] = aparse_error
    else:
        name = symbol_undefined
        formals = symbol_undefined
        bodies = symbol_undefined
        bodies = cdddr_hat(datum_reg)
        formals = caddr_hat(datum_reg)
        name = cadr_hat(datum_reg)
        return get_internal_defines_hat(bodies, datum_reg, handler_reg, fail_reg, make_cont2(b_cont2_43_d, name, formals, handler_reg, fail_reg, k_reg))

def b_macro_3_d():
    if true_q(symbol_q_hat(cadr_hat(datum_reg))):
        name = symbol_undefined
        bindings = symbol_undefined
        vars = symbol_undefined
        exps = symbol_undefined
        bodies = symbol_undefined
        name = cadr_hat(datum_reg)
        bindings = caddr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cdddr_hat(datum_reg)
        GLOBALS['value_reg'] = append(List(symbol_letrec), append(List(List(append(List(name), List(append(List(symbol_lambda), append(List(vars), at_hat(bodies))))))), List(append(List(name), at_hat(exps)))))
        GLOBALS['pc'] = apply_cont
    else:
        bindings = symbol_undefined
        vars = symbol_undefined
        exps = symbol_undefined
        bodies = symbol_undefined
        bindings = cadr_hat(datum_reg)
        vars = map_hat(car_hat, bindings)
        exps = map_hat(cadr_hat, bindings)
        bodies = cddr_hat(datum_reg)
        GLOBALS['value_reg'] = append(List(append(List(symbol_lambda), append(List(vars), at_hat(bodies)))), at_hat(exps))
        GLOBALS['pc'] = apply_cont

def b_macro_4_d():
    decls = symbol_undefined
    vars = symbol_undefined
    procs = symbol_undefined
    bodies = symbol_undefined
    decls = cadr_hat(datum_reg)
    vars = map_hat(car_hat, decls)
    procs = map_hat(cadr_hat, decls)
    bodies = cddr_hat(datum_reg)
    GLOBALS['k2_reg'] = make_cont2(b_cont2_46_d, bodies, k_reg)
    GLOBALS['procs_reg'] = procs
    GLOBALS['vars_reg'] = vars
    GLOBALS['pc'] = create_letrec_assignments_hat

def b_macro_5_d():
    name = symbol_undefined
    formals = symbol_undefined
    bodies = symbol_undefined
    bodies = cddr_hat(datum_reg)
    formals = cdadr_hat(datum_reg)
    name = caadr_hat(datum_reg)
    GLOBALS['value_reg'] = append(List(symbol_define), append(List(name), List(append(List(symbol_lambda), append(List(formals), at_hat(bodies))))))
    GLOBALS['pc'] = apply_cont

def b_macro_6_d():
    exps = symbol_undefined
    exps = cdr_hat(datum_reg)
    if true_q(null_q_hat(exps)):
        GLOBALS['value_reg'] = True
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(null_q_hat(cdr_hat(exps))):
            GLOBALS['value_reg'] = car_hat(exps)
            GLOBALS['pc'] = apply_cont
        else:
            GLOBALS['value_reg'] = append(List(symbol_if), append(List(car_hat(exps)), append(List(append(List(symbol_and), at_hat(cdr_hat(exps)))), List(False))))
            GLOBALS['pc'] = apply_cont

def b_macro_7_d():
    exps = symbol_undefined
    exps = cdr_hat(datum_reg)
    if true_q(null_q_hat(exps)):
        GLOBALS['value_reg'] = False
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(null_q_hat(cdr_hat(exps))):
            GLOBALS['value_reg'] = car_hat(exps)
            GLOBALS['pc'] = apply_cont
        else:
            GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(car_hat(exps)))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_or), at_hat(cdr_hat(exps))))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(symbol_bool), List(List(symbol_else_code))))))))
            GLOBALS['pc'] = apply_cont

def b_macro_8_d():
    clauses = symbol_undefined
    clauses = cdr_hat(datum_reg)
    if true_q(null_q_hat(clauses)):
        GLOBALS['adatum_reg'] = datum_reg
        GLOBALS['msg_reg'] = "empty (cond) expression"
        GLOBALS['pc'] = amacro_error
    else:
        first_clause = symbol_undefined
        other_clauses = symbol_undefined
        other_clauses = cdr_hat(clauses)
        first_clause = car_hat(clauses)
        if true_q((null_q_hat(first_clause)) or (not(list_q_hat(first_clause)))):
            GLOBALS['adatum_reg'] = first_clause
            GLOBALS['msg_reg'] = "improper cond clause"
            GLOBALS['pc'] = amacro_error
        else:
            test_exp = symbol_undefined
            then_exps = symbol_undefined
            then_exps = cdr_hat(first_clause)
            test_exp = car_hat(first_clause)
            if true_q(eq_q_hat(test_exp, symbol_else)):
                if true_q(null_q_hat(then_exps)):
                    GLOBALS['adatum_reg'] = first_clause
                    GLOBALS['msg_reg'] = "improper else clause"
                    GLOBALS['pc'] = amacro_error
                else:
                    if true_q(null_q_hat(cdr_hat(then_exps))):
                        GLOBALS['value_reg'] = car_hat(then_exps)
                        GLOBALS['pc'] = apply_cont
                    else:
                        GLOBALS['value_reg'] = append(List(symbol_begin), at_hat(then_exps))
                        GLOBALS['pc'] = apply_cont
            else:
                if true_q(null_q_hat(then_exps)):
                    if true_q(null_q_hat(other_clauses)):
                        GLOBALS['value_reg'] = append(List(symbol_let), append(List(List(append(List(symbol_bool), List(test_exp)))), List(append(List(symbol_if), append(List(symbol_bool), List(symbol_bool))))))
                        GLOBALS['pc'] = apply_cont
                    else:
                        GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_cond), at_hat(other_clauses)))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(symbol_bool), List(List(symbol_else_code))))))))
                        GLOBALS['pc'] = apply_cont
                else:
                    if true_q(eq_q_hat(car_hat(then_exps), symbol__is_to_)):
                        if true_q(null_q_hat(cdr_hat(then_exps))):
                            GLOBALS['adatum_reg'] = first_clause
                            GLOBALS['msg_reg'] = "improper => clause"
                            GLOBALS['pc'] = amacro_error
                        else:
                            if true_q(null_q_hat(other_clauses)):
                                GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), List(append(List(symbol_th), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(cadr_hat(then_exps))))))))), List(append(List(symbol_if), append(List(symbol_bool), List(append(List(List(symbol_th)), List(symbol_bool))))))))
                                GLOBALS['pc'] = apply_cont
                            else:
                                GLOBALS['value_reg'] = append(List(symbol_let), append(List(append(List(append(List(symbol_bool), List(test_exp))), append(List(append(List(symbol_th), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(cadr_hat(then_exps))))))), List(append(List(symbol_else_code), List(append(List(symbol_lambda), append(List(symbol_emptylist), List(append(List(symbol_cond), at_hat(other_clauses))))))))))), List(append(List(symbol_if), append(List(symbol_bool), append(List(append(List(List(symbol_th)), List(symbol_bool))), List(List(symbol_else_code))))))))
                                GLOBALS['pc'] = apply_cont
                    else:
                        if true_q(null_q_hat(other_clauses)):
                            if true_q(null_q_hat(cdr_hat(then_exps))):
                                GLOBALS['value_reg'] = append(List(symbol_if), append(List(test_exp), List(car_hat(then_exps))))
                                GLOBALS['pc'] = apply_cont
                            else:
                                GLOBALS['value_reg'] = append(List(symbol_if), append(List(test_exp), List(append(List(symbol_begin), at_hat(then_exps)))))
                                GLOBALS['pc'] = apply_cont
                        else:
                            if true_q(null_q_hat(cdr_hat(then_exps))):
                                GLOBALS['value_reg'] = append(List(symbol_if), append(List(test_exp), append(List(car_hat(then_exps)), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                GLOBALS['pc'] = apply_cont
                            else:
                                GLOBALS['value_reg'] = append(List(symbol_if), append(List(test_exp), append(List(append(List(symbol_begin), at_hat(then_exps))), List(append(List(symbol_cond), at_hat(other_clauses))))))
                                GLOBALS['pc'] = apply_cont

def b_macro_9_d():
    bindings = symbol_undefined
    bodies = symbol_undefined
    bodies = cddr_hat(datum_reg)
    bindings = cadr_hat(datum_reg)
    GLOBALS['bodies_reg'] = bodies
    GLOBALS['bindings_reg'] = bindings
    GLOBALS['pc'] = nest_let_star_bindings_hat

def b_macro_10_d():
    exp = symbol_undefined
    clauses = symbol_undefined
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    GLOBALS['k2_reg'] = make_cont2(b_cont2_48_d, exp, k_reg)
    GLOBALS['clauses_reg'] = clauses
    GLOBALS['var_reg'] = symbol_r
    GLOBALS['pc'] = case_clauses_to_cond_clauses_hat

def b_macro_11_d():
    exp = symbol_undefined
    clauses = symbol_undefined
    clauses = cddr_hat(datum_reg)
    exp = cadr_hat(datum_reg)
    GLOBALS['k2_reg'] = make_cont2(b_cont2_48_d, exp, k_reg)
    GLOBALS['clauses_reg'] = clauses
    GLOBALS['var_reg'] = symbol_r
    GLOBALS['pc'] = record_case_clauses_to_cond_clauses_hat

def b_macro_12_d():
    datatype_name = symbol_undefined
    type_tester_name = symbol_undefined
    datatype_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(datatype_name), "?"))
    if true_q(not(eq_q_hat(caddr_hat(datum_reg), type_tester_name))):
        GLOBALS['adatum_reg'] = caddr_hat(datum_reg)
        GLOBALS['msg_reg'] = format("datatype tester predicate not named ~a", type_tester_name)
        GLOBALS['pc'] = amacro_error
    else:
        variants = symbol_undefined
        variants = cdddr_hat(datum_reg)
        GLOBALS['k2_reg'] = make_cont2(b_cont2_51_d, type_tester_name, k_reg)
        GLOBALS['variants_reg'] = variants
        GLOBALS['pc'] = make_dd_variant_constructors_hat

def b_macro_13_d():
    type_name = symbol_undefined
    type_tester_name = symbol_undefined
    exp = symbol_undefined
    clauses = symbol_undefined
    type_name = cadr_hat(datum_reg)
    type_tester_name = string_to_symbol(string_append(symbol_to_string_hat(type_name), "?"))
    exp = caddr_hat(datum_reg)
    clauses = cdddr_hat(datum_reg)
    GLOBALS['k2_reg'] = make_cont2(b_cont2_54_d, exp, type_name, type_tester_name, k_reg)
    GLOBALS['clauses_reg'] = clauses
    GLOBALS['var_reg'] = symbol_r
    GLOBALS['pc'] = record_case_clauses_to_cond_clauses_hat

def b_macro_14_d(proc, env, info):
    GLOBALS['k_reg'] = make_cont(b_cont_46_d, proc, env, info, handler_reg, fail_reg, k_reg)
    GLOBALS['x_reg'] = datum_reg
    GLOBALS['pc'] = unannotate_cps

def next_avail(n):
    return string_ref(chars_to_scan, n)

def remaining(n):
    return (1) + (n)

def initialize_scan_counters():
    GLOBALS['scan_line'] = 1
    GLOBALS['scan_char'] = 1
    GLOBALS['scan_position'] = 1
    GLOBALS['last_scan_line'] = scan_line
    GLOBALS['last_scan_char'] = scan_char
    GLOBALS['last_scan_position'] = scan_position

def increment_scan_counters(chars):
    GLOBALS['last_scan_line'] = scan_line
    GLOBALS['last_scan_char'] = scan_char
    GLOBALS['last_scan_position'] = scan_position
    if true_q(char_is__q(next_avail(chars), make_char('\n'))):
        GLOBALS['scan_line'] = (1) + (scan_line)
        GLOBALS['scan_char'] = 1
    else:
        GLOBALS['scan_char'] = (1) + (scan_char)
    GLOBALS['scan_position'] = (1) + (scan_position)

def mark_token_start():
    GLOBALS['token_start_line'] = scan_line
    GLOBALS['token_start_char'] = scan_char
    GLOBALS['token_start_position'] = scan_position

def scan_input():
    initialize_scan_counters()
    GLOBALS['chars_to_scan'] = string_append(input_reg, string(make_char('\0')))
    GLOBALS['chars_reg'] = 0
    GLOBALS['pc'] = scan_input_loop

def scan_input_loop():
    GLOBALS['k_reg'] = make_cont3(b_cont3_1_d, src_reg, handler_reg, k_reg)
    GLOBALS['buffer_reg'] = symbol_emptylist
    GLOBALS['action_reg'] = List(symbol_goto, symbol_start_state)
    GLOBALS['pc'] = apply_action

def apply_action():
    if true_q((car(action_reg)) is (symbol_shift)):
        next = symbol_undefined
        next = list_ref(action_reg, 1)
        increment_scan_counters(chars_reg)
        GLOBALS['buffer_reg'] = cons(next_avail(chars_reg), buffer_reg)
        GLOBALS['chars_reg'] = remaining(chars_reg)
        GLOBALS['action_reg'] = next
        GLOBALS['pc'] = apply_action
    else:
        if true_q((car(action_reg)) is (symbol_replace)):
            new_char = symbol_undefined
            next = symbol_undefined
            next = list_ref(action_reg, 2)
            new_char = list_ref(action_reg, 1)
            increment_scan_counters(chars_reg)
            GLOBALS['chars_reg'] = remaining(chars_reg)
            GLOBALS['buffer_reg'] = cons(new_char, buffer_reg)
            GLOBALS['action_reg'] = next
            GLOBALS['pc'] = apply_action
        else:
            if true_q((car(action_reg)) is (symbol_drop)):
                next = symbol_undefined
                next = list_ref(action_reg, 1)
                increment_scan_counters(chars_reg)
                GLOBALS['chars_reg'] = remaining(chars_reg)
                GLOBALS['action_reg'] = next
                GLOBALS['pc'] = apply_action
            else:
                if true_q((car(action_reg)) is (symbol_goto)):
                    state = symbol_undefined
                    state = list_ref(action_reg, 1)
                    if true_q((state) is (symbol_token_start_state)):
                        mark_token_start()
                    action = symbol_undefined
                    action = apply_state(state, next_avail(chars_reg))
                    if true_q((action) is (symbol_error)):
                        GLOBALS['pc'] = unexpected_char_error
                    else:
                        GLOBALS['action_reg'] = action
                        GLOBALS['pc'] = apply_action
                else:
                    if true_q((car(action_reg)) is (symbol_emit)):
                        token_type = symbol_undefined
                        token_type = list_ref(action_reg, 1)
                        GLOBALS['k_reg'] = make_cont(b_cont_1_d, chars_reg, fail_reg, k_reg)
                        GLOBALS['token_type_reg'] = token_type
                        GLOBALS['pc'] = convert_buffer_to_token
                    else:
                        raise Exception("symbol_apply_action: " + format("invalid action: ~a", *[action_reg]))

def scan_error():
    GLOBALS['exception_reg'] = make_exception("ScanError", msg_reg, src_reg, line_reg, char_reg)
    GLOBALS['pc'] = apply_handler2

def unexpected_char_error():
    c = symbol_undefined
    c = next_avail(chars_reg)
    if true_q(char_is__q(c, make_char('\0'))):
        GLOBALS['char_reg'] = scan_char
        GLOBALS['line_reg'] = scan_line
        GLOBALS['msg_reg'] = "unexpected end of input"
        GLOBALS['pc'] = scan_error
    else:
        GLOBALS['char_reg'] = scan_char
        GLOBALS['line_reg'] = scan_line
        GLOBALS['msg_reg'] = format("unexpected character '~a' encountered", c)
        GLOBALS['pc'] = scan_error

def convert_buffer_to_token():
    buffer = symbol_undefined
    buffer = reverse(buffer_reg)
    if true_q((token_type_reg) is (symbol_end_marker)):
        GLOBALS['value_reg'] = make_token1(symbol_end_marker)
        GLOBALS['pc'] = apply_cont
    else:
        if true_q((token_type_reg) is (symbol_integer)):
            GLOBALS['value_reg'] = make_token2(symbol_integer, list_to_string(buffer))
            GLOBALS['pc'] = apply_cont
        else:
            if true_q((token_type_reg) is (symbol_decimal)):
                GLOBALS['value_reg'] = make_token2(symbol_decimal, list_to_string(buffer))
                GLOBALS['pc'] = apply_cont
            else:
                if true_q((token_type_reg) is (symbol_rational)):
                    GLOBALS['value_reg'] = make_token2(symbol_rational, list_to_string(buffer))
                    GLOBALS['pc'] = apply_cont
                else:
                    if true_q((token_type_reg) is (symbol_identifier)):
                        GLOBALS['value_reg'] = make_token2(symbol_identifier, string_to_symbol(list_to_string(buffer)))
                        GLOBALS['pc'] = apply_cont
                    else:
                        if true_q((token_type_reg) is (symbol_boolean)):
                            GLOBALS['value_reg'] = make_token2(symbol_boolean, (char_is__q(car(buffer), make_char('t'))) or (char_is__q(car(buffer), make_char('T'))))
                            GLOBALS['pc'] = apply_cont
                        else:
                            if true_q((token_type_reg) is (symbol_character)):
                                GLOBALS['value_reg'] = make_token2(symbol_character, car(buffer))
                                GLOBALS['pc'] = apply_cont
                            else:
                                if true_q((token_type_reg) is (symbol_named_character)):
                                    name = symbol_undefined
                                    name = list_to_string(buffer)
                                    if true_q(string_is__q(name, "nul")):
                                        GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\0'))
                                        GLOBALS['pc'] = apply_cont
                                    else:
                                        if true_q(string_is__q(name, "space")):
                                            GLOBALS['value_reg'] = make_token2(symbol_character, make_char(' '))
                                            GLOBALS['pc'] = apply_cont
                                        else:
                                            if true_q(string_is__q(name, "tab")):
                                                GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\t'))
                                                GLOBALS['pc'] = apply_cont
                                            else:
                                                if true_q(string_is__q(name, "newline")):
                                                    GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\n'))
                                                    GLOBALS['pc'] = apply_cont
                                                else:
                                                    if true_q(string_is__q(name, "linefeed")):
                                                        GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\n'))
                                                        GLOBALS['pc'] = apply_cont
                                                    else:
                                                        if true_q(string_is__q(name, "backspace")):
                                                            GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\b'))
                                                            GLOBALS['pc'] = apply_cont
                                                        else:
                                                            if true_q(string_is__q(name, "return")):
                                                                GLOBALS['value_reg'] = make_token2(symbol_character, make_char('\r'))
                                                                GLOBALS['pc'] = apply_cont
                                                            else:
                                                                if true_q(string_is__q(name, "page")):
                                                                    GLOBALS['value_reg'] = make_token2(symbol_character, make_char(u"\u000C"))
                                                                    GLOBALS['pc'] = apply_cont
                                                                else:
                                                                    GLOBALS['char_reg'] = token_start_char
                                                                    GLOBALS['line_reg'] = token_start_line
                                                                    GLOBALS['msg_reg'] = format("invalid character name #\\~a", name)
                                                                    GLOBALS['pc'] = scan_error
                                else:
                                    if true_q((token_type_reg) is (symbol_string)):
                                        GLOBALS['value_reg'] = make_token2(symbol_string, list_to_string(buffer))
                                        GLOBALS['pc'] = apply_cont
                                    else:
                                        GLOBALS['value_reg'] = make_token1(token_type_reg)
                                        GLOBALS['pc'] = apply_cont

def make_token1(token_type):
    start = symbol_undefined
    end = symbol_undefined
    end = List(last_scan_line, last_scan_char, last_scan_position)
    start = List(token_start_line, token_start_char, token_start_position)
    if true_q((token_type) is (symbol_end_marker)):
        return List(token_type, end, end)
    else:
        return List(token_type, start, end)

def make_token2(token_type, token_info):
    return List(token_type, token_info, List(token_start_line, token_start_char, token_start_position), List(last_scan_line, last_scan_char, last_scan_position))

def token_type_q(token, class_):
    return (car(token)) is (class_)

def get_token_start(token):
    return rac(rdc(token))

def get_token_end(token):
    return rac(token)

def get_token_start_line(token):
    return car(get_token_start(token))

def get_token_start_char(token):
    return cadr(get_token_start(token))

def get_token_start_pos(token):
    return caddr(get_token_start(token))

def rac(ls):
    if true_q(null_q(cdr(ls))):
        return car(ls)
    else:
        current = symbol_undefined
        current = cdr(ls)
        while pair_q(cdr(current)):
            current = cdr(current)
        return car(current)

def rdc(ls):
    if true_q(null_q(cdr(ls))):
        return List()
    else:
        retval = symbol_undefined
        front = symbol_undefined
        current = symbol_undefined
        retval = List(car(ls))
        front = retval
        current = cdr(ls)
        while pair_q(cdr(current)):
            set_cdr_b(retval, List(car(current)))
            retval = cdr(retval)
            current = cdr(current)
        return front

def snoc(x, ls):
    if true_q(null_q(ls)):
        return List(x)
    else:
        retval = symbol_undefined
        front = symbol_undefined
        current = symbol_undefined
        retval = List(car(ls))
        front = retval
        current = cdr(ls)
        while pair_q(current):
            set_cdr_b(retval, List(car(current)))
            retval = cdr(retval)
            current = cdr(current)
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
    if true_q((state) is (symbol_start_state)):
        if true_q(char_whitespace_q(c)):
            return List(symbol_drop, List(symbol_goto, symbol_start_state))
        else:
            if true_q(char_is__q(c, make_char(';'))):
                return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if true_q(char_is__q(c, make_char('\0'))):
                    return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                else:
                    return List(symbol_goto, symbol_token_start_state)
    else:
        if true_q((state) is (symbol_token_start_state)):
            if true_q(char_is__q(c, make_char('('))):
                return List(symbol_drop, List(symbol_emit, symbol_lparen))
            else:
                if true_q(char_is__q(c, make_char('['))):
                    return List(symbol_drop, List(symbol_emit, symbol_lbracket))
                else:
                    if true_q(char_is__q(c, make_char(')'))):
                        return List(symbol_drop, List(symbol_emit, symbol_rparen))
                    else:
                        if true_q(char_is__q(c, make_char(']'))):
                            return List(symbol_drop, List(symbol_emit, symbol_rbracket))
                        else:
                            if true_q(char_is__q(c, make_char("'"))):
                                return List(symbol_drop, List(symbol_emit, symbol_apostrophe))
                            else:
                                if true_q(char_is__q(c, make_char('`'))):
                                    return List(symbol_drop, List(symbol_emit, symbol_backquote))
                                else:
                                    if true_q(char_is__q(c, make_char(','))):
                                        return List(symbol_drop, List(symbol_goto, symbol_comma_state))
                                    else:
                                        if true_q(char_is__q(c, make_char('#'))):
                                            return List(symbol_drop, List(symbol_goto, symbol_hash_prefix_state))
                                        else:
                                            if true_q(char_is__q(c, make_char('"'))):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_state))
                                            else:
                                                if true_q(char_initial_q(c)):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if true_q(char_sign_q(c)):
                                                        return List(symbol_shift, List(symbol_goto, symbol_signed_state))
                                                    else:
                                                        if true_q(char_is__q(c, make_char('.'))):
                                                            return List(symbol_shift, List(symbol_goto, symbol_decimal_point_state))
                                                        else:
                                                            if true_q(char_numeric_q(c)):
                                                                return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                            else:
                                                                return symbol_error
        else:
            if true_q((state) is (symbol_comment_state)):
                if true_q(char_is__q(c, make_char('\n'))):
                    return List(symbol_drop, List(symbol_goto, symbol_start_state))
                else:
                    if true_q(char_is__q(c, make_char('\0'))):
                        return List(symbol_drop, List(symbol_emit, symbol_end_marker))
                    else:
                        return List(symbol_drop, List(symbol_goto, symbol_comment_state))
            else:
                if true_q((state) is (symbol_comma_state)):
                    if true_q(char_is__q(c, make_char('@'))):
                        return List(symbol_drop, List(symbol_emit, symbol_comma_at))
                    else:
                        return List(symbol_emit, symbol_comma)
                else:
                    if true_q((state) is (symbol_hash_prefix_state)):
                        if true_q(char_boolean_q(c)):
                            return List(symbol_shift, List(symbol_emit, symbol_boolean))
                        else:
                            if true_q(char_is__q(c, make_char('\\'))):
                                return List(symbol_drop, List(symbol_goto, symbol_character_state))
                            else:
                                if true_q(char_is__q(c, make_char('('))):
                                    return List(symbol_drop, List(symbol_emit, symbol_lvector))
                                else:
                                    return symbol_error
                    else:
                        if true_q((state) is (symbol_character_state)):
                            if true_q(char_alphabetic_q(c)):
                                return List(symbol_shift, List(symbol_goto, symbol_alphabetic_character_state))
                            else:
                                if true_q(not(char_is__q(c, make_char('\0')))):
                                    return List(symbol_shift, List(symbol_emit, symbol_character))
                                else:
                                    return symbol_error
                        else:
                            if true_q((state) is (symbol_alphabetic_character_state)):
                                if true_q(char_alphabetic_q(c)):
                                    return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    return List(symbol_emit, symbol_character)
                            else:
                                if true_q((state) is (symbol_named_character_state)):
                                    if true_q(char_delimiter_q(c)):
                                        return List(symbol_emit, symbol_named_character)
                                    else:
                                        return List(symbol_shift, List(symbol_goto, symbol_named_character_state))
                                else:
                                    if true_q((state) is (symbol_string_state)):
                                        if true_q(char_is__q(c, make_char('"'))):
                                            return List(symbol_drop, List(symbol_emit, symbol_string))
                                        else:
                                            if true_q(char_is__q(c, make_char('\\'))):
                                                return List(symbol_drop, List(symbol_goto, symbol_string_escape_state))
                                            else:
                                                if true_q(not(char_is__q(c, make_char('\0')))):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    return symbol_error
                                    else:
                                        if true_q((state) is (symbol_string_escape_state)):
                                            if true_q(char_is__q(c, make_char('"'))):
                                                return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                            else:
                                                if true_q(char_is__q(c, make_char('\\'))):
                                                    return List(symbol_shift, List(symbol_goto, symbol_string_state))
                                                else:
                                                    if true_q(char_is__q(c, make_char('b'))):
                                                        return List(symbol_replace, make_char('\b'), List(symbol_goto, symbol_string_state))
                                                    else:
                                                        if true_q(char_is__q(c, make_char('f'))):
                                                            return List(symbol_replace, make_char(u"\u000C"), List(symbol_goto, symbol_string_state))
                                                        else:
                                                            if true_q(char_is__q(c, make_char('n'))):
                                                                return List(symbol_replace, make_char('\n'), List(symbol_goto, symbol_string_state))
                                                            else:
                                                                if true_q(char_is__q(c, make_char('t'))):
                                                                    return List(symbol_replace, make_char('\t'), List(symbol_goto, symbol_string_state))
                                                                else:
                                                                    if true_q(char_is__q(c, make_char('r'))):
                                                                        return List(symbol_replace, make_char('\r'), List(symbol_goto, symbol_string_state))
                                                                    else:
                                                                        return symbol_error
                                        else:
                                            if true_q((state) is (symbol_identifier_state)):
                                                if true_q(char_subsequent_q(c)):
                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                else:
                                                    if true_q(char_delimiter_q(c)):
                                                        return List(symbol_emit, symbol_identifier)
                                                    else:
                                                        return symbol_error
                                            else:
                                                if true_q((state) is (symbol_signed_state)):
                                                    if true_q(char_numeric_q(c)):
                                                        return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                    else:
                                                        if true_q(char_is__q(c, make_char('.'))):
                                                            return List(symbol_shift, List(symbol_goto, symbol_signed_decimal_point_state))
                                                        else:
                                                            if true_q(char_delimiter_q(c)):
                                                                return List(symbol_emit, symbol_identifier)
                                                            else:
                                                                if true_q(char_subsequent_q(c)):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                else:
                                                    if true_q((state) is (symbol_decimal_point_state)):
                                                        if true_q(char_numeric_q(c)):
                                                            return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                        else:
                                                            if true_q(char_delimiter_q(c)):
                                                                return List(symbol_emit, symbol_dot)
                                                            else:
                                                                if true_q(char_subsequent_q(c)):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                else:
                                                                    return symbol_error
                                                    else:
                                                        if true_q((state) is (symbol_signed_decimal_point_state)):
                                                            if true_q(char_numeric_q(c)):
                                                                return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                            else:
                                                                if true_q(char_delimiter_q(c)):
                                                                    return List(symbol_emit, symbol_identifier)
                                                                else:
                                                                    if true_q(char_subsequent_q(c)):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                    else:
                                                                        return symbol_error
                                                        else:
                                                            if true_q((state) is (symbol_whole_number_state)):
                                                                if true_q(char_numeric_q(c)):
                                                                    return List(symbol_shift, List(symbol_goto, symbol_whole_number_state))
                                                                else:
                                                                    if true_q(char_is__q(c, make_char('.'))):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if true_q(char_is__q(c, make_char('/'))):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state))
                                                                        else:
                                                                            if true_q((char_is__q(c, make_char('e'))) or (char_is__q(c, make_char('E')))):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                            else:
                                                                                if true_q(char_delimiter_q(c)):
                                                                                    return List(symbol_emit, symbol_integer)
                                                                                else:
                                                                                    if true_q(char_subsequent_q(c)):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                            else:
                                                                if true_q((state) is (symbol_fractional_number_state)):
                                                                    if true_q(char_numeric_q(c)):
                                                                        return List(symbol_shift, List(symbol_goto, symbol_fractional_number_state))
                                                                    else:
                                                                        if true_q((char_is__q(c, make_char('e'))) or (char_is__q(c, make_char('E')))):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_suffix_state))
                                                                        else:
                                                                            if true_q(char_delimiter_q(c)):
                                                                                return List(symbol_emit, symbol_decimal)
                                                                            else:
                                                                                if true_q(char_subsequent_q(c)):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                else:
                                                                    if true_q((state) is (symbol_rational_number_state)):
                                                                        if true_q(char_numeric_q(c)):
                                                                            return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                        else:
                                                                            if true_q(char_delimiter_q(c)):
                                                                                return List(symbol_emit, symbol_identifier)
                                                                            else:
                                                                                if true_q(char_subsequent_q(c)):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                else:
                                                                                    return symbol_error
                                                                    else:
                                                                        if true_q((state) is (symbol_rational_number_state_star)):
                                                                            if true_q(char_numeric_q(c)):
                                                                                return List(symbol_shift, List(symbol_goto, symbol_rational_number_state_star))
                                                                            else:
                                                                                if true_q(char_delimiter_q(c)):
                                                                                    return List(symbol_emit, symbol_rational)
                                                                                else:
                                                                                    if true_q(char_subsequent_q(c)):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                    else:
                                                                                        return symbol_error
                                                                        else:
                                                                            if true_q((state) is (symbol_suffix_state)):
                                                                                if true_q(char_sign_q(c)):
                                                                                    return List(symbol_shift, List(symbol_goto, symbol_signed_exponent_state))
                                                                                else:
                                                                                    if true_q(char_numeric_q(c)):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if true_q(char_delimiter_q(c)):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if true_q(char_subsequent_q(c)):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                            else:
                                                                                if true_q((state) is (symbol_signed_exponent_state)):
                                                                                    if true_q(char_numeric_q(c)):
                                                                                        return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                    else:
                                                                                        if true_q(char_delimiter_q(c)):
                                                                                            return List(symbol_emit, symbol_identifier)
                                                                                        else:
                                                                                            if true_q(char_subsequent_q(c)):
                                                                                                return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                            else:
                                                                                                return symbol_error
                                                                                else:
                                                                                    if true_q((state) is (symbol_exponent_state)):
                                                                                        if true_q(char_numeric_q(c)):
                                                                                            return List(symbol_shift, List(symbol_goto, symbol_exponent_state))
                                                                                        else:
                                                                                            if true_q(char_delimiter_q(c)):
                                                                                                return List(symbol_emit, symbol_decimal)
                                                                                            else:
                                                                                                if true_q(char_subsequent_q(c)):
                                                                                                    return List(symbol_shift, List(symbol_goto, symbol_identifier_state))
                                                                                                else:
                                                                                                    return symbol_error
                                                                                    else:
                                                                                        raise Exception("symbol_apply_state: " + format("invalid state: ~a", *[state]))

def aatom_q(x):
    return (pair_q(x)) and ((car(x)) is (atom_tag))

def apair_q(x):
    return (pair_q(x)) and ((car(x)) is (pair_tag))

def annotated_q(x):
    return (pair_q(x)) and (((car(x)) is (atom_tag)) or ((car(x)) is (pair_tag)))

def untag_atom_hat(aatom):
    return cadr(aatom)

def atom_q_hat(asexp):
    return (car(asexp)) is (atom_tag)

def pair_q_hat(asexp):
    return (car(asexp)) is (pair_tag)

def null_q_hat(asexp):
    return (atom_q_hat(asexp)) and (null_q(untag_atom_hat(asexp)))

def symbol_q_hat(asexp):
    return (atom_q_hat(asexp)) and (symbol_q(untag_atom_hat(asexp)))

def string_q_hat(asexp):
    return (atom_q_hat(asexp)) and (string_q(untag_atom_hat(asexp)))

def vector_q_hat(asexp):
    return (atom_q_hat(asexp)) and (vector_q(untag_atom_hat(asexp)))

def car_hat(asexp):
    return cadr(asexp)

def cdr_hat(asexp):
    return caddr(asexp)

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
    return (cadr(asexp)) is (sym)

def vector_to_list_hat(asexp):
    return vector_to_list(cadr(asexp))

def symbol_to_string_hat(asexp):
    return symbol_to_string(cadr(asexp))

def list_q_hat(asexp):
    return (null_q_hat(asexp)) or ((pair_q_hat(asexp)) and (list_q_hat(caddr(asexp))))

def at_hat(alist):
    if true_q(null_q_hat(alist)):
        return symbol_emptylist
    else:
        return cons(car_hat(alist), at_hat(cdr_hat(alist)))

def length_hat(asexp):
    if true_q(null_q_hat(asexp)):
        return 0
    else:
        return (1) + (length_hat(cdr_hat(asexp)))

def cons_hat(a, b, info):
    return List(pair_tag, a, b, info)

def map_hat(f_hat, asexp):
    if true_q(null_q_hat(asexp)):
        return make_null_hat()
    else:
        return cons_hat(f_hat(car_hat(asexp)), map_hat(f_hat, cdr_hat(asexp)), symbol_none)

def make_null_hat():
    return List(atom_tag, symbol_emptylist, symbol_none)

def list_hat(x):
    return cons_hat(x, make_null_hat(), symbol_none)

def annotate_cps():
    if true_q(not(_starreader_generates_annotated_sexps_q_star)):
        GLOBALS['value_reg'] = x_reg
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(annotated_q(x_reg)):
            GLOBALS['value_reg'] = x_reg
            GLOBALS['pc'] = apply_cont
        else:
            if true_q(pair_q(x_reg)):
                GLOBALS['k_reg'] = make_cont(b_cont_3_d, x_reg, info_reg, k_reg)
                GLOBALS['info_reg'] = symbol_none
                GLOBALS['x_reg'] = car(x_reg)
                GLOBALS['pc'] = annotate_cps
            else:
                GLOBALS['value_reg'] = List(atom_tag, x_reg, info_reg)
                GLOBALS['pc'] = apply_cont

def unannotate_cps():
    if true_q(aatom_q(x_reg)):
        GLOBALS['x_reg'] = cadr(x_reg)
        GLOBALS['pc'] = unannotate_cps
    else:
        if true_q(apair_q(x_reg)):
            GLOBALS['k_reg'] = make_cont(b_cont_7_d, x_reg, k_reg)
            GLOBALS['x_reg'] = cadr(x_reg)
            GLOBALS['pc'] = unannotate_cps
        else:
            if true_q(pair_q(x_reg)):
                GLOBALS['k_reg'] = make_cont(b_cont_5_d, x_reg, k_reg)
                GLOBALS['x_reg'] = car(x_reg)
                GLOBALS['pc'] = unannotate_cps
            else:
                if true_q(vector_q(x_reg)):
                    GLOBALS['k_reg'] = make_cont(b_cont_6_d, k_reg)
                    GLOBALS['x_reg'] = vector_to_list(x_reg)
                    GLOBALS['pc'] = unannotate_cps
                else:
                    GLOBALS['value_reg'] = x_reg
                    GLOBALS['pc'] = apply_cont

def filename_cache(filename):
    if true_q(hasitem_native(_starfilename_dict_star, filename)):
        return getitem_native(_starfilename_dict_star, filename)
    else:
        index = symbol_undefined
        index = vlist_length_native(_starfilename_vector_star)
        vlist_append_native(_starfilename_vector_star, filename)
        setitem_native(_starfilename_dict_star, filename, index)
        return index

def get_filename_from_index(index):
    return vlist_ref_native(_starfilename_vector_star, index)

def make_info(src, start, end):
    return cons(filename_cache(src), append(start, end))

def replace_info(asexp, new_info):
    if true_q(atom_q_hat(asexp)):
        return List(atom_tag, cadr(asexp), new_info)
    else:
        return List(pair_tag, cadr(asexp), caddr(asexp), new_info)

def get_srcfile(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return get_filename_from_index(car(info))

def get_start_line(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return cadr(info)

def get_start_char(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return caddr(info)

def get_start_pos(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return cadddr(info)

def get_end_line(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return car(cddddr(info))

def get_end_char(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return cadr(cddddr(info))

def get_end_pos(info):
    if true_q((info) is (symbol_none)):
        return symbol_none
    else:
        return caddr(cddddr(info))

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
    return car(x)

def rest_of(x):
    return cdr(x)

def unexpected_token_error():
    token = symbol_undefined
    token = first(tokens_reg)
    if true_q(token_type_q(token, symbol_end_marker)):
        GLOBALS['msg_reg'] = "unexpected end of input"
        GLOBALS['pc'] = read_error
    else:
        GLOBALS['msg_reg'] = format("unexpected '~a' encountered", car(token))
        GLOBALS['pc'] = read_error

def read_error():
    token = symbol_undefined
    token = first(tokens_reg)
    GLOBALS['exception_reg'] = make_exception("ReadError", msg_reg, src_reg, get_token_start_line(token), get_token_start_char(token))
    GLOBALS['pc'] = apply_handler2

def read_sexp():
    start = symbol_undefined
    end = symbol_undefined
    end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if true_q((car(temp_1)) is (symbol_integer)):
        str = symbol_undefined
        str = list_ref(temp_1, 1)
        GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
        GLOBALS['info_reg'] = make_info(src_reg, start, end)
        GLOBALS['x_reg'] = string_to_integer(str)
        GLOBALS['pc'] = annotate_cps
    else:
        if true_q((car(temp_1)) is (symbol_decimal)):
            str = symbol_undefined
            str = list_ref(temp_1, 1)
            GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
            GLOBALS['info_reg'] = make_info(src_reg, start, end)
            GLOBALS['x_reg'] = string_to_decimal(str)
            GLOBALS['pc'] = annotate_cps
        else:
            if true_q((car(temp_1)) is (symbol_rational)):
                str = symbol_undefined
                str = list_ref(temp_1, 1)
                num = symbol_undefined
                num = string_to_rational(str)
                if true_q(true_q(num)):
                    GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    GLOBALS['info_reg'] = make_info(src_reg, start, end)
                    GLOBALS['x_reg'] = num
                    GLOBALS['pc'] = annotate_cps
                else:
                    GLOBALS['msg_reg'] = format("cannot represent ~a", str)
                    GLOBALS['pc'] = read_error
            else:
                if true_q((car(temp_1)) is (symbol_boolean)):
                    bool = symbol_undefined
                    bool = list_ref(temp_1, 1)
                    GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                    GLOBALS['info_reg'] = make_info(src_reg, start, end)
                    GLOBALS['x_reg'] = bool
                    GLOBALS['pc'] = annotate_cps
                else:
                    if true_q((car(temp_1)) is (symbol_character)):
                        char = symbol_undefined
                        char = list_ref(temp_1, 1)
                        GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                        GLOBALS['info_reg'] = make_info(src_reg, start, end)
                        GLOBALS['x_reg'] = char
                        GLOBALS['pc'] = annotate_cps
                    else:
                        if true_q((car(temp_1)) is (symbol_string)):
                            str = symbol_undefined
                            str = list_ref(temp_1, 1)
                            GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                            GLOBALS['info_reg'] = make_info(src_reg, start, end)
                            GLOBALS['x_reg'] = str
                            GLOBALS['pc'] = annotate_cps
                        else:
                            if true_q((car(temp_1)) is (symbol_identifier)):
                                id = symbol_undefined
                                id = list_ref(temp_1, 1)
                                GLOBALS['k_reg'] = make_cont(b_cont_9_d, end, tokens_reg, fail_reg, k_reg)
                                GLOBALS['info_reg'] = make_info(src_reg, start, end)
                                GLOBALS['x_reg'] = id
                                GLOBALS['pc'] = annotate_cps
                            else:
                                if true_q((car(temp_1)) is (symbol_apostrophe)):
                                    GLOBALS['keyword_reg'] = symbol_quote
                                    GLOBALS['pc'] = read_abbreviation
                                else:
                                    if true_q((car(temp_1)) is (symbol_backquote)):
                                        GLOBALS['keyword_reg'] = symbol_quasiquote
                                        GLOBALS['pc'] = read_abbreviation
                                    else:
                                        if true_q((car(temp_1)) is (symbol_comma)):
                                            GLOBALS['keyword_reg'] = symbol_unquote
                                            GLOBALS['pc'] = read_abbreviation
                                        else:
                                            if true_q((car(temp_1)) is (symbol_comma_at)):
                                                GLOBALS['keyword_reg'] = symbol_unquote_splicing
                                                GLOBALS['pc'] = read_abbreviation
                                            else:
                                                if true_q((car(temp_1)) is (symbol_lparen)):
                                                    tokens = symbol_undefined
                                                    tokens = rest_of(tokens_reg)
                                                    GLOBALS['k_reg'] = make_cont4(b_cont4_1_d, src_reg, start, k_reg)
                                                    GLOBALS['expected_terminator_reg'] = symbol_rparen
                                                    GLOBALS['tokens_reg'] = tokens
                                                    GLOBALS['pc'] = read_sexp_sequence
                                                else:
                                                    if true_q((car(temp_1)) is (symbol_lbracket)):
                                                        tokens = symbol_undefined
                                                        tokens = rest_of(tokens_reg)
                                                        GLOBALS['k_reg'] = make_cont4(b_cont4_1_d, src_reg, start, k_reg)
                                                        GLOBALS['expected_terminator_reg'] = symbol_rbracket
                                                        GLOBALS['tokens_reg'] = tokens
                                                        GLOBALS['pc'] = read_sexp_sequence
                                                    else:
                                                        if true_q((car(temp_1)) is (symbol_lvector)):
                                                            GLOBALS['k_reg'] = make_cont4(b_cont4_2_d, src_reg, start, k_reg)
                                                            GLOBALS['tokens_reg'] = rest_of(tokens_reg)
                                                            GLOBALS['pc'] = read_vector_sequence
                                                        else:
                                                            GLOBALS['pc'] = unexpected_token_error

def read_abbreviation():
    start = symbol_undefined
    keyword_end = symbol_undefined
    keyword_end = get_token_end(first(tokens_reg))
    start = get_token_start(first(tokens_reg))
    GLOBALS['k_reg'] = make_cont(b_cont_10_d, src_reg, start, tokens_reg, handler_reg, fail_reg, k_reg)
    GLOBALS['info_reg'] = make_info(src_reg, start, keyword_end)
    GLOBALS['x_reg'] = keyword_reg
    GLOBALS['pc'] = annotate_cps

def read_vector_sequence():
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if true_q((car(temp_1)) is (symbol_rparen)):
        GLOBALS['expected_terminator_reg'] = symbol_rparen
        GLOBALS['sexps_reg'] = symbol_emptylist
        GLOBALS['pc'] = close_sexp_sequence
    else:
        if true_q((car(temp_1)) is (symbol_dot)):
            GLOBALS['msg_reg'] = "unexpected dot (.)"
            GLOBALS['pc'] = read_error
        else:
            GLOBALS['k_reg'] = make_cont4(b_cont4_5_d, src_reg, handler_reg, k_reg)
            GLOBALS['pc'] = read_sexp

def read_sexp_sequence():
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if true_q(memq(car(temp_1), List(symbol_rparen, symbol_rbracket))):
        GLOBALS['sexps_reg'] = symbol_emptylist
        GLOBALS['pc'] = close_sexp_sequence
    else:
        if true_q((car(temp_1)) is (symbol_dot)):
            GLOBALS['msg_reg'] = "unexpected dot (.)"
            GLOBALS['pc'] = read_error
        else:
            GLOBALS['k_reg'] = make_cont4(b_cont4_7_d, expected_terminator_reg, src_reg, handler_reg, k_reg)
            GLOBALS['pc'] = read_sexp

def close_sexp_sequence():
    end = symbol_undefined
    end = get_token_end(first(tokens_reg))
    temp_1 = symbol_undefined
    temp_1 = first(tokens_reg)
    if true_q(memq(car(temp_1), List(symbol_rparen, symbol_rbracket))):
        if true_q(token_type_q(first(tokens_reg), expected_terminator_reg)):
            GLOBALS['value4_reg'] = fail_reg
            GLOBALS['value3_reg'] = rest_of(tokens_reg)
            GLOBALS['value2_reg'] = end
            GLOBALS['value1_reg'] = sexps_reg
            GLOBALS['pc'] = apply_cont4
        else:
            if true_q((expected_terminator_reg) is (symbol_rparen)):
                GLOBALS['msg_reg'] = "parenthesized list terminated by bracket"
                GLOBALS['pc'] = read_error
            else:
                if true_q((expected_terminator_reg) is (symbol_rbracket)):
                    GLOBALS['msg_reg'] = "bracketed list terminated by parenthesis"
                    GLOBALS['pc'] = read_error
    else:
        GLOBALS['pc'] = unexpected_token_error

def make_binding(value, docstring):
    return cons(value, docstring)

def binding_value(binding):
    return car(binding)

def binding_docstring(binding):
    return cdr(binding)

def set_binding_value_b(binding, value):
    set_car_b(binding, value)

def set_binding_docstring_b(binding, docstring):
    set_cdr_b(binding, docstring)

def make_frame(variables, values, docstrings):
    return List(list_to_vector(Map(make_binding, values, docstrings)), variables)

def empty_frame_q(frame):
    return null_q(cadr(frame))

def frame_bindings(frame):
    return car(frame)

def environment_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_environment))

def make_empty_environment():
    return List(symbol_environment, make_frame(symbol_emptylist, symbol_emptylist, symbol_emptylist))

def make_initial_environment(vars, vals, docstrings):
    return List(symbol_environment, make_frame(vars, vals, docstrings))

def first_frame(env):
    return cadr(env)

def first_frame_vars(env):
    return cadr(first_frame(env))

def initial_contours(env):
    return cdr(first_frame(env))

def frames(env):
    return cdr(env)

def add_binding(new_var, new_binding, frame):
    bindings = symbol_undefined
    vars = symbol_undefined
    vars = cadr(frame)
    bindings = vector_to_list(car(frame))
    return List(list_to_vector(append(bindings, List(new_binding))), append(vars, List(new_var)))

def set_first_frame_b(env, new_frame):
    set_car_b(cdr(env), new_frame)

def extend(env, variables, values, docstrings):
    return cons(symbol_environment, cons(make_frame(variables, values, docstrings), cdr(env)))

def search_env(env, variable):
    return search_frames(cdr(env), variable)

def search_frames(frames, variable):
    if true_q(null_q(frames)):
        return False
    else:
        binding = symbol_undefined
        binding = search_frame(car(frames), variable)
        if true_q(binding):
            return binding
        else:
            return search_frames(cdr(frames), variable)

def in_first_frame_q(var, env):
    return true_q(memq(var, first_frame_vars(env)))

def get_first_frame_value(var, env):
    return binding_value(search_frame(first_frame(env), var))

def lookup_value_by_lexical_address():
    bindings = symbol_undefined
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = binding_value(vector_ref(bindings, offset_reg))
    GLOBALS['pc'] = apply_cont2

def lookup_binding_by_lexical_address():
    bindings = symbol_undefined
    bindings = frame_bindings(list_ref(frames_reg, depth_reg))
    GLOBALS['value2_reg'] = fail_reg
    GLOBALS['value1_reg'] = vector_ref(bindings, offset_reg)
    GLOBALS['pc'] = apply_cont2

def lookup_value():
    GLOBALS['sk_reg'] = make_cont2(b_cont2_3_d, k_reg)
    GLOBALS['dk_reg'] = make_cont3(b_cont3_3_d, k_reg)
    GLOBALS['gk_reg'] = make_cont2(b_cont2_4_d, k_reg)
    GLOBALS['pc'] = lookup_variable

def lookup_variable():
    binding = symbol_undefined
    binding = search_env(env_reg, var_reg)
    if true_q(binding):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = binding
        GLOBALS['k_reg'] = sk_reg
        GLOBALS['pc'] = apply_cont2
    else:
        components = symbol_undefined
        components = split_variable(var_reg)
        if true_q((null_q(cdr(components))) and (dlr_env_contains(car(components)))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = car(components)
            GLOBALS['k_reg'] = gk_reg
            GLOBALS['pc'] = apply_cont2
        else:
            if true_q((not(null_q(cdr(components)))) and (dlr_env_contains(car(components))) and (dlr_object_contains(dlr_env_lookup(car(components)), components))):
                GLOBALS['value3_reg'] = fail_reg
                GLOBALS['value2_reg'] = components
                GLOBALS['value1_reg'] = dlr_env_lookup(car(components))
                GLOBALS['k_reg'] = dk_reg
                GLOBALS['pc'] = apply_cont3
            else:
                if true_q(null_q(cdr(components))):
                    GLOBALS['info_reg'] = var_info_reg
                    GLOBALS['msg_reg'] = format("unbound variable '~a'", var_reg)
                    GLOBALS['pc'] = runtime_error
                else:
                    GLOBALS['module_reg'] = env_reg
                    GLOBALS['path_reg'] = ""
                    GLOBALS['components_reg'] = components
                    GLOBALS['pc'] = lookup_variable_components

def lookup_variable_components():
    var = symbol_undefined
    binding = symbol_undefined
    var = car(components_reg)
    binding = search_env(module_reg, var)
    if true_q(binding):
        if true_q(null_q(cdr(components_reg))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = binding
            GLOBALS['k_reg'] = sk_reg
            GLOBALS['pc'] = apply_cont2
        else:
            value = symbol_undefined
            new_path = symbol_undefined
            new_path = (format("~a", var) if string_is__q(path_reg, "") else format("~a.~a", path_reg, var))
            value = binding_value(binding)
            if true_q(environment_q(value)):
                GLOBALS['module_reg'] = value
                GLOBALS['path_reg'] = new_path
                GLOBALS['components_reg'] = cdr(components_reg)
                GLOBALS['pc'] = lookup_variable_components
            else:
                if true_q(dlr_object_contains(value, components_reg)):
                    GLOBALS['value3_reg'] = fail_reg
                    GLOBALS['value2_reg'] = components_reg
                    GLOBALS['value1_reg'] = value
                    GLOBALS['k_reg'] = dk_reg
                    GLOBALS['pc'] = apply_cont3
                else:
                    GLOBALS['info_reg'] = var_info_reg
                    GLOBALS['msg_reg'] = format("'~a' is not a module", new_path)
                    GLOBALS['pc'] = runtime_error
    else:
        if true_q(string_is__q(path_reg, "")):
            GLOBALS['info_reg'] = var_info_reg
            GLOBALS['msg_reg'] = format("undefined item in '~a'", var)
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['info_reg'] = var_info_reg
            GLOBALS['msg_reg'] = format("unbound variable '~a' in module '~a'", var, path_reg)
            GLOBALS['pc'] = runtime_error

def lookup_binding_in_first_frame():
    frame = symbol_undefined
    frame = first_frame(env_reg)
    binding = symbol_undefined
    binding = search_frame(frame, var_reg)
    if true_q(binding):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = binding
        GLOBALS['pc'] = apply_cont2
    else:
        new_binding = symbol_undefined
        new_binding = make_binding(symbol_undefined, "")
        new_frame = symbol_undefined
        new_frame = add_binding(var_reg, new_binding, frame)
        set_first_frame_b(env_reg, new_frame)
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = new_binding
        GLOBALS['pc'] = apply_cont2

def split_variable(var):
    strings = symbol_undefined
    strings = string_split(symbol_to_string(var), make_char('.'))
    if true_q(member("", strings)):
        return symbol_emptylist
    else:
        return Map(string_to_symbol, strings)

def id_q(exp):
    return (symbol_q(exp)) or (association_q(exp))

def head(formals):
    if true_q(symbol_q(formals)):
        return symbol_emptylist
    else:
        if true_q(association_q(formals)):
            return symbol_emptylist
        else:
            if true_q(pair_q(cdr(formals))):
                return cons(car(formals), head(cdr(formals)))
            else:
                return List(car(formals))

def last(formals):
    if true_q(symbol_q(formals)):
        return formals
    else:
        if true_q(association_q(formals)):
            return formals
        else:
            if true_q(pair_q(cdr(formals))):
                return last(cdr(formals))
            else:
                return cdr(formals)

def anything_q(datum):
    return True

def application_q_hat(asexp):
    return (list_q_hat(asexp)) and (not(null_q_hat(asexp))) and (not(reserved_keyword_q(untag_atom_hat(car_hat(asexp)))))

def reserved_keyword_q(x):
    return (symbol_q(x)) and (not((memq(x, get_reserved_keywords())) is (False)))

def get_reserved_keywords():
    return List(symbol_quote, symbol_func, symbol_define_b, symbol_quasiquote, symbol_lambda, symbol_if, symbol_set_b, symbol_define, symbol_begin, symbol_cond, symbol_and, symbol_or, symbol_let, symbol_let_star, symbol_letrec, symbol_case, symbol_record_case, symbol_try, symbol_catch, symbol_finally, symbol_raise, symbol_define_syntax, symbol_choose, symbol_define_datatype, symbol_cases, symbol_trace_lambda)

def mit_style_define_q_hat(asexp):
    return (define_q_hat(asexp)) and (not(symbol_q_hat(cadr_hat(asexp))))

def literal_q(datum):
    return (number_q(datum)) or (boolean_q(datum)) or (null_q(datum)) or (char_q(datum)) or (string_q(datum))

def literal_q_hat(asexp):
    return ((car(asexp)) is (atom_tag)) and ((number_q(untag_atom_hat(asexp))) or (boolean_q(untag_atom_hat(asexp))) or (null_q(untag_atom_hat(asexp))) or (char_q(untag_atom_hat(asexp))) or (string_q(untag_atom_hat(asexp))))

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
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    if true_q(literal_q_hat(adatum_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = lit_aexp(untag_atom_hat(adatum_reg), info)
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(symbol_q_hat(adatum_reg)):
            if true_q(_staruse_lexical_address_star):
                GLOBALS['info_reg'] = info
                GLOBALS['depth_reg'] = 0
                GLOBALS['id_reg'] = untag_atom_hat(adatum_reg)
                GLOBALS['pc'] = get_lexical_address
            else:
                GLOBALS['value2_reg'] = fail_reg
                GLOBALS['value1_reg'] = var_aexp(untag_atom_hat(adatum_reg), info)
                GLOBALS['pc'] = apply_cont2
        else:
            if true_q(vector_q_hat(adatum_reg)):
                GLOBALS['k_reg'] = make_cont(b_cont_19_d, info, fail_reg, k_reg)
                GLOBALS['x_reg'] = adatum_reg
                GLOBALS['pc'] = unannotate_cps
            else:
                if true_q(quote_q_hat(adatum_reg)):
                    GLOBALS['k_reg'] = make_cont(b_cont_20_d, info, fail_reg, k_reg)
                    GLOBALS['x_reg'] = adatum_reg
                    GLOBALS['pc'] = unannotate_cps
                else:
                    if true_q(quasiquote_q_hat(adatum_reg)):
                        GLOBALS['k_reg'] = make_cont(b_cont_18_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                        GLOBALS['depth_reg'] = 0
                        GLOBALS['ax_reg'] = cadr_hat(adatum_reg)
                        GLOBALS['pc'] = qq_expand_cps
                    else:
                        if true_q(unquote_q_hat(adatum_reg)):
                            GLOBALS['msg_reg'] = "misplaced"
                            GLOBALS['pc'] = aparse_error
                        else:
                            if true_q(unquote_splicing_q_hat(adatum_reg)):
                                GLOBALS['msg_reg'] = "misplaced"
                                GLOBALS['pc'] = aparse_error
                            else:
                                if true_q(syntactic_sugar_q_hat(adatum_reg)):
                                    GLOBALS['k_reg'] = make_cont2(b_cont2_35_d, senv_reg, handler_reg, k_reg)
                                    GLOBALS['pc'] = expand_once_hat
                                else:
                                    if true_q(if_then_q_hat(adatum_reg)):
                                        GLOBALS['k_reg'] = make_cont2(b_cont2_31_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                        GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                        GLOBALS['pc'] = aparse
                                    else:
                                        if true_q(if_else_q_hat(adatum_reg)):
                                            GLOBALS['k_reg'] = make_cont2(b_cont2_34_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                            GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                            GLOBALS['pc'] = aparse
                                        else:
                                            if true_q(help_q_hat(adatum_reg)):
                                                var_info = symbol_undefined
                                                var_info = get_source_info(cadr_hat(adatum_reg))
                                                GLOBALS['value2_reg'] = fail_reg
                                                GLOBALS['value1_reg'] = help_aexp(untag_atom_hat(cadr_hat(adatum_reg)), var_info, info)
                                                GLOBALS['pc'] = apply_cont2
                                            else:
                                                if true_q(assignment_q_hat(adatum_reg)):
                                                    GLOBALS['k_reg'] = make_cont2(b_cont2_29_d, adatum_reg, info, k_reg)
                                                    GLOBALS['adatum_reg'] = caddr_hat(adatum_reg)
                                                    GLOBALS['pc'] = aparse
                                                else:
                                                    if true_q(association_q_hat(adatum_reg)):
                                                        GLOBALS['k_reg'] = make_cont2(b_cont2_27_d, adatum_reg, info, k_reg)
                                                        GLOBALS['adatum_reg'] = caddr_hat(adatum_reg)
                                                        GLOBALS['pc'] = aparse
                                                    else:
                                                        if true_q(func_q_hat(adatum_reg)):
                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_28_d, info, k_reg)
                                                            GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                                            GLOBALS['pc'] = aparse
                                                        else:
                                                            if true_q(callback_q_hat(adatum_reg)):
                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_24_d, info, k_reg)
                                                                GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                                                GLOBALS['pc'] = aparse
                                                            else:
                                                                if true_q(define_q_hat(adatum_reg)):
                                                                    if true_q(mit_style_define_q_hat(adatum_reg)):
                                                                        GLOBALS['k_reg'] = make_cont(b_cont_15_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                        GLOBALS['datum_reg'] = adatum_reg
                                                                        GLOBALS['macro_reg'] = mit_define_transformer_hat
                                                                        GLOBALS['pc'] = apply_macro
                                                                    else:
                                                                        if true_q(numeric_equal(length_hat(adatum_reg), 3)):
                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_26_d, adatum_reg, info, k_reg)
                                                                            GLOBALS['adatum_reg'] = caddr_hat(adatum_reg)
                                                                            GLOBALS['pc'] = aparse
                                                                        else:
                                                                            if true_q((numeric_equal(length_hat(adatum_reg), 4)) and (string_q_hat(caddr_hat(adatum_reg)))):
                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_25_d, adatum_reg, info, k_reg)
                                                                                GLOBALS['adatum_reg'] = cadddr_hat(adatum_reg)
                                                                                GLOBALS['pc'] = aparse
                                                                            else:
                                                                                GLOBALS['msg_reg'] = "bad concrete syntax:"
                                                                                GLOBALS['pc'] = aparse_error
                                                                else:
                                                                    if true_q(define_b_q_hat(adatum_reg)):
                                                                        if true_q(mit_style_define_q_hat(adatum_reg)):
                                                                            GLOBALS['k_reg'] = make_cont(b_cont_15_d, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                            GLOBALS['datum_reg'] = adatum_reg
                                                                            GLOBALS['macro_reg'] = mit_define_transformer_hat
                                                                            GLOBALS['pc'] = apply_macro
                                                                        else:
                                                                            if true_q(numeric_equal(length_hat(adatum_reg), 3)):
                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_22_d, adatum_reg, info, k_reg)
                                                                                GLOBALS['adatum_reg'] = caddr_hat(adatum_reg)
                                                                                GLOBALS['pc'] = aparse
                                                                            else:
                                                                                if true_q((numeric_equal(length_hat(adatum_reg), 4)) and (string_q_hat(caddr_hat(adatum_reg)))):
                                                                                    GLOBALS['k_reg'] = make_cont2(b_cont2_21_d, adatum_reg, info, k_reg)
                                                                                    GLOBALS['adatum_reg'] = cadddr_hat(adatum_reg)
                                                                                    GLOBALS['pc'] = aparse
                                                                                else:
                                                                                    GLOBALS['msg_reg'] = "bad concrete syntax:"
                                                                                    GLOBALS['pc'] = aparse_error
                                                                    else:
                                                                        if true_q(define_syntax_q_hat(adatum_reg)):
                                                                            name = symbol_undefined
                                                                            name = define_var_hat(adatum_reg)
                                                                            if true_q(lambda_q_hat(caddr_hat(adatum_reg))):
                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_23_d, name, info, k_reg)
                                                                                GLOBALS['adatum_reg'] = caddr_hat(adatum_reg)
                                                                                GLOBALS['pc'] = aparse
                                                                            else:
                                                                                aclauses = symbol_undefined
                                                                                aclauses = cddr_hat(adatum_reg)
                                                                                GLOBALS['k_reg'] = make_cont(b_cont_16_d, aclauses, name, info, fail_reg, k_reg)
                                                                                GLOBALS['x_reg'] = aclauses
                                                                                GLOBALS['pc'] = unannotate_cps
                                                                        else:
                                                                            if true_q(define_tests_q_hat(adatum_reg)):
                                                                                name = symbol_undefined
                                                                                aclauses = symbol_undefined
                                                                                aclauses = cddr_hat(adatum_reg)
                                                                                name = define_var_hat(adatum_reg)
                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_19_d, name, info, k_reg)
                                                                                GLOBALS['adatum_list_reg'] = aclauses
                                                                                GLOBALS['pc'] = aparse_all
                                                                            else:
                                                                                if true_q(run_tests_q_hat(adatum_reg)):
                                                                                    args = symbol_undefined
                                                                                    args = cdr_hat(adatum_reg)
                                                                                    if true_q(null_q_hat(args)):
                                                                                        GLOBALS['value2_reg'] = fail_reg
                                                                                        GLOBALS['value1_reg'] = run_tests_aexp(symbol_emptylist)
                                                                                        GLOBALS['pc'] = apply_cont2
                                                                                    else:
                                                                                        if true_q((symbol_q_hat(car_hat(args))) and (list_of_test_groups_q_hat(cdr_hat(args)))):
                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_20_d, k_reg)
                                                                                            GLOBALS['args_reg'] = list_hat(args)
                                                                                            GLOBALS['pc'] = aparse_unit_tests
                                                                                        else:
                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_20_d, k_reg)
                                                                                            GLOBALS['args_reg'] = args
                                                                                            GLOBALS['pc'] = aparse_unit_tests
                                                                                else:
                                                                                    if true_q(begin_q_hat(adatum_reg)):
                                                                                        if true_q(null_q_hat(cdr_hat(adatum_reg))):
                                                                                            GLOBALS['msg_reg'] = "bad concrete syntax:"
                                                                                            GLOBALS['pc'] = aparse_error
                                                                                        else:
                                                                                            if true_q(null_q_hat(cddr_hat(adatum_reg))):
                                                                                                GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                                                                                GLOBALS['pc'] = aparse
                                                                                            else:
                                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_17_d, info, k_reg)
                                                                                                GLOBALS['adatum_list_reg'] = cdr_hat(adatum_reg)
                                                                                                GLOBALS['pc'] = aparse_all
                                                                                    else:
                                                                                        if true_q(lambda_no_defines_q_hat(adatum_reg)):
                                                                                            GLOBALS['k_reg'] = make_cont(b_cont_13_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                            GLOBALS['x_reg'] = cadr_hat(adatum_reg)
                                                                                            GLOBALS['pc'] = unannotate_cps
                                                                                        else:
                                                                                            if true_q(trace_lambda_no_defines_q_hat(adatum_reg)):
                                                                                                GLOBALS['k_reg'] = make_cont(b_cont_12_d, adatum_reg, senv_reg, info, handler_reg, fail_reg, k_reg)
                                                                                                GLOBALS['x_reg'] = caddr_hat(adatum_reg)
                                                                                                GLOBALS['pc'] = unannotate_cps
                                                                                            else:
                                                                                                if true_q(try_q_hat(adatum_reg)):
                                                                                                    if true_q(numeric_equal(length_hat(adatum_reg), 2)):
                                                                                                        GLOBALS['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                        GLOBALS['pc'] = aparse
                                                                                                    else:
                                                                                                        if true_q((numeric_equal(length_hat(adatum_reg), 3)) and (catch_q_hat(caddr_hat(adatum_reg)))):
                                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_16_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                            GLOBALS['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                            GLOBALS['pc'] = aparse
                                                                                                        else:
                                                                                                            if true_q((numeric_equal(length_hat(adatum_reg), 3)) and (finally_q_hat(caddr_hat(adatum_reg)))):
                                                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_11_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                GLOBALS['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                                GLOBALS['pc'] = aparse
                                                                                                            else:
                                                                                                                if true_q((numeric_equal(length_hat(adatum_reg), 4)) and (catch_q_hat(caddr_hat(adatum_reg))) and (finally_q_hat(cadddr_hat(adatum_reg)))):
                                                                                                                    GLOBALS['k_reg'] = make_cont2(b_cont2_14_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                    GLOBALS['adatum_reg'] = try_body_hat(adatum_reg)
                                                                                                                    GLOBALS['pc'] = aparse
                                                                                                                else:
                                                                                                                    GLOBALS['msg_reg'] = "bad try syntax:"
                                                                                                                    GLOBALS['pc'] = aparse_error
                                                                                                else:
                                                                                                    if true_q(raise_q_hat(adatum_reg)):
                                                                                                        GLOBALS['k_reg'] = make_cont2(b_cont2_7_d, info, k_reg)
                                                                                                        GLOBALS['adatum_reg'] = cadr_hat(adatum_reg)
                                                                                                        GLOBALS['pc'] = aparse
                                                                                                    else:
                                                                                                        if true_q(choose_q_hat(adatum_reg)):
                                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_8_d, info, k_reg)
                                                                                                            GLOBALS['adatum_list_reg'] = cdr_hat(adatum_reg)
                                                                                                            GLOBALS['pc'] = aparse_all
                                                                                                        else:
                                                                                                            if true_q(application_q_hat(adatum_reg)):
                                                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_6_d, adatum_reg, senv_reg, info, handler_reg, k_reg)
                                                                                                                GLOBALS['adatum_reg'] = car_hat(adatum_reg)
                                                                                                                GLOBALS['pc'] = aparse
                                                                                                            else:
                                                                                                                GLOBALS['msg_reg'] = "bad concrete syntax:"
                                                                                                                GLOBALS['pc'] = aparse_error

def aparse_unit_tests():
    if true_q(null_q_hat(args_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(symbol_q_hat(car_hat(args_reg))):
            GLOBALS['k_reg'] = make_cont2(b_cont2_37_d, args_reg, k_reg)
            GLOBALS['args_reg'] = cdr_hat(args_reg)
            GLOBALS['pc'] = aparse_unit_tests
        else:
            if true_q((list_q_hat(car_hat(args_reg))) and (not(null_q_hat(car_hat(args_reg)))) and (symbol_q_hat(caar_hat(args_reg))) and (list_of_test_groups_q_hat(cdar_hat(args_reg)))):
                GLOBALS['k_reg'] = make_cont2(b_cont2_36_d, args_reg, k_reg)
                GLOBALS['args_reg'] = cdr_hat(args_reg)
                GLOBALS['pc'] = aparse_unit_tests
            else:
                GLOBALS['adatum_reg'] = car_hat(args_reg)
                GLOBALS['msg_reg'] = "bad unit test syntax:"
                GLOBALS['pc'] = aparse_error

def aparse_all():
    if true_q(null_q_hat(adatum_list_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_39_d, adatum_list_reg, senv_reg, handler_reg, k_reg)
        GLOBALS['adatum_reg'] = car_hat(adatum_list_reg)
        GLOBALS['pc'] = aparse

def aparse_error():
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    GLOBALS['k_reg'] = make_cont(b_cont_22_d, msg_reg, info, handler_reg, fail_reg)
    GLOBALS['x_reg'] = adatum_reg
    GLOBALS['pc'] = unannotate_cps

def aparse_sexps():
    if true_q(token_type_q(first(tokens_reg), symbol_end_marker)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont4(b_cont4_9_d, senv_reg, src_reg, handler_reg, k_reg)
        GLOBALS['pc'] = read_sexp

def get_lexical_address():
    if true_q(null_q(senv_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = var_aexp(id_reg, info_reg)
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(memq(id_reg, car(senv_reg))):
            GLOBALS['offset_reg'] = 0
            GLOBALS['contours_reg'] = car(senv_reg)
            GLOBALS['pc'] = get_lexical_address_offset
        else:
            GLOBALS['depth_reg'] = (depth_reg) + (1)
            GLOBALS['senv_reg'] = cdr(senv_reg)
            GLOBALS['pc'] = get_lexical_address

def get_lexical_address_offset():
    if true_q((car(contours_reg)) is (id_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = lexical_address_aexp(depth_reg, offset_reg, id_reg, info_reg)
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['offset_reg'] = (offset_reg) + (1)
        GLOBALS['contours_reg'] = cdr(contours_reg)
        GLOBALS['pc'] = get_lexical_address_offset

def get_internal_defines_hat(bodies, adatum, handler, fail, k):
    if true_q(null_q_hat(bodies)):
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['adatum_reg'] = adatum
        GLOBALS['msg_reg'] = "no body expressions found for"
        GLOBALS['pc'] = aparse_error
    else:
        if true_q(define_q_hat(car_hat(bodies))):
            return get_internal_defines_hat(cdr_hat(bodies), adatum, handler, fail, make_cont2(b_cont2_44_d, bodies, k))
        else:
            return any_internal_defines_q_hat(cdr_hat(bodies), make_cont(b_cont_25_d, adatum, bodies, handler, fail, k))

def any_internal_defines_q_hat(exps, k):
    if true_q(null_q_hat(exps)):
        GLOBALS['value_reg'] = False
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(define_q_hat(car_hat(exps))):
            GLOBALS['value_reg'] = True
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont
        else:
            return any_internal_defines_q_hat(cdr_hat(exps), k)

def create_letrec_bindings_hat(defines, handler, fail, k):
    if true_q(null_q(defines)):
        GLOBALS['value_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont
    else:
        return create_letrec_bindings_hat(cdr(defines), handler, fail, make_cont(b_cont_26_d, defines, handler, fail, k))

def get_define_var_and_exp_hat(adatum, handler, fail, k):
    if true_q(mit_style_define_q_hat(adatum)):
        name = symbol_undefined
        formals = symbol_undefined
        bodies = symbol_undefined
        bodies = cddr_hat(adatum)
        formals = cdadr_hat(adatum)
        name = caadr_hat(adatum)
        GLOBALS['value2_reg'] = append(List(symbol_lambda), append(List(formals), at_hat(bodies)))
        GLOBALS['value1_reg'] = name
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(numeric_equal(length_hat(adatum), 3)):
            name = symbol_undefined
            exp = symbol_undefined
            exp = caddr_hat(adatum)
            name = define_var_hat(adatum)
            GLOBALS['value2_reg'] = exp
            GLOBALS['value1_reg'] = name
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont2
        else:
            if true_q((numeric_equal(length_hat(adatum), 4)) and (string_q_hat(caddr_hat(adatum)))):
                name = symbol_undefined
                exp = symbol_undefined
                exp = cadddr_hat(adatum)
                name = define_var_hat(adatum)
                GLOBALS['value2_reg'] = exp
                GLOBALS['value1_reg'] = name
                GLOBALS['k_reg'] = k
                GLOBALS['pc'] = apply_cont2
            else:
                GLOBALS['fail_reg'] = fail
                GLOBALS['handler_reg'] = handler
                GLOBALS['adatum_reg'] = adatum
                GLOBALS['msg_reg'] = "bad concrete syntax:"
                GLOBALS['pc'] = aparse_error

def create_letrec_assignments_hat():
    if true_q(null_q_hat(vars_reg)):
        GLOBALS['value2_reg'] = symbol_emptylist
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_47_d, procs_reg, vars_reg, k2_reg)
        GLOBALS['procs_reg'] = cdr_hat(procs_reg)
        GLOBALS['vars_reg'] = cdr_hat(vars_reg)
        GLOBALS['pc'] = create_letrec_assignments_hat

def amacro_error():
    info = symbol_undefined
    info = get_source_info(adatum_reg)
    GLOBALS['exception_reg'] = make_exception("MacroError", msg_reg, get_start_line(info), get_srcfile(info), get_start_char(info))
    GLOBALS['pc'] = apply_handler2

def nest_let_star_bindings_hat():
    if true_q((null_q_hat(bindings_reg)) or (null_q_hat(cdr_hat(bindings_reg)))):
        GLOBALS['value_reg'] = append(List(symbol_let), append(List(bindings_reg), at_hat(bodies_reg)))
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_27_d, bindings_reg, k_reg)
        GLOBALS['bindings_reg'] = cdr_hat(bindings_reg)
        GLOBALS['pc'] = nest_let_star_bindings_hat

def case_clauses_to_simple_cond_clauses_hat():
    if true_q(null_q_hat(clauses_reg)):
        GLOBALS['value_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_28_d, clauses_reg, var_reg, k_reg)
        GLOBALS['clauses_reg'] = cdr_hat(clauses_reg)
        GLOBALS['pc'] = case_clauses_to_simple_cond_clauses_hat

def case_clauses_to_cond_clauses_hat():
    if true_q(null_q_hat(clauses_reg)):
        GLOBALS['value2_reg'] = symbol_emptylist
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_49_d, clauses_reg, var_reg, k2_reg)
        GLOBALS['clauses_reg'] = cdr_hat(clauses_reg)
        GLOBALS['pc'] = case_clauses_to_cond_clauses_hat

def record_case_clauses_to_cond_clauses_hat():
    if true_q(null_q_hat(clauses_reg)):
        GLOBALS['value2_reg'] = symbol_emptylist
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_50_d, clauses_reg, var_reg, k2_reg)
        GLOBALS['clauses_reg'] = cdr_hat(clauses_reg)
        GLOBALS['pc'] = record_case_clauses_to_cond_clauses_hat

def make_dd_variant_constructors_hat():
    if true_q(null_q_hat(variants_reg)):
        GLOBALS['value2_reg'] = symbol_emptylist
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_53_d, variants_reg, k2_reg)
        GLOBALS['variant_reg'] = car_hat(variants_reg)
        GLOBALS['pc'] = make_dd_variant_constructor_hat

def make_dd_variant_constructor_hat():
    name = symbol_undefined
    fields = symbol_undefined
    fields = cdr_hat(variant_reg)
    name = car_hat(variant_reg)
    GLOBALS['k_reg'] = make_cont(b_cont_29_d, fields, name, k2_reg)
    GLOBALS['cdrs_reg'] = symbol_args
    GLOBALS['fields_reg'] = fields
    GLOBALS['name_reg'] = name
    GLOBALS['pc'] = verify_dd_constructor_fields_hat

def verify_dd_constructor_fields_hat():
    if true_q(null_q_hat(fields_reg)):
        GLOBALS['value_reg'] = append(List(symbol_cons), append(List(append(List(symbol_quote), List(name_reg))), List(symbol_args)))
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_30_d, cdrs_reg, fields_reg, name_reg, k_reg)
        GLOBALS['cdrs_reg'] = append(List(symbol_cdr), List(cdrs_reg))
        GLOBALS['fields_reg'] = cdr_hat(fields_reg)
        GLOBALS['pc'] = verify_dd_constructor_fields_hat

def make_macro_env_hat():
    return make_initial_environment(List(symbol_lambda, symbol_λ, symbol_trace_lambda, symbol_and, symbol_or, symbol_cond, symbol_let, symbol_letrec, symbol_let_star, symbol_case, symbol_record_case, symbol_define_datatype, symbol_cases), List(lambda_transformer_hat, lambda_transformer_hat, trace_lambda_transformer_hat, and_transformer_hat, or_transformer_hat, cond_transformer_hat, let_transformer_hat, letrec_transformer_hat, let_star_transformer_hat, case_transformer_hat, record_case_transformer_hat, define_datatype_transformer_hat, cases_transformer_hat), List(string_append("(lambda ...) - lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(lambda ...) - lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(trace-lambda name ...) - trace-lambda with internal definitions", "\n", "Example:\n", "    In  [1]: (trace-lambda name (a b) (define c 3) (list a b c))\n", "    Out [1]: <procedure>\n"), string_append("(and ...) - short-circuiting `and` macro\n", "\n", "Example:\n", "    In  [1]: (and)\n", "    Out [1]: #t\n", "    In  [2]: (and #t #f)\n", "    Out [2]: #f\n"), string_append("(or ...) - short-circuiting `or` macro", "\n", "Example:\n", "    In  [1]: (or)\n", "    Out [1]: #f\n", "    In  [2]: (or #t #f)\n", "    Out [2]: #t\n"), string_append("(cond (TEST RETURN)...) - conditional evaluation macro", "\n", "Example:\n", "    In  [1]: (cond ((= 1 2) 3)(else 4))\n", "    Out [1]: 4\n"), string_append("(let ((VAR VALUE)...)...) - local variable macro", "\n", "Example:\n", "    In  [1]: (let ((x 3)) x)\n", "    Out [1]: 3\n"), string_append("(letrec ((VAR VALUE)...)...) - recursive local variable macro", "\n", "Example:\n", "    In  [*]: (letrec ((loop (lambda () (loop)))) (loop))\n"), string_append("(let* ((VAR VALUE)...)...) - cascading local variable macro", "\n", "Example:\n", "    In  [1]: (let* ((a 1)(b a)(c b)) c)\n", "    Out [1]: 1\n"), string_append("(case THING (ITEM RETURN)...)) - case macro", "\n", "Example:\n", "    In  [1]: (case 1 (1 2)(3 4))\n", "    Out [1]: 2\n"), string_append("(record-case ) - record-case macro for define-datatype", "\n", "Example:\n", "    In  [1]: (record-case ddtype (subtype (part...) return)...)\n"), string_append("(define-datatype NAME NAME? (TYPE (PART TEST))...) - defines new datatypes and support functions (macro)", "\n", "Example:\n", "    In  [1]: (define-datatype e e?)\n", "    In  [1]: (e? 1)\n", "    Out [1]: #f\n"), string_append("(cases ...) - cases macro for a more flexible case", "\n", "Example:\n", "    In  [1]: (cases 1 ((1 2) 3))\n", "    Out [1]: 3\n")))

def make_pattern_macro_hat(clauses, aclauses):
    return List(symbol_pattern_macro, clauses, aclauses)

def pattern_macro_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_pattern_macro))

def macro_clauses(macro):
    return cadr(macro)

def macro_aclauses(macro):
    return caddr(macro)

def define_syntax_clause_q(x):
    return (list_q(x)) and (numeric_equal(length(x), 2)) and (pattern_q(car(x))) and (pattern_q(cadr(x)))

def define_syntax_clause_q_hat(x):
    return (list_q_hat(x)) and (numeric_equal(length_hat(x), 2)) and (apattern_q(car_hat(x))) and (apattern_q(cadr_hat(x)))

def apattern_q(x):
    return (aatom_q(x)) or ((apair_q(x)) and (apattern_q(cadr(x))) and (apattern_q(caddr(x))))

def list_of_define_syntax_clauses_q_hat(alist):
    return (null_q_hat(alist)) or ((define_syntax_clause_q_hat(car_hat(alist))) and (list_of_define_syntax_clauses_q_hat(cdr_hat(alist))))

def expand_once_hat():
    macro_keyword = symbol_undefined
    macro_keyword = untag_atom_hat(car_hat(adatum_reg))
    macro = symbol_undefined
    macro = get_first_frame_value(macro_keyword, macro_env)
    if true_q(pattern_macro_q(macro)):
        GLOBALS['k_reg'] = make_cont2(b_cont2_55_d, macro_keyword, k_reg)
        GLOBALS['aclauses_reg'] = macro_aclauses(macro)
        GLOBALS['clauses_reg'] = macro_clauses(macro)
        GLOBALS['pc'] = process_macro_clauses_hat
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_32_d, adatum_reg, macro_keyword, fail_reg, k_reg)
        GLOBALS['datum_reg'] = adatum_reg
        GLOBALS['macro_reg'] = macro
        GLOBALS['pc'] = apply_macro

def process_macro_clauses_hat():
    if true_q(null_q(clauses_reg)):
        GLOBALS['msg_reg'] = "no matching clause found for"
        GLOBALS['pc'] = aparse_error
    else:
        left_pattern = symbol_undefined
        right_pattern = symbol_undefined
        left_apattern = symbol_undefined
        right_apattern = symbol_undefined
        right_apattern = cadar_hat(aclauses_reg)
        left_apattern = caar_hat(aclauses_reg)
        right_pattern = cadar(clauses_reg)
        left_pattern = caar(clauses_reg)
        GLOBALS['k_reg'] = make_cont(b_cont_34_d, aclauses_reg, adatum_reg, clauses_reg, left_apattern, left_pattern, right_apattern, right_pattern, handler_reg, fail_reg, k_reg)
        GLOBALS['x_reg'] = adatum_reg
        GLOBALS['pc'] = unannotate_cps

def qq_expand_cps():
    if true_q(quasiquote_q_hat(ax_reg)):
        GLOBALS['k_reg'] = make_cont(b_cont_39_d, k_reg)
        GLOBALS['depth_reg'] = (depth_reg) + (1)
        GLOBALS['ax_reg'] = cdr_hat(ax_reg)
        GLOBALS['pc'] = qq_expand_cps
    else:
        if true_q((unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg))):
            if true_q(GreaterThan(depth_reg, 0)):
                GLOBALS['k_reg'] = make_cont(b_cont_40_d, ax_reg, k_reg)
                GLOBALS['depth_reg'] = (depth_reg) - (1)
                GLOBALS['ax_reg'] = cdr_hat(ax_reg)
                GLOBALS['pc'] = qq_expand_cps
            else:
                if true_q((unquote_q_hat(ax_reg)) and (not(null_q_hat(cdr_hat(ax_reg)))) and (null_q_hat(cddr_hat(ax_reg)))):
                    GLOBALS['value_reg'] = cadr_hat(ax_reg)
                    GLOBALS['pc'] = apply_cont
                else:
                    GLOBALS['value_reg'] = append(List(symbol_quote), List(ax_reg))
                    GLOBALS['pc'] = apply_cont
        else:
            if true_q(vector_q_hat(ax_reg)):
                GLOBALS['k_reg'] = make_cont(b_cont_38_d, depth_reg, k_reg)
                GLOBALS['info_reg'] = symbol_none
                GLOBALS['x_reg'] = vector_to_list_hat(ax_reg)
                GLOBALS['pc'] = annotate_cps
            else:
                if true_q(not(pair_q_hat(ax_reg))):
                    GLOBALS['value_reg'] = append(List(symbol_quote), List(ax_reg))
                    GLOBALS['pc'] = apply_cont
                else:
                    if true_q(null_q_hat(cdr_hat(ax_reg))):
                        GLOBALS['ax_reg'] = car_hat(ax_reg)
                        GLOBALS['pc'] = qq_expand_list_cps
                    else:
                        GLOBALS['k_reg'] = make_cont(b_cont_36_d, ax_reg, depth_reg, k_reg)
                        GLOBALS['ax_reg'] = car_hat(ax_reg)
                        GLOBALS['pc'] = qq_expand_list_cps

def qq_expand_list_cps():
    if true_q(quasiquote_q_hat(ax_reg)):
        GLOBALS['k_reg'] = make_cont(b_cont_44_d, k_reg)
        GLOBALS['depth_reg'] = (depth_reg) + (1)
        GLOBALS['ax_reg'] = cdr_hat(ax_reg)
        GLOBALS['pc'] = qq_expand_cps
    else:
        if true_q((unquote_q_hat(ax_reg)) or (unquote_splicing_q_hat(ax_reg))):
            if true_q(GreaterThan(depth_reg, 0)):
                GLOBALS['k_reg'] = make_cont(b_cont_45_d, ax_reg, k_reg)
                GLOBALS['depth_reg'] = (depth_reg) - (1)
                GLOBALS['ax_reg'] = cdr_hat(ax_reg)
                GLOBALS['pc'] = qq_expand_cps
            else:
                if true_q(unquote_q_hat(ax_reg)):
                    GLOBALS['value_reg'] = append(List(symbol_List), cdr_hat(ax_reg))
                    GLOBALS['pc'] = apply_cont
                else:
                    if true_q(null_q_hat(cddr_hat(ax_reg))):
                        GLOBALS['value_reg'] = cadr_hat(ax_reg)
                        GLOBALS['pc'] = apply_cont
                    else:
                        GLOBALS['value_reg'] = append(List(symbol_append), cdr_hat(ax_reg))
                        GLOBALS['pc'] = apply_cont
        else:
            if true_q(vector_q_hat(ax_reg)):
                GLOBALS['k_reg'] = make_cont(b_cont_41_d, k_reg)
                GLOBALS['pc'] = qq_expand_cps
            else:
                if true_q(not(pair_q_hat(ax_reg))):
                    GLOBALS['value_reg'] = append(List(symbol_quote), List(List(ax_reg)))
                    GLOBALS['pc'] = apply_cont
                else:
                    if true_q(null_q_hat(cdr_hat(ax_reg))):
                        GLOBALS['k_reg'] = make_cont(b_cont_41_d, k_reg)
                        GLOBALS['ax_reg'] = car_hat(ax_reg)
                        GLOBALS['pc'] = qq_expand_list_cps
                    else:
                        GLOBALS['k_reg'] = make_cont(b_cont_43_d, ax_reg, depth_reg, k_reg)
                        GLOBALS['ax_reg'] = car_hat(ax_reg)
                        GLOBALS['pc'] = qq_expand_list_cps

def aunparse(aexp):
    if true_q((car(aexp)) is (symbol_lit_aexp)):
        datum = symbol_undefined
        datum = list_ref(aexp, 1)
        if true_q(literal_q(datum)):
            return datum
        else:
            if true_q(vector_q(datum)):
                return datum
            else:
                return append(List(symbol_quote), List(datum))
    else:
        if true_q((car(aexp)) is (symbol_var_aexp)):
            id = symbol_undefined
            id = list_ref(aexp, 1)
            return id
        else:
            if true_q((car(aexp)) is (symbol_lexical_address_aexp)):
                id = symbol_undefined
                id = list_ref(aexp, 3)
                return id
            else:
                if true_q((car(aexp)) is (symbol_if_aexp)):
                    test_aexp = symbol_undefined
                    then_aexp = symbol_undefined
                    else_aexp = symbol_undefined
                    else_aexp = list_ref(aexp, 3)
                    then_aexp = list_ref(aexp, 2)
                    test_aexp = list_ref(aexp, 1)
                    return append(List(symbol_if), append(List(aunparse(test_aexp)), append(List(aunparse(then_aexp)), List(aunparse(else_aexp)))))
                else:
                    if true_q((car(aexp)) is (symbol_assign_aexp)):
                        var = symbol_undefined
                        rhs_exp = symbol_undefined
                        rhs_exp = list_ref(aexp, 2)
                        var = list_ref(aexp, 1)
                        return append(List(symbol_set_b), append(List(var), List(aunparse(rhs_exp))))
                    else:
                        if true_q((car(aexp)) is (symbol_func_aexp)):
                            exp = symbol_undefined
                            exp = list_ref(aexp, 1)
                            return append(List(symbol_func), List(aunparse(exp)))
                        else:
                            if true_q((car(aexp)) is (symbol_callback_aexp)):
                                exp = symbol_undefined
                                exp = list_ref(aexp, 1)
                                return append(List(symbol_callback), List(aunparse(exp)))
                            else:
                                if true_q((car(aexp)) is (symbol_define_aexp)):
                                    id = symbol_undefined
                                    docstring = symbol_undefined
                                    rhs_exp = symbol_undefined
                                    rhs_exp = list_ref(aexp, 3)
                                    docstring = list_ref(aexp, 2)
                                    id = list_ref(aexp, 1)
                                    if true_q(string_is__q(docstring, "")):
                                        return append(List(symbol_define), append(List(id), List(aunparse(rhs_exp))))
                                    else:
                                        return append(List(symbol_define), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                else:
                                    if true_q((car(aexp)) is (symbol_define_b_aexp)):
                                        id = symbol_undefined
                                        docstring = symbol_undefined
                                        rhs_exp = symbol_undefined
                                        rhs_exp = list_ref(aexp, 3)
                                        docstring = list_ref(aexp, 2)
                                        id = list_ref(aexp, 1)
                                        if true_q(string_is__q(docstring, "")):
                                            return append(List(symbol_define_b), append(List(id), List(aunparse(rhs_exp))))
                                        else:
                                            return append(List(symbol_define_b), append(List(id), append(List(docstring), List(aunparse(rhs_exp)))))
                                    else:
                                        if true_q((car(aexp)) is (symbol_define_syntax_aexp)):
                                            name = symbol_undefined
                                            clauses = symbol_undefined
                                            clauses = list_ref(aexp, 2)
                                            name = list_ref(aexp, 1)
                                            return append(List(symbol_define_syntax), append(List(name), clauses))
                                        else:
                                            if true_q((car(aexp)) is (symbol_begin_aexp)):
                                                exps = symbol_undefined
                                                exps = list_ref(aexp, 1)
                                                return append(List(symbol_begin), Map(aunparse, exps))
                                            else:
                                                if true_q((car(aexp)) is (symbol_lambda_aexp)):
                                                    formals = symbol_undefined
                                                    bodies = symbol_undefined
                                                    bodies = list_ref(aexp, 2)
                                                    formals = list_ref(aexp, 1)
                                                    return append(List(symbol_lambda), append(List(formals), Map(aunparse, bodies)))
                                                else:
                                                    if true_q((car(aexp)) is (symbol_mu_lambda_aexp)):
                                                        formals = symbol_undefined
                                                        runt = symbol_undefined
                                                        bodies = symbol_undefined
                                                        bodies = list_ref(aexp, 3)
                                                        runt = list_ref(aexp, 2)
                                                        formals = list_ref(aexp, 1)
                                                        return append(List(symbol_lambda), append(List(append(formals, runt)), Map(aunparse, bodies)))
                                                    else:
                                                        if true_q((car(aexp)) is (symbol_app_aexp)):
                                                            operator = symbol_undefined
                                                            operands = symbol_undefined
                                                            operands = list_ref(aexp, 2)
                                                            operator = list_ref(aexp, 1)
                                                            return append(List(aunparse(operator)), Map(aunparse, operands))
                                                        else:
                                                            if true_q((car(aexp)) is (symbol_try_catch_aexp)):
                                                                body = symbol_undefined
                                                                catch_var = symbol_undefined
                                                                catch_exps = symbol_undefined
                                                                catch_exps = list_ref(aexp, 3)
                                                                catch_var = list_ref(aexp, 2)
                                                                body = list_ref(aexp, 1)
                                                                return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps))))))
                                                            else:
                                                                if true_q((car(aexp)) is (symbol_try_finally_aexp)):
                                                                    body = symbol_undefined
                                                                    finally_exps = symbol_undefined
                                                                    finally_exps = list_ref(aexp, 2)
                                                                    body = list_ref(aexp, 1)
                                                                    return append(List(symbol_try), append(List(aunparse(body)), List(append(List(symbol_finally), Map(aunparse, finally_exps)))))
                                                                else:
                                                                    if true_q((car(aexp)) is (symbol_try_catch_finally_aexp)):
                                                                        body = symbol_undefined
                                                                        catch_var = symbol_undefined
                                                                        catch_exps = symbol_undefined
                                                                        finally_exps = symbol_undefined
                                                                        finally_exps = list_ref(aexp, 4)
                                                                        catch_exps = list_ref(aexp, 3)
                                                                        catch_var = list_ref(aexp, 2)
                                                                        body = list_ref(aexp, 1)
                                                                        return append(List(symbol_try), append(List(aunparse(body)), append(List(append(List(symbol_catch), append(List(catch_var), Map(aunparse, catch_exps)))), List(append(List(symbol_finally), Map(aunparse, finally_exps))))))
                                                                    else:
                                                                        if true_q((car(aexp)) is (symbol_raise_aexp)):
                                                                            exp = symbol_undefined
                                                                            exp = list_ref(aexp, 1)
                                                                            return append(List(symbol_raise), List(aunparse(exp)))
                                                                        else:
                                                                            if true_q((car(aexp)) is (symbol_choose_aexp)):
                                                                                exps = symbol_undefined
                                                                                exps = list_ref(aexp, 1)
                                                                                return append(List(symbol_choose), Map(aunparse, exps))
                                                                            else:
                                                                                raise Exception("symbol_aunparse: " + format("bad abstract syntax: ~s", *[aexp]))

def exception_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_exception))

def path_join(path, filename):
    if true_q(null_q(path)):
        return filename
    else:
        return path_join(cdr(path), string_append(car(path), "/", filename))

def use_lexical_address(*args):
    args = List(*args)
    if true_q(null_q(args)):
        return _staruse_lexical_address_star
    else:
        GLOBALS['_staruse_lexical_address_star'] = true_q(car(args))
        return void_value

def handle_exception(exc):
    display(get_traceback_string(exc))
    return void_value

def get_traceback_string(exc):
    if true_q(list_q(cadr(exc))):
        error_type = symbol_undefined
        message = symbol_undefined
        src_file = symbol_undefined
        src_line = symbol_undefined
        src_col = symbol_undefined
        stack = symbol_undefined
        retval = symbol_undefined
        retval = ""
        stack = cadddr(cddr(cadr(exc)))
        src_col = cadddr(cdr(cadr(exc)))
        src_line = cadddr(cadr(exc))
        src_file = caddr(cadr(exc))
        message = cadr(cadr(exc))
        error_type = car(cadr(exc))
        retval = string_append(retval, format("~%Traceback (most recent call last):~%"))
        while not(null_q(stack)):
            retval = string_append(retval, format_exception_line(car(stack)))
            stack = cdr(stack)
        if true_q(not((src_file) is (symbol_none))):
            retval = string_append(retval, format("  File \"~a\", line ~a, col ~a~%", src_file, src_line, src_col))
        return string_append(retval, format("~a: ~a~%", error_type, message))
    else:
        retval = symbol_undefined
        retval = format("~%Traceback (most recent call last):~%")
        return string_append(retval, format("Raised Exception: ~a~%", cadr(exc)))

def get_exception_values(exc):
    if true_q(list_q(cadr(exc))):
        error_type = symbol_undefined
        message = symbol_undefined
        message = cadr(cadr(exc))
        error_type = car(cadr(exc))
        return list_to_vector(List(error_type, message))
    else:
        return list_to_vector(List("UnhandledException", cadr(exc)))

def format_exception_line(line):
    if true_q(list_q(line)):
        filename = symbol_undefined
        line_number = symbol_undefined
        column_number = symbol_undefined
        column_number = caddr(line)
        line_number = cadr(line)
        filename = car(line)
        if true_q(numeric_equal(length(line), 3)):
            return format("  File \"~a\", line ~a, col ~a~%", filename, line_number, column_number)
        else:
            return format("  File \"~a\", line ~a, col ~a, in '~a'~%", filename, line_number, column_number, cadddr(line))
    else:
        return format("  Source \"~a\"~%", line)

def start_rm():
    GLOBALS['toplevel_env'] = make_toplevel_env()
    GLOBALS['macro_env'] = make_macro_env_hat()
    GLOBALS['unit_test_table'] = dict()
    return read_eval_print_loop_rm()

def read_eval_print_loop_rm():
    input_ = symbol_undefined
    input_ = read_multiline("==> ")
    result = symbol_undefined
    result = execute_rm(input_, symbol_stdin)
    while not(end_of_session_q(result)):
        if true_q(exception_q(result)):
            handle_exception(result)
        else:
            if true_q(not(void_q(result))):
                if true_q(_starneed_newline_star):
                    newline()
                safe_print(result)
        input_ = read_multiline("==> ")
        result = execute_rm(input_, symbol_stdin)
    return symbol_goodbye

def execute_string_top(input_, source):
    return execute_rm(input_, source)

def execute_string_rm(input_):
    return execute_rm(input_, symbol_stdin)

def execute_file_rm(filename):
    return execute_rm(read_content(filename), filename)

def execute_rm(input_, src):
    GLOBALS['load_stack'] = symbol_emptylist
    initialize_execute_b()
    GLOBALS['k_reg'] = REP_k
    GLOBALS['fail_reg'] = _starlast_fail_star
    GLOBALS['handler_reg'] = REP_handler
    GLOBALS['src_reg'] = src
    GLOBALS['input_reg'] = input_
    GLOBALS['pc'] = scan_input
    result = symbol_undefined
    result = trampoline()
    if true_q(exception_q(result)):
        return result
    else:
        GLOBALS['_startokens_left_star'] = result
        if true_q(token_type_q(first(_startokens_left_star), symbol_end_marker)):
            return void_value
        else:
            return execute_loop_rm(src)

def execute_loop_rm(src):
    execute_next_expression_rm(src)
    result = symbol_undefined
    result = trampoline()
    if true_q((exception_q(result)) or (end_of_session_q(result)) or (token_type_q(first(_startokens_left_star), symbol_end_marker))):
        return result
    else:
        return execute_loop_rm(src)

def execute_next_expression_rm(src):
    GLOBALS['k_reg'] = make_cont4(b_cont4_10_d)
    GLOBALS['fail_reg'] = _starlast_fail_star
    GLOBALS['handler_reg'] = REP_handler
    GLOBALS['src_reg'] = src
    GLOBALS['tokens_reg'] = _startokens_left_star
    GLOBALS['pc'] = read_sexp

def try_parse(input_):
    GLOBALS['load_stack'] = symbol_emptylist
    GLOBALS['k_reg'] = make_cont2(b_cont2_60_d)
    GLOBALS['fail_reg'] = _starlast_fail_star
    GLOBALS['handler_reg'] = try_parse_handler
    GLOBALS['src_reg'] = symbol_stdin
    GLOBALS['input_reg'] = input_
    GLOBALS['pc'] = scan_input
    return trampoline()

def initialize_globals():
    GLOBALS['_starfilename_dict_star'] = dict()
    GLOBALS['_starfilename_vector_star'] = vlist()
    filename_cache(symbol_stdin)
    GLOBALS['toplevel_env'] = make_toplevel_env()
    GLOBALS['macro_env'] = make_macro_env_hat()
    GLOBALS['unit_test_table'] = dict()
    GLOBALS['load_stack'] = symbol_emptylist
    initialize_execute_b()
    GLOBALS['_starlast_fail_star'] = REP_fail

def make_debugging_k(exp, k):
    return make_cont2(b_cont2_61_d, exp, k)

def handle_debug_info(exp, result):
    printf("~s => ~a~%", aunparse(exp), make_safe(result))

def get_use_stack_trace():
    return _staruse_stack_trace_star

def set_use_stack_trace_b(value):
    GLOBALS['_staruse_stack_trace_star'] = true_q(value)

def initialize_stack_trace_b():
    set_car_b(_starstack_trace_star, symbol_emptylist)

def initialize_execute_b():
    GLOBALS['_closure_depth'] = 0
    GLOBALS['_trace_pause'] = False
    initialize_stack_trace_b()

def push_stack_trace_b(exp):
    set_car_b(_starstack_trace_star, cons(exp, car(_starstack_trace_star)))

def pop_stack_trace_b(exp):
    if true_q(not(null_q(car(_starstack_trace_star)))):
        set_car_b(_starstack_trace_star, cdr(car(_starstack_trace_star)))

def m():
    if true_q(_startracing_on_q_star):
        highlight_expression(exp_reg)
    k = symbol_undefined
    k = (make_debugging_k(exp_reg, k_reg) if _startracing_on_q_star else k_reg)
    if true_q((car(exp_reg)) is (symbol_lit_aexp)):
        datum = symbol_undefined
        datum = list_ref(exp_reg, 1)
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = datum
        GLOBALS['k_reg'] = k
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q((car(exp_reg)) is (symbol_var_aexp)):
            id = symbol_undefined
            info = symbol_undefined
            info = list_ref(exp_reg, 2)
            id = list_ref(exp_reg, 1)
            GLOBALS['k_reg'] = k
            GLOBALS['var_info_reg'] = info
            GLOBALS['var_reg'] = id
            GLOBALS['pc'] = lookup_value
        else:
            if true_q((car(exp_reg)) is (symbol_lexical_address_aexp)):
                depth = symbol_undefined
                offset = symbol_undefined
                offset = list_ref(exp_reg, 2)
                depth = list_ref(exp_reg, 1)
                GLOBALS['k_reg'] = k
                GLOBALS['frames_reg'] = frames(env_reg)
                GLOBALS['offset_reg'] = offset
                GLOBALS['depth_reg'] = depth
                GLOBALS['pc'] = lookup_value_by_lexical_address
            else:
                if true_q((car(exp_reg)) is (symbol_func_aexp)):
                    exp = symbol_undefined
                    exp = list_ref(exp_reg, 1)
                    GLOBALS['k_reg'] = make_cont2(b_cont2_83_d, k)
                    GLOBALS['exp_reg'] = exp
                    GLOBALS['pc'] = m
                else:
                    if true_q((car(exp_reg)) is (symbol_callback_aexp)):
                        exp = symbol_undefined
                        exp = list_ref(exp_reg, 1)
                        GLOBALS['k_reg'] = make_cont2(b_cont2_81_d, k)
                        GLOBALS['exp_reg'] = exp
                        GLOBALS['pc'] = m
                    else:
                        if true_q((car(exp_reg)) is (symbol_if_aexp)):
                            test_exp = symbol_undefined
                            then_exp = symbol_undefined
                            else_exp = symbol_undefined
                            else_exp = list_ref(exp_reg, 3)
                            then_exp = list_ref(exp_reg, 2)
                            test_exp = list_ref(exp_reg, 1)
                            GLOBALS['k_reg'] = make_cont2(b_cont2_82_d, else_exp, then_exp, env_reg, handler_reg, k)
                            GLOBALS['exp_reg'] = test_exp
                            GLOBALS['pc'] = m
                        else:
                            if true_q((car(exp_reg)) is (symbol_help_aexp)):
                                var = symbol_undefined
                                var_info = symbol_undefined
                                var_info = list_ref(exp_reg, 2)
                                var = list_ref(exp_reg, 1)
                                GLOBALS['sk_reg'] = make_cont2(b_cont2_78_d, k)
                                GLOBALS['dk_reg'] = make_cont3(b_cont3_5_d, k)
                                GLOBALS['gk_reg'] = make_cont2(b_cont2_79_d, k)
                                GLOBALS['var_info_reg'] = var_info
                                GLOBALS['var_reg'] = var
                                GLOBALS['pc'] = lookup_variable
                            else:
                                if true_q((car(exp_reg)) is (symbol_association_aexp)):
                                    var = symbol_undefined
                                    exp = symbol_undefined
                                    exp = list_ref(exp_reg, 2)
                                    var = list_ref(exp_reg, 1)
                                    GLOBALS['k_reg'] = make_cont2(b_cont2_80_d, var, k)
                                    GLOBALS['exp_reg'] = exp
                                    GLOBALS['pc'] = m
                                else:
                                    if true_q((car(exp_reg)) is (symbol_assign_aexp)):
                                        var = symbol_undefined
                                        rhs_exp = symbol_undefined
                                        var_info = symbol_undefined
                                        var_info = list_ref(exp_reg, 3)
                                        rhs_exp = list_ref(exp_reg, 2)
                                        var = list_ref(exp_reg, 1)
                                        GLOBALS['k_reg'] = make_cont2(b_cont2_75_d, var, var_info, env_reg, handler_reg, k)
                                        GLOBALS['exp_reg'] = rhs_exp
                                        GLOBALS['pc'] = m
                                    else:
                                        if true_q((car(exp_reg)) is (symbol_define_aexp)):
                                            var = symbol_undefined
                                            docstring = symbol_undefined
                                            rhs_exp = symbol_undefined
                                            rhs_exp = list_ref(exp_reg, 3)
                                            docstring = list_ref(exp_reg, 2)
                                            var = list_ref(exp_reg, 1)
                                            GLOBALS['k_reg'] = make_cont2(b_cont2_77_d, docstring, var, env_reg, handler_reg, k)
                                            GLOBALS['exp_reg'] = rhs_exp
                                            GLOBALS['pc'] = m
                                        else:
                                            if true_q((car(exp_reg)) is (symbol_define_b_aexp)):
                                                var = symbol_undefined
                                                docstring = symbol_undefined
                                                rhs_exp = symbol_undefined
                                                rhs_exp = list_ref(exp_reg, 3)
                                                docstring = list_ref(exp_reg, 2)
                                                var = list_ref(exp_reg, 1)
                                                GLOBALS['k_reg'] = make_cont2(b_cont2_71_d, docstring, var, k)
                                                GLOBALS['exp_reg'] = rhs_exp
                                                GLOBALS['pc'] = m
                                            else:
                                                if true_q((car(exp_reg)) is (symbol_define_syntax_aexp)):
                                                    name = symbol_undefined
                                                    clauses = symbol_undefined
                                                    aclauses = symbol_undefined
                                                    aclauses = list_ref(exp_reg, 3)
                                                    clauses = list_ref(exp_reg, 2)
                                                    name = list_ref(exp_reg, 1)
                                                    GLOBALS['k_reg'] = make_cont2(b_cont2_72_d, aclauses, clauses, k)
                                                    GLOBALS['env_reg'] = macro_env
                                                    GLOBALS['var_reg'] = name
                                                    GLOBALS['pc'] = lookup_binding_in_first_frame
                                                else:
                                                    if true_q((car(exp_reg)) is (symbol_define_syntax_transformer_aexp)):
                                                        name = symbol_undefined
                                                        rhs_exp = symbol_undefined
                                                        info = symbol_undefined
                                                        info = list_ref(exp_reg, 3)
                                                        rhs_exp = list_ref(exp_reg, 2)
                                                        name = list_ref(exp_reg, 1)
                                                        GLOBALS['k_reg'] = make_cont2(b_cont2_70_d, name, env_reg, info, handler_reg, k)
                                                        GLOBALS['exp_reg'] = rhs_exp
                                                        GLOBALS['pc'] = m
                                                    else:
                                                        if true_q((car(exp_reg)) is (symbol_define_tests_aexp)):
                                                            name = symbol_undefined
                                                            aclauses = symbol_undefined
                                                            info = symbol_undefined
                                                            info = list_ref(exp_reg, 3)
                                                            aclauses = list_ref(exp_reg, 2)
                                                            name = list_ref(exp_reg, 1)
                                                            if true_q(hasitem_native(unit_test_table, name)):
                                                                GLOBALS['info_reg'] = info
                                                                GLOBALS['msg_reg'] = format("duplicate unit test group name '~a'; did you forget to (clear-unit-tests)?", name)
                                                                GLOBALS['pc'] = runtime_error
                                                            else:
                                                                setitem_native(unit_test_table, name, List(aclauses, env_reg))
                                                                GLOBALS['value2_reg'] = fail_reg
                                                                GLOBALS['value1_reg'] = void_value
                                                                GLOBALS['k_reg'] = k
                                                                GLOBALS['pc'] = apply_cont2
                                                        else:
                                                            if true_q((car(exp_reg)) is (symbol_run_tests_aexp)):
                                                                tests = symbol_undefined
                                                                tests = list_ref(exp_reg, 1)
                                                                if true_q(null_q(tests)):
                                                                    GLOBALS['k_reg'] = k
                                                                    GLOBALS['wrong_reg'] = 0
                                                                    GLOBALS['right_reg'] = 0
                                                                    GLOBALS['start_time_reg'] = get_current_time()
                                                                    GLOBALS['tests_reg'] = Map(List, dict_to_keys(unit_test_table))
                                                                    GLOBALS['pc'] = run_unit_tests
                                                                else:
                                                                    GLOBALS['k_reg'] = k
                                                                    GLOBALS['wrong_reg'] = 0
                                                                    GLOBALS['right_reg'] = 0
                                                                    GLOBALS['start_time_reg'] = get_current_time()
                                                                    GLOBALS['tests_reg'] = tests
                                                                    GLOBALS['pc'] = run_unit_tests
                                                            else:
                                                                if true_q((car(exp_reg)) is (symbol_begin_aexp)):
                                                                    exps = symbol_undefined
                                                                    exps = list_ref(exp_reg, 1)
                                                                    GLOBALS['k_reg'] = k
                                                                    GLOBALS['exps_reg'] = exps
                                                                    GLOBALS['pc'] = eval_sequence
                                                                else:
                                                                    if true_q((car(exp_reg)) is (symbol_lambda_aexp)):
                                                                        formals = symbol_undefined
                                                                        bodies = symbol_undefined
                                                                        bodies = list_ref(exp_reg, 2)
                                                                        formals = list_ref(exp_reg, 1)
                                                                        GLOBALS['value2_reg'] = fail_reg
                                                                        GLOBALS['value1_reg'] = closure(formals, bodies, env_reg)
                                                                        GLOBALS['k_reg'] = k
                                                                        GLOBALS['pc'] = apply_cont2
                                                                    else:
                                                                        if true_q((car(exp_reg)) is (symbol_mu_lambda_aexp)):
                                                                            formals = symbol_undefined
                                                                            runt = symbol_undefined
                                                                            bodies = symbol_undefined
                                                                            bodies = list_ref(exp_reg, 3)
                                                                            runt = list_ref(exp_reg, 2)
                                                                            formals = list_ref(exp_reg, 1)
                                                                            GLOBALS['value2_reg'] = fail_reg
                                                                            GLOBALS['value1_reg'] = mu_closure(formals, get_symbol(runt), bodies, env_reg)
                                                                            GLOBALS['k_reg'] = k
                                                                            GLOBALS['pc'] = apply_cont2
                                                                        else:
                                                                            if true_q((car(exp_reg)) is (symbol_trace_lambda_aexp)):
                                                                                name = symbol_undefined
                                                                                formals = symbol_undefined
                                                                                bodies = symbol_undefined
                                                                                bodies = list_ref(exp_reg, 3)
                                                                                formals = list_ref(exp_reg, 2)
                                                                                name = list_ref(exp_reg, 1)
                                                                                GLOBALS['value2_reg'] = fail_reg
                                                                                GLOBALS['value1_reg'] = trace_closure(name, formals, bodies, env_reg)
                                                                                GLOBALS['k_reg'] = k
                                                                                GLOBALS['pc'] = apply_cont2
                                                                            else:
                                                                                if true_q((car(exp_reg)) is (symbol_mu_trace_lambda_aexp)):
                                                                                    name = symbol_undefined
                                                                                    formals = symbol_undefined
                                                                                    runt = symbol_undefined
                                                                                    bodies = symbol_undefined
                                                                                    bodies = list_ref(exp_reg, 4)
                                                                                    runt = list_ref(exp_reg, 3)
                                                                                    formals = list_ref(exp_reg, 2)
                                                                                    name = list_ref(exp_reg, 1)
                                                                                    GLOBALS['value2_reg'] = fail_reg
                                                                                    GLOBALS['value1_reg'] = mu_trace_closure(name, formals, get_symbol(runt), bodies, env_reg)
                                                                                    GLOBALS['k_reg'] = k
                                                                                    GLOBALS['pc'] = apply_cont2
                                                                                else:
                                                                                    if true_q((car(exp_reg)) is (symbol_try_catch_aexp)):
                                                                                        body = symbol_undefined
                                                                                        cvar = symbol_undefined
                                                                                        cexps = symbol_undefined
                                                                                        cexps = list_ref(exp_reg, 3)
                                                                                        cvar = list_ref(exp_reg, 2)
                                                                                        body = list_ref(exp_reg, 1)
                                                                                        new_handler = symbol_undefined
                                                                                        new_handler = try_catch_handler(cvar, cexps, env_reg, handler_reg, k)
                                                                                        GLOBALS['k_reg'] = k
                                                                                        GLOBALS['handler_reg'] = new_handler
                                                                                        GLOBALS['exp_reg'] = body
                                                                                        GLOBALS['pc'] = m
                                                                                    else:
                                                                                        if true_q((car(exp_reg)) is (symbol_try_finally_aexp)):
                                                                                            body = symbol_undefined
                                                                                            fexps = symbol_undefined
                                                                                            fexps = list_ref(exp_reg, 2)
                                                                                            body = list_ref(exp_reg, 1)
                                                                                            new_handler = symbol_undefined
                                                                                            new_handler = try_finally_handler(fexps, env_reg, handler_reg)
                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_66_d, fexps, env_reg, handler_reg, k)
                                                                                            GLOBALS['handler_reg'] = new_handler
                                                                                            GLOBALS['exp_reg'] = body
                                                                                            GLOBALS['pc'] = m
                                                                                        else:
                                                                                            if true_q((car(exp_reg)) is (symbol_try_catch_finally_aexp)):
                                                                                                body = symbol_undefined
                                                                                                cvar = symbol_undefined
                                                                                                cexps = symbol_undefined
                                                                                                fexps = symbol_undefined
                                                                                                fexps = list_ref(exp_reg, 4)
                                                                                                cexps = list_ref(exp_reg, 3)
                                                                                                cvar = list_ref(exp_reg, 2)
                                                                                                body = list_ref(exp_reg, 1)
                                                                                                new_handler = symbol_undefined
                                                                                                new_handler = try_catch_finally_handler(cvar, cexps, fexps, env_reg, handler_reg, k)
                                                                                                GLOBALS['k_reg'] = make_cont2(b_cont2_66_d, fexps, env_reg, handler_reg, k)
                                                                                                GLOBALS['handler_reg'] = new_handler
                                                                                                GLOBALS['exp_reg'] = body
                                                                                                GLOBALS['pc'] = m
                                                                                            else:
                                                                                                if true_q((car(exp_reg)) is (symbol_raise_aexp)):
                                                                                                    exp = symbol_undefined
                                                                                                    exp = list_ref(exp_reg, 1)
                                                                                                    GLOBALS['k_reg'] = make_cont2(b_cont2_67_d, handler_reg)
                                                                                                    GLOBALS['exp_reg'] = exp
                                                                                                    GLOBALS['pc'] = m
                                                                                                else:
                                                                                                    if true_q((car(exp_reg)) is (symbol_choose_aexp)):
                                                                                                        exps = symbol_undefined
                                                                                                        exps = list_ref(exp_reg, 1)
                                                                                                        GLOBALS['k_reg'] = k
                                                                                                        GLOBALS['exps_reg'] = exps
                                                                                                        GLOBALS['pc'] = eval_choices
                                                                                                    else:
                                                                                                        if true_q((car(exp_reg)) is (symbol_app_aexp)):
                                                                                                            operator = symbol_undefined
                                                                                                            operands = symbol_undefined
                                                                                                            info = symbol_undefined
                                                                                                            info = list_ref(exp_reg, 3)
                                                                                                            operands = list_ref(exp_reg, 2)
                                                                                                            operator = list_ref(exp_reg, 1)
                                                                                                            GLOBALS['k_reg'] = make_cont2(b_cont2_64_d, exp_reg, operator, env_reg, info, handler_reg, k)
                                                                                                            GLOBALS['exps_reg'] = operands
                                                                                                            GLOBALS['pc'] = m_star
                                                                                                        else:
                                                                                                            GLOBALS['info_reg'] = symbol_none
                                                                                                            GLOBALS['msg_reg'] = format("unknown abstract syntax type: ~a", car(exp_reg))
                                                                                                            GLOBALS['pc'] = runtime_error

def run_unit_tests():
    if true_q(null_q(tests_reg)):
        total = symbol_undefined
        total = Apply(plus, Map(length, Map(car, dict_to_values(unit_test_table))))
        printf("=================\n")
        printf("Testing completed!\n")
        printf("  Time : ~a seconds~%", format_float(4, 2, (get_current_time()) - (start_time_reg)))
        printf("  Total tests defined: ~s ~%", total)
        printf("  Total tests tested : ~s ~%", (right_reg) + (wrong_reg))
        printf("                Right: ~s ~%", right_reg)
        printf("                Wrong: ~s ~%", wrong_reg)
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_84_d, start_time_reg, tests_reg, handler_reg, k_reg)
        GLOBALS['test_reg'] = car(tests_reg)
        GLOBALS['pc'] = run_unit_test

def run_unit_test():
    test_name = symbol_undefined
    nums = symbol_undefined
    entry = symbol_undefined
    test_name = car(test_reg)
    nums = cdr(test_reg)
    entry = getitem_native(unit_test_table, test_name)
    if true_q((entry) is (False)):
        GLOBALS['info_reg'] = symbol_none
        GLOBALS['msg_reg'] = format("test group '~a' not found", test_name)
        GLOBALS['pc'] = runtime_error
    else:
        assertions = symbol_undefined
        env = symbol_undefined
        assertions = car(entry)
        env = cadr(entry)
        printf("Testing group '~a'...\n", test_name)
        if true_q(null_q(nums)):
            GLOBALS['env_reg'] = env
            GLOBALS['verbose_reg'] = False
            GLOBALS['assertions_reg'] = assertions
            GLOBALS['test_name_reg'] = test_name
            GLOBALS['pc'] = run_unit_test_cases
        else:
            GLOBALS['k_reg'] = make_cont2(b_cont2_85_d, right_reg, test_name, wrong_reg, env, handler_reg, k_reg)
            GLOBALS['assertions_reg'] = assertions
            GLOBALS['nums_reg'] = nums
            GLOBALS['test_name_reg'] = test_name
            GLOBALS['pc'] = filter_assertions

def filter_assertions():
    if true_q(null_q(nums_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        case_name = symbol_undefined
        if true_q(number_q(car(nums_reg))):
            case_name = format("case ~a", car(nums_reg))
        else:
            case_name = car(nums_reg)
        return lookup_assertions(test_name_reg, case_name, assertions_reg, symbol_emptylist, handler_reg, fail_reg, make_cont2(b_cont2_87_d, assertions_reg, nums_reg, test_name_reg, handler_reg, k_reg))

def lookup_assertions(test_name, case_name, assertions, accum, handler, fail, k):
    if true_q(null_q(assertions)):
        if true_q(null_q(accum)):
            GLOBALS['fail_reg'] = fail
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = symbol_none
            GLOBALS['msg_reg'] = format("~a unit test '~a' not found", test_name, case_name)
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['value2_reg'] = fail
            GLOBALS['value1_reg'] = accum
            GLOBALS['k_reg'] = k
            GLOBALS['pc'] = apply_cont2
    else:
        assertion = symbol_undefined
        app_aexp_args = symbol_undefined
        assertion = car(assertions)
        app_aexp_args = caddr(assertion)
        if true_q(numeric_equal(length(app_aexp_args), 4)):
            lit_aexp_datum = symbol_undefined
            lit_aexp_datum = cadr(cadddr(app_aexp_args))
            if true_q((string_q(lit_aexp_datum)) and (((string_startswith_q(case_name, "case ")) and (string_is__q(lit_aexp_datum, case_name))) or ((not(string_startswith_q(case_name, "case "))) and (string_startswith_q(lit_aexp_datum, case_name))))):
                return lookup_assertions(test_name, case_name, cdr(assertions), cons(assertion, accum), handler, fail, k)
            else:
                return lookup_assertions(test_name, case_name, cdr(assertions), accum, handler, fail, k)

def internal_exception_q(exception):
    return (pair_q(exception)) and (string_q(car(exception))) and ((string_is__q(car(exception), "AssertionError")) or (string_is__q(car(exception), "UnhandledException")) or (string_is__q(car(exception), "RunTimeError")) or (string_is__q(car(exception), "ScanError")) or (string_is__q(car(exception), "ParseError")) or (string_is__q(car(exception), "ReadError")))

def run_unit_test_cases():
    if true_q(null_q(assertions_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = List(right_reg, wrong_reg)
        GLOBALS['pc'] = apply_cont2
    else:
        test_case_handler = symbol_undefined
        test_case_handler = make_handler2(b_handler2_4_d, assertions_reg, right_reg, test_name_reg, verbose_reg, wrong_reg, env_reg, handler_reg, k_reg)
        initialize_stack_trace_b()
        GLOBALS['k_reg'] = make_cont2(b_cont2_89_d, assertions_reg, right_reg, test_name_reg, verbose_reg, wrong_reg, env_reg, handler_reg, k_reg)
        GLOBALS['handler_reg'] = test_case_handler
        GLOBALS['exp_reg'] = car(assertions_reg)
        GLOBALS['pc'] = m

def get_exception_info(exception):
    source = symbol_undefined
    line = symbol_undefined
    column = symbol_undefined
    column = car(cddddr(exception))
    line = cadddr(exception)
    source = caddr(exception)
    if true_q((source) is (symbol_none)):
        return symbol_none
    else:
        return format("line ~a, column ~a of ~a", line, column, source)

def make_exception(exception_type, message, source, line, column):
    return List(exception_type, message, source, line, column, make_stack_trace())

def get_exception_message(exception):
    return cadr(exception)

def make_stack_trace():
    trace = symbol_undefined
    trace = car(_starstack_trace_star)
    return reverse(Map(format_stack_trace, trace))

def get_procedure_name(aexp):
    if true_q(macro_derived_source_info_q(aexp)):
        return rac(get_source_info(aexp))
    else:
        if true_q((car(aexp)) is (symbol_app_aexp)):
            operator = symbol_undefined
            operator = list_ref(aexp, 1)
            if true_q((car(operator)) is (symbol_lexical_address_aexp)):
                id = symbol_undefined
                id = list_ref(operator, 3)
                return id
            else:
                if true_q((car(operator)) is (symbol_var_aexp)):
                    id = symbol_undefined
                    id = list_ref(operator, 1)
                    return id
                else:
                    if true_q((car(operator)) is (symbol_lambda_aexp)):
                        formals = symbol_undefined
                        formals = list_ref(operator, 1)
                        return append(List(symbol_lambda), append(List(formals), List(symbol_dotdotdot)))
                    else:
                        if true_q((car(operator)) is (symbol_mu_lambda_aexp)):
                            formals = symbol_undefined
                            runt = symbol_undefined
                            runt = list_ref(operator, 2)
                            formals = list_ref(operator, 1)
                            return append(List(symbol_lambda), append(List(append(formals, runt)), List(symbol_dotdotdot)))
                        else:
                            if true_q((car(operator)) is (symbol_trace_lambda_aexp)):
                                name = symbol_undefined
                                name = list_ref(operator, 1)
                                return name
                            else:
                                if true_q((car(operator)) is (symbol_mu_trace_lambda_aexp)):
                                    name = symbol_undefined
                                    name = list_ref(operator, 1)
                                    return name
                                else:
                                    return symbol_application
        else:
            return symbol_unknown

def format_stack_trace(exp):
    info = symbol_undefined
    info = rac(exp)
    if true_q((info) is (symbol_none)):
        return symbol_macro_generated_exp
    else:
        return List(get_srcfile(info), get_start_line(info), get_start_char(info), get_procedure_name(exp))

def runtime_error():
    if true_q((info_reg) is (symbol_none)):
        GLOBALS['exception_reg'] = make_exception("RunTimeError", msg_reg, symbol_none, symbol_none, symbol_none)
        GLOBALS['pc'] = apply_handler2
    else:
        src = symbol_undefined
        line_number = symbol_undefined
        char_number = symbol_undefined
        char_number = get_start_char(info_reg)
        line_number = get_start_line(info_reg)
        src = get_srcfile(info_reg)
        GLOBALS['exception_reg'] = make_exception("RunTimeError", msg_reg, src, line_number, char_number)
        GLOBALS['pc'] = apply_handler2

def assertion_error():
    if true_q((info_reg) is (symbol_none)):
        GLOBALS['exception_reg'] = make_exception("AssertionError", msg_reg, symbol_none, symbol_none, symbol_none)
        GLOBALS['pc'] = apply_handler2
    else:
        src = symbol_undefined
        line_number = symbol_undefined
        char_number = symbol_undefined
        char_number = get_start_char(info_reg)
        line_number = get_start_line(info_reg)
        src = get_srcfile(info_reg)
        GLOBALS['exception_reg'] = make_exception("AssertionError", msg_reg, src, line_number, char_number)
        GLOBALS['pc'] = apply_handler2

def m_star():
    if true_q(null_q(exps_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_90_d, exps_reg, env_reg, handler_reg, k_reg)
        GLOBALS['exp_reg'] = car(exps_reg)
        GLOBALS['pc'] = m

def eval_sequence():
    if true_q(null_q(cdr(exps_reg))):
        GLOBALS['exp_reg'] = car(exps_reg)
        GLOBALS['pc'] = m
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_91_d, exps_reg, env_reg, handler_reg, k_reg)
        GLOBALS['exp_reg'] = car(exps_reg)
        GLOBALS['pc'] = m

def try_catch_handler(cvar, cexps, env, handler, k):
    return make_handler2(b_handler2_5_d, cexps, cvar, env, handler, k)

def try_finally_handler(fexps, env, handler):
    return make_handler2(b_handler2_6_d, fexps, env, handler)

def try_catch_finally_handler(cvar, cexps, fexps, env, handler, k):
    return make_handler2(b_handler2_7_d, cexps, cvar, fexps, env, handler, k)

def eval_choices():
    if true_q(null_q(exps_reg)):
        GLOBALS['pc'] = apply_fail
    else:
        new_fail = symbol_undefined
        new_fail = make_fail(b_fail_5_d, exps_reg, env_reg, handler_reg, fail_reg, k_reg)
        GLOBALS['fail_reg'] = new_fail
        GLOBALS['exp_reg'] = car(exps_reg)
        GLOBALS['pc'] = m

def make_empty_docstrings(n):
    if true_q(numeric_equal(n, 0)):
        return symbol_emptylist
    else:
        return cons("", make_empty_docstrings((n) - (1)))

def association(var, value):
    return List(var, symbol_colon, value)

def closure(formals, bodies, env):
    return make_proc(b_proc_1_d, bodies, formals, env)

def mu_closure(formals, runt, bodies, env):
    return make_proc(b_proc_2_d, bodies, formals, runt, env)

def make_trace_depth_string(level):
    if true_q(numeric_equal(level, 0)):
        return ""
    else:
        return string_append(" |", make_trace_depth_string((level) - (1)))

def trace_closure(name, formals, bodies, env):
    trace_depth = symbol_undefined
    trace_depth = 0
    return make_proc(b_proc_3_d, bodies, name, trace_depth, formals, env)

def continuation_object_q(x):
    return (pair_q(x)) and (memq(car(x), List(symbol_continuation, symbol_continuation2, symbol_continuation3, symbol_continuation4)))

def mu_trace_closure(name, formals, runt, bodies, env):
    trace_depth = symbol_undefined
    trace_depth = 0
    return make_proc(b_proc_4_d, bodies, name, trace_depth, formals, runt, env)

def all_char_q(ls):
    return (null_q(ls)) or ((char_q(car(ls))) and (all_char_q(cdr(ls))))

def void_q(x):
    return (x) is (void_value)

def end_of_session_q(x):
    return (x) is (end_of_session)

def string_join():
    if true_q(null_q(items_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = ""
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(null_q(cdr(items_reg))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = format("~a", car(items_reg))
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2
        else:
            GLOBALS['k2_reg'] = make_cont2(b_cont2_94_d, items_reg, sep_reg, k2_reg)
            GLOBALS['items_reg'] = cdr(items_reg)
            GLOBALS['pc'] = string_join

def safe_print(arg):
    GLOBALS['_starneed_newline_star'] = False
    pretty_print(make_safe(arg))

def procedure_object_q(x):
    return (procedure_q(x)) or ((pair_q(x)) and ((car(x)) is (symbol_procedure)))

def environment_object_q(x):
    return (pair_q(x)) and ((car(x)) is (symbol_environment))

def ends_with_newline_q(s):
    len = symbol_undefined
    len = string_length(s)
    return (GreaterThan(len, 0)) and (equal_q(substring(s, (len) - (1), len), "\n"))

def load_file():
    if true_q(member(filename_reg, load_stack)):
        printf("skipping recursive load of ~a~%", filename_reg)
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(not(string_q(filename_reg))):
            GLOBALS['msg_reg'] = format("filename '~a' is not a string", filename_reg)
            GLOBALS['pc'] = runtime_error
        else:
            if true_q(not(file_exists_q(filename_reg))):
                GLOBALS['msg_reg'] = format("attempted to load nonexistent file '~a'", filename_reg)
                GLOBALS['pc'] = runtime_error
            else:
                GLOBALS['load_stack'] = cons(filename_reg, load_stack)
                GLOBALS['k_reg'] = make_cont2(b_cont2_100_d, filename_reg, env2_reg, handler_reg, k_reg)
                GLOBALS['src_reg'] = filename_reg
                GLOBALS['input_reg'] = read_content(filename_reg)
                GLOBALS['pc'] = scan_input

def read_and_eval_asexps():
    if true_q(token_type_q(first(tokens_reg), symbol_end_marker)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont4(b_cont4_13_d, src_reg, env2_reg, handler_reg, k_reg)
        GLOBALS['pc'] = read_sexp

def load_files():
    if true_q(null_q(filenames_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = void_value
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k_reg'] = make_cont2(b_cont2_103_d, filenames_reg, env2_reg, info_reg, handler_reg, k_reg)
        GLOBALS['filename_reg'] = car(filenames_reg)
        GLOBALS['paths_reg'] = SCHEMEPATH
        GLOBALS['pc'] = find_file_and_load

def find_file_and_load():
    if true_q(string_startswith_q(filename_reg, "/")):
        GLOBALS['pc'] = load_file
    else:
        if true_q(null_q(paths_reg)):
            GLOBALS['msg_reg'] = format("attempted to load nonexistent file '~a'", filename_reg)
            GLOBALS['pc'] = runtime_error
        else:
            path = symbol_undefined
            path = path_join(List(car(paths_reg)), filename_reg)
            if true_q(file_exists_q(path)):
                GLOBALS['filename_reg'] = path
                GLOBALS['pc'] = load_file
            else:
                GLOBALS['paths_reg'] = cdr(paths_reg)
                GLOBALS['pc'] = find_file_and_load

def length_loop():
    if true_q(null_q(x_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = sum_reg
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(not(pair_q(x_reg))):
            GLOBALS['msg_reg'] = format("length called on improper list ~s", ls_reg)
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['sum_reg'] = (sum_reg) + (1)
            GLOBALS['x_reg'] = cdr(x_reg)
            GLOBALS['pc'] = length_loop

def make_set():
    if true_q(null_q(lst_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = lst_reg
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_105_d, lst_reg, k2_reg)
        GLOBALS['lst_reg'] = cdr(lst_reg)
        GLOBALS['pc'] = make_set

def equal_objects_q():
    if true_q(((null_q(x_reg)) and (null_q(y_reg))) or ((boolean_q(x_reg)) and (boolean_q(y_reg)) and (((x_reg) and (y_reg)) or ((not(x_reg)) and (not(y_reg))))) or ((symbol_q(x_reg)) and (symbol_q(y_reg)) and ((x_reg) is (y_reg))) or ((number_q(x_reg)) and (number_q(y_reg)) and (numeric_equal(x_reg, y_reg))) or ((char_q(x_reg)) and (char_q(y_reg)) and (char_is__q(x_reg, y_reg))) or (((x_reg) is (void_value)) and ((y_reg) is (void_value))) or ((string_q(x_reg)) and (string_q(y_reg)) and (string_is__q(x_reg, y_reg)))):
        GLOBALS['value_reg'] = True
        GLOBALS['pc'] = apply_cont
    else:
        if true_q((pair_q(x_reg)) and (pair_q(y_reg))):
            GLOBALS['k_reg'] = make_cont(b_cont_51_d, x_reg, y_reg, k_reg)
            GLOBALS['y_reg'] = car(y_reg)
            GLOBALS['x_reg'] = car(x_reg)
            GLOBALS['pc'] = equal_objects_q
        else:
            if true_q((vector_q(x_reg)) and (vector_q(y_reg)) and (numeric_equal(vector_length(x_reg), vector_length(y_reg)))):
                GLOBALS['i_reg'] = (vector_length(x_reg)) - (1)
                GLOBALS['v2_reg'] = y_reg
                GLOBALS['v1_reg'] = x_reg
                GLOBALS['pc'] = equal_vectors_q
            else:
                if true_q((box_q(x_reg)) and (box_q(y_reg))):
                    GLOBALS['y_reg'] = unbox(y_reg)
                    GLOBALS['x_reg'] = unbox(x_reg)
                    GLOBALS['pc'] = equal_objects_q
                else:
                    GLOBALS['value_reg'] = False
                    GLOBALS['pc'] = apply_cont

def equal_vectors_q():
    if true_q(LessThan(i_reg, 0)):
        GLOBALS['value_reg'] = True
        GLOBALS['pc'] = apply_cont
    else:
        GLOBALS['k_reg'] = make_cont(b_cont_52_d, i_reg, v1_reg, v2_reg, k_reg)
        GLOBALS['y_reg'] = vector_ref(v2_reg, i_reg)
        GLOBALS['x_reg'] = vector_ref(v1_reg, i_reg)
        GLOBALS['pc'] = equal_objects_q

def member_loop():
    if true_q(null_q(y_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = False
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(not(pair_q(y_reg))):
            GLOBALS['msg_reg'] = format("member called on improper list ~s", ls_reg)
            GLOBALS['pc'] = runtime_error
        else:
            GLOBALS['k_reg'] = make_cont(b_cont_53_d, ls_reg, x_reg, y_reg, info_reg, handler_reg, fail_reg, k_reg)
            GLOBALS['y_reg'] = car(y_reg)
            GLOBALS['pc'] = equal_objects_q

def get_primitive():
    sym = symbol_undefined
    sym = car(args_reg)
    GLOBALS['k_reg'] = make_cont2(b_cont2_107_d, args_reg, sym, info_reg, handler_reg, k_reg)
    GLOBALS['var_info_reg'] = symbol_none
    GLOBALS['var_reg'] = sym
    GLOBALS['pc'] = lookup_value

def append2():
    if true_q(null_q(ls1_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = ls2_reg
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_108_d, ls1_reg, k2_reg)
        GLOBALS['ls1_reg'] = cdr(ls1_reg)
        GLOBALS['pc'] = append2

def append_all():
    if true_q(null_q(lists_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(null_q(cdr(lists_reg))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = car(lists_reg)
            GLOBALS['k_reg'] = k2_reg
            GLOBALS['pc'] = apply_cont2
        else:
            if true_q(not(list_q(car(lists_reg)))):
                GLOBALS['msg_reg'] = format("append called on incorrect list structure ~s", car(lists_reg))
                GLOBALS['pc'] = runtime_error
            else:
                GLOBALS['k2_reg'] = make_cont2(b_cont2_109_d, lists_reg, k2_reg)
                GLOBALS['lists_reg'] = cdr(lists_reg)
                GLOBALS['pc'] = append_all

def get_completions(args, env):
    if true_q(null_q(args)):
        return append(get_variables_from_frames(frames(env)), get_variables_from_frames(frames(macro_env)))
    else:
        if true_q(environment_q(car(args))):
            return append(get_variables_from_frames(frames(car(args))), get_variables_from_frames(frames(macro_env)))
        else:
            return get_external_members(car(args))

def directory(args, env):
    if true_q((null_q(args)) or (environment_q(car(args)))):
        return sort(symbolLessThan_q, (get_variables_from_frames(frames(env)) if null_q(args) else get_variables_from_frames(frames(car(args)))))
    else:
        return get_external_members(car(args))

def get_variables_from_frame(frame):
    return cadr(frame)

def get_variables_from_frames(frames):
    return flatten(Map(get_variables_from_frame, frames))

def symbolLessThan_q(a, b):
    a_string = symbol_undefined
    b_string = symbol_undefined
    b_string = symbol_to_string(b)
    a_string = symbol_to_string(a)
    return stringLessThan_q(a_string, b_string)

def flatten(lists):
    if true_q(null_q(lists)):
        return symbol_emptylist
    else:
        if true_q(list_q(car(lists))):
            return append(flatten(car(lists)), flatten(cdr(lists)))
        else:
            return cons(car(lists), flatten(cdr(lists)))

def map_primitive():
    if true_q(iterator_q(car(args_reg))):
        GLOBALS['generator_reg'] = car(args_reg)
        GLOBALS['pc'] = iterate_collect
    else:
        len = symbol_undefined
        list_args = symbol_undefined
        list_args = listify(args_reg)
        len = length(args_reg)
        if true_q(numeric_equal(len, 1)):
            GLOBALS['list1_reg'] = car(list_args)
            GLOBALS['pc'] = map1
        else:
            if true_q(numeric_equal(len, 2)):
                GLOBALS['list2_reg'] = cadr(list_args)
                GLOBALS['list1_reg'] = car(list_args)
                GLOBALS['pc'] = map2
            else:
                GLOBALS['lists_reg'] = list_args
                GLOBALS['pc'] = mapN

def listify(arg_list):
    if true_q(null_q(arg_list)):
        return symbol_emptylist
    else:
        if true_q(list_q(car(arg_list))):
            return cons(car(arg_list), listify(cdr(arg_list)))
        else:
            if true_q(vector_q(car(arg_list))):
                return cons(vector_to_list(car(arg_list)), listify(cdr(arg_list)))
            else:
                if true_q(string_q(car(arg_list))):
                    return cons(string_to_list(car(arg_list)), listify(cdr(arg_list)))
                else:
                    if true_q(iter_q(car(arg_list))):
                        return cons(vector_to_list(list_native(car(arg_list))), listify(cdr(arg_list)))
                    else:
                        raise Exception("symbol_Map: " + format("cannot use object type '~a' in map", *[get_type(car(arg_list))]))

def iterate():
    iterator = symbol_undefined
    iterator = get_iterator(generator_reg)
    GLOBALS['iterator_reg'] = iterator
    GLOBALS['pc'] = iterate_continue

def iterate_continue():
    item = symbol_undefined
    item = next_item(iterator_reg)
    if true_q(null_q(item)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_110_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        GLOBALS['info_reg'] = symbol_none
        GLOBALS['env2_reg'] = env_reg
        GLOBALS['args_reg'] = List(item)
        GLOBALS['pc'] = apply_proc

def iterate_collect():
    iterator = symbol_undefined
    iterator = get_iterator(generator_reg)
    GLOBALS['iterator_reg'] = iterator
    GLOBALS['pc'] = iterate_collect_continue

def iterate_collect_continue():
    item = symbol_undefined
    item = next_item(iterator_reg)
    if true_q(null_q(item)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        GLOBALS['k2_reg'] = make_cont2(b_cont2_111_d, iterator_reg, proc_reg, env_reg, handler_reg, k_reg)
        GLOBALS['info_reg'] = symbol_none
        GLOBALS['env2_reg'] = env_reg
        GLOBALS['args_reg'] = List(item)
        GLOBALS['pc'] = apply_proc

def map1():
    if true_q(null_q(list1_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(dlr_proc_q(proc_reg)):
            GLOBALS['k_reg'] = make_cont2(b_cont2_113_d, list1_reg, proc_reg, k_reg)
            GLOBALS['list1_reg'] = cdr(list1_reg)
            GLOBALS['pc'] = map1
        else:
            GLOBALS['k2_reg'] = make_cont2(b_cont2_112_d, list1_reg, proc_reg, env_reg, handler_reg, k_reg)
            GLOBALS['info_reg'] = symbol_none
            GLOBALS['env2_reg'] = env_reg
            GLOBALS['args_reg'] = List(car(list1_reg))
            GLOBALS['pc'] = apply_proc

def map2():
    if true_q(null_q(list1_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(dlr_proc_q(proc_reg)):
            GLOBALS['k_reg'] = make_cont2(b_cont2_115_d, list1_reg, list2_reg, proc_reg, k_reg)
            GLOBALS['list2_reg'] = cdr(list2_reg)
            GLOBALS['list1_reg'] = cdr(list1_reg)
            GLOBALS['pc'] = map2
        else:
            GLOBALS['k2_reg'] = make_cont2(b_cont2_114_d, list1_reg, list2_reg, proc_reg, env_reg, handler_reg, k_reg)
            GLOBALS['info_reg'] = symbol_none
            GLOBALS['env2_reg'] = env_reg
            GLOBALS['args_reg'] = List(car(list1_reg), car(list2_reg))
            GLOBALS['pc'] = apply_proc

def mapN():
    if true_q(null_q(car(lists_reg))):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(dlr_proc_q(proc_reg)):
            GLOBALS['k_reg'] = make_cont2(b_cont2_117_d, lists_reg, proc_reg, k_reg)
            GLOBALS['lists_reg'] = Map(cdr, lists_reg)
            GLOBALS['pc'] = mapN
        else:
            GLOBALS['k2_reg'] = make_cont2(b_cont2_116_d, lists_reg, proc_reg, env_reg, handler_reg, k_reg)
            GLOBALS['info_reg'] = symbol_none
            GLOBALS['env2_reg'] = env_reg
            GLOBALS['args_reg'] = Map(car, lists_reg)
            GLOBALS['pc'] = apply_proc

def for_each_primitive():
    if true_q(iterator_q(car(lists_reg))):
        GLOBALS['generator_reg'] = car(lists_reg)
        GLOBALS['pc'] = iterate
    else:
        arg_list = symbol_undefined
        arg_list = listify(lists_reg)
        if true_q(null_q(car(arg_list))):
            GLOBALS['value2_reg'] = fail_reg
            GLOBALS['value1_reg'] = void_value
            GLOBALS['pc'] = apply_cont2
        else:
            if true_q(dlr_proc_q(proc_reg)):
                dlr_apply(proc_reg, Map(car, arg_list))
                GLOBALS['lists_reg'] = Map(cdr, arg_list)
                GLOBALS['pc'] = for_each_primitive
            else:
                GLOBALS['k2_reg'] = make_cont2(b_cont2_118_d, arg_list, proc_reg, env_reg, handler_reg, k_reg)
                GLOBALS['info_reg'] = symbol_none
                GLOBALS['env2_reg'] = env_reg
                GLOBALS['args_reg'] = Map(car, arg_list)
                GLOBALS['pc'] = apply_proc

def apply_native(proc, args):
    if true_q(dlr_proc_q(proc)):
        return dlr_apply(proc, args)
    else:
        return Apply(proc, args)

def make_dict():
    if true_q(null_q(args_reg)):
        GLOBALS['value2_reg'] = fail_reg
        GLOBALS['value1_reg'] = symbol_emptylist
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(association_q(car(args_reg))):
            GLOBALS['k2_reg'] = make_cont2(b_cont2_121_d, args_reg, k2_reg)
            GLOBALS['args_reg'] = cdr(args_reg)
            GLOBALS['pc'] = make_dict
        else:
            GLOBALS['k2_reg'] = make_cont2(b_cont2_120_d, args_reg, k2_reg)
            GLOBALS['args_reg'] = cdr(args_reg)
            GLOBALS['pc'] = make_dict

def make_toplevel_env():
    primitives = symbol_undefined
    primitives = List(List(symbol_p, modulo_prim, "(% arg0 arg1): modulo procedure for two arguments (aliases mod and modulo)"), List(symbol_multiply, times_prim, "(* ...): multiplication procedure; multiplies all arguments"), List(symbol_plus, plus_prim, "(+ ...): addition procedure; adds all arguments"), List(symbol_minus, minus_prim, "(- ...): subtraction procedure; subtracts all arguments"), List(symbol_divide, divide_prim, "(/ ...): division procedure; divides all arguments"), List(symbol___, quotient_prim, "(// arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases div and quotient)"), List(symbol_LessThan, lt_prim, "(< arg0 arg1): less-than procedure for two arguments"), List(symbol_LessThanEqual, lt_or_eq_prim, "(<= arg0 arg1): less-than or equal procedure for two arguments"), List(symbol_numeric_equal, equal_sign_prim, "(= arg0 arg1): numeric equality procedure for two arguments"), List(symbol_GreaterThan, gt_prim, "(> arg0 arg1): greater-than procedure for two arguments"), List(symbol_GreaterThanEqual, gt_or_eq_prim, "(>= arg0 arg1): greater-than or equal procedure for two arguments"), List(symbol_SCHEMEPATH, SCHEMEPATH, "List of search directories used with (load NAME)"), List(symbol_abort, abort_prim, "(abort) : aborts processing and returns to top level"), List(symbol_abs, abs_prim, "(abs value): absolute value procedure"), List(symbol_append, append_prim, "(append ...): append lists together into a single list"), List(symbol_Apply, apply_prim, "(apply PROCEDURE '(args...)): apply the PROCEDURE to the args"), List(symbol_assert, assert_prim, "(assert OPERATOR EXPRESSION ANSWER): assert that (OPERATOR EXPRESSION ANSWER) is #t"), List(symbol_assv, assv_prim, "(assv KEY ((ITEM VALUE) ...)): look for KEY in ITEMs; return matching (ITEM VALUE) or #f if not found"), List(symbol_atom_q, atom_q_prim, "(atom? ITEM): return #t if ITEM is a atom, #f otherwise"), List(symbol_boolean_q, boolean_q_prim, "(boolean? ITEM): return #t if ITEM is a boolean value"), List(symbol_box, box_prim, "(box ITEM): return a new box containing ITEM"), List(symbol_box_q, box_q_prim, "(box? ITEM): return #t if ITEM is a boxed value"), List(symbol_caaaar, caaaar_prim, "caaaar ...): "), List(symbol_caaadr, caaadr_prim, "(caaadr ...): "), List(symbol_caaar, caaar_prim, "(caaar ...): "), List(symbol_caadar, caadar_prim, "(caadar ...): "), List(symbol_caaddr, caaddr_prim, "(caaddr ...): "), List(symbol_caadr, caadr_prim, "(caadr ...): "), List(symbol_caar, caar_prim, "(caar ...): "), List(symbol_cadaar, cadaar_prim, "(cadaar ...): "), List(symbol_cadadr, cadadr_prim, "(cadadr ...): "), List(symbol_cadar, cadar_prim, "(cadar ...): "), List(symbol_caddar, caddar_prim, "(caddar ...): "), List(symbol_cadddr, cadddr_prim, "(cadddr ...): "), List(symbol_caddr, caddr_prim, "(caddr ITEM): return the (car (cdr (cdr ITEM)))"), List(symbol_cadr, cadr_prim, "(cadr ITEM): return the (car (cdr ITEM))"), List(symbol_call_with_current_continuation, call_cc_prim, "(call-with-current-continuation ...): "), List(symbol_call_cc, call_cc_prim, "(call/cc ...): "), List(symbol_car, car_prim, "(car LIST) returns the first element of LIST"), List(symbol_cd, current_directory_prim, "(cd [PATH]): get the current directory, or set it if PATH is given (alias current-directory)"), List(symbol_cdaaar, cdaaar_prim, "(cdaaar ...): "), List(symbol_cdaadr, cdaadr_prim, "(cdaadr ...): "), List(symbol_cdaar, cdaar_prim, "(cdaar ...): "), List(symbol_cdadar, cdadar_prim, "(cdadar ...): "), List(symbol_cdaddr, cdaddr_prim, "(cdaddr ...): "), List(symbol_cdadr, cdadr_prim, "(cdadr ...): "), List(symbol_cdar, cdar_prim, "(cdar ...): "), List(symbol_cddaar, cddaar_prim, "(cddaar ...): "), List(symbol_cddadr, cddadr_prim, "(cddadr ...): "), List(symbol_cddar, cddar_prim, "(cddar ...): "), List(symbol_cdddar, cdddar_prim, "(cdddar ...): "), List(symbol_cddddr, cddddr_prim, "(cddddr ...): "), List(symbol_cdddr, cdddr_prim, "(cdddr ...): "), List(symbol_cddr, cddr_prim, "(cddr ...): "), List(symbol_cdr, cdr_prim, "(cdr LIST) returns rest of LIST after (car LIST)"), List(symbol_char_to_integer, char_to_integer_prim, "(char->integer CHAR): return associated number of CHAR "), List(symbol_char_to_string, char_to_string_prim, "(char->string CHAR): "), List(symbol_char_alphabetic_q, char_alphabetic_q_prim, "(char-alphabetic? CHAR): return #t if CHAR is an alphabetic character, #f otherwise"), List(symbol_char_numeric_q, char_numeric_q_prim, "(char-numeric? CHAR): return #t if CHAR is a whitespace character, #f otherwise"), List(symbol_char_whitespace_q, char_whitespace_q_prim, "(char-whitespace? CHAR): return #t if CHAR is a whitespace character, #f otherwise"), List(symbol_char_is__q, char_is__q_prim, "(char=? CHAR1 CHAR2): return #t if CHAR1 has the same values as CHAR2, #f otherwise"), List(symbol_char_q, char_q_prim, "(char? ITEM): return #t if ITEM is a character, #f otherwise"), List(symbol_clear_unit_tests, clear_unit_tests_prim, "(clear-unit-tests): clear old unit tests. Usually run before define-tests"), List(symbol_cons, cons_prim, "(cons ITEM1 ITEM2): return a list with ITEM1 as car and ITEM2 as cdr (ITEM2 is typically a list)"), List(symbol_current_directory, current_directory_prim, "(current-directory [PATH]): get the current directory, or set it if PATH is given (alias cd)"), List(symbol_current_environment, current_environment_prim, "(current-environment): returns the current environment"), List(symbol_current_time, current_time_prim, "(current-time): returns the current time as number of seconds since 1970-1-1"), List(symbol_cut, cut_prim, "(cut ARGS...): return to toplevel with ARGS"), List(symbol_dir, dir_prim, "(dir [ITEM]): return items in environment, or, if ITEM is given, the items in module"), List(symbol_display, display_prim, "(display ITEM): display the ITEM as output"), List(symbol_div, quotient_prim, "(div arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and quotient)"), List(symbol_eq_q, eq_q_prim, "(eq? ITEM1 ITEM2): return #t if ITEM1 is eq to ITEM2, #f otherwise"), List(symbol_equal_q, equal_q_prim, "(equal? ITEM1 ITEM2): return #t if ITEM1 is equal to ITEM2, #f otherwise"), List(symbol_eqv_q, eqv_q_prim, "(eqv? ITEM1 ITEM2): return #t if ITEM1 and ITEM2 have the same value"), List(symbol_error, error_prim, "(error NAME MESSAGE): create an exception in NAME with MESSAGE"), List(symbol_eval, eval_prim, "(eval LIST): evaluates the LIST as a Scheme expression"), List(symbol_eval_ast, eval_ast_prim, "(eval-ast AST): evaluates the Abstract Syntax Tree as a Scheme expression (see parse and parse-string)"), List(symbol_even_q, even_q_prim, "(even? NUMBER): returns #t if NUMBER is odd, #f otherwise"), List(symbol_exit, exit_prim, "(exit): Exit the interpreter"), List(symbol_expt, expt_prim, "(expt BASE POWER): raise a base number to a power"), List(symbol_for_each, for_each_prim, "(for-each PROCEDURE LIST): apply PROCEDURE to each item in LIST, but don't return results"), List(symbol_format, format_prim, "(format STRING ITEM ...): format the string with ITEMS as arguments"), List(symbol_get, get_prim, "(get ...): "), List(symbol_get_completions, get_completions_prim, "(get-completions ...): returns completions for TAB"), List(symbol_get_stack_trace, get_stack_trace_prim, "(get-stack-trace): return the current stack trace"), List(symbol_hasitem, hasitem_prim, "(hasitem DICTIONARY KEY): does the DICTIONARY have this key?"), List(symbol_host_environment, host_environment_prim, "(host-environment): get the host environment (\"python\" or \"scheme\")"), List(symbol_import, import_prim, "(import MODULE...): import host-system modules; MODULEs are strings"), List(symbol_import_as, import_as_prim, "(import-as MODULE NAME): import a host-system module; MODULE is a string, and NAME is a symbol or string. Use * for NAME to import into toplevel environment"), List(symbol_import_from, import_from_prim, "(import-from MODULE NAME...): import from host-system module; MODULE is a string, and NAME is a symbol or string"), List(symbol_integer_to_char, integer_to_char_prim, "(integer->char INTEGER): return the assocated character of INTEGER"), List(symbol_iter_q, iter_q_prim, "(iter? ITEM): return #t if ITEM is a iterator, #f otherwise"), List(symbol_length, length_prim, "(length LIST): returns the number of elements in top level of LIST"), List(symbol_List, list_prim, "(list ITEM ...): returns a list composed of all of the items"), List(symbol_list_to_string, list_to_string_prim, "(list->string LIST): returns the LIST as a string"), List(symbol_list_to_vector, list_to_vector_prim, "(list->vector LIST): returns the LIST as a vector"), List(symbol_list_ref, list_ref_prim, "(list-ref LIST INDEX): returns the item in LIST at INDEX (zero-based)"), List(symbol_list_q, list_q_prim, "(list? ITEM): return #t if ITEM is a list, #f otherwise"), List(symbol_load, load_prim, "(load FILENAME...): loads the given FILENAMEs"), List(symbol_load_as, load_as_prim, "(load-as FILENAME MODULE-NAME): load the filename, putting items in MODULE-NAME namespace"), List(symbol_macros, macros_prim, "(macros): return the names of the macros"), List(symbol_make_set, make_set_prim, "(make-set LIST): returns a list of unique items from LIST"), List(symbol_make_vector, make_vector_prim, "(make-vector LENGTH): returns a vector of length LENGTH"), List(symbol_Map, map_prim, "(map PROCEDURE LIST...): apply PROCEDURE to each element of LIST, and return return results"), List(symbol_max, max_prim, "(max ...): returns the maximum value from the list of values"), List(symbol_member, member_prim, "(member ITEM LIST): return LIST if ITEM in top level of LIST"), List(symbol_memq, memq_prim, "(memq ...): "), List(symbol_memv, memv_prim, "(memv ...): "), List(symbol_min, min_prim, "(min ...): returns the minimum value from the list of values"), List(symbol_mod, modulo_prim, "(mod arg0 arg1): modulo procedure for two arguments (aliases % and modulo)"), List(symbol_modulo, modulo_prim, "(modulo arg0 arg1): modulo procedure for two arguments (aliases mod and %)"), List(symbol_newline, newline_prim, "(newline): displays a new line in output"), List(symbol_not, not_prim, "(not ITEM): returns the boolean not of ITEM; ITEM is only #t when #t, otherwise #f"), List(symbol_null_q, null_q_prim, "(null? ITEM): return #t if ITEM is empty list, #f otherwise"), List(symbol_number_to_string, number_to_string_prim, "(number->string NUMBER): return NUMBER as a string"), List(symbol_number_q, number_q_prim, "(number? ITEM): return #t if ITEM is a number, #f otherwise"), List(symbol_odd_q, odd_q_prim, "(odd? NUMBER): returns #t if NUMBER is even, #f otherwise"), List(symbol_pair_q, pair_q_prim, "(pair? ITEM): "), List(symbol_parse, parse_prim, "(parse LIST): parse a list; returns Abstract Syntax Tree (AST)"), List(symbol_parse_string, parse_string_prim, "(parse-string STRING): parse a string; returns Abstract Syntax Tree (AST)"), List(symbol_print, print_prim, "(print ITEM): "), List(symbol_printf, printf_prim, "(printf FORMAT ARGS...): "), List(symbol_procedure_q, procedure_q_prim, "(procedure? ITEM): return #t if ITEM is a procedure, #f otherwise"), List(symbol_python_eval, python_eval_prim, "(python-eval PYTHON-EXPRESSION [globals [locals]]): return the result of evaluating PYTHON-EXPRESSION string"), List(symbol_python_exec, python_exec_prim, "(python-exec PYTHON-STATEMENTS [globals [locals]]): return the result of evaluating PYTHON-STATEMENTS string"), List(symbol_quit, exit_prim, "(quit): Exit the interpreter"), List(symbol_quotient, quotient_prim, "(quotient arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and div)"), List(symbol_rac, rac_prim, "(rac LIST): return the last item of LIST"), List(symbol_random, random_prim, "(random N): return a random number in the range [0, N)"), List(symbol_Range, range_prim, "(range END), (range START END), or (RANGE START END STEP): (all integers)"), List(symbol_rdc, rdc_prim, "(rdc LIST): return everything but last item in LIST"), List(symbol_read_string, read_string_prim, "(read-string ...): "), List(symbol_remainder, remainder_prim, "(remainder NUMBER1 NUMBER2): returns the remainder after dividing NUMBER1 by NUMBER2"), List(symbol_require, require_prim, "(require ...): "), List(symbol_reverse, reverse_prim, "(reverse LIST): "), List(symbol_round, round_prim, "(round NUMBER): round NUMBER to the nearest integer (may return float)"), List(symbol_set_car_b, set_car_b_prim, "(set-car! LIST ITEM): set the car of LIST to be ITEM"), List(symbol_set_cdr_b, set_cdr_b_prim, "(set-cdr! LIST ITEM): set the car of LIST to be ITEM (which is typically a list)"), List(symbol_snoc, snoc_prim, "(snoc ITEM LIST): cons the ITEM onto the end of LIST"), List(symbol_sqrt, sqrt_prim, "(sqrt NUMBER): return the square root of NUMBER"), List(symbol_string, string_prim, "(string ITEM): returns ITEM as a string"), List(symbol_string_to_list, string_to_list_prim, "(string->list STRING): string STRING as a list of characters"), List(symbol_string_to_number, string_to_number_prim, "(string->number STRING): return STRING as a number"), List(symbol_string_to_symbol, string_to_symbol_prim, "(string->symbol STRING): return STRING as a symbol"), List(symbol_string_length, string_length_prim, "(string-length STRING): returns the length of a string"), List(symbol_string_ref, string_ref_prim, "(string-ref STRING INDEX): return the character of STRING at position INDEX"), List(symbol_stringLessThan_q, stringLessThan_q_prim, "(string<? STRING1 STRING2): compare two strings to see if STRING1 is less than STRING2"), List(symbol_string_is__q, string_is__q_prim, "(string=? STRING1 STRING2): return #t if STRING1 is the same as STRING2, #f otherwise"), List(symbol_string_q, string_q_prim, "(string? ITEM): return #t if ITEM is a string, #f otherwise"), List(symbol_string_join, string_join_prim, "(string-join \", \" '(1 2 3)): gives \"1, 2, 3\""), List(symbol_substring, substring_prim, "(substring STRING START [END]): return the substring of STRING starting with position START and ending before END. If END is not provided, it defaults to the length of the STRING"), List(symbol_symbol_to_string, symbol_to_string_prim, "(symbol->string SYMBOL): return SYMBOL as a string"), List(symbol_symbol_q, symbol_q_prim, "(symbol? ITEM): return #t if ITEM is a symbol, #f otherwise"), List(symbol_unbox, unbox_prim, "(unbox BOX): return the contents of BOX"), List(symbol_unparse, unparse_prim, "(unparse AST): "), List(symbol_unparse_procedure, unparse_procedure_prim, "(unparse-procedure ...): "), List(symbol_use_stack_trace, use_stack_trace_prim, "(use-stack-trace BOOLEAN): set stack-trace usage on/off"), List(symbol_use_tracing, use_tracing_prim, "(use-tracing [BOOLEAN]): get tracing setting, or set it on/off if BOOLEAN is given"), List(symbol_vector, vector_prim, "(vector [ITEMS]...): return ITEMs as a vector"), List(symbol_vector_to_list, vector_to_list_prim, "(vector->list VECTOR): return VECTOR as a list"), List(symbol_vector_length, vector_length_prim, "(vector-length VECTOR): returns length of VECTOR"), List(symbol_vector_ref, vector_ref_prim, "(vector-ref VECTOR INDEX): "), List(symbol_vector_set_b, vector_set_b_prim, "(vector-set! VECTOR INDEX VALUE): sets the item at INDEX of VECTOR"), List(symbol_vector_q, vector_q_prim, "(vector? ITEM): return #t if ITEM is a vector, #f otherwise"), List(symbol_void, void_prim, "(void): The null value symbol"), List(symbol_zero_q, zero_q_prim, "(zero? NUMBER): return #t if NUMBER is equal to zero, #f otherwise"), List(symbol_assq, assq_prim, "(assq ...): "), List(symbol_dict, dict_prim, "(dict ...): "), List(symbol_float, float_prim, "(float NUMBER): return NUMBER as a floating point value"), List(symbol_getitem, getitem_prim, "(getitem DICTIONARY ITEM): returns the VALUE of DICTIONARY[ITEM]"), List(symbol_globals, globals_prim, "(globals): get global environment"), List(symbol_int_, int_prim, "(int NUMBER): return NUMBER as an integer"), List(symbol_property, property_prim, "(property ...): "), List(symbol_rational, rational_prim, "(rational NUMERATOR DENOMINTAOR): return a rational number"), List(symbol_reset_toplevel_env, reset_toplevel_env_prim, "(reset-toplevel-env): reset the toplevel environment"), List(symbol_setitem, setitem_prim, "(setitem DICTIONARY ITEM VALUE): sets and returns DICTIONARY[ITEM] with VALUE"), List(symbol_sort, sort_prim, "(sort PROCEDURE LIST): sort the list using PROCEDURE to compare items"), List(symbol_string_append, string_append_prim, "(string-append STRING1 STRING2): append two strings together"), List(symbol_string_split, string_split_prim, "(string-split STRING CHAR): return a list with substrings of STRING where split by CHAR"), List(symbol_typeof, typeof_prim, "(typeof ITEM): returns type of ITEM"), List(symbol_use_lexical_address, use_lexical_address_prim, "(use-lexical-address [BOOLEAN]): get lexical-address setting, or set it on/off if BOOLEAN is given"))
    return make_initial_env_extended(Map(car, primitives), Map(cadr, primitives), Map(caddr, primitives))

def reset_toplevel_env():
    GLOBALS['toplevel_env'] = make_toplevel_env()
    return void_value

def make_external_proc(external_function_object):
    return make_proc(b_proc_179_d, external_function_object)

def process_formals_and_args(params, args, info, handler, fail):
    return cons(process_formals(params, info, handler, fail), process_args(args, params, info, handler, fail))

def process_formals(params, info, handler, fail):
    return Map(get_symbol, params)

def process_args(args, params, info, handler, fail):
    return args

def get_values_for_params(params, associations, used, info, handler, fail):
    if true_q(null_q(params)):
        if true_q((not(null_q(associations))) and (association_q(car(associations))) and ((caar(associations)) is (symbol_multiply))):
            return List(get_value(car(associations)))
        else:
            return symbol_emptylist
    else:
        return cons(get_value_from_associations(car(params), associations, info, handler, fail), get_values_for_params(cdr(params), associations, cons(car(params), used), info, handler, fail))

def get_value_from_associations(param, associations, info, handler, fail):
    symbol = symbol_undefined
    value = symbol_undefined
    symbol = get_symbol(param)
    value = assq(symbol, associations)
    if true_q(value):
        return get_value(value)
    else:
        if true_q(association_q(param)):
            return get_value(param)
        else:
            GLOBALS['fail_reg'] = fail
            GLOBALS['handler_reg'] = handler
            GLOBALS['info_reg'] = info
            GLOBALS['msg_reg'] = format("missing parameter: ~a", param)
            GLOBALS['pc'] = runtime_error

def get_arg_associations(args, params, must_be_association, info, handler, fail):
    if true_q(null_q(args)):
        return symbol_emptylist
    else:
        if true_q(association_q(car(args))):
            return cons(car(args), get_arg_associations(cdr(args), params, True, info, handler, fail))
        else:
            if true_q(must_be_association):
                GLOBALS['fail_reg'] = fail
                GLOBALS['handler_reg'] = handler
                GLOBALS['info_reg'] = info
                GLOBALS['msg_reg'] = format("non-keyword arg following keyword arg: ~a", car(args))
                GLOBALS['pc'] = runtime_error
            else:
                if true_q(null_q(params)):
                    return List(association(symbol_multiply, args))
                else:
                    return cons(association(get_symbol(car(params)), car(args)), get_arg_associations(cdr(args), cdr(params), False, info, handler, fail))

def get_symbol(item):
    if true_q(association_q(item)):
        return car(item)
    else:
        if true_q(symbol_q(item)):
            return item
        else:
            raise Exception("symbol_get_symbol: " + format("invalid symbol ~a", *[item]))

def get_value(item):
    if true_q(association_q(item)):
        return caddr(item)
    else:
        return item

def association_q(x):
    return (list_q(x)) and (numeric_equal(length(x), 3)) and ((cadr(x)) is (symbol_colon))

def make_associations(dict):
    if true_q(null_q(dict)):
        return symbol_emptylist
    else:
        keyword = symbol_undefined
        value = symbol_undefined
        value = cadar(dict)
        keyword = caar(dict)
        return cons(association(keyword, value), make_associations(cdr(dict)))

def pattern_q(x):
    return (null_q(x)) or (number_q(x)) or (boolean_q(x)) or (symbol_q(x)) or ((pair_q(x)) and (pattern_q(car(x))) and (pattern_q(cdr(x))))

def pattern_variable_q(x):
    return (symbol_q(x)) and (equal_q("?", substring(symbol_to_string(x), 0, 1)))

def constant_q(x):
    return (not(pattern_variable_q(x))) and (not(pair_q(x)))

def occurs_q():
    if true_q(constant_q(pattern_reg)):
        GLOBALS['value_reg'] = False
        GLOBALS['pc'] = apply_cont
    else:
        if true_q(pattern_variable_q(pattern_reg)):
            GLOBALS['value_reg'] = equal_q(var_reg, pattern_reg)
            GLOBALS['pc'] = apply_cont
        else:
            GLOBALS['k_reg'] = make_cont(b_cont_54_d, pattern_reg, var_reg, k_reg)
            GLOBALS['pattern_reg'] = car(pattern_reg)
            GLOBALS['pc'] = occurs_q

def unify_patterns_hat():
    if true_q(pattern_variable_q(p1_reg)):
        if true_q(pattern_variable_q(p2_reg)):
            GLOBALS['value_reg'] = make_sub(symbol_unit, p1_reg, p2_reg, ap2_reg)
            GLOBALS['pc'] = apply_cont
        else:
            GLOBALS['k_reg'] = make_cont(b_cont_55_d, ap2_reg, p1_reg, p2_reg, k_reg)
            GLOBALS['pattern_reg'] = p2_reg
            GLOBALS['var_reg'] = p1_reg
            GLOBALS['pc'] = occurs_q
    else:
        if true_q(pattern_variable_q(p2_reg)):
            GLOBALS['temp_1'] = p2_reg
            GLOBALS['temp_2'] = p1_reg
            GLOBALS['temp_3'] = ap2_reg
            GLOBALS['temp_4'] = ap1_reg
            GLOBALS['p1_reg'] = temp_1
            GLOBALS['p2_reg'] = temp_2
            GLOBALS['ap1_reg'] = temp_3
            GLOBALS['ap2_reg'] = temp_4
            GLOBALS['pc'] = unify_patterns_hat
        else:
            if true_q((constant_q(p1_reg)) and (constant_q(p2_reg)) and (equal_q(p1_reg, p2_reg))):
                GLOBALS['value_reg'] = make_sub(symbol_empty)
                GLOBALS['pc'] = apply_cont
            else:
                if true_q((pair_q(p1_reg)) and (pair_q(p2_reg))):
                    GLOBALS['apair2_reg'] = ap2_reg
                    GLOBALS['apair1_reg'] = ap1_reg
                    GLOBALS['pair2_reg'] = p2_reg
                    GLOBALS['pair1_reg'] = p1_reg
                    GLOBALS['pc'] = unify_pairs_hat
                else:
                    GLOBALS['value_reg'] = False
                    GLOBALS['pc'] = apply_cont

def unify_pairs_hat():
    GLOBALS['k_reg'] = make_cont(b_cont_57_d, apair1_reg, apair2_reg, pair1_reg, pair2_reg, k_reg)
    GLOBALS['ap2_reg'] = car_hat(apair2_reg)
    GLOBALS['ap1_reg'] = car_hat(apair1_reg)
    GLOBALS['p2_reg'] = car(pair2_reg)
    GLOBALS['p1_reg'] = car(pair1_reg)
    GLOBALS['pc'] = unify_patterns_hat

def instantiate_hat():
    if true_q(constant_q(pattern_reg)):
        GLOBALS['value2_reg'] = ap_reg
        GLOBALS['value1_reg'] = pattern_reg
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q(pattern_variable_q(pattern_reg)):
            GLOBALS['avar_reg'] = ap_reg
            GLOBALS['var_reg'] = pattern_reg
            GLOBALS['pc'] = apply_sub_hat
        else:
            if true_q(pair_q(pattern_reg)):
                GLOBALS['k2_reg'] = make_cont2(b_cont2_125_d, ap_reg, pattern_reg, s_reg, k2_reg)
                GLOBALS['ap_reg'] = car_hat(ap_reg)
                GLOBALS['pattern_reg'] = car(pattern_reg)
                GLOBALS['pc'] = instantiate_hat
            else:
                raise Exception("symbol_instantiate_hat: " + format("bad pattern: ~a", *[pattern_reg]))

def make_sub(*args):
    args = List(*args)
    return cons(symbol_substitution, args)

def apply_sub_hat():
    temp_1 = symbol_undefined
    temp_1 = cdr(s_reg)
    if true_q((car(temp_1)) is (symbol_empty)):
        GLOBALS['value2_reg'] = avar_reg
        GLOBALS['value1_reg'] = var_reg
        GLOBALS['k_reg'] = k2_reg
        GLOBALS['pc'] = apply_cont2
    else:
        if true_q((car(temp_1)) is (symbol_unit)):
            new_var = symbol_undefined
            new_pattern = symbol_undefined
            new_apattern = symbol_undefined
            new_apattern = list_ref(temp_1, 3)
            new_pattern = list_ref(temp_1, 2)
            new_var = list_ref(temp_1, 1)
            if true_q(equal_q(var_reg, new_var)):
                GLOBALS['value2_reg'] = new_apattern
                GLOBALS['value1_reg'] = new_pattern
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2
            else:
                GLOBALS['value2_reg'] = avar_reg
                GLOBALS['value1_reg'] = var_reg
                GLOBALS['k_reg'] = k2_reg
                GLOBALS['pc'] = apply_cont2
        else:
            if true_q((car(temp_1)) is (symbol_composite)):
                s1 = symbol_undefined
                s2 = symbol_undefined
                s2 = list_ref(temp_1, 2)
                s1 = list_ref(temp_1, 1)
                GLOBALS['k2_reg'] = make_cont2(b_cont2_126_d, s2, k2_reg)
                GLOBALS['s_reg'] = s1
                GLOBALS['pc'] = apply_sub_hat
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
or_transformer_hat = make_macro(b_macro_7_d)
cond_transformer_hat = make_macro(b_macro_8_d)
let_star_transformer_hat = make_macro(b_macro_9_d)
case_transformer_hat = make_macro(b_macro_10_d)
record_case_transformer_hat = make_macro(b_macro_11_d)
define_datatype_transformer_hat = make_macro(b_macro_12_d)
cases_transformer_hat = make_macro(b_macro_13_d)
macro_env = symbol_undefined
REP_k = make_cont2(b_cont2_57_d)
REP_handler = make_handler2(b_handler2_2_d)
REP_fail = make_fail(b_fail_1_d)
_starlast_fail_star = REP_fail
_startokens_left_star = symbol_undefined
try_parse_handler = make_handler2(b_handler2_3_d)
unit_test_table = symbol_undefined
_startracing_on_q_star = False
_starstack_trace_star = List(symbol_emptylist)
_staruse_stack_trace_star = True
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
get_prim = make_proc(b_proc_114_d)
call_cc_prim = make_proc(b_proc_116_d)
abort_prim = make_proc(b_proc_117_d)
require_prim = make_proc(b_proc_118_d)
cut_prim = make_proc(b_proc_119_d)
reverse_prim = make_proc(b_proc_120_d)
append_prim = make_proc(b_proc_121_d)
string_to_number_prim = make_proc(b_proc_122_d)
string_is__q_prim = make_proc(b_proc_123_d)
list_to_vector_prim = make_proc(b_proc_124_d)
list_to_string_prim = make_proc(b_proc_125_d)
char_to_string_prim = make_proc(b_proc_126_d)
string_to_list_prim = make_proc(b_proc_127_d)
string_to_symbol_prim = make_proc(b_proc_128_d)
symbol_to_string_prim = make_proc(b_proc_129_d)
vector_to_list_prim = make_proc(b_proc_130_d)
vector_length_prim = make_proc(b_proc_131_d)
get_completions_prim = make_proc(b_proc_132_d)
dir_prim = make_proc(b_proc_133_d)
macros_prim = make_proc(b_proc_134_d)
current_time_prim = make_proc(b_proc_135_d)
map_prim = make_proc(b_proc_136_d)
for_each_prim = make_proc(b_proc_137_d)
format_prim = make_proc(b_proc_138_d)
current_environment_prim = make_proc(b_proc_139_d)
import_prim = make_proc(b_proc_140_d)
import_as_prim = make_proc(b_proc_141_d)
import_from_prim = make_proc(b_proc_142_d)
not_prim = make_proc(b_proc_143_d)
printf_prim = make_proc(b_proc_144_d)
vector_prim = make_proc(b_proc_145_d)
vector_set_b_prim = make_proc(b_proc_146_d)
vector_ref_prim = make_proc(b_proc_147_d)
make_vector_prim = make_proc(b_proc_148_d)
error_prim = make_proc(b_proc_149_d)
list_ref_prim = make_proc(b_proc_150_d)
current_directory_prim = make_proc(b_proc_151_d)
round_prim = make_proc(b_proc_152_d)
use_stack_trace_prim = make_proc(b_proc_153_d)
use_tracing_prim = make_proc(b_proc_154_d)
eqv_q_prim = make_proc(b_proc_155_d)
vector_q_prim = make_proc(b_proc_156_d)
atom_q_prim = make_proc(b_proc_157_d)
iter_q_prim = make_proc(b_proc_158_d)
getitem_prim = make_proc(b_proc_159_d)
setitem_prim = make_proc(b_proc_160_d)
hasitem_prim = make_proc(b_proc_161_d)
list_q_prim = make_proc(b_proc_162_d)
procedure_q_prim = make_proc(b_proc_163_d)
stringLessThan_q_prim = make_proc(b_proc_164_d)
float_prim = make_proc(b_proc_165_d)
globals_prim = make_proc(b_proc_166_d)
int_prim = make_proc(b_proc_167_d)
assq_prim = make_proc(b_proc_168_d)
dict_prim = make_proc(b_proc_169_d)
property_prim = make_proc(b_proc_170_d)
rational_prim = make_proc(b_proc_171_d)
reset_toplevel_env_prim = make_proc(b_proc_172_d)
sort_prim = make_proc(b_proc_173_d)
string_append_prim = make_proc(b_proc_174_d)
string_split_prim = make_proc(b_proc_175_d)
typeof_prim = make_proc(b_proc_176_d)
use_lexical_address_prim = make_proc(b_proc_177_d)
host_environment_prim = make_proc(b_proc_178_d)
toplevel_env = symbol_undefined
pc_halt_signal = False
def run(setup, *args):
    args = List(*args)
    Apply(setup, args)
    return trampoline()

initialize_globals()

if __name__ == '__main__':
    print('Calysto Scheme, version 1.4.0')
    print('----------------------------')
    print('Use (exit) to exit')
    import sys
    for filename in sys.argv[1:]:
        if filename.startswith('-'): continue
        if load_native(filename): continue
        break
    if '-i' in sys.argv[1:] or sys.argv[1:] == []:
        read_eval_print_loop_rm()
        print()
