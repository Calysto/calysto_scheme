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
    opts = yasi.parse_options([])
    opts.dialect = "scheme"
except:
    yasi = None

PY3 = sys.version_info[0] == 3

__version__ = "1.4.7"

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

def sort_native(args, env2, info, handler, fail, k2):
    ## just passing extra args to be compatible with chez scheme
    p = args.car
    arg = args.cdr.car
    try:
        GLOBALS['value1_reg'] = sort(p, arg)
        GLOBALS['value2_reg'] = fail
        GLOBALS['k_reg'] = k2
        GLOBALS['pc'] = apply_cont2
    except Exception as exc:
        GLOBALS['fail_reg'] = fail
        GLOBALS['handler_reg'] = handler
        GLOBALS['info_reg'] = info
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
    return pair_q(item) and (item.car is symbol_procedure)

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
    for larg in largs:
        if association_q(larg):
            sym = symbol_to_string(larg.car)
            if sym == "*":
                fargs = larg.cdr.cdr.car
            elif sym == "**":
                fkwargs = larg.cdr.cdr.car
            else:
                fkwargs[sym] = larg.cdr.cdr.car
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
        return thing[item]
    else:
        return thing.__getitem__(item)

def setitem_native(thing, item, value):
    if isinstance(thing, dict):
        thing[item] = value
    else:
        thing.__setitem__(item, value)

def hasitem_native(thing, item):
    if isinstance(thing, dict):
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
