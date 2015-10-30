import sys
sys.path.append("..")

from scheme import (process_formals_and_args, car, cdr, 
                    execute_string_rm, make_symbol as sym, 
                    ENVIRONMENT, List)
CORRECT = 0
ERROR = 0
ENVIRONMENT["DEBUG"] = True

equal_q = execute_string_rm("(func (lambda (a b) (equal? a b)))")

def test(id, name, formals, args, cformals=None, cargs=None, error=None):
    global CORRECT, ERROR
    try:
        results = process_formals_and_args(formals, args)
    except Exception as e:
        if error and error in str(e):
            CORRECT += 2
            return
        else:
            print(id, "UNEXPECTED ERROR:", name, str(e))
            ERROR += 2
            return
    # No error, expected or otherwise:
    rformals = car(results)
    # Formals:
    test_name = "formals do not match:"
    if  equal_q(rformals, cformals):
        CORRECT += 1
    elif error == test_name:
        CORRECT += 1
    else:
        ERROR += 1
        print(id, "ERROR:", name, test_name, rformals, "should be:", cformals)
    # Args:
    rargs = cdr(results)
    test_name = "args do not match:"
    if  equal_q(rargs, cargs):
        CORRECT += 1
    elif error == test_name:
        CORRECT += 1
    else:
        ERROR += 1
        print(id, "ERROR:", name, test_name, rargs, "should be:", cargs)

## Begin testing:

## Runts are not handled by associations:
##   * not handled by named arguments
##   * not handled by default values

## Basics, make sure that standard methods work:

test(1.1, "((lambda n ...))", 
     List(), List(), 
     List(), List())
test(1.2, "((lambda n ...) 1)", 
     List(), List(1), 
     List(), List(1))
test(1.3, "((lambda n ...) 1 2)", 
     List(), List(1, 2), 
     List(), List(1, 2))
test(1.4, "((lambda (n) ...) 1)", 
     List(sym("n")), List(1), 
     List(sym("n")), List(1))
test(1.5, "((lambda (a b) ...) 1)", 
     List(sym("a"), sym("b")), List(1), 
     List(sym("a"), sym("b")), 
     error="No default value for b")
test(1.6, "((lambda (a . b) ...) 1 2)", 
     List(sym("a")), List(1, 2), 
     List(sym("a")), List(1, 2))

## Default values:

test(2.1, "((lambda ((n : 1)) ...) 2)", 
     List(List(sym("n"), sym(":"), 1)), List(2), 
     List(sym("n")), List(2))
test(2.2, "((lambda ((n : 1))))", 
     List(List(sym("n"), sym(":"), 1)), List(), 
     List(sym("n")), List(1))

## Named values:

test(3.1, "((lambda (n) ...) (n : 2))", 
     List(sym("n")), List(List(sym("n"), sym(":"), 2)), 
     List(sym("n")), List(2))
test(3.2, "((lambda (a b) ...) 1 (b : 2))", 
     List(sym("a"), sym("b")), List(1, List(List(sym("b"), sym(":"), 2))), 
     List(sym("a"), sym("b")), List(1, 2))

## Named values and default values:

## Varargs as parameters:

## Varargs as arguments:

## kwargs as parameters:

## kwargs as arguments:


print()
print("CORRECT:", CORRECT, "ERROR:", ERROR)
