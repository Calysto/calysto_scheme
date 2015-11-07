from __future__ import division, print_function

class Translator(object):
    def __init__(self, flags=None):
        self.program = []
        self.symbols = self.get_initial_symbols()
        self.options = {}
        ## --noprimitives
        if flags:
            for flag in flags:
                if flag.startswith("--"):
                    self.options[flag[2:]] = True
        self.initialize()

    def initialize(self):
        pass

    def get_initial_symbols(self):
        return ["()"]

    def Print(self, indent, *args, **kwargs):
        kwargs["file"] = self.fp
        print(" " * indent, end="", file=self.fp)
        print(*args, **kwargs)

    def to_ignore(self):
        # FIXME: get rid of these by marking them as native,
        # or move to overrides
        return [
            "void-value", "restart-rm", "trampoline",
            "read-content", "read-multiline-test",
            "string->integer", "string->decimal", "string->rational", 
            "string-split", 
            "get-current-time", "type?", 
            "read-eval-print-loop", "unparse", "unparse-exps", "qq-expand-cps_",
            "qq-expand-list-cps_", 
            ## defined in Scheme.cs:
            "true?", 
            "init-cont", "init-cont2", "init-cont3", "init-cont4",
            "init-handler", "init-handler2", "init-fail",
            "make-cont", "make-cont2", "make-cont3", "make-cont4", "make-macro", "make-proc",
            "make-fail", "make-handler", "make-handler2",
        ] + self.overrides()

    def overrides(self):
        return ["length-one?", "length-two?", "length-at-least?", "all-numeric?", "pair?", "make-safe"]

    def parse(self, text):
        self.program = self.parser(self.lexer(text))

    def parse_file(self, filename):
        text = open(filename).read()
        self.program = self.parser(self.lexer(text))

    def function_q(self, expr):
        if len(expr) > 2:
            if isinstance(expr[2], list):
                if expr[0] in ["define", "define+", "define*"] and len(expr[2]) > 0:
                    if expr[2][0] == "lambda":
                        return True
                    elif isinstance(expr[1], list):
                        return True
        return False

    def fix_symbol_name(self, name):
        # name used in symbol_NAME
        return self.fix_name(name)

    def fix_name(self, name):
        if name == "()":
            return "emptylist"
        elif name == "def":
            return "def_"
        elif name == "input":
            return "input_"
        elif name == "class":
            return "class_"
        elif name == "list":
            return "List"
        elif name == "apply":
            return "Apply"
        elif name == "apply!":
            return "Apply"
        elif name == "range":
            return "Range"
        elif name == "map":
            return "Map"
        elif name == "=":
            return "numeric_equal"
        elif name == "<=":
            return "LessThanEqual"
        elif name == "<":
            return "LessThan"
        elif name == ">":
            return "GreaterThan"
        elif name == ">=":
            return "GreaterThanEqual"
        elif name == "-":
            return "minus"
        elif name == "*":
            return "multiply"
        elif name == "/":
            return "divide"
        elif name == "+":
            return "plus"
        elif name == "int":
            return "int_"
        elif name.startswith('"'):
            return name
        elif name.startswith("#"):
            return self.replace_char(name)
        elif name.startswith("<") and name.endswith(">"):
            name = "b_" + name[1:-1] + "_d"
        for pattern in [("->", "_to_"), (">", "to_"), ("<", "LessThan"), ("*", "_star"),
                        ("=", "_is_"), ("-", "_"), ("?", "_q"), ("!", "_b"), ("/", "_"),
                        (".", "dot"), ("+", "plus"), ("%" "percent"), ("^", "_hat"),
                        (":", "colon")]:
            name = name.replace(pattern[0], pattern[1])
        return name

    def make_symbol_name(self, symbol):
        if symbol not in self.symbols:
            self.symbols.append(symbol)
        return "symbol_" + self.fix_symbol_name(symbol)

    def lexer(self, text):
        """
        Lexer for Scheme code. Takes a string of Scheme code and breaks it
        up into a flat list of the important parts.
    
        > lexer("(define x (lambda (x) (cond ((test? x) (func x)) (else return))))"))
        ['(', 'define', 'x', '(', 'lambda', '(', 'x', ')', '(', 'cond', '(', '(', 'test?',
         'x', ')', '(', 'func', 'x', ')', ')', '(', 'else', 'return', ')', ')', ')', ')']
        """
        state = None
        current = ""
        retval = []
        i = 0
        while i < len(text):
            c = text[i]
            if state == "in-comment":
                if c == "\n":
                    state = None
                # else, ignore
            elif state == "in-string":
                if c == '\\':
                    current += c
                    i += 1
                    c = text[i]
                    current += c
                elif c == '"':
                    state = None
                    current += c
                    retval.append(current)
                    current = ""
                else:
                    current += c
            else:
                if c in ["(", ")", "'"]:
                    if current:
                        retval.append(current)
                        current = ""
                    retval.append(c)
                elif c == "#":
                    if current:
                        retval.append(current)
                        current = ""
                    i += 1
                    if text[i] != "\\":
                        current = "#" + text[i]
                    else:
                        i += 1
                        current = "#\\" + text[i]
                        i += 1
                        while (not text[i] in [' ', ')']) and i < len(text):
                            current += text[i]
                            i += 1
                        i -= 1
                elif c == ";":
                    if current:
                        retval.append(current)
                        current = ""
                    state = "in-comment"
                elif c == '"':
                    if current:
                        retval.append(current)
                        current = ""
                    current = '"'
                    state = "in-string"
                elif c in [" ", "\n"]:
                    if current:
                        retval.append(current)
                        current = ""
                else:
                    current += c
            i += 1
        if current:
            retval.append(current)
        return retval

    def replace_char(self, name):
        if name == "#t":
            return "True"
        elif name == "#f":
            return "False"
        elif name == "#\\newline":
            return "make_char('\\n')"
        elif name == "#\\nul":
            return "make_char('\\0')"
        elif name == "#\\return":
            return "make_char('\\r')"
        elif name == "#\\space":
            return "make_char(' ')"
        elif name == "#\\tab":
            return "make_char('\\t')"
        elif name == "#\\backspace":
            return "make_char('\\b')"
        elif name == "#\\page":
            return "make_char(u\"\\u000C\")"
        elif name == "#\\'":
            return "make_char(\"'\")"
        elif name == "#\\\\":
            return "make_char('\\\\')"
        elif len(name) == 3:
            return "make_char('%s')" % name[2]
        else:
            raise Exception("unknown char: " + name)

    def parser(self, lexed):
        """
        Take a lexed list and parse it into a tree of s-expressions represented
        as lists. Side-effect: collects and replaces symbols.
        
        > parser(lexer("(define x (lambda (x) (cond ((test? x) (func x)) (else return))))"))
        [['define', 'x', ['lambda', ['x'], ['cond', [['test?', 'x'], 
                                                     ['func', 'x']], ['else', 'return']]]]]
        """
        retval = []
        stack = []
        current = None
        i = 0
        while i < len(lexed):
            item = lexed[i]
            if item == "(": ## (define x 1)   ((test? 1) 1)
                if current is not None:
                    stack.append(current)
                current = []
            elif item == ")":
                if len(stack) > 0:
                    temp = stack.pop()
                    if current is not None:
                        temp.append(current)
                    current = temp
                else:
                    if current is not None:
                        retval.append(current)
                    current = None
            elif item == "'": ## quoted
                if i + 1 < len(lexed):
                    if lexed[i + 1] != "(":
                        i += 1
                        current.append(self.make_symbol_name(lexed[i]))
                    else:
                        i += 2
                        current.append("symbol_emptylist")
                else: # same as any item
                    current.append(item)
            else:
                current.append(item)
            i += 1
        if current:
            retval.append(current)
        if stack:
            raise Exception("stack:", stack)
        return retval

class PythonTranslator(Translator):
    def preamble(self):
        self.Print(0, """####################################################
## Scheme in Python
##
## Jim Marshall
## Doug Blank
####################################################

""")
        self.Print(0, file("Scheme.py").read())

    def process_function_definition(self, expr, locals, indent):
        ## (define x (lambda (x) (cond ((test? x) (func x)) (else return))))
        convert_args = ""
        if isinstance(expr[1], list):
            function_name = self.fix_name(expr[1][0])
            args = self.fix_name(expr[1][1])
        else:
            function_name = self.fix_name(expr[1])
            if isinstance(expr[2][1], list):
                args = ", ".join(map(self.fix_name, expr[2][1]))
            else:
                args = "*%s" % self.fix_name(expr[2][1]) # var args
                convert_args = "%s = List(*%s)" % (self.fix_name(expr[2][1]), 
                                                   self.fix_name(expr[2][1]))
        if ", dot, " in args:
            args = args.replace(", dot, ", ", *") # var args on end
            var_arg = args.rsplit("*", 1)[1]
            convert_args = "%s = List(*%s)" % (var_arg, var_arg)
        self.Print(indent, "def %s(%s):" % (function_name, args))
        if convert_args:
            self.Print(indent + 4, convert_args)
        body = expr[2][2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "")

    def process_infix_op(self, expr, op):
        retval = "(%s)" % self.process_app(expr[1])
        for e in expr[2:]:
            retval += " %s (%s)" % (op, self.process_app(e))
        return retval

    def process_app(self, expr):
        if isinstance(expr, list):
            if expr[0] in ['and', 'or', '+', '-', '*']:
                return self.process_infix_op(expr, expr[0])
            elif expr[0] == "eq?":
                return self.process_infix_op(expr, "is")
            elif expr[0] == 'if':
                return "(%s if %s else %s)" % (self.process_app(expr[2]),
                                               self.process_app(expr[1]),
                                               self.process_app(expr[3]))
            else:
                if expr[0] == "error":
                    # (error 'tag "msg ~a" args...)
                    exception_message = "\"%s: \" + format(%s, *[%s])" % (
                        expr[1], expr[2], ", ".join([self.process_app(e) for e in expr[3:]]))
                    return "raise Exception(%s)" % exception_message
                else:
                    ## function call:
                    return "%s(%s)" % (self.fix_name(expr[0]),
                                       ", ".join([self.process_app(e) for e in expr[1:]]))
        else:
            return self.fix_name(expr)

    def process_while(self, expr, locals, indent):
        # (while test body...)
        self.Print(indent, "while %s:" % self.process_app(expr[1]))
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)

    def process_let(self, expr, locals, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            # locals:
            self.Print(indent, "%s = %s" % (self.fix_name(pair[0]), self.process_app(pair[1])))
        locals.extend([pair[0] for pair in expr[1]])
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent)

    def process_if(self, expr, locals, indent):
        ## (if 1 2 3)
        self.Print(indent, "if true_q(%s):" % self.process_app(expr[1]))
        self.process_statement(expr[2], locals, indent + 4)
        if len(expr) > 3:
            self.Print(indent, "else:")
            self.process_statement(expr[3], locals, indent + 4)

    def get_define_name(self, expr):
        ## (define function ...)
        return expr[1]

    def check_global(self, name, locals):
        if name in locals:
            return self.fix_name(name)
        else:
            return "GLOBALS['%s']" % self.fix_name(name)

    def process_assignment(self, expr, locals, indent):
        self.Print(indent, "%s = %s" % (self.check_global(expr[1], locals), 
                           self.process_app(expr[2])))

    def process_return(self, expr, indent):
        self.Print(indent, "return %s" % self.process_app(expr[1]))

    def process_definition(self, expr, locals, indent):
        # global
        self.Print(indent, "%s = %s" % (self.fix_name(expr[1]), self.process_app(expr[2])))

    def process_cond(self, expr, locals, indent):
        ## (cond (test result) ...)
        self.Print(indent, "if true_q(%s):" % self.process_app(expr[1][0]))
        self.process_statement(expr[1][1], locals, indent + 4)
        for rest in expr[2:]:
            if rest[0] == "else":
                self.Print(indent, "else:")
            else:
                self.Print(indent, "elif %s:" % self.process_app(rest[0]))
            self.process_statement(rest[1], locals, indent + 4)

    def process_statement(self, expr, locals, indent):
        if self.function_q(expr):
            # handles all define/*/+ functions
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_function_definition(expr, locals, indent)
        elif expr[0] == "define": # global variable
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_definition(expr, locals, indent)
        elif expr[0] == "define-native":
            pass
        elif expr[0] == "let":
            self.process_let(expr, locals, indent)
        elif expr[0] == "if":
            self.process_if(expr, locals, indent)
        elif expr[0] == "cond":
            self.process_cond(expr, locals, indent)
        elif expr[0] == "while":
            self.process_while(expr, locals, indent)
        elif expr[0] == "load":
            pass
        elif expr[0] == "set!":
            self.process_assignment(expr, locals, indent)
        elif expr[0] == "begin":
            for e in expr[1:]:
                self.process_statement(e, locals, indent)
        elif expr[0] == "return*":
            self.process_return(expr, indent)
        else: # must be a function call
            self.Print(indent, self.process_app(expr))

    def to_ignore(self):
        return super(PythonTranslator, self).to_ignore() + ["highlight-expression"]
                
    def translate(self, filename):
        with open('Scheme.py', 'rb') as fid:
            for line in fid:
                line = line.decode('utf-8')
                if line.startswith('__version__'):
                    __version__ = line.strip().split()[-1][1:-1]
                    break

        self.fp = open(filename, "w")
        self.preamble()
        for symbol in self.symbols:
            if self.options.get("noprimitives", False) and symbol.endswith("-prim"):
                continue
            self.Print(0, "%s = make_symbol(\"%s\")" % (self.make_symbol_name(symbol), symbol))
        self.Print(0, "")
        for statement in self.program:
            self.process_statement(statement, [], 0)
        self.Print(0, "")
        self.Print(0, "if __name__ == '__main__':")
        self.Print(4, "print('Calysto Scheme, version %s')" % __version__)
        self.Print(4, "print('----------------------------')")
        self.Print(4, "print('Use (exit) to exit')")
        self.Print(4, "GLOBALS['toplevel_env'] = make_toplevel_env()")
        self.Print(4, "GLOBALS['macro_env'] = make_macro_env_hat()")
        self.Print(4, "import sys")
        self.Print(4, "for filename in sys.argv[1:]:")
        self.Print(4, "    if filename.startswith('-'): continue")
        self.Print(4, "    if load_native(filename): continue")
        self.Print(4, "    break")
        self.Print(4, "if '-i' in sys.argv[1:] or sys.argv[1:] == []:")
        self.Print(4, "    read_eval_print_loop_rm()")
        self.Print(4, "    print()")
        self.Print(0, "else:")
        self.Print(4, "initialize_globals()")

class CSharpTranslator(Translator):
    def preamble(self):
        self.Print(0, """// -------------------------------------------------
/*
 Scheme in C#

 Jim Marshall
 Doug Blank
*/
// -------------------------------------------------

#pragma warning disable 109
using System;
using System.Reflection;

public class PJScheme:Scheme
{

  static object void_value = null;

  public static object trampoline () {
	while (pc != null) {
            try {
	        pc ();
	    } catch (Exception e ) {
                if (config.DEBUG > 0) {
                    exception_reg = make_exception("UnHandledException", e.ToString(), symbol_none, symbol_none, symbol_none);
                } else {
                    string [] parts = get_parts(e.ToString(), NEWLINE_STRING);
		    exception_reg = make_exception("UnHandledException", parts[0].ToString(), symbol_none, symbol_none, symbol_none);
                }
		pc = (Function) apply_handler2;
	    }
	}
	return (final_reg);
  }

  public static Closure dlr_func(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return delegate (object[] args) { 
      proc_reg = schemeProc;
      args_reg = PJScheme.array_to_list (args);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public delegate object ParamsFunction (params object[] args); 

  public static ParamsFunction callback(object schemeProc) {
    // Return a Csharp function that when invoked acts
    // like schemeProc by calling apply_proc on its args.
    return args => { 
      proc_reg = schemeProc;
      args_reg = PJScheme.array_to_list (args);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      return PJScheme.trampoline();
    };
  }

  public static object apply_comparison_rm(object schemeProc, object arg1, object arg2) {
      // used from non-pcs code that evaluates a scheme proc in sort
      save_k2_reg = k2_reg;
      proc_reg = schemeProc;
      args_reg = PJScheme.list (arg1, arg2);
      handler_reg = REP_handler;
      k2_reg = REP_k;
      pc = (Function) apply_proc;
      object retval = PJScheme.trampoline();
      k2_reg = save_k2_reg;
      return retval;
  }

   static object save_k2_reg = null;
   static int _closure_depth = 0;
   static bool _trace_pause = false;

   public static bool get_trace_pause () {
      return _trace_pause;
   }

   public static void set_trace_pause (bool value) {
      _trace_pause = value;
   }

   public static int get_closure_depth ()
   {
      return _closure_depth;
   }

   public static void increment_closure_depth ()
   {
      _closure_depth++;

   }

   public static void decrement_closure_depth ()
   {
      _closure_depth--;
   }

   public static object repeat(object item, object times) {
      object retval = symbol_emptylist;
      for (int i=0; i < ((int)times); i++) {
          retval = cons(item, retval);
      }
      return retval;
   }

   public static bool use_lexical_address(object value) {
	if (!null_q(value)) {
	    value = car(value);
	    _staruse_lexical_address_star = true_q(value);
	}
	return (bool) _staruse_lexical_address_star;
   }

   // *tracing-on?*
   public static object tracing_on(object value) {
	if (null_q(value)) {
	    return _startracing_on_q_star;
	} else {
	    value = car(value);
	    _startracing_on_q_star = (bool)value;
	    return null;
	}
   }

""")

    def contains_return(self, statements):
        if statements == []:
            return False
        for statement in statements:
            if isinstance(statement, list):
                if self.contains_return(statement):
                    return True
            elif statement == "return*":
                return True
        return False

    def process_function_definition(self, expr, locals, indent):
        ## (define x (lambda (x) (cond ((test? x) (func x)) (else return))))
        convert_args = ""
        if self.contains_return(expr[2][2:]):
            if "?" in expr[1]:
                return_type = "bool"
            elif expr[0] == "define*":
                return_type = "void"
            elif expr[0].endswith("-rm"):
                return_type = "void"
            else:
                return_type = "object"
        else:
            return_type = "void"
        if isinstance(expr[1], list):
            function_name = self.fix_name(expr[1][0])
            args = self.fix_name(expr[1][1])
        else:
            function_name = self.fix_name(expr[1])
            if isinstance(expr[2][1], list):
                args = ", ".join(map(self.fix_name, expr[2][1]))
            else:
                args = "params object [] t_%s" % self.fix_name(expr[2][1]) # var args
                convert_args = "object %s = sList(t_%s);" % (self.fix_name(expr[2][1]), 
                                                             self.fix_name(expr[2][1]))
        if ", dot, " in args:
            args = args.replace(", dot, ", ", params object [] t_") # var args on end
            original_arg = args.rsplit("[] t_", 1)[1]
            var_arg = args.rsplit("[] ", 1)[1]
            convert_args = "object %s = sList(%s);" % (original_arg, var_arg)
        self.Print(indent, "public static %s %s(%s) {" % (return_type, function_name, self.make_arg_types(args)))
        if convert_args:
            self.Print(indent + 4, convert_args)
        body = expr[2][2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "}")
        self.Print(indent, "")

    def make_arg_types(self, string):
        retval = ""
        if string:
            args = string.split(", ")
            for arg in args:
                if retval:
                    retval += ", "
                if " " in arg:
                    retval += arg
                else:
                    retval += "object " + arg
        return retval

    def process_infix_op(self, expr, op):
        retval = "true_q(%s)" % self.process_app(expr[1])
        for e in expr[2:]:
            retval += " %s true_q(%s)" % (op, self.process_app(e))
        return "(%s)" % retval

    def process_app(self, expr):
        if isinstance(expr, list):
            #if expr[0] in ['+', '-', '*']:
            #    return self.process_infix_op(expr, expr[0])
            #elif expr[0] == "eq?":
            #    return self.process_infix_op(expr, "is")
            if expr[0] == "and":
                return self.process_infix_op(expr, "&&")
            elif expr[0] == "or":
                return self.process_infix_op(expr, "||")
            elif expr[0] == 'if':
                # if 1 2 3
                return "(%s ? %s : %s)" % (self.process_app(expr[1]),
                                           self.process_app(expr[2]),
                                           self.process_app(expr[3]))
            else:
                ## function call:
                if expr[0] == "not":
                    return "(! true_q(%s))" % ", ".join([self.process_app(e) for e in expr[1:]])
                elif expr[0] == "error":
                    exception_msg = "\"%s: \" + format(%s, %s)" % (
                        expr[1], expr[2], ", ".join([self.process_app(e) for e in expr[3:]]))
                    return "throw new Exception(%s);" % exception_msg
                else:
                    # Handle make_cont(function, ...) -> make_cont(id, "cont", ...)
                    if expr[0].startswith("make-"):
                        make_, what = expr[0].split("-", 1)
                        if what in self.methods:
                            id = expr[1].split("-", 1)[1][:-1] # <cont-3>
                            self.methods[what]["count"] = max(self.methods[what]["count"], int(id))
                            return "%s(%s)" % (self.fix_name(expr[0]),
                                               ", ".join([self.process_app(e) for e in (['"%s"' % what, id] + expr[2:])]))
                    return "%s(%s)" % (self.fix_name(expr[0]),
                                       ", ".join([self.process_app(e) for e in expr[1:]]))
        else:
            return self.fix_function_name_as_argument(expr)

    def fix_function_name_as_argument(self, name):
        if name.startswith('"'):
            return name
        elif name.endswith("-prim"):
            return self.fix_name(name)
        elif name.startswith("#\\"):
            return self.fix_name(name)
        elif name == "string":
            return "make_string_proc"
        elif name.endswith("*") and name != "*":
            return self.fix_name(name)
        # the names of cps functions:
        elif name in ["equal-objects?", "equal-vectors?", "occurs?", "quote?^"]:
            return self.fix_name(name)
        # the name of C# functions, which need a delegate:
        elif (name in ["=", ">=", "car", "cdr", "string-length", "+", "string-ref", "sqrt",
                       "/", "remainder", "quotient", "char->integer", "integer->char",
                       "cons", "cadr", "-", "*", "%", "<", ">", "<=", "abs", "memq",
                       "range", "snoc", "rac", "rdc", "set-car!", "set-cdr!", "reverse", 
                       "string->number", "list->vector", "list->string", "format", "printf",
                       "vector-native", "vector-ref", "make-vector", "list-ref", "setup", 
                       "safe-print", "modulo", "vector_native", "car^", "cadr^", "string->symbol",
                       "make-binding", "aunparse", "format-stack-trace", "get-variables-from-frame",
                       "vector->list", "char->string", "string->list", "symbol->string", 
                       "float", "int", "globals", "assq", "dict", "property",
                       "reset-toplevel-env", "sort", "string-append", "string-split", "make-symbol",
                       "type", "use-lexical-address", "min", "max",
                       "caar", "cadr", "cdar", "cddr" , 
                       "caaar", "caadr", "cadar", "caddr" , "cdaar", "cdadr", "cddar", "cdddr",
                       "caaaar", "caaadr", "caadar", "caaddr" , "cadaar", "cadadr", "caddar", "cadddr",
                       "cdaaar", "cdaadr", "cdadar", "cdaddr" , "cddaar", "cddadr", "cdddar", "cddddr",
                       "contains-native", "getitem-native", "setitem-native"
                   ] or 
            "?" in name):
            return self.fix_name(name) + "_proc"
        else:
            return self.fix_name(name)

    def process_while(self, expr, locals, indent):
        # (while test body...)
        self.Print(indent, "while (true_q(%s)) {" % self.process_app(expr[1]))
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent + 4)
        self.Print(indent, "}")

    def process_let(self, expr, locals, indent):
        # (let ((x 1)(y u)) ...)
        for pair in expr[1]:
            # locals:
            self.Print(indent, "object %s = %s;" % (self.fix_name(pair[0]), self.process_app(pair[1])))
        locals.extend([pair[0] for pair in expr[1]])
        body = expr[2:]
        for statement in body:
            self.process_statement(statement, locals, indent)

    def process_if(self, expr, locals, indent):
        ## (if 1 2 3)
        self.Print(indent, "if (true_q(%s)) {" % self.process_app(expr[1]))
        self.process_statement(expr[2], locals, indent + 4)
        if len(expr) > 3:
            self.Print(indent, "} else {")
            self.process_statement(expr[3], locals, indent + 4)
            self.Print(indent, "}")
        else:
            self.Print(indent, "}")

    def get_define_name(self, expr):
        ## (define function ...)
        return expr[1]

    def check_global(self, name, locals):
        return self.fix_name(name)

    def process_assignment(self, expr, locals, indent):
        self.Print(indent, "%s = %s;" % (self.check_global(expr[1], locals), 
                           self.process_app(expr[2])))

    def process_return(self, expr, indent):
        self.Print(indent, "return %s;" % self.process_app(expr[1]))

    def process_definition(self, expr, locals, indent):
        # global
        if expr[1] == "pc":
            self.Print(indent, "public static Function %s = (Function) null;" % (self.fix_name(expr[1]), ))
        elif "?^" in expr[1]:
            self.Print(indent, "public static Func<object,bool> %s = %s;" % (self.fix_name(expr[1]), self.process_app(expr[2])))
        elif expr[2] in ["#t", "#f"]:
            self.Print(indent, "public static bool %s = %s;" % (self.fix_name(expr[1]), self.process_app(expr[2])))
        else:
            self.Print(indent, "public static object %s = %s;" % (self.fix_name(expr[1]), self.process_app(expr[2])))

    def process_cond(self, expr, locals, indent):
        ## (cond (test result) ...)
        self.Print(indent, "if (true_q(%s)) {" % self.process_app(expr[1][0]))
        self.process_statement(expr[1][1], locals, indent + 4)
        for rest in expr[2:]:
            if rest[0] == "else":
                self.Print(indent, "} else {")
            else:
                self.Print(indent, "} else if (true_q(%s)) {" % self.process_app(rest[0]))
            self.process_statement(rest[1], locals, indent + 4)
            self.Print(indent, "}")

    def process_statement(self, expr, locals, indent):
        if (self.options.get("noprimitives", False) and 
            isinstance(expr, list) and len(expr) > 1 and 
            isinstance(expr[1], str) and 
            (expr[1].endswith("-prim") or expr[1] == "make-toplevel-env")):
            return
        if self.function_q(expr):
            # handles all define/*/+ functions
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_function_definition(expr, locals, indent)
        elif expr[0] == "define": # global variable
            if not self.get_define_name(expr) in self.to_ignore():
                self.process_definition(expr, locals, indent)
        elif expr[0] == "define-native":
            pass
        elif expr[0] == "let":
            self.process_let(expr, locals, indent)
        elif expr[0] == "if":
            self.process_if(expr, locals, indent)
        elif expr[0] == "cond":
            self.process_cond(expr, locals, indent)
        elif expr[0] == "while":
            self.process_while(expr, locals, indent)
        elif expr[0] == "load":
            pass
        elif expr[0] == "set!":
            self.process_assignment(expr, locals, indent)
        elif expr[0] == "begin":
            for e in expr[1:]:
                self.process_statement(e, locals, indent)
        elif expr[0] == "return*":
            self.process_return(expr, indent)
        else: # must be a function call
            self.Print(indent, "%s;" % self.process_app(expr))
                
    def translate(self, filename):
        indent = 0
        self.fp = open(filename, "w")
        self.preamble()
        for symbol in self.symbols:
            if self.options.get("noprimitives", False) and symbol.endswith("-prim"):
                continue
            self.Print(indent + 4, "public static object %s = make_symbol(\"%s\");" % (self.make_symbol_name(symbol), symbol))
        self.Print(indent + 4, "")
        for statement in self.program:
            self.process_statement(statement, [], indent + 4)
        self.generate_extras(indent + 4)
        if self.options.get("noprimitives", False):
            self.Print(indent + 4, "public static object make_toplevel_env() {")
            self.Print(indent + 8, "object variables = symbol_emptylist;")
            self.Print(indent + 8, "object values = symbol_emptylist;")
            self.Print(indent + 8, "return make_initial_env_extended(variables, values);")
            self.Print(indent + 4, "}")
            self.Print(indent + 4, "")
        self.Print(indent, "}")

    def fix_symbol_name(self, name):
        # name used in symbol_NAME
        if name == "eq?":
            return "eq_q"
        elif (name == "equal?"):
            return "equal_q"
        else:
            return self.fix_name(name)

    def fix_name(self, name):
        if (name == "list"):
            return "sList";
        elif (name == "string"):
            return "make_string";
        elif (name == "float"):
            return "float_";
        elif (name == "int"):
            return "int_";
        elif (name == "operator"):
            return "operator_";
        elif (name == "bool"):
            return "bool_";
        elif (name == "char"):
            return "char_";
        elif (name == "eq?"):
            return "Eq";
        elif (name == "equal?"):
            return "equal_q";
        elif name == "map":
            return "map"
        elif name == "apply":
            return "apply"
        elif name == "apply!":
            return "ApplyPlus"
        elif name == "apply*":
            return "dlr_apply"
        elif name == "+":
            return "Add"
        elif name == "-":
            return "Subtract"
        elif name == "*":
            return "Multiply"
        elif name == "/":
            return "Divide"
        else:
            return super(CSharpTranslator, self).fix_name(name)

    def replace_char(self, name):
        if name == "#t":
            return "true"
        elif name == "#f":
            return "false"
        elif name == "#\\page":
            return "'\\f'"
        elif name == "#\\newline":
            return "'\\n'"
        elif name == "#\\nul":
            return "'\\0'"
        elif name == "#\\return":
            return "'\\r'"
        elif name == "#\\space":
            return "' '"
        elif name == "#\\tab":
            return "'\\t'"
        elif name == "#\\backspace":
            return "'\\b'"
        elif name == "#\\page":
            return "u\"\\u000C\""
        elif name == "#\\'":
            return "'\\''"
        elif name == "#\\\\":
            return "'\\\\'"
        elif len(name) == 3:
            return "'%s'" % name[2]
        else:
            raise Exception("replace_char: '%s'" % name)

    def initialize(self):
        self.methods = {
            "cont": {
                "tag": "continuation",
                "count": 0,
            }, 
            "cont2": {
                "tag": "continuation2",
                "count": 0,
            }, 
            "cont3": {
                "tag": "continuation3",
                "count": 0,
            }, 
            "cont4": {
                "tag": "continuation4",
                "count": 0,
            }, 
            "macro": {
                "tag": "macro-transformer",
                "count": 0,
            },
            "fail": {
                "tag": "fail-continuation",
                "count": 0,
            }, 
            "handler": {
                "tag": "handler",
                "count": 0,
            }, 
            "handler2": {
                "tag": "handler2",
                "count": 0,
            }, 
            "proc": {
                "tag": "procedure",
                "count": 0,
            }
        }

    def get_initial_symbols(self):
        return ["()", "<extension>", "method", "field", "constructor", 
                "property", "done", "module"]

    def generate_extras(self, indent):
        for mi in self.methods:
            self.Print(indent, "public static MethodInfo[] mi_%s;" % mi)
        self.Print(indent, "")
        self.Print(indent, "public static void initialize_method_info() {")
        for mi in self.methods:
            self.Print(indent + 4, "mi_%s = new MethodInfo[%s];" % (mi, int(self.methods[mi]["count"]) + 1))
        self.Print(indent + 4, "")
        for mi in self.methods:
            self.Print(indent + 4,  "for (int i = 1; i < %s; i++) {" % int(self.methods[mi]["count"] + 1)) 
            self.Print(indent + 8,  "mi_%s[i] = typeof(PJScheme).GetMethod(String.Format(\"b_%s_{0}_d\", i));" % (mi, mi))
            self.Print(indent + 8,  "if (mi_%s[i] == null) {" % mi)
            self.Print(indent + 12, "throw new Exception(String.Format(\"Undefined mi: mi_%s[{0}]\", i));" % mi)
	    self.Print(indent + 8,  "}")
            self.Print(indent + 4,  "}")
            self.Print(indent + 4, "")
        self.Print(indent,  "}")
        self.Print(indent,  "")
        for mi in self.methods:
            self.Print(indent + 0,  "public static object make_%s (string what, int id, params object[] args) {" % mi)
            self.Print(indent + 4,  "return sList(make_symbol(\"%s\"), what, id, args);" % self.methods[mi]["tag"])
            self.Print(indent + 0,  "}")
            self.Print(indent + 0,  "")

    def overrides(self):
        return ["pc-halt-signal", "map^", "set-use-stack-trace", "run", 
                "handle-debug-info", "safe-print", "reset-toplevel-env",
                "make-safe", "highlight-expression"]

if __name__ == "__main__":
    ## infile outfile
    import sys
    if sys.argv[2].rsplit(".", 1)[1] == "cs":
        translator = CSharpTranslator(sys.argv)
    elif sys.argv[2].rsplit(".", 1)[1] == "py":
        translator = PythonTranslator(sys.argv)
    else:
        raise Exception("invalid file type: " + sys.argv[2].rsplit(".", 1)[1])
    translator.parse_file(sys.argv[1])
    translator.translate(sys.argv[2])
