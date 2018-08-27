;; Unit tests for Scheme functions

;;(use-lexical-address #f)
;;(define! DEBUG #t)

(clear-unit-tests)

;; ;;---------------------------------------------------------------------
;; ;; collect is like list comprehension in Python

(define-syntax collect
  [(collect ?exp for ?var in ?list)
   (filter-map (lambda (?var) ?exp) (lambda (?var) #t) ?list)]
  [(collect ?exp for ?var in ?list if ?condition)
   (filter-map (lambda (?var) ?exp) (lambda (?var) ?condition) ?list)])

(define filter-map
  (lambda (f pred? values)
    (if (null? values)
	'()
	(if (pred? (car values))
	    (cons (f (car values)) (filter-map f pred? (cdr values)))
	    (filter-map f pred? (cdr values))))))

(define-syntax time
  [(time ?exp) (let ((start (current-time)))
		 ?exp
		 (- (current-time) start))])

;;---------------------------------------------------------------------
;; for loops

(define-syntax for
  [(for ?exp times do . ?bodies)
   (for-repeat ?exp (lambda () . ?bodies))]
  [(for ?var in ?exp do . ?bodies)
   (for-iterate1 ?exp (lambda (?var) . ?bodies))]
  [(for ?var at (?i) in ?exp do . ?bodies)
   (for-iterate2 0 ?exp (lambda (?var ?i) . ?bodies))]
  [(for ?var at (?i ?j . ?rest) in ?exp do . ?bodies)
   (for ?var at (?i) in ?exp do
	(for ?var at (?j . ?rest) in ?var do . ?bodies))])

(define for-repeat
  (lambda (n f)
    (if (< n 1)
	'done
	(begin
	  (f)
	  (for-repeat (- n 1) f)))))

(define for-iterate1
  (lambda (values f)
    (if (null? values)
	'done
	(begin
	  (f (car values))
	  (for-iterate1 (cdr values) f)))))

(define for-iterate2
  (lambda (i values f)
    (if (null? values)
	'done
	(begin
	  (f (car values) i)
	  (for-iterate2 (+ i 1) (cdr values) f)))))

(define matrix2d
  '((10 20)
    (30 40)
    (50 60)
    (70 80)))

(define matrix3d
  '(((10 20 30) (40 50 60))
    ((70 80 90) (100 110 120))
    ((130 140 150) (160 170 180))
    ((190 200 210) (220 230 240))))

;;---------------------------------------------------------------------
;; streams

(define-syntax scons
  [(scons ?x ?y) (cons ?x (lambda () ?y))])

(define scar car)

(define scdr
  (lambda (s)
    (let ((result ((cdr s))))
      (set-cdr! s (lambda () result))
      result)))

(define first
  (lambda (n s)
    (if (= n 0)
	'()
	(cons (scar s) (first (- n 1) (scdr s))))))

(define nth
  (lambda (n s)
    (if (= n 0)
	(scar s)
	(nth (- n 1) (scdr s)))))

(define smap
  (lambda (f s)
    (scons (f (scar s)) (smap f (scdr s)))))

(define ones (scons 1 ones))

(define nats (scons 0 (combine nats + ones)))

(define combine
  (lambda (s1 op s2)
    (scons (op (scar s1) (scar s2)) (combine (scdr s1) op (scdr s2)))))

(define fibs (scons 1 (scons 1 (combine fibs + (scdr fibs)))))

(define facts (scons 1 (combine facts * (scdr nats))))

(define ! (lambda (n) (nth n facts)))

(define my-odd? 'undefined)
(define my-even? 'undefined)

(letrec
    ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
     (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
  (set! my-odd? odd)
  (set! my-even? even))

(define un-parse
  (lambda (exp)
    (cases lc-exp exp
	   (var-exp (var) var)
	   (lambda-exp (bound-var body) (list bound-var body))
	   (app-exp (rator rand) (list rator rand)))))


(define-tests main
  (assert equal?
	  `(list ,(+ 1 2) 4)
	  '(list 3 4)
	  "quasiquote")
  (assert equal?
	  (% 10 3)
	  1
	  "modulo")
  (assert equal?
	  (% 10 3)
	  1
	  "modulo")
  (assert equal?
	  (% 10 3)
	  1
	  "modulo"
	  )
  (assert equal?
	  (* 2 3)
	  6
	  "*")
  (assert equal?
	  (+ 7 8)
	  15
	  "+")
  (assert equal?
	  (- 5 2)
	  3
	  "-")
  (assert equal?
	  (/ 3 4)
	  3/4
	  "/")
  (assert equal?
	  (/ 4 3)
	  4/3
	  "/")
  (assert equal?
	  (// 3 4)
	  0
	  "//")
  (assert equal?
	  (// 4 3)
	  1
	  "//")
  (assert equal?
	  (< 5 2)
	  #f
	  "<")
  (assert equal?
	  (<= 5 6)
	  #t
	  "<=")
  (assert equal?
	  (= 6 7)
	  #f
	  "=")
  (assert equal?
	  (> 9 2)
	  #t
	  ">")
  (assert equal?
	  (>= 4 5)
	  #f
	  ">=")
  (assert =
	  (min 0 -7 4 10 7)
	  -7
	  "min")
  (assert =
	  (max 0 -7 4 10 7)
	  10
	  "max")
  (assert equal?
	  (abs -1)
	  1
	  "abs")
  (assert equal?
	  (and 4 1 2 #t '() 0)
	  0
	  "and")
  (assert equal?
	  (append '(1 2 3) '(4 5 6))
	  '(1 2 3 4 5 6)
	  "append")
  (assert equal?
	  (apply car '((1)))
	  1
	  "apply")
  (assert equal?
	  (assq 1 '((1 2) (3 4)))
	  '(1 2)
	  "assq")
  (assert equal?
	  (assv 1 '((1 2) (3 4)))
	  '(1 2)
	  "assv")
  (assert equal?
	  (atom? 1)
	  #t
	  "atom?")
  (assert equal?
	  (boolean? #t)
	  #t
	  "boolean?")
  (assert equal?
	  (caaaar '(((((hello there) this is a test) what is this) another item) in the list))
	  '(hello there)
	  "caaaar")
  (assert equal?
	  (caaadr '(((((hello there) this is a test) what is this) another item) ((((((1 2 3 ) 4 5 6) 7 8 9) 10 11 12) 13 14 15) 16 17 18)))
	  '((((1 2 3) 4 5 6) 7 8 9) 10 11 12)
	  "caaadr")
  (assert equal?
	  (caaar '(((((hello there) this is a test) what is this) another item) in the list))
	  '((hello there) this is a test)
	  "caaar")
  (assert equal?
	  (caadar '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) in the list))
	  '((1 2 3) 4 5 6)
	  "caadar")
  (assert equal?
	  (caaddr
	   '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) head ((1 2) 3 4) in the list))
	  '(1 2)
	  "caaddr")
  (assert equal?
	  (caadr '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) (in this) ((7 8)) the list))
	  'in
	  "caadr")
  (assert equal?
	  (caar '(((((hello there) this is a test) what is this) another item) in the list))
	  '(((hello there) this is a test) what is this)
	  "caar")
  (assert equal?
	  (cadaar '(((((hello there) this is a test) (what) is this) (yet another) item) in the list))
	  '(what)
	  "cadaar")
  (assert equal?
	  (cadadr '(((((hello there) this is a test) what is this) (yet another) item) (in the) list))
	  'the
	  "cadadr")
  (assert equal?
	  (cadar '(((((hello there) this is a test) what is this) (yet another) item) in the list))
	  '(yet another)
	  "cadar")
  (assert equal?
	  (caddar '(((((hello there) this is a test) what is this) another item) in the list))
	  'item
	  "caddar")
  (assert equal?
	  (cadddr '(((((hello there) this is a test) what is this) another item) in the list))
	  'list
	  "cadddr")
  (assert equal?
	  (caddr '(((((hello there) this is a test) what is this) another item) in the list))
	  'the
	  "caddr")
  (assert equal?
	  (cadr '(((((hello there) this is a test) what is this) another item) in the list))
	  'in
	  "cadr")
  (assert equal?
	  (car '(((((hello there) this is a test) what is this) another item) in the list))
	  '((((hello there) this is a test) what is this) another item)
	  "car")
  (assert =
	  (case 'thing1 (thing2 1) (thing1 2))
	  2
	  "case")
  (assert =
	  (case 'thing1 (thing2 1) ((thing1 thing3) 2))
	  2
	  "case-2")
  (assert =
	  (case 'thingx (thing2 1) ((thing1 thing3) 2) (else 3))
	  3
	  "case-3")
  (assert (lambda (a b) (string? a))
	  (cd)
	  ""
	  "cd")
  (assert equal?
	  (cdaaar '(((((hello there) this is a test) what is this) another item)))
	  '(this is a test)
	  "cdaaar")
  (assert equal?
	  (cdaadr '(((((hello there) this is a test) what is this) another item) ((7 8)) 9 10))
	  '(8)
	  "cdaadr")
  (assert equal?
	  (cdaar '(((((hello there) this is a test) what is this) another item)))
	  '(what is this)
	  "cdaar")
  (assert equal?
	  (cdadar '(((((hello there) this is a test) what is this) (another two) items)))
	  '(two)
	  "cdadar")
  (assert equal?
	  (cdaddr '(((((hello there) this is a test) what is this) another item) 1 (2 5) 3 4))
	  '(5)
	  "cdaddr")
  (assert equal?
	  (cdadr '(((((hello there) this is a test) what is this) another item) (1 6) (2 5) 3 4))
	  '(6)
	  "cdadr")
  (assert equal?
	  (cdar '(((((hello there) this is a test) what is this) another item)))
	  '(another item)
	  "cdar")
  (assert equal?
	  (cddaar '(((((hello there) this is a test) what is this) another item) 1 (2) 3))
	  '(is this)
	  "cddaar")
  (assert equal?
	  (cddadr '(((((hello there) this is a test) what is this) another item) (7 13) (8 12) 9 10))
	  '()
	  "cddadr")
  (assert equal?
	  (cddar '(((((hello there) this is a test) what is this) another item)))
	  '(item)
	  "cddar")
  (assert equal?
	  (cdddar '(((((hello there) this is a test) what is this) another item)))
	  '()
	  "cdddar")
  (assert equal?
	  (cddddr '(((((hello there) this is a test) what is this) another item) 1 2 3 4 5))
	  '(4 5)
	  "cddddr")
  (assert equal?
	  (cdddr '(((((hello there) this is a test) what is this) another item) 1 2 3 4))
	  '(3 4)
	  "cdddr")
  (assert equal?
	  (cddr '(((((hello there) this is a test) what is this) another item) 1 2 3))
	  '(2 3)
	  "cddr")
  (assert equal?
	  (cdr '(((((hello there) this is a test) what is this) another item) 1 2 3))
	  '(1 2 3)
	  "cdr")
  (assert =
	  (char->integer #\a)
	  97
	  "char->integer")
  (assert equal?
	  (char->string #\b)
	  "b"
	  "char->string")
  (assert equal?
	  (char-alphabetic? #\A)
	  #t
	  "char-alphabetic?")
  (assert equal?
	  (char-numeric? #\1)
	  #t
	  "char-numeric?")
  (assert equal?
	  (char-whitespace? #\t)
	  #f
	  "char-whitespace?")
  (assert equal?
	  (char-whitespace? #\tab)
	  #t
	  "char-whitespace?")
  (assert equal?
	  (char-whitespace? #\newline)
	  #t
	  "char-whitespace?")
  (assert equal?
	  (char-whitespace? #\a)
	  #f
	  "char-whitespace?")
  (assert equal?
	  (char=? #\a #\a)
	  #t
	  "char=?")
  (assert equal?
	  (char=? #\a #\b)
	  #f
	  "char=?")
  (assert equal?
	  (char? 2)
	  #f
	  "char?")
  (assert =
	  (cond (#f 1) (else 2))
	  2
	  "cond")
  (assert equal?
	  (cons 1 '())
	  '(1)
	  "cons")
  (assert (lambda (a b) (string? a))
	  (current-directory)
	  "."
	  "current-directory")
  (assert >=
	  (length (dir (current-environment)))
	  180
	  "current-environment")
  (assert (lambda (a b) (< (- a b) .1))
	  (current-time)
	  (current-time)
	  "current-time")
  (assert equal?
	  (letrec ((loop (lambda (n) (if (= n 0) (set! var (cut 23)) (loop (- n 1)))))
		   (var 0))
	    (loop 10)
	    var)
	  '(23)
	  "cut")
  (assert (lambda (a b) #t)
	  (dict '((1 2) (3 4)))
	  'none
	  "dict")
  (assert >=
	  (length (dir))
	  180
	  "dir")
  (assert eq?
	  (eq? 'a 'a)
	  #t
	  "eq?")
  (assert eq?
	  (equal? 1 1.0)
	  #t
	  "equal?")
  (assert eq?
	  (eqv? 1 1)
	  #t
	  "eqv?")
  (assert equal?
	  (try (error 'a "message") (catch e e (cadr e)))
	  "Error in 'a': message"
	  "error")
  (assert =
	  (eval '(+ 1 2))
	  3
	  "eval")
  (assert =
	  (eval-ast (parse '(+ 3 4)))
	  7
	  "eval-ast")
  (assert eq?
	  (even? 33)
	  #f
	  "even?")
  (assert =
	  (float 23)
	  23.0
	  "float")
  (assert equal?
	  (for-each (lambda (n) (+ n 1)) '(1 2 3))
	  (void)
	  "for-each")
  (assert equal?
	  (format "~a ~s ~%" "hello" "hello")
	  "hello \"hello\" \n"
	  "format")
  (assert eq?
	  (boolean? (use-lexical-address))
	  #t
	  "use-lexical-address")
  (assert equal?
	  (import "math")
	  (if (string=? (host-environment) "python")
	      '(math)
	      #f)
	  "import")
  (assert =
	  (int 12.8)
	  12
	  "int")
  (assert equal?
	  (integer->char 97)
	  #\a
	  "integer->char")
  (assert equal?
	  (let ((nums
		 (lambda (nums)
		   (define odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
		   (define even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
		   (list (map odd nums) (map even nums)))))
	    (nums '(1 2 3 4 5)))
	  '((#t #f #t #f #t) (#f #t #f #t #f))
	  "internal-defines")
  (assert eq?
	  (iter? 3)
	  #f
	  "iter?")
  (assert =
	  (length '(1 2 3))
	  3
	  "length")
  (assert =
	  (let ((x 1)) x)
	  1
	  "let")
  (assert equal?
	  (let ((v (vector 1 2 3))) (vector-set! v 2 'a) v)
	  (vector 1 2 'a)
	  "let")
  (assert =
	  (let* ((x 1)(y (+ x 1))) y)
	  2
	  "let*")
  (assert eq?
	  (letrec ((loop (lambda (n) (if (= n 0) 'ok (loop (- n 1)))))) (loop 10))
	  'ok
	  "letrec")
  (assert equal?
	  (list 1 2)
	  '(1 2)
	  "list")
  (assert equal?
	  (list->string '(#\1 #\2 #\3))
	  "123"
	  "list->string")
  (assert equal?
	  (list->vector '(1 2 3))
	  (vector 1 2 3)
	  "list->vector")
  (assert =
	  (list-ref '(1 2 3) 1)
	  2
	  "list-ref")
  (assert eq?
	  (list? '(1 2 3))
	  #t
	  "list?")
  (assert equal?
	  (sort < (make-set '(1 2 3 1 2)))
	  '(1 2 3)
	  "make-set")
  (assert equal?
	  (make-vector 3)
	  (vector 0 0 0)
	  "make-vector")
  (assert equal?
	  (map (lambda (n) (+ n 1)) (range 5))
	  '(1 2 3 4 5)
	  "map")
  (assert equal?
	  (member "b" '("a" "b" "c"))
	  '("b" "c")
	  "member")
  (assert equal?
	  (memq 'b '(a b c))
	  '(b c)
	  "memq")
  (assert equal?
	  (memv 2 '(1.0 2.0 3.0))
	  #f
	  "memv")
  (assert equal?
	  (memv 2.0 '(1.0 2.0 3.0))
	  '(2.0 3.0)
	  "memv")
  (assert eq?
	  (not #f)
	  #t
	  "not")
  (assert eq?
	  (null? '())
	  #t
	  "null?")
  (assert equal?
	  (number->string 23)
	  "23"
	  "number->string")
  (assert equal?
	  (number? 23)
	  #t
	  "number?")
  (assert equal?
	  (odd? 45)
	  #t
	  "odd?")
  (assert equal?
	  (or #t (/ 1 0))
	  #t
	  "or")
  (assert equal?
	  (pair? '())
	  #f
	  "pair?")
  (assert equal?
	  (pair? (cons 1 2))
	  #t
	  "pair?")

  (assert equal?
	  (floors2)
	  '((baker: 3) (cooper: 2) (fletcher: 4) (miller: 5) (smith: 1))
	  "choose")

  (assert equal?
	  (parse '(let ((- +)(+ -)) (+ 1 2)))
	  (if (use-lexical-address)
	      '(app-aexp (lambda-aexp (- +) ((app-aexp (lexical-address-aexp 0 1 + none) ((lit-aexp 1 none) (lit-aexp 2 none)) none)) none) ((lexical-address-aexp 0 2 + none) (lexical-address-aexp 0 3 - none)) none)
	      '(app-aexp (lambda-aexp (- +) ((app-aexp (var-aexp + none) ((lit-aexp 1 none) (lit-aexp 2 none)) none)) none) ((var-aexp + none) (var-aexp - none)) none))
	  "parse")

  (assert equal?
	  (parse-string "(let ((- +)) (- 7 8))")
	  (if (use-lexical-address)
	      '(app-aexp (lambda-aexp (-) ((app-aexp (lexical-address-aexp 0 0 - (stdin 1 15 15 1 15 15)) ((lit-aexp 7 (stdin 1 17 17 1 17 17)) (lit-aexp 8 (stdin 1 19 19 1 19 19))) (stdin 1 14 14 1 20 20))) none) ((lexical-address-aexp 0 2 + (stdin 1 10 10 1 10 10))) (stdin 1 1 1 1 21 21 let))
	      '(app-aexp (lambda-aexp (-) ((app-aexp (var-aexp - (stdin 1 15 15 1 15 15)) ((lit-aexp 7 (stdin 1 17 17 1 17 17)) (lit-aexp 8 (stdin 1 19 19 1 19 19))) (stdin 1 14 14 1 20 20))) none) ((var-aexp + (stdin 1 10 10 1 10 10))) (stdin 1 1 1 1 21 21 let)))
	  "parse-string")

  (assert eq?
	  (procedure? procedure?)
	  #t
	  "procedure?")
  (assert =
	  (quotient 1 4)
	  0
	  "quotient")
  (assert =
	  (rac '(1 2 3))
	  3
	  "rac")
  (assert equal?
	  (range 10)
	  '(0 1 2 3 4 5 6 7 8 9)
	  "range")
  (assert =
	  (rational 3 4)
	  3/4
	  "rational")
  (assert equal?
	  (rdc '(1 2 3))
	  '(1 2)
	  "rdc")
  (assert =
	  (remainder 1 4)
	  1
	  "remainder")
  (assert eq?
	  (require #t)
	  'ok
	  "require") ;; requires an expression to be true
  (assert equal?
	  (reverse '(1 2 3))
	  '(3 2 1)
	  "reverse")
  (assert =
	  (round 45.5)
	  46
	  "round-1")
  (assert =
	  (round 45.4)
	  45
	  "round-2")
  (assert equal?
	  (let ((x '(1 2 3))) (set-car! x 0) x)
	  '(0 2 3)
	  "set-car!")
  (assert equal?
	  (let ((x '(1 2 3))) (set-cdr! x '(3 4)) x)
	  '(1 3 4)
	  "set-cdr!")
  (assert equal?
	  (snoc 0 '(1 2 3))
	  '(1 2 3 0)
	  "snoc")
  (assert equal?
	  (sort < '(3 7 1 2))
	  '(1 2 3 7)
	  "sort")
  (assert equal?
	  (sqrt 3)
	  1.7320508075688772
	  "sqrt")
  (assert equal?
	  (string #\1 #\2)
	  "12"
	  "string")
  (assert equal?
	  (string->list "hello world")
	  '(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d)
	  "string->list")
  (assert equal?
	  (string->number "12.1")
	  12.1
	  "string->number")
  (assert eq?
	  (string->symbol "hello")
	  'hello
	  "string->symbol")
  (assert equal?
	  (string-append "hell" "o")
	  "hello"
	  "string-append")
  (assert =
	  (string-length "what")
	  4
	  "string-length")
  (assert equal?
	  (string-ref "what" 2)
	  #\a
	  "string-ref")
  (assert equal?
	  (string-split "hello.world" #\.)
	  '("hello" "world")
	  "string-split")
  (assert eq?
	  (string<? "a" "b")
	  #t
	  "string<?")
  (assert eq?
	  (string=? "a" "b")
	  #f
	  "string=?")
  (assert eq?
	  (string? "hello")
	  #t
	  "string?")
  (assert equal?
	  (substring "hello" 1 3)
	  "el"
	  "substring")
  (assert equal?
	  (symbol->string 'hello)
	  "hello"
	  "symbol->string")
  (assert eq?
	  (symbol? 'hello)
	  #t
	  "symbol?")
  (assert eq?
	  (typeof 23)
	  (typeof 24)
	  "typeof")
  (assert eq?
	  (typeof "hello")
	  (typeof "goodbye")
	  "typeof")
  (assert (lambda (x y) (not (eq? x y)))
	  (typeof '(2 3 4))
	  (typeof "(2 3 4)")
	  "typeof")
  (assert equal?
	  (unparse (parse '(+ 1 2)))
	  '(+ 1 2)
	  "unparse")
  (assert eq?
	  (use-stack-trace)
	  #t
	  "use-stack-trace")
  (assert eq?
	  (use-tracing)
	  #f
	  "use-tracing")
  (assert equal?
	  (vector 1 2 3)
	  (vector 1 2 3)
	  "vector")
  (assert equal?
	  (vector->list (vector 1 2 3))
	  '(1 2 3)
	  "vector->lsit")
  (assert =
	  (vector-ref (vector 1 2 3) 2)
	  3
	  "vector-ref")
  (assert eq?
	  (vector? (vector))
	  #t
	  "vector?")
  (assert equal?
	  (void)
	  (void)
	  "(void)")
  (assert equal?
	  (zero? 0.0)
	  #t
	  "zero?")

  (assert eq?
	  (my-odd? 42)
	  #f
	  "my-odd")
  (assert eq?
	  (my-even? 42)
	  #t
	  "my-even")
  (assert eq?
	  (my-odd? 43)
	  #t
	  "my-odd")
  (assert eq?
	  (my-even? 43)
	  #f
	  "my-even")

  (assert equal?
	  (collect (* n n) for n in (range 10))
	  '(0 1 4 9 16 25 36 49 64 81)
	  "case 1")

  (assert equal?
	  (collect (* n n) for n in (range 5 20 3))
	  '(25 64 121 196 289)
	  "case 2")

  (assert equal?
	  '(36 49 64 81)
	  (collect (* n n) for n in (range 10) if (> n 5))
	  "case 3")

  (assert equal?
	  (begin (define hello 0)
		 (for 5 times do (set! hello (+ hello 1)))
		 hello)


	  5
	  "case 4")

  (assert equal?
	  'done
	  (for sym in '(a b c d) do (define x 1) (set! x sym) x)
	  "case 5")
  (assert equal?
	  'done
	  (for n in (range 10 20 2) do n)
	  "case 6")
  (assert equal?
	  'done
	  (for n at (i j) in matrix2d do (list n 'coords: i j))
	  "case 7")
  (assert equal?
	  'done
	  (for n at (i j k) in matrix3d do (list n 'coords: i j k))
	  "case 8")
  (assert equal?
	  120
	  (! 5)
	  "case 9")
  (assert equal?
	  3628800
	  (nth 10 facts)
	  "case 10")
  (assert equal?
	  10946
	  (nth 20 fibs)
	  "case 11")
  (assert equal?
	  '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181
	      6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040)
	  (first 30 fibs)
	  "case 12")
  )

(define-tests mu-lambda
  (assert equal?
	  '(1 2 3 4 5)
	  ((lambda x x) 1 2 3 4 5)
	  "case 13")
  (assert equal?
	  '(1 (2 3 4 5))
	  ((lambda (x . y) (list x y)) 1 2 3 4 5)
	  "case 13-1")
  (assert equal?
	  '(1 2 (3 4 5))
	  ((lambda (a b . z) (list a b z)) 1 2 3 4 5)
	  "case 14")
  (assert equal?
	  '(1 2 (3))
	  ((lambda (a b . z) (list a b z)) 1 2 3)
	  "case 15")
  (assert equal?
	  '(1 2 ())
	  ((lambda (a b . z) (list a b z)) 1 2)
	  "case 16")
  (assert equal?
	  "not enough arguments given"
	  (try ((lambda (a b . z) (list a b z)) 1)
	       (catch e e "not enough arguments given"))
	  "case 17")
  )

(let ()
  (define f1 (lambda (a b c) (list a b c)))
  (define (f2) (list 42))
  (define (f3 . x) (list x))
  (define (f4 a b c . x) (list a b c x))
  (define (f5 a b c x) (list a b c x))
  (define-tests define
    (assert equal?
	    '((1 2 3) (42) ((1 2 3)) (1 2 3 (4 5)) (1 2 3 4))
	    (list (f1 1 2 3) (f2) (f3 1 2 3) (f4 1 2 3 4 5) (f5 1 2 3 4))
	    "case 18")))

(define-tests call/cc
  (assert equal?
	  40
	  (* 10 (call/cc (lambda (k) 4)))
	  "case 19")
  (assert equal?
	  40
	  (* 10 (call/cc (lambda (k) (+ 1 (k 4)))))
	  "case 20")
  (assert equal?
	  50
	  (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (j (k 5)))))))))
	  "case 21")
  (assert equal?
	  60
	  (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (k (j 5)))))))))
	  "case 22")
  )

(define-tests try
  (assert equal?
	  3
	  (try 3)
	  "case 23")
  (assert equal?
	  3
	  (try 3 (finally 'yes 4))
	  "case 24")
  (assert equal?
	  'yes
	  (try (raise 'yes) (catch e e))
	  "case 25")
  (assert equal?
	  'yes
	  (try (try (raise 'yes)) (catch e e))
	  "case 26")
  (assert equal?
	  'oops
	  (try (try (begin 'one (raise 'oops) 'two)) (catch e e))
	  "case 27")
  (assert equal?
	  40
	  (* 10 (try (begin 'one (raise 'oops) 'two)
		     (catch ex 3 4)))
	  "case 28")
  (assert equal?
	  50
	  (* 10 (try (begin 'one 'two 5)
		     (catch ex 3 4)))
	  "case 29")
  (assert equal?
	  40
	  (* 10 (try (begin 'one (raise 'oops) 5)
		     (catch ex (list 'ex: ex) 4)))
	  "case 30")
  (assert equal?
	  'oops
	  (try (* 10 (try (begin 'one (raise 'oops) 5)
			  (catch ex (list 'ex: ex) (raise ex) 4))) (catch e e))
	  "case 31")
  (assert equal?
	  'oops
	  (try (* 10 (try (begin 'one (raise 'oops) 5)
			  (catch ex (list 'ex: ex) (raise ex) 4)
			  (finally 'two 7))) (catch e e))
	  "case 32")
  (assert equal?
	  77
	  (try (* 10 (try (begin 'one (raise 'oops) 5)
			  (catch ex (list 'ex: ex) (raise 'bar) 4)))
	       (catch x 'hello 77))
	  "case 33")
  (assert equal?
	  3
	  (try 3 (finally 'hi 4))
	  "case 34")
  (assert equal?
	  5
	  (div 10 2)
	  "case 35")
  (assert equal?
	  "division by zero"
	  (try (div 10 0) (catch e (cadr e)))
	  "case 36")
  (assert equal?
	  "division by zero"
	  (try (let ((x (try (div 10 0)))) x) (catch e (cadr e)))
	  "case 37")
  (assert equal?
	  5
	  (let ((x (try (div 10 2) (catch e -1)))) x)
	  "case 38")
  (assert equal?
	  -1
	  (let ((x (try (div 10 0) (catch e -1)))) x)
	  "case 39")
  (assert equal?
	  5
	  (let ((x (try (div 10 2) (catch e -1) (finally 'closing-files 42))))  x)
	  "case 40")
  (assert equal?
	  -1
	  (let ((x (try (div 10 0) (catch e -1) (finally 'closing-files 42))))  x)
	  "case 41")
  (assert equal?
	  5
	  (let ((x (try (div 10 2) (finally 'closing-files 42))))  x)
	  "case 42")
  (assert equal?
	  'foo
	  (try (let ((x (try (div 10 0) (catch e -1 (raise 'foo)) (finally 'closing-files 42))))  x) (catch e e))
	  "case 43")
  (assert equal?
	  'ack
	  (try (let ((x (try (div 10 0)
			     (catch e -1 (raise 'foo))
			     (finally 'closing-files (raise 'ack) 42))))
		 x) (catch e e))
	  "case 44")
  (assert equal?
	  99
	  (try (let ((x (try (div 10 0)
			     (catch e -1 (raise 'foo))
			     (finally 'closing-files (raise 'ack) 42))))
		 x)
	       (catch e (if (equal? e 'ack) 99 (raise 'doug)))
	       (finally 'closing-outer-files))
	  "case 45")
  (assert equal?
	  'doug
	  (try (try (let ((x (try (div 10 0)
				  (catch e -1 (raise 'foo))
				  (finally 'closing-files (raise 'ack) 42))))
		      x)
		    (catch e (if (equal? e 'foo) 99 (raise 'doug)))
		    (finally 'closing-outer-files)) (catch e e))
	  "case 46")
  )

(define-tests loop
  (assert equal?
	  'blastoff!
	  (try (let loop ((n 5))
		 n
		 (if (= n 0)
		     (raise 'blastoff!))
		 (loop (- n 1)))
	       (catch e e))
	  "case 47")
  )

(define-tests macros
  (assert equal?
	  #t
	  (let ((bool 5))
	    (or (= bool 4) (= bool 5)))
	  "case 48")
  (assert equal?
	  6
	  (let ((bool 5))
	    (or (= bool 4) 6))
	  "case 49")
  (assert equal?
	  #f
	  (let ((bool 5))
	    (and (= bool 5) (> bool 0) (= bool 4)))
	  "case 50")
  (assert equal?
	  5
	  (let ((r 5))
	    (case 'banana
	      (apple 'no)
	      ((cherry banana) 1 2 r)
	      (else 'no)))
	  "case 51")
  (assert equal?
	  '((6) orange 5)
	  (let ((r 5))
	    (record-case (cons 'banana (cons 'orange (cons (* 2 3) '())))
			 (apple (a b c) (list c b a r))
			 ((cherry banana) (a . b) (list b a r))
			 ((orange) () 'no)
			 (else 2 3 4)))
	  "case 52")
  )

(define-tests datatype
  (assert (lambda (a b) (procedure? a))
	  lc-exp?
	  #t
	  "define-datatype-1")
  (assert (lambda (a b) (procedure? a))
	  var-exp
	  #t
	  "define-datatype-2")
  (assert (lambda (a b) (procedure? a))
	  lambda-exp
	  #t
	  "define-datatype-3")
  (assert (lambda (a b) (procedure? a))
	  app-exp
	  #t
	  "define-datatype-4")
  (assert (lambda (a b) (lc-exp? a))
	  (var-exp 'a)
	  #t
	  "define-datatype-5")
  (assert (lambda (a b) (lc-exp? a))
	  (lambda-exp 'a (var-exp 'a))
	  #t
	  "define-datatype-6")
  (assert (lambda (a b) (lc-exp? a))
	  (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a))
	  #t
	  "define-datatype-7")
  (assert eq?
	  (un-parse (var-exp 'a))
	  'a
	  "define-datatype-8")
  (assert equal?
	  (un-parse (lambda-exp 'a (var-exp 'a)))
	  '(a (var-exp a))
	  "define-datatype-8")
  (assert equal?
	  (un-parse (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a)))
	  '((lambda-exp a (var-exp a)) (var-exp a))
	  "define-datatype-8")
  )

;; ;; ---------------------------------------------------------------
;; ;; named parameters and defaults

(define-tests named-parameters
  ;; (verify2 'default-1 1 ((lambda ((n : 1)) n)))
  ;; (verify2 'default-2 2 ((lambda ((n : 2)) n)))
  ;; (verify2 'default-3 3 ((lambda ((n : 1)) n) 3))
  (assert equal?
	  '(1 2 3)
	  ((lambda (a b c) (list a b c)) 1 2 3)
	  "named-1")
  ;; (verify2 'named-2 '(1 2 3) ((lambda (a b c) (list a b c)) 1 2 (c : 3)))
  ;; (verify2 'named-3 '(1 2 3) ((lambda (a b c) (list a b c)) 1 (b : 2) (c : 3)))
  ;; (verify2 'named-4 '(1 2 3) ((lambda (a b c) (list a b c)) (a : 1) (b : 2) (c : 3)))
  ;; (verify2 'named-5 '(1 2 3) ((lambda (a b c) (list a b c)) 1 (c : 3) (b : 2)))
  ;; (verify2 'default-named-1 3 ((lambda ((n : 1)) n) (n : 3)))
  )


;; ---------------------------------------------------------------
;; choose

(define distinct?
  (lambda (nums)
    (or (null? nums)
        (null? (cdr nums))
	(and (not (member (car nums) (cdr nums)))
	     (distinct? (cdr nums))))))

(define floors2
  (lambda ()
    (let ((baker (choose 1 2 3 4 5)))
      (require (not (= baker 5)))
      (let ((fletcher (choose 1 2 3 4 5)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(let ((cooper (choose 1 2 3 4 5)))
	  (require (not (= cooper 1)))
	  (require (not (= (abs (- fletcher cooper)) 1)))
	  (let ((smith (choose 1 2 3 4 5)))
	    (require (not (= (abs (- smith fletcher)) 1)))
	    (let ((miller (choose 1 2 3 4 5)))
	      (require (> miller cooper))
	      (require (distinct? (list baker cooper fletcher miller smith)))
	      (list
	       (list 'baker: baker)
	       (list 'cooper: cooper)
	       (list 'fletcher: fletcher)
	       (list 'miller: miller)
	       (list 'smith: smith)))))))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))


(run-tests)
