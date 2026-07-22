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

;;(define-syntax time
;;  [(time ?exp) (let ((start (current-time)))
;;		 ?exp
;;		 (- (current-time) start))])

(define-syntax time
  [(time ?exp) (let* ((run (lambda () ?exp))
		      (start (current-time))
		      (result (run))
		      (end (current-time)))
		 (display (list 'took (- end start) 'seconds))
		 (newline)
		 result)])

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

(define fact (lambda (n) (nth n facts)))

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

(define-tests defines
  (assert equal?
	  (begin
	    (define x 42)
	    x)
	  42
	  "define")
  (assert equal?
	  (begin
	    (define x "my help" 42)
	    (help x))
	  "my help"
	  "define with docs")
  (assert equal?
	  (begin
	    (define (x y) y)
	    (x 38))
	  38
	  "define mit-style")
  (assert equal?
	  (try
	   (parse '(define 1 1))
	   (catch e 88))
	  88
	  "bad define 1")
  )

(define-tests range
  (assert equal?
	  (range 10)
	  '(0 1 2 3 4 5 6 7 8 9)
	  "range 1")
  (assert equal?
	  (range 1 10)
	  '(1 2 3 4 5 6 7 8 9)
	  "range 2")
  (assert equal?
	  (range 9 -1 -1)
	  '(9 8 7 6 5 4 3 2 1 0)
	  "range 1")
  )

(define-tests sort
  (assert equal?
	  (sort < (range 100))
	  (range 100)
	  "sort 1")
  (assert equal?
	  (sort < (range 99 -1 -1))
	  (range 100)
	  "sort 2")
  (assert equal?
	  (sort (lambda (a b) (> a b)) (range 100))
	  (range 99 -1 -1)
	  "sort 3")
  (assert equal?
	  (sort string<? '("z" "c" "a" "b"))
	  '("a" "b" "c" "z")
	  "sort 4")
  (assert equal?
	  (list-ref (sort < (range 9999 -1 -1)) 5)
	  5
	  "sort 5")
  (assert equal?
	  (if (equal? (host-environment) "python")
	      (try (sort (lambda (a b) (< a b)) '(1 2 "a" 3 4))
		   (catch e 'ok))
	      'ok) ;; scheme host doesn't protect native functions from crashing
	  'ok
	  "sort 6")
  (assert equal?
	  (try (sort (lambda (a b) (< a b)) '(1 2 8 3 4))
	       (catch e 'ok))
	  '(1 2 3 4 8)
	  "sort 7")
)

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
	  (dict '((a : 1) (b : 2)))
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
	  (try (error 'a "message") (catch e (get-exception-message e)))
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
	      #f) ;; scheme host doesn't have python libs
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
	  (car (parse '(let ((- +)(+ -)) (+ 1 2))))
	  'app-aexp
	  "parse")

  (assert equal?
	  (length (parse-string "(let ((- +)) (- 7 8))"))
	  4
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
  ;; Default changed to #f: *use-stack-trace* wraps every non-JIT/
  ;; non-Phase-2 call's continuation in an extra pop-frame -- ~10-13%
  ;; wall-clock and ~25-45% memory reduction on the trampoline path with
  ;; it off (see interpreter-cps.ss and README-PERFORMANCE.md).
  (assert eq?
	  (use-stack-trace)
	  #f
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
	  (fact 5)
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
	       (catch e "not enough arguments given"))
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
	  (try 3 (catch e e))
	  "case 23")
  (assert equal?
	  3
	  (try 3 (finally 'yes 4))
	  "case 24")
  (assert equal?
	  "yes"
	  (try (raise "yes") (catch e (get-exception-message e)))
	  "case 25")
  (assert equal?
	  "oops"
	  (try (begin 'one (raise "oops") 'two) (catch e (get-exception-message e)))
	  "case 27")
  (assert equal?
	  40
	  (* 10 (try (begin 'one (raise "oops") 'two)
		     (catch ex 3 4)))
	  "case 28")
  (assert equal?
	  50
	  (* 10 (try (begin 'one 'two 5)
		     (catch ex 3 4)))
	  "case 29")
  (assert equal?
	  40
	  (* 10 (try (begin 'one (raise "oops") 5)
		     (catch ex (list 'ex: ex) 4)))
	  "case 30")
  (assert equal?
	  "oops"
	  (try (* 10 (try (begin 'one (raise "oops") 5)
			  (catch ex (list 'ex: ex) (raise ex) 4)))
	       (catch e (get-exception-message e)))
	  "case 31")
  (assert equal?
	  "oops"
	  (try (* 10 (try (begin 'one (raise "oops") 5)
			  (catch ex (list 'ex: ex) (raise ex) 4)
			  (finally 'two 7)))
	       (catch e (get-exception-message e)))
	  "case 32")
  (assert equal?
	  77
	  (try (* 10 (try (begin 'one (raise "oops") 5)
			  (catch ex (list 'ex: ex) (raise "bar") 4)))
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
	  (try (div 10 0) (catch e (get-exception-message e)))
	  "case 36")
  (assert equal?
	  "division by zero"
	  (try (let ((x (try (div 10 0)
			     (catch e (get-exception-message e)))))
		 x)
	       (catch e (get-exception-message e)))
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
	  "foo"
	  (try (let ((x (try (div 10 0) (catch e -1 (raise "foo")) (finally 'closing-files 42))))  x) (catch e (get-exception-message e)))
	  "case 43")
  (assert equal?
	  "ack"
	  (try (let ((x (try (div 10 0)
			     (catch e -1 (raise "foo"))
			     (finally 'closing-files (raise "ack") 42))))
		 x) (catch e (get-exception-message e)))
	  "case 44")
  (assert equal?
	  99
	  (try (let ((x (try (div 10 0)
			     (catch e -1 (raise "foo"))
			     (finally 'closing-files (raise "ack") 42))))
		 x)
	       (catch e (if (equal? (get-exception-message e) "ack") 99 (raise "doug")))
	       (finally 'closing-outer-files))
	  "case 45")
  (assert equal?
	  "doug"
	  (try (try (let ((x (try (div 10 0)
				  (catch e -1 (raise "foo"))
				  (finally 'closing-files (raise "ack") 42))))
		      x)
		    (catch e (if (equal? e 'foo) 99 (raise "doug")))
		    (finally 'closing-outer-files)) (catch e (get-exception-message e)))
	  "case 46")
  )

(define-tests loop
  (assert equal?
	  "blastoff!"
	  (try (let loop ((n 5))
		 n
		 (if (= n 0)
		     (raise "blastoff!"))
		 (loop (- n 1)))
	       (catch e (get-exception-message e)))
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

(define-tests jit-edge-cases
  ;; Phase 4: tail-call flattening -- guards against RecursionError past
  ;; ~4,900-5,000 iterations (fixed by flattening self-recursive tail calls
  ;; in the JIT and Phase 2 direct-eval interpreter)
  (define (jit-count-loop n acc) (if (= n 0) acc (jit-count-loop (- n 1) (+ acc 1))))
  (assert equal?
	  200000
	  (jit-count-loop 200000 0)
	  "case 53")

  ;; Phase 4: mutual tail recursion (each side's JIT compilation depends on
  ;; the other, so both safely fall back to the flattened Phase 2 path)
  (define (jit-even? n) (if (= n 0) #t (jit-odd? (- n 1))))
  (define (jit-odd? n) (if (= n 0) #f (jit-even? (- n 1))))
  (assert equal?
	  #t
	  (jit-even? 50000)
	  "case 54")

  ;; Phase 5: closures over a function's own parameter, called repeatedly
  ;; so make-adder gets a chance to JIT-compile -- and each closure keeps
  ;; independent captured state
  (define (jit-make-adder k) (lambda (x) (+ x k)))
  (define (jit-adder-driver n acc)
    (if (= n 0) acc (jit-adder-driver (- n 1) (+ acc ((jit-make-adder n) 0)))))
  (assert equal?
	  5050
	  (jit-adder-driver 100 0)
	  "case 55")
  (define jit-add5 (jit-make-adder 5))
  (define jit-add10 (jit-make-adder 10))
  (assert equal?
	  (list 15 20 6)
	  (list (jit-add5 10) (jit-add10 10) (jit-add5 1))
	  "case 56")

  ;; Phase 5: nested closures -- the innermost lambda captures a variable
  ;; two lexical levels out, not just its immediately enclosing frame
  (define (jit-make-adder2 a) (lambda (b) (lambda (c) (+ a b c))))
  (define (jit-adder2-driver n acc)
    (if (= n 0) acc (jit-adder2-driver (- n 1) (+ acc (((jit-make-adder2 n) 2) 3)))))
  (assert equal?
	  5550
	  (jit-adder2-driver 100 0)
	  "case 57")

  ;; Phase 5: immediately-invoked lambda under JIT, e.g. ((lambda (x) ...) e)
  ;; -- regression test for the "'tuple' object is not callable" bug caused
  ;; by let/or/and desugaring to this shape
  (define (jit-iife k) ((lambda (x) (+ x k)) 100))
  (define (jit-run-iife n acc) (if (= n 0) acc (jit-run-iife (- n 1) (+ acc (jit-iife n)))))
  (assert equal?
	  15050
	  (jit-run-iife 100 0)
	  "case 58")

  ;; Phase 6: a parameter used in operator position, called with a closure
  (define (jit-apply-twice f x) (f (f x)))
  (define (jit-apply-twice-driver n acc)
    (if (= n 0) acc (jit-apply-twice-driver (- n 1) (+ acc (jit-apply-twice (jit-make-adder 3) n)))))
  (assert equal?
	  5650
	  (jit-apply-twice-driver 100 0)
	  "case 59")

  ;; Phase 6: a parameter used in operator position, called with a
  ;; first-class primitive
  (define (jit-call-with f a b) (f a b))
  (define (jit-run-prim n acc) (if (= n 0) acc (jit-run-prim (- n 1) (+ acc (if (jit-call-with < n 999999) 1 0)))))
  (assert equal?
	  100
	  (jit-run-prim 100 0)
	  "case 60")

  ;; Phase 6: a parameter used in operator position, called with a captured
  ;; continuation from call/cc
  (define (jit-call-it f x) (f x))
  (assert equal?
	  43
	  (+ 1 (call/cc (lambda (k) (jit-call-it k 42) 999)))
	  "case 61")

  ;; Correctness regression: a Phase-2/JIT-eligible closure whose body does
  ;; real work and *then* calls something outside the fast-path allow-list
  ;; (here, a nested closure that uses set!, which is not JIT/Phase-2
  ;; eligible) must not silently re-execute that work. apply_proc's
  ;; fallback catches _TrampolineFallback around the *whole* closure body
  ;; and re-runs it entirely via the slow trampoline on any mid-body
  ;; failure -- including work that already ran and had real (observable)
  ;; side effects. jit-dbl-fib-1 counts its own calls via a vector (not
  ;; set!, so it stays Phase-2/JIT-eligible itself); naive recursion to
  ;; n=10 makes exactly 177 calls. Before this was fixed, the trailing
  ;; call to jit-dbl-trigger (unrelated to the count, added only to
  ;; provoke the fallback) caused the entire body -- including the fib
  ;; computation that had already finished -- to run a second time via the
  ;; slow trampoline, doubling the counter to 354.
  (define jit-dbl-counter-1 (vector 0))
  (define (jit-dbl-trigger) (let ((x 0)) (set! x (+ x 1)) x))
  (define (jit-dbl-fib-1 n)
    (vector-set! jit-dbl-counter-1 0 (+ (vector-ref jit-dbl-counter-1 0) 1))
    (if (< n 2) 1 (+ (jit-dbl-fib-1 (- n 1)) (jit-dbl-fib-1 (- n 2)))))
  (let ((jit-dbl-start 0))
    (jit-dbl-fib-1 10)
    (jit-dbl-trigger))
  (assert equal?
	  177
	  (vector-ref jit-dbl-counter-1 0)
	  "case 62")

  ;; Correctness regression, one level of nesting deeper: an OUTER closure
  ;; does its own work, then calls a HELPER closure whose own body does
  ;; work and then triggers the same fallback. Before the fix this could
  ;; even compound rather than just double (observed 2x on the outer
  ;; closure's own counter and 3x on the inner one in manual testing),
  ;; since a mid-body failure inside a non-tail nested call unwinds past
  ;; *multiple* logical closures to the one shared fallback point at the
  ;; outermost apply_proc -- not just the closure whose body actually
  ;; failed.
  (define jit-dbl-counter-o (vector 0))
  (define jit-dbl-counter-h (vector 0))
  (define (jit-dbl-h n)
    (vector-set! jit-dbl-counter-h 0 (+ (vector-ref jit-dbl-counter-h 0) 1))
    (jit-dbl-trigger)
    n)
  (define (jit-dbl-o n)
    (vector-set! jit-dbl-counter-o 0 (+ (vector-ref jit-dbl-counter-o 0) 1))
    (+ n (jit-dbl-h n)))
  (jit-dbl-o 5)
  (assert equal?
	  (list 1 1)
	  (list (vector-ref jit-dbl-counter-o 0) (vector-ref jit-dbl-counter-h 0))
	  "case 63")

  ;; Correctness regression, a specific gap found (and fixed) during
  ;; development of _is_phase2_safe itself: a closure that does work and
  ;; then *returns* a dotted-formals lambda (lambda (a . rest) ...) as a
  ;; plain value -- NOT calling it, just creating it. _eval_direct has no
  ;; case for mu-lambda-aexp at all (only plain lambda-aexp), so
  ;; evaluating this always falls to its `else: raise
  ;; _TrampolineFallback()`. The existing mu-lambda test group above only
  ;; exercises dotted-formals lambdas in IMMEDIATELY-CALLED (IIFE)
  ;; position, e.g. ((lambda (a . z) ...) args...) -- which is already
  ;; excluded from Phase 2 for an unrelated reason (a computed/nested
  ;; operator can't be statically proven safe), so it never exercised
  ;; this specific "returned as a value" shape. _is_phase2_safe initially
  ;; (wrongly) treated mu-lambda-aexp as safe by copying
  ;; _is_direct_eval_safe's lambda-boundary pattern without checking it
  ;; against _eval_direct's actual cases -- certifying a closure Phase 2
  ;; could not actually evaluate, which crashed instead of silently
  ;; double-executing (apply_proc no longer catches a fallback from a
  ;; certified-safe closure -- see README-PERFORMANCE.md's "Phase 8") but
  ;; still broke previously-working code. Confirms both that the counter
  ;; isn't doubled AND that the call doesn't crash.
  (define jit-dbl-counter-mu (vector 0))
  (define (jit-dbl-make-variadic n)
    (vector-set! jit-dbl-counter-mu 0 (+ (vector-ref jit-dbl-counter-mu 0) 1))
    (lambda (a . rest) (+ n a (length rest))))
  (define jit-dbl-variadic-fn (jit-dbl-make-variadic 100))
  (assert equal?
	  (list 1 103)
	  (list (vector-ref jit-dbl-counter-mu 0) (jit-dbl-variadic-fn 1 2 3))
	  "case 64")

  ;; Correctness regression: a pre-existing instance of the same bug
  ;; class, unrelated to mu-lambda, found while auditing which AST node
  ;; types _is_direct_eval_safe's set!-only check (the OLD, still-used
  ;; gate for whether a closure is JIT/Phase-2-*eligible* at all) fails
  ;; to exclude. _eval_direct has no case for try-catch-aexp either, so a
  ;; closure using (try ... (catch ...)) after doing other work was
  ;; already double-executing that work on the original, unmodified
  ;; interpreter (confirmed directly against the pre-Phase-8 commit) --
  ;; _is_phase2_safe's default-deny design (only 7 explicitly-recognized
  ;; node types are ever treated as safe; everything else, including
  ;; try/catch, raise, and choose, falls through to "unsafe") closes this
  ;; as a side effect, without having been specifically designed for it.
  (define jit-dbl-counter-try (vector 0))
  (define (jit-dbl-work-then-try n)
    (vector-set! jit-dbl-counter-try 0 (+ (vector-ref jit-dbl-counter-try 0) 1))
    (try (/ 1 0) (catch e "caught")))
  (jit-dbl-work-then-try 5)
  (assert equal?
	  1
	  (vector-ref jit-dbl-counter-try 0)
	  "case 65")

  ;; call/cc is a primitive, not a special AST node, so a call to it from
  ;; inside a JIT/Phase-2-eligible closure is just an ordinary application
  ;; of an unrecognized primitive -- _apply_direct has no entry for it in
  ;; _FAST_PRIM_SPECS and it isn't a Phase-2-safe closure either, so it
  ;; raises _TrampolineFallback and the whole call re-runs on the
  ;; always-correct trampoline, which knows how to honor call/cc's real
  ;; continuation-capture semantics. jit-cc-loop is otherwise identical in
  ;; shape to jit-count-loop (case 53): plain tail self-recursion that the
  ;; JIT flattens into a Python while-loop. escape is called from deep
  ;; inside that flattened loop (at n=7000, out of 200000 iterations) --
  ;; this checks that unwinding out of a JIT'd/flattened loop via a
  ;; captured continuation lands on the right value and doesn't corrupt
  ;; the compiled function for later calls.
  (define (jit-cc-loop n escape)
    (if (= n 0)
	'looped-to-zero
	(begin
	  (if (= n 7000) (escape 'escaped-early) #f)
	  (jit-cc-loop (- n 1) escape))))
  (assert equal?
	  'escaped-early
	  (call/cc (lambda (k) (jit-cc-loop 200000 k)))
	  "case 66")
  ;; same compiled jit-cc-loop, called again right after the escape above,
  ;; this time with a plain non-escaping continuation -- confirms the
  ;; earlier non-local exit didn't leave the JIT cache or the flattened
  ;; loop's Python frame in a bad state.
  (assert equal?
	  'looped-to-zero
	  (call/cc (lambda (k) (jit-cc-loop 200000 (lambda (x) x))))
	  "case 67")

  ;; call/cc early-exit from NON-tail, doubly-recursive JIT-compiled
  ;; search (fib-shaped, like Phase 7's motivating case, not a flattened
  ;; tail loop). jit-cc-tree-counter tracks how many calls actually ran;
  ;; this is the call/cc analogue of case 62/63's try-catch double-
  ;; execution regression -- confirms that escaping via a captured
  ;; continuation out of the middle of a JIT'd non-tail recursion, after
  ;; real side-effecting work already happened, does not replay that work
  ;; a second time when the trampoline fallback kicks in for the call/cc
  ;; call itself. Descending 10 -> 9 -> 8 -> 7 along the first recursive
  ;; branch before matching target=7 and escaping, exactly 4 calls should
  ;; have run (the second branch at each level, and everything below 7,
  ;; must never execute).
  (define jit-cc-tree-counter (vector 0))
  (define (jit-cc-tree-search n target found)
    (vector-set! jit-cc-tree-counter 0 (+ (vector-ref jit-cc-tree-counter 0) 1))
    (if (= n target)
	(found n)
	(if (= n 0)
	    'not-found
	    (begin
	      (jit-cc-tree-search (- n 1) target found)
	      (jit-cc-tree-search (- n 1) target found)))))
  (assert equal?
	  (list 7 4)
	  (list (call/cc (lambda (k) (jit-cc-tree-search 10 7 k)))
		(vector-ref jit-cc-tree-counter 0))
	  "case 68")

  ;; choose/require backtracking search repeatedly calling a pure,
  ;; Phase-2/JIT-eligible helper (jit-choose-square has no set!, so it's
  ;; fast-path-eligible on its own) across every branch the search visits
  ;; -- including branches that later fail `require` and get backed out
  ;; of. choose/require themselves are never JIT/Phase-2-eligible (choose
  ;; isn't one of _is_phase2_safe's recognized node types, so it always
  ;; runs on the trampoline), but the helper they call on each branch is
  ;; -- this checks that a JIT-compiled helper keeps returning correct,
  ;; non-duplicated-looking results when invoked over and over by
  ;; backtracking control flow it has no awareness of. The exact call
  ;; count (28) pins down the search order so a future change to choose's
  ;; backtracking order would be caught here too.
  (define jit-choose-counter (vector 0))
  (define (jit-choose-square x)
    (vector-set! jit-choose-counter 0 (+ (vector-ref jit-choose-counter 0) 1))
    (* x x))
  (define (jit-choose-pair-search)
    (let ((a (choose 1 2 3 4 5)))
      (let ((b (choose 1 2 3 4 5)))
	(require (= (+ (jit-choose-square a) (jit-choose-square b)) 25))
	(require (< a b))
	(list a b))))
  (assert equal?
	  (list '(3 4) 28)
	  (list (jit-choose-pair-search) (vector-ref jit-choose-counter 0))
	  "case 69")

  ;; choose/require backtracking search where the per-branch helper is
  ;; itself a JIT-compiled SELF-RECURSIVE function (naive fib, the JIT's
  ;; own canonical example -- see README-PERFORMANCE.md), not just a
  ;; single-call primitive-shaped helper like case 69. This exercises
  ;; repeated invocation of one compiled instance across many backtracked
  ;; (tried-then-abandoned) branches: a=1..5 all fail the first require
  ;; before a=6 succeeds, then b=1..4 fail the second require before b=5
  ;; succeeds.
  (define (jit-choose-fib n) (if (< n 2) n (+ (jit-choose-fib (- n 1)) (jit-choose-fib (- n 2)))))
  (define (jit-choose-fib-search)
    (let ((a (choose 1 2 3 4 5 6 7 8)))
      (require (> (jit-choose-fib a) 5))
      (let ((b (choose 1 2 3 4 5 6 7 8)))
	(require (= (+ (jit-choose-fib a) (jit-choose-fib b)) 13))
	(list a b))))
  (assert equal?
	  '(6 5)
	  (jit-choose-fib-search)
	  "case 70")

  ;; Multi-shot re-entrant continuations (a real generator/coroutine built
  ;; on call/cc, not just a one-shot escape like cases 66-68) driving a
  ;; JIT-eligible helper across several separate resumptions. Naively,
  ;; resuming a saved continuation from a *different* top-level form than
  ;; the one that captured it breaks in this interpreter: execute-loop-rm
  ;; (see interpreter-cps.ss) drives the whole file through ONE shared
  ;; global register/pc trampoline, calling (trampoline) once per
  ;; top-level form and looping at the Python/register-machine level, not
  ;; via Scheme continuations. A captured continuation's chain still
  ;; bottoms out at REP-k (the single shared toplevel continuation), so
  ;; invoking it from a later form's trampoline() call doesn't return a
  ;; value to that form -- it hijacks that form's still-running
  ;; trampoline() instance to run the OLD continuation's leftover work
  ;; instead, silently discarding whatever the later form was doing. This
  ;; is by design (REP-k is deliberately shared/global), not a bug -- but
  ;; it means user code that wants an isolated, "resettable" call needs a
  ;; fresh continuation boundary of its own.
  ;;
  ;; `callback` (special form, not a regular primitive -- see
  ;; callback-aexp in parser-cps.ss/interpreter-cps.ss) provides exactly
  ;; that: (callback thunk) wraps thunk in a native Python closure that,
  ;; when called, sets handler_reg/k2_reg to fresh REP_handler/REP_k and
  ;; runs a NESTED (trampoline) call to completion (see dlr_func/callback
  ;; in Scheme.py) -- the same mechanism used to hand Scheme procedures to
  ;; host callbacks (e.g. `sort`'s comparator). Because that nested
  ;; trampoline has its OWN fresh REP_k, a continuation captured inside it
  ;; bottoms out there instead of at the outer shared REP_k, so invoking
  ;; it later doesn't reach past the (callback ...) boundary -- resetting
  ;; the continuation stack for that call. Wrapping both the generator's
  ;; first call and every resume in (callback ...) makes a real multi-shot
  ;; generator work correctly from ordinary sequential code.
  ;;
  ;; jit-gen2-double is a trivial pure helper (JIT/Phase-2-eligible on its
  ;; own); jit-gen2 itself never is (uses define/set!, like all
  ;; call/cc-based generators), so it always runs on the trampoline -- but
  ;; every value it yields comes from the JIT-compiled helper.
  (define jit-gen2-saved-k #f)
  (define (jit-gen2-double x) (* x 2))
  (define (jit-gen2)
    (call/cc (lambda (return)
      (define (yield v)
	(call/cc (lambda (k)
	  (set! jit-gen2-saved-k k)
	  (return v))))
      (let loop ((i 1))
	(yield (jit-gen2-double i))
	(loop (+ i 1))))))
  (define jit-gen2-run (callback (lambda () (jit-gen2))))
  (define jit-gen2-resume (callback (lambda () (jit-gen2-saved-k 'go))))
  (assert equal?
	  '(2 4 6 8)
	  (list (jit-gen2-run) (jit-gen2-resume) (jit-gen2-resume) (jit-gen2-resume))
	  "case 71")

  ;; Same (callback ...)-reset multi-shot generator pattern, but yielding
  ;; values from a JIT-compiled SELF-RECURSIVE helper (naive fib, like
  ;; case 70) instead of a single-expression primitive-shaped one --
  ;; confirms the compiled instance keeps producing correct results
  ;; across several independent nested-trampoline re-entries, each
  ;; resuming mid-loop from a completely separate top-level form. The
  ;; final direct call to jit-gen3-fib afterward confirms driving it
  ;; through the generator/callback machinery didn't leave its JIT-cached
  ;; compiled function in a bad state.
  (define (jit-gen3-fib n) (if (< n 2) n (+ (jit-gen3-fib (- n 1)) (jit-gen3-fib (- n 2)))))
  (define jit-gen3-saved-k #f)
  (define (jit-gen3)
    (call/cc (lambda (return)
      (define (yield v)
	(call/cc (lambda (k)
	  (set! jit-gen3-saved-k k)
	  (return v))))
      (let loop ((i 0))
	(yield (jit-gen3-fib i))
	(loop (+ i 1))))))
  (define jit-gen3-run (callback (lambda () (jit-gen3))))
  (define jit-gen3-resume (callback (lambda () (jit-gen3-saved-k 'go))))
  (assert equal?
	  '(0 1 1 2 3 5)
	  (list (jit-gen3-run) (jit-gen3-resume) (jit-gen3-resume) (jit-gen3-resume)
		(jit-gen3-resume) (jit-gen3-resume))
	  "case 72")
  (assert equal?
	  610
	  (jit-gen3-fib 15)
	  "case 73")
  )

(run-tests)
