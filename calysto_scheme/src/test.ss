;; Unit tests for Scheme functions

;;(use-lexical-address #f)
;;(define! DEBUG #t)

(define start-time (current-time))

(clear-unit-tests)

(define-tests functions-1
  (assert equal?
	  `(list ,(+ 1 2) 4)
	  '(list 3 4)
	  "quasiquote")
  (assert equal?
	  (% 10 3)
	  1
	  "1 %")
  (assert equal?
	  (% 10 3)
	  1
	  "2 %")
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
	'(math)
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

)

;; **here**

;; (if (use-lexical-address)
;;     (verify 'parse (parse '(let ((- +)(+ -)) (+ 1 2))) equal? '(app-aexp (lambda-aexp (- +) ((app-aexp (lexical-address-aexp 0 1 + none) ((lit-aexp 1 none) (lit-aexp 2 none)) none)) none) ((lexical-address-aexp 0 2 + none) (lexical-address-aexp 0 3 - none)) none))
;;     (verify 'parse (parse '(let ((- +)(+ -)) (+ 1 2))) equal? '(app-aexp (lambda-aexp (- +) ((app-aexp (var-aexp + none) ((lit-aexp 1 none) (lit-aexp 2 none)) none)) none) ((var-aexp + none) (var-aexp - none)) none)))
;; (if (use-lexical-address)
;;     (verify 'parse-string (parse-string "(let ((- +)) (- 7 8))") equal? '(app-aexp (lambda-aexp (-) ((app-aexp (lexical-address-aexp 0 0 - (stdin 1 15 15 1 15 15)) ((lit-aexp 7 (stdin 1 17 17 1 17 17)) (lit-aexp 8 (stdin 1 19 19 1 19 19))) (stdin 1 14 14 1 20 20))) none) ((lexical-address-aexp 0 2 + (stdin 1 10 10 1 10 10))) (stdin 1 1 1 1 21 21 let)))
;;     (verify 'parse-string (parse-string "(let ((- +)) (- 7 8))") equal? '(app-aexp (lambda-aexp (-) ((app-aexp (var-aexp - (stdin 1 15 15 1 15 15)) ((lit-aexp 7 (stdin 1 17 17 1 17 17)) (lit-aexp 8 (stdin 1 19 19 1 19 19))) (stdin 1 14 14 1 20 20))) none) ((var-aexp + (stdin 1 10 10 1 10 10))) (stdin 1 1 1 1 21 21 let))))


;; (verify 'procedure? (procedure? procedure?) eq? #t)
;; (verify 'quotient (quotient 1 4) = 0)
;; (verify 'rac (rac '(1 2 3)) = 3)
;; (verify 'range (range 10) equal? '(0 1 2 3 4 5 6 7 8 9))
;; (verify 'rational (rational 3 4) = 3/4)
;; (verify 'rdc (rdc '(1 2 3)) equal? '(1 2))
;; (verify 'remainder (remainder 1 4) = 1)
;; (verify 'require (require #t) eq? 'ok) ;; requires an expression to be true
;; (verify 'reverse (reverse '(1 2 3)) equal? '(3 2 1))
;; (verify 'round-1 (round 45.5) = 46)
;; (verify 'round-2 (round 45.4) = 45)
;; (verify 'set-car! (let ((x '(1 2 3))) (set-car! x 0) x) equal? '(0 2 3))
;; (verify 'set-cdr! (let ((x '(1 2 3))) (set-cdr! x '(3 4)) x) equal? '(1 3 4))
;; (verify 'snoc (snoc 0 '(1 2 3)) equal? '(1 2 3 0))
;; (verify 'sort (sort < '(3 7 1 2)) equal? '(1 2 3 7))
;; (verify 'sqrt (sqrt 3) equal? 1.7320508075688772)
;; (verify 'string (string #\1 #\2) equal? "12")
;; (verify 'string->list (string->list "hello world")
;; 	equal? '(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d))
;; (verify 'string->number (string->number "12.1") equal? 12.1)
;; (verify 'string->symbol (string->symbol "hello") eq? 'hello)
;; (verify 'string-append (string-append "hell" "o") equal? "hello")
;; (verify 'string-length (string-length "what") = 4)
;; (verify 'string-ref (string-ref "what" 2) equal? #\a)
;; (verify 'string-split (string-split "hello.world" #\.) equal? '("hello" "world"))
;; (verify 'string<? (string<? "a" "b") eq? #t)
;; (verify 'string=? (string=? "a" "b") eq? #f)
;; (verify 'string? (string? "hello") eq? #t)
;; (verify 'substring (substring "hello" 1 3) equal? "el")
;; (verify 'symbol->string (symbol->string 'hello) equal? "hello")
;; (verify 'symbol? (symbol? 'hello) eq? #t)
;; (verify 'typeof (typeof 23) eq? (typeof 24))
;; (verify 'typeof (typeof "hello") eq? (typeof "goodbye"))
;; (verify 'typeof (typeof '(2 3 4)) (lambda (x y) (not (eq? x y))) (typeof "(2 3 4)"))
;; (verify 'unparse (unparse (parse '(+ 1 2))) equal? '(+ 1 2))
;; (verify 'use-stack-trace (use-stack-trace) eq? #t)
;; (verify 'use-tracing (use-tracing) eq? #f)
;; (verify 'vector (vector 1 2 3) equal? (vector 1 2 3))
;; (verify 'vector->lsit (vector->list (vector 1 2 3)) equal? '(1 2 3))
;; (verify 'vector-ref (vector-ref (vector 1 2 3) 2) = 3)
;; (verify 'vector? (vector? (vector)) eq? #t)
;; (verify '(void) (void) equal? (void))
;; (verify 'zero? (zero? 0.0) equal? #t)

;; ;;---------------------------------------------------------------------
;; ;; collect is like list comprehension in Python

;; (define-syntax collect
;;   [(collect ?exp for ?var in ?list)
;;    (filter-map (lambda (?var) ?exp) (lambda (?var) #t) ?list)]
;;   [(collect ?exp for ?var in ?list if ?condition)
;;    (filter-map (lambda (?var) ?exp) (lambda (?var) ?condition) ?list)])

;; (define filter-map
;;   (lambda (f pred? values)
;;     (if (null? values)
;;       '()
;;       (if (pred? (car values))
;; 	  (cons (f (car values)) (filter-map f pred? (cdr values)))
;; 	  (filter-map f pred? (cdr values))))))

;; (define-syntax time
;;   [(time ?exp) (let ((start (current-time)))
;; 		 ?exp
;; 		 (- (current-time) start))])

;; ;;---------------------------------------------------------------------
;; ;; for loops

;; (define-syntax for
;;   [(for ?exp times do . ?bodies)
;;    (for-repeat ?exp (lambda () . ?bodies))]
;;   [(for ?var in ?exp do . ?bodies)
;;    (for-iterate1 ?exp (lambda (?var) . ?bodies))]
;;   [(for ?var at (?i) in ?exp do . ?bodies)
;;    (for-iterate2 0 ?exp (lambda (?var ?i) . ?bodies))]
;;   [(for ?var at (?i ?j . ?rest) in ?exp do . ?bodies)
;;    (for ?var at (?i) in ?exp do
;;      (for ?var at (?j . ?rest) in ?var do . ?bodies))])

;; (define for-repeat
;;   (lambda (n f)
;;     (if (< n 1)
;;       'done
;;       (begin
;; 	(f)
;; 	(for-repeat (- n 1) f)))))

;; (define for-iterate1
;;   (lambda (values f)
;;     (if (null? values)
;;       'done
;;       (begin
;; 	(f (car values))
;; 	(for-iterate1 (cdr values) f)))))

;; (define for-iterate2
;;   (lambda (i values f)
;;     (if (null? values)
;;       'done
;;       (begin
;; 	(f (car values) i)
;; 	(for-iterate2 (+ i 1) (cdr values) f)))))

;; (define matrix2d
;;   '((10 20)
;;     (30 40)
;;     (50 60)
;;     (70 80)))

;; (define matrix3d
;;   '(((10 20 30) (40 50 60))
;;     ((70 80 90) (100 110 120))
;;     ((130 140 150) (160 170 180))
;;     ((190 200 210) (220 230 240))))

;; ;;---------------------------------------------------------------------
;; ;; streams

;; (define-syntax scons
;;   [(scons ?x ?y) (cons ?x (lambda () ?y))])

;; (define scar car)

;; (define scdr
;;   (lambda (s)
;;     (let ((result ((cdr s))))
;;       (set-cdr! s (lambda () result))
;;       result)))

;; (define first
;;   (lambda (n s)
;;     (if (= n 0)
;;       '()
;;       (cons (scar s) (first (- n 1) (scdr s))))))

;; (define nth
;;   (lambda (n s)
;;     (if (= n 0)
;;       (scar s)
;;       (nth (- n 1) (scdr s)))))

;; (define smap
;;   (lambda (f s)
;;     (scons (f (scar s)) (smap f (scdr s)))))

;; (define ones (scons 1 ones))

;; (define nats (scons 0 (combine nats + ones)))

;; (define combine
;;   (lambda (s1 op s2)
;;     (scons (op (scar s1) (scar s2)) (combine (scdr s1) op (scdr s2)))))

;; (define fibs (scons 1 (scons 1 (combine fibs + (scdr fibs)))))

;; (define facts (scons 1 (combine facts * (scdr nats))))

;; (define ! (lambda (n) (nth n facts)))


;; ;; Calico Scheme Tests

;; (define my-odd? 'undefined)
;; (define my-even? 'undefined)

;; (letrec
;;     ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
;;      (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
;;   (set! my-odd? odd)
;;   (set! my-even? even))

;; (verify 'my-odd (my-odd? 42) eq? #f)
;; (verify 'my-even (my-even? 42) eq? #t)
;; (verify 'my-odd (my-odd? 43) eq? #t)
;; (verify 'my-even (my-even? 43) eq? #f)
;; (verify2 'test-1 '(0 1 4 9 16 25 36 49 64 81) (collect (* n n) for n in (range 10)))
;; (verify2 'test-2 '(25 64 121 196 289) (collect (* n n) for n in (range 5 20 3)))
;; (verify2 'test-3 '(36 49 64 81) (collect (* n n) for n in (range 10) if (> n 5)))
;; (verify2 'test-4 5 (begin (define hello 0)
;; 		 (for 5 times do (set! hello (+ hello 1)))
;; 		 hello))
;; (verify2 'test-5 'done (for sym in '(a b c d) do (define x 1) (set! x sym) x))
;; (verify2 'test-6 'done (for n in (range 10 20 2) do n))
;; (verify2 'test-7 'done (for n at (i j) in matrix2d do (list n 'coords: i j)))
;; (verify2 'test-8 'done (for n at (i j k) in matrix3d do (list n 'coords: i j k)))
;; (verify2 'test-9 120 (! 5))
;; (verify2 'test-10 3628800 (nth 10 facts))
;; (verify2 'test-11 10946 (nth 20 fibs))
;; (verify2 'test-12 '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181
;; 	    6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040)
;;         (first 30 fibs))

;; (define test-mu-lambda
;;   (lambda ()
;;     (verify2 'test-13 '(1 2 3 4 5)
;;       ((lambda x x) 1 2 3 4 5))
;;     (verify2 'test-13-1 '(1 (2 3 4 5))
;;       ((lambda (x . y) (list x y)) 1 2 3 4 5))
;;     (verify2 'test-14 '(1 2 (3 4 5))
;;       ((lambda (a b . z) (list a b z)) 1 2 3 4 5))
;;     (verify2 'test-15 '(1 2 (3))
;;       ((lambda (a b . z) (list a b z)) 1 2 3))
;;     (verify2 'test-16 '(1 2 ())
;;       ((lambda (a b . z) (list a b z)) 1 2))
;;     (verify2 'test-17 "not enough arguments given"
;;       (try ((lambda (a b . z) (list a b z)) 1)
;; 	       (catch e e "not enough arguments given")))
;;     ))

;; (define test-define
;;   (lambda ()
;;     (define f1 (lambda (a b c) (list a b c)))
;;     (define (f2) (list 42))
;;     (define (f3 . x) (list x))
;;     (define (f4 a b c . x) (list a b c x))
;;     (define (f5 a b c x) (list a b c x))
;;     (verify2 'test-18 '((1 2 3) (42) ((1 2 3)) (1 2 3 (4 5)) (1 2 3 4))
;;       (list (f1 1 2 3) (f2) (f3 1 2 3) (f4 1 2 3 4 5) (f5 1 2 3 4)))))

;; (define test-call/cc
;;   (lambda ()
;;     (verify2 'test-19 40
;;       (* 10 (call/cc (lambda (k) 4))))
;;     (verify2 'test-20 40
;;       (* 10 (call/cc (lambda (k) (+ 1 (k 4))))))
;;     (verify2 'test-21 50
;;       (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (j (k 5))))))))))
;;     (verify2 'test-22 60
;;       (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (k (j 5))))))))))))

;; (define test-try
;;   (lambda ()
;;     (verify2 'test-23 3
;;       (try 3))
;;     (verify2 'test-24 3
;;       (try 3 (finally 'yes 4)))
;;     (verify2 'test-25 'yes
;;       (try (raise 'yes) (catch e e)))
;;     (verify2 'test-26 'yes
;;       (try (try (raise 'yes)) (catch e e)))
;;     (verify2 'test-27 'oops
;;       (try (try (begin 'one (raise 'oops) 'two)) (catch e e)))
;;     (verify2 'test-28 40
;;       (* 10 (try (begin 'one (raise 'oops) 'two)
;;             (catch ex 3 4))))
;;     (verify2 'test-29 50
;;       (* 10 (try (begin 'one 'two 5)
;;             (catch ex 3 4))))
;;     (verify2 'test-30 40
;;       (* 10 (try (begin 'one (raise 'oops) 5)
;;             (catch ex (list 'ex: ex) 4))))
;;     (verify2 'test-31 'oops
;;       (try (* 10 (try (begin 'one (raise 'oops) 5)
;;             (catch ex (list 'ex: ex) (raise ex) 4))) (catch e e)))
;;     (verify2 'test-32 'oops
;;       (try (* 10 (try (begin 'one (raise 'oops) 5)
;;               (catch ex (list 'ex: ex) (raise ex) 4)
;;               (finally 'two 7))) (catch e e)))
;;     (verify2 'test-33 77
;;       (try (* 10 (try (begin 'one (raise 'oops) 5)
;; 		      (catch ex (list 'ex: ex) (raise 'bar) 4)))
;; 	   (catch x 'hello 77)))
;;     (verify2 'test-34 3
;;       (try 3 (finally 'hi 4)))
;;     (verify2 'test-35 5
;;       (div 10 2))
;;     (verify2 'test-36 "division by zero"
;;       (try (div 10 0) (catch e (cadr e))))
;;     (verify2 'test-37 "division by zero"
;;       (try (let ((x (try (div 10 0)))) x) (catch e (cadr e))))
;;     (verify2 'test-38 5
;;       (let ((x (try (div 10 2) (catch e -1)))) x))
;;     (verify2 'test-39 -1
;;       (let ((x (try (div 10 0) (catch e -1)))) x))
;;     (verify2 'test-40 5
;;       (let ((x (try (div 10 2) (catch e -1) (finally 'closing-files 42))))  x))
;;     (verify2 'test-41 -1
;;       (let ((x (try (div 10 0) (catch e -1) (finally 'closing-files 42))))  x))
;;     (verify2 'test-42 5
;;       (let ((x (try (div 10 2) (finally 'closing-files 42))))  x))
;;     (verify2 'test-43 'foo
;;       (try (let ((x (try (div 10 0) (catch e -1 (raise 'foo)) (finally 'closing-files 42))))  x) (catch e e)))
;;     (verify2 'test-44 'ack
;;       (try (let ((x (try (div 10 0)
;;                 (catch e -1 (raise 'foo))
;;                 (finally 'closing-files (raise 'ack) 42))))
;;        x) (catch e e)))
;;     (verify2 'test-45 99
;;       (try (let ((x (try (div 10 0)
;;                      (catch e -1 (raise 'foo))
;;                      (finally 'closing-files (raise 'ack) 42))))
;;             x)
;;        (catch e (if (equal? e 'ack) 99 (raise 'doug)))
;;        (finally 'closing-outer-files)))
;;     (verify2 'test-46 'doug
;;       (try (try (let ((x (try (div 10 0)
;;                      (catch e -1 (raise 'foo))
;;                      (finally 'closing-files (raise 'ack) 42))))
;;             x)
;;        (catch e (if (equal? e 'foo) 99 (raise 'doug)))
;;        (finally 'closing-outer-files)) (catch e e)))
;;     ))

;; (define test-loop
;;   (lambda ()
;;     (verify2 'test-47 'blastoff! (try (let loop ((n 5))
;;                             n
;;                             (if (= n 0)
;;                                 (raise 'blastoff!))
;;                             (loop (- n 1)))
;;                 (catch e e)))))

;; (define (test-macros)
;;   (verify2 'test-48 #t
;;     (let ((bool 5))
;;       (or (= bool 4) (= bool 5))))
;;   (verify2 'test-49 6
;;     (let ((bool 5))
;;       (or (= bool 4) 6)))
;;   (verify2 'test-50 #f
;;     (let ((bool 5))
;;       (and (= bool 5) (> bool 0) (= bool 4))))
;;   (verify2 'test-51 5
;;     (let ((r 5))
;;       (case 'banana
;; 	(apple 'no)
;; 	((cherry banana) 1 2 r)
;; 	(else 'no))))
;;   (verify2 'test-52 '((6) orange 5)
;;     (let ((r 5))
;;       (record-case (cons 'banana (cons 'orange (cons (* 2 3) '())))
;; 	(apple (a b c) (list c b a r))
;; 	((cherry banana) (a . b) (list b a r))
;; 	((orange) () 'no)
;; 	(else 2 3 4)))))

;; (test-mu-lambda)
;; (test-define)
;; (test-call/cc)
;; (test-loop)
;; (test-macros)
;; (test-try)

;; (define-datatype lc-exp lc-exp?
;;   (var-exp
;;    (var symbol?))
;;   (lambda-exp
;;    (bound-var symbol?)
;;    (body lc-exp?))
;;   (app-exp
;;    (rator lc-exp?)
;;    (rand lc-exp?)))

;; (verify 'define-datatype-1 lc-exp? (lambda (a b) (procedure? a)) #t)
;; (verify 'define-datatype-2 var-exp (lambda (a b) (procedure? a)) #t)
;; (verify 'define-datatype-3 lambda-exp (lambda (a b) (procedure? a)) #t)
;; (verify 'define-datatype-4 app-exp (lambda (a b) (procedure? a)) #t)

;; (verify 'define-datatype-5 (var-exp 'a) (lambda (a b) (lc-exp? a)) #t)
;; (verify 'define-datatype-6 (lambda-exp 'a (var-exp 'a)) (lambda (a b) (lc-exp? a)) #t)
;; (verify 'define-datatype-7 (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a)) (lambda (a b) (lc-exp? a)) #t)

;; (define un-parse
;;   (lambda (exp)
;;     (cases lc-exp exp
;;        (var-exp (var) var)
;;        (lambda-exp (bound-var body) (list bound-var body))
;;        (app-exp (rator rand) (list rator rand)))))

;; (verify 'define-datatype-8 (un-parse (var-exp 'a)) eq? 'a)
;; (verify 'define-datatype-8 (un-parse (lambda-exp 'a (var-exp 'a))) equal? '(a (var-exp a)))
;; (verify 'define-datatype-8 (un-parse (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a))) equal? '((lambda-exp a (var-exp a)) (var-exp a)))

;; ;; ---------------------------------------------------------------
;; ;; named parameters and defaults

;; ;;(verify2 'default-1 1 ((lambda ((n : 1)) n)))
;; ;;(verify2 'default-2 2 ((lambda ((n : 2)) n)))
;; ;;(verify2 'default-3 3 ((lambda ((n : 1)) n) 3))

;; (verify2 'named-1 '(1 2 3) ((lambda (a b c) (list a b c)) 1 2 3))
;; ;;(verify2 'named-2 '(1 2 3) ((lambda (a b c) (list a b c)) 1 2 (c : 3)))
;; ;;(verify2 'named-3 '(1 2 3) ((lambda (a b c) (list a b c)) 1 (b : 2) (c : 3)))
;; ;;(verify2 'named-4 '(1 2 3) ((lambda (a b c) (list a b c)) (a : 1) (b : 2) (c : 3)))
;; ;;(verify2 'named-5 '(1 2 3) ((lambda (a b c) (list a b c)) 1 (c : 3) (b : 2)))

;; ;;(verify2 'default-named-1 3 ((lambda ((n : 1)) n) (n : 3)))

;; ;; ---------------------------------------------------------------
;; ;; choose

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


;; ;; ---------------------------------------------------------------
;; ;; results
;; (newline)
;; (for-each (lambda (m) (printf "~a ~%" m)) (reverse report))
;; (printf "~%Results:~%    right = ~s~%    wrong = ~s ~%" right wrong)
;; (printf "Time: ~s seconds~%" (- (current-time) start-time))

(run-tests)
