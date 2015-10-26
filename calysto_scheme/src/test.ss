;; Unit tests for Scheme functions
(define start-time (current-time))

(define right 0)
(define wrong 0)
(define report '())

(define verify
  (lambda (name ans f exp)
    (let* ((result (f exp ans))) ;; should be true
      (if (eq? result #t)
	  (begin 
	    (printf ".")
	    (set! right (+ right 1)))
	  (begin
	    (printf "F")
	    (set! report (cons (format "~a:\n      was: ~s\nshould be: ~s" name ans exp) report))
	    (set! wrong (+ wrong 1)))))))

(define verify2
  (lambda (ans exp)
    (verify 'test ans equal? exp)))

(printf "Scheme Unit tests~%")

(verify 'quasiquote `(list ,(+ 1 2) 4) equal? '(list 3 4))
(verify '% (% 10 3) equal? 1)
(verify 'mod (% 10 3) equal? 1)
(verify 'modulo (% 10 3) equal? 1)
(verify '* (* 2 3) equal? 6)
(verify '+ (+ 7 8) equal? 15)
(verify '- (- 5 2) equal? 3)
(verify '/ (/ 3 4) equal? 3/4)
(verify '/ (/ 4 3) equal? 4/3)
(verify '// (// 3 4) equal? 0)
(verify '// (// 4 3) equal? 1)
(verify '< (< 5 2) equal? #f)
(verify '<= (<= 5 6) equal? #t)
(verify '= (= 6 7) equal? #f)
(verify '> (> 9 2) equal? #t)
(verify '>= (>= 4 5) equal? #f)

(verify 'min (min 0 -7 4 10 7) = -7)
(verify 'max (max 0 -7 4 10 7) = 10)

;;(abort) ;; aborts all processing and returns to the top level, optionally with a value
(verify 'abs (abs -1) equal? 1)
(verify 'and (and 4 1 2 #t '() 0) equal? 0)
(verify 'append (append '(1 2 3) '(4 5 6)) equal? '(1 2 3 4 5 6))
(verify 'apply (apply car '((1))) equal? 1)
;;(apply-with-keywords) ;; not yet implemented
(verify 'assq (assq 1 '((1 2) (3 4))) equal? '(1 2))
(verify 'assv (assv 1 '((1 2) (3 4))) equal? '(1 2))
(verify 'atom? (atom? 1) equal? #t)
(verify 'boolean? (boolean? #t) equal? #t)
(verify 'caaaar (caaaar '(((((hello there) this is a test) what is this) another item) in the list)) equal? '(hello there))
(verify 'caaadr (caaadr '(((((hello there) this is a test) what is this) another item) ((((((1 2 3 ) 4 5 6) 7 8 9) 10 11 12) 13 14 15) 16 17 18))) 
	equal? '((((1 2 3) 4 5 6) 7 8 9) 10 11 12))
(verify 'caaar (caaar '(((((hello there) this is a test) what is this) another item) in the list)) equal? '((hello there) this is a test))
(verify 'caadar (caadar '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) in the list)) equal? '((1 2 3) 4 5 6))
(verify 'caaddr (caaddr '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) head ((1 2) 3 4) in the list)) equal? '(1 2))
(verify 'caadr (caadr '(((((hello there) this is a test) what is this) (((1 2 3) 4 5 6) 7 8 9) another item) (in this) ((7 8)) the list)) equal? 'in)
(verify 'caar (caar '(((((hello there) this is a test) what is this) another item) in the list)) equal? '(((hello there) this is a test) what is this))
(verify 'cadaar (cadaar '(((((hello there) this is a test) (what) is this) (yet another) item) in the list)) equal? '(what))
(verify 'cadadr (cadadr '(((((hello there) this is a test) what is this) (yet another) item) (in the) list)) equal? 'the)
(verify 'cadar (cadar '(((((hello there) this is a test) what is this) (yet another) item) in the list)) equal? '(yet another))
(verify 'caddar (caddar '(((((hello there) this is a test) what is this) another item) in the list)) equal? 'item)
(verify 'cadddr (cadddr '(((((hello there) this is a test) what is this) another item) in the list)) equal? 'list)
(verify 'caddr (caddr '(((((hello there) this is a test) what is this) another item) in the list)) equal? 'the)
(verify 'cadr (cadr '(((((hello there) this is a test) what is this) another item) in the list)) equal? 'in)
;;(call-with-current-continuation) ;; see below
;;(call/cc) 
(verify 'car (car '(((((hello there) this is a test) what is this) another item) in the list)) 
	equal? '((((hello there) this is a test) what is this) another item))
(verify 'case (case 'thing1 (thing2 1) (thing1 2)) = 2)
(verify 'case-2 (case 'thing1 (thing2 1) ((thing1 thing3) 2)) = 2)
(verify 'case-3 (case 'thingx (thing2 1) ((thing1 thing3) 2) (else 3)) = 3)
;;(cases) ;; see define-datatype, below
(verify 'cd (cd) (lambda (a b) (string? a)) "")
(verify 'cdaaar (cdaaar '(((((hello there) this is a test) what is this) another item))) equal? '(this is a test))
(verify 'cdaadr (cdaadr '(((((hello there) this is a test) what is this) another item) ((7 8)) 9 10)) equal? '(8))
(verify 'cdaar (cdaar '(((((hello there) this is a test) what is this) another item))) equal? '(what is this))
(verify 'cdadar (cdadar '(((((hello there) this is a test) what is this) (another two) items))) equal? '(two))
(verify 'cdaddr (cdaddr '(((((hello there) this is a test) what is this) another item) 1 (2 5) 3 4)) equal? '(5))
(verify 'cdadr (cdadr '(((((hello there) this is a test) what is this) another item) (1 6) (2 5) 3 4)) equal? '(6))
(verify 'cdar (cdar '(((((hello there) this is a test) what is this) another item))) equal? '(another item))
(verify 'cddaar (cddaar '(((((hello there) this is a test) what is this) another item) 1 (2) 3)) equal? '(is this))
(verify 'cddadr (cddadr '(((((hello there) this is a test) what is this) another item) (7 13) (8 12) 9 10)) equal? '())
(verify 'cddar (cddar '(((((hello there) this is a test) what is this) another item))) equal? '(item))
(verify 'cdddar (cdddar '(((((hello there) this is a test) what is this) another item))) equal? '())
(verify 'cddddr (cddddr '(((((hello there) this is a test) what is this) another item) 1 2 3 4 5)) equal? '(4 5))
(verify 'cdddr (cdddr '(((((hello there) this is a test) what is this) another item) 1 2 3 4)) equal? '(3 4))
(verify 'cddr (cddr '(((((hello there) this is a test) what is this) another item) 1 2 3)) equal? '(2 3))
(verify 'cdr (cdr '(((((hello there) this is a test) what is this) another item) 1 2 3)) equal? '(1 2 3))
(verify 'char->integer (char->integer #\a) = 97)
(verify 'char->string (char->string #\b) equal? "b")
(verify 'char-alphabetic? (char-alphabetic? #\A) equal? #t)
(verify 'char-numeric? (char-numeric? #\1) equal? #t)
(verify 'char-whitespace? (char-whitespace? #\t) equal? #f)
(verify 'char-whitespace? (char-whitespace? #\tab) equal? #t)
(verify 'char-whitespace? (char-whitespace? #\newline) equal? #t)
(verify 'char-whitespace? (char-whitespace? #\a) equal? #f)
(verify 'char=? (char=? #\a #\a) equal? #t)
(verify 'char=? (char=? #\a #\b) equal? #f)
(verify 'char? (char? 2) equal? #f)
(verify 'cond (cond (#f 1) (else 2)) = 2)
(verify 'cons (cons 1 '()) equal? '(1))
(verify 'current-directory (current-directory) (lambda (a b) (string? a)) ".")
(verify 'current-environment (length (dir (current-environment))) < 160)
(verify 'current-time (current-time) (lambda (a b) (< (- a b) .1)) (current-time))
(verify 'cut (letrec ((loop (lambda (n) (if (= n 0) (set! var (cut 23)) (loop (- n 1)))))
		      (var 0))
	       (loop 10)
	       var) equal? '(23))
;;(define-datatype) ;; see below
(verify 'dict (dict '((1 2) (3 4))) (lambda (a b) #t) 'none)
(verify 'dir (length (dir)) < 170)
;;(display 1) ;; no newline
(verify 'eq? (eq? 'a 'a) eq? #t)
(verify 'equal? (equal? 1 1.0) eq? #t)
(verify 'eqv? (eqv? 1 1) eq? #t)
(verify 'error (try (error 'a "message") (catch e e (cadr e))) equal? "Error in 'a': message")
(verify 'eval (eval '(+ 1 2)) = 3)
(verify 'eval-ast (eval-ast (parse '(+ 3 4))) = 7)
(verify 'even? (even? 33) eq? #f)
;;(exit) ;; does this bring down the whole system?
(verify 'float (float 23) = 23.0)
(verify 'for-each (for-each (lambda (n) (+ n 1)) '(1 2 3)) equal? (void))
(verify 'format (format "~a ~s ~%" "hello" "hello") equal? "hello \"hello\" \n")
;;(get) ;; used with import
(verify 'get-stack-trace (caddr (cadar (get-stack-trace))) <= 73)
;;(verify 'globals (globals) equal? (globals))
;;(import "test")
(verify 'int (int 12.8) = 13)
(verify 'integer->char (integer->char 97) equal? #\a)
(verify 'iter? (iter? 3) eq? #f)
(verify 'length (length '(1 2 3)) = 3)
(verify 'let (let ((x 1)) x) = 1)
(verify 'let* (let* ((x 1)(y (+ x 1))) y) = 2)
(verify 'letrec (letrec ((loop (lambda (n) (if (= n 0) 'ok (loop (- n 1)))))) (loop 10)) eq? 'ok)
(verify 'list (list 1 2) equal? '(1 2))
(verify 'list->string (list->string '(#\1 #\2 #\3)) equal? "123")
(verify 'list->vector (list->vector '(1 2 3)) equal? (vector 1 2 3))
(verify 'list-ref (list-ref '(1 2 3) 1) = 2)
(verify 'list? (list? '(1 2 3)) eq? #t)
;;(load) ;; need a file to load
(verify 'make-set (sort < (make-set '(1 2 3 1 2))) equal? '(1 2 3))
(verify 'make-vector (make-vector 3) equal? (vector 0 0 0))
(verify 'map (map (lambda (n) (+ n 1)) (range 5)) equal? '(1 2 3 4 5))
(verify 'member (member "b" '("a" "b" "c")) equal? '("b" "c"))
(verify 'memq (memq 'b '(a b c)) equal? '(b c))
(verify 'memv (memv 2 '(1.0 2.0 3.0)) equal? '(2.0 3.0)) 
;;(newline) ;; outputs a newline
(verify 'not (not #f) eq? #t)
(verify 'null? (null? '()) eq? #t)
(verify 'number->string (number->string 23) equal? "23")
(verify 'number? (number? 23) equal? #t)
(verify 'odd? (odd? 45) equal? #t)
(verify 'or (or #t (/ 1 0)) equal? #t)
(verify 'pair? (pair? '()) equal? #f)
(verify 'pair? (pair? (cons 1 2)) equal? #t)
(verify 'parse (parse '(+ 1 2)) equal? '(app-aexp (lexical-address-aexp 0 1 + none) ((lit-aexp 1 none) (lit-aexp 2 none)) none))
(verify 'parse-string (parse-string "(- 7 8)") equal? '(app-aexp (lexical-address-aexp 0 2 - (stdin 1 2 2 1 2 2)) ((lit-aexp 7 (stdin 1 4 4 1 4 4)) (lit-aexp 8 (stdin 1 6 6 1 6 6))) (stdin 1 1 1 1 7 7)))
;;(print "hello!")
;;(printf "hello ~a!" 'mate)
(verify 'procedure? (procedure? procedure?) eq? #t)
;;(property)
(verify 'quotient (quotient 1 4) = 0)
(verify 'rac (rac '(1 2 3)) = 3)
(verify 'range (range 10) equal? '(0 1 2 3 4 5 6 7 8 9))
(verify 'rational (rational 3 4) = 3/4)
(verify 'rdc (rdc '(1 2 3)) equal? '(1 2))
(verify 'read-string (read-string '(1 2 3)) equal? '((pair) ((atom) 1 (stdin 1 2 2 1 2 2)) ((pair) ((atom) 2 (stdin 1 4 4 1 4 4)) ((pair) ((atom) 3 (stdin 1 6 6 1 6 6)) ((atom) () none) none) none) (stdin 1 1 1 1 7 7)))
;;(record-case) ;; see macros below
(verify 'remainder (remainder 1 4) = 1)
(verify 'require (require #t) eq? 'ok) ;; requires an expression to be true
;;(reset-toplevel-env) ;; can't test here
(verify 'reverse (reverse '(1 2 3)) equal? '(3 2 1))
(verify 'round-1 (round 45.5) = 46)
(verify 'round-2 (round 45.4) = 45)
(verify 'set-car! (let ((x '(1 2 3))) (set-car! x 0) x) equal? '(0 2 3))
(verify 'set-cdr! (let ((x '(1 2 3))) (set-cdr! x '(3 4)) x) equal? '(1 3 4))
(verify 'snoc (snoc 0 '(1 2 3)) equal? '(1 2 3 0))
(verify 'sort (sort < '(3 7 1 2)) equal? '(1 2 3 7))
(verify 'sqrt (sqrt 3) equal? 1.7320508075688772)
(verify 'string (string #\1 #\2) equal? "12")
(verify 'string->list (string->list "hello world") 
	equal? '(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d))
(verify 'string->number (string->number "12.1") equal? 12.1)
(verify 'string->symbol (string->symbol "hello") eq? 'hello)
(verify 'string-append (string-append "hell" "o") equal? "hello")
(verify 'string-length (string-length "what") = 4)
(verify 'string-ref (string-ref "what" 2) equal? #\a)
(verify 'string-split (string-split "hello.world" #\.) equal? '("hello" "world"))
(verify 'string<? (string<? "a" "b") eq? #t)
(verify 'string=? (string=? "a" "b") eq? #f)
(verify 'string? (string? "hello") eq? #t)
(verify 'substring (substring "hello" 1 3) equal? "el")
(verify 'symbol (symbol "hello") eq? 'hello)
(verify 'symbol->string (symbol->string 'hello) equal? "hello")
(verify 'symbol? (symbol? 'hello) eq? #t)
(verify 'typeof (typeof 23) eq? (typeof 24))
(verify 'unparse (unparse (parse '(+ 1 2))) equal? '(+ 1 2))
;;(unparse-procedure (lambda (n) (+ n 1))) ;; no longer possible?
(verify 'use-lexial-address (use-lexical-address) eq? #t)
(verify 'use-satck-trace (use-stack-trace) eq? #t)
(verify 'use-tracing (use-tracing) eq? #f)
(verify 'import (try 
		(import "math")
		(catch e e 
		       (import "Graphics"))) (lambda (a b) (not (null? b))) '())
(verify 'vector (vector 1 2 3) equal? (vector 1 2 3))
(verify 'vector->lsit (vector->list (vector 1 2 3)) equal? '(1 2 3))
(verify 'vector-ref (vector-ref (vector 1 2 3) 2) = 3)

(verify 'let (let ((v (vector 1 2 3))) (vector-set! v 2 'a) v) equal? (vector 1 2 'a))
(verify 'vector? (vector? (vector)) eq? #t)
(verify '(void) (void) equal? (void))
(verify 'zero? (zero? 0.0) equal? #t)

;;---------------------------------------------------------------------
;; collect is like list comprehension in Python

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


;; Calico Scheme Tests

(define my-odd? 'undefined)
(define my-even? 'undefined)

(letrec
    ((odd (lambda (n) (if (= n 0) #f (even (- n 1)))))
     (even (lambda (n) (if (= n 0) #t (odd (- n 1))))))
  (set! my-odd? odd)
  (set! my-even? even))

(verify 'my-odd (my-odd? 42) eq? #f)
(verify 'my-even (my-even? 42) eq? #t)
(verify 'my-odd (my-odd? 43) eq? #t)
(verify 'my-even (my-even? 43) eq? #f)
(verify2 '(0 1 4 9 16 25 36 49 64 81) (collect (* n n) for n in (range 10)))
(verify2 '(25 64 121 196 289) (collect (* n n) for n in (range 5 20 3)))
(verify2 '(36 49 64 81) (collect (* n n) for n in (range 10) if (> n 5)))
(verify2 5 (begin (define hello 0)
		 (for 5 times do (set! hello (+ hello 1)))
		 hello))
(verify2 'done (for sym in '(a b c d) do (define x 1) (set! x sym) x))
(verify2 'done (for n in (range 10 20 2) do n))
(verify2 'done (for n at (i j) in matrix2d do (list n 'coords: i j)))
(verify2 'done (for n at (i j k) in matrix3d do (list n 'coords: i j k)))
(verify2 120 (! 5))
(verify2 3628800 (nth 10 facts))
(verify2 10946 (nth 20 fibs))
(verify2 '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181
	    6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040)
        (first 30 fibs))

(define test-mu-lambda
  (lambda ()
    (verify2 '(1 2 3 4 5)
      ((lambda x x) 1 2 3 4 5))
    (verify2 '(1 (2 3 4 5))
      ((lambda (x . y) (list x y)) 1 2 3 4 5))
    (verify2 '(1 2 (3 4 5))
      ((lambda (a b . z) (list a b z)) 1 2 3 4 5))
    (verify2 '(1 2 (3))
      ((lambda (a b . z) (list a b z)) 1 2 3))
    (verify2 '(1 2 ())
      ((lambda (a b . z) (list a b z)) 1 2))
    (verify2 "not enough arguments given"
      (try ((lambda (a b . z) (list a b z)) 1)
	       (catch e e "not enough arguments given")))
    ))

(define test-define
  (lambda ()
    (define f1 (lambda (a b c) (list a b c)))
    (define (f2) (list 42))
    (define (f3 . x) (list x))
    (define (f4 a b c . x) (list a b c x))
    (define (f5 a b c x) (list a b c x))
    (verify2 '((1 2 3) (42) ((1 2 3)) (1 2 3 (4 5)) (1 2 3 4))
      (list (f1 1 2 3) (f2) (f3 1 2 3) (f4 1 2 3 4 5) (f5 1 2 3 4)))))

(define test-call/cc
  (lambda ()
    (verify2 40
      (* 10 (call/cc (lambda (k) 4))))
    (verify2 40
      (* 10 (call/cc (lambda (k) (+ 1 (k 4))))))
    (verify2 50
      (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (j (k 5))))))))))
    (verify2 60
      (* 10 (call/cc (lambda (k) (+ 1 (call/cc (lambda (j) (+ 2 (k (j 5))))))))))))

(define test-try
  (lambda ()
    (verify2 3
      (try 3))
    (verify2 3
      (try 3 (finally 'yes 4)))
    (verify2 'yes
      (try (raise 'yes) (catch e e)))
    (verify2 'yes
      (try (try (raise 'yes)) (catch e e)))
    (verify2 'oops
      (try (try (begin 'one (raise 'oops) 'two)) (catch e e)))
    (verify2 40
      (* 10 (try (begin 'one (raise 'oops) 'two)
            (catch ex 3 4))))
    (verify2 50
      (* 10 (try (begin 'one 'two 5)
            (catch ex 3 4))))
    (verify2 40
      (* 10 (try (begin 'one (raise 'oops) 5)
            (catch ex (list 'ex: ex) 4))))
    (verify2 'oops
      (try (* 10 (try (begin 'one (raise 'oops) 5)
            (catch ex (list 'ex: ex) (raise ex) 4))) (catch e e)))
    (verify2 'oops
      (try (* 10 (try (begin 'one (raise 'oops) 5)
              (catch ex (list 'ex: ex) (raise ex) 4)
              (finally 'two 7))) (catch e e)))
    (verify2 77
      (try (* 10 (try (begin 'one (raise 'oops) 5)
		      (catch ex (list 'ex: ex) (raise 'bar) 4)))
	   (catch x 'hello 77)))
    (verify2 3
      (try 3 (finally 'hi 4)))
    (verify2 5
      (div 10 2))
    (verify2 "division by zero"
      (try (div 10 0) (catch e (cadr e))))
    (verify2 "division by zero"
      (try (let ((x (try (div 10 0)))) x) (catch e (cadr e))))
    (verify2 5
      (let ((x (try (div 10 2) (catch e -1)))) x))
    (verify2 -1
      (let ((x (try (div 10 0) (catch e -1)))) x))
    (verify2 5
      (let ((x (try (div 10 2) (catch e -1) (finally 'closing-files 42))))  x))
    (verify2 -1
      (let ((x (try (div 10 0) (catch e -1) (finally 'closing-files 42))))  x))
    (verify2 5
      (let ((x (try (div 10 2) (finally 'closing-files 42))))  x))
    (verify2 'foo
      (try (let ((x (try (div 10 0) (catch e -1 (raise 'foo)) (finally 'closing-files 42))))  x) (catch e e)))
    (verify2 'ack
      (try (let ((x (try (div 10 0)
                (catch e -1 (raise 'foo))
                (finally 'closing-files (raise 'ack) 42))))
       x) (catch e e)))
    (verify2 99
      (try (let ((x (try (div 10 0)
                     (catch e -1 (raise 'foo))
                     (finally 'closing-files (raise 'ack) 42))))
            x)
       (catch e (if (equal? e 'ack) 99 (raise 'doug)))
       (finally 'closing-outer-files)))
    (verify2 'doug
      (try (try (let ((x (try (div 10 0)
                     (catch e -1 (raise 'foo))
                     (finally 'closing-files (raise 'ack) 42))))
            x)
       (catch e (if (equal? e 'foo) 99 (raise 'doug)))
       (finally 'closing-outer-files)) (catch e e)))
    ))

(define test-loop
  (lambda ()
    (verify2 'blastoff! (try (let loop ((n 5))
                            n
                            (if (= n 0)
                                (raise 'blastoff!))
                            (loop (- n 1)))
                (catch e e)))))

(define (test-macros)
  (verify2 #t
    (let ((bool 5))
      (or (= bool 4) (= bool 5))))
  (verify2 6
    (let ((bool 5))
      (or (= bool 4) 6)))
  (verify2 #f
    (let ((bool 5))
      (and (= bool 5) (> bool 0) (= bool 4))))
  (verify2 5
    (let ((r 5))
      (case 'banana
	(apple 'no)
	((cherry banana) 1 2 r)
	(else 'no))))
  (verify2 '((6) orange 5)
    (let ((r 5))
      (record-case (cons 'banana (cons 'orange (cons (* 2 3) '())))
	(apple (a b c) (list c b a r))
	((cherry banana) (a . b) (list b a r))
	((orange) () 'no)
	(else 2 3 4)))))

(test-mu-lambda)
(test-define)
(test-call/cc)
(test-loop)
(test-macros)
(test-try)

(define-datatype lc-exp lc-exp?
  (var-exp 
   (var symbol?))
  (lambda-exp 
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(verify 'define-datatype-1 lc-exp? (lambda (a b) (procedure? b)) '())
(verify 'define-datatype-2 var-exp (lambda (a b) (procedure? b)) '())
(verify 'define-datatype-3 lambda-exp (lambda (a b) (procedure? b)) '())
(verify 'define-datatype-4 app-exp (lambda (a b) (procedure? b)) '())

(verify 'define-datatype-5 (var-exp 'a) (lambda (a b) (lc-exp? b)) '())
(verify 'define-datatype-6 (lambda-exp 'a (var-exp 'a)) (lambda (a b) (lc-exp? b)) '())
(verify 'define-datatype-7 (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a)) (lambda (a b) (lc-exp? b)) '())

(define un-parse
  (lambda (exp)
    (cases lc-exp exp
       (var-exp (var) var)
       (lambda-exp (bound-var body) (list bound-var body))
       (app-exp (rator rand) (list rator rand)))))

(verify 'define-datatype-8 (un-parse (var-exp 'a)) eq? 'a)
(verify 'define-datatype-8 (un-parse (lambda-exp 'a (var-exp 'a))) equal? '(a (var-exp a)))
(verify 'define-datatype-8 (un-parse (app-exp (lambda-exp 'a (var-exp 'a)) (var-exp 'a))) equal? '((lambda-exp a (var-exp a)) (var-exp a)))

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

(verify 'choose (floors2) equal? '((baker: 3) (cooper: 2) (fletcher: 4) (miller: 5) (smith: 1)))

(newline)
(for-each (lambda (m) (printf "~a ~%" m)) (reverse report))
(printf "~%Results:~%    right = ~s~%    wrong = ~s ~%" right wrong)
(printf "Time: ~s seconds~%" (- (current-time) start-time))
