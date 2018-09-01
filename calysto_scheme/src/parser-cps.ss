;; Calico Scheme parser
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

(load "transformer-macros.ss")

;;--------------------------------------------------------------------------
;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader-cps.ss")
(load "unifier-cps.ss")

(define *use-lexical-address* #t)

;; for the macro environment
(load "environments-cps.ss")

;;--------------------------------------------------------------------------
;; these definitions enable parser-cps.ss to be run directly in Petite
;; Chez Scheme, because parser-cps.ss uses environments-cps.ss, which
;; relies on these functions being defined.

(define-native dlr-proc? (lambda (x) #f))
(define-native dlr-apply apply)
(define-native dlr-func (lambda (x) x))
(define-native callback (lambda args #f))
(define-native dlr-env-contains (lambda (x) #f))
(define-native dlr-env-lookup (lambda (x) #f))
(define-native dlr-object? (lambda (x) #f))
(define-native dlr-lookup-components (lambda (x y) #f))
(define-native set-global-value! (lambda (var x) #f))
(define-native set-global-docstring! (lambda (var x) #f))
(define-native printf-prim printf)
(define-native import-native (lambda ignore #f))
(define-native iterator? (lambda ignore #f))
(define-native get_type (lambda (x) 'unknown))

;;--------------------------------------------------------------------------
;; The core grammar
;;
;; <exp> ::= <literal>
;;         | <vector>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (func <exp>)
;;         | (define <var> <exp>)
;;         | (define <var> <docstring> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (define-syntax <keyword> <lambda>)
;;         | (define-tests <var> <assertion> ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (trace-lambda name (<formal> ...) <exp> ...)
;;         | (trace-lambda name <formal> <exp> ...)
;;         | (<exp> <exp> ...)
;;         | (try <body> (catch <var> <exp> ...))
;;         | (try <body> (finally <exp> ...))
;;         | (try <body> (catch <var> <exp> ...) (finally <exp> ...))
;;         | (raise <exp>)
;;         | (choose <exp> ...)

;;--------------------------------------------------------------------------

(define id?
  (lambda (exp)
    (or (symbol? exp)
	(association? exp))))

(define-datatype aexpression aexpression?
  (lit-aexp
    (datum anything?)
    (info source-info?))
  (var-aexp
    (id symbol?)
    (info source-info?))
  (lexical-address-aexp
    (depth number?)
    (offset number?)
    (id symbol?)
    (info source-info?))
  (if-aexp
    (test-aexp aexpression?)
    (then-aexp aexpression?)
    (else-aexp aexpression?)
    (info source-info?))
  (help-aexp
    (var symbol?)
    (var-info source-info?)
    (info source-info?))
  (association-aexp
    (var symbol?)
    (exp aexpression?)
    (info source-info?))
  (assign-aexp
    (var symbol?)
    (rhs-exp aexpression?)
    (var-info source-info?)
    (info source-info?))
  (func-aexp
    (exp aexpression?)
    (info source-info?))
  (callback-aexp
    (exp aexpression?)
    (info source-info?))
  (define-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define!-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define-syntax-aexp
    (name symbol?)
    (clauses (list-of define-syntax-clause?))
    (aclauses list-of-define-syntax-clauses?^)
    (info source-info?))
  ;; new
  (define-syntax-transformer-aexp
    (name symbol?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define-tests-aexp
    (name symbol?)
    (aclauses (list-of aexpression?))
    (info source-info?))
  (run-tests-aexp
    (tests (list-of list?)))
  (begin-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  (lambda-aexp
    (formals (list-of id?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-lambda-aexp
    (formals (list-of id?))
    (runt id?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (trace-lambda-aexp
    (name symbol?)
    (formals (list-of id?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-trace-lambda-aexp
    (name symbol?)
    (formals (list-of id?))
    (runt id?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (app-aexp
    (operator aexpression?)
    (operands (list-of aexpression?))
    (info source-info?))
  (try-catch-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (info source-info?))
  (try-finally-aexp
    (body aexpression?)
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (try-catch-finally-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (raise-aexp
    (exp aexpression?)
    (info source-info?))
  (choose-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  )

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((association? formals) '())
      ((pair? (cdr formals)) (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((association? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define anything?
  (lambda (datum) #t))

(define application?^
  (lambda (asexp)
    (and (list?^ asexp)
	 (not (null?^ asexp))
	 (not (reserved-keyword? (untag-atom^ (car^ asexp)))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
	 (not (eq? (memq x (get-reserved-keywords)) #f)))))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define begin cond and or
	    let let* letrec case record-case try catch finally raise
	    define-syntax choose define-datatype cases trace-lambda)))

(define mit-style-define?^
  (lambda (asexp)
    (and (define?^ asexp)
	 (pair?^ (cadr^ asexp)))))

;; used in aunparse
(define literal?
  (lambda (datum)
    (or (number? datum)
	(boolean? datum)
	(null? datum)
	(char? datum)
	(string? datum))))

(define literal?^
  (lambda (asexp)
    (and (eq? (car asexp) atom-tag)
	 (or (number? (untag-atom^ asexp))
	     (boolean? (untag-atom^ asexp))
	     (null? (untag-atom^ asexp))
	     (char? (untag-atom^ asexp))
	     (string? (untag-atom^ asexp))))))

(define syntactic-sugar?^
  (lambda (asexp)
    (and (pair?^ asexp)
	 (symbol?^ (car^ asexp))
	 (in-first-frame? (untag-atom^ (car^ asexp)) macro-env))))
;;	 (true? (search-env macro-env (untag-atom^ (car^ asexp)))))))

(define-native tagged-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (car^ asexp) keyword)))))

(define-native tagged-list-or^
  (lambda (keyword1 keyword2 op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (or (eq?^ (car^ asexp) keyword1)
	       (eq?^ (car^ asexp) keyword2))))))

(define-native tagged2-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (cadr^ asexp) keyword)))))

(define list-of-test-groups?^
  (lambda (x)
    (or (null?^ x)
	(and (pair?^ x)
	     (atom?^ (car^ x))
	     (or (integer? (untag-atom^ (car^ x)))
		 (string? (untag-atom^ (car^ x))))
	     (list-of-test-groups?^ (cdr^ x))))))

(define quote?^ (tagged-list^ 'quote = 2))
(define quasiquote?^ (tagged-list^ 'quasiquote = 2))
(define unquote?^ (tagged-list^ 'unquote >= 2))  ;; >= for alan bawden's qq-expand algorithm
(define unquote-splicing?^ (tagged-list^ 'unquote-splicing >= 2))
(define if-then?^ (tagged-list^ 'if = 3))
(define if-else?^ (tagged-list^ 'if = 4))
(define help?^ (tagged-list^ 'help = 2))
(define association?^ (tagged2-list^ ': = 3))
(define assignment?^ (tagged-list^ 'set! = 3))
(define func?^ (tagged-list^ 'func = 2))
(define callback?^ (tagged-list^ 'callback = 2))
(define define?^ (tagged-list^ 'define >= 3))
(define define!?^ (tagged-list^ 'define! >= 3))
(define define-syntax?^ (tagged-list^ 'define-syntax >= 3))
(define define-var^ (lambda (x) (untag-atom^ (cadr^ x))))
(define define-docstring^ (lambda (x) (untag-atom^ (caddr^ x))))
(define begin?^ (tagged-list^ 'begin >= 2))
(define lambda?^ (tagged-list-or^ 'lambda 'λ >= 3))
(define lambda-no-defines?^ (tagged-list^ 'lambda-no-defines >= 3))
;;(define trace-lambda?^ (tagged-list^ 'trace-lambda >= 4))
(define trace-lambda-no-defines?^ (tagged-list^ 'trace-lambda-no-defines >= 4))
(define raise?^ (tagged-list^ 'raise = 2))
(define choose?^ (tagged-list^ 'choose >= 1))
(define try?^ (tagged-list^ 'try >= 2))
(define try-body^ (lambda (x) (cadr^ x)))
(define catch?^ (tagged-list^ 'catch >= 3))
(define catch-var^ (lambda (x) (untag-atom^ (cadr^ (caddr^ x)))))
(define catch-exps^ (lambda (x) (cddr^ (caddr^ x))))
(define finally?^ (tagged-list^ 'finally >= 2))
(define try-finally-exps^ (lambda (x) (cdr^ (caddr^ x))))
(define try-catch-finally-exps^ (lambda (x) (cdr^ (cadddr^ x))))
(define define-tests?^ (tagged-list^ 'define-tests >= 2))
(define run-tests?^ (tagged-list^ 'run-tests >= 1))

(define* aparse
  (lambda (adatum senv handler fail k)   ;; k receives 2 args: aexp, fail
    (let ((info (get-source-info adatum)))
      (cond
	((literal?^ adatum) (k (lit-aexp (untag-atom^ adatum) info) fail))
	((symbol?^ adatum)
	 (if *use-lexical-address*
	     (get-lexical-address (untag-atom^ adatum) senv 0 info fail k)
	     (k (var-aexp (untag-atom^ adatum) info) fail)))
	((vector?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp v info) fail))))
	((quote?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp (cadr v) info) fail))))
	((quasiquote?^ adatum)
	 (qq-expand-cps (cadr^ adatum) 0
	   (lambda-cont (v)
	     (annotate-cps v 'none
	       (lambda-cont (expansion)
		 (if (original-source-info? adatum)
		   (aparse (replace-info expansion (snoc 'quasiquote info)) senv handler fail k)
		   (aparse (replace-info expansion info) senv handler fail k)))))))
	((unquote?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((unquote-splicing?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((syntactic-sugar?^ adatum)
	 (expand-once^ adatum handler fail
	   (lambda-cont2 (expansion fail)
	     (aparse expansion senv handler fail k))))
	((if-then?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (if-aexp v1 v2 (lit-aexp #f 'none) info) fail))))))
	((if-else?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (aparse (cadddr^ adatum) senv handler fail
		   (lambda-cont2 (v3 fail)
		     (k (if-aexp v1 v2 v3 info) fail))))))))
	((help?^ adatum)
	   (let ((var-info (get-source-info (cadr^ adatum))))
	     (k (help-aexp (untag-atom^ (cadr^ adatum)) var-info info) fail)))
	((assignment?^ adatum)
	 (aparse (caddr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (assign-aexp (untag-atom^ (cadr^ adatum)) v var-info info) fail)))))
	((association?^ adatum)
	 (aparse (caddr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (association-aexp (untag-atom^ (car^ adatum)) v var-info info) fail)))))
	((func?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (e fail)
	     (k (func-aexp e info) fail))))
	((callback?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (e fail)
	     (k (callback-aexp e info) fail))))
	((define?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((and (= (length^ adatum) 3)
		 (symbol?^ (cadr^ adatum))) ;; (define <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4)
		 (symbol?^ (cadr^ adatum))
		 (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define!?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define! <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define! <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define-syntax?^ adatum)
	 (let ((name (define-var^ adatum)))
	   (if (lambda?^ (caddr^ adatum))
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (body fail)
		 (k (define-syntax-transformer-aexp name body info) fail)))
	     (let ((aclauses (cddr^ adatum)))
	       (unannotate-cps aclauses
		 (lambda-cont (clauses)
		   (k (define-syntax-aexp name clauses aclauses info) fail)))))))
	((define-tests?^ adatum)
	 (let ((name (define-var^ adatum))
	       (aclauses (cddr^ adatum)))
	   (aparse-all aclauses senv handler fail
	     (lambda-cont2 (exps fail)
	       (k (define-tests-aexp name exps info) fail)))))
	((run-tests?^ adatum)
	 (let ((args (cdr^ adatum)))
	   (cond
	     ((null?^ args) (k (run-tests-aexp '()) fail))
	     ((and (symbol?^ (car^ args)) (list-of-test-groups?^ (cdr^ args)))
	      (aparse-unit-tests (list^ args) handler fail
		(lambda-cont2 (tests fail)
		  (k (run-tests-aexp tests) fail))))
	     (else (aparse-unit-tests args handler fail
		     (lambda-cont2 (tests fail)
		       (k (run-tests-aexp tests) fail)))))))
	((begin?^ adatum)
	 (cond
	   ((null?^ (cdr^ adatum)) (aparse-error "bad concrete syntax:" adatum handler fail))
	   ((null?^ (cddr^ adatum)) (aparse (cadr^ adatum) senv handler fail k))
	   (else (aparse-all (cdr^ adatum) senv handler fail
		   (lambda-cont2 (exps fail)
		     (k (begin-aexp exps info) fail))))))
	((lambda-no-defines?^ adatum)
	 (unannotate-cps (cadr^ adatum)
	    (lambda-cont (formals)
	        (let ((formals-list
			(if (and (list? formals) (not (association? formals)))
			    formals
			    (cons (last formals) (head formals)))))
		  (aparse-all (cddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (and (list? formals) (not (association? formals)))
			     (k (lambda-aexp formals bodies info) fail)
			     (k (mu-lambda-aexp (head formals) (last formals) bodies info) fail))))))))
	((trace-lambda-no-defines?^ adatum)
	 (unannotate-cps (caddr^ adatum)
	      (lambda-cont (formals)
		(let ((formals-list
			(if (and (list? formals) (not (association? formals)))
			    formals
			    (cons (last formals) (head formals))))
		      (name (untag-atom^ (cadr^ adatum))))
		  (aparse-all (cdddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (and (list? formals) (not (association? formals)))
			     (k (trace-lambda-aexp name formals bodies info) fail)
			     (k (mu-trace-lambda-aexp name (head formals) (last formals) bodies info) fail))))))))
	((try?^ adatum)
	 (cond
	  ;; (try <body>)
	   ((= (length^ adatum) 2)
	    (aparse (try-body^ adatum) senv handler fail k))
	   ;; (try <body> (catch <var> <exp> ...))
	   ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		 (let ((cvar (catch-var^ adatum)))
		   (aparse-all (catch-exps^ adatum) (cons (list cvar) senv) handler fail
		     (lambda-cont2 (cexps fail)
		      (k (try-catch-aexp body cvar cexps info) fail)))))))
	   ;; (try <body> (finally <exp> ...))
	   ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (try-finally-exps^ adatum) senv handler fail
		  (lambda-cont2 (fexps fail)
		    (k (try-finally-aexp body fexps info) fail))))))
	   ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	   ((and (= (length^ adatum) 4) (catch?^ (caddr^ adatum)) (finally?^ (cadddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(let ((cvar (catch-var^ adatum)))
		  (aparse-all (catch-exps^ adatum) (cons (list cvar) senv) handler fail
		    (lambda-cont2 (cexps fail)
		      (aparse-all (try-catch-finally-exps^ adatum) senv handler fail
			(lambda-cont2 (fexps fail)
			  (k (try-catch-finally-aexp body cvar cexps fexps info) fail)))))))))
	   (else (aparse-error "bad try syntax:" adatum handler fail))))
	((raise?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (k (raise-aexp v info) fail))))
	((choose?^ adatum)
	 (aparse-all (cdr^ adatum) senv handler fail
	   (lambda-cont2 (exps fail)
	     (k (choose-aexp exps info) fail))))
	((application?^ adatum)
	 (aparse (car^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse-all (cdr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (app-aexp v1 v2 info) fail))))))
	(else (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define* aparse-unit-tests
  (lambda (args handler fail k)
    (cond
      ((null?^ args) (k '() fail))
      ((symbol?^ (car^ args))
       (aparse-unit-tests (cdr^ args) handler fail
	 (lambda-cont2 (tests fail)
	   (k (cons (list (untag-atom^ (car^ args))) tests) fail))))
      ((and (list?^ (car^ args))
	    (not (null?^ (car^ args)))
	    (symbol?^ (caar^ args))
	    (list-of-test-groups?^ (cdar^ args)))
       (aparse-unit-tests (cdr^ args) handler fail
	 (lambda-cont2 (tests fail)
	   (unannotate-cps (car^ args)
	     (lambda-cont (test)
	       (k (cons test tests) fail))))))
      (else (aparse-error "bad unit test syntax:" (car^ args) handler fail)))))

(define* aparse-all
  (lambda (adatum-list senv handler fail k)
    (if (null?^ adatum-list)
      (k '() fail)
      (aparse (car^ adatum-list) senv handler fail
	(lambda-cont2 (a fail)
	  (aparse-all (cdr^ adatum-list) senv handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps adatum
	(lambda-cont (datum)
	  (handler (make-exception "ParseError" (format "~a ~a" msg datum)
			 (get-srcfile info)
			 (get-start-line info)
			 (get-start-char info))
		   fail))))))

;; used once in interpreter-cps.ss
(define* aparse-sexps
  (lambda (tokens src senv handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (adatum end tokens-left fail)
	  (aparse adatum senv handler fail
	    (lambda-cont2 (v1 fail)
	      (aparse-sexps tokens-left src senv handler fail
		(lambda-cont2 (v2 fail)
		  (k (cons v1 v2) fail))))))))))

(define* get-lexical-address
  (lambda (id senv depth info fail k)
    (cond
      ((null? senv) (k (var-aexp id info) fail))   ;; free!
      ((memq id (car senv))
       (get-lexical-address-offset id (car senv) depth 0 info fail k))
      (else (get-lexical-address id (cdr senv) (+ depth 1) info fail k)))))

(define* get-lexical-address-offset
  (lambda (id contours depth offset info fail k)
    (if (eq? (car contours) id)
      (k (lexical-address-aexp depth offset id info) fail)
      (get-lexical-address-offset id (cdr contours) depth (+ offset 1) info fail k))))

;;(define get-lexical-address
;;  ;; given an environment and variable id, return the depth of the
;;  ;; frame and offset OR return it as a var-exp signifying it as an
;;  ;; unbound variable
;;  (lambda (senv id info)
;;    (get-lexical-address-frames (frames senv) id 0 0 info)))

;;(define get-lexical-address-frames
;;  ;; given a list of frames, get the lexical address of variable
;;  (lambda (frames variable depth offset info)
;;    (cond
;;     ((null? frames) (var-aexp variable info)) ;; free!
;;     (else (let ((result (get-lexical-address-frame (car frames) variable depth offset info)))
;;	     (if (not (car result))
;;		 (get-lexical-address-frames (cdr frames) variable (+ 1 depth) 0 info)
;;		 (cadr result)))))))

;;(define get-lexical-address-frame
;;  ;; returns (#t pos) or (#f) signifying bound or free, respectively
;;  (lambda (frame variable depth offset info)
;;    (cond
;;     ((empty-frame? frame) (list #f)) ;; not in this frame
;;     ((>= offset (vector-length frame)) (list #f)) ;; not here
;;     ((eq? (binding-variable (vector-ref frame offset)) variable)
;;      (list #t (lexical-address-aexp depth offset variable info)))
;;     (else (get-lexical-address-frame frame variable depth (+ 1 offset) info)))))

;;--------------------------------------------------------------------------
;; Macro support

;; transformer macros:

(define lambda-transformer^
  (lambda-macro (adatum handler fail k)
    (if (< (length^ adatum) 3)
	(aparse-error "bad lambda expression:" adatum handler fail)
	(let ((formals (cadr^ adatum))
	      (bodies (cddr^ adatum)))
	  (get-internal-defines^ bodies adatum handler fail
	    (lambda-cont2 (defines bodies2)
	      (if (null? defines)
		  (k `(lambda-no-defines ,formals ,@(at^ bodies2)))
		  (create-letrec-bindings^ defines handler fail
		    (lambda-cont (bindings)
		      (k `(lambda-no-defines ,formals (letrec ,bindings ,@(at^ bodies2)))))))))))))

(define trace-lambda-transformer^
  (lambda-macro (adatum handler fail k)
    (if (< (length^ adatum) 4)
      (aparse-error "bad trace-lambda expression:" adatum handler fail)
      (let ((name (cadr^ adatum))
	    (formals (caddr^ adatum))
	    (bodies (cdddr^ adatum)))
	(get-internal-defines^ bodies adatum handler fail
	  (lambda-cont2 (defines bodies2)
	    (if (null? defines)
		(k `(trace-lambda-no-defines ,name ,formals ,@(at^ bodies2)))
		(create-letrec-bindings^ defines handler fail
		  (lambda-cont (bindings)
		    (k `(trace-lambda-no-defines ,name ,formals (letrec ,bindings ,@(at^ bodies2)))))))))))))

(define get-internal-defines^
  (lambda (bodies adatum handler fail k)  ;; k receives 2 args: defines, bodies2
    (cond
      ((null?^ bodies)
       (aparse-error "no body expressions found for" adatum handler fail))
      ((define?^ (car^ bodies))
       (get-internal-defines^ (cdr^ bodies) adatum handler fail
	 (lambda-cont2 (v1 v2)
	   (k (cons (car^ bodies) v1) v2))))
      (else (any-internal-defines?^ (cdr^ bodies)
	      (lambda-cont (v)
		(if v
		  (aparse-error "misplaced define in" adatum handler fail)
		  (k '() bodies))))))))

(define any-internal-defines?^
  (lambda (exps k)
    (if (null?^ exps)
      (k #f)
      (if (define?^ (car^ exps))
	  (k #t)
	  (any-internal-defines?^ (cdr^ exps) k)))))

(define create-letrec-bindings^
  (lambda (defines handler fail k)
    (if (null? defines)
      (k '())
      (create-letrec-bindings^ (cdr defines) handler fail
	(lambda-cont (bindings)
	  (get-define-var-and-exp^ (car defines) handler fail
	    (lambda-cont2 (var exp)
	      (k (cons `(,var ,exp) bindings)))))))))

(define get-define-var-and-exp^
  (lambda (adatum handler fail k)
    (if (mit-style-define?^ adatum)
	(let ((name (caadr^ adatum))
	      (formals (cdadr^ adatum))
	      (bodies (cddr^ adatum)))
	  (k name `(lambda ,formals ,@(at^ bodies))))
	(if (= (length^ adatum) 3) ;; (define <var> <body>)
	    (let ((name (define-var^ adatum))
		  (exp (caddr^ adatum)))
	      (k name exp))
	    (if (and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	      (let ((name (define-var^ adatum))
		    (exp (cadddr^ adatum)))
		(k name exp))
	      (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define let-transformer^
  (lambda-macro (adatum handler fail k)
    (if (symbol?^ (cadr^ adatum))
      ;; named let
      (let* ((name (cadr^ adatum))
	     (bindings (caddr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cdddr^ adatum)))
	(k `(letrec ((,name (lambda ,vars ,@(at^ bodies)))) (,name ,@(at^ exps)))))
      ;; ordinary let
      (let* ((bindings (cadr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cddr^ adatum)))
	(k `((lambda ,vars ,@(at^ bodies)) ,@(at^ exps)))))))

(define letrec-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((decls (cadr^ adatum))
	   (vars (map^ car^ decls))
	   (procs (map^ cadr^ decls))
	   (bodies (cddr^ adatum)))
      (create-letrec-assignments^ vars procs
	(lambda-cont2 (bindings assigns)  ;; bindings and assigns are unannotated
	  (k `(let ,bindings ,@assigns ,@(at^ bodies))))))))

(define* create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null?^ vars)
      (k2 '() '())
      (create-letrec-assignments^ (cdr^ vars) (cdr^ procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car^ vars) 'undefined) bindings)
	      (cons `(set! ,(car^ vars) ,(car^ procs)) assigns)))))))

(define mit-define-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((name (caadr^ adatum))
	  (formals (cdadr^ adatum))
	  (bodies (cddr^ adatum)))
      (k `(define ,name (lambda ,formals ,@(at^ bodies)))))))

(define and-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#t))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(if ,(car^ exps) (and ,@(at^ (cdr^ exps))) #f)))))))

;; avoids variable capture
(define or-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#f))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(let ((bool ,(car^ exps))
			(else-code (lambda () (or ,@(at^ (cdr^ exps))))))
		    (if bool bool (else-code)))))))))

(define* amacro-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (handler (make-exception "MacroError" msg (get-start-line info)
		     (get-srcfile info)
		     (get-start-char info))
	       fail))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((clauses (cdr^ adatum)))
      (if (null?^ clauses)
	(amacro-error "empty (cond) expression" adatum handler fail)
	(let ((first-clause (car^ clauses))
	      (other-clauses (cdr^ clauses)))
	  (if (or (null?^ first-clause) (not (list?^ first-clause)))
	    (amacro-error "improper cond clause" first-clause handler fail)
	    (let ((test-exp (car^ first-clause))
		  (then-exps (cdr^ first-clause)))
	      (cond
		((eq?^ test-exp 'else)
		 (cond
		   ((null?^ then-exps)
		    (amacro-error "improper else clause" first-clause handler fail))
		   ((null?^ (cdr^ then-exps)) (k (car^ then-exps)))
		   (else (k `(begin ,@(at^ then-exps))))))
		((null?^ then-exps)
		 (if (null?^ other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@(at^ other-clauses)))))
			 (if bool bool (else-code))))))
		((eq?^ (car^ then-exps) '=>)
		 (cond
		   ((null?^ (cdr^ then-exps))
		    (amacro-error "improper => clause" first-clause handler fail))
		   ((null?^ other-clauses)
		    (k `(let ((bool ,test-exp)
			      (th (lambda () ,(cadr^ then-exps))))
			  (if bool ((th) bool)))))
		   (else (k `(let ((bool ,test-exp)
				   (th (lambda () ,(cadr^ then-exps)))
				   (else-code (lambda () (cond ,@(at^ other-clauses)))))
			       (if bool ((th) bool) (else-code)))))))
		((null?^ other-clauses)
		 (if (null?^ (cdr^ then-exps))
		   (k `(if ,test-exp ,(car^ then-exps)))
		   (k `(if ,test-exp (begin ,@(at^ then-exps))))))
		((null?^ (cdr^ then-exps))
		 (k `(if ,test-exp ,(car^ then-exps) (cond ,@(at^ other-clauses)))))
		(else (k `(if ,test-exp (begin ,@(at^ then-exps)) (cond ,@(at^ other-clauses)))))))))))))

(define let*-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((bindings (cadr^ adatum))
	  (bodies (cddr^ adatum)))
      (nest-let*-bindings^ bindings bodies k))))

(define* nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null?^ bindings)
	    (null?^ (cdr^ bindings)))
	(k `(let ,bindings ,@(at^ bodies)))
	(nest-let*-bindings^ (cdr^ bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car^ bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (case-clauses->cond-clauses^ 'r clauses
	  (lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null?^ clauses)
      (k '())
      (case-clauses->simple-cond-clauses^ var (cdr^ clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car^ clauses)))
	    (cond
	      ((eq?^ (car^ clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol?^ (car^ clause))
	       (k (cons `((eq? ,var ',(car^ clause)) ,@(at^ (cdr^ clause))) new-clauses)))
	      (else (k (cons `((memq ,var ',(car^ clause)) ,@(at^ (cdr^ clause)))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((eq? ,var ',(car^ clause)) (,name)) new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((memq ,var ',(car^ clause)) (,name)) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((eq? (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((memq (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

;;----------------------------------------------------------------------------

(define define-datatype-transformer^
  (lambda-macro (datatype-def handler fail k)
    (let* ((datatype-name (cadr^ datatype-def))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ datatype-name) "?"))))
      (if (not (eq?^ (caddr^ datatype-def) type-tester-name))
	(amacro-error
	  (format "datatype tester predicate not named ~a" type-tester-name)
	  (caddr^ datatype-def) handler fail)
	(let ((variants (cdddr^ datatype-def)))
	  (make-dd-variant-constructors^ variants
	    (lambda-cont2 (variant-names constructor-defs)
	      (let ((tester-def
		      `(define ,type-tester-name
			 (lambda (x)
			   (and (pair? x) (not (not (memq (car x) ',variant-names))))))))
		(k `(begin ,tester-def ,@constructor-defs))))))))))

(define* make-dd-variant-constructors^
  (lambda (variants k2)
    (if (null?^ variants)
      (k2 '() '())
      (make-dd-variant-constructor^ (car^ variants)
	(lambda-cont2 (name def)
	  (make-dd-variant-constructors^ (cdr^ variants)
	    (lambda-cont2 (names defs)
	      (k2 (cons name names) (cons def defs)))))))))

(define* make-dd-variant-constructor^
  (lambda (variant k2)
    (let ((name (car^ variant))
	  (fields (cdr^ variant)))
      (verify-dd-constructor-fields^ name fields 'args
	(lambda-cont (verify-code)
	  (let ((constructor-def
		  `(define ,name
		     (lambda args
		       (if (= (length args) ,(length^ fields))
			   ,verify-code
			   (error ',name "wrong number of arguments"))))))
	    (k2 name constructor-def)))))))

(define* verify-dd-constructor-fields^
  (lambda (name fields cdrs k)
    (if (null?^ fields)
	(k `(cons ',name args))
	(verify-dd-constructor-fields^ name (cdr^ fields) `(cdr ,cdrs)
	  (lambda-cont (verify-code)
	    (k `(if (,(cadar^ fields) (car ,cdrs))
		    ,verify-code
		    (error ',name "~a is not of type ~a" (car ,cdrs) ',(cadar^ fields)))))))))

(define cases-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((type-name (cadr^ adatum))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ type-name) "?")))
	   (exp (caddr^ adatum))
	   (clauses (cdddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings)
		(if (not (,type-tester-name r))
		  (error 'cases "~a is not a valid ~a" r ',type-name)
		  (cond ,@new-clauses)))))))))

;;----------------------------------------------------------------------------

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'lambda 'λ 'trace-lambda 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'define-datatype 'cases)
      (list lambda-transformer^
	    lambda-transformer^
	    trace-lambda-transformer^
	    and-transformer^
	    or-transformer^
	    cond-transformer^
	    let-transformer^
	    letrec-transformer^
	    let*-transformer^
	    case-transformer^
	    record-case-transformer^
	    define-datatype-transformer^
	    cases-transformer^
	    )
      (list (string-append "(lambda ...) - lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(lambda ...) - lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(trace-lambda name ...) - trace-lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (trace-lambda name (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(and ...) - short-circuiting `and` macro\n"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (and)\n"
			   "    Out [1]: #t\n"
			   "    In  [2]: (and #t #f)\n"
			   "    Out [2]: #f\n"
			   )
	    (string-append "(or ...) - short-circuiting `or` macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (or)\n"
			   "    Out [1]: #f\n"
			   "    In  [2]: (or #t #f)\n"
			   "    Out [2]: #t\n"
			   )
	    (string-append "(cond (TEST RETURN)...) - conditional evaluation macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (cond ((= 1 2) 3)(else 4))\n"
			   "    Out [1]: 4\n"
			   )
	    (string-append "(let ((VAR VALUE)...)...) - local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (let ((x 3)) x)\n"
			   "    Out [1]: 3\n"
			   )
	    (string-append "(letrec ((VAR VALUE)...)...) - recursive local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [*]: (letrec ((loop (lambda () (loop)))) (loop))\n"
			   )
	    (string-append "(let* ((VAR VALUE)...)...) - cascading local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (let* ((a 1)(b a)(c b)) c)\n"
			   "    Out [1]: 1\n"
			   )
	    (string-append "(case THING (ITEM RETURN)...)) - case macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (case 1 (1 2)(3 4))\n"
			   "    Out [1]: 2\n"
			   )
	    (string-append "(record-case ) - record-case macro for define-datatype"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (record-case ddtype (subtype (part...) return)...)\n"
			   )
	    (string-append "(define-datatype NAME NAME? (TYPE (PART TEST))...) - defines new datatypes and support functions (macro)"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (define-datatype e e?)\n"
			   "    In  [1]: (e? 1)\n"
			   "    Out [1]: #f\n"
			   )
	    (string-append "(cases ...) - cases macro for a more flexible case"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (cases 1 ((1 2) 3))\n"
			   "    Out [1]: 3\n"
			   )
	    ))))

(define macro-env 'undefined)

;; pattern macros:

;; used in interpreter-cps.ss
(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define pattern-macro?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'pattern-macro))))

(define macro-clauses
  (lambda (macro)
    (cadr macro)))

(define macro-aclauses
  (lambda (macro)
    (caddr macro)))

(define define-syntax-clause?
  (lambda (x)
    (and (list? x)
	 (= (length x) 2)
	 (pattern? (car x))
	 (pattern? (cadr x)))))

(define define-syntax-clause?^
  (lambda (x)
    (and (list?^ x)
	 (= (length^ x) 2)
	 (apattern? (car^ x))
	 (apattern? (cadr^ x)))))

(define apattern?
  (lambda (x)
    (or (aatom? x)
	(and (apair? x)
	     (apattern? (cadr x))
	     (apattern? (caddr x))))))

(define list-of-define-syntax-clauses?^
  (lambda (alist)
    (or (null?^ alist)
	(and (define-syntax-clause?^ (car^ alist))
	     (list-of-define-syntax-clauses?^ (cdr^ alist))))))

(define* expand-once^
  (lambda (adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (let ((macro-keyword (untag-atom^ (car^ adatum))))
      (let ((macro (get-first-frame-value macro-keyword macro-env)))
	(if (pattern-macro? macro)
	    (process-macro-clauses^
	      (macro-clauses macro) (macro-aclauses macro) adatum handler fail
	      (lambda-cont2 (asexp fail)
		(k (replace-info asexp (snoc macro-keyword (get-source-info asexp))) fail)))
	    ;; macro transformer functions take 1-arg continuations:
	    (macro adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (if (has-source-info? expansion)
		      (k expansion fail)
		      (let ((info (get-source-info adatum)))
			(if (original-source-info? adatum)
			  (k (replace-info expansion (snoc macro-keyword info)) fail)
			  (k (replace-info expansion info) fail)))))))))))))

(define* process-macro-clauses^
  (lambda (clauses aclauses adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (if (null? clauses)
      (aparse-error "no matching clause found for" adatum handler fail)
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses))
	    (left-apattern (caar^ aclauses))
	    (right-apattern (cadar^ aclauses)))
	(unannotate-cps adatum
	  (lambda-cont (datum)
	    (unify-patterns^ left-pattern datum left-apattern adatum
	      (lambda-cont (subst)
		(if subst
		  (instantiate^ right-pattern subst right-apattern (lambda-cont2 (v av) (k av fail)))
		  (process-macro-clauses^ (cdr clauses) (cdr^ aclauses) adatum handler fail k))))))))))

;;--------------------------------------------------------------------------------------------
;; quasiquote expansion
;;
;; based on Appendix B of Alan Bawden's paper "Quasiquotation in Lisp", with some optimizations
;;
;; this version matches the functionality of Petite's quasiquote expander

;; for testing only
(define qqtest
  (lambda (s)
    (let ((adatum (aread-string s)))
      (if (not (and (list?^ adatum) (= (length^ adatum) 2) (eq?^ (car^ adatum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset))
	(qq-expand-cps (cadr^ adatum) 0
	  (lambda-cont (v)
	    (annotate-cps v 'none
	      (lambda-cont (expansion)
		(replace-info expansion (snoc 'quasiquote (get-source-info adatum)))))))))))

;; expands annotated code
(define* qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car^ ax) ,v)))))
	 ((and (unquote?^ ax) (not (null?^ (cdr^ ax))) (null?^ (cddr^ ax))) (k (cadr^ ax)))
	 (else (k `(quote ,ax))))) ;; illegal
      ((vector?^ ax)
       (annotate-cps (vector->list^ ax) 'none
	 (lambda-cont (v)
	   (qq-expand-cps v depth
	     (lambda-cont (v2)
	       (k `(list->vector ,v2)))))))
      ((not (pair?^ ax)) (k `',ax))
      ((null?^ (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands annotated code
(define* qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car^ ax) ,v))))))
	 ((unquote?^ ax) (k `(list . ,(cdr^ ax))))
	 ((null?^ (cddr^ ax)) (k (cadr^ ax)))
	 (else (k `(append . ,(cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair?^ ax)) (k `'(,ax)))
      ((null?^ (cdr^ ax))
       (qq-expand-list-cps (car^ ax) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;; expands unannotated code
(define* qq-expand-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car x) ,v)))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (k (cadr x)))
	 (else (k `(quote ,x))))) ;; illegal
      ((vector? x)
       (qq-expand-cps_ (vector->list x) depth
	 (lambda-cont (v)
	   (k `(list->vector ,v)))))
      ((not (pair? x)) (k `',x))
      ((null? (cdr x)) (qq-expand-list-cps_ (car x) depth k))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands unannotated code
(define* qq-expand-list-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car x) ,v))))))
	 ((unquote? x) (k `(list . ,(cdr x))))
	 ((null? (cddr x)) (k (cadr x)))
	 (else (k `(append . ,(cdr x))))))
      ((vector? x)
       (qq-expand-cps_ x depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair? x)) (k `'(,x)))
      ((null? (cdr x))
       (qq-expand-list-cps_ (car x) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(define aunparse
  (lambda (aexp)
    (cases aexpression aexp
      (lit-aexp (datum info)
	(cond
	  ((literal? datum) datum)
	  ((vector? datum) datum)
	  (else `(quote ,datum))))
      (var-aexp (id info) id)
      (lexical-address-aexp (depth offset id info) id)
      (if-aexp (test-aexp then-aexp else-aexp info)
	`(if ,(aunparse test-aexp) ,(aunparse then-aexp) ,(aunparse else-aexp)))
      (assign-aexp (var rhs-exp var-info info)
	`(set! ,var ,(aunparse rhs-exp)))
      (func-aexp (exp info)
	`(func ,(aunparse exp)))
      (callback-aexp (exp info)
	`(callback ,(aunparse exp)))
      (define-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define ,id ,(aunparse rhs-exp))
	  `(define ,id ,docstring ,(aunparse rhs-exp))))
      (define!-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define! ,id ,(aunparse rhs-exp))
	  `(define! ,id ,docstring ,(aunparse rhs-exp))))
      (define-syntax-aexp (name clauses aclauses info)
	`(define-syntax ,name ,@clauses))
      (begin-aexp (exps info)
	`(begin ,@(map aunparse exps)))
      (lambda-aexp (formals bodies info)
	`(lambda ,formals ,@(map aunparse bodies)))
      (mu-lambda-aexp (formals runt bodies info)
	`(lambda (,@formals . ,runt) ,@(map aunparse bodies)))
      (app-aexp (operator operands info)
	`(,(aunparse operator) ,@(map aunparse operands)))
      (try-catch-aexp (body catch-var catch-exps info)
	`(try ,(aunparse body) (catch ,catch-var ,@(map aunparse catch-exps))))
      (try-finally-aexp (body finally-exps info)
	`(try ,(aunparse body) (finally ,@(map aunparse finally-exps))))
      (try-catch-finally-aexp (body catch-var catch-exps finally-exps info)
	`(try ,(aunparse body)
	      (catch ,catch-var ,@(map aunparse catch-exps))
	      (finally ,@(map aunparse finally-exps))))
      (raise-aexp (exp info)
	`(raise ,(aunparse exp)))
      (choose-aexp (exps info)
	`(choose ,@(map aunparse exps)))
      (else (error 'aunparse "bad abstract syntax: ~s" aexp)))))

(define expand-macro
  (lambda (transformer sexp)
    (transformer sexp init-handler2 init-fail init-cont)))

;; will be overridden by toplevel-env definition in interpreter-cps.ss
;;(define toplevel-env (make-empty-environment))

(define aparse-string
  (lambda (string)
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (aread-datum string "stdin" init-handler2 init-fail
      (lambda-cont3 (adatum tokens-left fail)
	(aparse adatum (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

(define aparse-file
  (lambda (filename)
    (aget-parsed-sexps filename)))

(define aprint-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (aget-parsed-sexps filename))))

(define aget-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens filename (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

;;--------------------------------------------------------------------------------------------
;; not used - for possible future reference

;; based on Alan Bawden's paper "Quasiquotation in Lisp"

;; for testing only
(define qq1
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand1 (cadr datum)))))

;; non-CPS Appendix A algorithm, with some optimizations
(define qq-expand1
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1 (qq-expand1 (cadr x))))
      ((unquote? x) (cadr x))
      ((unquote-splicing? x) `(quote ,x))  ;; illegal
      ((pair? x)
       (cond
	 ((null? (cdr x)) (qq-expand1-list (car x)))
	 ((unquote? (car x)) `(cons ,(cadr (car x)) ,(qq-expand1 (cdr x))))
	 (else `(append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x))))))
      ((vector? x) `(list->vector ,(qq-expand1 (vector->list x))))
      (else `(quote ,x)))))

(define qq-expand1-list
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1-list (qq-expand1 (cadr x))))
      ((unquote? x) `(list ,(cadr x)))
      ((unquote-splicing? x) (cadr x))
      ((pair? x)
       (cond
	 ((null? (cdr x)) `(list ,(qq-expand1-list (car x))))
	 ((unquote? (car x)) `(list (cons ,(cadr (car x)) ,(qq-expand1 (cdr x)))))
	 (else `(list (append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x)))))))
      ((vector? x) `(list ,(qq-expand1 x)))
      (else `(quote (,x))))))

;; for testing only
(define qq2
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand2 (cadr datum) 0))))

;; non-CPS Appendix B algorithm, with some optimizations
(define qq-expand2
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr x))
	 (else `(quote ,x))))  ;; illegal
      ((vector? x) `(list->vector ,(qq-expand2 (vector->list x) depth)))
      ((not (pair? x)) `',x)
      ((null? (cdr x)) (qq-expand2-list (car x) depth))
      (else `(append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth))))))

(define qq-expand2-list
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(list (cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1)))))
	 ((unquote? x) `(list . ,(cdr x)))
	 ((null? (cddr x)) (cadr x))
	 (else `(append . ,(cdr x)))))
      ((vector? x) `(list ,(qq-expand2 x depth)))
      ((not (pair? x)) `'(,x))
      ((null? (cdr x)) `(list ,(qq-expand2-list (car x) depth)))
      (else `(list (append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth)))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1))))
;;	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr^ ax))
;;	 (else `(quote ,ax))))  ;; illegal
;;      ((vector? x) `(list->vector ,(qq-expand (vector->list x) (retag (vector->list^ ax) 'none) depth)))
;;      ((not (pair? x)) `',ax)
;;      ((null? (cdr x)) (qq-expand-list (car x) (car^ ax) depth))
;;      (else `(append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand-list
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1)))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(list (cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1)))))
;;	 ((unquote? x) `(list . ,(^cdr^ ax)))
;;	 ((null? (cddr x)) (cadr^ ax))
;;	 (else `(append . ,(^cdr^ ax)))))
;;      ((vector? x) `(list ,(qq-expand x ax depth)))
;;      ((not (pair? x)) `'(,ax))
;;      ((null? (cdr x)) `(list ,(qq-expand-list (car x) (car^ ax) depth)))
;;      (else `(list (append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth)))))))

;;--------------------------------------------------------------------------------------------
;; my version - not correct

;;(define* expand-quasiquote
;;  (lambda (adatum k)
;;    (cond
;;      ((vector?^ adatum)
;;       (expand-quasiquote^ (vector->list^ adatum)
;;	 (lambda-cont (ls) (k `(list->vector ,ls)))))
;;      ((not (pair?^ adatum)) (k `(quote ,adatum)))
;;      ;; doesn't handle nested quasiquotes yet
;;      ((quasiquote?^ adatum) (k `(quote ,adatum)))
;;      ((unquote?^ adatum) (k (cadr^ adatum)))
;;      ((unquote-splicing? (car^ adatum))
;;       (if (null? (cdr^ adatum))
;;	 (k (cadr^ (car^ adatum)))
;;	 (expand-quasiquote^ (^cdr^ adatum)
;;	   (lambda-cont (v) (k `(append ,(cadr^ (car^ adatum)) ,v))))))
;;      ((quasiquote-list?^ adatum)
;;       (expand-quasiquote-list^ (get-sexp adatum)
;;	 (lambda-cont (v)
;;	   (k `(list ,@v)))))
;;      (else
;;	(expand-quasiquote^ (car^ adatum)
;;	  (lambda-cont (v1)
;;	    (expand-quasiquote^ (^cdr^ adatum)
;;	      (lambda-cont (v2)
;;		(k `(cons ,v1 ,v2))))))))))

;;(define* expand-quasiquote-list^  ;; maps expand-quasiquote to an arbitrary flat list
;;  (lambda (asexps k)
;;    (if (null? asexps)
;;      (k '())
;;      (expand-quasiquote^ (car asexps)
;;	(lambda-cont (v1)
;;	  (expand-quasiquote-list^ (cdr asexps)
;;	    (lambda-cont (v2)
;;	      (k (cons v1 v2)))))))))

;;(define quasiquote-list?^
;;  (lambda (adatum)
;;    (or (null?^ adatum)
;;	(and (pair?^ adatum)
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ adatum))
;;	     (not (unquote?^ adatum))
;;	     (not (unquote-splicing?^ adatum))
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ (car^ adatum)))
;;	     (not (unquote-splicing?^ (car^ adatum)))
;;	     (quasiquote-list?^ (^cdr^ adatum))))))

;;--------------------------------------------------------------------------------------------
;; temporary - stuff for testing

;;(define a 'apple)
;;(define b 'banana)
;;(define c '(cherry orange))
;;(define m 2)
;;(define n 3)
;;(define abc '(a b c))

;;;;(define parseexp (lambda (e) (parse e init-handler2 init-fail init-cont2)))

;;;(define-syntax for ((for ?exp times do . ?bodies) (for-repeat ?exp (lambda () . ?bodies))))

;;(define for-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "for-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'for-macro "huh?")))))

;;(define collect-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "collect-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'collect-macro "huh?")))))

;;(define for-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "for-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'for-macro^ "huh?")))))

;;(define collect-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "collect-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'collect-macro^ "huh?")))))

;;(define make-macro-env
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'for 'collect)
;;      (list and-transformer
;;	    or-transformer
;;	    cond-transformer
;;	    let-transformer
;;	    letrec-transformer
;;	    let*-transformer
;;	    case-transformer
;;	    record-case-transformer
;;	    (for-macro)      ;; for testing only
;;	    (collect-macro)  ;; for testing only
;;))))

;;(define make-macro-env^
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case) ;; 'for 'collect)
;;      (list and-transformer^
;;	    or-transformer^
;;	    cond-transformer^
;;	    let-transformer^
;;	    letrec-transformer^
;;	    let*-transformer^
;;	    case-transformer^
;;	    record-case-transformer^
;;;;	    (for-macro^)      ;; for testing only
;;;;	    (collect-macro^)  ;; for testing only
;;))))

;;(define macro-env (make-macro-env^))

;;;;(define macro-env 'undefined)

;;;;(define pmacro (lookup-value 'for macro-env init-handler2 init-fail init-cont2))

;;(define check
;;    (lambda (f)
;;      (let ((exps #f) (aexps #f))
;;        (set! macro-env (make-macro-env))
;;        (set! exps (parse-file f))
;;        (set! macro-env (make-macro-env^))
;;        (set! aexps (aparse-file f))
;;        (equal? (map unparse exps) (map aunparse aexps)))))
