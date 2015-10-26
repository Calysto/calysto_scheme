;; This program automatically converts CPS code with procedural
;; representations of continuations and closures into first-order
;; tail form.
;;
;; Written by James B. Marshall
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall

(case-sensitive #t)

(load "parser-cps.ss")

;; default transformer settings
(define *include-eopl-define-datatype-in-ds-code?* #t)
(define *include-define*-in-ds-code?* #t)
(define *generate-apply-functions-with-record-case-code?* #f)

;; assumes:
;; - lambda-cont, lambda-proc, etc. datatype forms
;; - define*
;; - no internal define/define*'s

;; assumes all definitions are in indiana style, not mit style
(define ignore-definition?
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (member (car x) '(define define* define+ define-native define-datatype))
	 (member (cadr x) *ignore-definitions*))))

;; these definitions will not be included in the transformed output code
(define *ignore-definitions*
  '(Main
    ;; unannotated functions and datatypes
    qqtest qq-expand-cps_ qq-expand-list-cps_ qq1 qq-expand1 qq-expand1-list qq2 qq-expand2 qq-expand2-list
    expand-macro aparse-string aparse-file aprint-parsed-sexps aget-parsed-sexps
    scan-string scan-file aread-string aread-datum aread-file aread-file-loop
    annotate unannotate up aup print-sub
    ))

;;-------------------------------------------------------------------------------
;; recognized datatypes

(define make-all-datatypes
  (lambda ()
    (list
      (make-datatype 'continuation 'cont '((k k2 REP-k) value))
      (make-datatype 'continuation2 'cont2 '((k k2 REP-k gk sk) value1 value2))
      (make-datatype 'continuation3 'cont3 '((k k2 dk) value1 value2 value3))
      (make-datatype 'continuation4 'cont4 '((k k2) value1 value2 value3 value4))
      (make-datatype 'fail-continuation 'fail '((fail)))
      (make-datatype 'handler 'handler '((handler) exception))
      (make-datatype 'handler2 'handler2 '((handler) exception fail))
      (make-datatype 'procedure 'proc '((proc) args env2 info handler fail k2))
      (make-datatype 'floopproc 'fproc '((proc) args k-env k2))
      (make-datatype 'macro-transformer 'macro '((macro mit-define-transformer mit-define-transformer^) datum handler fail k))
      )))

;; determines the order of the free variables in data-structure records
(define record-field-order '(formals runt body env env2 k-env info handler fail k k2))

;;-------------------------------------------------------------------------------
;; (make-datatype <long-name> <short-name> (<app-symbols> <arg1> <arg2> ...))
;;
;; <long-name> is the type tag for objects of this datatype.
;;
;; <short-name> is the basis for the record tags, the lambda form, and
;; the name of the apply function for objects of this datatype.
;;
;; <app-symbols> is a list of all symbols used to refer to objects of
;; this datatype in application expressions in the source code.  The
;; first symbol in the list is used as the formal parameter for the
;; datatype object in the apply function.
;;
;; <argN> are the names of the other formal parameters in the apply function.
;;
;;-------------------------------------------------------------------------------
;; Example:
;; (define record-field-order '(x y z))
;; (make-datatype 'continuation2 'cont2 '((k k2 REP-k) value1 value2))
;; generates the following code and code transformations:
;;
;; (define make-cont2
;;   (lambda args
;;     (cons 'continuation2 args)))
;;
;; (lambda-cont2 (a b) ...y x...)    transforms to  (make-cont2 '<cont2-1> x y)
;; (lambda-cont2 (c d) ...z x y...)  transforms to  (make-cont2 '<cont2-2> x y z)
;;
;; (define* apply-cont2
;;   (lambda (k value1 value2)
;;     (record-case (cdr k)
;;       (<cont2-1> (x y) ...)
;;       (<cont2-2> (x y z) ...)
;;       ...)))
;;
;; (k 1 2)      transforms to  (apply-cont2 k 1 2)
;; (k2 a b)     transforms to  (apply-cont2 k2 a b)
;; (REP-k c d)  transforms to  (apply-cont2 REP-k c d)
;;
;;-------------------------------------------------------------------------------

(define need-eopl-support? #f)

(define default-top-level-symbols
  (filter top-level-bound? (environment-symbols (scheme-environment))))

(define syntactic-keywords
  (filter (lambda (x) (not (top-level-bound? x)))
	  (environment-symbols (scheme-environment))))

(define get-global-symbols
  (lambda (source-file)
    (call-with-input-file source-file
      (lambda (port)
	(let loop ((exp (read port)) (vars '()))
	  (cond
	    ((eof-object? exp) vars)
	    ((or (define? exp) (define*? exp))
	     (if (mit-style-define? exp)
		 (loop (read port) (union (caadr exp) vars))
		 (loop (read port) (union (cadr exp) vars))))
	    ((eopl-define-datatype? exp)
	     (loop (read port) (union (map car (cdddr exp)) vars)))
	    (else (loop (read port) vars))))))))

(define get-all-global-symbols
  (lambda (source-files)
    (union-all (map get-global-symbols source-files))))

(define make-datatype
  (lambda (type abbreviation application-template)
    (let ((lambda-name (string->symbol (format "lambda-~a" abbreviation)))
	  (make-name (string->symbol (format "make-~a" abbreviation)))
	  (apply-name (string->symbol (format "apply-~a" abbreviation)))
	  (obj-name (if (list? (car application-template))
			(caar application-template)
			(car application-template)))
	  (arg-names (cdr application-template))
	  (all-obj-names (if (list? (car application-template))
			     (car application-template)
			     (list (car application-template))))
	  (clauses '())
	  (counter 0))
      (lambda msg
	(record-case msg
	  (info ()
	    (let ((tag (string->symbol (format "<~a-N>" abbreviation))))
	      (printf "type: ~a~%" type)
	      (printf "(~a '~a ...)~%" make-name tag)
	      (printf "~a~%" `(,apply-name ,all-obj-names ,@arg-names))))
	  (get-type () type)
	  (get-make-name () make-name)
	  (get-apply-name () apply-name)
	  (reset ()
	    (set! clauses '())
	    (set! counter 0)
	    'ok)
	  (datatype-lambda? (code)
	    (eq? (car code) lambda-name))
	  (datatype-application? (code)
	    (or (and (symbol? (car code))
		     (member (car code) all-obj-names)
		     (= (length (cdr code)) (length arg-names)))
		(and (list? (car code))
		     (not (null? (car code)))
		     (symbol? (caar code))
		     (eq? (caar code) make-name))))
	  (add-clause (formals bodies fields params)
	    (let* ((new-lambda
		     (rename-lambda-formals
		       arg-names
		       `(lambda ,formals ,@(map (transform (union formals params)) bodies))
		       type))
		   (new-code (cddr new-lambda))
		   (identical-clause (find-clause fields new-code clauses)))
	      (if identical-clause
		(car identical-clause)
		(begin
		  (set! counter (+ counter 1))
		  (let* ((new-tag (string->symbol (format "<~a-~a>" abbreviation counter)))
			 (new-clause `(,new-tag ,fields ,@new-code)))
		    (set! clauses (cons new-clause clauses))
		    new-tag)))))
	  (print-code port
	    (if (null? clauses)
	      `(,type datatype contains no code)
	      (let* ((output-port (if (null? port) (current-output-port) (car port)))
		     (error-string (format "bad ~a: ~~a" type))
		     (define-sym (if *include-define*-in-ds-code?* 'define* 'define))
 		     (apply-function-code
		      (if *generate-apply-functions-with-record-case-code?*
 		       `(,define-sym ,apply-name
 			  (lambda (,obj-name ,@arg-names)
			    (record-case (cdr ,obj-name)
			      ,@(reverse clauses)
			      (else (error (quote ,apply-name) (format ,error-string ,obj-name))))))
;;			    (if (pair? ,obj-name)
;;				(record-case (cdr ,obj-name)
;;				    ,@(reverse clauses)
;;				    (else (error (quote ,apply-name) (format ,error-string ,obj-name))))
;;				(error (quote ,apply-name) (format "invalid procedure: ~a" ,obj-name)))))
		       `(,define-sym ,apply-name
			  (lambda (,obj-name ,@arg-names)
			    (apply+ (cadr ,obj-name) ,@arg-names (cddr ,obj-name))))))
 		     (make-function-code
		       ;; must use define, not define* here
		      `(define ,make-name
			 (lambda args (cons (quote ,type) args)))))
		(fprintf output-port ";;~a~%" (make-string 70 #\-))
		(fprintf output-port ";; ~a datatype~%~%" type)
		(pretty-print make-function-code output-port)
		(newline output-port)
		(pretty-print apply-function-code output-port)
		(newline output-port)
		(if (not *generate-apply-functions-with-record-case-code?*)
		  (for-each
		    (lambda (clause) 
		      (pretty-print (make-obj-function arg-names clause) output-port)
		      (newline output-port))
		    (reverse clauses))))))
	  (else (error 'datatype (format "bad message: ~a" msg))))))))

;;------------------------------------------------------------------------
;; continuation records look like this if
;; *generate-apply-functions-with-record-case-code?* is true:
;; (continuation <tag> args sym handler k ...etc... )
;; or this if it is false:
;; (continuation <tag> <func> args sym handler k ...etc... )

(define make-obj-function
  (lambda (arg-names clause)
    (let ((name (car clause))
	  (fields (cadr clause)))
      `(define+ ,name (lambda (,@arg-names fields)
			(let ,(make-let-bindings fields 0)
			  ,@(cddr clause)))))))

(define make-let-bindings
  (lambda (fields pos)
    (if (null? fields)
      '()
      (let ((first-binding
	      (cond
		((= pos 0) `(,(car fields) (car fields)))
		((= pos 1) `(,(car fields) (cadr fields)))
		((= pos 2) `(,(car fields) (caddr fields)))
		((= pos 3) `(,(car fields) (cadddr fields)))
		(else `(,(car fields) (list-ref fields ,pos))))))
	(cons first-binding (make-let-bindings (cdr fields) (+ pos 1)))))))

;; define+ means "don't registerize this function's parameters or let,
;; but do transform the body.  apply+ means "don't registerize this
;; function call".  examples:
;;
;;(define+ <cont-79>
;;  (lambda (value fields)
;;    (let ((args (list-ref fields 0))
;;	  (sym (list-ref fields 1))
;;	  (handler (list-ref fields 2))
;;	  (k (list-ref fields 3)))
;;      (cond
;;       ((null? (cdr args)) (apply-cont k value))
;;       ((not (environment? value))
;;	(apply-handler handler (format "~a is not a module" sym)))
;;       (else (get-primitive (cdr args) value handler k))))))
;;
;; data-structure versions:
;;
;;(define* make-cont (lambda args (cons 'continuation args)))
;;
;;(define* apply-cont
;;  (lambda (k value)
;;    (apply+ (cadr k) value (cddr k))))
;;
;;(define* apply-cont2
;;  (lambda (k2 value1 value2)
;;    (apply+ (cadr k2) value1 value2 (cddr k2))))
;;
;;;; registerized versions:
;;
;;(define* apply-cont
;;  (lambda ()
;;    ((cadr k_reg) value_reg (cddr k_reg))))
;;
;;(define* apply-cont2
;;  (lambda ()
;;    ((cadr k2_reg) value1_reg value2_reg (cddr k2_reg))))
;;------------------------------------------------------------------------

(define find-clause
  (lambda (fields code clauses)
    (cond
      ((null? clauses) #f)
      ((and (equal? (cadar clauses) fields) (equal? (cddar clauses) code))
       (car clauses))
      (else (find-clause fields code (cdr clauses))))))

(define ds-transform-file
  (lambda (source-file . opts)
    (printf "ds-transform: transforming ~s~%" source-file)
    (set! all-datatypes (make-all-datatypes))
    (set! need-eopl-support? #f)
    ;; check for name conflicts
    (let ((globally-defined-symbols (get-global-symbols source-file)))
      (for-each
	(lambda (dt)
	  (let ((type (dt 'get-type))
		(make-name (dt 'get-make-name))
		(apply-name (dt 'get-apply-name)))
	    (if (member make-name globally-defined-symbols)
	      (error-in-source #f
		(format "Cannot create ~a datatype because ~a is already defined in ~a"
			type make-name source-file)))
	    (if (member apply-name globally-defined-symbols)
	      (error-in-source #f
		(format "Cannot create ~a datatype because ~a is already defined in ~a"
			type apply-name source-file)))
	    (if (member make-name default-top-level-symbols)
	      (printf "~%*** Warning: ~a datatype will redefine top-level symbol ~a~%"
			type make-name))
	    (if (member apply-name default-top-level-symbols)
	      (printf "~%*** Warning: ~a datatype will redefine top-level symbol ~a~%"
			type apply-name))))
	all-datatypes))
    (let ((eopl-defs '())
	  (function-defs '())
	  (other-defs '()))
      (letrec
	((read-transformed-exps
	   (lambda (input-port)
	     (let ((exp (read input-port)))
	       (cond
		 ((eof-object? exp)
		  (set! eopl-defs (reverse eopl-defs))
		  (set! function-defs (reverse function-defs))
		  (set! other-defs (reverse other-defs)))
		 ;; ignore top level calls to load
		 ((load? exp)
		  (read-transformed-exps input-port))
		 ((ignore-definition? exp)
		  (printf "skipping ~a definition\n" (cadr exp))
		  (read-transformed-exps input-port))
		 ;; must transform the expression before calling the recursion
		 (else (let ((texp ((transform '()) exp)))
			 (cond
			   ((eopl-define-datatype? texp)
			    (set! need-eopl-support? #t)
			    (set! eopl-defs (cons texp eopl-defs)))
			   ((function-definition? texp)
			    (set! function-defs (cons texp function-defs)))
			   (else (set! other-defs (cons texp other-defs))))
			 (read-transformed-exps input-port)))))))
	 (print-exp
	   (lambda (output-port)
	     (lambda (exp)
	       (pretty-print exp output-port)
	       (newline output-port))))
	 (print-code
	   (lambda (output-port)
	     (if *include-define*-in-ds-code?*
		 (fprintf output-port "(load \"transformer-macros.ss\")~%~%"))
	     ;; did we see a define-datatype or cases form?
	     (if need-eopl-support?
	       (begin
		 (fprintf output-port ";;~a~%" (make-string 70 #\-))
		 (fprintf output-port ";; EOPL support~%~%")
		 (if *include-eopl-define-datatype-in-ds-code?*
		   (begin
		     (fprintf output-port "(load \"petite-init.ss\")~%")
		     (fprintf output-port "(load \"define-datatype.ss\")~%~%")
		     (for-each (print-exp output-port) eopl-defs))
		   (begin
		     (for-each
		       (lambda (dd)
			 (fprintf output-port ";; ~a datatype~%" (cadr dd))
			 (for-each
			   (lambda (def) (pretty-print def output-port))
			   (expand-eopl-define-datatype dd))
			 (newline output-port))
		       eopl-defs)))))
	     ;; write datatype code
	     (for-each
	       (lambda (dt) (dt 'print-code output-port))
	       all-datatypes)
	     ;; write representation independent code
	     (fprintf output-port ";;~a~%" (make-string 70 #\-))
	     (fprintf output-port ";; main program~%~%")
	     (for-each (print-exp output-port) function-defs)
	     (for-each (print-exp output-port) other-defs))))
	(call-with-input-file source-file
	  (lambda (input-port)
	    (read-transformed-exps input-port)))
	(if (null? opts)
	  (begin
	    (newline)
	    (print-code (current-output-port)))
	  (let ((output-filename (car opts)))
	    (call-with-output-file output-filename
	      (lambda (output-port)
		(print-code output-port)
		(printf "Output written to ~a~%" output-filename)))))))))

(define eopl-define-datatype?
  (lambda (exp)
    (and (list? exp)
	 (not (null? exp))
	 (eq? (car exp) 'define-datatype))))

(define expand-eopl-define-datatype
  (lambda (def)
    (map (lambda (name) `(define ,name (lambda args (cons ',name args))))
	 (map car (cdddr def)))))

(define function-definition?
  (lambda (exp)
    (and (or (define? exp)
	     (define*? exp))
	 (or (mit-style-define? exp)
	     (lambda? (caddr exp))))))

(define special-form?
  (lambda (keyword op len)
    (lambda (sexp)
      (and (list? sexp)
	   (op (length sexp) len)
	   (symbol? (car sexp))
	   (eq? (car sexp) keyword)))))

(define mit-style-define?
  (lambda (sexp)
    (not (symbol? (cadr sexp)))))

(define define? (special-form? 'define >= 3))
(define define*? (special-form? 'define* >= 3))
(define define+? (special-form? 'define+ >= 3))
(define define-native? (special-form? 'define-native >= 3))
(define lambda? (special-form? 'lambda >= 3))
(define quasiquote? (special-form? 'quasiquote = 2))
(define unquote? (special-form? 'unquote >= 2))
(define unquote-splicing? (special-form? 'unquote-splicing >= 2))
(define begin? (special-form? 'begin >= 2))

(define mit-define->define
  (lambda (def)
    ;; could be a define or define*
    (let ((name (caadr def))
	  (formals (cdadr def))
	  (bodies (cddr def)))
      `(,(car def) ,name (lambda ,formals ,@bodies)))))

(define load?
  (lambda (x)
    (and (list? x)
	 (not (null? x))
	 (eq? (car x) 'load))))

(define datatype-lambda?
  (lambda (code)
    (and (list? code)
	 (>= (length code) 3)
	 (symbol? (car code))
	 (ormap (lambda (dt) (dt 'datatype-lambda? code))
		all-datatypes))))

(define datatype-application?
  (lambda (code)
    (and (list? code)
	 (ormap (lambda (dt) (dt 'datatype-application? code))
		all-datatypes))))

(define get-datatype
  (lambda (msg code datatypes)
    (if ((car datatypes) msg code)
      (car datatypes)
      (get-datatype msg code (cdr datatypes)))))

(define transform
  (lambda (params)
    (lambda (code)
      (cond
        ((null? code) code)
	((literal? code) code)
	((vector? code) code)
	((symbol? code) code)
	((datatype-lambda? code)
	 (let* ((dt (get-datatype 'datatype-lambda? code all-datatypes))
		(formals (cadr code))
		(bodies (cddr code))
		(fields (sort-fields
			  (filter
			    ;; remove any vars that are not lexically bound
			    (lambda (var) (member var params))
			    (free code '()))
			  record-field-order))
		(tag (dt 'add-clause formals bodies fields params))
		(make-name (dt 'get-make-name)))
	   (if *generate-apply-functions-with-record-case-code?*
	       `(,make-name (quote ,tag) ,@fields)
	       `(,make-name ,tag ,@fields))))
	((datatype-application? code)
	 (let ((dt (get-datatype 'datatype-application? code all-datatypes)))
	   ((transform params) (cons (dt 'get-apply-name) code))))
	(else
	  (record-case code
	    (quote (datum) code)
	    (quasiquote (datum)
	      (list 'quasiquote (transform-quasiquote datum params)))
	    (if (test . conseqs)
	      `(if ,@(map (transform params) (cdr code))))
	    (cond clauses
	      `(cond ,@(map (transform-cond-clause params) clauses)))
	    (lambda (formals . bodies)
	      `(lambda ,formals ,@(map (transform (union formals params)) bodies)))
	    (let (bindings . bodies)
	      (if (symbol? bindings)
		;; named let
		(let ((name (cadr code))
		      (vars (map car (caddr code)))
		      (exps (map cadr (caddr code)))
		      (bodies (cdddr code)))
		  `(let ,name ,(map list vars (map (transform params) exps))
		     ,@(map (transform (union name (union vars params))) bodies)))
		;; ordinary let
		(let ((vars (map car bindings))
		      (exps (map cadr bindings)))
		  `(let ,(map list vars (map (transform params) exps))
		     ,@(map (transform (union vars params)) bodies)))))
	    (let* (bindings . bodies)
	      (let ((vars (map car bindings))
		    (exps (map cadr bindings)))
		`(let* ,(transform-let*-bindings vars exps params)
		   ,@(map (transform (union vars params)) bodies))))
	    (letrec (decls . bodies)
	      (let ((vars (map car decls))
		    (procs (map cadr decls)))
		`(letrec ,(map list vars (map (transform (union vars params)) procs))
		   ,@(map (transform (union vars params)) bodies))))
	    (set! (var rhs-exp)
	      `(set! ,var ,((transform params) rhs-exp)))
	    (begin exps
	      `(begin ,@(map (transform params) exps)))
	    ((define define*) (name . bodies)
	     (cond
	       ((mit-style-define? code)
		((transform params) (mit-define->define code)))
	       (*include-define*-in-ds-code?*
		`(,(car code) ,name ,((transform params) (car bodies))))
	       (else `(define ,name ,((transform params) (car bodies))))))
	    (define-syntax args code)
	    (and exps
	      `(and ,@(map (transform params) exps)))
	    (or exps
	      `(or ,@(map (transform params) exps)))
	    (case (exp . clauses)
	      `(case ,((transform params) exp)
		 ,@(transform-case-clauses clauses params)))
	    (record-case (exp . clauses)
	      `(record-case ,((transform params) exp)
		 ,@(transform-record-case-clauses clauses params)))
	    ;; EOPL
	    (define-datatype args code)
	    (cases (type exp . clauses)
	      (set! need-eopl-support? #t)
	      (if *include-eopl-define-datatype-in-ds-code?*
		`(cases ,type ,((transform params) exp)
		   ,@(transform-record-case-clauses clauses params))
		((transform params) `(record-case ,exp ,@clauses))))
	    (halt* (value)
	      (if *include-define*-in-ds-code?*
		`(halt* ,((transform params) value))
		((transform params) value)))
	    (else (if (member (car code) syntactic-keywords)
		    (error-in-source code
		      "I don't know how to process the above code.")
		    (map (transform params) code)))))))))
  
(define transform-let*-bindings
  (lambda (vars exps params)
    (if (null? vars)
      '()
      (let ((var (car vars))
	    (exp (car exps)))
	(cons (list var ((transform params) exp))
	      (transform-let*-bindings (cdr vars) (cdr exps) (union var params)))))))

(define transform-cond-clause
  (lambda (params)
    (lambda (clause)
      (if (eq? (car clause) 'else)
	  `(else ,@(map (transform params) (cdr clause)))
	  (map (transform params) clause)))))

(define transform-quasiquote
  (lambda (datum params)
    (cond
      ((vector? datum) (list->vector (transform-quasiquote (vector->list datum) params)))
      ((not (pair? datum)) datum)
      ;; doesn't handle nested quasiquotes yet
      ((quasiquote? datum) datum)
      ((unquote? datum) (list 'unquote ((transform params) (cadr datum))))
      ((unquote-splicing? (car datum))
       (let* ((uqs (car datum))
	      (transformed-uqs (list 'unquote-splicing ((transform params) (cadr uqs)))))
	 (if (null? (cdr datum))
	     (cons transformed-uqs '())
	     (cons transformed-uqs (transform-quasiquote (cdr datum) params)))))
      (else
	(cons (transform-quasiquote (car datum) params)
	      (transform-quasiquote (cdr datum) params))))))

(define transform-record-case-clauses
  (lambda (clauses params)
    (map (lambda (clause)
	   (if (eq? (car clause) 'else)
	     `(else ,@(map (transform params) (cdr clause)))
	     (let ((tags (car clause))
		   (formals (cadr clause))
		   (bodies (cddr clause)))
	       `(,tags ,formals ,@(map (transform (union formals params)) bodies)))))
	 clauses)))

(define transform-case-clauses
  (lambda (clauses params)
    (map (lambda (clause) (cons (car clause) (map (transform params) (cdr clause))))
	 clauses)))

(define sort-fields
  (lambda (fields field-order)
    (letrec
	((pos (lambda (x ls)
		(if (eq? (car ls) x)
		  0
		  (+ 1 (pos x (cdr ls)))))))
      (sort (lambda (f1 f2)
	      (let ((f1-mem (member f1 field-order))
		    (f2-mem (member f2 field-order)))
		(cond
		  ((and f1-mem f2-mem) (< (pos f1 field-order) (pos f2 field-order)))
		  ((and (not f1-mem) f2-mem) #t)
		  ((and f1-mem (not f2-mem)) #f)
		  (else (alphabetical? f1 f2)))))
	    fields))))

(define alphabetical?
  (lambda (sym1 sym2)
    (string<? (symbol->string sym1) (symbol->string sym2))))

(define error-in-source
  (lambda (code . messages)
    (printf "~%*** ERROR:~%~%")
    (if code
      (begin
	(pretty-print code)
	(newline)))
    (for-each (lambda (m) (printf "~a~%" m)) messages)
    (newline)
    (reset)))

;;-------------------------------------------------------------------------------
;; free variables

(define free
  (lambda (code params)
    (cond
      ((null? code) '())
      ((literal? code) '())
      ((vector? code) '())
      ((datatype-lambda? code) (all-free (cddr code) (union (cadr code) params)))
      ((symbol? code) (if (member code params) '() (list code)))
      (else
	(record-case code
	  (quote (datum) '())
	  (quasiquote (datum) (free-in-quasiquote datum params))
	  (if (test . conseqs) (all-free (cdr code) params))
	  (cond clauses
	    (union-all (map (lambda (clause)
			      (if (eq? (car clause) 'else)
				(all-free (cdr clause) params)
				(all-free clause params)))
			    clauses)))
	  (lambda (formals . bodies) (all-free bodies (union formals params)))
	  (let (bindings . bodies) (free (let-transformer code (lambda (v) v)) params))
	  (let* (bindings . bodies) (free (let*-transformer code (lambda (v) v)) params))
	  (letrec (decls . bodies) (free (letrec-transformer code (lambda (v) v)) params))
	  (set! (var rhs-exp) (union var (free rhs-exp params)))
	  (begin exps (all-free exps params))
	  ((define define*) (name . bodies)
	    (if (mit-style-define? code)
	      (all-free bodies (union name params))
	      (free (car bodies) (union name params))))
	  (define-syntax args '())
	  (and exps (all-free exps params))
	  (or exps (all-free exps params))
	  (case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (lambda (clause) (all-free (cdr clause) params))
				   clauses))))
	  (record-case (exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-record-case-clause params) clauses))))
	  ;; EOPL
	  (define-datatype args '())
	  (cases (type exp . clauses)
	    (union (free exp params)
		   (union-all (map (free-in-record-case-clause params) clauses))))
	  (else (if (member (car code) syntactic-keywords)
		  (error 'free (format "don't know how to process ~a" code))
		  (all-free code params))))))))

(define all-free
  (lambda (exps params)
    (union-all (map (lambda (e) (free e params)) exps))))

(define free-in-quasiquote
  (lambda (datum params)
    (cond
     ((vector? datum) (free-in-quasiquote (vector->list datum) params))
     ((not (pair? datum)) '())
     ;; doesn't handle nested quasiquotes yet
     ((quasiquote? datum) '())
     ((unquote? datum) (free (cadr datum) params))
     ((unquote-splicing? datum) (free (cadr datum) params))
     (else (union (free-in-quasiquote (car datum) params)
		  (free-in-quasiquote (cdr datum) params))))))
  
(define free-in-record-case-clause
  (lambda (params)
    (lambda (clause)
      (if (eq? (car clause) 'else)
	(all-free (cdr clause) params)
	(let ((formals (cadr clause))
	      (bodies (cddr clause)))
	  (all-free bodies (union formals params)))))))

;; s1 can be an improper list or a symbol and s2 can be a symbol
(define union
  (lambda (s1 s2)
    (cond
      ((symbol? s2) (union s1 (list s2)))
      ((null? s1) s2)
      ((symbol? s1) (if (member s1 s2) s2 (cons s1 s2)))
      ((member (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2))))))

(define union-all
  (lambda (sets)
    (if (null? sets)
      '()
      (union (car sets) (union-all (cdr sets))))))

;; ls can be an improper list or a symbol
(define mem?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((symbol? ls) (eq? x ls))
      ((eq? (car ls) x) #t)
      (else (mem? x (cdr ls))))))

;; ls can be an improper list or a symbol
(define subst
  (lambda (old new ls)
    (cond
      ((null? ls) '())
      ((symbol? ls) (if (eq? ls old) new ls))
      ((eq? (car ls) old) (cons new (cdr ls)))
      (else (cons (car ls) (subst old new (cdr ls)))))))

;; ls1 and ls2 can be improper lists or symbols
(define lengths=?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((or (null? ls1) (null? ls2)) #f)
      ((and (symbol? ls1) (symbol? ls2)) #t)
      ((or (symbol? ls1) (symbol? ls2)) #f)
      (else (lengths=? (cdr ls1) (cdr ls2))))))

(define rename-lambda-formals
  (lambda (new-formals lambda-exp . opt)
    (let ((formals (cadr lambda-exp))
	  (type (if (null? opt) "" (format " ~a" (car opt)))))
      ;; formals and new-formals can be improper lists
      (if (not (lengths=? formals new-formals))
	  (error-in-source lambda-exp
	    (format "Lambda parameters are not compatible with~a datatype parameters ~a."
		    type new-formals))
	  (letrec
	    ((loop
	       (lambda (old new exp)
		 (cond
		   ((null? old) exp)
		   ((symbol? old) (rename-formal old new exp))
		   (else (let ((new-exp (rename-formal (car old) (car new) exp)))
			   (loop (cdr old) (cdr new) new-exp)))))))
	    (loop formals new-formals lambda-exp))))))

(define rename-formal
  (lambda (old-var new-var lambda-exp)
    (let ((formals (cadr lambda-exp))
	  (bodies (cddr lambda-exp)))
      (cond
	((not (mem? old-var formals))
	 (error-in-source lambda-exp
	   (format "~a is not a parameter in the above lambda expression." old-var)))
	((eq? new-var old-var) lambda-exp)
	((mem? new-var formals)
	 (error-in-source lambda-exp
	   (format "~a is already a parameter in the above lambda expression." new-var)))
	((member new-var (all-free bodies '()))
	 (error-in-source lambda-exp
	   (format "Cannot safely rename ~a to ~a in the above lambda expression" old-var new-var)
	   (format "because ~a already occurs free within its body." new-var)))
	((ormap (contains-unsafe-lambda? old-var new-var) bodies)
	 (error-in-source lambda-exp
	   (format "Cannot safely rename ~a to ~a in the above lambda expression" old-var new-var)
	   (format "because it would be captured by an inner binding.")))
	(else `(lambda ,(subst old-var new-var formals)
		 ,@(map (rename-free-occurrences old-var new-var)
			bodies)))))))

;; some tests:
;; ok:
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'x 'z '(lambda (x) (y (lambda (z x) (x z)))))
;; (rename-formal 'a 'foo '(lambda (a) (let loop ((a loop) (b a)) (loop a b c))))
;; (rename-formal 'list 'cdr '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'z 'c '(lambda (list z) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; should not work:
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z) (foo x)))))
;; (rename-formal 'x 'foo '(lambda (x) (y (lambda (z foo) (z x)))))
;; (rename-formal 'x 'foo '(lambda (x y foo) (y (lambda (z) (foo x)))))
;; (rename-formal 'x 'y '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'x 'z '(lambda (x) (y (lambda (z) (z x)))))
;; (rename-formal 'a 'loop '(lambda (a) (let loop ((a loop) (b a)) (loop a b c))))
;; (rename-formal 'list 'c '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'list 'y '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'z 'b '(lambda (list) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'z 'b '(lambda (list z) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))
;; (rename-formal 'z 'a '(lambda (list z) (let* ((a a) (b a) (c (+ b z))) (list a (car b) c) y)))

(define rename-free-occurrences
  (lambda (old-var new-var)
    (letrec
      ((rename
	(lambda (exp)
	  (cond
	    ((null? exp) exp)
	    ((literal? exp) exp)
	    ((vector? exp) exp)
	    ((symbol? exp) (if (eq? exp old-var) new-var exp))
	    (else
	     (record-case exp
	       (quote (datum) exp)
	       (quasiquote (datum)
		 (list 'quasiquote (rename-in-quasiquote datum)))
	       (if (test . conseqs)
		 `(if ,@(map rename (cdr exp))))
	       (cond clauses
		 `(cond ,@(map (lambda (clause)
				 (if (eq? (car clause) 'else)
				   (cons 'else (map rename (cdr clause)))
				   (map rename clause)))
			       clauses)))
	       (lambda (formals . bodies)
		 (if (mem? old-var formals)
		   exp
		   `(lambda ,formals ,@(map rename bodies))))
	       (let (bindings . bodies)
		 (if (symbol? bindings)
		   ;; named let
		   (let ((name (cadr exp))
			 (vars (map car (caddr exp)))
			 (exps (map cadr (caddr exp)))
			 (bodies (cdddr exp)))
		     `(let ,name ,(map list vars (map rename exps))
			,@(if (or (member old-var vars) (eq? old-var name))
			    bodies
			    (map rename bodies))))
		   ;; ordinary let
		   (let ((vars (map car bindings))
			 (exps (map cadr bindings)))
		     `(let ,(map list vars (map rename exps))
			,@(if (member old-var vars)
			    bodies
			    (map rename bodies))))))
	       (let* (bindings . bodies)
		 (let ((vars (map car bindings)))
		   (letrec
		     ((rename-binding-exps
		       (lambda (bindings)
			 (if (null? bindings)
			   '()
			   (let* ((var (caar bindings))
				  (exp (cadar bindings))
				  (new-binding (list var (rename exp))))
			     (if (eq? var old-var)
			       (cons new-binding (cdr bindings))
			       (cons new-binding (rename-binding-exps (cdr bindings)))))))))
		     `(let* ,(rename-binding-exps bindings)
			,@(if (member old-var vars)
			    bodies
			    (map rename bodies))))))
	       (letrec (decls . bodies)
		 (let ((vars (map car decls))
		       (procs (map cadr decls)))
		   (if (member old-var vars)
		     exp
		     `(letrec ,(map list vars (map rename procs))
			,@(map rename bodies)))))
	       (set! (var rhs-exp) `(set! ,var ,(rename rhs-exp)))
	       (begin exps `(begin ,@(map rename exps)))
	       (and exps `(and ,@(map rename exps)))
	       (or exps `(or ,@(map rename exps)))
	       (case (exp . clauses)
		 `(case ,(rename exp)
		    ,@(map (lambda (clause)
			     (if (eq? (car clause) 'else)
			       (cons 'else (map rename (cdr clause)))
			       (map rename clause)))
			   clauses)))
	       (record-case (exp . clauses)
		 `(record-case ,(rename exp)
		    ,@(map rename-record-case-clause clauses)))
	       (cases (type exp . clauses)
		 `(cases ,type ,(rename exp)
		    ,@(map rename-record-case-clause clauses)))
	       (else (if (member (car exp) syntactic-keywords)
		       (error 'rename (format "don't know how to process ~a" exp))
		       (map rename exp))))))))
       (rename-in-quasiquote
	 (lambda (datum)
	   (cond
	     ((vector? datum) (list->vector (rename-in-quasiquote (vector->list datum))))
	     ((not (pair? datum)) datum)
	     ;; doesn't handle nested quasiquotes yet
	     ((quasiquote? datum) datum)
	     ((unquote? datum) (list 'unquote (rename (cadr datum))))
	     ((unquote-splicing? datum) (list 'unquote-splicing (rename (cadr datum))))
	     (else (cons (rename-in-quasiquote (car datum))
			 (rename-in-quasiquote (cdr datum)))))))
       (rename-record-case-clause
	 (lambda (clause)
	   (if (eq? (car clause) 'else)
	     (cons 'else (map rename (cdr clause)))
	     (let ((tags (car clause))
		   (formals (cadr clause)))
	       (if (member old-var formals)
		 clause
		 `(,tags ,formals ,@(map rename (cddr clause)))))))))
      rename)))

(define contains-unsafe-lambda?
  (lambda (old-var new-var)
    (letrec
      ((unsafe?
	(lambda (exp)
	  (cond
	    ((null? exp) #f)
	    ((literal? exp) #f)
	    ((vector? exp) #f)
	    ((symbol? exp) #f)
	    (else
	      (record-case exp
		(quote (datum) #f)
		(quasiquote (datum) (unsafe-in-quasiquote? datum))
		(if (test . conseqs) (ormap unsafe? (cdr exp)))
		(cond clauses
		  (ormap (lambda (clause)
			   (if (eq? (car clause) 'else)
			       (ormap unsafe? (cdr clause))
			       (ormap unsafe? clause)))
			 clauses))
	       (lambda (formals . bodies)
		 (cond
		   ((mem? old-var formals) #f)
		   ((and (mem? new-var formals) (member old-var (all-free bodies '()))) #t)
		   (else (ormap unsafe? bodies))))
	       (let (bindings . bodies) (unsafe? (let-transformer exp (lambda (v) v))))
	       (let* (bindings . bodies) (unsafe? (let*-transformer exp (lambda (v) v))))
	       (letrec (decls . bodies) (unsafe? (letrec-transformer exp (lambda (v) v))))
	       (set! (var rhs-exp) (unsafe? rhs-exp))
	       (begin exps (ormap unsafe? exps))
	       (and exps (ormap unsafe? exps))
	       (or exps (ormap unsafe? exps))
	       (case (kexp . clauses)
		 (or (unsafe? kexp)
		      (ormap (lambda (clause)
			       (if (eq? (car clause) 'else)
				 (ormap unsafe? (cdr clause))
				 (ormap unsafe? clause)))
			      clauses)))
	       (record-case (rexp . clauses)
		 (unsafe? (record-case-transformer exp (lambda (v) v))))
	       (cases (type rexp . clauses)
		 (unsafe? (record-case-transformer `(record-case ,rexp ,@clauses) (lambda (v) v))))
	       (else (if (member (car exp) syntactic-keywords)
		       (error 'unsafe? (format "don't know how to process ~a" exp))
		       (ormap unsafe? exp))))))))
       (unsafe-in-quasiquote?
	 (lambda (datum)
	   (cond
	     ((vector? datum) (unsafe-in-quasiquote? (vector->list datum)))
	     ((not (pair? datum)) #f)
	     ;; doesn't handle nested quasiquotes yet
	     ((quasiquote? datum) #f)
	     ((unquote? datum) (unsafe? (cadr datum)))
	     ((unquote-splicing? datum) (unsafe? (cadr datum)))
	     (else (or (unsafe-in-quasiquote? (car datum))
		       (unsafe-in-quasiquote? (cdr datum))))))))
      unsafe?)))

(define show-function-signatures
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
	(let loop ((exp (read port)) (sigs '()))
	  (cond
	    ((eof-object? exp)
	     (newline))
	    ((or (define? exp) (define*? exp))
	     (cond
	       ((mit-style-define? exp)
		(pretty-print (cadr exp))
		(loop (read port) (cons (cadr exp) sigs)))
	       ((lambda? (caddr exp))
		(let ((sig `(,(cadr exp) ,@(cadr (caddr exp)))))
		  (pretty-print sig)
		  (loop (read port) (cons sig sigs))))
	       (else (loop (read port) sigs))))
	    (else (loop (read port) sigs))))))))


(define all-datatypes (make-all-datatypes))

;;-------------------------------------------------------------------------------
;; macro transformers

(define let-transformer
  (lambda (code k)
    (if (symbol? (cadr code))
      ;; named let
      (let* ((name (cadr code))
	     (bindings (caddr code))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cdddr code)))
	(k `(letrec ((,name (lambda ,vars ,@bodies))) (,name ,@exps))))
      ;; ordinary let
      (let* ((bindings (cadr code))
	     (vars (map car bindings))
	     (exps (map cadr bindings))
	     (bodies (cddr code)))
	(k `((lambda ,vars ,@bodies) ,@exps))))))

(define letrec-transformer
  (lambda (code k)
    (let* ((decls (cadr code))
	   (vars (map car decls))
	   (procs (map cadr decls))
	   (bodies (cddr code)))
      (create-letrec-assignments vars procs
	(lambda (bindings assigns)  ;; bindings and assigns are unannotated
	  (k `(let ,bindings ,@assigns ,@bodies)))))))

(define create-letrec-assignments
  (lambda (vars procs k2)
    (if (null? vars)
      (k2 '() '())
      (create-letrec-assignments (cdr vars) (cdr procs)
	(lambda (bindings assigns)
	  (k2 (cons `(,(car vars) 'undefined) bindings)
	      (cons `(set! ,(car vars) ,(car procs)) assigns)))))))

;; avoids variable capture
(define cond-transformer
  (lambda (code k)
    (let ((clauses (cdr code)))
      (if (null? clauses)
	(amacro-error 'cond-transformer code)
	(let ((first-clause (car clauses))
	      (other-clauses (cdr clauses)))
	  (if (or (null? first-clause) (not (list? first-clause)))
	    (amacro-error 'cond-transformer code)
	    (let ((test-exp (car first-clause))
		  (then-exps (cdr first-clause)))
	      (cond
		((eq? test-exp 'else)
		 (cond
		   ((null? then-exps) (amacro-error 'cond-transformer '(else)))
		   ((null? (cdr then-exps)) (k (car then-exps)))
		   (else (k `(begin ,@then-exps)))))
		((null? then-exps)
		 (if (null? other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@other-clauses))))
			 (if bool bool (else-code))))))
		((eq? (car then-exps) '=>)
		 (cond
		   ((null? (cdr then-exps)) (amacro-error 'cond-transformer first-clause))
		   ((null? other-clauses)
		    (k `(let ((bool ,test-exp)
			      (th (lambda () ,(cadr then-exps))))
			  (if bool ((th) bool)))))
		   (else (k `(let ((bool ,test-exp)
				   (th (lambda () ,(cadr then-exps)))
				   (else-code (lambda () (cond ,@other-clauses))))
			       (if bool ((th) bool) (else-code)))))))
		((null? other-clauses)
		 (if (null? (cdr then-exps))
		   (k `(if ,test-exp ,(car then-exps)))
		   (k `(if ,test-exp (begin ,@then-exps)))))
		((null? (cdr then-exps))
		 (k `(if ,test-exp ,(car then-exps) (cond ,@other-clauses))))
		(else (k `(if ,test-exp (begin ,@then-exps) (cond ,@other-clauses))))))))))))

(define let*-transformer
  (lambda (code k)
    (let ((bindings (cadr code))
	  (bodies (cddr code)))
      (nest-let*-bindings bindings bodies k))))

(define nest-let*-bindings
  (lambda (bindings bodies k)
    (if (or (null? bindings)
	    (null? (cdr bindings)))
	(k `(let ,bindings ,@bodies))
	(nest-let*-bindings (cdr bindings) bodies
	  (lambda (v)
	    (k `(let (,(car bindings)) ,v)))))))

;; avoids variable capture
(define record-case-transformer
  (lambda (code k)
    (let ((exp (cadr code))
	  (clauses (cddr code)))
      (record-case-clauses->cond-clauses 'r clauses
	(lambda (bindings new-clauses)
	  (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define record-case-clauses->cond-clauses
  (lambda (var clauses k2)
    (if (null? clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses var (cdr clauses)
	(lambda (bindings new-clauses)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(cdr clause))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol? (car clause))
		(let ((name (car clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((eq? (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar clause)))
		  (k2 (cons `(,name (lambda ,(cadr clause) ,@(cddr clause))) bindings)
		      (cons `((memq (car ,var) ',(car clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))
