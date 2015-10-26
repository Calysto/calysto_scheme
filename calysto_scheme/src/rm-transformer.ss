;; This program automatically converts CPS code in first-order tail
;; form into trampolined register machine form.
;;
;; Written by James B. Marshall
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;;
;; assumptions:
;; - code in first-order tail form
;; - define* functions will be converted to 0 arguments
;; - no internal define/define*'s
;; - define-native functions will be included as-is

(case-sensitive #t)

(load "ds-transformer.ss")

;; these definitions will not be included in the transformed output code
(define *ignore-definitions*
  '(start restart read-eval-print-loop execute execute-string execute-file execute-loop
     execute-next-expression
     ))

;; default transformer settings
(define *include-eopl-define-datatype-in-registerized-code?* #f)
(define *include-define*-in-registerized-code?* #t)
(define *generate-low-level-registerized-code?* #t)

(define low-level-output
  (lambda ()
    (set! *include-eopl-define-datatype-in-registerized-code?* #f)
    (set! *include-define*-in-registerized-code?* #f)
    (set! *generate-low-level-registerized-code?* #t)
    (printf "compile level: low-level register code, no define-datatype, no define*~%")))

(define compile-level-output
  (lambda ()
    (set! *include-eopl-define-datatype-in-registerized-code?* #f)
    (set! *include-define*-in-registerized-code?* #t)
    (set! *generate-low-level-registerized-code?* #t)
    (printf "compile level: low-level register code, define* included, no define-datatype~%")))

(define high-level-output
  (lambda ()
    (set! *include-eopl-define-datatype-in-registerized-code?* #t)
    (set! *include-define*-in-registerized-code?* #f)
    (set! *generate-low-level-registerized-code?* #f)
    (printf "compile level: high-level register code, define-datatype included, no define*~%")))

(define compile
  (lambda (base-filename)
    (delete-file (format "~a-ds.ss" base-filename))
    (delete-file (format "~a-rm.ss" base-filename))
    (ds-transform-file (format "~a.ss" base-filename) (format "~a-ds.ss" base-filename))
    (rm-transform-file (format "~a-ds.ss" base-filename) (format "~a-rm.ss" base-filename))
    'done!))

;;-------------------------------------------------------------------------------

;;(define get-define*-symbols
;;  (lambda (source-file)
;;    (call-with-input-file source-file
;;      (lambda (port)
;;	(let loop ((exp (read port)) (syms* '()))
;;	  (cond
;;	    ((eof-object? exp) syms*)
;;	    ((define? exp) (loop (read port) syms*))
;;	    ((define*? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) (union (caadr exp) syms*))
;;		 (loop (read port) (union (cadr exp) syms*))))
;;	    ((eopl-define-datatype? exp) (loop (read port) syms*))
;;	    (else (loop (read port) syms*))))))))

;;(define get-all-define*-symbols
;;  (lambda (source-files)
;;    (union-all (map get-define*-symbols source-files))))

;;(define get-defined-symbols
;;  (lambda (source-file)
;;    (call-with-input-file source-file
;;      (lambda (port)
;;	(let loop ((exp (read port)) (syms '()) (syms* '()))
;;	  (cond
;;	    ((eof-object? exp) (cons syms syms*))
;;	    ((define? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) (union (caadr exp) syms) syms*)
;;		 (loop (read port) (union (cadr exp) syms) syms*)))
;;	    ((define*? exp)
;;	     (if (mit-style? exp)
;;		 (loop (read port) syms (union (caadr exp) syms*))
;;		 (loop (read port) syms (union (cadr exp) syms*))))
;;	    ((eopl-define-datatype? exp)
;;	     (loop (read port) (union (map car (cdddr exp)) syms) syms*))
;;	    (else (loop (read port) syms syms*))))))))

;;(define get-all-defined-symbols
;;  (lambda (source-files)
;;    (union-all (map get-defined-symbols source-files))))

(define need-eopl-support? #f)

(define make-register-table
  (lambda ()
    (let ((all-registers '(final_reg))
	  (registerized-functions '())
	  (temp-vars '()))
      (lambda msg
	(record-case msg
	  (info ()
	    (printf "registers:~%")
	    (for-each (lambda (r) (printf "   ~a~%" r)) all-registers)
	    (printf "temporary vars:~%")
	    (for-each (lambda (r) (printf "   ~a~%" r)) temp-vars)
	    (for-each (lambda (f) (printf "~a: ~a~%" (car f) (cdr f)))
		      registerized-functions))
	  (get-registers () (sort alphabetical? all-registers))
	  (get-temp-vars () temp-vars)
	  (registerize-application? (code)
	    (and (symbol? (car code))
		 (assq (car code) registerized-functions)))
	  (registerize (code)
	    (let* ((operator (car code))
		   (operands (cdr code))
		   (params (cdr (assq operator registerized-functions)))
		   (assigns (params->assignments params operands))
		   (final-assigns (sort-assignments (remove-redundancies assigns)))
		   (pc-assign `(set! pc ,operator)))
	      `(begin ,@final-assigns ,pc-assign)))
	  (add-function (name regs)
	    (if (assq name registerized-functions)
	      (error-in-source #f
		(format "Symbol ~a is defined more than once at top level." name)))
	    (set! all-registers (union regs all-registers))
	    (set! registerized-functions (cons (cons name regs) registerized-functions))
	    'ok)
	  (add-temps (temps)
	    (set! temp-vars (union temps temp-vars))
	    'ok)
	  (else (error 'register-table (format "bad message: ~a" msg))))))))

(define register-table (make-register-table))

(define rm-transform-file
  (lambda (source-file . opts)
    (printf "rm-transform: transforming ~s~%" source-file)
    (set! register-table (make-register-table))
    (set! need-eopl-support? #f)
    (let ((eopl-defs '())
	  (defs '()))
      (letrec
	((transform-definitions
	   (lambda (input-port)
	     (let ((exp (read input-port)))
	       (if (eof-object? exp)
		 (begin
		   (set! eopl-defs (reverse eopl-defs))
		   (set! defs (reverse (map rm-transform defs))))
		 (begin
		   (cond
		     ((eopl-define-datatype? exp)
		      (set! need-eopl-support? #t)
		      (set! eopl-defs (cons exp eopl-defs)))
		     ((ignore-definition? exp)
		      (printf "skipping ~a definition\n" (cadr exp)))
		     ((or (define? exp) (define*? exp) (define+? exp))
		      (set! defs (cons (preprocess-define exp) defs)))
		     ((define-native? exp)
		      (set! defs (cons exp defs)))
		     ;; skip top level calls to load
		     (else 'skip))
		   (transform-definitions input-port))))))
	 (print-exp
	   (lambda (output-port)
	     (lambda (exp)
	       (pretty-print exp output-port)
	       (newline output-port))))
	 (print-code
	   (lambda (output-port)
	     (if *include-define*-in-registerized-code?*
		 (fprintf output-port "(load \"transformer-macros.ss\")~%~%"))
	     ;; did we see a define-datatype or cases form?
	     (if need-eopl-support?
	       (begin
		 (fprintf output-port ";;~a~%" (make-string 70 #\-))
		 (fprintf output-port ";; EOPL support~%~%")
		 (if *include-eopl-define-datatype-in-registerized-code?*
		   (begin
		     (fprintf output-port "(load \"petite-init.ss\")~%")
		     (fprintf output-port "(load \"define-datatype.ss\")~%~%")
		     (for-each (print-exp output-port) eopl-defs))
		   (begin
		     (for-each
		       (lambda (dd)
			 (fprintf output-port ";; ~a datatype~%" (cadr dd))
			 (for-each
			   (lambda (def)
			     (if *include-define*-in-registerized-code?*
			       (pretty-print (returnize def) output-port)
			       (pretty-print def output-port)))
			   (map rm-transform (expand-eopl-define-datatype dd)))
			 (newline output-port))
		       eopl-defs)))))
	     ;; global registers
	     (fprintf output-port ";;~a~%~%" (make-string 70 #\-))
	     (fprintf output-port ";; global registers~%")
	     (fprintf output-port "(define pc 'undefined)~%")
	     (for-each
	       (lambda (r) (fprintf output-port "(define ~a 'undefined)~%" r))
	       (register-table 'get-registers))
	     (newline output-port)
	     ;; temporary registers
	     (if (not (null? (register-table 'get-temp-vars)))
	       (begin
		 (fprintf output-port ";; temporary registers~%")
		 (for-each
		   (lambda (t) (fprintf output-port "(define ~a 'undefined)~%" t))
		   (register-table 'get-temp-vars))
		 (newline output-port)))
	     ;; registerized function definitions
	     (if *include-define*-in-registerized-code?*
	       (for-each (print-exp output-port) (map returnize defs))
	       (for-each (print-exp output-port) defs))
	     ;; trampoline
	     (fprintf output-port ";; the trampoline~%")
	     (pretty-print
	       '(define trampoline
		  (lambda ()
		    (if pc
			(begin (pc) (trampoline))
			final_reg)))
	       output-port)
	     (pretty-print
	       '(define pc-halt-signal #f)
	       output-port)
	     (newline output-port)
	     (pretty-print
	       '(define run (lambda (setup . args) (apply setup args) (return* (trampoline))))
	       output-port)
	     (newline output-port))))
	(call-with-input-file source-file
	  (lambda (input-port)
	    (transform-definitions input-port)))
	(let ((line-length (pretty-line-length)))
	  (pretty-line-length 400)
	  (if (null? opts)
	      (begin
		(newline)
		(print-code (current-output-port)))
	      (let ((output-filename (car opts)))
		(call-with-output-file output-filename
		  (lambda (output-port)
		    (print-code output-port)
		    (printf "Output written to ~a~%" output-filename)))))
	  (pretty-line-length line-length))))))

(define preprocess-define
  (lambda (def)
    (cond
      ((mit-style-define? def) (preprocess-define (mit-define->define def)))
      ((or (define*? def) (define+? def))
       (let* ((name (cadr def))
	      (lambda-exp (caddr def))
	      (formals (cadr lambda-exp))
	      (registers (get-register-names formals))
	      (new-lambda (rename-lambda-formals registers lambda-exp))
	      (new-bodies (cddr new-lambda)))
	 (register-table 'add-function name registers)
	 (if *include-define*-in-registerized-code?*
	     `(,(car def) ,name (lambda () ,@new-bodies)) ;; define* or define+
	     `(define ,name (lambda () ,@new-bodies)))))
      (else def))))

(define rm-transform
  (letrec
    ((transform
      (lambda (code)
	(cond
	 ((null? code) code)
	 ((literal? code) code)
	 ((vector? code) code)
	 ((symbol? code) code)
	 ;; should catch lambda-cont, lambda-whatever, etc. here
	 (else
	  (record-case code
;;	    (quote (datum) code)
	    (quote (datum)
	      (if *include-define*-in-registerized-code?*
		(expand-quote datum)
		code))
	    (quasiquote (datum)
	      (if *generate-low-level-registerized-code?*
		(transform (qq-expand-cps_ datum 0 (lambda (v) v)))
		(list 'quasiquote (transform-quasiquote datum))))
	    (if (test . conseqs)
	      `(if ,@(map transform (cdr code))))
	    (cond clauses
	      (if *generate-low-level-registerized-code?*
		(transform (cond-transformer code (lambda (v) v)))
		`(cond ,@(map (lambda (clause)
				(if (eq? (car clause) 'else)
				  `(else ,@(consolidate (map transform (cdr clause))))
				  (consolidate (map transform clause))))
			      clauses))))
	    (lambda (formals . bodies)
	      `(lambda ,formals ,@(consolidate (map transform bodies))))
	    (let (bindings . bodies)
	      (if (symbol? bindings)
		 ;; named let
		 (if *generate-low-level-registerized-code?*
		   (transform (let-transformer code (lambda (v) v)))
		   (let* ((name (cadr code))
			  (bindings (caddr code))
			  (bodies (cdddr code))
			  (vars (map car bindings))
			  (exps (map cadr bindings))
			  (new-bindings (map list vars (map transform exps)))
			  (new-bodies (consolidate (map transform bodies))))
		     `(let ,name ,new-bindings ,@new-bodies)))
		 ;; ordinary let
		 (if *generate-low-level-registerized-code?*
		   (let* ((vars (map car bindings))
			  (exps (map cadr bindings))
			  (texps (map transform exps))
			  (assigns (map (lambda (v e) `(set! ,v ,e)) vars texps))
			  (local-decls (map (lambda (v) `(,v 'undefined)) vars))
			  ;; remove any (set! x 'undefined) assignments introduced by letrec
			  (assigns (filter
				     (lambda (x) (not (equal? (caddr x) ''undefined)))
				     assigns))
			  (new-bodies (consolidate
					(append (sort-assignments assigns)
						(map transform bodies)))))
		     `(let ,local-decls ,@new-bodies))
		   (let* ((vars (map car bindings))
			  (exps (map cadr bindings))
			  (new-bindings (map list vars (map transform exps)))
			  (new-bodies (consolidate (map transform bodies))))
		     `(let ,new-bindings ,@new-bodies)))))
	    (let* (bindings . bodies)
	      (if *generate-low-level-registerized-code?*
		(let* ((vars (map car bindings))
		       (exps (map cadr bindings))
		       (texps (map transform exps))
		       (assigns (map (lambda (v e) `(set! ,v ,e)) vars texps))
		       (local-decls (map (lambda (v) `(,v 'undefined)) vars))
		       (new-bodies (consolidate (append assigns (map transform bodies)))))
		  `(let ,local-decls ,@new-bodies))
		(let* ((vars (map car bindings))
		       (exps (map cadr bindings))
		       (new-bindings (map list vars (map transform exps)))
		       (new-bodies (consolidate (map transform bodies))))
		  `(let* ,new-bindings ,@new-bodies))))
	    (letrec (decls . bodies)
	      (if *generate-low-level-registerized-code?*
		(transform (letrec-transformer code (lambda (v) v)))
		(let* ((vars (map car decls))
		       (procs (map cadr decls))
		       (new-procs (map list vars (map transform procs)))
		       (new-bodies (consolidate (map transform bodies))))
		  `(letrec ,new-procs ,@new-bodies))))
	    (set! (var rhs-exp)
	      `(set! ,var ,(transform rhs-exp)))
	    (begin exps
	      (let ((new-exps (consolidate (map transform exps))))
		(if (null? (cdr new-exps))
		  (car new-exps)
		  `(begin ,@new-exps))))
	    ((define define* define-native) (name body)
	      `(,(car code) ,name ,(transform body)))
	    (define+ (name body)
	      (let* ((let-exp (caddr body)) ;; body is a preprocessed lambda
		     (let-vars (map car (cadr let-exp)))
		     (let-bodies (cddr let-exp)))
		`(define ,name (lambda ,let-vars ,@(consolidate (map transform let-bodies))))))
	    (apply+ args
	      `(apply! ,(car args) ,(rac args)))
	    (define-syntax args code)
	    (and exps
	      `(and ,@(map transform exps)))
	    (or exps
	      `(or ,@(map transform exps)))
	    (case (exp . clauses)
	      (if *generate-low-level-registerized-code?*
		(let* ((var (if (symbol? exp)
			      exp
			      (car (make-temp-vars 1 (list code)))))
		       (cond-clauses
			 (map (lambda (clause)
				(if (eq? (car clause) 'else)
				  `(else ,@(cdr clause))
				  (let ((tags (car clause))
					(conseqs (cdr clause)))
				    (if (symbol? tags)
				      `((eq? ,var ',tags) ,@conseqs)
				      `((memq ,var ',tags) ,@conseqs)))))
			      clauses))
		       (cond-exp `(cond ,@cond-clauses)))
		  (if (symbol? exp)
		    (transform cond-exp)
		    (transform `(let ((,var ,exp)) ,cond-exp))))
		`(case ,(transform exp)
		   ,@(transform-case-clauses clauses))))
	    (record-case (exp . clauses)
	      (if *generate-low-level-registerized-code?*
		(let* ((var (if (symbol? exp)
			      exp
			      (car (make-temp-vars 1 (list code)))))
		       (cond-clauses
			 (map (lambda (clause)
				(if (eq? (car clause) 'else)
				  `(else ,@(cdr clause))
				  (let ((tags (car clause))
					(formals (cadr clause))
					(conseqs (cddr clause)))
				    (if (symbol? tags)
				      `((eq? (car ,var) ',tags)
					,@(rc-clause->let var formals conseqs))
				      `((memq (car ,var) ',tags)
					,@(rc-clause->let var formals conseqs))))))
			      clauses))
		       (cond-exp `(cond ,@cond-clauses)))
		  (if (symbol? exp)
		    (transform cond-exp)
		    (transform `(let ((,var ,exp)) ,cond-exp))))
		`(record-case ,(transform exp)
		   ,@(transform-record-case-clauses clauses))))
	    ;; EOPL
	    (define-datatype args code)
	    (cases (type exp . clauses)
	      (set! need-eopl-support? #t)
	      (if *include-eopl-define-datatype-in-registerized-code?*
		`(cases ,type ,(transform exp)
		   ,@(transform-record-case-clauses clauses))
		(transform `(record-case ,exp ,@clauses))))
	    (halt* (value)
	      `(begin (set! final_reg ,(transform value)) (set! pc pc-halt-signal)))
	    (else (cond
		    ((memq (car code) syntactic-keywords)
		     (error-in-source code "I don't know how to process the above code."))
		    ((register-table 'registerize-application? code)
		     (transform (register-table 'registerize code)))
		    (else (map transform code)))))))))
     (transform-quasiquote
       (lambda (datum)
	 (cond
	   ((vector? datum) (list->vector (transform-quasiquote (vector->list datum))))
	   ((not (pair? datum)) datum)
	   ;; doesn't handle nested quasiquotes
	   ((quasiquote? datum) datum)
	   ((unquote? datum) (list 'unquote (transform (cadr datum))))
	   ((unquote-splicing? (car datum))
	    (let* ((uqs (car datum))
		   (transformed-uqs (list 'unquote-splicing (transform (cadr uqs)))))
	      (if (null? (cdr datum))
		(cons transformed-uqs '())
		(cons transformed-uqs (transform-quasiquote (cdr datum))))))
	   (else
	     (cons (transform-quasiquote (car datum))
		   (transform-quasiquote (cdr datum)))))))
     (transform-record-case-clauses
       (lambda (clauses)
	 (map (lambda (clause)
		(if (eq? (car clause) 'else)
		  `(else ,@(consolidate (map transform (cdr clause))))
		  (let ((tags (car clause))
			(formals (cadr clause))
			(bodies (cddr clause)))
		    `(,tags ,formals ,@(consolidate (map transform bodies))))))
	   clauses)))
     (transform-case-clauses
       (lambda (clauses)
	 (map (lambda (clause)
		(cons (car clause) (consolidate (map transform (cdr clause)))))
	   clauses))))
    transform))

(define consolidate
  (lambda (exps)
    (cond
      ((null? exps) '())
      ((begin? (car exps)) (append (cdar exps) (consolidate (cdr exps))))
      (else (cons (car exps) (consolidate (cdr exps)))))))

(define rc-clause->let
  (lambda (var formals conseqs)
    (let ((free-vars (free conseqs '())))
      (letrec
	((make-bindings
	   (lambda (i formals)
	     (cond
	       ((null? formals) '())
	       ((memq (car formals) free-vars)
		(cons (list (car formals) `(list-ref ,var ,i))
		      (make-bindings (+ i 1) (cdr formals))))
	       ;; record-case formal is not used in conseqs, so no need to include it in the let
	       (else (make-bindings (+ i 1) (cdr formals)))))))
	(if (null? formals)
	    conseqs
	    (let ((let-bindings (make-bindings 1 formals)))
	      (if (null? let-bindings)
		  conseqs
		  (list `(let ,let-bindings ,@conseqs)))))))))

(define sort-assignments
  (lambda (assigns)
    (let ((graph (make-graph assigns)))
      (if (contains-cycle? graph)
	(make-temp-assignments assigns)
	(map vertex-contents (topological-sort graph))))))

(define get-neighbors
  (lambda (v graph)
    (filter (lambda (u)
	      (and (not (eq? v u))
		   (let* ((assign1 (vertex-contents v))
			  (assign2 (vertex-contents u))
			  (free-vars1 (free assign1 '()))
			  (var2 (cadr assign2)))
		     (memq var2 free-vars1))))
	    graph)))

(define make-vertex (lambda (x) (cons 'unmarked x)))
(define vertex-contents cdr)
(define vertex-tag? (lambda (v tag) (eq? (car v) tag)))
(define mark-vertex! set-car!)

(define make-graph (lambda (values) (map make-vertex values)))

(define mark-all-vertices!
  (lambda (graph tag)
    (for-each (lambda (v) (mark-vertex! v tag)) graph)))

(define make-stack (lambda () (cons 'stack '())))
(define stack-contents cdr)
(define push-stack! (lambda (x s) (set-cdr! s (cons x (cdr s)))))

(define contains-cycle?
  (lambda (graph)
    (call/cc
      (lambda (return)
	(letrec
	  ((visit-vertex
	     (lambda (v)
	       (mark-vertex! v 'grey)
	       (for-each
		 (lambda (u)
		   (cond
		     ((vertex-tag? u 'grey) (return #t))
		     ((vertex-tag? u 'white) (visit-vertex u))))
		 (get-neighbors v graph))
	       (mark-vertex! v 'black))))
	  (mark-all-vertices! graph 'white)
	  (for-each
	    (lambda (v)
	      (if (vertex-tag? v 'white)
		  (visit-vertex v)))
	    graph)
	  #f)))))

(define topological-sort
  (lambda (graph)
    (let ((stack (make-stack)))
      (letrec
	((depth-first-search
	   (lambda (v)
	     (mark-vertex! v 'visited)
	     (for-each
	       (lambda (neighbor)
		 (if (not (vertex-tag? neighbor 'visited))
		     (depth-first-search neighbor)))
	       (get-neighbors v graph))
	     (push-stack! v stack))))
	(mark-all-vertices! graph 'unvisited)
	(for-each
	  (lambda (v)
	    (if (not (vertex-tag? v 'visited))
		(depth-first-search v)))
	  graph)
	(stack-contents stack)))))

(define make-temp-vars
  (lambda (num exps)
    (let ((free-vars (all-free exps '())))
      (letrec
	((make-safe-temps
	   (lambda (i temps)
	     (if (= (length temps) num)
	       (reverse temps)
	       (let ((sym (string->symbol (format "temp_~a" i))))
		 (if (memq sym free-vars)
		   (make-safe-temps (+ i 1) temps)
		   (make-safe-temps (+ i 1) (cons sym temps))))))))
	(let ((temp-vars (make-safe-temps 1 '())))
	  (register-table 'add-temps temp-vars)
	  temp-vars)))))

(define make-temp-assignments
  (lambda (assigns)
    (let* ((registers (map cadr assigns))
	   (exps (map caddr assigns))
	   (temp-vars (make-temp-vars (length assigns) assigns))
	   (temp-assigns (map (lambda (t e) `(set! ,t ,e)) temp-vars exps))
	   (reg-assigns (map (lambda (r t) `(set! ,r ,t)) registers temp-vars)))
      (append temp-assigns reg-assigns))))

(define remove-redundancies
  (lambda (assigns)
    (filter (lambda (a) (not (eq? (cadr a) (caddr a))))
	    assigns)))

(define param->assign
  (lambda (param operand)
    `(set! ,param ,operand)))

;; params could be an improper list
(define params->assignments
  (lambda (params operands)
    (cond
      ((null? params) '())
      ((symbol? params) (list (param->assign params `(list ,@operands))))
      (else (cons (param->assign (car params) (car operands))
		  (params->assignments (cdr params) (cdr operands)))))))

(define param->reg-name
  (lambda (param)
    (string->symbol (format "~a_reg" param))))

;; params could be an improper list
(define get-register-names
  (lambda (params)
    (cond
      ((null? params) '())
      ((symbol? params) (param->reg-name params))
      (else (cons (param->reg-name (car params)) (get-register-names (cdr params)))))))

;;----------------------------------------------------------

(define expand-quote
  (lambda (datum)
    (cond
      ((null? datum) ''())
      ((number? datum) datum)
      ((boolean? datum) datum)
      ((char? datum) datum)
      ((list? datum)
       `(list ,@(map expand-quote datum)))
      ((pair? datum)
      `(cons ,(expand-quote (car datum))
	     ,(expand-quote (cdr datum))))
      ((symbol? datum) `(quote ,datum))
      (else (format "~a" datum)))))

(define returnize
  (lambda (code)
    (cond
      ((null? code) `(return* ,code))
      ((literal? code) `(return* ,code))
      ((vector? code) `(return* ,code))
      ((symbol? code) `(return* ,code))
      (else
        (record-case code
	  (quote (datum) `(return* ,code))
	  (quasiquote (datum) `(return* ,code))
	  (if (test . conseqs)
	    `(if ,test ,@(map returnize conseqs)))
	  (cond clauses
	    `(cond ,@(map returnize-last clauses)))
	  (lambda (formals . bodies)
	    `(lambda ,formals ,@(returnize-last bodies)))
	  (let (bindings . bodies)
	    (if (symbol? bindings)
	      ;; named let
	      (let* ((name (cadr code))
		     (bindings (caddr code))
		     (bodies (cdddr code)))
		`(let ,name ,bindings ,@(returnize-last bodies)))
	      ;; ordinary let
	      `(let ,bindings ,@(returnize-last bodies))))
	  (let* (bindings . bodies)
	    `(let* ,bindings ,@(returnize-last bodies)))
	  (letrec (decls . bodies)
	    (let* ((vars (map car decls))
		   (procs (map cadr decls))
		   (new-procs (map returnize procs))
		   (new-decls (map list vars new-procs)))
	      `(letrec ,new-decls ,@(returnize-last bodies))))
	  (set! (var rhs-exp)
	    (if (lambda? rhs-exp)
	      `(set! ,var ,(returnize rhs-exp))
	      code))
	  (begin exps (returnize-last code))
	  ((define define*) (name body)
	   (if (lambda? body)
	       `(,(car code) ,name ,(returnize body))
	       `(,(car code) ,name ,body)))
	  (apply+ args code)
	  (define-native args code)
	  (define-syntax args code)
	  (and exps `(return* ,code))
	  (or exps `(return* ,code))
	  (case (exp . clauses)
	    `(case ,exp ,@(map returnize-last clauses)))
	  (record-case (exp . clauses)
	    `(record-case ,exp ,@(map returnize-last clauses)))
	  ;; EOPL
	  (define-datatype args code)
	  (cases (type exp . clauses)
	    `(cases ,type ,exp ,@(map returnize-last clauses)))
	  (halt* (value) `(return* ,value))
;;	  (error (value) `(return* ,code))
	  ((error printf pretty-print) args code)
	  (else (cond
		  ((ends-with-a-bang? (car code)) code)
		  ((memq (car code) syntactic-keywords)
		   (error-in-source code "I don't know how to process the above code."))
		  (else `(return* ,code)))))))))

(define ends-with-a-bang?
  (lambda (symbol)
    (let ((name (symbol->string symbol)))
      (equal? (string-ref name (- (string-length name) 1)) #\!))))

(define returnize-last
  (lambda (exps)
    (if (null? (cdr exps))
      (list (returnize (car exps)))
      (cons (car exps) (returnize-last (cdr exps))))))

