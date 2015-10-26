(load "transformer-macros.ss")

;; Unification pattern-matcher

(define pattern?
  (lambda (x)
    (or (null? x)
	(number? x)
	(boolean? x)
	(symbol? x)
	(and (pair? x)
	     (pattern? (car x))
	     (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
	 (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x))
	 (not (pair? x)))))

(define* occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda-cont (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

;;-----------------------------------------------------------------------------------------------

;; for testing only
(define up
  (lambda (p1 p2)
    (unify-patterns p1 p2 init-cont)))

;; for testing only
(define aup
  (lambda (s1 s2)
    (let* ((ap1 (aread-string s1))
	   (ap2 (aread-string s2))
	   (p1 (unannotate ap1))
	   (p2 (unannotate ap2)))
      (unify-patterns^ p1 p2 ap1 ap2
	(lambda-cont (s)
	  (if (not s)
	    s
	    (print-sub s)))))))

(define print-sub
  (lambda (s)
    (record-case (cdr s)
      (empty () (void))
      (unit (new-var new-pattern new-apattern)
	(display new-var)
	(display " = ")
	(newline)
	(pretty-print new-pattern)
	(pretty-print new-apattern)
	(newline))
      (composite (s1 s2)
	(print-sub s1)
	(print-sub s2))
      (else (error 'print-sub "bad substitution: ~s" s)))))

;;-----------------------------------------------------------------------------------------------
;; annotated version

(define* unify-patterns^
  (lambda (p1 p2 ap1 ap2 k)    ;; k receives: subst/#f
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2 ap2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2 ap2)))))))
      ((pattern-variable? p2) (unify-patterns^ p2 p1 ap2 ap1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs^ p1 p2 ap1 ap2 k))
      (else (k #f)))))

(define* unify-pairs^
  (lambda (pair1 pair2 apair1 apair2 k)
    (unify-patterns^ (car pair1) (car pair2) (car^ apair1) (car^ apair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate^ (cdr pair1) s-car (cdr^ apair1)
	    (lambda-cont2 (new-cdr1 new-acdr1)
	      (instantiate^ (cdr pair2) s-car (cdr^ apair2)
		(lambda-cont2 (new-cdr2 new-acdr2)
		  (unify-patterns^ new-cdr1 new-cdr2 new-acdr1 new-acdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate^
  (lambda (pattern s ap k2)   ;; k2 receives: sexp, asexp
    (cond
      ((constant? pattern) (k2 pattern ap))
      ((pattern-variable? pattern) (apply-sub^ s pattern ap k2))
      ((pair? pattern)
       (instantiate^ (car pattern) s (car^ ap)
	 (lambda-cont2 (a aa)
	   (instantiate^ (cdr pattern) s (cdr^ ap)
	     (lambda-cont2 (b ab)
	       (k2 (cons a b) (cons^ aa ab (get-source-info ap))))))))
      (else (error 'instantiate^ "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

(define* apply-sub^
  (lambda (s var avar k2)        ;; k2 receives: sexp, asexp
    (record-case (cdr s)
      (empty () (k2 var avar))
      (unit (new-var new-pattern new-apattern)
	(if (equal? var new-var)
	  (k2 new-pattern new-apattern)
	  (k2 var avar)))
      (composite (s1 s2)
	(apply-sub^ s1 var avar
	  (lambda-cont2 (pattern apattern)
	    (instantiate^ pattern s2 apattern k2))))
      (else (error 'apply-sub^ "bad substitution: ~a" s)))))

;;-----------------------------------------------------------------------------------------------
;; unannotated version

;;(define* unify-patterns
;;  (lambda (p1 p2 k)
;;    (cond
;;      ((pattern-variable? p1)
;;       (if (pattern-variable? p2)
;;	 (k (make-sub 'unit p1 p2))
;;	 (occurs? p1 p2
;;	   (lambda-cont (bool)
;;	     (if bool
;;	       (k #f)
;;	       (k (make-sub 'unit p1 p2)))))))
;;      ((pattern-variable? p2) (unify-patterns p2 p1 k))
;;      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
;;      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
;;      (else (k #f)))))

;;(define* unify-pairs
;;  (lambda (pair1 pair2 k)
;;    (unify-patterns (car pair1) (car pair2)
;;      (lambda-cont (s-car)
;;	(if (not s-car)
;;	  (k #f)
;;	  (instantiate (cdr pair1) s-car
;;	    (lambda-cont (new-cdr1)
;;	      (instantiate (cdr pair2) s-car
;;		(lambda-cont (new-cdr2)
;;		  (unify-patterns new-cdr1 new-cdr2
;;		    (lambda-cont (s-cdr)
;;		      (if (not s-cdr)
;;			(k #f)
;;			(k (make-sub 'composite s-car s-cdr))))))))))))))

;;(define* instantiate
;;  (lambda (pattern s k)
;;    (cond
;;      ((constant? pattern) (k pattern))
;;      ((pattern-variable? pattern) (apply-sub s pattern k))
;;      ((pair? pattern)
;;       (instantiate (car pattern) s
;;	 (lambda-cont (a)
;;	   (instantiate (cdr pattern) s
;;	     (lambda-cont (b)
;;	       (k (cons a b)))))))
;;      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;;;(define extend-sub
;;;;  (lambda (old-s new-var new-pattern)
;;;;    (list 'extended new-var new-pattern old-s)))

;;(define* apply-sub
;;  (lambda (s var k)
;;    (record-case (cdr s)
;;      (empty () (k var))
;;;;      (extended (new-var new-pattern old-s)
;;;;	(if (equal? var new-var)
;;;;	  (k new-pattern)
;;;;	  (apply-sub old-s var k)))
;;      (unit (new-var new-pattern)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (k var)))
;;      (composite (s1 s2)
;;	(apply-sub s1 var
;;	  (lambda-cont (pattern)
;;	    (instantiate pattern s2 k))))
;;      (else (error 'apply-sub "bad substitution: ~a" s)))))
