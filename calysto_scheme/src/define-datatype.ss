;; define-datatype.scm 

;;; This is an r5rs-compliant datatype system.

;;; exports define-datatype, isa, cases, list-of, always?

;;; All other procedures are prefixed with define-datatype:
;;; Any other exports are unintentional.

;;; (define-datatype name
;;;  (variant-name (field-name predicate-exp) ...)
;;;  ...)
;;; * variant-names become constructors
;;; * The field-names are used only for debugging.


; Will Clinger writes (4/2000):
; * Fixed bug in DEFINE-DATATYPE.
;   (It was generating code of the form (begin <expression> <definition> ...),
;   which is illegal in R5RS Scheme.)
; * Removed tests ERR20, ERR22, and ERR27 from DO-ALL-TESTS because:
;     * Test ERR20 contains an undefined variable UUU?
;     * Test ERR22 contains an undefined variable ? and also calls a
;       non-procedure.
;     * Test ERR27 contained an illegal syntax 5?, which I changed to 5,
;       creating a call to a non-procedure.
;
; Several things should be fixed but haven't been:
; * The banner's date is extracted in a fragile way.
; * Several test cases are of the form (begin <definition> ... <expression>),
;   which is illegal in R5RS Scheme.

;;; This version goes back to Erik Hilsdale, I think.

;;; A consequence of this design is that if someone decides to have a
;;; variant named `else', they cannot use it in the last clause of
;;; cases.  Another consequence is that lexically binding the
;;; identifier `else', a la (lambda (else) (cases ... (else ...))),
;;; causes else-clauses to break, so don't do that.

;;; Representation of raw data:
;;; variant-registry: '((variant-name . type-name) ...)
;;; type-registry: '(type-name ...)
;;; type-name is '(variant-names . ((variant-name field-name ...) ...))
;;; variant-name is a procedure for constructing the variant.
;;; All error messages are as they should be, except for when a
;;; datatype expression is given a non-symbol as a type or
;;; any constructor is a non-symbol.  This is because define has
;;; its own error message in that case.  We could, of course, solve
;;; these problems with (eval '---).

;;; The global names created by the test suite are all preceded
;;; by define-datatype:test:

;; new error reporting system added by mw Mon Apr 24 14:49:03 2000.
(define define-datatype:report-error eopl:error)

(define define-datatype:reset-registries 'ignored)
(define define-datatype:is-a-type? 'ignored)
(define define-datatype:datatype-checker&registry-updater 'ignored)
(define define-datatype:case-checker 'ignored)

(let ((define-datatype:type-registry '())
      (define-datatype:variant-registry '()))  

  (set! define-datatype:reset-registries
    (lambda ()
      (set! define-datatype:type-registry '())
      (set! define-datatype:variant-registry '())
      #t))

  (set! define-datatype:is-a-type?
    (lambda (type-name)
      (memq type-name define-datatype:type-registry)))

  (set! define-datatype:datatype-checker&registry-updater
    (letrec ((set?
               (lambda (s)
                 (if (null? s) #t
                     (and (not (memq (car s) (cdr s))) (set? (cdr s)))))))
      (lambda (Type-name Variants)
        (if (not (symbol? Type-name))
            (define-datatype:report-error 'define-datatype
              "~nThe data type name ~s is not an identifier."
              Type-name))
        (for-each
          (lambda (variant)
            (if (not (symbol? (car variant)))
                (define-datatype:report-error 'define-datatype
                  (string-append
                    "(While defining the ~a datatype)~n"
                    "  The variant-name ~s is not an identifier.")
                  Type-name (car variant))))
          Variants)
        (let ((variant-names (map car Variants)))
          (if (not (set? variant-names))
              (define-datatype:report-error 'define-datatype
                (string-append
                  "(While defining the ~a datatype)~n"
                  "  Some of the variant-names are repeated: ~s.")
                Type-name variant-names))
          (for-each
            (lambda (v)
              (cond  ;;; This assq cannot be changed.
                ((assq v define-datatype:variant-registry) =>
                 (lambda (pair)
                   (if (not (eq? (cdr pair) Type-name))
                       (define-datatype:report-error 'define-datatype
                         (string-append
                           "(While defining the ~a data type)~n"
                           "  The variant-name ~s has already been~n"
                           "  used as a variant name in ~s.")
                         Type-name v (cdr pair)))))))
            variant-names)
          (cond ;;; This assq could be a memq over variant names, only.
                ;;; but would require a third local registry.
            ((assq Type-name define-datatype:variant-registry) =>
             (lambda (pair)
               (define-datatype:report-error 'define-datatype
                 (string-append
                   "(While defining the ~a data type)~n"
                   "  The type name ~s has already been~n"
                   "  used as a variant name ~s in the~n"
                   "  data type ~s.")
                 Type-name Type-name (car pair) (cdr pair))))
            ((memq Type-name variant-names)
             (define-datatype:report-error 'define-datatype
               (string-append
                 "(While defining the ~a data type)~n"
                 "  Variant name is the same as the data type name.")
               Type-name)))
          (for-each
            (lambda (variant-name)
              (cond
                ((memq variant-name define-datatype:type-registry)
                 (define-datatype:report-error 'define-datatype
                   (string-append
                     "(While defining the ~a data type)~n"
                     "  The variant name ~s has already been~n"
                     "  used as a type name.")
                   Type-name variant-name))))
            variant-names)
          (set! define-datatype:variant-registry
            (append
              (map (lambda (v) (cons v Type-name)) variant-names)
              define-datatype:variant-registry))
          (cond 
            ((memq Type-name define-datatype:type-registry) =>
             (lambda (pair)
               (set-car! pair Type-name)))
            (else
              (set! define-datatype:type-registry
                (cons Type-name define-datatype:type-registry))))))))
  
  (set! define-datatype:case-checker
    (let ((remq-or-false
            (lambda (sym ls)
              (call-with-current-continuation
                (lambda (k)
                  (let f ((ls ls))
                    (cond ((null? ls) (k #f))
                          ((eq? (car ls) sym) (cdr ls))
                          (else (cons (car ls) (f (cdr ls)))))))))))
      (lambda (Type-value Type-name Expression clauses)
        (cond
           ((assq Type-name define-datatype:variant-registry) =>
           (lambda (variant-namextype-name)
             (define-datatype:pretty-print
               (cons 'cases
                 (cons Type-name
                   (cons Expression clauses))))
             (define-datatype:report-error 'cases
               (string-append
                 "The data type ~s should not be a variant name.~n")
               Type-name)))
          (else
            (if (eq? Type-name Expression)
                (begin
                  (define-datatype:pretty-print
                    (cons 'cases
                      (cons Type-name
                        (cons Expression clauses))))
                  (define-datatype:report-error 'cases
                    (string-append
                      "The data type ~s should not be the same~n"
                      "  as a lexical variable.")
                    Type-name))
                (let ((variant-table (cdr Type-value)))
                  (let f ((clauses* clauses)
                          (unused-variants (map car variant-table)))
                    (if (null? clauses*)
                        (if (not (null? unused-variants))
                            (begin
                              (define-datatype:pretty-print
                                (cons 'cases
                                  (cons Type-name
                                    (cons Expression clauses*))))
                              (define-datatype:report-error 'cases "Missing variant clauses for ~s."
                                unused-variants)))
                        (let* ((head-clause (car clauses*))
                               (tail-clauses (cdr clauses*))
                               (purported-variant (car head-clause)))
                          (if (eq? purported-variant Expression)
                              (begin
                                (define-datatype:pretty-print
                                  (cons 'cases
                                    (cons Type-name
                                      (cons Expression clauses))))
                                (define-datatype:report-error 'cases
                                  (string-append
                                    "The variant name ~s should not be the same~n"
                                    "  as a lexical variable.")
                                  Expression))
                              (cond
                                ((and (null? tail-clauses) (eq? purported-variant 'else))
                                        ; do nothing, we're fine
                                 )                        
                                ((assq purported-variant variant-table)
                                 =>
                                 (lambda (p)
                                   (let ((fields (cdr p))
                                         (purported-fields (cadr head-clause))
                                         (new-unused-variants-or-false
                                           (remq-or-false
                                             purported-variant
                                             unused-variants)))
                                     (if (not (=
                                                (length fields)
                                                (length purported-fields)))
                                         (begin
                                           (define-datatype:pretty-print
                                             (cons 'cases
                                               (cons Type-name
                                                 (cons Expression clauses))))
                                           (define-datatype:report-error 'cases "Bad fields in ~s." head-clause)))
                                     (if (not new-unused-variants-or-false)
                                         (begin
                                           (define-datatype:pretty-print
                                             (cons 'cases
                                               (cons Type-name
                                                 (cons Expression clauses))))
                                           (define-datatype:report-error 'cases "Duplicate variant clause: ~s."
                                             head-clause)))
                                     (f tail-clauses new-unused-variants-or-false))))
                                (else
                                  (define-datatype:pretty-print
                                    (cons 'cases
                                      (cons Type-name
                                        (cons Expression clauses))))
                                  (define-datatype:report-error 'cases
                                    "Bad clause: ~s."
                                    head-clause)))))))))
            ))
        ))))

(define-syntax isa
  (syntax-rules ()
    ((_)
     (define-datatype:report-error 'isa "isa expects 1 argument, not 0."))
    ((_ type-name)
     (if (symbol? 'type-name)
         (lambda args
           (if (null? args)
               (define-datatype:report-error 'isa
                 "(isa ~s) expects 1 argument, not 0." 'type-name)
               (if (null? (cdr args))
                   (let ((variant (car args)))
                     (let ((type-info type-name)) 
                       (if (and (pair? type-info) (list? (car type-info)))
                           (and (pair? variant)
                                (memq (car variant) (car type-info)) #t)
                           (define-datatype:report-error 'isa
                             (string-append
                               "(isa ~s) did not get a data type bound to an~n"
                               "  appropriate structure: ~s.~n"
                               "  This tends to happen when the type name is~n"
                               "  bound to a lexical variable.")
                             'type-name type-info))))
                   (define-datatype:report-error 'isa
                     (string-append
                       "(isa ~s) expects 1 argument, not ~s.~n"
                       "  With argument list = ~s.")
                     'type-name (length args) args))))
         (define-datatype:report-error 'isa "Type name is not a symbol: ~s." 'type-name)))
    ((_  type-name other ...)
     (define-datatype:report-error 'isa "(isa ~s) expects 1 argument, not ~s with ~s."
       'type-name (add1 (length '(other ...)))
       (cons 'isa '(type-name other ...))))))

(define-syntax define-datatype
  (syntax-rules ()
    ((_ Type-name)
     (define-datatype:report-error 'define-datatype
       (string-append
         "~n  There are no variants: ~n  ~s.")
       '(define-datatype Type-name)))
    ((_ Type-name Type-name?)
     (define-datatype:report-error 'define-datatype
       (string-append
         "~n  There are no variants: ~n  ~s.")
       '(define-datatype Type-name Type-name?)))
    ((_ Type-name Type-name?
       (Variant-name (Field-name Pred?) ...)
       ...)
     (begin
       ;[wdc]
       (define ignored
               (define-datatype:datatype-checker&registry-updater
               'Type-name 
               '((Variant-name (Field-name Pred?) ...)
                 ...)))
       ;[\wdc]
       (define Type-name
         (cons '(Variant-name ...)
           '((Variant-name Field-name ...) ...)))
       (define Type-name?
         (if (symbol? 'Type-name)
           (lambda args
             (if (null? args)
               (define-datatype:report-error 'Type-name? "expects 1 argument, not 0.")
               (if (null? (cdr args))
                 (let ((variant (car args)))
                   (let ((type-info Type-name)) 
                     (if (and (pair? type-info) (list? (car type-info)))
                       (and (pair? variant)
                         (memq (car variant) (car type-info)) #t)
                       (define-datatype:report-error 'Type-name?
                         (string-append
                           "did not get a data type bound to an~n"
                           "  appropriate structure: ~s.~n"
                           "  This tends to happen when the type name is~n"
                           "  bound to a lexical variable.")
                         'type-name type-info))))
                 (define-datatype:report-error 'Type-name?
                   (string-append
                     "expects 1 argument, not ~s.~n"
                     "  With argument list = ~s.")
                    (length args) args))))
           (define-datatype:report-error 'Type-name "Type name is not a symbol: ~s." 'type-name)))
       (define Variant-name
         (let ((expected-length (length '(Field-name ...)))
               (field-names '(Field-name ...))
               (pred-names '(Pred? ...))
               (preds (list (lambda (x) (Pred? x)) ...)))
           (lambda args
             (if (not (= (length args) expected-length))
               (define-datatype:report-error 'Variant-name
                 (string-append
                   "Expected ~s arguments but got ~s arguments."
                   "~n  Fields are: ~s ~n  Args are: ~s.")
                 expected-length (length args) '(Field-name ...) args))
             (for-each
               (lambda (a f p pname)
                 (if (not (p a))
                   (define-datatype:report-error 'Variant-name "~n Bad ~a field (~s ~s) ==> #f."
                     f pname a)))
               args
               field-names
               preds
               pred-names)
             (cons 'Variant-name args))))
       ...))))
 
(define-syntax cases
  (syntax-rules ()
    ((_ Type-name Expression . Clauses)
     (let ((type-predicate? (isa Type-name)))
       (define-datatype:case-checker
         Type-name
         'Type-name
         'Expression
         'Clauses)
       (let ((x Expression))
         (if (type-predicate? x)
           (define-datatype:case-helper x . Clauses)
           (begin
             (define-datatype:pretty-print
               (cons 'cases
                 (cons 'Type-name
                   (cons 'Expression 'Clauses))))
             (define-datatype:report-error 'cases
               "~n  Not a ~a variant: ~s." 'Type-name x))))))))

;;; this only works because no-variant datatypes are invalid.
(define-syntax define-datatype:case-helper
  (syntax-rules (else)
    ((_ Variant (else Body0 Body1 ...))
     (begin Body0 Body1 ...))
    ((_ Variant (Purported-variant-name (Purported-field-name ...)
                  Body0 Body1 ...))
     (apply (lambda (Purported-field-name ...) Body0 Body1 ...)
       (cdr Variant)))
    ((_ Variant (Purported-variant-name (Purported-field-name ...)
                  Body0 Body1 ...)
       Clause ...)
     (if (eq? (car Variant) 'Purported-variant-name)
         (apply (lambda (Purported-field-name ...) Body0 Body1 ...)
           (cdr Variant))
         (define-datatype:case-helper Variant Clause ...)))
    ((_ Variant Neither-an-else-nor-clause ...)
     (define-datatype:report-error 'cases
       "~n  Not a ~a clause: ~s." 'Type-name
       (list Neither-an-else-nor-clause ...)))))

;;; ------------------------------
;;; general helpers

(define always?
  (lambda (x) #t))

(define list-of
  (lambda (pred . l)
    (let ((all-preds (cons pred l)))
      (lambda (obj)
        (let loop ((obj obj) (preds '()))
          (or 
            ;; if list is empty, preds should be, too
            (and (null? obj) (null? preds))
            (if (null? preds)
                ;; if preds is empty, but list isn't, then recycle
                (loop obj all-preds)
                ;; otherwise check and element and recur.
                (and (pair? obj)
                     ((car preds) (car obj))
                     (loop (cdr obj) (cdr preds))))))))))
