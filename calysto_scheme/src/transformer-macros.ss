;; continuations

(define-syntax lambda-cont
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-cont2
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-cont3
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-cont4
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-fail
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; procedures (closures and primitives)

(define-syntax lambda-proc
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; exception handlers

(define-syntax lambda-handler
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

(define-syntax lambda-handler2
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; macro transformer functions

(define-syntax lambda-macro
  (syntax-rules ()
    [(_ formals body ...) (lambda formals body ...)]))

;; definitions to registerize

(define-syntax define*
  (syntax-rules ()
    [(_ name body ...) (define name body ...)]))

;; definitions that should NOT be transformed to C#

(define-syntax define-native
  (syntax-rules ()
    [(_ name body ...) (define name body ...)]))

;; where the computation should halt

(define-syntax halt*
  (syntax-rules ()
    [(_ value) value]))

;; for languages that need a return statement

(define-syntax return*
  (syntax-rules ()
    [(_ value) value]
    [(_ type value) value]))

(define-syntax define+
  (syntax-rules ()
    [(_ name body ...) (define name body ...)]))

(define-syntax apply+
  (syntax-rules ()
    [(_ proc args ...) (proc args ...)]))

(define-syntax apply!
  (syntax-rules ()
    [(_ proc args) (apply proc args)]))

(define-syntax while
  (syntax-rules ()
    [(_ test body ...) (letrec ((loop (lambda () 
					(if test 
					    (begin body ... (loop))
					    #f))))
			 (loop))]))

