(load "transformer-macros.ss")

;; Environments represented as data structures

;; <env> ::= (environment <frame> ...)
;; <frame> ::= (<vector-of-bindings> <list-of-vars>)
;; <binding> ::= (<value> . <docstring>)

;; bindings

(define make-binding
  (lambda (value docstring)
    (cons value docstring)))

(define binding-value
  (lambda (binding)
    (car binding)))

(define binding-docstring
  (lambda (binding)
    (cdr binding)))

(define set-binding-value!
  (lambda (binding value)
    (set-car! binding value)))

(define set-binding-docstring!
  (lambda (binding docstring)
    (set-cdr! binding docstring)))

;; frames

(define make-frame
  (lambda (variables values docstrings)
    (list (list->vector (map make-binding values docstrings)) variables)))

(define empty-frame?
  (lambda (frame)
    (null? (cadr frame))))

(define frame-bindings
  (lambda (frame)
    (car frame)))

;; environments

;; <environment> ::= (environment <frame> <frame>*)

(define environment?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

(define make-empty-environment
  (lambda ()
    (list 'environment (make-frame '() '() '()))))

(define make-initial-environment
  (lambda (vars vals docstrings)
    (list 'environment (make-frame vars vals docstrings))))

(define first-frame
  (lambda (env)
    (cadr env)))

(define first-frame-vars
  (lambda (env)
    (cadr (first-frame env))))

(define initial-contours
  (lambda (env)
    (cdr (first-frame env))))

(define frames
  (lambda (env)
    (cdr env)))

(define add-binding
  (lambda (new-var new-binding frame)
    (let ((bindings (vector->list (car frame)))
	  (vars (cadr frame)))
      ;; must add new binding to the end of the frame, not the front, to
      ;; preserve correct lexical addresses of other variables in frame
      (list (list->vector (append bindings (list new-binding)))
	    (append vars (list new-var))))))

(define set-first-frame!
  (lambda (env new-frame)
    (set-car! (cdr env) new-frame)))

(define extend
  (lambda (env variables values docstrings)
    (cons 'environment (cons (make-frame variables values docstrings) (cdr env)))))

;; variable lookup

(define search-env
  (lambda (env variable)
    (search-frames (cdr env) variable)))

(define search-frames
  (lambda (frames variable)
    (if (null? frames)
      #f
      (let ((binding (search-frame (car frames) variable)))
        (if binding
          binding
          (search-frames (cdr frames) variable))))))

(define-native search-frame
  (lambda (frame var)
    (search-for-binding var (car frame) (cadr frame) 0)))

(define-native search-for-binding
  (lambda (var bindings variables i)
    (cond
     ((null? variables) #f)
     ((eq? (car variables) var) (vector-ref bindings i))
     (else (search-for-binding var bindings (cdr variables) (+ i 1))))))

;; for macro environments
(define in-first-frame?
  (lambda (var env)
    (true? (memq var (first-frame-vars env)))))

;; for macro environments
(define get-first-frame-value
  (lambda (var env)
    (binding-value (search-frame (first-frame env) var))))

(define* lookup-value-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (k (binding-value (vector-ref bindings offset)) fail))))

(define* lookup-binding-by-lexical-address
  (lambda (depth offset frames fail k)
    (let ((bindings (frame-bindings (list-ref frames depth))))
      (k (vector-ref bindings offset) fail))))

(define* lookup-value
  (lambda (var env var-info handler fail k)
    (lookup-variable var env var-info handler fail
      (lambda-cont2 (var fail)
	(k (dlr-env-lookup var) fail))
      (lambda-cont3 (dlr-obj components fail)
	(k (get-external-member dlr-obj components) fail))
      (lambda-cont2 (binding fail)
	(k (binding-value binding) fail)))))

(define* lookup-variable
  (lambda (var env var-info handler fail gk dk sk) ;; gk: global dlr, dk: dlr obj, sk: scheme module
    ;;(printf "in lookup-variable var: ~s env: ~s\n" var env)
    (let ((binding (search-env env var)))  ;; look in scheme env for variable as a whole
      (if binding
	(sk binding fail)  ;; found it in the scheme env
	(let ((components (split-variable var)))  ;; split into components, if any
	  (cond
	    ((and (null? (cdr components)) (dlr-env-contains (car components)))
	     (gk (car components) fail))  ;; (set! Myro 42)
	    ((and (not (null? (cdr components)))  ;; (set! Myro.robot 42)
		  (dlr-env-contains (car components))
		  (dlr-object-contains (dlr-env-lookup (car components)) components))
	     (dk (dlr-env-lookup (car components)) components fail))
	    ((null? (cdr components))  ;; (set! a.b..c val)
	     (runtime-error (format "unbound variable '~a'" var) var-info handler fail))
	    (else (lookup-variable-components components "" env var-info handler fail dk sk))))))))

;; math.x.y.z where math is a module or a DLR module/item
;; components: '(math x y z) "" ...
;; components: '(x y z) "math" ...
;; components: '(y z) "math.x" ...
;; components: '(z) "math.x.y" ...
(define* lookup-variable-components
  (lambda (components path module var-info handler fail dk sk)
    ;;(printf "in lookup-variable-components. components: ~s path: ~s\n" components path)
    (let* ((var (car components))
	   (binding (search-env module var)))
      (cond
	(binding  ;; (set! c val)
	  (if (null? (cdr components))
	    (sk binding fail)
	    (let ((value (binding-value binding))
		  (new-path (if (string=? path "") (format "~a" var) (format "~a.~a" path var))))
	      (cond
	        ((environment? value)
		 (lookup-variable-components (cdr components) new-path value var-info handler fail dk sk))
		((dlr-object-contains value components)
		 (dk value components fail))
		(else (runtime-error (format "'~a' is not a module" new-path) var-info handler fail))))))
	((string=? path "") (runtime-error (format "undefined item in '~a'" var) var-info handler fail))
	(else (runtime-error (format "unbound variable '~a' in module '~a'" var path) var-info handler fail))))))

;; adds a new binding for var to the first frame if one doesn't exist
(define* lookup-binding-in-first-frame
  (lambda (var env handler fail k)
    (let ((frame (first-frame env)))
      (let ((binding (search-frame frame var)))
        (if binding
	  (k binding fail)
	  (let ((new-binding (make-binding 'undefined "")))
	    (let ((new-frame (add-binding var new-binding frame)))
	      (set-first-frame! env new-frame)
	      (k new-binding fail))))))))

;; (split-variable 'a) => (a)
;; (split-variable 'a.b.c.d) => (a b c d)
;; (split-variable 'a.b..c) => ()
(define split-variable
  (lambda (var)
    (let ((strings (string-split (symbol->string var) #\.)))
      (if (member "" strings)
	'()
	(map string->symbol strings)))))

(define string-split
  (lambda (s delimiter-char)
    (letrec
      ((position
	(lambda (chars)
	  (if (char=? (car chars) delimiter-char)
	      0
	      (+ 1 (position (cdr chars))))))
       (split
	 (lambda (chars)
	   (cond
	     ((null? chars) '())
	     ((not (member delimiter-char chars)) (list (apply string chars)))
	     (else (let ((n (position chars)))
		     (cons (apply string (list-head chars n))
			   (split (cdr (list-tail chars n))))))))))
      (split (string->list s)))))

