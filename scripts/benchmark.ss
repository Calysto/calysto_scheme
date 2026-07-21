;;; Performance benchmark suite for Calysto Scheme's interpreter/JIT.
;;;
;;; `fib` alone doesn't say much about how the trampoline/direct-eval/JIT
;;; optimizations behave across the range of shapes real programs use:
;;; tail loops, mutual recursion, closures, higher-order functions, and
;;; mutable state. This file times a handful of those shapes.
;;;
;;; Each benchmark runs twice, timed separately with `current-time`:
;;;   - warmup:   a small input, run first so every closure involved gets
;;;               a chance to JIT-compile before the real measurement
;;;   - measured: the real input, timed after warmup so the number reflects
;;;               steady-state speed rather than one-time compile cost
;;;
;;; Run with:  python3 scripts/benchmark.py

(define (round4 x)
  (float (/ (round (* x 10000)) 10000)))

;; NOTE: `elapsed` deliberately sequences with `begin` + a top-level `set!`
;; instead of `(let ((_bench_start (current-time))) ...)`. A `let` desugars
;; to a closure call, and a closure call that raises `_TrampolineFallback`
;; partway through its body (e.g. hits `current-time` or `printf`, neither
;; of which is in `_fast_prim_map`) gets its *entire body* silently
;; re-executed from scratch via the slow trampoline (see `apply_proc`'s
;; fallback in Scheme.py) -- including whatever already ran successfully
;; before the failure, such as `?exp` itself. That silently doubled every
;; number this file reports (confirmed by an instrumented counter: `(fib
;; 10)` inside `(let ((s (current-time))) (fib 10) (printf ...))` ran 354
;; times, not 177). `begin` at top level is plain sequencing, not a
;; closure call, so it isn't subject to that retry.
(define _bench_start 0)
(define-syntax elapsed
  [(elapsed ?exp)
   (begin
     (set! _bench_start (current-time))
     ?exp
     (round4 (- (current-time) _bench_start)))])

(define-syntax bench
  [(bench ?title ?warmup-label ?warmup-exp ?measured-label ?measured-exp)
   (begin
     (printf "~%~a~%" ?title)
     (printf "  warmup   ~a: ~as~%" ?warmup-label (elapsed ?warmup-exp))
     (printf "  measured ~a: ~as~%" ?measured-label (elapsed ?measured-exp)))])

(printf "Calysto Scheme benchmark suite~%")
(printf "===============================~%")

;; ---------------------------------------------------------------------
;; 1. Naive recursion: exponential call count, no memoization, no set!.
;;    The textbook JIT case -- a single self-recursive closure.

(define (bench-fib n)
  (if (< n 2) n (+ (bench-fib (- n 1)) (bench-fib (- n 2)))))

(bench "1. Naive recursion -- fib(n), exponential calls, no memoization"
       "fib(5)"  (bench-fib 5)
       "fib(30)" (bench-fib 30))

;; ---------------------------------------------------------------------
;; 2. Self tail-recursion: the idiomatic Scheme loop. Exercises Phase 4's
;;    tail-call flattening (a plain recursive translation would blow the
;;    Python stack well before these iteration counts).

(define (bench-count-loop n acc)
  (if (= n 0) acc (bench-count-loop (- n 1) (+ acc 1))))

(bench "2. Self tail-recursion -- counting loop"
       "1K iters"   (bench-count-loop 1000 0)
       "3M iters"   (bench-count-loop 3000000 0))

;; ---------------------------------------------------------------------
;; 3. Mutual tail-recursion: two closures that call *each other* in tail
;;    position. Included deliberately: unlike self-recursion, JIT
;;    compilation of one side requires the other side's compiled Python
;;    function to already exist. If `bench-even?` is *called* before
;;    `bench-odd?` has ever run (so `bench-odd?` isn't JIT'd yet),
;;    `bench-even?`'s own compile attempt fails and gets permanently
;;    cached as "don't JIT" -- so this pattern silently falls back to the
;;    slow trampoline for the rest of the program's life. Compare this
;;    number to #2 above (same shape of work, just split across two
;;    functions) to see the gap.

(define (bench-even? n) (if (= n 0) #t (bench-odd? (- n 1))))
(define (bench-odd? n) (if (= n 0) #f (bench-even? (- n 1))))

(bench "3. Mutual tail-recursion -- even?/odd? (JIT compile-order gap, see comment above)"
       "1K"   (bench-even? 1000)
       "50K"  (bench-even? 50000))

;; ---------------------------------------------------------------------
;; 4. Closures allocated per call: `bench-make-adder` returns a *new*
;;    closure every time it's invoked. Each new closure is a distinct
;;    proc object, so the JIT cache (keyed by object identity) can't
;;    reuse a previous compile -- every single closure pays its own
;;    compile() cost the first (and only) time it's called.

(define (bench-make-adder k) (lambda (x) (+ x k)))
(define (bench-adder-loop n acc)
  (if (= n 0) acc (bench-adder-loop (- n 1) (+ acc ((bench-make-adder n) 0)))))

(bench "4. Closures allocated per call -- fresh closure + compile every iteration"
       "200"    (bench-adder-loop 200 0)
       "20000"  (bench-adder-loop 20000 0))

;; ---------------------------------------------------------------------
;; 5. Nested closures: the innermost lambda captures a free variable two
;;    lexical levels out, not just its immediately enclosing frame.

(define (bench-make-adder2 a) (lambda (b) (lambda (c) (+ a b c))))
(define (bench-adder2-loop n acc)
  (if (= n 0) acc (bench-adder2-loop (- n 1) (+ acc (((bench-make-adder2 n) 2) 3)))))

(bench "5. Nested closures -- depth-2 free-variable capture"
       "200"   (bench-adder2-loop 200 0)
       "5000"  (bench-adder2-loop 5000 0))

;; ---------------------------------------------------------------------
;; 6. Higher-order function: a parameter used in *operator position*,
;;    called with a closure argument. The JIT can't know at compile time
;;    whether the argument will be a Python callable or a Scheme proc
;;    tuple, so this goes through the `_jit_call` runtime dispatch helper.

(define (bench-apply-twice f x) (f (f x)))
(define (bench-apply-twice-loop n acc)
  (if (= n 0)
      acc
      (bench-apply-twice-loop (- n 1) (+ acc (bench-apply-twice (bench-make-adder 3) n)))))

(bench "6. HOF -- parameter called in operator position"
       "200"   (bench-apply-twice-loop 200 0)
       "5000"  (bench-apply-twice-loop 5000 0))

;; ---------------------------------------------------------------------
;; 7. Built-in `map` driving a user lambda, from inside a JIT-eligible
;;    caller. Exercises the fast-prim-map path's interaction with a
;;    compiled closure argument.

(define (bench-build-list n acc)
  (if (= n 0) acc (bench-build-list (- n 1) (cons n acc))))
(define (bench-sum-list lst acc)
  (if (null? lst) acc (bench-sum-list (cdr lst) (+ acc (car lst)))))
(define (bench-map-test lst)
  (bench-sum-list (map (lambda (x) (* x x)) lst) 0))

(define bench-small-list (bench-build-list 200 '()))
(define bench-big-list (bench-build-list 20000 '()))

(bench "7. map + closure over a list"
       "200 elts"    (bench-map-test bench-small-list)
       "20000 elts"  (bench-map-test bench-big-list))

;; ---------------------------------------------------------------------
;; 8. Mutable state: any closure containing `set!` is excluded from both
;;    the direct-eval fast path and the JIT (see README-PERFORMANCE.md,
;;    Phase 2/3 safety analysis) and always runs through the original
;;    trampoline. Timed at the *same* iteration count as an equivalent
;;    pure loop so the two numbers are directly comparable.

(define (bench-stateful-loop n)
  (let ((acc 0))
    (let loop ((i 0))
      (if (= i n)
          acc
          (begin (set! acc (+ acc 1)) (loop (+ i 1)))))))

(printf "~%8. Mutable state -- set! forces the slow trampoline path (compare to #2)~%")
(printf "  warmup   stateful, 500 iters   : ~as~%" (elapsed (bench-stateful-loop 500)))
(let* ((pure-time (elapsed (bench-count-loop 20000 0)))
       (stateful-time (elapsed (bench-stateful-loop 20000))))
  (printf "  measured pure loop,     20000 iters: ~as~%" pure-time)
  (printf "  measured stateful loop, 20000 iters: ~as~%" stateful-time)
  (if (> pure-time 0)
      (printf "  slowdown from set!: ~ax~%" (round4 (/ stateful-time pure-time)))
      (void)))

(printf "~%Done.~%")
