;;; Cross-version benchmark: same script, run unmodified against tagged
;;; releases (checked out via `git worktree`) to compare interpreter/JIT
;;; performance across versions.
;;;
;;; Unlike a `time python3 scheme.py ...` wall-clock measurement, every
;;; number here is timed with `current-time` *inside* the already-running
;;; interpreter, so Python/interpreter start-up is never part of the
;;; result. Each shape is run twice:
;;;   - warmup:   a small input, discarded, so JIT-eligible closures (on
;;;               versions that have a JIT) get a chance to compile
;;;   - measured: the real input, timed after warmup, so the number
;;;               reflects steady-state speed rather than one-time
;;;               compile cost -- this is the "fairly reasonable" number.
;;;
;;; A second, larger input per shape is also run (still warmed up first)
;;; to report a "best case" number: how fast the current interpreter goes
;;; once given room to show it. On older/slower versions this best-case
;;; input is skipped rather than run to completion, since it can take
;;; minutes or crash outright (see README-PERFORMANCE.md).
;;;
;;; Run with:  python3 scheme.py scripts/cross_version_bench.ss

(define (round4 x)
  (float (/ (round (* x 10000)) 10000)))

(define-syntax elapsed
  [(elapsed ?exp)
   (let ((_bench_start (current-time)))
     ?exp
     (round4 (- (current-time) _bench_start)))])

;; Runs `?warmup-exp` once (discarded) then times `?measured-exp`,
;; printing a `RESULT <label> <seconds>` line the driver script parses --
;; or `CRASH <label> <message>` if either run raises (e.g. v2.0.1's
;; RecursionError on deep tail loops), so one crashing shape doesn't stop
;; the rest of the suite from running.
(define-syntax result
  [(result ?label ?warmup-exp ?measured-exp)
   (try
    (begin
      ?warmup-exp
      (printf "RESULT ~a ~a~%" ?label (elapsed ?measured-exp)))
    (catch e (printf "CRASH ~a ~a~%" ?label (get-exception-message e))))])

(printf "Cross-version benchmark~%")
(printf "========================~%")

;; ---------------------------------------------------------------------
;; 1. Naive recursion: fib(20), the benchmark used throughout
;;    README-PERFORMANCE.md's phase table.

(define (cvb-fib n) (if (< n 2) n (+ (cvb-fib (- n 1)) (cvb-fib (- n 2)))))

(result "fib20" (cvb-fib 10) (cvb-fib 20))

;; ---------------------------------------------------------------------
;; 2. Self tail-recursion: counting loop. v2.0.1 (Phase 1-3, no tail-call
;;    flattening) crashes with RecursionError above ~5,000 iterations, so
;;    3,000/6,000 stay in the "fairly reasonable" cross-version set.

(define (cvb-loop n acc) (if (= n 0) acc (cvb-loop (- n 1) (+ acc 1))))

(result "loop3000" (cvb-loop 100 0) (cvb-loop 3000 0))
(result "loop6000" (cvb-loop 100 0) (cvb-loop 6000 0))

;; ---------------------------------------------------------------------
;; 3. Mutual tail-recursion: even?/odd? calling each other. Never JIT-
;;    compiles (see scripts/benchmark.ss #3), so this is close to a
;;    trampoline-speed floor on every version.

(define (cvb-even? n) (if (= n 0) #t (cvb-odd? (- n 1))))
(define (cvb-odd? n) (if (= n 0) #f (cvb-even? (- n 1))))

(result "mutual2000" (cvb-even? 50) (cvb-even? 2000))

(printf "~%Done.~%")
