;;; Best-case numbers: the same shapes as scripts/cross_version_bench.ss,
;;; but scaled up to sizes that are only safe/fast enough to run on the
;;; current interpreter -- v1.4.8 would take minutes-to-hours and v2.0.1
;;; crashes outright above ~5,000 tail-call iterations (see
;;; README-PERFORMANCE.md). This shows the ceiling the JIT/trampoline
;;; work enables now that both speed and correctness allow it, not a
;;; number comparable across versions.
;;;
;;; Same warmup/measured split as scripts/cross_version_bench.ss.
;;;
;;; Run with:  python3 scheme.py scripts/best_case_bench.ss

(define (round4 x)
  (float (/ (round (* x 10000)) 10000)))

;; NOTE: `begin` + top-level `set!` here deliberately, not `let` -- see the
;; matching comment in scripts/benchmark.ss. A `let`-based timer wraps the
;; measured expression in a closure call; if any later statement in that
;; same closure body hits a primitive outside `_fast_prim_map` (e.g.
;; `current-time` itself), the whole call gets silently re-executed via
;; the slow-trampoline fallback -- doubling the measured expression's
;; actual work while under-reporting it, since the timer restarts on the
;; retry. Confirmed via an instrumented call counter.
(define _bench_start 0)
(define-syntax elapsed
  [(elapsed ?exp)
   (begin
     (set! _bench_start (current-time))
     ?exp
     (round4 (- (current-time) _bench_start)))])

(define-syntax result
  [(result ?label ?warmup-exp ?measured-exp)
   (begin
     ?warmup-exp
     (printf "RESULT ~a ~a~%" ?label (elapsed ?measured-exp)))])

(printf "Best-case benchmark (current version only)~%")
(printf "=============================================~%")

(define (cvb-fib n) (if (< n 2) n (+ (cvb-fib (- n 1)) (cvb-fib (- n 2)))))
(result "fib37" (cvb-fib 10) (cvb-fib 37))

(define (cvb-loop n acc) (if (= n 0) acc (cvb-loop (- n 1) (+ acc 1))))
(result "loop3000000" (cvb-loop 100 0) (cvb-loop 3000000 0))

(define (cvb-even? n) (if (= n 0) #t (cvb-odd? (- n 1))))
(define (cvb-odd? n) (if (= n 0) #f (cvb-even? (- n 1))))
(result "mutual50000" (cvb-even? 50) (cvb-even? 50000))

(printf "~%Done.~%")
