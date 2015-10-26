(print-brackets #f)

;;; defining eopl fcns in R5RS scheme

(define eopl:printf
  (lambda (format . args)
    (let ((len (string-length format)))
      (let loop ((i 0) (args args))
        (let ((output
                (lambda (fn)
                  (fn (car args))
                  (loop  (+ i 2) (cdr args))))
              (outputc
                (lambda (fn)
                  (fn)
                  (loop (+ i 2) args))))
          (if (>= i len) #t
            (let ((c (string-ref format i)))
              (if (char=? c #\~)
                (case (string-ref format (+ i 1))
                  ((#\s) (output write))
                  ((#\a) (output display))
                  ((#\c) (output write-char))
                  ((#\% #\n) (outputc newline))
                  ((#\~) (outputc (lambda () (write-char #\~))))
                  (else
                    (write
                      "error in eopl:printf: unknown format character ")
                    (write-char  (string-ref format (+ i 1)))
                    (write-char #\newline)
                    (eopl:error-stop)))
                (begin
                  (display c)
                  (loop (+ i 1) args))))))))))

(define eopl:error
  (lambda (who format . data)
    (eopl:printf "Error reported by ~s:~%" who)
    (apply eopl:printf (cons format data))
    (newline)
    (eopl:error-stop)))

;; make error stop invoke the debugger
(define eopl:error-stop break)

(set! sllgen:pretty-print pretty-print)
(set! eopl:pretty-print pretty-print)
(set! define-datatype:pretty-print pretty-print)

(load "define-datatype.ss")

(printf "Loaded EOPL init file~%")
