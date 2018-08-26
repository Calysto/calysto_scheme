(define equal=?
  (letrec
      ((equal=?
	(lambda (e1 e2)
	  (cond
	    ((and (null? e1) (null? e2)) #t)
	    ((or (null? e1) (null? e2)) #f)
	    ((and (number? e1) (number? e2)) (= e1 e2))
	    ((or (number? e1) (number? e2)) #f)
	    ((and (pair? e1) (pair? e2))
	     (and (equal=? (car e1) (car e2))
		  (equal=? (cdr e1) (cdr e2))))
	    (else #f)))))
    equal=?))

;; for testing floating-point equality to within a given tolerance
(define equal-approx?
  (lambda (epsilon)
    (lambda (n1 n2)
      (and (number? n1)
	   (number? n2)
	   (<= (abs (- n1 n2)) epsilon)))))

(define equal-sets?
  (letrec
    ((f1? (lambda (x1 x2) (and (f2? x1) (f2? x2) (f5? x1 x2) (f5? x2 x1))))
     (f2? (lambda (x) (and (list? x) (f3? x))))
     (f3? (lambda (x) (cond ((null? x) #t) ((f4? (car x) (cdr x)) #f) (else (f3? (cdr x))))))
     (f4? (lambda (x y) (and (not (null? y)) (or (equal? (car y) x) (f4? x (cdr y))))))
     (f5? (lambda (x1 x2) (or (null? x1) (and (f4? (car x1) x2) (f5? (cdr x1) x2))))))
    f1?))

(define equal-multisets?
  (letrec
    ((f1? (lambda (x1 x2) (and (list? x1) (list? x2) (f5? x1 x2) (f5? x2 x1))))
     (f4? (lambda (x y) (and (not (null? y)) (or (equal? (car y) x) (f4? x (cdr y))))))
     (f5? (lambda (x1 x2) (or (null? x1) (and (f4? (car x1) x2) (f5? (cdr x1) x2))))))
    f1?))
