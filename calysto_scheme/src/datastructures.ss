;; Data structures for implementing Scheme

;; Dictionary
;;  hash in Python
;;  associtive list in Scheme

(define-native dict
  (lambda args
    (cond
      ((null? args) (list 'dictionary '()))
      (else (list 'dictionary (car args))))))

(define-native setitem-native
  (lambda (dict keyword value)
    (let ((entry (assoc (to-string keyword) (cadr dict))))
      (if entry
	  (set-car! (cdr entry) value)
	  (set-car! (cdr dict) (cons (list (to-string keyword) value) (cadr dict)))))))

(define-native getitem-native
  (lambda (dict keyword)
    (let ((entry (assoc (to-string keyword) (cadr dict))))
      (if entry
	  (cadr entry)
	  #f))))

(define-native hasitem-native
  (lambda (dict keyword)
    (let ((entry (assoc (to-string keyword) (cadr dict))))
      (if entry
	  #t
	  #f))))

(define-native dict->keys
  (lambda (dict)
    (map car (cadr dict))))

(define-native dict->values
  (lambda (dict)
    (map cadr (cadr dict))))

;; A Vector List
;;  Python list in Python
;;  vector in Scheme

(define-native vlist
  (lambda assoc
    (cons 'vlist (vector))))

(define-native vlist-append-native
  (lambda (vec item)
    (set-cdr! vec (list->vector
		   (snoc item (vector->list (cdr vec)))))))

(define-native vlist-length-native
  (lambda (vec)
    (vector-length (cdr vec))))

(define-native vlist-ref-native
  (lambda (vec index)
    (vector-ref (cdr vec) index)))
