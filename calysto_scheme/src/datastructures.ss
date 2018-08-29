;; Data structures for implementing Scheme

;; Dictionary
;;  hash in Python
;;  associtive list in Scheme

(define-native dict
  (lambda assoc
    (cons 'dictionary '())))

(define-native setitem-native
  (lambda (dict keyword value)
    (let ((entry (assoc keyword (cdr dict))))
      (if entry
	  (set-cdr! entry value)
	  (set-cdr! dict (cons (cons keyword value) (cdr dict)))))))

(define-native getitem-native
  (lambda (dict keyword)
    (let ((entry (assoc keyword (cdr dict))))
      (if entry
	  (cdr entry)
	  #f))))

(define-native hasitem-native
  (lambda (dict keyword)
    (let ((entry (assoc keyword (cdr dict))))
      (if entry
	  #t
	  #f))))

(define-native dict->keys
  (lambda (dict)
    (map car (cdr dict))))

(define-native dict->values
  (lambda (dict)
    (map cdr (cdr dict))))

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
