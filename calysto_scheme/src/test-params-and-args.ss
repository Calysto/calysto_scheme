
(try 
 ((lambda (a b c (x : 5) (y : *) (z : **))
    (print "ok" a b c x y z))
  (list 1 2 3 4 5 6 7 (x : 0) (apple : 10) (banana : 20)))
 (catch e e
	(print "correct! " (cadr e)))
 )
;;   error

(try 
 ((lambda (a b c (x : 5) (y : *))
    (print "ok" a b c x y))
  (list 1 2 3 (apple : 10) (banana : 20) (z : 77)))
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **))
    (print "ok" a b c x y z))
  (list 1 2 3 (x : 0) (apple : 10) (banana : 20)))
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 4 (* : '(10 11 12)) (apple : 30) (** : (dict (cherry 40) (kiwi 50)))))
 ;;   a=1, b=2, c=3, x=4, y=(10 11 12), z=dict
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 (** : '((x 40)))))
 ;;   a=1, b=2, c=3, x=40, y=(), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **))
    (print "ok" a b c x y z))
  (list 1 2 3))
 ;;   a=1, b=2, c=3, x=5, y=(), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 4))
 ;;   a=1, b=2, c=3, x=4, y=(), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 4 5))
 ;;   a=1, b=2, c=3, x=4, y=(5), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 4 5 6))
 ;;   a=1, b=2, c=3, x=4, y=(5 6), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 1 2 3 4 5 6 7))
 ;;   a=1, b=2, c=3, x=4, y=(5 6 7), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list (* : '(1 2 3 4 5 6 7 8))))
 ;;   a=1, b=2, c=3, x=4, y=(5 6 7 8), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda (a b c (x : 5) (y : *) (z : **)) 
    (print "ok" a b c x y z))
  (list 10 (* : '(1 2 3 4 5 6 7 8))))
 ;;   a=10, b=1, c=2, x=3, y=(4 5 6 7 8), z={}
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : *) (z : **)) 
    (print y z))
  (list 1 2 3 4 5))
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : *) (z : **)) 
    (print y z))
  '((* : (1 2 3 4 5))))
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : *) (z : **)) 
    (print "ok" y z))
  '((y : 3)))
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : *)) 
    (print "ok" y))
  '((y : 3)))
 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : **)) 
    (print "ok" y))
  '((y : 3))) 
 (catch e e
	(print "error! " (cadr e)))
 )

(try
 ((lambda ((y : *) (z : **)) 
    (print "ok" y z))
  '())
 (catch e e
	(print "error! " (cadr e)))
 )
