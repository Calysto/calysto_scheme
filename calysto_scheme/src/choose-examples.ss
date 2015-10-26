(define-syntax time 
  [(time ?exp) (let* ((run (lambda () ?exp))
		      (start (current-time))
		      (result (run))
		      (end (current-time)))
		 (display (list 'took (- end start) 'seconds))
		 (newline)
		 result)])

;;----------------------------------------------------------------------------

(define ace?
  (lambda (card)
    (equal? card 'ace)))

(define face-card?
  (lambda (card)
    (member card '(jack queen king))))

(define total
  (lambda (cards)
    (cond
      ((null? cards) 0)
      ((face-card? (car cards)) (+ 10 (total (cdr cards))))
      ((ace? (car cards)) (+ (choose 1 11) (total (cdr cards))))
      (else (+ (car cards) (total (cdr cards)))))))

(define distinct?
  (lambda (nums)
    (or (null? nums)
        (null? (cdr nums))
	(and (not (member (car nums) (cdr nums)))
	     (distinct? (cdr nums))))))

(define all-totals
  (lambda (cards)
    (let loop ((totals '()))
      (require (distinct? totals))
      (choose (loop (cons (total cards) totals))
	      totals))))

;;----------------------------------------------------------------------------
;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an
;; apartment house that contains only five floors.  Baker does not live on the
;; top floor.  Cooper does not live on the bottom floor.  Fletcher does not
;; live on either the top or the bottom floor.  Miller lives on a higher floor
;; than does Cooper.  Smith does not live on a floor adjacent to Fletcher's.
;; Fletcher does not live on a floor adjacent to Cooper's.  Where does
;; everyone live?

(define floors
  (lambda ()
    (let ((baker (choose 1 2 3 4 5))
	  (cooper (choose 1 2 3 4 5))
	  (fletcher (choose 1 2 3 4 5))
	  (miller (choose 1 2 3 4 5))
	  (smith (choose 1 2 3 4 5)))
      (require (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list
	(list 'baker baker)
	(list 'cooper cooper)
	(list 'fletcher fletcher)
	(list 'miller miller)
	(list 'smith smith)))))

;; more efficient version that applies constraints as soon as possible:
(define floors2
  (lambda ()
    (let ((baker (choose 1 2 3 4 5)))
      (require (not (= baker 5)))
      (let ((fletcher (choose 1 2 3 4 5)))
	(require (not (= fletcher 5)))
	(require (not (= fletcher 1)))
	(let ((cooper (choose 1 2 3 4 5)))
	  (require (not (= cooper 1)))
	  (require (not (= (abs (- fletcher cooper)) 1)))
	  (let ((smith (choose 1 2 3 4 5)))
	    (require (not (= (abs (- smith fletcher)) 1)))
	    (let ((miller (choose 1 2 3 4 5)))
	      (require (> miller cooper))
	      (require (distinct? (list baker cooper fletcher miller smith)))
	      (list
	        (list 'baker: baker)
		(list 'cooper: cooper)
		(list 'fletcher: fletcher)
		(list 'miller: miller)
		(list 'smith: smith)))))))))

;;-----------------------------------------------------------------------------
;; Five schoolgirls sat for an examination.  Their parents--so they
;; thought--showed an undue degree of interest in the result.  They therefore
;; agreed that, in writing home about the examination, each girl should make
;; one true statement and one untrue one.  The following are the relevant
;; passages from their letters:
;;
;; Betty: "Kitty was second in the examination.  I was only third."
;; Ethel: "You'll be glad to hear that I was on top.  Joan was second."
;; Joan: "I was third, and poor old Ethel was bottom."
;; Kitty: "I came out second.  Mary was only fourth."
;; Mary: "I was fourth.  Top place was taken by Betty."
;;
;; What in fact was the order in which the five girls were placed?

(define xor
  (lambda (p1 p2)
    (or (and p1 (not p2)) (and (not p1) p2))))

(define liars
  (lambda ()
    (let ((betty (choose 1 2 3 4 5))
	  (ethel (choose 1 2 3 4 5))
	  (joan (choose 1 2 3 4 5))
	  (kitty (choose 1 2 3 4 5))
	  (mary (choose 1 2 3 4 5)))
      (require (distinct? (list betty ethel joan kitty mary)))
      (require (xor (= kitty 2) (= betty 3)))
      (require (xor (= ethel 1) (= joan 2)))
      (require (xor (= joan 3) (= ethel 5)))
      (require (xor (= kitty 2) (= mary 4)))
      (require (xor (= mary 4) (= betty 1)))
      (list
	(list 'betty betty)
	(list 'ethel ethel)
	(list 'joan joan)
	(list 'kitty kitty)
	(list 'mary mary)))))

;; more efficient version that applies constraints as soon as possible:
(define liars2
  (lambda ()
    (let ((betty (choose 1 2 3 4 5))
	  (kitty (choose 1 2 3 4 5)))
      (require (xor (= kitty 2) (= betty 3)))
      (let ((mary (choose 1 2 3 4 5)))
	(require (xor (= mary 4) (= betty 1)))
	(require (xor (= kitty 2) (= mary 4)))
	(let ((ethel (choose 1 2 3 4 5))
	      (joan (choose 1 2 3 4 5)))
	  (require (xor (= ethel 1) (= joan 2)))
	  (require (xor (= joan 3) (= ethel 5)))
	  (require (distinct? (list betty ethel joan kitty mary)))
	  (list
	    (list 'betty betty)
	    (list 'ethel ethel)
	    (list 'joan joan)
	    (list 'kitty kitty)
	    (list 'mary mary)))))))

;;----------------------------------------------------------------------------
;; Pythagorean triples: (a, b, c) such that a^2 + b^2 = c^2

(define choose-integer-between
  (lambda (low high)
    (require (not (> low high)))
    (choose low (choose-integer-between (+ low 1) high))))

(define pythagorean-triple-between
  (lambda (low high)
    (let ((i (choose-integer-between low high)))
      (let ((j (choose-integer-between i high)))
	(let ((k (choose-integer-between j high)))
	  (require (= (+ (* i i) (* j j)) (* k k)))
	  (list i j k))))))

;; (pythagorean-triple-between 1 20)

;;----------------------------------------------------------------------------
;; A binary search tree
;;
;;       14                   
;;      /  \                  
;;     /    \                 
;;    7      26               
;;     \    /  \              
;;     12  20   31             
;;        /
;;       17
                                     
(define example-tree
  '(14 (7 () (12 () ()))
       (26 (20 (17 () ()) ())
	   (31 () ()))))

(define empty? (lambda (tree) (null? tree)))
(define root (lambda (tree) (car tree)))
(define left-branch (lambda (tree) (car (cdr tree))))
(define right-branch (lambda (tree) (car (cdr (cdr tree)))))

(define guess-path
  (lambda (n tree)
    (require (not (empty? tree)))
    (if (= (root tree) n)
      '()
      (choose
	(cons 'left (guess-path n (left-branch tree)))
	(cons 'right (guess-path n (right-branch tree)))))))

;; (guess-path 17 example-tree) => (right left left)

;;----------------------------------------------------------------------------
;; Nondeterministic "natural language" parser

;; word categories
(define articles '(ARTICLE the a))
(define nouns '(NOUN student professor cat class hat dog))
(define verbs '(VERB studies lectures eats sleeps))
(define prepositions '(PREP for to in by with))

(define *unprocessed-input* '())

(define parse
  (lambda (sentence)
    (set! *unprocessed-input* sentence)
    (let ((output (parse-sentence)))
      (require (null? *unprocessed-input*))
      output)))

(define parse-word
  (lambda (word-list)
    (require (not (null? *unprocessed-input*)))
    (let ((word-type (car word-list))
	  (words (cdr word-list))
	  (next-word (car *unprocessed-input*)))
      (require (memq next-word words))
      (set! *unprocessed-input* (cdr *unprocessed-input*))
      (list word-type next-word))))

(define parse-sentence
  (lambda ()
    (list 'SENTENCE
      (parse-noun-phrase)
      (parse-verb-phrase))))

(define parse-prepositional-phrase
  (lambda ()
    (list 'PREP-PHRASE
      (parse-word prepositions)
      (parse-noun-phrase))))

(define parse-simple-noun-phrase
  (lambda ()
    (list 'SIMPLE-NOUN-PHRASE
      (parse-word articles)
      (parse-word nouns))))

(define parse-noun-phrase
  (lambda ()
    (extend-noun-phrase (parse-simple-noun-phrase))))

(define extend-noun-phrase
  (lambda (np)
    (choose np (extend-noun-phrase
		 (list 'NOUN-PHRASE np (parse-prepositional-phrase))))))

(define parse-verb-phrase
  (lambda ()
    (extend-verb-phrase (parse-word verbs))))

(define extend-verb-phrase
  (lambda (vp)
    (choose vp (extend-verb-phrase
		 (list 'VERB-PHRASE vp (parse-prepositional-phrase))))))

;; (parse '(the cat in the hat sleeps))
;; (parse '(the professor lectures to the student with the cat))
;; (parse '(the professor lectures to the student in the class with the cat))
;; (choose)
;; etc.
;;
;;----------------------------------------------------------------------------
;; map coloring

(define choose-color
  (lambda ()
    (choose 'red 'yellow 'blue 'white)))

(define-syntax color
  [(color ?country different from . ?neighbors)
   (require (not (member ?country (list . ?neighbors))))])

(define color-europe
  (lambda ()
    (let ((portugal (choose-color))
          (spain (choose-color))
          (france (choose-color))
          (belgium (choose-color))
          (holland (choose-color))
          (germany (choose-color))
          (luxembourg (choose-color))
          (italy (choose-color))
          (switzerland (choose-color))
          (austria (choose-color)))
      ;; apply the constraints
      (color portugal different from spain)
      (color spain different from france portugal)
      (color france different from spain italy switzerland belgium germany luxembourg)
      (color belgium different from france holland luxembourg germany)
      (color holland different from belgium germany)
      (color germany different from france austria switzerland holland belgium luxembourg)
      (color luxembourg different from france belgium germany)
      (color italy different from france switzerland austria)
      (color switzerland different from france italy austria germany)
      (color austria different from italy switzerland germany)
      ;; return a coloring that satisfies the constraints
      (list (list 'portugal portugal)
	    (list 'spain spain)
	    (list 'france france)
	    (list 'belgium belgium)
	    (list 'holland holland)
	    (list 'germany germany)
	    (list 'luxembourg luxembourg)
	    (list 'italy italy)
	    (list 'switzerland switzerland)
	    (list 'austria austria)))))

;; more efficient version that applies constraints as soon as possible:
(define color-europe2
  (lambda ()
    (let ((portugal (choose-color))
          (spain (choose-color)))
      (color portugal different from spain)
      (let ((france (choose-color)))
	(color spain different from france portugal)
	(let ((italy (choose-color))
	      (switzerland (choose-color))
	      (belgium (choose-color))
	      (germany (choose-color))
	      (luxembourg (choose-color)))
	  (color france different from spain italy switzerland belgium germany luxembourg)
	  (color luxembourg different from france belgium germany)
	  (let ((holland (choose-color)))
	    (color belgium different from france holland luxembourg germany)
	    (color holland different from belgium germany)
	    (let ((austria (choose-color)))
	      (color germany different from france austria switzerland holland belgium luxembourg)
	      (color italy different from france switzerland austria)
	      (color switzerland different from france italy austria germany)
	      (color austria different from italy switzerland germany)
	      ;; return a coloring that satisfies the constraints
	      (list (list 'portugal portugal)
		    (list 'spain spain)
		    (list 'france france)
		    (list 'belgium belgium)
		    (list 'holland holland)
		    (list 'germany germany)
		    (list 'luxembourg luxembourg)
		    (list 'italy italy)
		    (list 'switzerland switzerland)
		    (list 'austria austria)))))))))

