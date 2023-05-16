;; Calysto Scheme scanner and s-expression reader
;;
;; includes support for vectors, rationals, exponents, and backquote
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/jmarshall
;; doug.blank@gmail.com
;; https://github.com/dsblank

(load "transformer-macros.ss")
(load "datastructures.ss")

;;------------------------------------------------------------------------
;; scanner - character stream represented as a position number

(define chars-to-scan 'undefined)

(define next-avail
  (lambda (n) (string-ref chars-to-scan n)))

(define remaining
  (lambda (n) (+ 1 n)))

(define scan-line 'undefined)
(define scan-char 'undefined)
(define scan-position 'undefined)
(define last-scan-line 'undefined)
(define last-scan-char 'undefined)
(define last-scan-position 'undefined)

(define initialize-scan-counters
  (lambda ()
    (set! scan-line 1)
    (set! scan-char 1)
    (set! scan-position 1)
    (set! last-scan-line scan-line)
    (set! last-scan-char scan-char)
    (set! last-scan-position scan-position)))

(define increment-scan-counters
  (lambda (chars)
    (set! last-scan-line scan-line)
    (set! last-scan-char scan-char)
    (set! last-scan-position scan-position)
    (cond
      ((char=? (next-avail chars) #\newline)
       (set! scan-line (+ 1 scan-line))
       (set! scan-char 1))
      (else
        (set! scan-char (+ 1 scan-char))))
    (set! scan-position (+ 1 scan-position))))

(define token-start-line 'undefined)
(define token-start-char 'undefined)
(define token-start-position 'undefined)

(define mark-token-start
  (lambda ()
    (set! token-start-line scan-line)
    (set! token-start-char scan-char)
    (set! token-start-position scan-position)))

;; scan-input takes a string and returns a list of tokens created
;; from all of the characters in the string

(define* scan-input
  (lambda (input src handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (initialize-scan-counters)
    (set! chars-to-scan (string-append input (string #\nul)))
    (scan-input-loop 0 src handler fail k)))

(define* scan-input-loop
  (lambda (chars src handler fail k)   ;; k receives 2 args: a list of tokens, fail
    (apply-action '(goto start-state) '() chars src handler fail
      (lambda-cont3 (token chars-left fail)
        (if (token-type? token 'end-marker)
          (k (list token) fail)
          (scan-input-loop chars-left src handler fail
            (lambda-cont2 (tokens fail)
              (k (cons token tokens) fail))))))))

;;------------------------------------------------------------------------
;; scanner actions

;; <action> ::= (shift <next-action>)
;;            | (replace <new-char> <next-action>)
;;            | (drop <next-action>)
;;            | (goto <state>)
;;            | (emit <token-type>)

;; might be better if actions were:
;;(shift next-state)
;;(shift/emit token-type)
;;(replace new-char next-state)
;;(drop next-state)
;;(drop/emit token-type)
;;(emit token-type)

(define* apply-action
  (lambda (action buffer chars src handler fail k)  ;; k receives 3 args: token, chars-left, fail
;;    (display "action: ")
;;    (display action)
;;    (display ", buffer: ")
;;    (write buffer)
;;    (newline)
    (record-case action
      (shift (next)
        (increment-scan-counters chars)
        (apply-action next (cons (next-avail chars) buffer) (remaining chars) src handler fail k))
      (replace (new-char next)
        (increment-scan-counters chars)
        (apply-action next (cons new-char buffer) (remaining chars) src handler fail k))
      (drop (next)
        (increment-scan-counters chars)
        (apply-action next buffer (remaining chars) src handler fail k))
      (goto (state)
        (if (eq? state 'token-start-state)
          (mark-token-start))
        (let ((action (apply-state state (next-avail chars))))
          (if (eq? action 'error)
            (unexpected-char-error chars src handler fail)
            (apply-action action buffer chars src handler fail k))))
      (emit (token-type)
        (convert-buffer-to-token token-type buffer src handler fail
          (lambda-cont (token)
            (k token chars fail))))
      (else (error 'apply-action "invalid action: ~a" action)))))

(define* scan-error
  (lambda (msg line char src handler fail)
    (handler (make-exception "ScanError" msg src line char) fail)))

(define* unexpected-char-error
  (lambda (chars src handler fail)
    (let ((c (next-avail chars)))
      (if (char=? c #\nul)
	(scan-error "unexpected end of input" scan-line scan-char src handler fail)
	(scan-error (format "unexpected character '~a' encountered" c) scan-line scan-char src handler fail)))))

(define* convert-buffer-to-token
  (lambda (token-type buffer src handler fail k)  ;; k receives 1 argument: token
    (let ((buffer (reverse buffer)))
      (case token-type
        (end-marker
          (k (make-token1 'end-marker)))
        (integer
          (k (make-token2 'integer (list->string buffer))))
        (decimal
          (k (make-token2 'decimal (list->string buffer))))
        (rational
          (k (make-token2 'rational (list->string buffer))))
        (identifier
          (k (make-token2 'identifier (string->symbol (list->string buffer)))))
        (boolean
          (k (make-token2 'boolean (or (char=? (car buffer) #\t) (char=? (car buffer) #\T)))))
        (character
          (k (make-token2 'character (car buffer))))
        (named-character
          (let ((name (list->string buffer)))
            (cond
              ((string=? name "nul") (k (make-token2 'character #\nul)))
              ((string=? name "space") (k (make-token2 'character #\space)))
              ((string=? name "tab") (k (make-token2 'character #\tab)))
              ((string=? name "newline") (k (make-token2 'character #\newline)))
              ((string=? name "linefeed") (k (make-token2 'character #\newline)))
              ((string=? name "backspace") (k (make-token2 'character #\backspace)))
              ((string=? name "return") (k (make-token2 'character #\return)))
              ((string=? name "page") (k (make-token2 'character #\page)))
	      (else (scan-error (format "invalid character name #\\~a" name)
				token-start-line token-start-char src handler fail)))))
        (string
          (k (make-token2 'string (list->string buffer))))
        (else
          (k (make-token1 token-type)))))))

;;------------------------------------------------------------------------
;; tokens
;;
;; <token> ::= (<token-type1> <token-start> <token-end>)
;;           | (<token-type2> <token-info> <token-start> <token-end>)
;;
;; <token-type1> ::= end-marker | lparen | rparen | lbracket | rbracket | lvector
;;                 | apostrophe | backquote | comma | comma-at | dot
;;
;; <token-type2> ::= integer | decimal | rational | boolean | character | string | identifier
;;
;; <token-start/end> ::= (<line> <char> <pos>)

(define make-token1
  (lambda (token-type)
    (let ((start (list token-start-line token-start-char token-start-position))
	  (end (list last-scan-line last-scan-char last-scan-position)))
      (if (eq? token-type 'end-marker)
	(list token-type end end)
	(list token-type start end)))))

(define make-token2
  (lambda (token-type token-info)
    (list token-type token-info
          (list token-start-line token-start-char token-start-position)
          (list last-scan-line last-scan-char last-scan-position))))

(define token-type?
  (lambda (token class)
    (eq? (car token) class)))

(define get-token-start
  (lambda (token)
    (rac (rdc token))))

(define get-token-end
  (lambda (token)
    (rac token)))

(define get-token-start-line
  (lambda (token)
    (car (get-token-start token))))

(define get-token-start-char
  (lambda (token)
    (cadr (get-token-start token))))

(define get-token-start-pos
  (lambda (token)
    (caddr (get-token-start token))))

(define rac
  (lambda (ls)
      (if (null? (cdr ls))
	  (car ls)
	  (let ((current (cdr ls)))
	    (while (pair? (cdr current))
	       (set! current (cdr current)))
	    (car current)))))

(define rdc
  (lambda (ls)
      (if (null? (cdr ls))
	  (list)
	  (let* ((retval (list (car ls)))
		 (front retval)
		 (current (cdr ls)))
	    (while (pair? (cdr current))
	       (set-cdr! retval (list (car current)))
	       (set! retval (cdr retval))
	       (set! current (cdr current)))
	    front))))

(define snoc
  (lambda (x ls)
      (if (null? ls)
	  (list x)
	  (let* ((retval (list (car ls)))
		 (front retval)
		 (current (cdr ls)))
	    (while (pair? current)
	       (set-cdr! retval (list (car current)))
	       (set! retval (cdr retval))
	       (set! current (cdr current)))
	    (set-cdr! retval (list x))
	    front))))

;;------------------------------------------------------------------------
;; character categories

(define char-delimiter?
  (lambda (c)
    (or (char-whitespace? c)
        (char=? c #\')
        (char=? c #\()
        (char=? c #\[)
        (char=? c #\))
        (char=? c #\])
        (char=? c #\")
        (char=? c #\;)
        (char=? c #\#)
        (char=? c #\nul))))

(define char-initial?
  (lambda (c)
    (or (char-alphabetic? c)
        (char=? c #\!)
        (char=? c #\$)
        (char=? c #\%)
        (char=? c #\&)
        (char=? c #\*)
        (char=? c #\/)
        (char=? c #\:)
        (char=? c #\<)
        (char=? c #\=)
        (char=? c #\>)
        (char=? c #\?)
        (char=? c #\^)
        (char=? c #\_)
        (char=? c #\~))))

(define char-special-subsequent?
  (lambda (c)
    (or (char=? c #\+)
        (char=? c #\-)
        (char=? c #\@)
        (char=? c #\.))))

(define char-subsequent?
  (lambda (c)
    (or (char-initial? c)
        (char-numeric? c)
        (char-special-subsequent? c))))

(define char-sign?
  (lambda (c)
    (or (char=? c #\+)
        (char=? c #\-))))

(define char-boolean?
  (lambda (c)
    (or (char=? c #\t)
        (char=? c #\T)
        (char=? c #\f)
        (char=? c #\F))))

;;------------------------------------------------------------------------
;; finite-state automaton

;; this is just a table lookup
(define apply-state
  (lambda (state c)
    (case state
      (start-state
        (cond
          ((char-whitespace? c) '(drop (goto start-state)))
          ((char=? c #\;) '(drop (goto comment-state)))
          ((char=? c #\nul) '(drop (emit end-marker)))
          (else '(goto token-start-state))))
      (token-start-state
        (cond
          ((char=? c #\() '(drop (emit lparen)))
          ((char=? c #\[) '(drop (emit lbracket)))
          ((char=? c #\)) '(drop (emit rparen)))
          ((char=? c #\]) '(drop (emit rbracket)))
          ((char=? c #\') '(drop (emit apostrophe)))
          ((char=? c #\`) '(drop (emit backquote)))
          ((char=? c #\,) '(drop (goto comma-state)))
          ((char=? c #\#) '(drop (goto hash-prefix-state)))
          ((char=? c #\") '(drop (goto string-state)))
          ((char-initial? c) '(shift (goto identifier-state)))
          ((char-sign? c) '(shift (goto signed-state)))
          ((char=? c #\.) '(shift (goto decimal-point-state)))
          ((char-numeric? c) '(shift (goto whole-number-state)))
          (else 'error)))
      (comment-state
        (cond
          ((char=? c #\newline) '(drop (goto start-state)))
          ((char=? c #\nul) '(drop (emit end-marker)))
          (else '(drop (goto comment-state)))))
      (comma-state
        (cond
          ((char=? c #\@) '(drop (emit comma-at)))
          (else '(emit comma))))
      (hash-prefix-state
        (cond
          ((char-boolean? c) '(shift (emit boolean)))
          ((char=? c #\\) '(drop (goto character-state)))
          ((char=? c #\() '(drop (emit lvector)))
          (else 'error)))
      (character-state
        (cond
          ((char-alphabetic? c) '(shift (goto alphabetic-character-state)))
          ((not (char=? c #\nul)) '(shift (emit character)))
          (else 'error)))
      (alphabetic-character-state
        (cond
          ((char-alphabetic? c) '(shift (goto named-character-state)))
          (else '(emit character))))
      (named-character-state
        (cond
          ((char-delimiter? c) '(emit named-character))
          (else '(shift (goto named-character-state)))))
      (string-state
        (cond
          ((char=? c #\") '(drop (emit string)))
          ((char=? c #\\) '(drop (goto string-escape-state)))
          ((not (char=? c #\nul)) '(shift (goto string-state)))
          (else 'error)))
      (string-escape-state
        (cond
          ((char=? c #\") '(shift (goto string-state)))
          ((char=? c #\\) '(shift (goto string-state)))
          ((char=? c #\b) '(replace #\backspace (goto string-state)))
          ((char=? c #\f) '(replace #\page (goto string-state)))
          ((char=? c #\n) '(replace #\newline (goto string-state)))
          ((char=? c #\t) '(replace #\tab (goto string-state)))
          ((char=? c #\r) '(replace #\return (goto string-state)))
          (else 'error)))
      (identifier-state
        (cond
          ((char-subsequent? c) '(shift (goto identifier-state)))
          ((char-delimiter? c) '(emit identifier))
          (else 'error)))
      (signed-state
        (cond
          ((char-numeric? c) '(shift (goto whole-number-state)))
          ((char=? c #\.) '(shift (goto signed-decimal-point-state)))
          ((char-delimiter? c) '(emit identifier))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (decimal-point-state
        (cond
          ((char-numeric? c) '(shift (goto fractional-number-state)))
          ((char-delimiter? c) '(emit dot))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (signed-decimal-point-state
        (cond
          ((char-numeric? c) '(shift (goto fractional-number-state)))
          ((char-delimiter? c) '(emit identifier))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (whole-number-state
        (cond
          ((char-numeric? c) '(shift (goto whole-number-state)))
          ((char=? c #\.) '(shift (goto fractional-number-state)))
          ((char=? c #\/) '(shift (goto rational-number-state)))
          ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
          ((char-delimiter? c) '(emit integer))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (fractional-number-state
        (cond
          ((char-numeric? c) '(shift (goto fractional-number-state)))
          ((or (char=? c #\e) (char=? c #\E)) '(shift (goto suffix-state)))
          ((char-delimiter? c) '(emit decimal))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (rational-number-state
        (cond
          ((char-numeric? c) '(shift (goto rational-number-state*)))
          ((char-delimiter? c) '(emit identifier))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (rational-number-state*
        (cond
          ((char-numeric? c) '(shift (goto rational-number-state*)))
          ((char-delimiter? c) '(emit rational))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (suffix-state
        (cond
          ((char-sign? c) '(shift (goto signed-exponent-state)))
          ((char-numeric? c) '(shift (goto exponent-state)))
          ((char-delimiter? c) '(emit identifier))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (signed-exponent-state
        (cond
          ((char-numeric? c) '(shift (goto exponent-state)))
          ((char-delimiter? c) '(emit identifier))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (exponent-state
        (cond
          ((char-numeric? c) '(shift (goto exponent-state)))
          ((char-delimiter? c) '(emit decimal))
          ((char-subsequent? c) '(shift (goto identifier-state)))
          (else 'error)))
      (else
        (error 'apply-state "invalid state: ~a" state)))))

;;------------------------------------------------------------------------
;; annotated s-expressions
;;
;; <aexp> ::= (#&atom <number> <info>)
;;          | (#&atom <boolean> <info>)
;;          | (#&atom <character> <info>)
;;          | (#&atom <string> <info>)
;;          | (#&atom <vector> <info>)
;;          | (#&atom <symbol> <info>)
;;          | (#&atom () <info>)
;;          | (#&pair <aexp> <aexp> <info>)
;;
;; <info> ::= (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos>)
;;          | (<srcfile> <start_line> <start_char> <start_pos> <end_line> <end_char> <end_pos> <macro-name>+)

(define atom-tag (box 'atom))
(define pair-tag (box 'pair))

(define aatom?
  (lambda (x)
    (and (pair? x) (eq? (car x) atom-tag))))

(define apair?
  (lambda (x)
    (and (pair? x) (eq? (car x) pair-tag))))

(define annotated?
  (lambda (x)
    (and (pair? x) (or (eq? (car x) atom-tag) (eq? (car x) pair-tag)))))

(define untag-atom^
  (lambda (aatom)
    (cadr aatom)))

(define atom?^
  (lambda (asexp)
    (eq? (car asexp) atom-tag)))

(define pair?^
  (lambda (asexp)
    (eq? (car asexp) pair-tag)))

(define null?^
  (lambda (asexp)
    (and (atom?^ asexp) (null? (untag-atom^ asexp)))))

(define symbol?^
  (lambda (asexp)
    (and (atom?^ asexp)
	 (symbol? (untag-atom^ asexp)))))

(define string?^
  (lambda (asexp)
    (and (atom?^ asexp) (string? (untag-atom^ asexp)))))

(define vector?^
  (lambda (asexp)
    (and (atom?^ asexp) (vector? (untag-atom^ asexp)))))

(define car^ (lambda (asexp) (cadr asexp)))
(define cdr^ (lambda (asexp) (caddr asexp)))
(define cadr^ (lambda (asexp) (car^ (cdr^ asexp))))
(define cdar^ (lambda (asexp) (cdr^ (car^ asexp))))
(define caar^ (lambda (asexp) (car^ (car^ asexp))))
(define cddr^ (lambda (asexp) (cdr^ (cdr^ asexp))))
(define cdddr^ (lambda (asexp) (cdr^ (cdr^ (cdr^ asexp)))))
(define caddr^ (lambda (asexp) (car^ (cdr^ (cdr^ asexp)))))
(define cdadr^ (lambda (asexp) (cdr^ (car^ (cdr^ asexp)))))
(define cadar^ (lambda (asexp) (car^ (cdr^ (car^ asexp)))))
(define caadr^ (lambda (asexp) (car^ (car^ (cdr^ asexp)))))
(define cadddr^ (lambda (asexp) (car^ (cdr^ (cdr^ (cdr^ asexp))))))
(define eq?^ (lambda (asexp sym) (eq? (cadr asexp) sym)))
(define vector->list^ (lambda (asexp) (vector->list (cadr asexp))))
(define symbol->string^ (lambda (asexp) (symbol->string (cadr asexp))))

(define list?^
  (lambda (asexp)
    (or (null?^ asexp)
	(and (pair?^ asexp) (list?^ (caddr asexp))))))

;; must wrap annotated lists with this before using ,@
(define at^
  (lambda (alist)
    (if (null?^ alist)
      '()
      (cons (car^ alist) (at^ (cdr^ alist))))))

(define length^
  (lambda (asexp)
    (cond
      ((null?^ asexp) 0)
      (else (+ 1 (length^ (cdr^ asexp)))))))

(define cons^
  (lambda (a b info)
    (list pair-tag a b info)))

(define map^
  (lambda (f^ asexp)
    (cond
      ((null?^ asexp) (make-null^))
      (else (cons^ (f^ (car^ asexp)) (map^ f^ (cdr^ asexp)) 'none)))))
      
(define make-null^
  (lambda ()
    (list atom-tag '() 'none)))

(define list^
  (lambda (x)
    (cons^ x (make-null^) 'none)))

(define *reader-generates-annotated-sexps?* #t)

;; for manual testing only
(define annotate
  (lambda (x info)
    (cond
      ((not *reader-generates-annotated-sexps?*) x)
      ((annotated? x) x)
      ((pair? x) (list pair-tag (annotate (car x) 'none) (annotate (cdr x) 'none) info))
      (else (list atom-tag x info)))))

(define* annotate-cps
  (lambda (x info k)   ;; k receives 1 arg: an annotated sexp
    (cond
      ((not *reader-generates-annotated-sexps?*) (k x))
      ((annotated? x) (k x))
      ((pair? x)
       (annotate-cps (car x) 'none
	 (lambda-cont (v1)
	   (annotate-cps (cdr x) 'none
	     (lambda-cont (v2)
	       (k (list pair-tag v1 v2 info)))))))
      (else (k (list atom-tag x info))))))

;; for manual testing only
(define unannotate
  (lambda (x)
    (cond
      ((aatom? x) (unannotate (cadr x)))
      ((apair? x) (cons (unannotate (cadr x)) (unannotate (caddr x))))
      ((pair? x) (cons (unannotate (car x)) (unannotate (cdr x))))
      ((vector? x) (list->vector (unannotate (vector->list x))))
      (else x))))

(define* unannotate-cps
  (lambda (x k)   ;; k receives 1 arg: an unannotated sexp
    (cond
      ((aatom? x)
       (unannotate-cps (cadr x) k))
      ((apair? x)
       (unannotate-cps (cadr x)
	 (lambda-cont (v1)
	   (unannotate-cps (caddr x)
	     (lambda-cont (v2)
	       (k (cons v1 v2)))))))
      ((pair? x)
       (unannotate-cps (car x)
	 (lambda-cont (v1)
	   (unannotate-cps (cdr x)
	     (lambda-cont (v2)
	       (k (cons v1 v2)))))))
      ((vector? x)
       (unannotate-cps (vector->list x)
	 (lambda-cont (ls)
	   (k (list->vector ls)))))
      (else (k x)))))

(define *filename-dict* (dict))
(define *filename-vector* (vlist))

(define filename-cache
  (lambda (filename)
    (if (hasitem-native *filename-dict* filename)
	(getitem-native *filename-dict* filename)
	(begin
	  (let ((index (vlist-length-native *filename-vector*)))
	    (vlist-append-native *filename-vector* filename)
	    (setitem-native *filename-dict* filename index)
	    index)))))

(define get-filename-from-index
  (lambda (index)
    (vlist-ref-native *filename-vector* index)))

(define make-info
  (lambda (src start end)
    (cons (filename-cache src) (append start end))))

(define replace-info
  (lambda (asexp new-info)
    (if (atom?^ asexp)
      (list atom-tag (cadr asexp) new-info)
      (list pair-tag (cadr asexp) (caddr asexp) new-info))))

(define get-srcfile
  (lambda (info)
    (if (eq? info 'none) 'none (get-filename-from-index (car info)))))

(define get-start-line
  (lambda (info)
    (if (eq? info 'none) 'none (cadr info))))

(define get-start-char
  (lambda (info)
    (if (eq? info 'none) 'none (caddr info))))

(define get-start-pos
  (lambda (info)
    (if (eq? info 'none) 'none (cadddr info))))

(define get-end-line
  (lambda (info)
    (if (eq? info 'none) 'none (car (cddddr info)))))

(define get-end-char
  (lambda (info)
    (if (eq? info 'none) 'none (cadr (cddddr info)))))

(define get-end-pos
  (lambda (info)
    (if (eq? info 'none) 'none (caddr (cddddr info)))))

(define get-source-info
  (lambda (asexp)
    (rac asexp)))

(define source-info?
  (lambda (x)
    (or (eq? x 'none) (list? x))))

(define has-source-info?
  (lambda (asexp)
    (not (eq? (get-source-info asexp) 'none))))

;; true if no macro name at end of info list
(define original-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
	 (= (length (get-source-info asexp)) 7))))

(define macro-derived-source-info?
  (lambda (asexp)
    (and (has-source-info? asexp)
	 (= (length (get-source-info asexp)) 8))))

;;------------------------------------------------------------------------
;; recursive descent parser
;;
;; <sexp> ::= <number> | <boolean> | <character> | <string> | <identifier>
;;          | ' <sexp>
;;          | ( <sexp>* )
;;          | ( <sexp>+ . <sexp> )
;;          | [ <sexp>* ]
;;          | [ <sexp>+ . <sexp> ]
;;          | ` <sexp>
;;          | , <sexp>
;;          | ,@ <sexp>

;; token stream represented as a list
(define first (lambda (x) (car x)))
(define rest-of (lambda (x) (cdr x)))

(define string->integer
  (lambda (str)
    (string->number str)))

(define string->decimal
  (lambda (str)
    (string->number str)))

(define string->rational
  (lambda (str)
    (string->number str)))

(define true?
  (lambda (v) 
    (if v #t #f)))

(define* unexpected-token-error
  (lambda (tokens src handler fail)
    (let ((token (first tokens)))
      (if (token-type? token 'end-marker)
	(read-error "unexpected end of input" tokens src handler fail)
	(read-error (format "unexpected '~a' encountered" (car token)) tokens src handler fail)))))

(define* read-error
  (lambda (msg tokens src handler fail)
    (let ((token (first tokens)))
      (handler (make-exception "ReadError" msg src 
		     (get-token-start-line token) 
		     (get-token-start-char token))
	       fail))))

;; returns the entire file contents as a single string
(define read-content
  (lambda (filename)
    (apply string
      (call-with-input-file filename
        (lambda (port)
          (let loop ((char (read-char port)))
            (if (eof-object? char)
              '()
              (cons char (loop (read-char port))))))))))

(define* read-sexp
  (lambda (tokens src handler fail k)   ;; k receives 4 args: sexp, end, tokens-left, fail
    (let ((start (get-token-start (first tokens)))
	  (end (get-token-end (first tokens))))
      (record-case (first tokens)
	(integer (str)
	  (annotate-cps (string->integer str) (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(decimal (str)
	  (annotate-cps (string->decimal str) (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(rational (str)
	  (let ((num (string->rational str)))
	    (if (true? num)
	      (annotate-cps num (make-info src start end)
		(lambda-cont (sexp)
		  (k sexp end (rest-of tokens) fail)))
	      (read-error (format "cannot represent ~a" str) tokens src handler fail))))
	(boolean (bool)
	  (annotate-cps bool (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(character (char)
	  (annotate-cps char (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(string (str)
	  (annotate-cps str (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(identifier (id)
	  (annotate-cps id (make-info src start end)
	    (lambda-cont (sexp)
	      (k sexp end (rest-of tokens) fail))))
	(apostrophe () (read-abbreviation tokens 'quote src handler fail k))
	(backquote () (read-abbreviation tokens 'quasiquote src handler fail k))
	(comma () (read-abbreviation tokens 'unquote src handler fail k))
	(comma-at () (read-abbreviation tokens 'unquote-splicing src handler fail k))
	(lparen ()
	  (let ((tokens (rest-of tokens)))
	    (read-sexp-sequence tokens 'rparen src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(annotate-cps sexps (make-info src start end)
		  (lambda-cont (sexp)
		    (k sexp end tokens-left fail)))))))
	(lbracket ()
	  (let ((tokens (rest-of tokens)))
	    (read-sexp-sequence tokens 'rbracket src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(annotate-cps sexps (make-info src start end)
		  (lambda-cont (sexp)
		    (k sexp end tokens-left fail)))))))
	(lvector ()
	  (read-vector-sequence (rest-of tokens) src handler fail
	    (lambda-cont4 (sexps end tokens-left fail)
	      (annotate-cps (list->vector sexps) (make-info src start end)
		(lambda-cont (sexp)
		  (k sexp end tokens-left fail))))))
	(else (unexpected-token-error tokens src handler fail))))))

(define* read-abbreviation
  (lambda (tokens keyword src handler fail k)
    (let ((start (get-token-start (first tokens)))
	  (keyword-end (get-token-end (first tokens))))
      (annotate-cps keyword (make-info src start keyword-end)
	(lambda-cont (v)
	  (read-sexp (rest-of tokens) src handler fail
	    (lambda-cont4 (sexp end tokens-left fail)
	      (annotate-cps (list v sexp) (make-info src start end)
		(lambda-cont (v2)
		  (k v2 end tokens-left fail))))))))))

(define* read-vector-sequence
  (lambda (tokens src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (record-case (first tokens)
      (rparen ()
	(close-sexp-sequence '() tokens 'rparen src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp1 end tokens-left fail)
	    (read-vector-sequence tokens-left src handler fail
	      (lambda-cont4 (sexps end tokens-left fail)
		(k (cons sexp1 sexps) end tokens-left fail)))))))))

(define* read-sexp-sequence
  (lambda (tokens expected-terminator src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (record-case (first tokens)
      ((rparen rbracket) ()
       (close-sexp-sequence '() tokens expected-terminator src handler fail k))
      (dot ()
	(read-error "unexpected dot (.)" tokens src handler fail))
      (else
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp1 end tokens-left fail)
	    (if (token-type? (first tokens-left) 'dot)
	      (read-sexp (rest-of tokens-left) src handler fail
		(lambda-cont4 (sexp2 end tokens-left fail)
		  (close-sexp-sequence
		    (cons sexp1 sexp2) tokens-left expected-terminator src handler fail k)))
	      (read-sexp-sequence tokens-left expected-terminator src handler fail
		(lambda-cont4 (sexps end tokens-left fail)
		  (k (cons sexp1 sexps) end tokens-left fail))))))))))

(define* close-sexp-sequence
  (lambda (sexps tokens expected-terminator src handler fail k)  ;; k gets 4 args: sexps, end, tokens-left, fail
    (let ((end (get-token-end (first tokens))))
      (record-case (first tokens)
	((rparen rbracket) ()
	 (cond
	   ((token-type? (first tokens) expected-terminator)
	    (k sexps end (rest-of tokens) fail))
	   ((eq? expected-terminator 'rparen)
	    (read-error "parenthesized list terminated by bracket" tokens src handler fail))
	   ((eq? expected-terminator 'rbracket)
	    (read-error "bracketed list terminated by parenthesis" tokens src handler fail))))
	(else (unexpected-token-error tokens src handler fail))))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(define init-cont (lambda-cont (v) (halt* v)))
(define init-cont2 (lambda-cont2 (v1 v2) (halt* v1)))
(define init-cont3 (lambda-cont3 (v1 v2 v3) (halt* v1)))
(define init-cont4 (lambda-cont4 (v1 v2 v3 v4) (halt* v1)))
(define init-handler (lambda-handler (e) (halt* (list 'exception e))))
(define init-handler2 (lambda-handler2 (e fail) (halt* (list 'exception e))))
(define init-fail (lambda-fail () (halt* "no more choices")))

(define scan-string
  (lambda (input)
    (scan-input input "stdin" init-handler2 init-fail init-cont2)))

(define scan-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail init-cont2)))

(define aread-string
  (lambda (input)
    (aread-datum input "stdin" init-handler2 init-fail init-cont3)))

(define* aread-datum
  (lambda (input src handler fail k)  ;; k receives 3 args: sexp, tokens-left, fail
    (scan-input input src handler fail
      (lambda-cont2 (tokens fail)
        (read-sexp tokens src handler fail
          (lambda-cont4 (sexp end tokens-left fail)
            (if (token-type? (first tokens-left) 'end-marker)
              (k sexp tokens-left fail)
	      (read-error "tokens left over" tokens-left src handler fail))))))))

(define aread-file
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
        (aread-file-loop tokens filename init-handler2 init-fail init-cont2)))))

(define* aread-file-loop
  (lambda (tokens src handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
        (lambda-cont4 (sexp end tokens-left fail)
	  (aread-file-loop tokens-left src handler fail
	    (lambda-cont2 (sexps fail)
	      (k (cons sexp sexps) fail))))))))

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

;; Calysto Scheme parser
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/jmarshall
;; doug.blank@gmail.com
;; https://github.com/dsblank

(load "transformer-macros.ss")

;;--------------------------------------------------------------------------
;; List structure parser

(load "petite-init.ss")
(load "define-datatype.ss")

(load "reader-cps.ss")
(load "unifier-cps.ss")

(define *use-lexical-address* #t)

;; for the macro environment
(load "environments-cps.ss")

;;--------------------------------------------------------------------------
;; these definitions enable parser-cps.ss to be run directly in Petite
;; Chez Scheme, because parser-cps.ss uses environments-cps.ss, which
;; relies on these functions being defined.

(define-native dlr-proc? (lambda (x) #f))
(define-native dlr-apply apply)
(define-native dlr-func (lambda (x) x))
(define-native callback (lambda args #f))
(define-native dlr-env-contains (lambda (x) #f))
(define-native dlr-env-lookup (lambda (x) #f))
(define-native dlr-object? (lambda (x) #f))
(define-native dlr-lookup-components (lambda (x y) #f))
(define-native set-global-value! (lambda (var x) #f))
(define-native set-global-docstring! (lambda (var x) #f))
(define-native printf-prim printf)
(define-native import-native (lambda ignore #f))
(define-native iterator? (lambda ignore #f))
(define-native get_type (lambda (x) 'unknown))

;;--------------------------------------------------------------------------
;; The core grammar
;;
;; <exp> ::= <literal>
;;         | <vector>
;;         | (quote <datum>)
;;         | (quasiquote <datum>)
;;         | <var>
;;         | (if <exp> <exp> <exp>)
;;         | (set! <var> <exp>)
;;         | (func <exp>)
;;         | (define <var> <exp>)
;;         | (define <var> <docstring> <exp>)
;;         | (define-syntax <keyword> (<pattern> <pattern>) ...)
;;         | (define-syntax <keyword> <lambda>)
;;         | (define-tests <var> <assertion> ...)
;;         | (begin <exp> ...)
;;         | (lambda (<formal> ...) <exp> ...)
;;         | (lambda <formal> <exp> ...)
;;         | (trace-lambda name (<formal> ...) <exp> ...)
;;         | (trace-lambda name <formal> <exp> ...)
;;         | (<exp> <exp> ...)
;;         | (try <body> (catch <var> <exp> ...))
;;         | (try <body> (finally <exp> ...))
;;         | (try <body> (catch <var> <exp> ...) (finally <exp> ...))
;;         | (raise <exp>)
;;         | (choose <exp> ...)

;;--------------------------------------------------------------------------

(define id?
  (lambda (exp)
    (or (symbol? exp)
	(association? exp))))

(define-datatype aexpression aexpression?
  (lit-aexp
    (datum anything?)
    (info source-info?))
  (var-aexp
    (id symbol?)
    (info source-info?))
  (lexical-address-aexp
    (depth number?)
    (offset number?)
    (id symbol?)
    (info source-info?))
  (if-aexp
    (test-aexp aexpression?)
    (then-aexp aexpression?)
    (else-aexp aexpression?)
    (info source-info?))
  (help-aexp
    (var symbol?)
    (var-info source-info?)
    (info source-info?))
  (association-aexp
    (var symbol?)
    (exp aexpression?)
    (info source-info?))
  (assign-aexp
    (var symbol?)
    (rhs-exp aexpression?)
    (var-info source-info?)
    (info source-info?))
  (func-aexp
    (exp aexpression?)
    (info source-info?))
  (callback-aexp
    (exp aexpression?)
    (info source-info?))
  (define-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define!-aexp
    (id symbol?)
    (docstring string?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define-syntax-aexp
    (name symbol?)
    (clauses (list-of define-syntax-clause?))
    (aclauses list-of-define-syntax-clauses?^)
    (info source-info?))
  ;; new
  (define-syntax-transformer-aexp
    (name symbol?)
    (rhs-exp aexpression?)
    (info source-info?))
  (define-tests-aexp
    (name symbol?)
    (aclauses (list-of aexpression?))
    (info source-info?))
  (run-tests-aexp
    (tests (list-of list?)))
  (begin-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  (lambda-aexp
    (formals (list-of id?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-lambda-aexp
    (formals (list-of id?))
    (runt id?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (trace-lambda-aexp
    (name symbol?)
    (formals (list-of id?))
    (bodies (list-of aexpression?))
    (info source-info?))
  (mu-trace-lambda-aexp
    (name symbol?)
    (formals (list-of id?))
    (runt id?)
    (bodies (list-of aexpression?))
    (info source-info?))
  (app-aexp
    (operator aexpression?)
    (operands (list-of aexpression?))
    (info source-info?))
  (try-catch-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (info source-info?))
  (try-finally-aexp
    (body aexpression?)
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (try-catch-finally-aexp
    (body aexpression?)
    (catch-var symbol?)
    (catch-exps (list-of aexpression?))
    (finally-exps (list-of aexpression?))
    (info source-info?))
  (raise-aexp
    (exp aexpression?)
    (info source-info?))
  (choose-aexp
    (exps (list-of aexpression?))
    (info source-info?))
  )

(define head
  (lambda (formals)
    (cond
      ((symbol? formals) '())
      ((association? formals) '())
      ((pair? (cdr formals)) (cons (car formals) (head (cdr formals))))
      (else (list (car formals))))))

(define last
  (lambda (formals)
    (cond
      ((symbol? formals) formals)
      ((association? formals) formals)
      ((pair? (cdr formals)) (last (cdr formals)))
      (else (cdr formals)))))

(define anything?
  (lambda (datum) #t))

(define application?^
  (lambda (asexp)
    (and (list?^ asexp)
	 (not (null?^ asexp))
	 (not (reserved-keyword? (untag-atom^ (car^ asexp)))))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x)
	 (not (eq? (memq x (get-reserved-keywords)) #f)))))

(define get-reserved-keywords
  (lambda ()
    '(quote func define! quasiquote lambda if set! define begin cond and or
	    let let* letrec case record-case try catch finally raise
	    define-syntax choose define-datatype cases trace-lambda)))

(define mit-style-define?^
  (lambda (asexp)
    (and (define?^ asexp)
	 (pair?^ (cadr^ asexp)))))

;; used in aunparse
(define literal?
  (lambda (datum)
    (or (number? datum)
	(boolean? datum)
	(null? datum)
	(char? datum)
	(string? datum))))

(define literal?^
  (lambda (asexp)
    (and (eq? (car asexp) atom-tag)
	 (or (number? (untag-atom^ asexp))
	     (boolean? (untag-atom^ asexp))
	     (null? (untag-atom^ asexp))
	     (char? (untag-atom^ asexp))
	     (string? (untag-atom^ asexp))))))

(define syntactic-sugar?^
  (lambda (asexp)
    (and (pair?^ asexp)
	 (symbol?^ (car^ asexp))
	 (in-first-frame? (untag-atom^ (car^ asexp)) macro-env))))
;;	 (true? (search-env macro-env (untag-atom^ (car^ asexp)))))))

(define-native tagged-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (car^ asexp) keyword)))))

(define-native tagged-list-or^
  (lambda (keyword1 keyword2 op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (or (eq?^ (car^ asexp) keyword1)
	       (eq?^ (car^ asexp) keyword2))))))

(define-native tagged2-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (cadr^ asexp) keyword)))))

(define list-of-test-groups?^
  (lambda (x)
    (or (null?^ x)
	(and (pair?^ x)
	     (atom?^ (car^ x))
	     (or (integer? (untag-atom^ (car^ x)))
		 (string? (untag-atom^ (car^ x))))
	     (list-of-test-groups?^ (cdr^ x))))))

(define quote?^ (tagged-list^ 'quote = 2))
(define quasiquote?^ (tagged-list^ 'quasiquote = 2))
(define unquote?^ (tagged-list^ 'unquote >= 2))  ;; >= for alan bawden's qq-expand algorithm
(define unquote-splicing?^ (tagged-list^ 'unquote-splicing >= 2))
(define if-then?^ (tagged-list^ 'if = 3))
(define if-else?^ (tagged-list^ 'if = 4))
(define help?^ (tagged-list^ 'help = 2))
(define association?^ (tagged2-list^ ': = 3))
(define assignment?^ (tagged-list^ 'set! = 3))
(define func?^ (tagged-list^ 'func = 2))
(define callback?^ (tagged-list^ 'callback = 2))
(define define?^ (tagged-list^ 'define >= 3))
(define define!?^ (tagged-list^ 'define! >= 3))
(define define-syntax?^ (tagged-list^ 'define-syntax >= 3))
(define define-var^ (lambda (x) (untag-atom^ (cadr^ x))))
(define define-docstring^ (lambda (x) (untag-atom^ (caddr^ x))))
(define begin?^ (tagged-list^ 'begin >= 2))
(define lambda?^ (tagged-list-or^ 'lambda 'λ >= 3))
(define lambda-no-defines?^ (tagged-list^ 'lambda-no-defines >= 3))
;;(define trace-lambda?^ (tagged-list^ 'trace-lambda >= 4))
(define trace-lambda-no-defines?^ (tagged-list^ 'trace-lambda-no-defines >= 4))
(define raise?^ (tagged-list^ 'raise = 2))
(define choose?^ (tagged-list^ 'choose >= 1))
(define try?^ (tagged-list^ 'try >= 2))
(define try-body^ (lambda (x) (cadr^ x)))
(define catch?^ (tagged-list^ 'catch >= 3))
(define catch-var^ (lambda (x) (untag-atom^ (cadr^ (caddr^ x)))))
(define catch-exps^ (lambda (x) (cddr^ (caddr^ x))))
(define finally?^ (tagged-list^ 'finally >= 2))
(define try-finally-exps^ (lambda (x) (cdr^ (caddr^ x))))
(define try-catch-finally-exps^ (lambda (x) (cdr^ (cadddr^ x))))
(define define-tests?^ (tagged-list^ 'define-tests >= 2))
(define run-tests?^ (tagged-list^ 'run-tests >= 1))

(define* aparse
  (lambda (adatum senv handler fail k)   ;; k receives 2 args: aexp, fail
    (let ((info (get-source-info adatum)))
      (cond
	((literal?^ adatum) (k (lit-aexp (untag-atom^ adatum) info) fail))
	((symbol?^ adatum)
	 (if *use-lexical-address*
	     (get-lexical-address (untag-atom^ adatum) senv 0 info fail k)
	     (k (var-aexp (untag-atom^ adatum) info) fail)))
	((vector?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp v info) fail))))
	((quote?^ adatum)
	 (unannotate-cps adatum
	   (lambda-cont (v)
	     (k (lit-aexp (cadr v) info) fail))))
	((quasiquote?^ adatum)
	 (qq-expand-cps (cadr^ adatum) 0
	   (lambda-cont (v)
	     (annotate-cps v 'none
	       (lambda-cont (expansion)
		 (if (original-source-info? adatum)
		   (aparse (replace-info expansion (snoc 'quasiquote info)) senv handler fail k)
		   (aparse (replace-info expansion info) senv handler fail k)))))))
	((unquote?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((unquote-splicing?^ adatum) (aparse-error "misplaced" adatum handler fail))
	((syntactic-sugar?^ adatum)
	 (expand-once^ adatum handler fail
	   (lambda-cont2 (expansion fail)
	     (aparse expansion senv handler fail k))))
	((if-then?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (if-aexp v1 v2 (lit-aexp #f 'none) info) fail))))))
	((if-else?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (aparse (cadddr^ adatum) senv handler fail
		   (lambda-cont2 (v3 fail)
		     (k (if-aexp v1 v2 v3 info) fail))))))))
	((help?^ adatum)
	   (let ((var-info (get-source-info (cadr^ adatum))))
	     (k (help-aexp (untag-atom^ (cadr^ adatum)) var-info info) fail)))
	((assignment?^ adatum)
	 (aparse (caddr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (assign-aexp (untag-atom^ (cadr^ adatum)) v var-info info) fail)))))
	((association?^ adatum)
	 (aparse (caddr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (let ((var-info (get-source-info (cadr^ adatum))))
	       (k (association-aexp (untag-atom^ (car^ adatum)) v var-info info) fail)))))
	((func?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (e fail)
	     (k (func-aexp e info) fail))))
	((callback?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (e fail)
	     (k (callback-aexp e info) fail))))
	((define?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((and (= (length^ adatum) 3)
		 (symbol?^ (cadr^ adatum))) ;; (define <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4)
		 (symbol?^ (cadr^ adatum))
		 (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define!?^ adatum)
	 (cond
	   ((mit-style-define?^ adatum)
	    (mit-define-transformer^ adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (aparse (replace-info expansion info) senv handler fail k))))))
	   ((= (length^ adatum) 3) ;; (define! <var> <body>)
	    (aparse (caddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) "" body info) fail))))
	   ((and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define! <var> <docstring> <body>)
	    (aparse (cadddr^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(k (define!-aexp (define-var^ adatum) (define-docstring^ adatum) body info) fail))))
	   (else (aparse-error "bad concrete syntax:" adatum handler fail))))
	((define-syntax?^ adatum)
	 (let ((name (define-var^ adatum)))
	   (if (lambda?^ (caddr^ adatum))
	     (aparse (caddr^ adatum) senv handler fail
	       (lambda-cont2 (body fail)
		 (k (define-syntax-transformer-aexp name body info) fail)))
	     (let ((aclauses (cddr^ adatum)))
	       (unannotate-cps aclauses
		 (lambda-cont (clauses)
		   (k (define-syntax-aexp name clauses aclauses info) fail)))))))
	((define-tests?^ adatum)
	 (let ((name (define-var^ adatum))
	       (aclauses (cddr^ adatum)))
	   (aparse-all aclauses senv handler fail
	     (lambda-cont2 (exps fail)
	       (k (define-tests-aexp name exps info) fail)))))
	((run-tests?^ adatum)
	 (let ((args (cdr^ adatum)))
	   (cond
	     ((null?^ args) (k (run-tests-aexp '()) fail))
	     ((and (symbol?^ (car^ args)) (list-of-test-groups?^ (cdr^ args)))
	      (aparse-unit-tests (list^ args) handler fail
		(lambda-cont2 (tests fail)
		  (k (run-tests-aexp tests) fail))))
	     (else (aparse-unit-tests args handler fail
		     (lambda-cont2 (tests fail)
		       (k (run-tests-aexp tests) fail)))))))
	((begin?^ adatum)
	 (cond
	   ((null?^ (cdr^ adatum)) (aparse-error "bad concrete syntax:" adatum handler fail))
	   ((null?^ (cddr^ adatum)) (aparse (cadr^ adatum) senv handler fail k))
	   (else (aparse-all (cdr^ adatum) senv handler fail
		   (lambda-cont2 (exps fail)
		     (k (begin-aexp exps info) fail))))))
	((lambda-no-defines?^ adatum)
	 (unannotate-cps (cadr^ adatum)
	    (lambda-cont (formals)
	        (let ((formals-list
			(if (and (list? formals) (not (association? formals)))
			    formals
			    (cons (last formals) (head formals)))))
		  (aparse-all (cddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (and (list? formals) (not (association? formals)))
			     (k (lambda-aexp formals bodies info) fail)
			     (k (mu-lambda-aexp (head formals) (last formals) bodies info) fail))))))))
	((trace-lambda-no-defines?^ adatum)
	 (unannotate-cps (caddr^ adatum)
	      (lambda-cont (formals)
		(let ((formals-list
			(if (and (list? formals) (not (association? formals)))
			    formals
			    (cons (last formals) (head formals))))
		      (name (untag-atom^ (cadr^ adatum))))
		  (aparse-all (cdddr^ adatum) (cons formals-list senv) handler fail
		      (lambda-cont2 (bodies fail)
			 (if (and (list? formals) (not (association? formals)))
			     (k (trace-lambda-aexp name formals bodies info) fail)
			     (k (mu-trace-lambda-aexp name (head formals) (last formals) bodies info) fail))))))))
	((try?^ adatum)
	 (cond
	   ;; (try <body>) - removed because redefining try gave silent error
	   ;; see: https://github.com/Calysto/calysto_scheme/issues/35
	   ;; ((= (length^ adatum) 2)  (aparse (try-body^ adatum) senv handler fail k))

	   ;; (try <body> (catch <var> <exp> ...))
	   ((and (= (length^ adatum) 3) (catch?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		 (let ((cvar (catch-var^ adatum)))
		   (aparse-all (catch-exps^ adatum) (cons (list cvar) senv) handler fail
		     (lambda-cont2 (cexps fail)
		      (k (try-catch-aexp body cvar cexps info) fail)))))))
	   ;; (try <body> (finally <exp> ...))
	   ((and (= (length^ adatum) 3) (finally?^ (caddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(aparse-all (try-finally-exps^ adatum) senv handler fail
		  (lambda-cont2 (fexps fail)
		    (k (try-finally-aexp body fexps info) fail))))))
	   ;; (try <body> (catch <var> <exp> ...) (finally <exp> ...))
	   ((and (= (length^ adatum) 4) (catch?^ (caddr^ adatum)) (finally?^ (cadddr^ adatum)))
	    (aparse (try-body^ adatum) senv handler fail
	      (lambda-cont2 (body fail)
		(let ((cvar (catch-var^ adatum)))
		  (aparse-all (catch-exps^ adatum) (cons (list cvar) senv) handler fail
		    (lambda-cont2 (cexps fail)
		      (aparse-all (try-catch-finally-exps^ adatum) senv handler fail
			(lambda-cont2 (fexps fail)
			  (k (try-catch-finally-aexp body cvar cexps fexps info) fail)))))))))
	   (else (aparse-error "bad try syntax:" adatum handler fail))))
	((raise?^ adatum)
	 (aparse (cadr^ adatum) senv handler fail
	   (lambda-cont2 (v fail)
	     (k (raise-aexp v info) fail))))
	((choose?^ adatum)
	 (aparse-all (cdr^ adatum) senv handler fail
	   (lambda-cont2 (exps fail)
	     (k (choose-aexp exps info) fail))))
	((application?^ adatum)
	 (aparse (car^ adatum) senv handler fail
	   (lambda-cont2 (v1 fail)
	     (aparse-all (cdr^ adatum) senv handler fail
	       (lambda-cont2 (v2 fail)
		 (k (app-aexp v1 v2 info) fail))))))
	(else (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define* aparse-unit-tests
  (lambda (args handler fail k)
    (cond
      ((null?^ args) (k '() fail))
      ((symbol?^ (car^ args))
       (aparse-unit-tests (cdr^ args) handler fail
	 (lambda-cont2 (tests fail)
	   (k (cons (list (untag-atom^ (car^ args))) tests) fail))))
      ((and (list?^ (car^ args))
	    (not (null?^ (car^ args)))
	    (symbol?^ (caar^ args))
	    (list-of-test-groups?^ (cdar^ args)))
       (aparse-unit-tests (cdr^ args) handler fail
	 (lambda-cont2 (tests fail)
	   (unannotate-cps (car^ args)
	     (lambda-cont (test)
	       (k (cons test tests) fail))))))
      (else (aparse-error "bad unit test syntax:" (car^ args) handler fail)))))

(define* aparse-all
  (lambda (adatum-list senv handler fail k)
    (if (null?^ adatum-list)
      (k '() fail)
      (aparse (car^ adatum-list) senv handler fail
	(lambda-cont2 (a fail)
	  (aparse-all (cdr^ adatum-list) senv handler fail
	    (lambda-cont2 (b fail)
	      (k (cons a b) fail))))))))

(define* aparse-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (unannotate-cps adatum
	(lambda-cont (datum)
	  (handler (make-exception "ParseError" (format "~a ~a" msg datum)
			 (get-srcfile info)
			 (get-start-line info)
			 (get-start-char info))
		   fail))))))

;; used once in interpreter-cps.ss
(define* aparse-sexps
  (lambda (tokens src senv handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k '() fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (adatum end tokens-left fail)
	  (aparse adatum senv handler fail
	    (lambda-cont2 (v1 fail)
	      (aparse-sexps tokens-left src senv handler fail
		(lambda-cont2 (v2 fail)
		  (k (cons v1 v2) fail))))))))))

(define* get-lexical-address
  (lambda (id senv depth info fail k)
    (cond
      ((null? senv) (k (var-aexp id info) fail))   ;; free!
      ((memq id (car senv))
       (get-lexical-address-offset id (car senv) depth 0 info fail k))
      (else (get-lexical-address id (cdr senv) (+ depth 1) info fail k)))))

(define* get-lexical-address-offset
  (lambda (id contours depth offset info fail k)
    (if (eq? (car contours) id)
      (k (lexical-address-aexp depth offset id info) fail)
      (get-lexical-address-offset id (cdr contours) depth (+ offset 1) info fail k))))

;;(define get-lexical-address
;;  ;; given an environment and variable id, return the depth of the
;;  ;; frame and offset OR return it as a var-exp signifying it as an
;;  ;; unbound variable
;;  (lambda (senv id info)
;;    (get-lexical-address-frames (frames senv) id 0 0 info)))

;;(define get-lexical-address-frames
;;  ;; given a list of frames, get the lexical address of variable
;;  (lambda (frames variable depth offset info)
;;    (cond
;;     ((null? frames) (var-aexp variable info)) ;; free!
;;     (else (let ((result (get-lexical-address-frame (car frames) variable depth offset info)))
;;	     (if (not (car result))
;;		 (get-lexical-address-frames (cdr frames) variable (+ 1 depth) 0 info)
;;		 (cadr result)))))))

;;(define get-lexical-address-frame
;;  ;; returns (#t pos) or (#f) signifying bound or free, respectively
;;  (lambda (frame variable depth offset info)
;;    (cond
;;     ((empty-frame? frame) (list #f)) ;; not in this frame
;;     ((>= offset (vector-length frame)) (list #f)) ;; not here
;;     ((eq? (binding-variable (vector-ref frame offset)) variable)
;;      (list #t (lexical-address-aexp depth offset variable info)))
;;     (else (get-lexical-address-frame frame variable depth (+ 1 offset) info)))))

;;--------------------------------------------------------------------------
;; Macro support

;; transformer macros:

(define lambda-transformer^
  (lambda-macro (adatum handler fail k)
    (if (< (length^ adatum) 3)
	(aparse-error "bad lambda expression:" adatum handler fail)
	(let ((formals (cadr^ adatum))
	      (bodies (cddr^ adatum)))
	  (get-internal-defines^ bodies adatum handler fail
	    (lambda-cont2 (defines bodies2)
	      (if (null? defines)
		  (k `(lambda-no-defines ,formals ,@(at^ bodies2)))
		  (create-letrec-bindings^ defines handler fail
		    (lambda-cont (bindings)
		      (k `(lambda-no-defines ,formals (letrec ,bindings ,@(at^ bodies2)))))))))))))

(define trace-lambda-transformer^
  (lambda-macro (adatum handler fail k)
    (if (< (length^ adatum) 4)
      (aparse-error "bad trace-lambda expression:" adatum handler fail)
      (let ((name (cadr^ adatum))
	    (formals (caddr^ adatum))
	    (bodies (cdddr^ adatum)))
	(get-internal-defines^ bodies adatum handler fail
	  (lambda-cont2 (defines bodies2)
	    (if (null? defines)
		(k `(trace-lambda-no-defines ,name ,formals ,@(at^ bodies2)))
		(create-letrec-bindings^ defines handler fail
		  (lambda-cont (bindings)
		    (k `(trace-lambda-no-defines ,name ,formals (letrec ,bindings ,@(at^ bodies2)))))))))))))

(define get-internal-defines^
  (lambda (bodies adatum handler fail k)  ;; k receives 2 args: defines, bodies2
    (cond
      ((null?^ bodies)
       (aparse-error "no body expressions found for" adatum handler fail))
      ((define?^ (car^ bodies))
       (get-internal-defines^ (cdr^ bodies) adatum handler fail
	 (lambda-cont2 (v1 v2)
	   (k (cons (car^ bodies) v1) v2))))
      (else (any-internal-defines?^ (cdr^ bodies)
	      (lambda-cont (v)
		(if v
		  (aparse-error "misplaced define in" adatum handler fail)
		  (k '() bodies))))))))

(define any-internal-defines?^
  (lambda (exps k)
    (if (null?^ exps)
      (k #f)
      (if (define?^ (car^ exps))
	  (k #t)
	  (any-internal-defines?^ (cdr^ exps) k)))))

(define create-letrec-bindings^
  (lambda (defines handler fail k)
    (if (null? defines)
      (k '())
      (create-letrec-bindings^ (cdr defines) handler fail
	(lambda-cont (bindings)
	  (get-define-var-and-exp^ (car defines) handler fail
	    (lambda-cont2 (var exp)
	      (k (cons `(,var ,exp) bindings)))))))))

(define get-define-var-and-exp^
  (lambda (adatum handler fail k)
    (if (mit-style-define?^ adatum)
	(let ((name (caadr^ adatum))
	      (formals (cdadr^ adatum))
	      (bodies (cddr^ adatum)))
	  (k name `(lambda ,formals ,@(at^ bodies))))
	(if (= (length^ adatum) 3) ;; (define <var> <body>)
	    (let ((name (define-var^ adatum))
		  (exp (caddr^ adatum)))
	      (k name exp))
	    (if (and (= (length^ adatum) 4) (string?^ (caddr^ adatum))) ;; (define <var> <docstring> <body>)
	      (let ((name (define-var^ adatum))
		    (exp (cadddr^ adatum)))
		(k name exp))
	      (aparse-error "bad concrete syntax:" adatum handler fail))))))

(define let-transformer^
  (lambda-macro (adatum handler fail k)
    (if (symbol?^ (cadr^ adatum))
      ;; named let
      (let* ((name (cadr^ adatum))
	     (bindings (caddr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cdddr^ adatum)))
	(k `(letrec ((,name (lambda ,vars ,@(at^ bodies)))) (,name ,@(at^ exps)))))
      ;; ordinary let
      (let* ((bindings (cadr^ adatum))
	     (vars (map^ car^ bindings))
	     (exps (map^ cadr^ bindings))
	     (bodies (cddr^ adatum)))
	(k `((lambda ,vars ,@(at^ bodies)) ,@(at^ exps)))))))

(define letrec-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((decls (cadr^ adatum))
	   (vars (map^ car^ decls))
	   (procs (map^ cadr^ decls))
	   (bodies (cddr^ adatum)))
      (create-letrec-assignments^ vars procs
	(lambda-cont2 (bindings assigns)  ;; bindings and assigns are unannotated
	  (k `(let ,bindings ,@assigns ,@(at^ bodies))))))))

(define* create-letrec-assignments^
  (lambda (vars procs k2)
    (if (null?^ vars)
      (k2 '() '())
      (create-letrec-assignments^ (cdr^ vars) (cdr^ procs)
	(lambda-cont2 (bindings assigns)
	  (k2 (cons `(,(car^ vars) 'undefined) bindings)
	      (cons `(set! ,(car^ vars) ,(car^ procs)) assigns)))))))

(define mit-define-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((name (caadr^ adatum))
	  (formals (cdadr^ adatum))
	  (bodies (cddr^ adatum)))
      (k `(define ,name (lambda ,formals ,@(at^ bodies)))))))

(define and-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#t))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(if ,(car^ exps) (and ,@(at^ (cdr^ exps))) #f)))))))

;; avoids variable capture
(define or-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exps (cdr^ adatum)))
      (cond
	((null?^ exps) (k '#f))
	((null?^ (cdr^ exps)) (k (car^ exps)))
	(else (k `(let ((bool ,(car^ exps))
			(else-code (lambda () (or ,@(at^ (cdr^ exps))))))
		    (if bool bool (else-code)))))))))

(define* amacro-error
  (lambda (msg adatum handler fail)
    (let ((info (get-source-info adatum)))
      (handler (make-exception "MacroError" msg (get-start-line info)
		     (get-srcfile info)
		     (get-start-char info))
	       fail))))

;; correctly handles single-expression clauses and avoids variable capture
(define cond-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((clauses (cdr^ adatum)))
      (if (null?^ clauses)
	(amacro-error "empty (cond) expression" adatum handler fail)
	(let ((first-clause (car^ clauses))
	      (other-clauses (cdr^ clauses)))
	  (if (or (null?^ first-clause) (not (list?^ first-clause)))
	    (amacro-error "improper cond clause" first-clause handler fail)
	    (let ((test-exp (car^ first-clause))
		  (then-exps (cdr^ first-clause)))
	      (cond
		((eq?^ test-exp 'else)
		 (cond
		   ((null?^ then-exps)
		    (amacro-error "improper else clause" first-clause handler fail))
		   ((null?^ (cdr^ then-exps)) (k (car^ then-exps)))
		   (else (k `(begin ,@(at^ then-exps))))))
		((null?^ then-exps)
		 (if (null?^ other-clauses)
		   (k `(let ((bool ,test-exp))
			 (if bool bool)))
		   (k `(let ((bool ,test-exp)
			     (else-code (lambda () (cond ,@(at^ other-clauses)))))
			 (if bool bool (else-code))))))
		((eq?^ (car^ then-exps) '=>)
		 (cond
		   ((null?^ (cdr^ then-exps))
		    (amacro-error "improper => clause" first-clause handler fail))
		   ((null?^ other-clauses)
		    (k `(let ((bool ,test-exp)
			      (th (lambda () ,(cadr^ then-exps))))
			  (if bool ((th) bool)))))
		   (else (k `(let ((bool ,test-exp)
				   (th (lambda () ,(cadr^ then-exps)))
				   (else-code (lambda () (cond ,@(at^ other-clauses)))))
			       (if bool ((th) bool) (else-code)))))))
		((null?^ other-clauses)
		 (if (null?^ (cdr^ then-exps))
		   (k `(if ,test-exp ,(car^ then-exps)))
		   (k `(if ,test-exp (begin ,@(at^ then-exps))))))
		((null?^ (cdr^ then-exps))
		 (k `(if ,test-exp ,(car^ then-exps) (cond ,@(at^ other-clauses)))))
		(else (k `(if ,test-exp (begin ,@(at^ then-exps)) (cond ,@(at^ other-clauses)))))))))))))

(define let*-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((bindings (cadr^ adatum))
	  (bodies (cddr^ adatum)))
      (nest-let*-bindings^ bindings bodies k))))

(define* nest-let*-bindings^
  (lambda (bindings bodies k)
    (if (or (null?^ bindings)
	    (null?^ (cdr^ bindings)))
	(k `(let ,bindings ,@(at^ bodies)))
	(nest-let*-bindings^ (cdr^ bindings) bodies
	  (lambda-cont (v)
	    (k `(let (,(car^ bindings)) ,v)))))))

;; avoids variable capture
(define case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (case-clauses->cond-clauses^ 'r clauses
	  (lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	    (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* case-clauses->simple-cond-clauses^
  (lambda (var clauses k)
    (if (null?^ clauses)
      (k '())
      (case-clauses->simple-cond-clauses^ var (cdr^ clauses)
	(lambda-cont (new-clauses)
	  (let ((clause (car^ clauses)))
	    (cond
	      ((eq?^ (car^ clause) 'else)
	       (k (cons clause new-clauses)))
	      ((symbol?^ (car^ clause))
	       (k (cons `((eq? ,var ',(car^ clause)) ,@(at^ (cdr^ clause))) new-clauses)))
	      (else (k (cons `((memq ,var ',(car^ clause)) ,@(at^ (cdr^ clause)))
			     new-clauses))))))))))

(define* case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons '(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((eq? ,var ',(car^ clause)) (apply ,name '())) new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda () ,@(at^ (cdr^ clause)))) bindings)
		      (cons `((memq ,var ',(car^ clause)) (apply ,name '())) new-clauses)))))))))))

;; avoids variable capture
(define record-case-transformer^
  (lambda-macro (adatum handler fail k)
    (let ((exp (cadr^ adatum))
	  (clauses (cddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings) (cond ,@new-clauses))))))))

(define* record-case-clauses->cond-clauses^
  (lambda (var clauses k2)
    (if (null?^ clauses)
      (k2 '() '())
      (record-case-clauses->cond-clauses^ var (cdr^ clauses)
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (let ((clause (car^ clauses)))
	    (if (eq?^ (car^ clause) 'else)
	      (k2 (cons `(else-code (lambda () ,@(at^ (cdr^ clause)))) bindings)
		  (cons `(else (else-code)) new-clauses))
	      (if (symbol?^ (car^ clause))
		(let ((name (car^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((eq? (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))
		(let ((name (caar^ clause)))
		  (k2 (cons `(,name (lambda ,(cadr^ clause) ,@(at^ (cddr^ clause)))) bindings)
		      (cons `((memq (car ,var) ',(car^ clause)) (apply ,name (cdr ,var)))
			    new-clauses)))))))))))

;;----------------------------------------------------------------------------

(define define-datatype-transformer^
  (lambda-macro (datatype-def handler fail k)
    (let* ((datatype-name (cadr^ datatype-def))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ datatype-name) "?"))))
      (if (not (eq?^ (caddr^ datatype-def) type-tester-name))
	(amacro-error
	  (format "datatype tester predicate not named ~a" type-tester-name)
	  (caddr^ datatype-def) handler fail)
	(let ((variants (cdddr^ datatype-def)))
	  (make-dd-variant-constructors^ variants
	    (lambda-cont2 (variant-names constructor-defs)
	      (let ((tester-def
		      `(define ,type-tester-name
			 (lambda (x)
			   (and (pair? x) (not (not (memq (car x) ',variant-names))))))))
		(k `(begin ,tester-def ,@constructor-defs))))))))))

(define* make-dd-variant-constructors^
  (lambda (variants k2)
    (if (null?^ variants)
      (k2 '() '())
      (make-dd-variant-constructor^ (car^ variants)
	(lambda-cont2 (name def)
	  (make-dd-variant-constructors^ (cdr^ variants)
	    (lambda-cont2 (names defs)
	      (k2 (cons name names) (cons def defs)))))))))

(define* make-dd-variant-constructor^
  (lambda (variant k2)
    (let ((name (car^ variant))
	  (fields (cdr^ variant)))
      (verify-dd-constructor-fields^ name fields 'args
	(lambda-cont (verify-code)
	  (let ((constructor-def
		  `(define ,name
		     (lambda args
		       (if (= (length args) ,(length^ fields))
			   ,verify-code
			   (error ',name "wrong number of arguments"))))))
	    (k2 name constructor-def)))))))

(define* verify-dd-constructor-fields^
  (lambda (name fields cdrs k)
    (if (null?^ fields)
	(k `(cons ',name args))
	(verify-dd-constructor-fields^ name (cdr^ fields) `(cdr ,cdrs)
	  (lambda-cont (verify-code)
	    (k `(if (,(cadar^ fields) (car ,cdrs))
		    ,verify-code
		    (error ',name "~a is not of type ~a" (car ,cdrs) ',(cadar^ fields)))))))))

(define cases-transformer^
  (lambda-macro (adatum handler fail k)
    (let* ((type-name (cadr^ adatum))
	   (type-tester-name
	     (string->symbol (string-append (symbol->string^ type-name) "?")))
	   (exp (caddr^ adatum))
	   (clauses (cdddr^ adatum)))
      (record-case-clauses->cond-clauses^ 'r clauses
	(lambda-cont2 (bindings new-clauses)  ;; bindings and new-clauses are unannotated
	  (k `(let ((r ,exp) ,@bindings)
		(if (not (,type-tester-name r))
		  (error 'cases "~a is not a valid ~a" r ',type-name)
		  (cond ,@new-clauses)))))))))

;;----------------------------------------------------------------------------

(define make-macro-env^
  (lambda ()
    (make-initial-environment
      (list 'lambda 'λ 'trace-lambda 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'define-datatype 'cases)
      (list lambda-transformer^
	    lambda-transformer^
	    trace-lambda-transformer^
	    and-transformer^
	    or-transformer^
	    cond-transformer^
	    let-transformer^
	    letrec-transformer^
	    let*-transformer^
	    case-transformer^
	    record-case-transformer^
	    define-datatype-transformer^
	    cases-transformer^
	    )
      (list (string-append "(lambda ...) - lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(lambda ...) - lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (lambda (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(trace-lambda name ...) - trace-lambda with internal definitions"
	                   "\n"
			   "Example:\n"
			   "    In  [1]: (trace-lambda name (a b) (define c 3) (list a b c))\n"
			   "    Out [1]: <procedure>\n"
			   )
	    (string-append "(and ...) - short-circuiting `and` macro\n"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (and)\n"
			   "    Out [1]: #t\n"
			   "    In  [2]: (and #t #f)\n"
			   "    Out [2]: #f\n"
			   )
	    (string-append "(or ...) - short-circuiting `or` macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (or)\n"
			   "    Out [1]: #f\n"
			   "    In  [2]: (or #t #f)\n"
			   "    Out [2]: #t\n"
			   )
	    (string-append "(cond (TEST RETURN)...) - conditional evaluation macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (cond ((= 1 2) 3)(else 4))\n"
			   "    Out [1]: 4\n"
			   )
	    (string-append "(let ((VAR VALUE)...)...) - local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (let ((x 3)) x)\n"
			   "    Out [1]: 3\n"
			   )
	    (string-append "(letrec ((VAR VALUE)...)...) - recursive local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [*]: (letrec ((loop (lambda () (loop)))) (loop))\n"
			   )
	    (string-append "(let* ((VAR VALUE)...)...) - cascading local variable macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (let* ((a 1)(b a)(c b)) c)\n"
			   "    Out [1]: 1\n"
			   )
	    (string-append "(case THING (ITEM RETURN)...)) - case macro"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (case 1 (1 2)(3 4))\n"
			   "    Out [1]: 2\n"
			   )
	    (string-append "(record-case ) - record-case macro for define-datatype"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (record-case ddtype (subtype (part...) return)...)\n"
			   )
	    (string-append "(define-datatype NAME NAME? (TYPE (PART TEST))...) - defines new datatypes and support functions (macro)"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (define-datatype e e?)\n"
			   "    In  [1]: (e? 1)\n"
			   "    Out [1]: #f\n"
			   )
	    (string-append "(cases ...) - cases macro for a more flexible case"
			   "\n"
			   "Example:\n"
			   "    In  [1]: (cases 1 ((1 2) 3))\n"
			   "    Out [1]: 3\n"
			   )
	    ))))

(define macro-env 'undefined)

;; pattern macros:

;; used in interpreter-cps.ss
(define make-pattern-macro^
  (lambda (clauses aclauses)
    (list 'pattern-macro clauses aclauses)))

(define pattern-macro?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'pattern-macro))))

(define macro-clauses
  (lambda (macro)
    (cadr macro)))

(define macro-aclauses
  (lambda (macro)
    (caddr macro)))

(define define-syntax-clause?
  (lambda (x)
    (and (list? x)
	 (= (length x) 2)
	 (pattern? (car x))
	 (pattern? (cadr x)))))

(define define-syntax-clause?^
  (lambda (x)
    (and (list?^ x)
	 (= (length^ x) 2)
	 (apattern? (car^ x))
	 (apattern? (cadr^ x)))))

(define apattern?
  (lambda (x)
    (or (aatom? x)
	(and (apair? x)
	     (apattern? (cadr x))
	     (apattern? (caddr x))))))

(define list-of-define-syntax-clauses?^
  (lambda (alist)
    (or (null?^ alist)
	(and (define-syntax-clause?^ (car^ alist))
	     (list-of-define-syntax-clauses?^ (cdr^ alist))))))

(define* expand-once^
  (lambda (adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (let ((macro-keyword (untag-atom^ (car^ adatum))))
      (let ((macro (get-first-frame-value macro-keyword macro-env)))
	(if (pattern-macro? macro)
	    (process-macro-clauses^
	      (macro-clauses macro) (macro-aclauses macro) adatum handler fail
	      (lambda-cont2 (asexp fail)
		(k (replace-info asexp (snoc macro-keyword (get-source-info asexp))) fail)))
	    ;; macro transformer functions take 1-arg continuations:
	    (macro adatum handler fail
	      (lambda-cont (v)
		(annotate-cps v 'none
		  (lambda-cont (expansion)
		    (if (has-source-info? expansion)
		      (k expansion fail)
		      (let ((info (get-source-info adatum)))
			(if (original-source-info? adatum)
			  (k (replace-info expansion (snoc macro-keyword info)) fail)
			  (k (replace-info expansion info) fail)))))))))))))

(define* process-macro-clauses^
  (lambda (clauses aclauses adatum handler fail k)  ;; k receives 2 args: asexp, fail
    (if (null? clauses)
      (aparse-error "no matching clause found for" adatum handler fail)
      (let ((left-pattern (caar clauses))
	    (right-pattern (cadar clauses))
	    (left-apattern (caar^ aclauses))
	    (right-apattern (cadar^ aclauses)))
	(unannotate-cps adatum
	  (lambda-cont (datum)
	    (unify-patterns^ left-pattern datum left-apattern adatum
	      (lambda-cont (subst)
		(if subst
		  (instantiate^ right-pattern subst right-apattern (lambda-cont2 (v av) (k av fail)))
		  (process-macro-clauses^ (cdr clauses) (cdr^ aclauses) adatum handler fail k))))))))))

;;--------------------------------------------------------------------------------------------
;; quasiquote expansion
;;
;; based on Appendix B of Alan Bawden's paper "Quasiquotation in Lisp", with some optimizations
;;
;; this version matches the functionality of Petite's quasiquote expander

;; for testing only
(define qqtest
  (lambda (s)
    (let ((adatum (aread-string s)))
      (if (not (and (list?^ adatum) (= (length^ adatum) 2) (eq?^ (car^ adatum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset))
	(qq-expand-cps (cadr^ adatum) 0
	  (lambda-cont (v)
	    (annotate-cps v 'none
	      (lambda-cont (expansion)
		(replace-info expansion (snoc 'quasiquote (get-source-info adatum)))))))))))

;; expands annotated code
(define* qq-expand-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car^ ax) ,v)))))
	 ((and (unquote?^ ax) (not (null?^ (cdr^ ax))) (null?^ (cddr^ ax))) (k (cadr^ ax)))
	 (else (k `(quote ,ax))))) ;; illegal
      ((vector?^ ax)
       (annotate-cps (vector->list^ ax) 'none
	 (lambda-cont (v)
	   (qq-expand-cps v depth
	     (lambda-cont (v2)
	       (k `(list->vector ,v2)))))))
      ((not (pair?^ ax)) (k `',ax))
      ((null?^ (cdr^ ax)) (qq-expand-list-cps (car^ ax) depth k))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands annotated code
(define* qq-expand-list-cps
  (lambda (ax depth k)
    (cond
      ((quasiquote?^ ax)
       (qq-expand-cps (cdr^ ax) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote?^ ax) (unquote-splicing?^ ax))
       (cond
	 ((> depth 0)
	  (qq-expand-cps (cdr^ ax) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car^ ax) ,v))))))
	 ((unquote?^ ax) (k `(list . ,(cdr^ ax))))
	 ((null?^ (cddr^ ax)) (k (cadr^ ax)))
	 (else (k `(append . ,(cdr^ ax))))))
      ((vector?^ ax)
       (qq-expand-cps ax depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair?^ ax)) (k `'(,ax)))
      ((null?^ (cdr^ ax))
       (qq-expand-list-cps (car^ ax) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps (car^ ax) depth
	      (lambda-cont (v1)
		(qq-expand-cps (cdr^ ax) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;; expands unannotated code
(define* qq-expand-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(cons 'quasiquote ,v)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(cons ',(car x) ,v)))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (k (cadr x)))
	 (else (k `(quote ,x))))) ;; illegal
      ((vector? x)
       (qq-expand-cps_ (vector->list x) depth
	 (lambda-cont (v)
	   (k `(list->vector ,v)))))
      ((not (pair? x)) (k `',x))
      ((null? (cdr x)) (qq-expand-list-cps_ (car x) depth k))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(append ,v1 ,v2))))))))))

;; expands unannotated code
(define* qq-expand-list-cps_
  (lambda (x depth k)
    (cond
      ((quasiquote? x)
       (qq-expand-cps_ (cdr x) (+ depth 1)
	 (lambda-cont (v)
	   (k `(list (cons 'quasiquote ,v))))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0)
	  (qq-expand-cps_ (cdr x) (- depth 1)
	    (lambda-cont (v)
	      (k `(list (cons ',(car x) ,v))))))
	 ((unquote? x) (k `(list . ,(cdr x))))
	 ((null? (cddr x)) (k (cadr x)))
	 (else (k `(append . ,(cdr x))))))
      ((vector? x)
       (qq-expand-cps_ x depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      ((not (pair? x)) (k `'(,x)))
      ((null? (cdr x))
       (qq-expand-list-cps_ (car x) depth
	 (lambda-cont (v)
	   (k `(list ,v)))))
      (else (qq-expand-list-cps_ (car x) depth
	      (lambda-cont (v1)
		(qq-expand-cps_ (cdr x) depth
		  (lambda-cont (v2)
		    (k `(list (append ,v1 ,v2)))))))))))

;;------------------------------------------------------------------------
;; for manual testing only in scheme

(define aunparse
  (lambda (aexp)
    (cases aexpression aexp
      (lit-aexp (datum info)
	(cond
	  ((null? datum) '(quote ()))
	  ((literal? datum) datum)
	  ((vector? datum) datum)
	  (else `(quote ,datum))))
      (var-aexp (id info) id)
      (lexical-address-aexp (depth offset id info) id)
      (if-aexp (test-aexp then-aexp else-aexp info)
	`(if ,(aunparse test-aexp) ,(aunparse then-aexp) ,(aunparse else-aexp)))
      (assign-aexp (var rhs-exp var-info info)
	`(set! ,var ,(aunparse rhs-exp)))
      (func-aexp (exp info)
	`(func ,(aunparse exp)))
      (callback-aexp (exp info)
	`(callback ,(aunparse exp)))
      (define-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define ,id ,(aunparse rhs-exp))
	  `(define ,id ,docstring ,(aunparse rhs-exp))))
      (define!-aexp (id docstring rhs-exp info)
	(if (string=? docstring "")
	  `(define! ,id ,(aunparse rhs-exp))
	  `(define! ,id ,docstring ,(aunparse rhs-exp))))
      (define-syntax-aexp (name clauses aclauses info)
	`(define-syntax ,name ,@clauses))
      (begin-aexp (exps info)
	`(begin ,@(map aunparse exps)))
      (lambda-aexp (formals bodies info)
	`(lambda ,formals ,@(map aunparse bodies)))
      (mu-lambda-aexp (formals runt bodies info)
	`(lambda (,@formals . ,runt) ,@(map aunparse bodies)))
      (app-aexp (operator operands info)
	`(,(aunparse operator) ,@(map aunparse operands)))
      (try-catch-aexp (body catch-var catch-exps info)
	`(try ,(aunparse body) (catch ,catch-var ,@(map aunparse catch-exps))))
      (try-finally-aexp (body finally-exps info)
	`(try ,(aunparse body) (finally ,@(map aunparse finally-exps))))
      (try-catch-finally-aexp (body catch-var catch-exps finally-exps info)
	`(try ,(aunparse body)
	      (catch ,catch-var ,@(map aunparse catch-exps))
	      (finally ,@(map aunparse finally-exps))))
      (raise-aexp (exp info)
	`(raise ,(aunparse exp)))
      (choose-aexp (exps info)
	`(choose ,@(map aunparse exps)))
      (else (error 'aunparse "bad abstract syntax: ~s" aexp)))))

(define expand-macro
  (lambda (transformer sexp)
    (transformer sexp init-handler2 init-fail init-cont)))

;; will be overridden by toplevel-env definition in interpreter-cps.ss
;;(define toplevel-env (make-empty-environment))

(define aparse-string
  (lambda (string)
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (aread-datum string "stdin" init-handler2 init-fail
      (lambda-cont3 (adatum tokens-left fail)
	(aparse adatum (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

(define aparse-file
  (lambda (filename)
    (aget-parsed-sexps filename)))

(define aprint-parsed-sexps
  (lambda (filename)
    (for-each pretty-print (aget-parsed-sexps filename))))

(define aget-parsed-sexps
  (lambda (filename)
    (scan-input (read-content filename) filename init-handler2 init-fail
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens filename (initial-contours toplevel-env) init-handler2 init-fail init-cont2)))))

;;--------------------------------------------------------------------------------------------
;; not used - for possible future reference

;; based on Alan Bawden's paper "Quasiquotation in Lisp"

;; for testing only
(define qq1
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand1 (cadr datum)))))

;; non-CPS Appendix A algorithm, with some optimizations
(define qq-expand1
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1 (qq-expand1 (cadr x))))
      ((unquote? x) (cadr x))
      ((unquote-splicing? x) `(quote ,x))  ;; illegal
      ((pair? x)
       (cond
	 ((null? (cdr x)) (qq-expand1-list (car x)))
	 ((unquote? (car x)) `(cons ,(cadr (car x)) ,(qq-expand1 (cdr x))))
	 (else `(append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x))))))
      ((vector? x) `(list->vector ,(qq-expand1 (vector->list x))))
      (else `(quote ,x)))))

(define qq-expand1-list
  (lambda (x)
    (cond
      ((quasiquote? x) (qq-expand1-list (qq-expand1 (cadr x))))
      ((unquote? x) `(list ,(cadr x)))
      ((unquote-splicing? x) (cadr x))
      ((pair? x)
       (cond
	 ((null? (cdr x)) `(list ,(qq-expand1-list (car x))))
	 ((unquote? (car x)) `(list (cons ,(cadr (car x)) ,(qq-expand1 (cdr x)))))
	 (else `(list (append ,(qq-expand1-list (car x)) ,(qq-expand1 (cdr x)))))))
      ((vector? x) `(list ,(qq-expand1 x)))
      (else `(quote (,x))))))

;; for testing only
(define qq2
  (lambda (s)
    (let ((datum (read-string s)))
      (if (not (and (list? datum) (= (length datum) 2) (eq? (car datum) 'quasiquote)))
	(begin
	  (printf "Not a quasiquote expression!\n")
	  (reset)))
      (qq-expand2 (cadr datum) 0))))

;; non-CPS Appendix B algorithm, with some optimizations
(define qq-expand2
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1))))
	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr x))
	 (else `(quote ,x))))  ;; illegal
      ((vector? x) `(list->vector ,(qq-expand2 (vector->list x) depth)))
      ((not (pair? x)) `',x)
      ((null? (cdr x)) (qq-expand2-list (car x) depth))
      (else `(append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth))))))

(define qq-expand2-list
  (lambda (x depth)
    (cond
      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand2 (cdr x) (+ depth 1)))))
      ((or (unquote? x) (unquote-splicing? x))
       (cond
	 ((> depth 0) `(list (cons ',(car x) ,(qq-expand2 (cdr x) (- depth 1)))))
	 ((unquote? x) `(list . ,(cdr x)))
	 ((null? (cddr x)) (cadr x))
	 (else `(append . ,(cdr x)))))
      ((vector? x) `(list ,(qq-expand2 x depth)))
      ((not (pair? x)) `'(,x))
      ((null? (cdr x)) `(list ,(qq-expand2-list (car x) depth)))
      (else `(list (append ,(qq-expand2-list (car x) depth) ,(qq-expand2 (cdr x) depth)))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1))))
;;	 ((and (unquote? x) (not (null? (cdr x))) (null? (cddr x))) (cadr^ ax))
;;	 (else `(quote ,ax))))  ;; illegal
;;      ((vector? x) `(list->vector ,(qq-expand (vector->list x) (retag (vector->list^ ax) 'none) depth)))
;;      ((not (pair? x)) `',ax)
;;      ((null? (cdr x)) (qq-expand-list (car x) (car^ ax) depth))
;;      (else `(append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth))))))

;;;; walks through unannotated and annotated expressions in parallel, guided by unannotated expression
;;(define qq-expand-list
;;  (lambda (x ax depth)
;;    (cond
;;      ((quasiquote? x) `(list (cons 'quasiquote ,(qq-expand (cdr x) (^cdr^ ax) (+ depth 1)))))
;;      ((or (unquote? x) (unquote-splicing? x))
;;       (cond
;;	 ((> depth 0) `(list (cons ',(car^ ax) ,(qq-expand (cdr x) (^cdr^ ax) (- depth 1)))))
;;	 ((unquote? x) `(list . ,(^cdr^ ax)))
;;	 ((null? (cddr x)) (cadr^ ax))
;;	 (else `(append . ,(^cdr^ ax)))))
;;      ((vector? x) `(list ,(qq-expand x ax depth)))
;;      ((not (pair? x)) `'(,ax))
;;      ((null? (cdr x)) `(list ,(qq-expand-list (car x) (car^ ax) depth)))
;;      (else `(list (append ,(qq-expand-list (car x) (car^ ax) depth) ,(qq-expand (cdr x) (^cdr^ ax) depth)))))))

;;--------------------------------------------------------------------------------------------
;; my version - not correct

;;(define* expand-quasiquote
;;  (lambda (adatum k)
;;    (cond
;;      ((vector?^ adatum)
;;       (expand-quasiquote^ (vector->list^ adatum)
;;	 (lambda-cont (ls) (k `(list->vector ,ls)))))
;;      ((not (pair?^ adatum)) (k `(quote ,adatum)))
;;      ;; doesn't handle nested quasiquotes yet
;;      ((quasiquote?^ adatum) (k `(quote ,adatum)))
;;      ((unquote?^ adatum) (k (cadr^ adatum)))
;;      ((unquote-splicing? (car^ adatum))
;;       (if (null? (cdr^ adatum))
;;	 (k (cadr^ (car^ adatum)))
;;	 (expand-quasiquote^ (^cdr^ adatum)
;;	   (lambda-cont (v) (k `(append ,(cadr^ (car^ adatum)) ,v))))))
;;      ((quasiquote-list?^ adatum)
;;       (expand-quasiquote-list^ (get-sexp adatum)
;;	 (lambda-cont (v)
;;	   (k `(list ,@v)))))
;;      (else
;;	(expand-quasiquote^ (car^ adatum)
;;	  (lambda-cont (v1)
;;	    (expand-quasiquote^ (^cdr^ adatum)
;;	      (lambda-cont (v2)
;;		(k `(cons ,v1 ,v2))))))))))

;;(define* expand-quasiquote-list^  ;; maps expand-quasiquote to an arbitrary flat list
;;  (lambda (asexps k)
;;    (if (null? asexps)
;;      (k '())
;;      (expand-quasiquote^ (car asexps)
;;	(lambda-cont (v1)
;;	  (expand-quasiquote-list^ (cdr asexps)
;;	    (lambda-cont (v2)
;;	      (k (cons v1 v2)))))))))

;;(define quasiquote-list?^
;;  (lambda (adatum)
;;    (or (null?^ adatum)
;;	(and (pair?^ adatum)
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ adatum))
;;	     (not (unquote?^ adatum))
;;	     (not (unquote-splicing?^ adatum))
;;	     ;; doesn't handle nested quasiquotes yet
;;	     (not (quasiquote?^ (car^ adatum)))
;;	     (not (unquote-splicing?^ (car^ adatum)))
;;	     (quasiquote-list?^ (^cdr^ adatum))))))

;;--------------------------------------------------------------------------------------------
;; temporary - stuff for testing

;;(define a 'apple)
;;(define b 'banana)
;;(define c '(cherry orange))
;;(define m 2)
;;(define n 3)
;;(define abc '(a b c))

;;;;(define parseexp (lambda (e) (parse e init-handler2 init-fail init-cont2)))

;;;(define-syntax for ((for ?exp times do . ?bodies) (for-repeat ?exp (lambda () . ?bodies))))

;;(define for-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "for-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'for-macro "huh?")))))

;;(define collect-macro
;;  (lambda ()
;;    (cases expression (car (parse-file "collect-macro.ss"))
;;      (define-syntax-exp (name clauses)
;;	(make-pattern-macro clauses))
;;      (else (error 'collect-macro "huh?")))))

;;(define for-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "for-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'for-macro^ "huh?")))))

;;(define collect-macro^
;;  (lambda ()
;;    (cases aexpression (car (aparse-file "collect-macro.ss"))
;;      (define-syntax-aexp (name clauses aclauses info)
;;	(make-pattern-macro^ clauses aclauses))
;;      (else (error 'collect-macro^ "huh?")))))

;;(define make-macro-env
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case 'for 'collect)
;;      (list and-transformer
;;	    or-transformer
;;	    cond-transformer
;;	    let-transformer
;;	    letrec-transformer
;;	    let*-transformer
;;	    case-transformer
;;	    record-case-transformer
;;	    (for-macro)      ;; for testing only
;;	    (collect-macro)  ;; for testing only
;;))))

;;(define make-macro-env^
;;  (lambda ()
;;    (make-initial-environment
;;      (list 'and 'or 'cond 'let 'letrec 'let* 'case 'record-case) ;; 'for 'collect)
;;      (list and-transformer^
;;	    or-transformer^
;;	    cond-transformer^
;;	    let-transformer^
;;	    letrec-transformer^
;;	    let*-transformer^
;;	    case-transformer^
;;	    record-case-transformer^
;;;;	    (for-macro^)      ;; for testing only
;;;;	    (collect-macro^)  ;; for testing only
;;))))

;;(define macro-env (make-macro-env^))

;;;;(define macro-env 'undefined)

;;;;(define pmacro (lookup-value 'for macro-env init-handler2 init-fail init-cont2))

;;(define check
;;    (lambda (f)
;;      (let ((exps #f) (aexps #f))
;;        (set! macro-env (make-macro-env))
;;        (set! exps (parse-file f))
;;        (set! macro-env (make-macro-env^))
;;        (set! aexps (aparse-file f))
;;        (equal? (map unparse exps) (map aunparse aexps)))))
;; Calysto Scheme interpreter with support for choose
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/jmarshall
;; doug.blank@gmail.com
;; https://github.com/dsblank

(load "transformer-macros.ss")
(load "environments-cps.ss")
(load "parser-cps.ss")

;;----------------------------------------------------------------------------
;; to run the scheme data structure machine within Chez Scheme:
;; % scheme source-ds.ss
;; > (start)

;;----------------------------------------------------------------------------
;; to run the scheme register machine within Chez Scheme:
;; % scheme source-rm.ss
;; > (start)

;;----------------------------------------------------------------------------
;; used by scheme CPS, DS, RM, and Host RM code

(define REP-k
  (lambda-cont2 (v fail)
    (set! *last-fail* fail)
    (halt* v)))

(define REP-handler
  (lambda-handler2 (e fail)
    (set! *last-fail* fail)
    (halt* (list 'exception e))))

(define REP-fail
  (lambda-fail ()
    (halt* "no more choices")))

(define *last-fail* REP-fail)

(define *tokens-left* 'undefined)

(define exception?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'exception))))

;;----------------------------------------------------------------------------
;; used only by scheme CPS, DS, and RM code

;; dummy versions of functions defined in host code
(define-native dlr-proc? (lambda (x) #f))
(define-native dlr-apply apply)
(define-native dlr-func (lambda (x) x))
(define-native callback (lambda args #f))
(define-native dlr-env-contains (lambda (x) #f))
(define-native dlr-env-lookup (lambda (x) #f))
(define-native dlr-object? (lambda (x) #f))
(define-native dlr-lookup-components (lambda (x y) #f))
(define-native set-global-value! (lambda (var x) #f))
(define-native set-global-docstring! (lambda (var x) #f))
(define-native import-native (lambda ignore #f))
(define-native import-as-native (lambda ignore #f))
(define-native import-from-native (lambda ignore #f))
(define-native iterator? (lambda ignore #f))
(define-native get_type (lambda (x) 'unknown))
(define-native char->string (lambda (c) (string c)))
(define-native float (lambda (n) (exact->inexact n)))
(define-native int (lambda (n) (inexact->exact n)))
(define-native iter? (lambda (x) #f))
(define-native list-native (lambda (v) v))
(define-native python-eval (lambda v v))
(define-native python-exec (lambda v v))
(define-native SCHEMEPATH (list "."))
(define-native string-startswith?
  (lambda (string start)
    (and (>= (string-length string) (string-length start))
	 (string=? (substring string 0 (string-length start)) start))))
(define-native expt-native (lambda (base power) (expt base power)))
(define-native format-float
  (lambda (total right value)
    (format (format "~~~a,~aF" total right) value)))

;; is set to run unit tests from host language
(define make-test-callback
  (lambda (group-name case-name result traceback proc-exp test-exp result-val)
    void-value))

(define path-join
  (lambda (path filename)
    (cond
     ((null? path) filename)
     (else (path-join (cdr path)
		      (string-append (car path) "/" filename))))))

(define use-lexical-address
  (lambda args
    (cond
     ((null? args) *use-lexical-address*)
     (else
      (begin (set! *use-lexical-address* (true? (car args)))
	     void-value)))))

(define-native read-multiline
  (lambda (prompt)
    (printf prompt)
    (format "~s" (read))))

;; because read-multiline uses (read), it can only read a single sexp at a
;; time. it always returns a string version of its input. if the input
;; is the list (+ 2 3), the string "(+ 2 3)" is returned; if the input
;; is the string "apple", the string "\"apple\"" is returned; etc.
;;
;; read-multiline-test is only for testing the evaluation of multiple sexps
;; at once.  the user must type the input as a string enclosed by
;; double quotes.

(define read-multiline-test ;; redefine this to read-multiline to test
  (lambda (prompt)
    (printf prompt)
    (let loop ((input (read)))
      (if (string? input)
	  input
	  (begin
	    (printf "Error: input must be enclosed in quotation marks.\n==> ")
	    (loop (read)))))))

;;----------------------------------------------------------------------------
;; used only by scheme CPS and DS code

(define start
  (lambda ()
    ;; start with fresh environments
    (initialize-globals)
    (read-eval-print-loop)))

;; avoids reinitializing environments on startup (useful for crash recovery)
(define restart
  (lambda ()
    (printf "Restarting...\n")
    (read-eval-print-loop)))

(define read-eval-print-loop
  (lambda ()
    (let ((input (read-multiline "==> ")))
      ;; execute gets redefined as execute-rm when no-csharp-support.ss is loaded
      (let ((result (execute input "stdin")))
	(if (not (void? result))
	    (if (exception? result)
		(handle-exception result)
		(safe-print result)))
	(if *need-newline*
	  (newline))
	(if (end-of-session? result)
	  (halt* 'goodbye)
	  (read-eval-print-loop))))))

(define handle-exception
  (lambda (exc)
    (display (get-traceback-string exc))
    void-value))

(define get-traceback-string
  (lambda (exc)
    ;; (exception (exception-object "ReadError" "cannot represent 1/0" "stdin" 1 1 ()))
    (if (and (list? (cadr exc))
	     (= (length (cadr exc)) 7))
	(let ((error-type (list-ref (cadr exc) 1))
	      (message (list-ref (cadr exc) 2))
	      (src-file (list-ref (cadr exc) 3))
	      (src-line (list-ref (cadr exc) 4))
	      (src-col (list-ref (cadr exc) 5))
	      (stack (list-ref (cadr exc) 6))
	      (retval ""))
	  (set! retval (string-append retval (format "~%Traceback (most recent call last):~%")))
	  (while (not (null? stack))
		 (set! retval (string-append retval (format-exception-line (car stack))))
		 (set! stack (cdr stack)))
	  (if (not (eq? src-file 'none))
	      (set! retval (string-append retval
					  (format "  File \"~a\", line ~a, col ~a~%" src-file src-line src-col))))
	  (string-append retval (format "~a: ~a~%" error-type message)))
	(let ((retval (format "~%Traceback (most recent call last):~%")))
	  (string-append retval (format "Raised Exception: ~a~%" (cadr exc)))))))

(define get-exception-values
  (lambda (exc)
    ;; (exception (exception-object "ReadError" "cannot represent 1/0" "stdin" 1 1 ()))
    (if (and (list? (cadr exc))
	     (> (length (cadr exc)) 2))
	(let ((error-type (list-ref (cadr exc) 1))
	      (message (list-ref (cadr exc) 2)))
	  (list->vector (list error-type message)))
	(list->vector (list "UnhandledException" (cadr exc))))))

(define format-exception-line
  (lambda (line)
    (if (list? line)
	(let ((filename (car line))
	      (line-number (cadr line))
	      (column-number (caddr line)))
	  (if (= (length line) 3)
	      (format "  File \"~a\", line ~a, col ~a~%" filename line-number column-number)
	      (format "  File \"~a\", line ~a, col ~a, in '~a'~%" filename line-number column-number (cadddr line))))
	(format "  Source \"~a\"~%" line))))

(define execute-string
  (lambda (input)
    (execute input "stdin")))

(define execute-file
  (lambda (filename)
    (execute (read-content filename) filename)))

(define execute
  (lambda (input src)
    (set! load-stack '())
    (initialize-execute!)
    (let ((result (scan-input input src REP-handler *last-fail* REP-k)))
      (if (exception? result)
	result
	(begin
	  (set! *tokens-left* result)
	  (if (token-type? (first *tokens-left*) 'end-marker)
	    void-value
	    (execute-loop src)))))))

(define execute-loop
  (lambda (src)
    (let ((result (execute-next-expression src)))
      (if (or (exception? result)
	      (end-of-session? result)
	      (token-type? (first *tokens-left*) 'end-marker))
	  result
	  (execute-loop src)))))

(define execute-next-expression
  (lambda (src)
    (read-sexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum (initial-contours toplevel-env) REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;;----------------------------------------------------------------------------
;; used only by scheme RM code

(define read-eval-print-loop-rm
  (lambda ()
    (let ((input (read-multiline "==> ")))
      (let ((result (execute-rm input "stdin")))
	(while (not (end-of-session? result))
	   (cond
	    ((exception? result) (handle-exception result))
	    ((not (void? result))
	     (begin
	       (if *need-newline* (newline))
	       (safe-print result))))
	   (set! input (read-multiline "==> "))
	   (set! result (execute-rm input "stdin")))
	'goodbye))))

;;----------------------------------------------------------------------------
;; used only by scheme RM and Host RM code

(define execute-string-top
  (lambda (input source)
    ;; source is input symbol, aka "stdin"
    (execute-rm input source)))

(define execute-string-rm
  (lambda (input)
    (execute-rm input "stdin")))

(define execute-file-rm
  (lambda (filename)
    (execute-rm (read-content filename) filename)))

(define execute-rm
  (lambda (input src)
    (set! load-stack '())
    (initialize-execute!)
    (scan-input input src REP-handler *last-fail* REP-k)
    (let ((result (trampoline)))
      (if (exception? result)
	result
	(begin
	  (set! *tokens-left* result)
	  (if (token-type? (first *tokens-left*) 'end-marker)
	    void-value
	    (execute-loop-rm src)))))))

(define execute-loop-rm
  (lambda (src)
    (execute-next-expression-rm src)
    (let ((result (trampoline)))
      (if (or (exception? result)
	      (end-of-session? result)
	      (token-type? (first *tokens-left*) 'end-marker))
	result
	(execute-loop-rm src)))))

(define execute-next-expression-rm
  (lambda (src)
    (read-sexp *tokens-left* src REP-handler *last-fail*
      (lambda-cont4 (datum end tokens-left fail)
	(set! *tokens-left* tokens-left)
	(aparse datum (initial-contours toplevel-env) REP-handler fail
	  (lambda-cont2 (exp fail)
	    (m exp toplevel-env REP-handler fail REP-k)))))))

;;----------------------------------------------------------------------------
;; used only by Host RM code

(define try-parse-handler
  (lambda-handler2 (e fail)
    (halt* #f)))

(define try-parse
  (lambda (input)
    (set! load-stack '())
    (scan-input input "stdin" try-parse-handler *last-fail*
      (lambda-cont2 (tokens fail)
	(aparse-sexps tokens "stdin" (initial-contours toplevel-env) try-parse-handler fail
	  (lambda-cont2 (result fail)
	    (halt* #t)))))
    (trampoline)))

(define initialize-globals
  (lambda ()
    (set! *filename-dict* (dict))
    (set! *filename-vector* (vlist))
    (filename-cache "stdin")
    (set! toplevel-env (make-toplevel-env))
    (set! macro-env (make-macro-env^))
    (set! unit-test-table (dict))
    (set! load-stack '())
    (initialize-execute!)
    (set! *last-fail* REP-fail)))

(define unit-test-table 'undefined)

;;----------------------------------------------------------------------------

(define *tracing-on?* #f)

(define make-debugging-k
  (lambda (exp k)
    (lambda-cont2 (v fail)
      (handle-debug-info exp v)
      (k v fail))))

(define highlight-expression
  (lambda (exp)
    ;; call: (function 1 2 3)
    ;;          ["filename.ss" at line 13 column 4]
    (printf "call: ~s~%" (aunparse exp))
    (let ((info (rac exp)))
      (if (not (eq? info 'none))
	  (printf "['~a', line ~a, col ~a]~%"
		  (get-srcfile info)
		  (get-start-line info)
		  (get-start-char info))))))

(define handle-debug-info
  (lambda (exp result)
    (printf "~s => ~a~%" (aunparse exp) (make-safe result))))

(define *stack-trace* '(()))

(define *use-stack-trace* #t)

(define get-use-stack-trace
  (lambda ()
    *use-stack-trace*))

(define set-use-stack-trace!
  (lambda (value)
    (set! *use-stack-trace* (true? value))))

(define initialize-stack-trace!
  (lambda ()
    (set-car! *stack-trace* '())))

(define initialize-execute!
  (lambda ()
    (set! _closure_depth  0)
    (set! _trace_pause #f)
    (initialize-stack-trace!)))

(define push-stack-trace!
  (lambda (exp)
    ;;(printf "~a: ~a\n" 'push exp)
    ;; FIXME: limit size of stack!
    (set-car! *stack-trace* (cons exp (car *stack-trace*)))))

(define pop-stack-trace!
  (lambda (exp)
    ;;(printf "~a: ~a\n" 'pop exp)
    (if (not (null? (car *stack-trace*)))
	(set-car! *stack-trace* (cdr (car *stack-trace*))))))

(define* m
  (lambda (exp env handler fail k)   ;; fail is a lambda-handler2; k is a lambda-cont2
   (if *tracing-on?* (highlight-expression exp))
   (let ((k (if *tracing-on?* (make-debugging-k exp k) k)))
    (cases aexpression exp
      (lit-aexp (datum info) (k datum fail))
      (var-aexp (id info)
	(lookup-value id env info handler fail k))
      (lexical-address-aexp (depth offset id info)
	(lookup-value-by-lexical-address depth offset (frames env) fail k))
      (func-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (dlr-func proc) fail))))
      (callback-aexp (exp info)
	(m exp env handler fail
	  (lambda-cont2 (proc fail)
	    (k (callback proc) fail))))
      (if-aexp (test-exp then-exp else-exp info)
	(m test-exp env handler fail
	  (lambda-cont2 (bool fail)
	    (if bool
	      (m then-exp env handler fail k)
	      (m else-exp env handler fail k)))))
      (help-aexp (var var-info info)
	    (lookup-variable var env var-info handler fail
	      (lambda-cont2 (var fail)
		(k (help (dlr-env-lookup var)) fail))
	      (lambda-cont3 (dlr-obj components fail) ;; dlr-obj is Myro, components is (Myro robot)
		(k (help (get-external-member dlr-obj components)) fail))
	      (lambda-cont2 (binding fail)
		(k (binding-docstring binding) fail))))
      (association-aexp (var exp info)
	(m exp env handler fail
	  (lambda-cont2 (value fail)
	     (k (association var value) fail))))
      (assign-aexp (var rhs-exp var-info info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-variable var env var-info handler fail
	      (lambda-cont2 (var fail)
		(let ((old-value (dlr-env-lookup var)))
		  ;; need to undo the assignment if we back up
		  (set-global-value! var rhs-value)
		  (let ((new-fail (lambda-fail () (set-global-value! var old-value) (fail))))
		    (k void-value new-fail))))
	      (lambda-cont3 (dlr-obj components fail) ;; dlr-obj is Myro, components is (Myro robot)
		(let ((old-value (get-external-member dlr-obj components)))
		  (set-external-member! dlr-obj components rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-external-member! dlr-obj components old-value) (fail))))
		    (k void-value new-fail))))
	      (lambda-cont2 (binding fail)
		(let ((old-value (binding-value binding)))
		  (set-binding-value! binding rhs-value)
		  ;; need to undo the assignment if we back up
		  (let ((new-fail (lambda-fail () (set-binding-value! binding old-value) (fail))))
		    (k void-value new-fail))))))))
      (define-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (lookup-binding-in-first-frame var env handler fail
	      (lambda-cont2 (binding fail)
		(set-binding-value! binding rhs-value)
		(set-binding-docstring! binding docstring)
		;; definitions should occur only at top level, so no need to undo
		(k void-value fail))))))
      (define!-aexp (var docstring rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (rhs-value fail)
	    (if (procedure-object? rhs-value)
		(set-global-value! var (dlr-func rhs-value))
		(set-global-value! var rhs-value))
	    (set-global-docstring! var docstring)
	    (k void-value fail))))
      (define-syntax-aexp (name clauses aclauses info)
	(lookup-binding-in-first-frame name macro-env handler fail
	  (lambda-cont2 (binding fail)
	    (set-binding-value! binding (make-pattern-macro^ clauses aclauses))
	    (k void-value fail))))
      (define-syntax-transformer-aexp (name rhs-exp info)
	(m rhs-exp env handler fail
	  (lambda-cont2 (proc fail)
	    (let ((macro-transformer
		    (lambda-macro (adatum handler fail k2)  ;; k2 receives 1 arg
		      (unannotate-cps adatum
			(lambda-cont (sexp)
			  ;;(printf "~a transformer receives ~a\n" name sexp)
			  (proc (list sexp) env info handler fail
			    (lambda-cont2 (new-sexp fail)
			      ;;(printf "transformed expression to ~a\n" new-sexp)
			      (k2 new-sexp))))))))
	      (lookup-binding-in-first-frame name macro-env handler fail
		(lambda-cont2 (binding fail)
		  (set-binding-value! binding macro-transformer)
		  (k void-value fail)))))))
      (define-tests-aexp (name aclauses info)
	(if (hasitem-native unit-test-table name)
	    (runtime-error (format "duplicate unit test group name '~a'; did you forget to (clear-unit-tests)?" name)
			   info handler fail)
	    (begin
	      (setitem-native unit-test-table name (list aclauses env))
	      (k void-value fail))))
      (run-tests-aexp (tests)
	(if (null? tests)
	    (run-unit-tests (map list (dict->keys unit-test-table)) (get-current-time) 0 0 handler fail k)
	    (run-unit-tests tests (get-current-time) 0 0 handler fail k)))
      (begin-aexp (exps info)
	(eval-sequence exps env handler fail k))
      (lambda-aexp (formals bodies info)
	(k (closure formals bodies env) fail))
      (mu-lambda-aexp (formals runt bodies info)
	(k (mu-closure formals (get-symbol runt) bodies env) fail))
      (trace-lambda-aexp (name formals bodies info)
	  (k (trace-closure name formals bodies env) fail))
      (mu-trace-lambda-aexp (name formals runt bodies info)
	(k (mu-trace-closure name formals (get-symbol runt) bodies env) fail))
      (try-catch-aexp (body cvar cexps info)
	(let ((new-handler (try-catch-handler cvar cexps env handler k)))
	  (m body env new-handler fail k)))
      (try-finally-aexp (body fexps info)
	(let ((new-handler (try-finally-handler fexps env handler)))
	  (m body env new-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail)))))))
      (try-catch-finally-aexp (body cvar cexps fexps info)
	(let ((new-handler (try-catch-finally-handler cvar cexps fexps env handler k)))
	  (m body env new-handler fail
	     (lambda-cont2 (v fail)
	       ;;(printf "executing finally block~%")
	       (eval-sequence fexps env handler fail
		 (lambda-cont2 (v2 fail) (k v fail)))))))
      (raise-aexp (exp info)
	 (m exp env handler fail
	    ;; TODO: pass in more info to handler (k, env) to support resume, etc.
	    (lambda-cont2 (e fail)
	      (let ((src (get-srcfile info))
		    (line (get-start-line info))
		    (col (get-start-char info)))
		(cond
		 ((exception-object? e) (handler e fail))
		 ((string? e)
		  (handler (make-exception "Exception" e src line col) fail))
		 ((and (list? e)
		       (valid-exception-type? (car e))
		       (string? (cadr e)))
		  (handler (make-exception (car e) (cadr e) src line col) fail))
		 (else (runtime-error "bad exception type" info handler fail)))))))
      (choose-aexp (exps info)
	(eval-choices exps env handler fail k))
      (app-aexp (operator operands info)
	(m* operands env handler fail
	  (lambda-cont2 (args fail)
	    (m operator env handler fail
	      (lambda-cont2 (proc fail)
		(if *use-stack-trace* (push-stack-trace! exp))
		(cond
		  ((dlr-proc? proc)
		   (let ((result (dlr-apply proc args)))
		     (if *use-stack-trace* (pop-stack-trace! exp))
		     (k result fail)))
		  ((procedure-object? proc)
		   (if *use-stack-trace*
		       (proc args env info handler fail
			  (lambda-cont2 (v2 fail)
			     (pop-stack-trace! exp)
			     (k v2 fail)))
		       (proc args env info handler fail k)))
		  (else (runtime-error (format "attempt to apply non-procedure '~a'" proc)
				       info handler fail))))))))
      (else (runtime-error (format "unknown abstract syntax type: ~a" (car exp))
			   info handler fail))))))

(define* run-unit-tests
  (lambda (tests start-time right wrong handler fail k)
    (if (null? tests)
	(let ((total (apply + (map length (map car (dict->values unit-test-table))))))
	  (printf "=================\n")
	  (printf "Testing completed!\n")
	  (printf "  Time : ~a seconds~%" (format-float 4 2 (- (get-current-time) start-time)))
	  (printf "  Total tests defined: ~s ~%" total)
	  (printf "  Total tests tested : ~s ~%" (+ right wrong))
	  (printf "                Right: ~s ~%" right)
	  (printf "                Wrong: ~s ~%" wrong)
	  (k void-value fail))
	(run-unit-test (car tests) right wrong handler fail
	  (lambda-cont2 (results fail)
	     (let ((right2 (car results))
		   (wrong2 (cadr results)))
	       (run-unit-tests (cdr tests) start-time right2 wrong2 handler fail k)))))))

(define* run-unit-test
  (lambda (test right wrong handler fail k)
    (let* ((test-name (car test))
	   (nums (cdr test))
	   (entry (getitem-native unit-test-table test-name)))
      (if (eq? entry #f)
	(runtime-error (format "test group '~a' not found" test-name) 'none handler fail)
	(let* ((assertions (car entry))
	       (env (cadr entry)))
	  (printf "Testing group '~a'...\n" test-name)
	  (if (null? nums)
	      (run-unit-test-cases test-name assertions #f right wrong env handler fail k)
	      (filter-assertions test-name nums assertions handler fail
		(lambda-cont2 (filtered-assertions fail)
		  (run-unit-test-cases test-name filtered-assertions #t right wrong env handler fail k)))))))))

(define* filter-assertions
  (lambda (test-name nums assertions handler fail k)
    (if (null? nums)
	(k '() fail)
	(let ((case-name 'undefined))
	  (if (number? (car nums))
	      (set! case-name (format "case ~a" (car nums)))
	      (set! case-name (car nums))) ;; string
	  (lookup-assertions test-name case-name assertions '() handler fail
	    (lambda-cont2 (matched-exps fail)
	      (filter-assertions test-name (cdr nums) (cdr assertions) handler fail
		(lambda-cont2 (exps fail)
		  (k (append matched-exps exps) fail)))))))))

(define lookup-assertions
  (lambda (test-name case-name assertions accum handler fail k)
    (if (null? assertions)
	(if (null? accum)
	    (runtime-error (format "~a unit test '~a' not found" test-name case-name)
			   'none handler fail)
	    (k accum fail))
	;; extract <string> from parsed (assert <pred> <exp1> <exp2> <string>)
	;; (app-aexp <var-aexp> (<parsed-pred> <parsed-exp1> <parsed-exp2> <lit-aexp>))
	(let* ((assertion (car assertions))
	       (app-aexp-args (caddr assertion)))
	  (if (= (length app-aexp-args) 4)
	    (let ((lit-aexp-datum (cadr (cadddr app-aexp-args))))
	      (if (and (string? lit-aexp-datum)
		       (or (and (string-startswith? case-name "case ")
				(string=? lit-aexp-datum case-name))
			   (and (not (string-startswith? case-name "case "))
				(string-startswith? lit-aexp-datum case-name))))
		  (lookup-assertions test-name case-name (cdr assertions) (cons assertion accum) handler fail k)
		  (lookup-assertions test-name case-name (cdr assertions) accum handler fail k))))))))

(define valid-exception-type?
  (lambda (exception-type)
    (and (string? exception-type)
	 (or (string=? exception-type "AssertionError")
	     (string=? exception-type "Exception")
	     (string=? exception-type "KeyboardInterrupt")
	     (string=? exception-type "MacroError")
	     (string=? exception-type "ParseError")
	     (string=? exception-type "ReadError")
	     (string=? exception-type "RunTimeError")
	     (string=? exception-type "ScanError")
	     (string=? exception-type "UnhandledException")))))

(define* run-unit-test-cases
  (lambda (test-name assertions verbose right wrong env handler fail k)
    (if (null? assertions)
        (k (list right wrong) fail)
        (let ((test-case-handler
               (lambda-handler2 (e fail)
		 (let* ((msg (get-exception-message e))
			(where (get-exception-info e))
			(assert-exp (car assertions)) ;; (app-exp assert op e1 e2 name)
			(proc-exp (aunparse (car (cdr^ assert-exp))))
			(test-aexp (cadr (cdr^ assert-exp)))
			(test-exp (aunparse test-aexp))
			(result-exp (caddr (cdr^ assert-exp)))
			(traceback (get-traceback-string (list 'exception e))))
		   (if (> (string-length msg) 0)
		       (if (eq? where 'none)
			   (printf "  Error: ~a \"~a\"\n" test-name msg)
			   (printf "  Error: ~a \"~a\" at ~a\n" test-name msg where))
		       (if (eq? where 'none)
			   (printf "  Error: ~a\n" test-name)
			   (printf "  Error: ~a at ~a\n" test-name where)))
		   (initialize-stack-trace!)
		   (m result-exp env handler fail ;; FIXME: could have an error in result?
		      (lambda-cont2 (result-val fail)
			(m test-aexp env handler fail ;; FIXME: could have an error in test
			   (lambda-cont2 (test-val fail)
			      (if verbose
				  (begin
				    (printf "~a\n" traceback)
				    (printf "  Procedure    : ~a\n" proc-exp)
				    (printf "       src     : ~a\n" test-exp)
				    (printf "       src eval: ~a\n" test-val)
				    (printf "       result  : ~a\n" result-val)))
			      (make-test-callback test-name msg #f traceback proc-exp test-exp result-val)
			      (run-unit-test-cases test-name (cdr assertions) verbose right (+ wrong 1) env handler fail k)))))))))
	  (initialize-stack-trace!)
          (m (car assertions) env test-case-handler fail
	     (lambda-cont2 (v fail)
		(make-test-callback test-name "test" #t "" "" "" "") ;; traceback proc test result
		(run-unit-test-cases test-name (cdr assertions) verbose (+ right 1) wrong env handler fail k)))))))

(define get-exception-info
  (lambda (exception)
    (let ((source (list-ref exception 3))
          (line (list-ref exception 4))
          (column (list-ref exception 5)))
      (if (eq? source 'none)
          'none
          (format "line ~a, column ~a of ~a" line column source)))))

(define make-exception
  (lambda (exception-type message source line column)
    (list 'exception-object exception-type message source line column (make-stack-trace))))

(define get-exception-message
  (lambda (exception)
    ;; '(exception-object "Exception" "message" ...)
    (list-ref exception 2)))

(define make-stack-trace
  (lambda ()
    (let ((trace (car *stack-trace*)))
      (reverse (map format-stack-trace trace)))))

(define get-procedure-name
  (lambda (aexp)
    (if (macro-derived-source-info? aexp)
	(rac (get-source-info aexp))
	(cases aexpression aexp
	  (app-aexp (operator operands info)
	    (cases aexpression operator
	      (lexical-address-aexp (depth offset id info) id)
	      (var-aexp (id info) id)
	      (lambda-aexp (formals bodies info) `(lambda ,formals ...))
	      (mu-lambda-aexp (formals runt bodies info) `(lambda ,(append formals runt) ...))
	      (trace-lambda-aexp (name formals bodies info) name)
	      (mu-trace-lambda-aexp (name formals runt bodies info) name)
	      (else 'application)))
	  (else 'unknown)))))

(define format-stack-trace
  (lambda (exp)
    (let ((info (rac exp)))
      (if (eq? info 'none)
	  'macro-generated-exp
	  (list (get-srcfile info)
		(get-start-line info)
		(get-start-char info)
		(get-procedure-name exp))))))

(define* runtime-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (make-exception "RunTimeError" msg 'none 'none 'none) fail)
      (let ((src (get-srcfile info))
	    (line_number (get-start-line info))
	    (char_number (get-start-char info)))
	(handler (make-exception "RunTimeError" msg src line_number char_number) fail)))))

(define* assertion-error
  (lambda (msg info handler fail)
    (if (eq? info 'none)
      (handler (make-exception "AssertionError" msg 'none 'none 'none) fail)
      (let ((src (get-srcfile info))
	    (line_number (get-start-line info))
	    (char_number (get-start-char info)))
	(handler (make-exception "AssertionError" msg src line_number char_number) fail)))))

(define* m*
  (lambda (exps env handler fail k)
    (if (null? exps)
      (k '() fail)
      (m (car exps) env handler fail
	(lambda-cont2 (v1 fail)
	  (m* (cdr exps) env handler fail
	    (lambda-cont2 (v2 fail)
	      (k (cons v1 v2) fail))))))))

(define* eval-sequence
  (lambda (exps env handler fail k)
    (if (null? (cdr exps))
      (m (car exps) env handler fail k)
      (m (car exps) env handler fail
	(lambda-cont2 (result fail)
	  (eval-sequence (cdr exps) env handler fail k))))))

(define try-catch-handler
  (lambda (cvar cexps env handler k)
    (lambda-handler2 (e fail)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e) (list "try-catch handler"))))
	;;(printf "executing catch block~%")
	(eval-sequence cexps new-env handler fail k)))))

(define try-finally-handler
  (lambda (fexps env handler)
    (lambda-handler2 (e fail)
      ;;(printf "executing finally block~%")
      (eval-sequence fexps env handler fail
	(lambda-cont2 (v fail)
	  ;;(printf "propagating ~a exception~%" e)
	  (handler e fail))))))

(define try-catch-finally-handler
  (lambda (cvar cexps fexps env handler k)
    (lambda-handler2 (e fail)
      ;;(printf "try-handler: handling ~a exception~%" e)
      (let ((new-env (extend env (list cvar) (list e) (list "try-catch-finally handler"))))
	(let ((catch-handler (try-finally-handler fexps env handler)))
	  ;;(printf "executing catch block~%")
	  (eval-sequence cexps new-env catch-handler fail
	    (lambda-cont2 (v fail)
	      ;;(printf "executing finally block~%")
	      (eval-sequence fexps env handler fail
		(lambda-cont2 (v2 fail) (k v fail))))))))))

(define* eval-choices
  (lambda (exps env handler fail k)
    (if (null? exps)
      ;; no more choices, so backtrack to previous choice point
      (fail)
      (let ((new-fail (lambda-fail () (eval-choices (cdr exps) env handler fail k))))
	;; if new-fail is invoked, it will try the next choice
	(m (car exps) env handler new-fail k)))))

(define make-empty-docstrings
  (lambda (n)
    (cond
     ((= n 0) '())
     (else (cons "" (make-empty-docstrings (- n 1)))))))

(define association
  (lambda (var value)
    (list var ': value)))

(define closure
  (lambda (formals bodies env)
    (lambda-proc (args env2 info handler fail k2)
      (let* ((formals-and-args (process-formals-and-args formals args info handler fail))
	     (new-formals (car formals-and-args))
	     (new-args (cdr formals-and-args)))
	(if (= (length new-args) (length new-formals))
	    (eval-sequence bodies (extend env new-formals new-args (make-empty-docstrings (length new-args))) handler fail k2)
	    (runtime-error "incorrect number of arguments in application" info handler fail))))))

(define mu-closure
  (lambda (formals runt bodies env)
    (lambda-proc (args env2 info handler fail k2)
;;      (let* ((formals-and-args (process-formals-and-args formals args info handler fail))
;;	     (new-formals (car formals-and-args))
;;	     (new-args (cdr formals-and-args)))
     (let ((new-formals formals)
	   (new-args args))
	(if (>= (length new-args) (length new-formals))
	    (let ((new-env
		   (extend env
			   (cons runt new-formals)
			   (cons (list-tail new-args (length new-formals))
				 (list-head new-args (length new-formals)))
			   (make-empty-docstrings (+ 1 (length new-formals))))))
	      (eval-sequence bodies new-env handler fail k2))
	    (runtime-error "not enough arguments in application" info handler fail))))))

(define make-trace-depth-string
  (lambda (level)
    (if (= level 0)
      ""
      (string-append " |" (make-trace-depth-string (- level 1))))))

(define trace-closure
  (lambda (name formals bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
      (let* ((formals-and-args (process-formals-and-args formals args info handler fail))
	    (new-formals (car formals-and-args))
	    (new-args (cdr formals-and-args)))
	(if (= (length new-args) (length new-formals))
	    (begin
	      (printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name new-args))
	      ;;(printf "k: ~a\n" (make-safe-continuation k2))
	      (set! trace-depth (+ trace-depth 1))
	      (eval-sequence bodies (extend env new-formals new-args (make-empty-docstrings (length new-formals))) handler fail
	        (lambda-cont2 (v fail)
 	          (set! trace-depth (- trace-depth 1))
		  (printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		  (k2 v fail))))
	    (runtime-error "incorrect number of arguments in application" info handler fail)))))))

;; experimental
(define-native make-safe-continuation
  (lambda (k)
    (cond
      ((not (pair? k)) '<???>)
      ((eq? (car k) 'fail-continuation) '<fail>)
      ((memq (car k) '(handler handler2)) '<handler>)
      ((memq (car k) '(continuation continuation2 continuation3 continuation4))
       (cons (cadr k) (map make-safe-continuation (filter continuation-object? (cddr k)))))
      (else '<???>))))

;; experimental
(define continuation-object?
  (lambda (x)
    (and (pair? x) (memq (car x) '(continuation continuation2 continuation3 continuation4)))))

(define mu-trace-closure
  (lambda (name formals runt bodies env)
    (let ((trace-depth 0))
      (lambda-proc (args env2 info handler fail k2)
;;        (let* ((formals-and-args (process-formals-and-args formals args info handler fail))
;;	      (new-formals (car formals-and-args))
;;	      (new-args (cdr formals-and-args)))
       (let ((new-formals formals)
	     (new-args args))
	  (if (>= (length args) (length new-formals))
	      (let ((new-env
		     (extend env
			     (cons runt new-formals)
			     (cons (list-tail new-args (length new-formals))
				   (list-head new-args (length new-formals)))
			     (make-empty-docstrings (+ 1 (length new-formals))))))
		(printf "~acall: ~s~%" (make-trace-depth-string trace-depth) (cons name new-args))
		(set! trace-depth (+ trace-depth 1))
		(eval-sequence bodies new-env handler fail
		  (lambda-cont2 (v fail)
		     (set! trace-depth (- trace-depth 1))
		     (printf "~areturn: ~s~%" (make-trace-depth-string trace-depth) v)
		     (k2 v fail))))
	      (runtime-error "not enough arguments in application" info handler fail)))))))

;;----------------------------------------------------------------------------
;; Primitives

(define length-one?
  (lambda (ls)
    (and (not (null? ls)) (null? (cdr ls)))))

(define length-two?
  (lambda (ls)
    (and (not (null? ls)) (not (null? (cdr ls))) (null? (cddr ls)))))

(define length-at-least?
  (lambda (n ls)
    (cond
      ((< n 1) #t)
      ((or (null? ls) (not (pair? ls))) #f)
      (else (length-at-least? (- n 1) (cdr ls))))))

(define all-numeric?
  (lambda (ls)
    (or (null? ls)
	(and (number? (car ls))
	     (all-numeric? (cdr ls))))))

(define all-char?
  (lambda (ls)
    (or (null? ls)
	(and (char? (car ls))
	     (all-char? (cdr ls))))))

(define clear-unit-tests-prim
  (lambda-proc (args env2 info handler fail k2)
      (set! unit-test-table (dict))
      (k2 void-value fail)))

;; void
(define void-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 void-value fail)))

(define void-value '<void>)

(define void?
  (lambda (x) (eq? x void-value)))

;; zero?
(define zero?-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (= (car args) 0) fail)))

;; python-eval
(define python-eval-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (python-eval (car args)) fail)))

;; python-eval
(define python-exec-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (python-exec (car args)) fail)))

;; exit
(define exit-prim
  (lambda-proc (args env2 info handler fail k2)
    (halt* end-of-session)))

;; expt
(define expt-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (expt-native (car args) (cadr args)) fail)))

(define end-of-session?
  (lambda (x) (eq? x end-of-session)))

(define end-of-session '(exiting the interpreter))

;; (string-join ", " ('a 'b 'c))
;; "a, b, c"
(define string-join-prim
  (lambda-proc (args env2 info handler fail k2)
     (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of args to string-join; should be two" info handler fail))
      ((not (string? (car args)))
       (runtime-error "first arg to string-join must be a string" info handler fail))
      ((not (list? (cadr args)))
       (runtime-error "second arg to string-join must be a list" info handler fail))
      (else (string-join (car args) (cadr args) env2 info handler fail k2)))))

(define* string-join
  (lambda (sep items env2 info handler fail k2)
    (cond
     ((null? items) (k2 "" fail))
     ((null? (cdr items)) (k2 (format "~a" (car items)) fail))
     (else (string-join sep (cdr items) env2 info handler fail
	      (lambda-cont2 (v fail)
                  (k2 (string-append (format "~a" (car items)) sep v) fail)))))))

;; eval
(define eval-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((length-one? args)  ;; petite uses toplevel env
       (annotate-cps (car args) info
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours toplevel-env) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp toplevel-env handler fail k2))))))
      ((length-two? args)
       (annotate-cps (car args) info
	 (lambda-cont (adatum)
	   (aparse adatum (initial-contours (cadr args)) handler fail
	     (lambda-cont2 (exp fail)
	       (m exp (cadr args) handler fail k2))))))
      (else (runtime-error "incorrect number of arguments to eval" info handler fail)))))

;; eval-ast
(define eval-ast-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to eval-ast" info handler fail))
      ((not (list? (car args)))  ;; is there a better test for exp?  aexpression?
       (runtime-error "eval-ast called on non-abstract syntax tree argument" info handler fail))
      (else (m (car args) toplevel-env handler fail k2)))))  ;; petite uses toplevel env

;; parse
(define parse-prim
  (lambda-proc (args env2 info handler fail k2)
    (annotate-cps (car args) info
      (lambda-cont (adatum)
        (aparse adatum (initial-contours toplevel-env) handler fail k2)))))  ;; was env2

;; string-length
(define string-length-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string-length" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-length called on non-string argument" info handler fail))
      (else (k2 (apply string-length args) fail)))))

;; string-ref
(define string-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to string-ref" info handler fail))
      ((not (string? (car args)))
       (runtime-error "string-ref called with non-string first argument" info handler fail))
      ((not (number? (cadr args)))
       (runtime-error "string-ref called with non-numberic second argument" info handler fail))
      (else (k2 (apply string-ref args) fail)))))

;; unparse
(define unparse-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car args)) fail)))   ;; aunparse should be in CPS

;; unparse-procedure
(define unparse-procedure-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (aunparse (car (caddr (car args)))) fail)))  ;; aunparse should be in CPS

;; parse-string
(define parse-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) "stdin" handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens "stdin" handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (aparse adatum (initial-contours toplevel-env) handler fail k2)  ;; was env2
	      (read-error "tokens left over" tokens-left "stdin" handler fail))))))))

;; read-string
(define read-string-prim
  (lambda-proc (args env2 info handler fail k2)
    (scan-input (car args) "stdin" handler fail
      (lambda-cont2 (tokens fail)
	(read-sexp tokens "stdin" handler fail
	  (lambda-cont4 (adatum end tokens-left fail)
	    (if (token-type? (first tokens-left) 'end-marker)
	      (k2 adatum fail)
	      (read-error "tokens left over" tokens-left "stdin" handler fail))))))))

;; apply
(define apply-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((proc (car args))
	  (proc-args (cadr args)))
      (if (dlr-proc? proc)
	  (k2 (dlr-apply proc proc-args) fail)
	  (proc proc-args env2 info handler fail k2)))))

;; sqrt
(define sqrt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to sqrt" info handler fail))
      (else (k2 (apply sqrt args) fail)))))

;; odd?
(define odd?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to odd?" info handler fail))
      (else (k2 (odd? (car args)) fail)))))

;; even?
(define even?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to even?" info handler fail))
      (else (k2 (even? (car args)) fail)))))

;; // div quotient
(define quotient-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to quotient" info handler fail))
      ((member 0 (cdr args))
       (runtime-error "division by zero" info handler fail))
      (else (k2 (apply quotient args) fail)))))

;; remainder
(define remainder-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to remainder" info handler fail))
      (else (k2 (apply remainder args) fail)))))

;; print
(define print-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each safe-print args)
    (k2 void-value fail)))

;; string
(define string-prim
  ;; turns a list of char into a string
  (lambda-proc (args env2 info handler fail k2)
     ;;(k2 (apply string-append (map (lambda (c) (format "~s" c)) args)) fail)))
     (k2 (apply string args) fail)))

;; substring
(define substring-prim
  ;; (substring "string" start)
  ;; (substring "string" start stop)
  (lambda-proc (args env2 info handler fail k2)
     (if (= (length args) 3)
	 (k2 (substring (car args) (cadr args) (caddr args)) fail)
	 (k2 (substring (car args) (cadr args) (string-length (car args))) fail))))

;; number->string
(define number->string-prim
  ;; given a number, returns those digits as a string
  (lambda-proc (args env2 info handler fail k2)
     (k2 (number->string (car args)) fail)))

;; assv
(define assv-prim
  ;; given 'a '((b 1) (a 2)) returns (a 2)
  (lambda-proc (args env2 info handler fail k2)
     (k2 (assv (car args) (cadr args)) fail)))
;; memv
(define memv-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (memv (car args) (cadr args)) fail)))

(define safe-print
  (lambda (arg)
    (set! *need-newline* #f)
    (pretty-print (make-safe arg))))

(define make-safe
  (lambda (x)
    (cond
      ((procedure-object? x) '<procedure>)
      ((environment-object? x) '<environment>)
      ((exception-object? x) '<exception>)
      ((pair? x) (cons (make-safe (car x)) (make-safe (cdr x))))
      ((vector? x) (list->vector (make-safe (vector->list x))))
      (else x))))

(define exception-object?
  (lambda (x)
    (and (list? x)
	 (= (length x) 7)
	 (eq? (car x) 'exception-object)
	 (valid-exception-type? (cadr x))
	 (string? (cadr x)))))

(define procedure-object?
  (lambda (x)
    (or (procedure? x) (and (pair? x) (eq? (car x) 'procedure)))))

(define environment-object?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'environment))))

;; display
;; fix: why is this so complicated?
(define display-prim
  (lambda-proc (args env2 info handler fail k2)
    (let ((s (format "~a" (car args))))  ;; must use ~a, not ~s, to handle embedded newlines properly
      (set! *need-newline* (true? (not (ends-with-newline? s))))
      (display s)
      (k2 void-value fail))))

(define ends-with-newline?
  (lambda (s)
    (let ((len (string-length s)))
      (and (> len 0) (equal? (substring s (- len 1) len) "\n")))))

;; newline
(define newline-prim
  (lambda-proc (args env2 info handler fail k2)
    (set! *need-newline* #f)
    (newline)
    (k2 void-value fail)))

(define *need-newline* #f)

;; load
(define load-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-at-least? 1 args))
	(runtime-error "incorrect number of arguments to load" info handler fail)
	(load-files args toplevel-env info handler fail k2))))  ;; petite uses toplevel env

(define load-stack '())

(define* load-file
  (lambda (filename env2 info handler fail k)
    (cond
      ((member filename load-stack)
       (printf "skipping recursive load of ~a~%" filename)
       (k void-value fail))
      ((not (string? filename))
       (runtime-error (format "filename '~a' is not a string" filename) info handler fail))
      ((not (file-exists? filename))
       (runtime-error (format "attempted to load nonexistent file '~a'" filename) info handler fail))
      (else
       (set! load-stack (cons filename load-stack))
       (scan-input (read-content filename) filename handler fail
	 (lambda-cont2 (tokens fail)
	   (read-and-eval-asexps tokens filename env2 handler fail
	     (lambda-cont2 (v fail)
	       ;; pop load-stack
	       (if (null? load-stack)
		 (printf "WARNING: empty load-stack encountered!\n")  ;; should never happen
		 (set! load-stack (cdr load-stack)))
	       (k void-value fail)))))))))

(define* read-and-eval-asexps
  (lambda (tokens src env2 handler fail k)
    (if (token-type? (first tokens) 'end-marker)
      (k void-value fail)
      (read-sexp tokens src handler fail
	(lambda-cont4 (datum end tokens-left fail)
	  (aparse datum (initial-contours env2) handler fail  ;; was env2
	    (lambda-cont2 (exp fail)
	      (m exp env2 handler fail
		(lambda-cont2 (v fail)
		  (if (token-type? (first tokens-left) 'end-marker)
		    (k v fail)
		    (read-and-eval-asexps tokens-left src env2 handler fail k)))))))))))

(define* load-files
  (lambda (filenames env2 info handler fail k)
    (if (null? filenames)
      (k void-value fail)
      (find-file-and-load SCHEMEPATH (car filenames) env2 info handler fail
	(lambda-cont2 (v fail)
	  (load-files (cdr filenames) env2 info handler fail k))))))

;; load file from paths
(define* find-file-and-load
  (lambda (paths filename env2 info handler fail k)
    (cond
     ((string-startswith? filename "/")
      (load-file filename env2 info handler fail k))
     ((null? paths)
      (runtime-error (format "attempted to load nonexistent file '~a'" filename) info handler fail))
     (else (let ((path (path-join (list (car paths)) filename)))
	     (if (file-exists? path)
 		 (load-file path env2 info handler fail k)
		 (find-file-and-load (cdr paths) filename env2 info handler fail k)))))))

;; length
(define length-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (length-one? args)
      (length-loop (car args) 0 (car args) info handler fail k2)
      (runtime-error "incorrect number of arguments to length" info handler fail))))

(define* length-loop
  (lambda (x sum ls info handler fail k2)
    (cond
      ((null? x) (k2 sum fail))
      ((not (pair? x))
       (runtime-error (format "length called on improper list ~s" ls) info handler fail))
      (else (length-loop (cdr x) (+ sum 1) ls info handler fail k2)))))

;; symbol?
(define symbol?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error
         (format "incorrect number of arguments to symbol?: you gave ~s, should have been 1 argument" args)
         info handler fail))
      (else (k2 (apply symbol? args) fail)))))

;; number?
(define number?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to number?" info handler fail))
      (else (k2 (apply number? args) fail)))))

;; boolean?
(define boolean?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to boolean?" info handler fail))
      (else (k2 (apply boolean? args) fail)))))

;; string?
(define string?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string?" info handler fail))
      (else (k2 (apply string? args) fail)))))

;; char?
(define char?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char?" info handler fail))
      (else (k2 (apply char? args) fail)))))

;; char=?
(define char=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to char=?" info handler fail))
      ((or (not (char? (car args))) (not (char? (cadr args))))
       (runtime-error "char=? requires arguments of type char" info handler fail))
      (else (k2 (apply char=? args) fail)))))

;; char-whitespace?
(define char-whitespace?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-whitespace?" info handler fail))
      (else (k2 (apply char-whitespace? args) fail)))))

;; char->integer
(define char->integer-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char->integer" info handler fail))
      (else (k2 (apply char->integer args) fail)))))

;; integer->char
(define integer->char-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to integer->char" info handler fail))
      (else (k2 (apply integer->char args) fail)))))

;; char-alphabetic?
(define char-alphabetic?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-alphabetic?" info handler fail))
      (else (k2 (apply char-alphabetic? args) fail)))))

;; char-numeric?
(define char-numeric?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char-numeric?" info handler fail))
      (else (k2 (apply char-numeric? args) fail)))))

;; null?
(define null?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to null?" info handler fail))
      (else (k2 (apply null? args) fail)))))

;; box?
(define box?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to box?" info handler fail))
      (else (k2 (apply box? args) fail)))))

;; pair?
(define pair?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to pair?" info handler fail))
      (else (k2 (apply pair? args) fail)))))

;; box
(define box-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to box" info handler fail))
      (else (k2 (apply box args) fail)))))

;; unbox
(define unbox-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to unbox" info handler fail))
      ((not (box? (car args)))
       (runtime-error (format "unbox called on non-box ~s" (car args)) info handler fail))
      (else (k2 (apply unbox args) fail)))))

;; cons
(define cons-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to cons" info handler fail))
      (else (k2 (apply cons args) fail)))))

;; car
(define car-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to car" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "car called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply car args) fail)))))

;; cdr
(define cdr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdr args) fail)))))

;; cadr
(define cadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadr" info handler fail))
      ((not (length-at-least? 2 (car args)))
       (runtime-error (format "cadr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply cadr args) fail)))))

;; caddr
(define caddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caddr" info handler fail))
      ((not (length-at-least? 3 (car args)))
       (runtime-error (format "caddr called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply caddr args) fail)))))

(define caaaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaaar args) fail)))))

(define caaadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaadr args) fail)))))

(define caaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaar args) fail)))))

(define caadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caadar args) fail)))))

(define caaddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caaddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caaddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caaddr args) fail)))))

(define caadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caadr args) fail)))))

(define caar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caar args) fail)))))

(define cadaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadaar args) fail)))))

(define cadadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadadr args) fail)))))

(define cadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadar args) fail)))))

(define caddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to caddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "caddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply caddar args) fail)))))

(define cadddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cadddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cadddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cadddr args) fail)))))

(define cdaaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaaar args) fail)))))

(define cdaadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaadr args) fail)))))

(define cdaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaar args) fail)))))

(define cdadar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdadar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdadar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdadar args) fail)))))

(define cdaddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdaddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdaddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdaddr args) fail)))))

(define cdadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdadr args) fail)))))

(define cdar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdar args) fail)))))

(define cddaar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddaar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddaar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddaar args) fail)))))

(define cddadr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddadr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddadr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddadr args) fail)))))

(define cddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddar args) fail)))))

(define cdddar-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdddar" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdddar called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdddar args) fail)))))

(define cddddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddddr args) fail)))))

(define cdddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cdddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cdddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cdddr args) fail)))))

(define cddr-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to cddr" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "cddr called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply cddr args) fail)))))

;; list
(define list-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 args fail)))

;; (assert op exp result)
(define assert-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (or (= (length args) 3) (= (length args) 4)))
       (runtime-error "incorrect number of arguments to assert" info handler fail))
      ((not (procedure-object? (car args)))
       (runtime-error "assertion predicate is not a procedure" info handler fail))
      (else (let ((proc (car args))
                  (expression-result (cadr args))
                  (expected-result (caddr args)))
              (proc (list expression-result expected-result) env2 info handler fail
                    (lambda-cont2 (v fail)
                      (cond
                        ((eq? v #t) (k2 'ok fail))
                        ((= (length args) 3)
                         (assertion-error "" info handler fail))
			(else (assertion-error (cadddr args) info handler fail))))))))))

;; make-set
(define make-set-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to set" info handler fail))
      (else (make-set (car args) env2 info handler fail k2)))))

(define* make-set
  (lambda (lst env2 info handler fail k2)
    (if (null? lst)
      (k2 lst fail)
      (make-set (cdr lst) env2 info handler fail
	(lambda-cont2 (v fail)
	  (if (member (car lst) v)
	    (k2 v fail)
	    (k2 (cons (car lst) v) fail)))))))

;; +
(define plus-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply + args) fail)))

;; -
(define minus-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args)
       (runtime-error "incorrect number of arguments to -" info handler fail))
      (else (k2 (apply - args) fail)))))

;; *
(define times-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (apply * args) fail)))

;; /
(define divide-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (> (length args) 1) (member 0 (cdr args)))
       (runtime-error "division by zero" info handler fail))
      (else (k2 (apply / args) fail)))))

;; % and mod
(define modulo-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to %" info handler fail))
      ((= (cadr args) 0)
       (runtime-error "modulo by zero" info handler fail))
      (else (k2 (apply modulo args) fail)))))

;; min
(define min-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      (else (k2 (apply min args) fail)))))

;; max
(define max-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply max args) fail)))

;; <
(define lt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <" info handler fail))
      (else (k2 (apply < args) fail)))))

;; >
(define gt-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >" info handler fail))
      (else (k2 (apply > args) fail)))))

;; <=
(define lt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to <=" info handler fail))
      (else (k2 (apply <= args) fail)))))

;; >=
(define gt-or-eq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to >=" info handler fail))
      (else (k2 (apply >= args) fail)))))

;; =
(define equal-sign-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to =" info handler fail))
      ((not (all-numeric? args))
       (runtime-error "attempt to apply = on non-numeric argument" info handler fail))
      (else (k2 (apply = args) fail)))))

;; abs
(define abs-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to abs" info handler fail))
      (else (k2 (apply abs args) fail)))))

;; equal?
(define equal?-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to equal?" info handler fail)
      (equal-objects? (car args) (cadr args)
	(lambda-cont (bool) (k2 bool fail))))))

(define* equal-objects?
  (lambda (x y k)
    (cond
      ((or (and (null? x) (null? y))
	   ;; (eq? x y) would be easier, but Eq doesn't work correctly for bools in Scheme.cs:
	   (and (boolean? x) (boolean? y) (or (and x y) (and (not x) (not y))))
	   (and (symbol? x) (symbol? y) (eq? x y))
	   (and (number? x) (number? y) (= x y))
	   (and (char? x) (char? y) (char=? x y))
	   (and (eq? x void-value) (eq? y void-value))
	   (and (string? x) (string? y) (string=? x y)))
       (k #t))
      ((and (pair? x) (pair? y))
       (equal-objects? (car x) (car y)
	 (lambda-cont (bool)
	   (if bool
	     (equal-objects? (cdr x) (cdr y) k)
	     (k #f)))))
      ((and (vector? x) (vector? y) (= (vector-length x) (vector-length y)))
       (equal-vectors? x y (- (vector-length x) 1) k))
      ((and (box? x) (box? y))
       (equal-objects? (unbox x) (unbox y) k))
      (else (k #f)))))

(define* equal-vectors?
  (lambda (v1 v2 i k)
    (if (< i 0)
      (k #t)
      (equal-objects? (vector-ref v1 i) (vector-ref v2 i)
	(lambda-cont (bool)
	  (if bool
	    (equal-vectors? v1 v2 (- i 1) k)
	    (k #f)))))))

;; eq?
(define eq?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to eq?" info handler fail))
      (else (k2 (apply eq? args) fail)))))

;; memq
(define memq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to memq" info handler fail))
      (else (k2 (apply memq args) fail)))))

;; member
(define member-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (not (length-two? args))
      (runtime-error "incorrect number of arguments to member" info handler fail)
      (member-loop (car args) (cadr args) (cadr args) info handler fail k2))))

(define* member-loop
  (lambda (x y ls info handler fail k)
    (cond
      ((null? y) (k #f fail))
      ((not (pair? y))
       (runtime-error (format "member called on improper list ~s" ls) info handler fail))
      (else (equal-objects? x (car y)
	      (lambda-cont (bool)
		(if bool
		  (k y fail)
		  (member-loop x (cdr y) ls info handler fail k))))))))

;; random
(define random-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to random" info handler fail))
      ((not (positive? (car args)))
       (runtime-error "argument to random must be positive" info handler fail))
      (else (k2 (apply random args) fail)))))

;; range
(define range-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((or (null? args) (length-at-least? 4 args))
       (runtime-error "incorrect number of arguments to range" info handler fail))
      (else (k2 (apply range args) fail)))))

(define snoc-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply snoc args) fail)))

(define rac-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply rac args) fail)))

(define rdc-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (apply rdc args) fail)))

(define-native range
  (lambda args
    (letrec
	((range
	  (lambda (n end step acc) ;; count 10 1
	    (if (= n end)
	      (reverse acc)
	      (range (+ n step) end step (cons n acc))))))
      (cond
	((null? (cdr args)) (range 0 (car args) 1 '())) ;; (range 10)
	((null? (cddr args)) (range (car args) (cadr args) 1 '())) ;; (range 0 10)
	(else (range (car args) (cadr args) (caddr args) '())))))) ;; (range 0 10 1)

;; set-car!
(define set-car!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-car!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-car! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-car! args) fail)))))

;; set-cdr!
(define set-cdr!-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to set-cdr!" info handler fail))
      ((not (pair? (car args)))
       (runtime-error (format "set-cdr! called on non-pair ~s" (car args)) info handler fail))
      (else (k2 (apply set-cdr! args) fail)))))

;; load-as
(define load-as-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to load-as" info handler fail))
      (else (let ((filename (car args))
		  (module-name (cadr args)))
	      (lookup-binding-in-first-frame module-name env2 handler fail
		(lambda-cont2 (binding fail)
		  (let ((module (make-toplevel-env)))
		    (set-binding-value! binding module)
		    (find-file-and-load SCHEMEPATH filename module info handler fail k2)))))))))

;; get-stack-trace-prim
(define get-stack-trace-prim
  (lambda-proc (args env2 info handler fail k)
    (k (car *stack-trace*) fail)))

;; call/cc
(define call/cc-prim
  (lambda-proc (args env info handler fail k)
    (if (not (length-one? args))
      (runtime-error "incorrect number of arguments to call/cc" info handler fail)
      (let ((proc (car args)))
	(if (not (procedure-object? proc))
	  (runtime-error "call/cc called with non-procedure" info handler fail)
	  (let ((fake-k (lambda-proc (args env2 info handler fail k2) (k (car args) fail))))
	    (if (dlr-proc? proc)
	      (k (dlr-apply proc (list fake-k)) fail)
	      (proc (list fake-k) env info handler fail k))))))))

;; abort
(define abort-prim
  (lambda-proc (args env2 info handler fail k2)
    (if (null? args)
      (REP-k void-value fail)
      (REP-k (car args) fail))))

;; require
(define require-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to require" info handler fail))
      ((true? (car args)) (k2 'ok fail))
      (else (fail)))))

;; cut
(define cut-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 args REP-fail)))

;; reverse
(define reverse-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to reverse" info handler fail))
      ((not (list? args))
       (runtime-error (format "reverse called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply reverse args) fail)))))

;; append
(define append-prim
  (lambda-proc (args env2 info handler fail k2)
    (append-all args info handler fail k2)))
;;    (cond
;;      ((not (length-two? args))
;;       (runtime-error "incorrect number of arguments to append" info handler fail))
;;      ((not (list? (car args)))
;;       (runtime-error (format "append called on incorrect list structure ~s" (car args)) info handler fail))
;;      (else (append-all args (lambda-cont (v) (k2 v fail)))))))
;;      (else (k2 (apply append args) fail)))))

(define* append2
  (lambda (ls1 ls2 fail k2)
    (if (null? ls1)
      (k2 ls2 fail)
      (append2 (cdr ls1) ls2 fail
	(lambda-cont2 (v fail)
	  (k2 (cons (car ls1) v) fail))))))

(define* append-all
  (lambda (lists info handler fail k2)
    (cond
      ((null? lists) (k2 '() fail))
      ((null? (cdr lists)) (k2 (car lists) fail))
      ((not (list? (car lists)))
       (runtime-error (format "append called on incorrect list structure ~s" (car lists)) info handler fail))
      (else (append-all (cdr lists) info handler fail
	      (lambda-cont2 (ls fail)
		(append2 (car lists) ls fail k2)))))))

;; string->number
(define string->number-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->number" info handler fail))
      (else (k2 (apply string->number args) fail)))))

;; string=?
(define string=?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to string=?" info handler fail))
     (else (k2 (apply string=? args) fail)))))

;; list->vector
(define list->vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->vector" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->vector called on incorrect list structure ~s" (car args)) info handler fail))
      (else (k2 (apply list->vector args) fail)))))

;; list->string
(define list->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to list->string" info handler fail))
      ((not (list? (car args)))
       (runtime-error (format "list->string called on incorrect list structure ~s" (car args)) info handler fail))
      ((not (all-char? (car args)))
       (runtime-error (format "list->string called on non-char list ~s" (car args)) info handler fail))
      (else (k2 (apply list->string args) fail)))))

(define char->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to char->string" info handler fail))
      ((not (char? (car args)))
       (runtime-error (format "char->string called on non-char item ~s" (car args)) info handler fail))
      (else (k2 (apply char->string args) fail)))))


(define string->list-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->list" info handler fail))
      ((not (string? (car args)))
       (runtime-error (format "string->list called on non-string item ~s" (car args)) info handler fail))
      (else (k2 (apply string->list args) fail)))))


(define string->symbol-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to string->symbol" info handler fail))
      ((not (string? (car args)))
       (runtime-error (format "string->symbol called on non-string item ~s" (car args)) info handler fail))
      (else (k2 (apply string->symbol args) fail)))))


(define symbol->string-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to symbol->string" info handler fail))
      ((not (symbol? (car args)))
       (runtime-error (format "symbol->string called on non-symbol item ~s" (car args)) info handler fail))
      (else (k2 (apply symbol->string args) fail)))))

(define vector->list-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to vector->list" info handler fail))
      ((not (vector? (car args)))
       (runtime-error (format "vector->list called on incorrect vector structure ~s" (car args)) info handler fail))
      (else (k2 (apply vector->list args) fail)))))

(define vector-length-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to vector-length" info handler fail))
      ((not (vector? (car args)))
       (runtime-error (format "vector-length called on incorrect vector structure ~s" (car args)) info handler fail))
      (else (k2 (apply vector-length args) fail)))))

;; (get-completions)
(define get-completions-prim
  (lambda-proc (args env2 info handler fail k2)
	       (make-set (sort symbol<? (get-completions args env2))
			 env2 info handler fail k2)))

;; dir
(define dir-prim
  (lambda-proc (args env2 info handler fail k2)
    (make-set (directory args env2) env2 info handler fail k2)))

;; (macros)
(define macros-prim
  (lambda-proc (args env2 info handler fail k2)
     (make-set (sort symbol<?
		     (get-variables-from-frames (frames macro-env)))
	       env2 info handler fail k2)))


(define get-completions
  (lambda (args env)
    (cond
     ((null? args)
      (append (get-variables-from-frames (frames env))
	      (get-variables-from-frames (frames macro-env))))
     ((environment? (car args))
      (append (get-variables-from-frames (frames (car args)))
	      (get-variables-from-frames (frames macro-env))))
     (else
      (get-external-members (car args))))))

(define directory
  (lambda (args env)
    (if (or (null? args) (environment? (car args)))
	(sort symbol<? (if (null? args)
			   (get-variables-from-frames (frames env))
			   (get-variables-from-frames (frames (car args)))))
	(get-external-members (car args)))))

(define get-variables-from-frame
  (lambda (frame)
    (cadr frame)))

(define get-variables-from-frames
  (lambda (frames)
    (flatten (map get-variables-from-frame frames))))

(define symbol<?
  (lambda (a b)
    (let ((a_string (symbol->string a))
	  (b_string (symbol->string b)))
      (string<? a_string b_string))))

(define flatten
  (lambda (lists)
    (cond
      ((null? lists) '())
      ((list? (car lists))
       (append (flatten (car lists)) (flatten (cdr lists))))
      (else (cons (car lists) (flatten (cdr lists)))))))

;; current-time
(define current-time-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (get-current-time) fail)))

(define get-current-time
  (lambda ()
    (let ((now (current-time)))
      (+ (time-second now)
	 (inexact (/ (time-nanosecond now) 1000000000))))))

;; map
(define map-prim
  (lambda-proc (args env2 info handler fail k2)
    (map-primitive (car args) (cdr args) env2 handler fail k2)))

;; supports procedures of any number of arguments
(define* map-primitive
  (lambda (proc args env handler fail k)
    (if (iterator? (car args))
        (iterate-collect proc (car args) env handler fail k)
        (let ((len (length args))
              (list-args (listify args)))
          (cond
            ((= len 1) (map1 proc (car list-args) env handler fail k))
            ((= len 2) (map2 proc (car list-args) (cadr list-args) env handler fail k))
            (else (mapN proc list-args env handler fail k)))))))

(define listify
  (lambda (arg-list)
    (cond
     ((null? arg-list) '())
     ((list? (car arg-list))
      (cons (car arg-list) (listify (cdr arg-list))))
     ((vector? (car arg-list))
      (cons (vector->list (car arg-list)) (listify (cdr arg-list))))
     ((string? (car arg-list))
      (cons (string->list (car arg-list)) (listify (cdr arg-list))))
     ((iter? (car arg-list))
      (cons (vector->list (list-native (car arg-list))) (listify (cdr arg-list))))
     (else (error 'map "cannot use object type '~a' in map"
		  (get_type (car arg-list))))))) ;; get_type is defined in host

(define* iterate
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-continue proc iterator env handler fail k))))

(define* iterate-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env 'none handler fail
            (lambda-cont2 (v fail)
              (iterate-continue proc iterator env handler fail k)))))))

(define* iterate-collect
  (lambda (proc generator env handler fail k)
    (let ((iterator (get-iterator generator)))
      (iterate-collect-continue proc iterator env handler fail k))))

(define* iterate-collect-continue
  (lambda (proc iterator env handler fail k)
    (let ((item (next-item iterator)))
      (if (null? item)
          (k '() fail)
          (proc (list item) env 'none handler fail
            (lambda-cont2 (v1 fail)
              (iterate-collect-continue proc iterator env handler fail
                (lambda-cont2 (v2 fail)
                  (k (cons v1 v2) fail)))))))))

;; for improved efficiency
(define* map1
  (lambda (proc list1 env handler fail k)
    (if (null? list1)
      (k '() fail)
      (if (dlr-proc? proc)
	(map1 proc (cdr list1) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (list (car list1))) v2)
	       fail)))
	(proc (list (car list1)) env 'none handler fail
	  (lambda-cont2 (v1 fail)
	    (map1 proc (cdr list1) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

;; for improved efficiency
(define* map2
  (lambda (proc list1 list2 env handler fail k)
    (if (null? list1)
      (k '() fail)
      (if (dlr-proc? proc)
	(map2 proc (cdr list1) (cdr list2) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (list (car list1) (car list2))) v2)
	       fail)))
	(proc (list (car list1) (car list2)) env 'none handler fail
	  (lambda-cont2 (v1 fail)
	    (map2 proc (cdr list1) (cdr list2) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

(define* mapN
  (lambda (proc lists env handler fail k)
    (if (null? (car lists))
      (k '() fail)
      (if (dlr-proc? proc)
	(mapN proc (map cdr lists) env handler fail
	  (lambda-cont2 (v2 fail)
	    (k (cons (dlr-apply proc (map car lists)) v2)
	      fail)))
	(proc (map car lists) env 'none handler fail
	  (lambda-cont2 (v1 fail)
	    (mapN proc (map cdr lists) env handler fail
	      (lambda-cont2 (v2 fail)
		(k (cons v1 v2) fail)))))))))

;; for-each
(define for-each-prim
  (lambda-proc (args env2 info handler fail k2)
    (for-each-primitive (car args) (cdr args) env2 handler fail k2)))

(define* for-each-primitive
  (lambda (proc lists env handler fail k)
    (if (iterator? (car lists))
      (iterate proc (car lists) env handler fail k)
      (let ((arg-list (listify lists)))
	(if (null? (car arg-list))
	  (k void-value fail)
	  (if (dlr-proc? proc)
	    (begin
	      (dlr-apply proc (map car arg-list))
	      (for-each-primitive proc (map cdr arg-list) env handler fail k))
	    (proc (map car arg-list) env 'none handler fail
	      (lambda-cont2 (v1 fail)
		(for-each-primitive proc (map cdr arg-list) env handler fail k)))))))))

;; format
(define format-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((< (length args) 1)
       (runtime-error "incorrect number of arguments to format" info handler fail))
      (else (k2 (apply format args) fail)))))

;; env
(define current-environment-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 env2 fail)))

;; import
(define import-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (import-native args env2) fail)))

;; import-as
(define import-as-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (import-as-native (car args) (cadr args) env2) fail)))

;; import
(define import-from-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (import-from-native (car args) (cdr args) env2) fail)))

;; not
(define not-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to not" info handler fail))
      (else (k2 (not (true? (car args))) fail)))))

;; printf
(define printf-prim
  (lambda-proc (args env2 info handler fail k2)
    (apply printf args)
    (k2 void-value fail)))

;; vector
(define vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply vector_native args) fail)))

(define-native vector_native
  (lambda args
    (apply vector args)))

;; vector-set!
(define vector-set!-prim
  (lambda-proc (args env2 info handler fail k2)
    (vector-set! (car args) (cadr args) (caddr args))
    (k2 void-value fail)))

;; vector-ref
(define vector-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply vector-ref args) fail)))

;; make-vector
(define make-vector-prim
  (lambda-proc (args env2 info handler fail k2)
    (k2 (apply make-vector args) fail)))

;; error
(define error-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 1 args))
       (runtime-error "incorrect number of arguments to 'error' (should at least 1)" info handler fail))
      (else
       (let* ((location (format "Error in '~a': " (car args)))
	      (message (string-append location (apply format (cdr args)))))
	 (runtime-error message info handler fail))))))

;; list-ref
(define list-ref-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to list-ref" info handler fail))
      (else (k2 (apply list-ref args) fail)))))

;; current-directory
(define current-directory-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args) (k2 (current-directory) fail))
      ((length-one? args)
       (if (string? (car args))
	   (k2 (current-directory (car args)) fail)
	   (runtime-error "directory must be a string" info handler fail)))
      (else (runtime-error "incorrect number of arguments to current-directory" info handler fail)))))

(define round-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (number? (car args)))
       (k2 (round (car args)) fail))
      (else
       (runtime-error "round requires exactly one number" info handler fail)))))

(define use-stack-trace-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (boolean? (car args)))
       (begin
	 (set-use-stack-trace! (car args))
	 (k2 void-value fail)))
      ((null? args)
       (k2 *use-stack-trace* fail))
      (else
       (runtime-error "use-stack-trace requires exactly one boolean or nothing" info handler fail)))))

(define use-tracing-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((and (length-one? args) (boolean? (car args)))
       (begin
	 (set! *tracing-on?* (true? (car args)))
	 (k2 void-value fail)))
      ((null? args)
       (k2 *tracing-on?* fail))
      (else
       (runtime-error "use-tracing requires exactly one boolean or nothing" info handler fail)))))

(define eqv?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to eqv?" info handler fail))
     (else (k2 (apply eqv? args) fail)))))

(define vector?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to vector?" info handler fail))
     (else (k2 (apply vector? args) fail)))))

(define atom?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to atom?" info handler fail))
     (else (k2 (apply atom? args) fail)))))

(define iter?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to iter?" info handler fail))
     (else (k2 (apply iter? args) fail)))))

(define getitem-prim
  (lambda-proc (args env2 info handler fail k2)
       (k2 (apply getitem-native args) fail)))

(define setitem-prim
  (lambda-proc (args env2 info handler fail k2)
    (apply setitem-native args)
    (k2 void-value fail)))

(define hasitem-prim
  (lambda-proc (args env2 info handler fail k2)
       (k2 (apply hasitem-native args) fail)))

(define getattr-prim
  (lambda-proc (args env2 info handler fail k2)
       (k2 (apply getattr-native args) fail)))

(define setattr-prim
  (lambda-proc (args env2 info handler fail k2)
    (apply setattr-native args)
    (k2 void-value fail)))

(define hasattr-prim
  (lambda-proc (args env2 info handler fail k2)
       (k2 (apply hasattr-native args) fail)))

(define list?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to list?" info handler fail))
     (else (k2 (apply list? args) fail)))))

(define procedure?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-one? args))
      (runtime-error "incorrect number of arguments to procedure?" info handler fail))
     (else (k2 (procedure-object? (car args)) fail)))))

(define string<?-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
     ((not (length-two? args))
      (runtime-error "incorrect number of arguments to string<?" info handler fail))
     (else (k2 (apply string<? args) fail)))))

(define float-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to float" info handler fail))
      (else (k2 (apply float args) fail)))))

(define globals-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (null? args))
       (runtime-error "incorrect number of arguments to globals" info handler fail))
      (else (k2 (apply globals args) fail)))))

(define int-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to int" info handler fail))
      (else (k2 (apply truncate-to-integer args) fail)))))

;; for the Scheme version only (gets ignored when transformed to Python)
(define-native truncate-to-integer
  (lambda (x)
    (inexact->exact (truncate x))))

(define assq-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to assq" info handler fail))
      (else (k2 (apply assq args) fail)))))

(define dict-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((null? args) (k2 (apply-native dict (list '())) fail))
      (else (make-dict-tuples (car args) env2 info handler fail
	      (lambda-cont2 (tuples fail)
		(k2 (apply-native dict (list tuples)) fail)))))))

(define* make-dict-tuples
  (lambda (associations env2 info handler fail k2)
    (cond
      ((null? associations) (k2 '() fail))
      (else (make-dict-tuples (cdr associations) env2 info handler fail
	      (lambda-cont2 (v fail)
		(let ((key (to-string (car (car associations))))
		      (value (caddr (car associations))))
		  (k2 (cons (list key value) v) fail))))))))

(define to-string
  (lambda (obj)
    (cond
      ((symbol? obj) (symbol->string obj))
      (else obj))))

;; so that we can use scheme or python natives anywhere
;; only needed until apply can use associations with scheme
;; native functions
(define apply-native
  (lambda (proc args)
    (if (dlr-proc? proc)
	(dlr-apply proc args)
	(apply proc args))))

(define property-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to property" info handler fail))
      (else (k2 (apply property args) fail)))))

(define rational-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to rational" info handler fail))
      (else (k2 (apply / args) fail)))))

(define reset-toplevel-env-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (null? args))
       (runtime-error "incorrect number of arguments to reset-toplevel-env" info handler fail))
      (else (k2 (apply reset-toplevel-env args) fail)))))

(define sort-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to sort" info handler fail))
      (else (sort-native args env2 info handler fail k2)))))

;; for speed purposes, the python version of calysto uses python's
;; native sort function instead of sort-native, sort-elements, and
;; insert-element, which are only used by the scheme version.

(define* sort-native
  (lambda (args env2 info handler fail k2)
    (let ((pred (car args))
	  (elements (cadr args)))
      (sort-elements pred elements env2 info handler fail k2))))

(define* sort-elements
  (lambda (pred elements env2 info handler fail k2)
    (cond
      ((null? elements) (k2 '() fail))
      (else (sort-elements pred (cdr elements) env2 info handler fail
	      (lambda-cont2 (v fail)
		(insert-element pred (car elements) v env2 info handler fail k2)))))))

(define* insert-element
  (lambda (proc x elements env2 info handler fail k2)
    (cond
      ((null? elements) (k2 (list x) fail))
      (else (proc (list x (car elements)) env2 info handler fail
	      (lambda-cont2 (bool fail)
		(if bool
		  (k2 (cons x elements) fail)
		  (insert-element proc x (cdr elements) env2 info handler fail
		    (lambda-cont2 (v fail)
		      (k2 (cons (car elements) v) fail))))))))))

(define string-append-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-at-least? 2 args))
       (runtime-error "incorrect number of arguments to string-append" info handler fail))
      (else (k2 (apply string-append args) fail)))))

(define string-split-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-two? args))
       (runtime-error "incorrect number of arguments to string-split" info handler fail))
      (else (k2 (apply string-split args) fail)))))

(define typeof-prim
  (lambda-proc (args env2 info handler fail k2)
    (cond
      ((not (length-one? args))
       (runtime-error "incorrect number of arguments to typeof" info handler fail))
      (else (k2 (apply type args) fail)))))

;; for the Scheme version only (gets ignored when transformed to Python)
(define-native type
  (lambda (x)
    (cond
      ((environment? x) '<type:environment>)
      ((exception-object? x) '<type:exception>)
      ((procedure? x) '<type:procedure>)
      ((number? x) '<type:number>)
      ((symbol? x) '<type:symbol>)
      ((pair? x) '<type:pair>)
      ((string? x) '<type:string>)
      ((null? x) '<type:null>)
      ((boolean? x) '<type:boolean>)
      ((char? x) '<type:char>)
      ((vector? x) '<type:vector>)
      ((box? x) '<type:box>)
      (else '<type:unknown>))))

(define use-lexical-address-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (apply use-lexical-address args) fail)))

(define-native host-environment-native
  (lambda ()
    "scheme"))

(define host-environment-prim
  (lambda-proc (args env2 info handler fail k2)
      (k2 (host-environment-native) fail)))

(define get-exception-message-prim
  (lambda-proc (args env2 info handler fail k2)
     (k2 (get-exception-message (car args)) fail)))

;; -----------------------------------------------------
;; To add a new primitive:
;; -----------------------------------------------------
;; 1. Add new NAME-prim primitive procedures above here
;; 2. add (list 'NAME NAME-prim) to toplevel-env, below
;; 3. add NAME to Scheme.xx implementation
;; 4. if you use map or apply on it internally, and the
;;    the implementation language cannot pass functions
;;    as arguments, then add NAME_proc to Scheme.xx
;; -----------------------------------------------------

(define make-toplevel-env
  (lambda ()
    (let ((primitives
	   (list
	    (list '% modulo-prim "(% arg0 arg1): modulo procedure for two arguments (aliases mod and modulo)")
	    (list '* times-prim "(* ...): multiplication procedure; multiplies all arguments")
	    (list '+ plus-prim "(+ ...): addition procedure; adds all arguments")
	    (list '- minus-prim "(- ...): subtraction procedure; subtracts all arguments")
	    (list '/ divide-prim "(/ ...): division procedure; divides all arguments")
	    (list '// quotient-prim "(// arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases div and quotient)")
	    (list '< lt-prim "(< arg0 arg1): less-than procedure for two arguments")
	    (list '<= lt-or-eq-prim "(<= arg0 arg1): less-than or equal procedure for two arguments")
	    (list '= equal-sign-prim "(= arg0 arg1): numeric equality procedure for two arguments")
	    (list '> gt-prim "(> arg0 arg1): greater-than procedure for two arguments")
	    (list '>= gt-or-eq-prim "(>= arg0 arg1): greater-than or equal procedure for two arguments")
	    (list 'SCHEMEPATH SCHEMEPATH "List of search directories used with (load NAME)")
	    (list 'abort abort-prim "(abort) : aborts processing and returns to top level")
	    (list 'abs abs-prim "(abs value): absolute value procedure")
	    (list 'append append-prim "(append ...): append lists together into a single list")
	    (list 'apply apply-prim "(apply PROCEDURE '(args...)): apply the PROCEDURE to the args")
	    (list 'assert assert-prim "(assert OPERATOR EXPRESSION ANSWER): assert that (OPERATOR EXPRESSION ANSWER) is #t")
	    (list 'assq assq-prim "(assq ...): ")
	    (list 'assv assv-prim "(assv KEY ((ITEM VALUE) ...)): look for KEY in ITEMs; return matching (ITEM VALUE) or #f if not found")
	    (list 'atom? atom?-prim "(atom? ITEM): return #t if ITEM is a atom, #f otherwise")
	    (list 'boolean? boolean?-prim "(boolean? ITEM): return #t if ITEM is a boolean value")
	    (list 'box box-prim "(box ITEM): return a new box containing ITEM")
	    (list 'box? box?-prim "(box? ITEM): return #t if ITEM is a boxed value")
	    (list 'caaaar caaaar-prim "caaaar ...): ")
	    (list 'caaadr caaadr-prim "(caaadr ...): ")
	    (list 'caaar caaar-prim "(caaar ...): ")
	    (list 'caadar caadar-prim "(caadar ...): ")
	    (list 'caaddr caaddr-prim "(caaddr ...): ")
	    (list 'caadr caadr-prim "(caadr ...): ")
	    (list 'caar caar-prim "(caar ...): ")
	    (list 'cadaar cadaar-prim "(cadaar ...): ")
	    (list 'cadadr cadadr-prim "(cadadr ...): ")
	    (list 'cadar cadar-prim "(cadar ...): ")
	    (list 'caddar caddar-prim "(caddar ...): ")
	    (list 'cadddr cadddr-prim "(cadddr ...): ")
	    (list 'caddr caddr-prim "(caddr ITEM): return the (car (cdr (cdr ITEM)))")
	    (list 'cadr cadr-prim "(cadr ITEM): return the (car (cdr ITEM))")
	    (list 'call-with-current-continuation call/cc-prim "(call-with-current-continuation ...): ")
	    (list 'call/cc call/cc-prim "(call/cc ...): ")
	    (list 'car car-prim "(car LIST) returns the first element of LIST")
	    (list 'cd current-directory-prim "(cd [PATH]): get the current directory, or set it if PATH is given (alias current-directory)")
	    (list 'cdaaar cdaaar-prim "(cdaaar ...): ")
	    (list 'cdaadr cdaadr-prim "(cdaadr ...): ")
	    (list 'cdaar cdaar-prim "(cdaar ...): ")
	    (list 'cdadar cdadar-prim "(cdadar ...): ")
	    (list 'cdaddr cdaddr-prim "(cdaddr ...): ")
	    (list 'cdadr cdadr-prim "(cdadr ...): ")
	    (list 'cdar cdar-prim "(cdar ...): ")
	    (list 'cddaar cddaar-prim "(cddaar ...): ")
	    (list 'cddadr cddadr-prim "(cddadr ...): ")
	    (list 'cddar cddar-prim "(cddar ...): ")
	    (list 'cdddar cdddar-prim "(cdddar ...): ")
	    (list 'cddddr cddddr-prim "(cddddr ...): ")
	    (list 'cdddr cdddr-prim "(cdddr ...): ")
	    (list 'cddr cddr-prim "(cddr ...): ")
	    (list 'cdr cdr-prim "(cdr LIST) returns rest of LIST after (car LIST)")
	    (list 'char->integer char->integer-prim "(char->integer CHAR): return associated number of CHAR ")
	    (list 'char->string char->string-prim "(char->string CHAR): ")
	    (list 'char-alphabetic? char-alphabetic?-prim "(char-alphabetic? CHAR): return #t if CHAR is an alphabetic character, #f otherwise")
	    (list 'char-numeric? char-numeric?-prim "(char-numeric? CHAR): return #t if CHAR is a whitespace character, #f otherwise")
	    (list 'char-whitespace? char-whitespace?-prim "(char-whitespace? CHAR): return #t if CHAR is a whitespace character, #f otherwise")
	    (list 'char=? char=?-prim "(char=? CHAR1 CHAR2): return #t if CHAR1 has the same values as CHAR2, #f otherwise")
	    (list 'char? char?-prim "(char? ITEM): return #t if ITEM is a character, #f otherwise")
	    (list 'clear-unit-tests clear-unit-tests-prim "(clear-unit-tests): clear old unit tests. Usually run before define-tests")
	    (list 'cons cons-prim "(cons ITEM1 ITEM2): return a list with ITEM1 as car and ITEM2 as cdr (ITEM2 is typically a list)")
	    (list 'current-directory current-directory-prim "(current-directory [PATH]): get the current directory, or set it if PATH is given (alias cd)")
	    (list 'current-environment current-environment-prim "(current-environment): returns the current environment")
	    (list 'current-time current-time-prim "(current-time): returns the current time as number of seconds since 1970-1-1")
	    (list 'cut cut-prim "(cut ARGS...): return to toplevel with ARGS")
	    (list 'dict dict-prim "(dict ...): ")
	    (list 'dir dir-prim "(dir [ITEM]): return items in environment, or, if ITEM is given, the items in module")
	    (list 'display display-prim "(display ITEM): display the ITEM as output")
	    (list 'div quotient-prim "(div arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and quotient)")
	    (list 'eq? eq?-prim "(eq? ITEM1 ITEM2): return #t if ITEM1 is eq to ITEM2, #f otherwise")
	    (list 'equal? equal?-prim "(equal? ITEM1 ITEM2): return #t if ITEM1 is equal to ITEM2, #f otherwise")
	    (list 'eqv? eqv?-prim "(eqv? ITEM1 ITEM2): return #t if ITEM1 and ITEM2 have the same value")
	    (list 'error error-prim "(error NAME MESSAGE): create an exception in NAME with MESSAGE")
	    (list 'eval eval-prim "(eval LIST): evaluates the LIST as a Scheme expression")
	    (list 'eval-ast eval-ast-prim "(eval-ast AST): evaluates the Abstract Syntax Tree as a Scheme expression (see parse and parse-string)")
	    (list 'even? even?-prim "(even? NUMBER): returns #t if NUMBER is odd, #f otherwise")
	    (list 'exit exit-prim "(exit): Exit the interpreter")
	    (list 'expt expt-prim "(expt BASE POWER): raise a base number to a power")
	    (list 'float float-prim "(float NUMBER): return NUMBER as a floating point value")
	    (list 'for-each for-each-prim "(for-each PROCEDURE LIST): apply PROCEDURE to each item in LIST, but don't return results")
	    (list 'format format-prim "(format STRING ITEM ...): format the string with ITEMS as arguments")
	    (list 'get-attr getattr-prim "(get-attr THING ATTR): get the ATTRIBUTE from the THING")
	    (list 'get-completions get-completions-prim "(get-completions ...): returns completions for TAB")
	    (list 'get-item getitem-prim "(get-item THING ITEM): get the ITEM from the THING (dict or vector)")
	    (list 'get-stack-trace get-stack-trace-prim "(get-stack-trace): return the current stack trace")
	    (list 'get-exception-message get-exception-message-prim "(get-exception-message EXCEPTION): get the message from the exception")
	    (list 'globals globals-prim "(globals): get global environment")
	    (list 'has-attr? hasattr-prim "(has-attr? THING ATTR): does the THING have this attribute?")
	    (list 'has-item? hasitem-prim "(has-item? THING ITEM): does the THING (dict or vector) have this ITEM?")
	    (list 'host-environment host-environment-prim "(host-environment): get the host environment (\"python\" or \"scheme\")")
	    (list 'import import-prim "(import MODULE...): import host-system modules; MODULEs are strings")
	    (list 'import-as import-as-prim "(import-as MODULE NAME): import a host-system module; MODULE is a string, and NAME is a symbol or string. Use * for NAME to import into toplevel environment")
	    (list 'import-from import-from-prim "(import-from MODULE NAME...): import from host-system module; MODULE is a string, and NAME is a symbol or string")
	    (list 'int int-prim "(int NUMBER): return NUMBER as an integer")
	    (list 'integer->char integer->char-prim "(integer->char INTEGER): return the assocated character of INTEGER")
	    (list 'iter? iter?-prim "(iter? ITEM): return #t if ITEM is a iterator, #f otherwise")
	    (list 'length length-prim "(length LIST): returns the number of elements in top level of LIST")
	    (list 'list list-prim "(list ITEM ...): returns a list composed of all of the items")
	    (list 'list->string list->string-prim "(list->string LIST): returns the LIST as a string")
	    (list 'list->vector list->vector-prim "(list->vector LIST): returns the LIST as a vector")
	    (list 'list-ref list-ref-prim "(list-ref LIST INDEX): returns the item in LIST at INDEX (zero-based)")
	    (list 'list? list?-prim "(list? ITEM): return #t if ITEM is a list, #f otherwise")
	    (list 'load load-prim "(load FILENAME...): loads the given FILENAMEs")
	    (list 'load-as load-as-prim "(load-as FILENAME MODULE-NAME): load the filename, putting items in MODULE-NAME namespace")
	    (list 'macros macros-prim "(macros): return the names of the macros")
	    (list 'make-set make-set-prim "(make-set LIST): returns a list of unique items from LIST")
	    (list 'make-vector make-vector-prim "(make-vector LENGTH): returns a vector of length LENGTH")
	    (list 'map map-prim "(map PROCEDURE LIST...): apply PROCEDURE to each element of LIST, and return return results")
	    (list 'max max-prim "(max ...): returns the maximum value from the list of values")
	    (list 'member member-prim "(member ITEM LIST): return LIST if ITEM in top level of LIST")
	    (list 'memq memq-prim "(memq ...): ")
	    (list 'memv memv-prim "(memv ...): ")
	    (list 'min min-prim "(min ...): returns the minimum value from the list of values")
	    (list 'mod modulo-prim "(mod arg0 arg1): modulo procedure for two arguments (aliases % and modulo)")
	    (list 'modulo modulo-prim "(modulo arg0 arg1): modulo procedure for two arguments (aliases mod and %)")
	    (list 'newline newline-prim "(newline): displays a new line in output")
	    (list 'not not-prim "(not ITEM): returns the boolean not of ITEM; ITEM is only #t when #t, otherwise #f")
	    (list 'null? null?-prim "(null? ITEM): return #t if ITEM is empty list, #f otherwise")
	    (list 'number->string number->string-prim "(number->string NUMBER): return NUMBER as a string")
	    (list 'number? number?-prim "(number? ITEM): return #t if ITEM is a number, #f otherwise")
	    (list 'odd? odd?-prim "(odd? NUMBER): returns #t if NUMBER is even, #f otherwise")
	    (list 'pair? pair?-prim "(pair? ITEM): ")
	    (list 'parse parse-prim "(parse LIST): parse a list; returns Abstract Syntax Tree (AST)")
	    (list 'parse-string parse-string-prim "(parse-string STRING): parse a string; returns Abstract Syntax Tree (AST)")
	    (list 'print print-prim "(print ITEM): ")
	    (list 'printf printf-prim "(printf FORMAT ARGS...): ")
	    (list 'procedure? procedure?-prim "(procedure? ITEM): return #t if ITEM is a procedure, #f otherwise")
	    (list 'property property-prim "(property ...): ")
	    (list 'python-eval python-eval-prim "(python-eval PYTHON-EXPRESSION [globals [locals]]): return the result of evaluating PYTHON-EXPRESSION string")
	    (list 'python-exec python-exec-prim "(python-exec PYTHON-STATEMENTS [globals [locals]]): return the result of evaluating PYTHON-STATEMENTS string")
	    (list 'quit exit-prim "(quit): Exit the interpreter")
	    (list 'quotient quotient-prim "(quotient arg0 arg1): quotient procedure for rationals/ints; divides arg0 by arg1 (aliases // and div)")
	    (list 'rac rac-prim "(rac LIST): return the last item of LIST")
	    (list 'random random-prim "(random N): return a random number in the range [0, N)")
	    (list 'range range-prim "(range END), (range START END), or (RANGE START END STEP): (all integers)")
	    (list 'rational rational-prim "(rational NUMERATOR DENOMINTAOR): return a rational number")
	    (list 'rdc rdc-prim "(rdc LIST): return everything but last item in LIST")
	    (list 'read-string read-string-prim "(read-string ...): ")
	    (list 'remainder remainder-prim "(remainder NUMBER1 NUMBER2): returns the remainder after dividing NUMBER1 by NUMBER2")
	    (list 'require require-prim "(require ...): ")
	    (list 'reset-toplevel-env reset-toplevel-env-prim "(reset-toplevel-env): reset the toplevel environment")
	    (list 'reverse reverse-prim "(reverse LIST): ")
	    (list 'round round-prim "(round NUMBER): round NUMBER to the nearest integer (may return float)")
	    (list 'set-attr! setattr-prim "(setattr THING ATTR VALUE): sets THING.ITEM with VALUE")
	    (list 'set-car! set-car!-prim "(set-car! LIST ITEM): set the car of LIST to be ITEM")
	    (list 'set-cdr! set-cdr!-prim "(set-cdr! LIST ITEM): set the car of LIST to be ITEM (which is typically a list)")
	    (list 'set-item! setitem-prim "(setitem THING ITEM VALUE): sets THING[ITEM] with VALUE")
	    (list 'snoc snoc-prim "(snoc ITEM LIST): cons the ITEM onto the end of LIST")
	    (list 'sort sort-prim "(sort PROCEDURE LIST): sort the list using PROCEDURE to compare items")
	    (list 'sqrt sqrt-prim "(sqrt NUMBER): return the square root of NUMBER")
	    (list 'string string-prim "(string ITEM): returns ITEM as a string")
	    (list 'string->list string->list-prim "(string->list STRING): string STRING as a list of characters")
	    (list 'string->number string->number-prim "(string->number STRING): return STRING as a number")
	    (list 'string->symbol string->symbol-prim "(string->symbol STRING): return STRING as a symbol")
	    (list 'string-append string-append-prim "(string-append STRING1 STRING2): append two strings together")
	    (list 'string-join string-join-prim "(string-join \", \" '(1 2 3)): gives \"1, 2, 3\"")
	    (list 'string-length string-length-prim "(string-length STRING): returns the length of a string")
	    (list 'string-ref string-ref-prim "(string-ref STRING INDEX): return the character of STRING at position INDEX")
	    (list 'string-split string-split-prim "(string-split STRING CHAR): return a list with substrings of STRING where split by CHAR")
	    (list 'string<? string<?-prim "(string<? STRING1 STRING2): compare two strings to see if STRING1 is less than STRING2")
	    (list 'string=? string=?-prim "(string=? STRING1 STRING2): return #t if STRING1 is the same as STRING2, #f otherwise")
	    (list 'string? string?-prim "(string? ITEM): return #t if ITEM is a string, #f otherwise")
	    (list 'substring substring-prim "(substring STRING START [END]): return the substring of STRING starting with position START and ending before END. If END is not provided, it defaults to the length of the STRING")
	    (list 'symbol->string symbol->string-prim "(symbol->string SYMBOL): return SYMBOL as a string")
	    (list 'symbol? symbol?-prim "(symbol? ITEM): return #t if ITEM is a symbol, #f otherwise")
	    (list 'typeof typeof-prim "(typeof ITEM): returns type of ITEM")
	    (list 'unbox unbox-prim "(unbox BOX): return the contents of BOX")
	    (list 'unparse unparse-prim "(unparse AST): ")
	    (list 'unparse-procedure unparse-procedure-prim "(unparse-procedure ...): ")  ;; unparse should be in CPS
	    (list 'use-lexical-address use-lexical-address-prim "(use-lexical-address [BOOLEAN]): get lexical-address setting, or set it on/off if BOOLEAN is given")
	    (list 'use-stack-trace use-stack-trace-prim "(use-stack-trace BOOLEAN): set stack-trace usage on/off")
	    (list 'use-tracing use-tracing-prim "(use-tracing [BOOLEAN]): get tracing setting, or set it on/off if BOOLEAN is given")
	    (list 'vector vector-prim "(vector [ITEMS]...): return ITEMs as a vector")
	    (list 'vector->list vector->list-prim "(vector->list VECTOR): return VECTOR as a list")
	    (list 'vector-length vector-length-prim "(vector-length VECTOR): returns length of VECTOR")
	    (list 'vector-ref vector-ref-prim "(vector-ref VECTOR INDEX): ")
	    (list 'vector-set! vector-set!-prim "(vector-set! VECTOR INDEX VALUE): sets the item at INDEX of VECTOR")
	    (list 'vector? vector?-prim "(vector? ITEM): return #t if ITEM is a vector, #f otherwise")
	    (list 'void void-prim "(void): The null value symbol")
	    (list 'zero? zero?-prim "(zero? NUMBER): return #t if NUMBER is equal to zero, #f otherwise")
	    )))
      (make-initial-env-extended (map car primitives) (map cadr primitives) (map caddr primitives)))))

;; this is here as a hook for extending environments in host etc.
(define-native make-initial-env-extended
  (lambda (names procs docstrings)
    (make-initial-environment names procs docstrings)))

(define reset-toplevel-env
  (lambda ()
    (set! toplevel-env (make-toplevel-env))
    void-value))

(define toplevel-env 'undefined)

;;------------------------------------------------------------------------
;; Host support

(define make-external-proc
  (lambda (external-function-object)
    (lambda-proc (args env2 info handler fail k2)
      (k2 (apply* external-function-object args) fail))))

;; Named parameters, default values, and varargs:

(define process-formals-and-args
  (lambda (params args info handler fail)
    (cons
     (process-formals params info handler fail)
     (process-args args params info handler fail))))

(define process-formals
  (lambda (params info handler fail)
    ;;(printf "formals: ~a~%" params)
    (map get-symbol params)))

(define process-args
  (lambda (args params info handler fail)
    ;;(printf "args: ~a params: ~a~%" args params)
    args))

;; (define process-args
;;   (lambda (args params info handler fail)
;;     ;;(printf "args: ~a params: ~a~%" args params)
;;     ;; args))
;;     ;; FIXME: (a) ()
;;     (let ((retval (get-values-for-params
;; 		   params
;; 		   (get-arg-associations args params #f info handler fail)
;; 		   '() info handler fail)))
;;       ;;(printf "retval: ~a~%" retval)
;;       retval)))

(define get-values-for-params
  (lambda (params associations used info handler fail)
    ;; go down params and get values for each
    ;;(printf "get-values-for-params params: ~a~%" params)
    ;;(printf "get-values-for-params associations: ~a~%" associations)
    (cond
     ((null? params)
      (if (and (not (null? associations))
	       (association? (car associations))
	       (eq? (caar associations) '*))
	  (list (get-value (car associations)))
	  '()))
     (else
      (cons (get-value-from-associations (car params) associations info handler fail)
	    (get-values-for-params (cdr params)
				   associations
				   (cons (car params) used) info handler fail))))))

(define get-value-from-associations
  (lambda (param associations info handler fail)
    (let* ((symbol (get-symbol param))
	   (value (assq symbol associations)))
      (cond
       (value (get-value value))
       ((association? param) (get-value param))
       (else (runtime-error (format "missing parameter: ~a" param) info handler fail))))))

(define get-arg-associations
  (lambda (args params must-be-association info handler fail)
    (cond
     ((null? args) '())
     ((association? (car args)) ;; named arg
      (cons (car args)
	    (get-arg-associations (cdr args) params #t info handler fail)))
     (must-be-association
      (runtime-error (format "non-keyword arg following keyword arg: ~a" (car args)) info handler fail))
     ((null? params) ;; should be a runt FIXME
      (list (association '* args))) ;; return args, which should values
     (else (cons
	    (association (get-symbol (car params)) (car args))
	    (get-arg-associations (cdr args) (cdr params) #f info handler fail))))))

(define get-symbol
  (lambda (item)
    (cond
     ((association? item) (car item))
     ((symbol? item) item)
     (else (error 'get-symbol "invalid symbol ~a" item)))))

(define get-value
  (lambda (item)
    (cond
     ((association? item) (caddr item))
     (else item))))

(define association?
  (lambda (x)
    (and (list? x) (= (length x) 3) (eq? (cadr x) ':))))

(define make-associations
  (lambda (dict)
    (cond
      ((null? dict) '())
      (else (let ((keyword (caar dict))
		  (value (caddar dict)))
	      (cons (association keyword value) (make-associations (cdr dict))))))))

(load "transformer-macros.ss")

;; Unification pattern-matcher

(define pattern?
  (lambda (x)
    (or (null? x)
	(number? x)
	(boolean? x)
	(symbol? x)
	(and (pair? x)
	     (pattern? (car x))
	     (pattern? (cdr x))))))

(define pattern-variable?
  (lambda (x)
    (and (symbol? x)
	 (equal? "?" (substring (symbol->string x) 0 1)))))

(define constant?
  (lambda (x)
    (and (not (pattern-variable? x))
	 (not (pair? x)))))

(define* occurs?
  (lambda (var pattern k)
    (cond
      ((constant? pattern) (k #f))
      ((pattern-variable? pattern) (k (equal? var pattern)))
      (else (occurs? var (car pattern)
	      (lambda-cont (bool)
		(if bool
		  (k #t)
		  (occurs? var (cdr pattern) k))))))))

;;-----------------------------------------------------------------------------------------------

;; for testing only
(define up
  (lambda (p1 p2)
    (unify-patterns p1 p2 init-cont)))

;; for testing only
(define aup
  (lambda (s1 s2)
    (let* ((ap1 (aread-string s1))
	   (ap2 (aread-string s2))
	   (p1 (unannotate ap1))
	   (p2 (unannotate ap2)))
      (unify-patterns^ p1 p2 ap1 ap2
	(lambda-cont (s)
	  (if (not s)
	    s
	    (print-sub s)))))))

(define print-sub
  (lambda (s)
    (record-case (cdr s)
      (empty () (void))
      (unit (new-var new-pattern new-apattern)
	(display new-var)
	(display " = ")
	(newline)
	(pretty-print new-pattern)
	(pretty-print new-apattern)
	(newline))
      (composite (s1 s2)
	(print-sub s1)
	(print-sub s2))
      (else (error 'print-sub "bad substitution: ~s" s)))))

;;-----------------------------------------------------------------------------------------------
;; annotated version

(define* unify-patterns^
  (lambda (p1 p2 ap1 ap2 k)    ;; k receives: subst/#f
    (cond
      ((pattern-variable? p1)
       (if (pattern-variable? p2)
	 (k (make-sub 'unit p1 p2 ap2))
	 (occurs? p1 p2
	   (lambda-cont (bool)
	     (if bool
	       (k #f)
	       (k (make-sub 'unit p1 p2 ap2)))))))
      ((pattern-variable? p2) (unify-patterns^ p2 p1 ap2 ap1 k))
      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
      ((and (pair? p1) (pair? p2)) (unify-pairs^ p1 p2 ap1 ap2 k))
      (else (k #f)))))

(define* unify-pairs^
  (lambda (pair1 pair2 apair1 apair2 k)
    (unify-patterns^ (car pair1) (car pair2) (car^ apair1) (car^ apair2)
      (lambda-cont (s-car)
	(if (not s-car)
	  (k #f)
	  (instantiate^ (cdr pair1) s-car (cdr^ apair1)
	    (lambda-cont2 (new-cdr1 new-acdr1)
	      (instantiate^ (cdr pair2) s-car (cdr^ apair2)
		(lambda-cont2 (new-cdr2 new-acdr2)
		  (unify-patterns^ new-cdr1 new-cdr2 new-acdr1 new-acdr2
		    (lambda-cont (s-cdr)
		      (if (not s-cdr)
			(k #f)
			(k (make-sub 'composite s-car s-cdr))))))))))))))

(define* instantiate^
  (lambda (pattern s ap k2)   ;; k2 receives: sexp, asexp
    (cond
      ((constant? pattern) (k2 pattern ap))
      ((pattern-variable? pattern) (apply-sub^ s pattern ap k2))
      ((pair? pattern)
       (instantiate^ (car pattern) s (car^ ap)
	 (lambda-cont2 (a aa)
	   (instantiate^ (cdr pattern) s (cdr^ ap)
	     (lambda-cont2 (b ab)
	       (k2 (cons a b) (cons^ aa ab (get-source-info ap))))))))
      (else (error 'instantiate^ "bad pattern: ~a" pattern)))))

;;------------------------------------------------------------------
;; Substitutions represented as data structures

(define make-sub
  (lambda args
    (cons 'substitution args)))

(define* apply-sub^
  (lambda (s var avar k2)        ;; k2 receives: sexp, asexp
    (record-case (cdr s)
      (empty () (k2 var avar))
      (unit (new-var new-pattern new-apattern)
	(if (equal? var new-var)
	  (k2 new-pattern new-apattern)
	  (k2 var avar)))
      (composite (s1 s2)
	(apply-sub^ s1 var avar
	  (lambda-cont2 (pattern apattern)
	    (instantiate^ pattern s2 apattern k2))))
      (else (error 'apply-sub^ "bad substitution: ~a" s)))))

;;-----------------------------------------------------------------------------------------------
;; unannotated version

;;(define* unify-patterns
;;  (lambda (p1 p2 k)
;;    (cond
;;      ((pattern-variable? p1)
;;       (if (pattern-variable? p2)
;;	 (k (make-sub 'unit p1 p2))
;;	 (occurs? p1 p2
;;	   (lambda-cont (bool)
;;	     (if bool
;;	       (k #f)
;;	       (k (make-sub 'unit p1 p2)))))))
;;      ((pattern-variable? p2) (unify-patterns p2 p1 k))
;;      ((and (constant? p1) (constant? p2) (equal? p1 p2)) (k (make-sub 'empty)))
;;      ((and (pair? p1) (pair? p2)) (unify-pairs p1 p2 k))
;;      (else (k #f)))))

;;(define* unify-pairs
;;  (lambda (pair1 pair2 k)
;;    (unify-patterns (car pair1) (car pair2)
;;      (lambda-cont (s-car)
;;	(if (not s-car)
;;	  (k #f)
;;	  (instantiate (cdr pair1) s-car
;;	    (lambda-cont (new-cdr1)
;;	      (instantiate (cdr pair2) s-car
;;		(lambda-cont (new-cdr2)
;;		  (unify-patterns new-cdr1 new-cdr2
;;		    (lambda-cont (s-cdr)
;;		      (if (not s-cdr)
;;			(k #f)
;;			(k (make-sub 'composite s-car s-cdr))))))))))))))

;;(define* instantiate
;;  (lambda (pattern s k)
;;    (cond
;;      ((constant? pattern) (k pattern))
;;      ((pattern-variable? pattern) (apply-sub s pattern k))
;;      ((pair? pattern)
;;       (instantiate (car pattern) s
;;	 (lambda-cont (a)
;;	   (instantiate (cdr pattern) s
;;	     (lambda-cont (b)
;;	       (k (cons a b)))))))
;;      (else (error 'instantiate "bad pattern: ~a" pattern)))))

;;;;(define extend-sub
;;;;  (lambda (old-s new-var new-pattern)
;;;;    (list 'extended new-var new-pattern old-s)))

;;(define* apply-sub
;;  (lambda (s var k)
;;    (record-case (cdr s)
;;      (empty () (k var))
;;;;      (extended (new-var new-pattern old-s)
;;;;	(if (equal? var new-var)
;;;;	  (k new-pattern)
;;;;	  (apply-sub old-s var k)))
;;      (unit (new-var new-pattern)
;;	(if (equal? var new-var)
;;	  (k new-pattern)
;;	  (k var)))
;;      (composite (s1 s2)
;;	(apply-sub s1 var
;;	  (lambda-cont (pattern)
;;	    (instantiate pattern s2 k))))
;;      (else (error 'apply-sub "bad substitution: ~a" s)))))
