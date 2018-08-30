;; Calico Scheme scanner and s-expression reader
;;
;; includes support for vectors, rationals, exponents, and backquote
;;
;; Written by James B. Marshall and Douglas S. Blank
;; jmarshall@slc.edu
;; http://science.slc.edu/~jmarshall
;; dblank@brynmawr.edu
;; http://cs.brynmawr.edu/~dblank

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

