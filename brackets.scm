; From Eli Barzilay, eli@barzilay.org

;> (require "brackets.scm") 
;> (use-bracket-readtable) 
;> ([+ _ 1] 10) 
;11

(module brackets mzscheme
  
; main reader function for []s
; recursive read starts with default readtable's [ parser,
; but nested reads still use the curent readtable:

(define (delimiter? char)
  (or (eof-object? char)
      (char-whitespace? char)
      (memv char '(#\) #\] #\;))))

(define (read-square-brackets ch port src line col pos)
  `(make-br-fn ,(read/recursive port #\[ #f)))
  
(define (read-curly-brackets ch port src line col pos)
  ; ({x y} z) does the same as (@x%y z). It's bsolete, let's find new good semantics for {
  `(ofn map (ofn ,@(read/recursive port #\{ #f))))

(define (read-angled-brackets ch port src line col pos)
  (if (delimiter? (peek-char port)) (if (eqv? ch #\>) `> '<)
    (case (peek-char port)
      ((#\=) (read-char port)
	     (if (delimiter? (peek-char port)) (if (eqv? ch #\>) '>= '<=)
	       (error (string-append "Symbol may not start with " (list->string `(,ch #\=))))))
      ((#\: #\_) (error (string-append "SSexp may not start with " (list->string `(,ch)) ". Use lt | gt | le | ge")))
      ((#\[) `(,(if (eqv? ch #\>) `(ofn map ,(read port)))))
      (else `(,(if (eqv? ch #\>) 'car 'cdr) ,(read port))))))

(define (read-extended-quotation ch port src line col pos)
  (let ((uid (gensym)))
    (if (delimiter? (peek-char port)) (string->symbol (list->string (list ch)))
      (case ch
	((#\@) `(ofn map ,(read port)))
	((#\$) `(ofn apply ,(read port)))
	;((#\&) `(fn args (reduce ,(read port) args)))
	((#\&) `(fn ,uid (,(read port) ,uid)))
	((#\^) `(fn (,uid) (cons (,(read port) ,uid) ,uid)))
	((#\!) `(fn (,uid) (= ,(read port) ,uid)))))))

(define bracket-readtable
  (make-readtable
    (make-readtable
      (make-readtable
	(make-readtable
	  (make-readtable
	    (make-readtable
	      (make-readtable
		(make-readtable
		  (make-readtable
		    #f
		    #\[ 'terminating-macro read-square-brackets)
		  #\{ 'terminating-macro read-curly-brackets)
		#\> 'non-terminating-macro read-angled-brackets)
	      #\< 'non-terminating-macro read-angled-brackets)
	    #\@ 'non-terminating-macro read-extended-quotation)
	  #\& 'non-terminating-macro read-extended-quotation)
	#\^ 'non-terminating-macro read-extended-quotation)
      #\$ 'non-terminating-macro read-extended-quotation)
    #\! 'non-terminating-macro read-extended-quotation))

; call this to set the global readtable

(provide use-bracket-readtable)

(define (use-bracket-readtable)
  (current-readtable bracket-readtable))
  
; these two implement the required functionality for #reader
    
;(define (*read inp)
;  (parameterize ((current-readtable bracket-readtable))
;    (read inp)))

(define (*read . args)
  (parameterize ((current-readtable bracket-readtable))
    (read (if (null? args) (current-input-port) (car args)))))

(define (*read-syntax src port)
  (parameterize ((current-readtable bracket-readtable))
    (read-syntax src port)))

; and the need to be provided as `read' and `read-syntax'

(provide (rename *read read) (rename *read-syntax read-syntax))

)
