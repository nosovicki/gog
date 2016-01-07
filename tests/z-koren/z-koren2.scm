(define-macro (dd . vars)
  (let loop ((ls vars))
    (if (null? ls) '(begin (newline) (force-output))
      (cons 'begin
            (list `(map display	(list ',(car ls) ": " (if (ap? ,(car ls)) (ap->str ,(car ls)) ,(car ls)) "   "))
                  (loop (cdr ls)))))))

(define *radix-len* 2)
(define *radix* (expt 10 *radix-len*))

(define (scomp a b)
  (if (not (eq? (ap-sign a) (ap-sign b))) (if (eq? (ap-sign a) '+) 1 -1)
    ((if (eq? (ap-sign a) '+) + -)
     (if (< (ap-mag a) (ap-mag b)) -1
       (if (> (ap-mag a) (ap-mag b)) 1
	 (let comp ((a (ap-ls a)) (b (ap-ls b)))
	   (if (null? a) (if (null? b) 0 -1)
	     (if (null? b) 1
	       (if (< (car a) (car b)) -1
		 (if (> (car a) (car b)) 1
		   (comp (cdr a) (cdr b))))))))))))

(define (abs ap)
  (if (eq? (ap-sign ap) '+) ap
    (inverse ap)))

(define (transfer n ls)
  (cons (+ n (if (null? ls) 0 (car ls)))
        (if (null? ls) '() (cdr ls))))

(define (sreverse ls)
  (rr *radix-len* (reverse (rr 1 ls))))

(define (splus s1 s2)
  (if (eq? (ap-sign s1) '-) (sminus s2 s1)
    (if (eq? (ap-sign s2) '-) (sminus s1 s2)
      (ap
	(let plus ((a (ap-ls s1)) (b (ap-ls s2)))
	  (let ((r (+ (car a) (car b))))
	    (map display (list "*plus* " a " " b " " r "\n"))
	    (cons (modulo r *radix*)
		  (plus (transfer (inexact->exact (floor (/ r *radix*))) (cdr a))
			(cdr b)))))))))

(define (smul s1 s2)
  (ap
    (let loop ((a (reverse (ap-ls s1))) (b (reverse (ap-ls s2))))
      (if (null? b) '()
	(if (null? a) '()
	  (if (pair? b) (ap-ls (splus (ap (loop a (car b))) (ap (append (loop a (cdr b)) '(0)))))
	    (let ((r (* (car a) b)))
	      (ap-ls (splus (ap (list r)) (ap (append (loop (cdr a) b) '(0))))))))))))

(define (denul ls)
    (if (or (null? ls) (null? (cdr ls))) ls
      (if (eq? #\0 (car ls)) (denul (cdr ls))
        ls)))

(define (inverse ap)
  (mk-ap (if (eq? (ap-sign ap) '+) '- '+)
	 (ap-mag ap)
	 (ap-radix ap)
	 (ap-ls ap)))

(define (sminus s1 s2)
  (if (eq? (ap-sign s2) '-) (splus s1 s2)
    (if (eq? (ap-sign s1) '-) (inverse (splus s1 s2))
      (let ((rel (scomp s1 s2)))
	(if (= rel 0) (ap '(0))
	  (if (< rel 0) (inverse (sminus s2 s1))
	    (ap
	      (reverse
		(let loop ((a (reverse (ap-ls s1))) (b (reverse (ap-ls s2))))
		  (if (null? b) a
		    (let ((r (- (car a) (car b))))
		      (if (>= r 0) (cons r (loop (cdr a) (cdr b)))
			(cons (+ *radix* r) (loop (cdr a) (transfer 1 (cdr b))))))))))))))))

(define (Q-guess a b)
  (if (< (+ 1 (ap-mag a)) (ap-mag b)) 0
    (floor (/ (car (ap-ls a)) (+ 1 (car (ap-ls b)))))))

(define (sdiv s1 D)
  (ap (let div ((Q 0) (NN (ap '(0))) (N (ap-ls s1)))
	(let ((rem (sminus NN (smul D (ap (list Q))))))
	  (if (>= (scomp rem D) 0) (div (+ Q 1) NN N)
	    (if (null? N) (list Q)
	      (cons Q (div (Q-guess rem D)
			   (ap (append (ap-ls rem) (list (car N))))
			   (cdr N)))))))))

(define (savg s1 s2)
  (sdiv (splus s1 s2) (ap '(2))))

(define (fixed-point init act ok? avg)
  (let loop ((guess init))
    (dd guess)
    (let ((res (act guess)))
      (if (ok? guess res) res
          (loop (avg guess res))))))

(define (mkmagnitude n)
  (ap (cons 1 (let lp ((x n)) (if (= 0 x) '() (cons 0 (lp (- x 1))))))))

(define (cl->il ls)
  (map (lambda (c) (string->number (list->string c))) ls))

(define (rr n ls)
  (cl->il (tuples n (denul (sout ls)))))

(define (ap ls)
  (let ((ls (rr *radix-len* ls)))
    (map display (list "** " ls "\n"))
    (mk-ap '+ (length ls) *radix-len* ls)))

(define (mk-ap sign mag radix digits)
  (list 'ap sign mag radix digits))

(define (ap? ls)
  (or (and (pair? ls) (eq? 'ap (car ls)))
      (error "Not an arbitrary precision number" ls)))

(define (ap-sign ap) (and (ap? ap) (list-ref ap 1)))
(define (ap-mag ap) (and (ap? ap) (list-ref ap 2)))
(define (ap-radix ap) (and (ap? ap) (list-ref ap 3)))
(define (ap-ls ap) (and (ap? ap) (list-ref ap 4)))
(define (ap->str ap)
  (string-append (symbol->string (ap-sign ap))
		 ;"("
		 ;(number->string (ap-mag ap))
		 ;")"
		 (apply string-append
			(map (lambda (x) (string-append (number->string x) ""))
			     (ap-ls ap)))
		 ;" *" (number->string (ap-radix ap)) "*"
		 ))

(define (reradix fr to ls)
  (if (null? ls) '()
    (let loop ((n (car ls)) (zerofix 0) (rest (cdr ls)))
      (map display (list n " =" zerofix "= " rest "\n"))
      (if (<= to n) (loop (floor (/ n to)) zerofix (cons (modulo n to) rest))
	(if (null? rest) (list n)
	  (if (>= (* zerofix fr) to) (cons n (reradix fr to rest))
	    (if (= 0 n) (loop (car rest) (+ (* zerofix fr) fr) (cdr rest))
	      (let ((newn (+ (* n fr) (car rest))))
		(if (< (+ newn zerofix) to) (loop newn 0 (cdr rest))
		  (cons n (reradix fr to rest)))))))))))

(define (guess-radix n)
  (let it ((radix 10))
    (if (< n radix) radix
      (it (* 10 radix)))))

(define (tuples len numls)
  (let loop ((x len) (chunk '()) (ls numls))
    ;(map display (list x " " chunk " " ls "\n"))
    (if (null? ls) (list (reverse chunk))
      (if (= 0 x) (cons (reverse chunk) (loop len '() ls))
	(loop (- x 1) (cons (car ls) chunk) (cdr ls))))))

(define (sout ls)
  (if (null? ls) '()
    (append (string->list (number->string (car ls)))
	    (sout (cdr ls)))))

(define (ssqrt n)
  (let ((init (mkmagnitude (floor (/ (ap-mag n) 2)))))
    (let ((ans (fixed-point init
			    (lambda (guess)
			      (sdiv n guess))
			    (lambda (x y)
			      (<= (scomp (abs (sminus x y)) (ap '(1))) 0))
			    savg)))
	    ans)))

(define (go)
  (let rl ((n (read)) (x '()))
    (if (> n 0) (rl (- n 1) (cons (read) x))
      (let ((ls (ssqrt (reverse x))))
	;[ls let rev.x sdiv {[{[<[_.1 + 48 coerce 'char]> map x] reverse} coerce 'str coerce 'int] sqrt}]
	(display (length ls)) (newline)
	(let pl ((ls ls))
	  (and (not (null? ls))
	       (display (car ls))
	       (newline)
	       (pl (cdr ls))))))))

;(go)
