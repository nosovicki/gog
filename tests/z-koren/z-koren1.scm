(define-macro (dd . vars)
  (let loop ((ls vars))
    (if (null? ls) '(begin (newline) (force-output))
      (cons 'begin
            (list `(map (lambda (x) (if (pair? x) (map display x) (display x))) (list ',(car ls) ": " ,(car ls) "   "))
                  (loop (cdr ls)))))))

(define maxnum (expt 2 32))

(define (ssame? s1 s2)
  (let ((d (sdiff s1 s2)))
    (or (null? d) (equal? '(1) d))))

(define (sdiff s1 s2)
  (let loop ((clean #t) (a (reverse (sout s1))) (b (reverse (sout s2))))
    (if (null? a) b
      (if (null? b) a
        (let ((r (- (car a) (car b))))
          (if (not (and clean (= 0 r))) (cons r (loop #f (cdr a) (cdr b)))
            (loop #t (cdr a) (cdr b))))))))

(define (smul s1 s2)
  (let loop ((a (reverse (sout s1))) (b (reverse (sout s2))))
    (if (null? b) '()
      (if (null? a) '()
        (if (pair? b) (splus (loop a (car b)) (append (loop a (cdr b)) '(0)))
          (let ((r (* (car a) b)))
            (splus (sout (list r)) (append (loop (cdr a) b) '(0)))))))))

(define (mkmagnitude n)
  (cons 1 (let lp ((x n)) (if (= 0 x) '() (cons 0 (lp (- x 1)))))))

(define (sdiv1 s1 s2)
  (dd "===")
  (let ((a (sout s1)) (b (sout s2)))
    (let loop ((incr (mkmagnitude (+ 1 (- (length a) (length b))))) (old '(0)))
      (let ((ans (splus old incr)))
        (let ((ck (scomp a (smul b ans))))
          (if (> ck 0) (loop incr ans)
            (if (= ck 0) ans
              (if (< (length incr) 2) old
                (loop (cons 1 (cddr incr)) old)))))))))

(define (head n ls)
  (if (or (null? ls) (= 0 n)) '()
    (cons (car ls)
          (head (- n 1)
                (cdr ls)))))

(define (fixlen n ls)
  (if (= 0 n) '()
    (if (null? ls) (cons 0 (fixlen (- n 1) ls))
      (cons (car ls) (fixlen (- n 1) (cdr ls))))))

(define (ls->int ls)
  (car (sin ls)))

(define (int->ls i)
  (sout (list (floor i))))

(define (zeros n)
  (if (= n 0) '()
    (cons 0 (zeros (- n 1)))))

(define (zero? ls)
  (null? (denul ls)))

(define (sdiv-i s1 s2)
  (let div ((N '()) (D (sout s2)) (Q '()) (rest (sout s1)))
    (if (and (null? rest) (null? N)) (denul (reverse Q))
	(let digit ((guess 0))
	  (let ((rem (sminus N (smul D (list guess)))))
	    (if (>= (scomp rem D) 0) (digit (+ guess 1))
		(if (null? rest) (div '() D (cons guess Q) '())
		    (div (append rem (list (car rest)))
			 D (cons guess Q) (cdr rest)))))))))


(define (sdiv s1 D)
  (denul
   (let div ((Q 0) (NN '()) (N (sout s1)))
     (let ((rem (sminus NN (smul D (list Q)))))
       (if (>= (scomp rem D) 0) (div (+ Q 1) NN N)
	   (if (null? N) (list Q)
	       (cons Q (div 0 (append rem (list (car N)))
			    (cdr N)))))))))

(define (avg x y)
  (/ (+ x y) 2))

(define (splus s1 s2)
  (denul
    (sout
      (reverse
        (let plus ((a (reverse (sout s1))) (b (reverse (sout s2))))
          (if (null? a) b
            (if (null? b) a
              (let ((r (+ (car a) (car b))))
                (cons (modulo r 10)
                      (plus (transfer (floor (/ r 10)) (cdr a))
                            (cdr b)))))))))))

(define (scomp s1 s2)
  (let ((a (sout s1)) (b (sout s2)))
    (let ((la (length a)) (lb (length b)))
      (if (> la lb) 1
        (if (< la lb) -1
          (let loop ((a a) (b b))
            (if (and (null? a) (null? b)) 0
         ;     (if (and (null? (cdr a)) (null? (cdr b))
         ;              (<= (abs (- (car a) (car b))) 1)) 0
              (if (or (null? a) (> (car a) (car b))) 1
                (if (or (null? b) (< (car a) (car b))) -1
                  (loop (cdr a) (cdr b)))))))))))

(define (transfer n ls)
  (cons (+ n (if (null? ls) 0 (car ls)))
        (if (null? ls) '() (cdr ls))))

(define (fixed-point init act ok? avg)
  (let loop ((guess init))
    (let ((res (act guess)))
      (if (ok? guess res) res
          (loop (avg guess res))))))

(define (ssqrt s1)
  (let ((n (sout s1)))
    (let ((init (mkmagnitude (floor (/ (length n) 2)))))
      (let ((ans (fixed-point init
                              (lambda (guess)
                                (sdiv n guess))
                              (lambda (x y)
                                (let ((diff (denul (reverse (sdiff x y)))))
                                  (or (null? diff) (equal? '(1) diff))))
                              savg)))
        (let ((res (scomp n (smul ans ans))))
          (if (< res 0) (sminus ans '(1))
            (if (> res 0) (splus ans '(1))
              ans)))))))

(define (savg s1 s2)
  (sdiv (splus s1 s2) '(2)))

(define (nsqrt n)
  (round
    (fixed-point 1 (lambda (x) (/ n x))
                 (lambda (x y) (< (abs (- x y)) .1))
                 (lambda (x y)
                   (exact->inexact (/ (+ x y) 2))))))

(define (sout ls)
  (if (null? ls) '()
    (append (map (lambda (c) (- (char->integer c) 48)) (string->list (number->string (car ls))))
            (sout (cdr ls)))))

(define (denul ls)
    (if (null? ls) ls
      (if (= 0 (car ls)) (denul (cdr ls))
        ls)))

(define (sin ls)
  (let loop ((ls ls) (tmp 0) (res '()))
    (if (null? ls) (reverse (cons tmp res))
      (if (> tmp (/ (- maxnum (car ls)) 10)) (loop (cdr ls) (car ls) (cons tmp res))
        (loop (cdr ls) (+ (* tmp 10) (car ls)) res)))))

(define (go)
  (let rl ((n (read)) (tmp 0) (x '()))
    (if (= 0 n) (let ((ls (ssqrt (reverse (cons tmp x)))))
                  (display (length ls)) (newline)
                  (let pl ((ls ls)) (and (not (null? ls)) (display (car ls)) (newline) (pl (cdr ls)))))
      (if (> tmp maxnum) (rl (- n 1) (read) (cons tmp x))
        (rl (- n 1) (+ (* tmp 10) (read)) x)))))

(define (sinverse ls) (map - ls))

(define (sminus s1 s2)
  (denul
      (reverse
        (let loop ((a (reverse (sout s1))) (b (reverse (sout s2))))
          (if (null? a) (if (null? b) '(0) (sinverse b))
            (if (null? b) a
              (let ((r (- (car a) (car b))))
                (if (>= r 0) (cons r (loop (cdr a) (cdr b)))
                  (cons (+ 10 r) (loop (cdr a) (transfer 1 (cdr b))))))))))))

(define (check name a b)
  (lambda (x y)
    (let* ((res (a x y)) (ck (b res y)))
      (if (= 0 (scomp (denul x) (denul ck))) res
        (begin
          (for-each (lambda (ls) (map display ls) (display " ")) (list x y res))
          (newline)
          (for-each (lambda (ls) (map display ls) (display " ")) (list res y ck))
          (newline)
          (error "Wrong result in" name))))))

(load "test.scm")
