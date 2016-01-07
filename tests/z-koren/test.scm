(load "z-koren2.scm")

(define-macro (tt my ref)
  `(begin
     (display ',my) (display " --> ") (display ',ref) (newline)
     (let ((a (apply ,(car my) (map (lambda (x) (ap (list x))) ',(cdr my))))
	   (b (ap (list ,ref))))
       (or (equal? a b) (error (with-output-to-string '() (lambda () (map display (list ',my " <> " ',ref " >> ")))) (list a b))))))

(define-macro (t)
  (let ((x (expt (random-integer 999959900) 2)))
    `(tt (ssqrt ,x) (sqrt ,x))))

(define (seq n)
  (if (< n 0) '()
    (cons n (seq (- n 1)))))

(define (test)
  ;; (let ((a (denul '(0 0 1))) (b '(1)))
  ;;   (or (equal? a b) (error "denul (0 0 1) <> (1)" (list a b))))
  ;; (let ((a (denul '(1 0 0))) (b '(1 0 0)))
  ;;   (or (equal? a b) (error "denul (1 0 0) <> (1 0 0)" (list a b))))
  ;; (let ((b '(1 5 3 2 4 5 4 2 1 2 3 5 3 2 1)))
  ;;   (or (equal? b (ap (sin b))) (error "b <> (ap (sin b))" (list b (ap (sin b))))))
  ;; (tt (ap (sin '(12345))) 12345)
  ;; (let ((a '(12 34 56)) (b '(123 456)))
  ;;   (or (ssame? a b) (error "a <> b" (list a b))))
  ;; (let ((a '(12 35 56)) (b '(123 456)))
  ;;   (and (ssame? a b) (error "a == b" (list a b))))
  ;; (let ((a (transfer 1 '(1 2 3))) (b '(2 2 3)))
  ;;  (or (equal? a b) (error "a <> b" (list a b))))
  (tt ((lambda (x) x) 9874321) 9874321)
  (tt (splus 934 934) (+ 934 934))
  (tt (splus 999 999) (+ 999 999))
  ;(tt (scomp 000 0) 0)
  (tt (sminus 11 7) 4)
  ;; (tt (sminus '(3 4 8) '(5 9 9)) (- 348 599))
  (tt (smul 64326435 246924823) (* 64326435 246924823))
  (tt (smul 25 5) (* 25 5))
  (tt (smul 25 25) (* 25 25))
  (tt (smul 1 2501) (* 1 2501))
  ;; (tt (sminus '(4532) '(90849)) (- 4532 90849))
  ;; (tt (sminus '(1 2 3) '(1 2 2)) 1)
  (let ((a (scomp (ap (list 123)) (ap (list 231)))) (b -1))
    (or (= a b) (error "scomp 123 321" (list a b))))
  (let ((a (scomp (ap (list 123)) (ap (list 123)))) (b 0))
    (or (= a b) (error "scomp 123 123" (list a b))))
  (let ((a (scomp (ap (list 321)) (ap (list 123)))) (b 1))
    (or (= a b) (error "scomp 123 123" (list a b))))
  (tt (smul 1 9874321) 9874321)
  (tt (smul 244140620000000000000 2) (* 244140620000000000000 2))
  (tt (sminus 1000 10) (- 1000 10))
  (tt (sminus 488281250000000000003 488281240000000000000) (- 488281250000000000003 488281240000000000000))
  (tt (sminus 1953125000000000000000 1953125000000000000000) (- 1953125000000000000000 1953125000000000000000))
  (tt (sdiv 10 10) (/ 10 10))
  (tt (sdiv 100 10) (/ 100 10))
  (tt (sdiv 100000 10) (/ 100000 10))
  (tt (sdiv 999998000001 999999) (/ 999998000001 999999))
  (tt (sdiv 4 2) (/ 4 2))
  (tt (sdiv 9874321 9874321) (/ 9874321 9874321))
  (tt (sdiv 9874321 1) (/ 9874321 1))
  (tt (sdiv 9874320 4937160) (/ 9874320 4937160))
  (tt (sdiv 29007610924013695395135762 12349438329034) (/ 29007610924013695395135762 12349438329034))
  (tt (savg 200 400) 300)
  (tt (splus 1000000 999999) (+ 1000000 999999))
  (tt (sdiv 1999999 2) (floor (/ 1999999 2)))
  (tt (savg 1000000 999999) (floor (/ (+ 1000000 999999) 2)))
  (tt (sdiv 488281250000000000003 2) (floor (/ 488281250000000000003 2)))
  (tt (ssqrt 625) (sqrt 625))
  (tt (ssqrt 999998000001) (sqrt 999998000001))
  (map (lambda x (t)) (seq 25))
  (tt (ssqrt 1621774547423000424629143320179592481750332023169) (sqrt 1621774547423000424629143320179592481750332023169))
  ;(tt (ssqrt 86187984242120611772119567040962985029802947069183614603923871824624024277516919736284684518845397155905322404) (sqrt 86187984242120611772119567040962985029802947069183614603923871824624024277516919736284684518845397155905322404))
  )

(test)
