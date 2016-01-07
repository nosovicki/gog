(require "ac.scm") 
(require "brackets.scm")
(use-bracket-readtable)

(parameterize ((current-directory (current-load-relative-directory)))
  (aload "arc.arc")
  (aload "libs.arc")
  )

(define (test ls)
  (or (equal? (ac (car ls) '())
	      (ac (cadr ls) '()))
      (not (printf "Compilation diff: ~s <> ~s:\n~s\n~s\n" (car ls) (cadr ls) (ac (car ls) '()) (ac (cadr ls) '())))
      (error "")))

(define (test2 ls)
  (on-err (lambda (e)
	    (printf "Run-time error: ~s\n" (exn-message e))
	    (test ls))
	  (lambda ()
  (or (equal? (eval (ac (car ls) '()))
	     (eval (ac (cadr ls) '())))
      (not (printf "*** ~s <> ~s:\n~s\n~s\n" (car ls) (cadr ls) (eval (ac (car ls) '())) (eval (ac (cadr ls) '()))))
      (error "")))))

(printf "~a\n" '(=== Tests start ===))

;(display (ar-coerce _table (quote fn)))
(map test
     '(
       ((8 / 2 / 2) (/ (/ 8 2) 2))
       ((1 expt 2 + 1 * 2 is 0) (is (+ (expt 1 2) (* 1 2)) 0))
       ((34 * 4 expt 2 + 3 * 2 + 2) (+ (+ (* 34 (expt 4 2)) (* 3 2)) 2))
       ((25 * 2 expt 3 - 48 * 2 expt 2 is 34 * 4 expt 2 + 3 * 2 + 2)
        (is (- (* 25 (expt 2 3)) (* 48 (expt 2 2))) (+ (+ (* 34 (expt 4 2)) (* 3 2)) 2)))
       ((15 - 2 expt 3 * 3 is 1) (is (- 15 (* (expt 2 3) 3)) 1))
       ((1 expt 2 + 1 * 2 is 0) (is (+ (expt  1 2) (* 1 2)) 0))
       ((a = 2 + 3 * 5) (= a (+ 2 (* 3 5))))
       (('(12 32) rev car -) (- (car (rev '(12 32)))))
       ((2 < 3 and 4 > 5) (and (< 2 3) (> 4 5)))
       (('(1 2) join '(3 4)) (join '(1 2) '(3 4)))
       ((1 no:is 2) (no:is 1 2))
       ((5 [+ X Y] 3) ([+ X Y] 5 3))
       ((x = ([+ _ 1] 3) + 2) (= x (+ ([+ _ 1] 3) 2)))
       ))

(map test2
     '(
       ((let map 3 (+ map 1)) (let map 3 (map + 1)))
       ((let a 1 (a + a)) (let a 1 (+ a a)))
       ((= a (table) (a '(1)) 'x b (a '(1))) (= a (table) (a '(1)) 'x b (a '(1))))
       ))
