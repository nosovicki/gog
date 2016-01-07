(declare 'test t)
(def filter(f ls)
     (and ls
          (if (f >ls) (cons >ls (filter f <ls))
              (filter f <ls))))

(inner-defs = ("ac.scm" infile read [do <<<L] [filter [mem >*L '(define xdef odef)] L] @[if (isa L.1 'cons) L.1.0 L.1] @list_t listtab))

(def fix (ls) (and ls (if (isa ls 'cons) (cons >ls (fix <ls)) `(,ls))))
(example fix ((fix '(a b . c)) no:mismatch '(a b c)))
(example fix ((fix 'c) no:mismatch '(c)))
(example fix ((fix '(a b c)) no:mismatch '(a b c)))

(def process (ls (o env))
     (and ls
          (case (type ls)
            cons    (and (no:mem >ls '(quote quasiquote))
                         (case >ls
                           let          (cons >ls (if (ls.1 isa 'cons) (process <<ls (+ ls.1.@car env))
                                                    (process <<<ls (+ ls.2.@car env))))
                           let*         (cons >ls (process <<ls (+ ls.1.@car env)))
                           letrec       (cons >ls (process <<ls (+ ls.1.@car env)))
                           let-values   (cons >ls (process <<ls (+ ls.1.@car.$+ env)))
                           define       (cons >ls (if (ls.1 isa 'cons) (process <<ls (+ ls.1.fix env))
                                                    (process <<ls env)))
                           lambda       (cons >ls (process <<ls (+ ls.1.fix env)))
                           do           (cons >ls (process <<ls (+ ls.1.@car env)))
                           syntax-rules (cons >ls nil)
                           (+ (process >ls env) (process <ls env))))
            sym     (and (no:mem ls env) (no:inner-defs ls) `(,ls)))))

;(prn (process '(define (args FF) (if (not (pair? ls)) '() (if (fun (car ls)) (cons (FF ls) (let it ((AAA 'b)) (filter AAA (cdr ls)))) (filter fun (cdr '(X Y Z))))))))
;("ac.scm" file (ofn filter (ofn findsubseq "exn?")) prn)
("ac.scm" infile read process counts tablist @car prn)
;(prn inner-defs)
