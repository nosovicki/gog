(load "../lib/bool-vector.arc")
(cards = nil)
(cardx = nil)
(def inpls () (map [coerce _ 'int] (tokens (readline))))
(def inpls2 () (map [_ coerce 'cons tuples 2] (tokens (readline))))

(def rotate-set (fr to)
  (zap (fn (x y) (cons y x)) cards (list fr to)))

(def tell (n)
     (let ans -1
       (each range cards
             (and (<= range.0 n)
                  (>= range.1 n)
                  (zap - ans)))
       (if (ans < 0) 0 1)))

(def rotate-setx (dep fr to)
    (cardx = (gmem cardx (fn (x) (if x nil '(1))) (int->path dep fr) (int->path dep to))))

(def tellx (dep i) (mod (len (recall join nil cardx (int->path dep i))) 2))

(withs (desc (inpls)
        depth ((desc.0 coerce 'string) len))
  (for i 1 desc.1
    (let input (inpls)
      (if (input.0 is 2)
        ;(prn (tellx depth input.1))
        (with (a (tell input.1) b (tellx depth input.1)) (or (a is b) (prn a " : " b)))
        (do (apply rotate-set input.cdr)
            (rotate-setx depth input.1 input.2))))))
