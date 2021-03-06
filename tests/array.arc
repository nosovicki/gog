;; Create complement array B, then shift it in best position

(sz = (readline) coerce 'int)
(smallest = 10 expt 9)
(largest = -.smallest)
(cnt = 0)
(total = 0)

(def sum (ls) (reduce + (map abs ls)))
(def shift (ls n) (map [_ + n] ls))
(def say (n ls) (prn n) (on x ls (pr x " ")) (prn))

;; Create complement, making sure sum increases, and gather initial values
(def process (ls)
  (map [let b (((_ coerce 'int) -) + ++.cnt)
	 (zap + total b)
	 (zap min smallest b)
	 (zap max largest b)
	 b] ls))

;; Shift the array up and down so that it has the smallest sum
(def adj (ls tot up dwn)
 (with (newup (trunc:/ up 2)
        newdwn (trunc:/ dwn 2))
   (if (newup <= newdwn) (say tot ls)
       (with (a (shift ls -.newup)
              b (shift ls -.newdwn))
         (with (asum sum.a bsum sum.b)
           (if (asum < bsum) (adj a asum (up - newup) -.newup)
               (if (asum > bsum) (adj b bsum -.newdwn (dwn - newdwn))
                   (adj ls tot up.-- dwn.++))))))))

((readline) tokens process [adj _ total largest smallest])
