(def main ()
  ((for n 1 (read) (pr (read))) tostring [do (_ len prn) (on n _ (prn n))]))

(def mainx ()
  (((read) times (read)) ssqrt de0 [do (_ len prn) (map prn _)]))
; {[(read) times (read)] ssqrt ?{_ len prn} [_ mp prn])}

(def ssqrt0 (ls)
  ((afn (guess)
    (let res (sdiv ls guess)
      (if (seqv guess res) res
	  (self (sdiv (splus guess res) 2)))))
   (guess-root ls)))

(def n>ls (n)
  (map [_ coerce 'int - 48] (n coerce 'string coerce 'cons)))

(maxint = 2 expt 32 -> n>ls)
(grp = 4)

(def guess-root (ls)
  (sqrt ((maxint.len - 1 firstn ls)
	 mp [_ + 48 coerce 'char] coerce 'string coerce 'int)))

(def ls>n (ls)
  (let res 0 (while ls (zap [_a * 10 + _b] res (pop ls))) res))

(def fix (ls n)
  (join (map [do 0] (1 range (n - ls.len mod n))) ls))

(def ssqrt (ls)
  (let x (ls fix grp split grp)
    (let res (x.0 ls>n sqrt trunc)
      (rt (n>ls res) (x.0 -> ls>n - res expt 2 -> n>ls) x.1))))

(def rt (res rem rest)
  (prs '**rt** res rem rest "\n")
  (if (no rest) res
      (with (x (rest split grp) pre (** res '(2)))
	(let work (de0 (join rem x.0))
	  (let next (find-mul pre work)
	    (rt (join res next!ans) next!rem x.1))))))

(def >> (n ls)
  (if (no ls) (list n)
      (ls.0 + n cons ls.cdr)))

(def revop (f)
  [(f (rev _a) (rev _b)) rev de0])

(= ++ (revop (rfn pls (a b)
	       (if (no a) b
		   (no b) a
		   t (let r (a.0 + b.0)
		       (r mod 10 cons (((r / 10) trunc) >> a.cdr pls b.cdr)))))))

(= -x (revop (rfn min (a b)
	       (if (no a) '(0)
		   (if (no b) a
		       (let res (a.0 - b.0)
			 (if (res >= 0) (cons res (a.cdr min b.cdr))
			     (res + 10 cons (a.cdr min (1 >> b.cdr))))))))))

(def de0 (ls)
  (reclist [and (no (_ caris 0)) _] ls))

(def ** (a b)
  (de0 ((rfn mul (a b)
	  (and a b (if (b isa 'int) (a.cdr mul b join '(0) ++ (n>ls (a.0 * b)))
		       t ((a mul b.0) ++ (a mul b.cdr join '(0))))))
	(rev a) (rev b))))

(def <> (a b)
  (if (a.len < b.len) -1
      (a.len > b.len) 1
      t ((rfn scomp (a b)
	   (if (no a) 0
	       (a.0 < b.0) -1
	       (a.0 > b.0) 1
	       (scomp a.cdr b.cdr))) a b)))

(def // (ls n)
  (let div (afn (x ls)
	     (if (no ls) nil
		 (x < n) (cons 0 (self (x * 10 + ls.0) ls.cdr))
		  t (((x / n) trunc) cons ((ls.0 mod n cons ls.cdr) // n))


(def find-mul (pre x)
  ;; (prs '**find-mul** pre x #\newline) 
  ((rfn fm (fr to)
     (if (fr <> to > 0) (obj rem x ans '(0))
	 (let guess (fr ++ to // 2)
	   ;; (prs '**find-mul** fr guess to "\n")
	   (prs '** pre x fr guess to "\n")
	   (let approx (pre join guess ** guess)
	     (if (approx <> x > 0) (fm fr (guess -x '(1)))
		 (let rem (de0 (x -x approx))
		   (if (rem <> (pre join guess) >= 0) (fm (guess ++ '(1)) to)
		       (obj rem rem ans (de0 guess)))))))))
   '(1) (grp / 2 times 9)))

(def test (n)
  ((n expt 2) n>ls ssqrt ls>n))

;; (test 10293849302293848392029384829181248394738293847281827347382827347273849281747472717283948473829182734747281928234778979878279873492837429834729834792384729384729384729384729384729384722349872938472987347429384723948237493287423487910293849302293848392029384829181248394738293847281827347382827347)

(test 1234)

;(mainx)
