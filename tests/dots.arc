(def xy(f) (obj fn f compval nil val nil))
(def minmax(f) (obj fn f x (xy car) y (xy cadr)))
(def marginals() (obj min (minmax <=) max (minmax >=)))
(def dist (a b) (sqrt ((abs:- a.0 b.0) expt 2 + (abs:- a.1 b.1) expt 2)))

(def getdots (marginals)
 (repeat (read)
   (let ls (map [coerce _ 'int] (tokens (readline)))
     (each minmax (map cadr (tablist marginals))
       (each xy (minmax tablist [map cadr _] [keep [isa _ 'table] _])
         (and (or (no xy!compval) (minmax!fn (xy!fn ls) xy!compval))
              (xy!compval = (xy!fn ls))
              (xy!val = ls cons xy!val)))))))

(= colors (obj red (marginals) blu (marginals)))
(on marginals (map cadr (tablist colors)) (getdots marginals))

((map list (colors!red!min!y!val join colors!red!min!x!val)
           (colors!red!min!y!val join colors!red!min!x!val))
prn)
;((dist '(2 2) '(1 1)) prn)
(setinfixop 'string '==)
(foo = 2 expt 2 - 2 expt 3 / 2 string "foo is ")                ;; Iinfix notation
(foo prn [_ + 5] [_ - 1] [_ / 2] prn)  ;; Postfix notation
;(colors tablist prn)
