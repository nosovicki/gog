; avoid redefinition warning
($ (namespace-undefine-variable! (ac-global-name 'make-br-fn)))

(withs

  (prefx (afn (level char (o res "")) (if (level is 0) "" (+ char (self (- level 1) char))))
   arg (fn (lvl s)
	   (and (> len.s lvl)
		(begins s (prefx lvl "*"))
		(letter s.lvl)
		(is (upcase s.lvl) s.lvl)))

       args (afn (lvl e)
		 (case type.e
		   sym (if ssyntax.e (self lvl ssexpand.e)
			 (arg lvl (string e)) list.e)
		   cons (case car.e
			  quote nil
			  (mappend (ofn self lvl) e))))

       nest (afn (level body)
		 (if (no:isa body 'cons) body
		   (if (is (car body) 'make-br-fn) `(make-br-fn ,(cadr body) ,(+ 1 level))
		     (cons (self level (car body)) (self level (cdr body)))))))

  (mac make-br-fn (body (o level 0))
       `(fn ,(sort < (keys:counts:args level body))
	    ,(nest level body))))
