	(begin
	  (let ((zz "arc.arc"))
	    (namespace-set-variable-value! '_current-load-file* zz)
	    zz))


	(begin
	  (let ((zz (_table))) (namespace-set-variable-value! '_source-file* zz) zz))


	(begin (let ((zz (_table))) (namespace-set-variable-value! '_source* zz) zz))


	(begin (let ((zz (_table))) (namespace-set-variable-value! '_help* zz) zz))


	(begin
	  (let ((zz
		  (_annotate
		    'mac
		    (let ((| do| (lambda args `((fn () ,@(ar-nil-terminate args))))))
		      | do|))))
	    (namespace-set-variable-value! '_do zz)
	    zz))


	(_sref _sig 'args 'do)


	(_sref _source-file* _current-load-file* 'do)


	(begin
	  (let ((zz
		  (_annotate
		    'mac
		    (let ((| safeset|
			     (lambda (var val)
			       `(do (if
				      (bound ',var)
				      (do (disp "*** redefining " (stderr))
					(disp ',var (stderr))
					(disp #\newline (stderr))))
				  (assign ,var ,val)))))
		      | safeset|))))
	    (namespace-set-variable-value! '_safeset zz)
	    zz))


	(_sref _sig '(var val . nil) 'safeset)


	(_sref _source-file* _current-load-file* 'safeset)


	(begin
	  (let ((zz
		  (let ((| docify-body|
			   (lambda (body)
			     (if (not
				   (ar-false?
				     (if (not (ar-false? (_is (_type (_car body)) 'string)))
				       (_cdr body)
				       'nil)))
			       body
			       (_cons 'nil body)))))
		    | docify-body|)))
	    (namespace-set-variable-value! '_docify-body zz)
	    zz))


	(_sref _sig '(body . nil) 'docify-body)


	(_sref _source-file* _current-load-file* 'docify-body)


	(begin
	  (let ((zz
		  (_annotate
		    'mac
		    (let ((| def|
			     (lambda (name parms . body)
			       ((lambda g2441
				  (let* ((doc (ar-xcar (car g2441)))
					 (body (ar-xcdr (car g2441))))
				    `(do (sref sig ',parms ',name)
				       (sref help* ',doc ',name)
				       (sref source-file* current-load-file* ',name)
				       (sref
					 source*
					 '(def ,name ,parms ,@(ar-nil-terminate body))
					 ',name)
				       (safeset
					 ,name
					 (fn ,parms ,@(ar-nil-terminate body)))
				       (if (infixop ',name)
					 (setinfixop ',name (infixop ',name))))))
				(_docify-body body)))))
		      | def|))))
	    (namespace-set-variable-value! '_def zz)
	    zz))


	(_sref _sig '(name parms . body) 'def)


	(_sref _source-file* _current-load-file* 'def)


	((lambda ()
	   (_sref _sig '(xs . nil) 'caar)
	   (_sref _help* 'nil 'caar)
	   (_sref _source-file* _current-load-file* 'caar)
	   (_sref
	     _source*
	     '(def caar (xs . nil) (car (car xs . nil) . nil) . nil)
	     'caar)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'caar)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'caar (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| caar| (lambda (xs) (_car (_car xs))))) | caar|)))
		  (namespace-set-variable-value! '_caar zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'caar)))
	     (_setinfixop 'caar ((ar-coerce _infixop 'fn) 'caar))
	     'nil)))


	((lambda ()
	   (_sref _sig '(xs . nil) 'cadr)
	   (_sref _help* 'nil 'cadr)
	   (_sref _source-file* _current-load-file* 'cadr)
	   (_sref
	     _source*
	     '(def cadr (xs . nil) (car (cdr xs . nil) . nil) . nil)
	     'cadr)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'cadr)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'cadr (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| cadr| (lambda (xs) (_car (_cdr xs))))) | cadr|)))
		  (namespace-set-variable-value! '_cadr zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'cadr)))
	     (_setinfixop 'cadr ((ar-coerce _infixop 'fn) 'cadr))
	     'nil)))


	((lambda ()
	   (_sref _sig '(xs . nil) 'cddr)
	   (_sref _help* 'nil 'cddr)
	   (_sref _source-file* _current-load-file* 'cddr)
	   (_sref
	     _source*
	     '(def cddr (xs . nil) (cdr (cdr xs . nil) . nil) . nil)
	     'cddr)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'cddr)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'cddr (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| cddr| (lambda (xs) (_cdr (_cdr xs))))) | cddr|)))
		  (namespace-set-variable-value! '_cddr zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'cddr)))
	     (_setinfixop 'cddr ((ar-coerce _infixop 'fn) 'cddr))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'no)
	   (_sref _help* 'nil 'no)
	   (_sref _source-file* _current-load-file* 'no)
	   (_sref _source* '(def no (x . nil) (is x nil . nil) . nil) 'no)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'no)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'no (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| no| (lambda (x) (_is x 'nil)))) | no|)))
		  (namespace-set-variable-value! '_no zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'no)))
	     (_setinfixop 'no ((ar-coerce _infixop 'fn) 'no))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'acons)
	   (_sref _help* 'nil 'acons)
	   (_sref _source-file* _current-load-file* 'acons)
	   (_sref
	     _source*
	     '(def acons (x . nil) (is (type x . nil) (quote cons . nil) . nil) . nil)
	     'acons)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'acons)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'acons (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| acons| (lambda (x) (_is (_type x) 'cons)))) | acons|)))
		  (namespace-set-variable-value! '_acons zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'acons)))
	     (_setinfixop 'acons ((ar-coerce _infixop 'fn) 'acons))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'atom)
	   (_sref _help* 'nil 'atom)
	   (_sref _source-file* _current-load-file* 'atom)
	   (_sref
	     _source*
	     '(def atom (x . nil) (no (acons x . nil) . nil) . nil)
	     'atom)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'atom)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'atom (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| atom| (lambda (x) (_no (_acons x))))) | atom|)))
		  (namespace-set-variable-value! '_atom zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'atom)))
	     (_setinfixop 'atom ((ar-coerce _infixop 'fn) 'atom))
	     'nil)))


	((lambda ()
	   (_sref _sig '(xs . nil) 'copylist)
	   (_sref _help* 'nil 'copylist)
	   (_sref _source-file* _current-load-file* 'copylist)
	   (_sref
	     _source*
	     '(def
		copylist
		(xs . nil)
		(if (no xs . nil)
		  nil
		  (cons (car xs . nil) (copylist (cdr xs . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'copylist)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'copylist)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'copylist (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| copylist|
				 (lambda (xs)
				   (if (not (ar-false? (_no xs)))
				     'nil
				     (_cons (_car xs) (_copylist (_cdr xs)))))))
			  | copylist|)))
		  (namespace-set-variable-value! '_copylist zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'copylist)))
	     (_setinfixop 'copylist ((ar-coerce _infixop 'fn) 'copylist))
	     'nil)))


	((lambda ()
	   (_sref _sig 'args 'list)
	   (_sref _help* 'nil 'list)
	   (_sref _source-file* _current-load-file* 'list)
	   (_sref _source* '(def list args (copylist args . nil) . nil) 'list)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'list)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'list (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| list| (lambda args (_copylist args)))) | list|)))
		  (namespace-set-variable-value! '_list zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'list)))
	     (_setinfixop 'list ((ar-coerce _infixop 'fn) 'list))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'idfn)
	   (_sref _help* 'nil 'idfn)
	   (_sref _source-file* _current-load-file* 'idfn)
	   (_sref _source* '(def idfn (x . nil) x . nil) 'idfn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'idfn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'idfn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| idfn| (lambda (x) x))) | idfn|)))
		  (namespace-set-variable-value! '_idfn zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'idfn)))
	     (_setinfixop 'idfn ((ar-coerce _infixop 'fn) 'idfn))
	     'nil)))


	((lambda ()
	   (_sref _sig '(f xs . nil) 'map1)
	   (_sref _help* 'nil 'map1)
	   (_sref _source-file* _current-load-file* 'map1)
	   (_sref
	     _source*
	     '(def
		map1
		(f xs . nil)
		(if (no xs . nil)
		  nil
		  (cons (f (car xs . nil) . nil) (map1 f (cdr xs . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'map1)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'map1)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'map1 (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| map1|
				 (lambda (f xs)
				   (if (not (ar-false? (_no xs)))
				     'nil
				     (_cons
				       (ar-call-resolve-notation f (_car xs))
				       (_map1 f (_cdr xs)))))))
			  | map1|)))
		  (namespace-set-variable-value! '_map1 zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'map1)))
	     (_setinfixop 'map1 ((ar-coerce _infixop 'fn) 'map1))
	     'nil)))


	((lambda ()
	   (_sref _sig '(xs (o f list . nil) . nil) 'pair)
	   (_sref _help* 'nil 'pair)
	   (_sref _source-file* _current-load-file* 'pair)
	   (_sref
	     _source*
	     '(def
		pair
		(xs (o f list . nil) . nil)
		(if (no xs . nil)
		  nil
		  (no (cdr xs . nil) . nil)
		  (list (list (car xs . nil) . nil) . nil)
		  (cons
		    (f (car xs . nil) (cadr xs . nil) . nil)
		    (pair (cddr xs . nil) f . nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'pair)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'pair)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'pair (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(lambda g2442
			  (let* ((xs (car g2442))
				 (f
				   (if (pair? (ar-xcdr g2442))
				     (car (ar-xcdr g2442))
				     _list)))
			    (if (not (ar-false? (_no xs)))
			      'nil
			      (if (not (ar-false? (_no (_cdr xs))))
				(_list (_list (_car xs)))
				(_cons
				  (ar-call-resolve-notation f (_car xs) (_cadr xs))
				  (_pair (_cddr xs) f))))))))
		  (namespace-set-variable-value! '_pair zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'pair)))
	     (_setinfixop 'pair ((ar-coerce _infixop 'fn) 'pair))
	     'nil)))


	(begin
	  (let ((zz
		  (_annotate
		    'mac
		    (let ((| mac|
			     (lambda (name parms . body)
			       ((lambda g2443
				  (let* ((doc (ar-xcar (car g2443)))
					 (body (ar-xcdr (car g2443))))
				    `(do (sref sig ',parms ',name)
				       (sref help* ',doc ',name)
				       (sref source-file* current-load-file* ',name)
				       (sref
					 source*
					 '(mac ,name ,parms ,@(ar-nil-terminate body))
					 ',name)
				       (safeset
					 ,name
					 (annotate
					   'mac
					   (fn ,parms ,@(ar-nil-terminate body)))))))
				(_docify-body body)))))
		      | mac|))))
	    (namespace-set-variable-value! '_mac zz)
	    zz))


	(_sref _sig '(name parms . body) 'mac)


	(_sref _source-file* _current-load-file* 'mac)


	((lambda ()
	   (_sref _sig '(body . nil) 'make-br-fn)
	   (_sref _help* 'nil 'make-br-fn)
	   (_sref _source-file* _current-load-file* 'make-br-fn)
	   (_sref
	     _source*
	     '(mac
		make-br-fn
		(body . nil)
		(quasiquote (fn (_ . nil) (unquote body . nil) . nil) . nil)
		.
		nil)
	     'make-br-fn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'make-br-fn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'make-br-fn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| make-br-fn| (lambda (body) `(fn (_) ,body))))
			    | make-br-fn|))))
		  (namespace-set-variable-value! '_make-br-fn zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'and)
	   (_sref _help* 'nil 'and)
	   (_sref _source-file* _current-load-file* 'and)
	   (_sref
	     _source*
	     '(mac
		and
		args
		(if args
		  (if (cdr args . nil)
		    (quasiquote
		      (if (unquote (car args . nil) . nil)
			(and (unquote-splicing (cdr args . nil) . nil) . nil)
			.
			nil)
		      .
		      nil)
		    (car args . nil)
		    .
		    nil)
		  (quote t . nil)
		  .
		  nil)
		.
		nil)
	     'and)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'and)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'and (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| and|
				   (lambda args
				     (if (not (ar-false? args))
				       (if (not (ar-false? (_cdr args)))
					 `(if ,(_car args)
					    (and ,@(ar-nil-terminate (_cdr args))))
					 (_car args))
				       't))))
			    | and|))))
		  (namespace-set-variable-value! '_and zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(key al . nil) 'assoc)
	   (_sref _help* 'nil 'assoc)
	   (_sref _source-file* _current-load-file* 'assoc)
	   (_sref
	     _source*
	     '(def
		assoc
		(key al . nil)
		(if (atom al . nil)
		  nil
		  (and (acons (car al . nil) . nil) (is (caar al . nil) key . nil) . nil)
		  (car al . nil)
		  (assoc key (cdr al . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'assoc)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'assoc)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'assoc (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| assoc|
				 (lambda (key al)
				   (if (not (ar-false? (_atom al)))
				     'nil
				     (if (not
					   (ar-false?
					     (if (not (ar-false? (_acons (_car al))))
					       (_is (_caar al) key)
					       'nil)))
				       (_car al)
				       (_assoc key (_cdr al)))))))
			  | assoc|)))
		  (namespace-set-variable-value! '_assoc zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'assoc)))
	     (_setinfixop 'assoc ((ar-coerce _infixop 'fn) 'assoc))
	     'nil)))


	((lambda ()
	   (_sref _sig '(al key . nil) 'alref)
	   (_sref _help* 'nil 'alref)
	   (_sref _source-file* _current-load-file* 'alref)
	   (_sref
	     _source*
	     '(def alref (al key . nil) (cadr (assoc key al . nil) . nil) . nil)
	     'alref)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'alref)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'alref (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| alref| (lambda (al key) (_cadr (_assoc key al)))))
			  | alref|)))
		  (namespace-set-variable-value! '_alref zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'alref)))
	     (_setinfixop 'alref ((ar-coerce _infixop 'fn) 'alref))
	     'nil)))


	((lambda ()
	   (_sref _sig '(parms . body) 'with)
	   (_sref _help* 'nil 'with)
	   (_sref _source-file* _current-load-file* 'with)
	   (_sref
	     _source*
	     '(mac
		with
		(parms . body)
		(quasiquote
		  ((fn
		     (unquote (map1 car (pair parms . nil) . nil) . nil)
		     (unquote-splicing body . nil)
		     .
		     nil)
		   (unquote-splicing (map1 cadr (pair parms . nil) . nil) . nil)
		   .
		   nil)
		  .
		  nil)
		.
		nil)
	     'with)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'with)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'with (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| with|
				   (lambda (parms . body)
				     `((fn
					 ,(_map1 _car (_pair parms))
					 ,@(ar-nil-terminate body))
				       ,@(ar-nil-terminate (_map1 _cadr (_pair parms)))))))
			    | with|))))
		  (namespace-set-variable-value! '_with zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(var val . body) 'let)
	   (_sref _help* 'nil 'let)
	   (_sref _source-file* _current-load-file* 'let)
	   (_sref
	     _source*
	     '(mac
		let
		(var val . body)
		(quasiquote
		  (with
		    ((unquote var . nil) (unquote val . nil) . nil)
		    (unquote-splicing body . nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'let)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'let)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'let (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| let|
				   (lambda (var val . body)
				     `(with (,var ,val) ,@(ar-nil-terminate body)))))
			    | let|))))
		  (namespace-set-variable-value! '_let zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(parms . body) 'withs)
	   (_sref _help* 'nil 'withs)
	   (_sref _source-file* _current-load-file* 'withs)
	   (_sref
	     _source*
	     '(mac
		withs
		(parms . body)
		(if (no parms . nil)
		  (quasiquote (do (unquote-splicing body . nil) . nil) . nil)
		  (quasiquote
		    (let (unquote (car parms . nil) . nil)
		      (unquote (cadr parms . nil) . nil)
		      (withs
			(unquote (cddr parms . nil) . nil)
			(unquote-splicing body . nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'withs)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'withs)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'withs (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| withs|
				   (lambda (parms . body)
				     (if (not (ar-false? (_no parms)))
				       `(do ,@(ar-nil-terminate body))
				       `(let ,(_car parms)
					  ,(_cadr parms)
					  (withs
					    ,(_cddr parms)
					    ,@(ar-nil-terminate body)))))))
			    | withs|))))
		  (namespace-set-variable-value! '_withs zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'join)
	   (_sref _help* 'nil 'join)
	   (_sref _source-file* _current-load-file* 'join)
	   (_sref
	     _source*
	     '(def
		join
		args
		(if (no args . nil)
		  nil
		  (quasiquote
		    ((unquote-splicing (car args . nil) . nil)
		     (unquote-splicing (apply join (cdr args . nil) . nil) . nil)
		     .
		     nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'join)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'join)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'join (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| join|
				 (lambda args
				   (if (not (ar-false? (_no args)))
				     'nil
				     `(,@(ar-nil-terminate (_car args))
					,@(ar-nil-terminate
					    (_apply _join (_cdr args))))))))
			  | join|)))
		  (namespace-set-variable-value! '_join zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'join)))
	     (_setinfixop 'join ((ar-coerce _infixop 'fn) 'join))
	     'nil)))


	((lambda ()
	   (_sref _sig '(name parms . body) 'rfn)
	   (_sref _help* 'nil 'rfn)
	   (_sref _source-file* _current-load-file* 'rfn)
	   (_sref
	     _source*
	     '(mac
		rfn
		(name parms . body)
		(quasiquote
		  (let (unquote name . nil)
		    nil
		    (assign
		      (unquote name . nil)
		      (fn (unquote parms . nil) (unquote-splicing body . nil) . nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'rfn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'rfn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'rfn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| rfn|
				   (lambda (name parms . body)
				     `(let ,name
					nil
					(assign
					  ,name
					  (fn ,parms ,@(ar-nil-terminate body)))))))
			    | rfn|))))
		  (namespace-set-variable-value! '_rfn zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(parms . body) 'afn)
	   (_sref _help* 'nil 'afn)
	   (_sref _source-file* _current-load-file* 'afn)
	   (_sref
	     _source*
	     '(mac
		afn
		(parms . body)
		(quasiquote
		  (let self nil
		    (assign
		      self
		      (fn (unquote parms . nil) (unquote-splicing body . nil) . nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'afn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'afn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'afn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| afn|
				   (lambda (parms . body)
				     `(let self nil
					(assign
					  self
					  (fn ,parms ,@(ar-nil-terminate body)))))))
			    | afn|))))
		  (namespace-set-variable-value! '_afn zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'compose)
	   (_sref _help* 'nil 'compose)
	   (_sref _source-file* _current-load-file* 'compose)
	   (_sref
	     _source*
	     '(mac
		compose
		args
		(let g (uniq . nil)
		  (quasiquote
		    (fn
		      (unquote g . nil)
		      (unquote
			((afn
			   (fs . nil)
			   (if (cdr fs . nil)
			     (list (car fs . nil) (self (cdr fs . nil) . nil) . nil)
			     (quasiquote
			       (apply
				 (unquote
				   (if (car fs . nil) (car fs . nil) (quote idfn . nil) . nil)
				   .
				   nil)
				 (unquote g . nil)
				 .
				 nil)
			       .
			       nil)
			     .
			     nil)
			   .
			   nil)
			 args
			 .
			 nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'compose)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'compose)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'compose (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| compose|
				   (lambda args
				     ((let ((| compose|
					       (lambda (g)
						 `(fn
						    ,g
						    ,(ar-call-resolve-notation
						       ((let ((| compose|
								 (lambda (self)
								   (begin
								     (let ((zz
									     (let ((| self|
										      (lambda (fs)
											(if (not
											      (ar-false?
												(_cdr
												  fs)))
											  (_list
											    (_car fs)
											    (ar-call-resolve-notation
											      self
											      (_cdr
												fs)))
											  `(apply
											     ,(if (not
												    (ar-false?
												      (_car
													fs)))
												(_car
												  fs)
												'idfn)
											     ,g)))))
									       | self|)))
								       (set! self zz)
								       zz)))))
							  | compose|)
							'nil)
						       args)))))
					| compose|)
				      (_uniq)))))
			    | compose|))))
		  (namespace-set-variable-value! '_compose zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(f . nil) 'complement)
	   (_sref _help* 'nil 'complement)
	   (_sref _source-file* _current-load-file* 'complement)
	   (_sref
	     _source*
	     '(mac
		complement
		(f . nil)
		(let g (uniq . nil)
		  (quasiquote
		    (fn
		      (unquote g . nil)
		      (no (apply (unquote f . nil) (unquote g . nil) . nil) . nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'complement)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'complement)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'complement (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| complement|
				   (lambda (f)
				     ((let ((| complement|
					       (lambda (g) `(fn ,g (no (apply ,f ,g))))))
					| complement|)
				      (_uniq)))))
			    | complement|))))
		  (namespace-set-variable-value! '_complement zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(xs . nil) 'rev)
	   (_sref _help* 'nil 'rev)
	   (_sref _source-file* _current-load-file* 'rev)
	   (_sref
	     _source*
	     '(def
		rev
		(xs . nil)
		((afn
		   (xs acc . nil)
		   (if (no xs . nil)
		     acc
		     (self (cdr xs . nil) (cons (car xs . nil) acc . nil) . nil)
		     .
		     nil)
		   .
		   nil)
		 xs
		 nil
		 .
		 nil)
		.
		nil)
	     'rev)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'rev)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'rev (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| rev|
				 (lambda (xs)
				   (ar-call-resolve-notation
				     ((let ((| rev|
					       (lambda (self)
						 (begin
						   (let ((zz
							   (let ((| self|
								    (lambda (xs acc)
								      (if (not
									    (ar-false? (_no xs)))
									acc
									(ar-call-resolve-notation
									  self
									  (_cdr xs)
									  (_cons
									    (_car xs)
									    acc))))))
							     | self|)))
						     (set! self zz)
						     zz)))))
					| rev|)
				      'nil)
				     xs
				     'nil))))
			  | rev|)))
		  (namespace-set-variable-value! '_rev zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rev)))
	     (_setinfixop 'rev ((ar-coerce _infixop 'fn) 'rev))
	     'nil)))


	((lambda ()
	   (_sref _sig 'args 'isnt)
	   (_sref _help* 'nil 'isnt)
	   (_sref _source-file* _current-load-file* 'isnt)
	   (_sref
	     _source*
	     '(def isnt args (no (apply is args . nil) . nil) . nil)
	     'isnt)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'isnt)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'isnt (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| isnt| (lambda args (_no (_apply _is args)))))
			  | isnt|)))
		  (namespace-set-variable-value! '_isnt zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'isnt)))
	     (_setinfixop 'isnt ((ar-coerce _infixop 'fn) 'isnt))
	     'nil)))


	((lambda ()
	   (_sref _sig '(names . body) 'w/uniq)
	   (_sref _help* 'nil 'w/uniq)
	   (_sref _source-file* _current-load-file* 'w/uniq)
	   (_sref
	     _source*
	     '(mac
		w/uniq
		(names . body)
		(if (acons names . nil)
		  (quasiquote
		    (with
		      (unquote
			(apply
			  +
			  nil
			  (map1
			    (fn (n . nil) (list n (quote (uniq . nil) . nil) . nil) . nil)
			    names
			    .
			    nil)
			  .
			  nil)
			.
			nil)
		      (unquote-splicing body . nil)
		      .
		      nil)
		    .
		    nil)
		  (quasiquote
		    (let (unquote names . nil)
		      (uniq . nil)
		      (unquote-splicing body . nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'w/uniq)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'w/uniq)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'w/uniq (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| w/uniq|
				   (lambda (names . body)
				     (if (not (ar-false? (_acons names)))
				       `(with
					  ,(_apply
					     _+
					     'nil
					     (_map1
					       (let ((| w/uniq|
							(lambda (n) (_list n '(uniq . nil)))))
						 | w/uniq|)
					       names))
					  ,@(ar-nil-terminate body))
				       `(let ,names (uniq) ,@(ar-nil-terminate body))))))
			    | w/uniq|))))
		  (namespace-set-variable-value! '_w/uniq zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'or)
	   (_sref _help* 'nil 'or)
	   (_sref _source-file* _current-load-file* 'or)
	   (_sref
	     _source*
	     '(mac
		or
		args
		(and args
		     (w/uniq
		       g
		       (quasiquote
			 (let (unquote g . nil)
			   (unquote (car args . nil) . nil)
			   (if (unquote g . nil)
			     (unquote g . nil)
			     (or (unquote-splicing (cdr args . nil) . nil) . nil)
			     .
			     nil)
			   .
			   nil)
			 .
			 nil)
		       .
		       nil)
		     .
		     nil)
		.
		nil)
	     'or)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'or)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'or (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| or|
				   (lambda args
				     (if (not (ar-false? args))
				       ((let ((| or|
						 (lambda (g)
						   `(let ,g
						      ,(_car args)
						      (if ,g
							,g
							(or ,@(ar-nil-terminate
								(_cdr args))))))))
					  | or|)
					(_uniq))
				       'nil))))
			    | or|))))
		  (namespace-set-variable-value! '_or zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(x . nil) 'alist)
	   (_sref _help* 'nil 'alist)
	   (_sref _source-file* _current-load-file* 'alist)
	   (_sref
	     _source*
	     '(def
		alist
		(x . nil)
		(or (no x . nil) (is (type x . nil) (quote cons . nil) . nil) . nil)
		.
		nil)
	     'alist)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'alist)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'alist (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| alist|
				 (lambda (x)
				   ((let ((| alist|
					     (lambda (g2444)
					       (if (not (ar-false? g2444))
						 g2444
						 ((let ((| alist|
							   (lambda (g2445)
							     (if (not (ar-false? g2445))
							       g2445
							       'nil))))
						    | alist|)
						  (_is (_type x) 'cons))))))
				      | alist|)
				    (_no x)))))
			  | alist|)))
		  (namespace-set-variable-value! '_alist zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'alist)))
	     (_setinfixop 'alist ((ar-coerce _infixop 'fn) 'alist))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . choices) 'in)
	   (_sref _help* 'nil 'in)
	   (_sref _source-file* _current-load-file* 'in)
	   (_sref
	     _source*
	     '(mac
		in
		(x . choices)
		(w/uniq
		  g
		  (quasiquote
		    (let (unquote g . nil)
		      (unquote x . nil)
		      (or (unquote-splicing
			    (map1
			      (fn
				(c . nil)
				(quasiquote
				  (is (unquote g . nil) (unquote c . nil) . nil)
				  .
				  nil)
				.
				nil)
			      choices
			      .
			      nil)
			    .
			    nil)
			  .
			  nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'in)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'in)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'in (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| in|
				   (lambda (x . choices)
				     ((let ((| in|
					       (lambda (g)
						 `(let ,g
						    ,x
						    (or ,@(ar-nil-terminate
							    (_map1
							      (let ((| in|
								       (lambda (c) `(is ,g ,c))))
								| in|)
							      choices)))))))
					| in|)
				      (_uniq)))))
			    | in|))))
		  (namespace-set-variable-value! '_in zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(x y . nil) 'iso)
	   (_sref _help* 'nil 'iso)
	   (_sref _source-file* _current-load-file* 'iso)
	   (_sref
	     _source*
	     '(def
		iso
		(x y . nil)
		(or (is x y . nil)
		    (and (acons x . nil)
			 (acons y . nil)
			 (iso (car x . nil) (car y . nil) . nil)
			 (iso (cdr x . nil) (cdr y . nil) . nil)
			 .
			 nil)
		    .
		    nil)
		.
		nil)
	     'iso)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'iso)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'iso (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| iso|
				 (lambda (x y)
				   ((let ((| iso|
					     (lambda (g2446)
					       (if (not (ar-false? g2446))
						 g2446
						 ((let ((| iso|
							   (lambda (g2447)
							     (if (not (ar-false? g2447))
							       g2447
							       'nil))))
						    | iso|)
						  (if (not (ar-false? (_acons x)))
						    (if (not (ar-false? (_acons y)))
						      (if (not
							    (ar-false?
							      (_iso (_car x) (_car y))))
							(_iso (_cdr x) (_cdr y))
							'nil)
						      'nil)
						    'nil))))))
				      | iso|)
				    (_is x y)))))
			  | iso|)))
		  (namespace-set-variable-value! '_iso zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'iso)))
	     (_setinfixop 'iso ((ar-coerce _infixop 'fn) 'iso))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test . body) 'when)
	   (_sref _help* 'nil 'when)
	   (_sref _source-file* _current-load-file* 'when)
	   (_sref
	     _source*
	     '(mac
		when
		(test . body)
		(quasiquote
		  (if (unquote test . nil) (do (unquote-splicing body . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'when)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'when)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'when (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| when|
				   (lambda (test . body)
				     `(if ,test (do ,@(ar-nil-terminate body))))))
			    | when|))))
		  (namespace-set-variable-value! '_when zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(test . body) 'unless)
	   (_sref _help* 'nil 'unless)
	   (_sref _source-file* _current-load-file* 'unless)
	   (_sref
	     _source*
	     '(mac
		unless
		(test . body)
		(quasiquote
		  (if (no (unquote test . nil) . nil)
		    (do (unquote-splicing body . nil) . nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'unless)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'unless)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'unless (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| unless|
				   (lambda (test . body)
				     `(if (no ,test) (do ,@(ar-nil-terminate body))))))
			    | unless|))))
		  (namespace-set-variable-value! '_unless zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(test . body) 'while)
	   (_sref _help* 'nil 'while)
	   (_sref _source-file* _current-load-file* 'while)
	   (_sref
	     _source*
	     '(mac
		while
		(test . body)
		(w/uniq
		  (gf gp . nil)
		  (quasiquote
		    ((rfn
		       (unquote gf . nil)
		       ((unquote gp . nil) . nil)
		       (when (unquote gp . nil)
			 (unquote-splicing body . nil)
			 ((unquote gf . nil) (unquote test . nil) . nil)
			 .
			 nil)
		       .
		       nil)
		     (unquote test . nil)
		     .
		     nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'while)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'while)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'while (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| while|
				   (lambda (test . body)
				     ((let ((| while|
					       (lambda (gf gp)
						 `((rfn
						     ,gf
						     (,gp)
						     (when ,gp
						       ,@(ar-nil-terminate body)
						       (,gf ,test)))
						   ,test))))
					| while|)
				      (_uniq)
				      (_uniq)))))
			    | while|))))
		  (namespace-set-variable-value! '_while zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(seq . nil) 'empty)
	   (_sref _help* 'nil 'empty)
	   (_sref _source-file* _current-load-file* 'empty)
	   (_sref
	     _source*
	     '(def
		empty
		(seq . nil)
		(or (no seq . nil)
		    (and (or (is (type seq . nil) (quote string . nil) . nil)
			     (is (type seq . nil) (quote table . nil) . nil)
			     .
			     nil)
			 (is (len seq . nil) 0 . nil)
			 .
			 nil)
		    .
		    nil)
		.
		nil)
	     'empty)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'empty)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'empty (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| empty|
				 (lambda (seq)
				   ((let ((| empty|
					     (lambda (g2448)
					       (if (not (ar-false? g2448))
						 g2448
						 ((let ((| empty|
							   (lambda (g2449)
							     (if (not (ar-false? g2449))
							       g2449
							       'nil))))
						    | empty|)
						  (if (not
							(ar-false?
							  ((let ((| g2449|
								    (lambda (g2450)
								      (if (not
									    (ar-false? g2450))
									g2450
									((let ((| g2449|
										  (lambda (g2451)
										    (if (not
											  (ar-false?
											    g2451))
										      g2451
										      'nil))))
									   | g2449|)
									 (_is
									   (_type seq)
									   'table))))))
							     | g2449|)
							   (_is (_type seq) 'string))))
						    (_is (_len seq) 0)
						    'nil))))))
				      | empty|)
				    (_no seq)))))
			  | empty|)))
		  (namespace-set-variable-value! '_empty zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'empty)))
	     (_setinfixop 'empty ((ar-coerce _infixop 'fn) 'empty))
	     'nil)))


	((lambda ()
	   (_sref _sig '(f xs . nil) 'reclist)
	   (_sref _help* 'nil 'reclist)
	   (_sref _source-file* _current-load-file* 'reclist)
	   (_sref
	     _source*
	     '(def
		reclist
		(f xs . nil)
		(and xs (or (f xs . nil) (reclist f (cdr xs . nil) . nil) . nil) . nil)
		.
		nil)
	     'reclist)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'reclist)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'reclist (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| reclist|
				 (lambda (f xs)
				   (if (not (ar-false? xs))
				     ((let ((| reclist|
					       (lambda (g2452)
						 (if (not (ar-false? g2452))
						   g2452
						   ((let ((| reclist|
							     (lambda (g2453)
							       (if (not (ar-false? g2453))
								 g2453
								 'nil))))
						      | reclist|)
						    (_reclist f (_cdr xs)))))))
					| reclist|)
				      (ar-call-resolve-notation f xs))
				     'nil))))
			  | reclist|)))
		  (namespace-set-variable-value! '_reclist zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'reclist)))
	     (_setinfixop 'reclist ((ar-coerce _infixop 'fn) 'reclist))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test s (o start 0 . nil) . nil) 'recstring)
	   (_sref _help* 'nil 'recstring)
	   (_sref _source-file* _current-load-file* 'recstring)
	   (_sref
	     _source*
	     '(def
		recstring
		(test s (o start 0 . nil) . nil)
		((afn
		   (i . nil)
		   (and (< i (len s . nil) . nil)
			(or (test i . nil) (self (+ i 1 . nil) . nil) . nil)
			.
			nil)
		   .
		   nil)
		 start
		 .
		 nil)
		.
		nil)
	     'recstring)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'recstring)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'recstring (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(lambda g2454
			  (let* ((test (car g2454))
				 (s (car (ar-xcdr g2454)))
				 (start
				   (if (pair? (ar-xcdr (ar-xcdr g2454)))
				     (car (ar-xcdr (ar-xcdr g2454)))
				     0)))
			    (ar-call-resolve-notation
			      ((let ((| recstring|
					(lambda (self)
					  (begin
					    (let ((zz
						    (let ((| self|
							     (lambda (i)
							       (if (not
								     (ar-false?
								       (_< i (_len s))))
								 ((let ((| self|
									   (lambda (g2455)
									     (if (not
										   (ar-false?
										     g2455))
									       g2455
									       ((let ((| self|
											 (lambda (g2456)
											   (if (not
												 (ar-false?
												   g2456))
											     g2456
											     'nil))))
										  | self|)
										(ar-call-resolve-notation
										  self
										  (_+ i 1)))))))
								    | self|)
								  (ar-call-resolve-notation
								    test
								    i))
								 'nil))))
						      | self|)))
					      (set! self zz)
					      zz)))))
				 | recstring|)
			       'nil)
			      start)))))
		  (namespace-set-variable-value! '_recstring zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'recstring)))
	     (_setinfixop 'recstring ((ar-coerce _infixop 'fn) 'recstring))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'testify)
	   (_sref _help* 'nil 'testify)
	   (_sref _source-file* _current-load-file* 'testify)
	   (_sref
	     _source*
	     '(def
		testify
		(x . nil)
		(if (isa x (quote fn . nil) . nil)
		  x
		  (make-br-fn (is _ x . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'testify)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'testify)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'testify (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| testify|
				 (lambda (x)
				   (if (not (ar-false? (_isa x 'fn)))
				     x
				     (let ((| testify| (lambda () (_is __ x))))
				       | testify|)))))
			  | testify|)))
		  (namespace-set-variable-value! '_testify zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'testify)))
	     (_setinfixop 'testify ((ar-coerce _infixop 'fn) 'testify))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test seq . nil) 'some)
	   (_sref _help* 'nil 'some)
	   (_sref _source-file* _current-load-file* 'some)
	   (_sref
	     _source*
	     '(def
		some
		(test seq . nil)
		(let f (testify test . nil)
		  (if (alist seq . nil)
		    (reclist f:car seq . nil)
		    (recstring f:seq seq . nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'some)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'some)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'some (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| some|
				 (lambda (test seq)
				   ((let ((| some|
					     (lambda (f)
					       (if (not (ar-false? (_alist seq)))
						 (_reclist
						   (let ((| some|
							    (lambda g2457
							      (ar-call-resolve-notation
								f
								(_apply _car g2457)))))
						     | some|)
						   seq)
						 (_recstring
						   (let ((| some|
							    (lambda g2458
							      (ar-call-resolve-notation
								f
								(_apply seq g2458)))))
						     | some|)
						   seq)))))
				      | some|)
				    (_testify test)))))
			  | some|)))
		  (namespace-set-variable-value! '_some zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'some)))
	     (_setinfixop 'some ((ar-coerce _infixop 'fn) 'some))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test seq . nil) 'all)
	   (_sref _help* 'nil 'all)
	   (_sref _source-file* _current-load-file* 'all)
	   (_sref
	     _source*
	     '(def
		all
		(test seq . nil)
		(~some (complement (testify test . nil) . nil) seq . nil)
		.
		nil)
	     'all)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'all)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'all (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| all|
				 (lambda (test seq)
				   (_no
				     (_some
				       (let ((| all|
						(lambda g2459
						  (_no (_apply (_testify test) g2459)))))
					 | all|)
				       seq)))))
			  | all|)))
		  (namespace-set-variable-value! '_all zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'all)))
	     (_setinfixop 'all ((ar-coerce _infixop 'fn) 'all))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test seq . nil) 'mem)
	   (_sref _help* 'nil 'mem)
	   (_sref _source-file* _current-load-file* 'mem)
	   (_sref
	     _source*
	     '(def
		mem
		(test seq . nil)
		(let f (testify test . nil)
		  (reclist (make-br-fn (if (f:car _ . nil) _ . nil) . nil) seq . nil)
		  .
		  nil)
		.
		nil)
	     'mem)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'mem)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'mem (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| mem|
				 (lambda (test seq)
				   ((let ((| mem|
					     (lambda (f)
					       (_reclist
						 (let ((| mem|
							  (lambda ()
							    (if (not
								  (ar-false?
								    (ar-call-resolve-notation
								      f
								      (_car __))))
							      __
							      'nil))))
						   | mem|)
						 seq))))
				      | mem|)
				    (_testify test)))))
			  | mem|)))
		  (namespace-set-variable-value! '_mem zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'mem)))
	     (_setinfixop 'mem ((ar-coerce _infixop 'fn) 'mem))
	     'nil)))


	((lambda ()
	   (_sref _sig '(test seq . nil) 'find)
	   (_sref _help* 'nil 'find)
	   (_sref _source-file* _current-load-file* 'find)
	   (_sref
	     _source*
	     '(def
		find
		(test seq . nil)
		(let f (testify test . nil)
		  (if (alist seq . nil)
		    (reclist
		      (make-br-fn (if (f:car _ . nil) (car _ . nil) . nil) . nil)
		      seq
		      .
		      nil)
		    (recstring
		      (make-br-fn (if (f:seq _ . nil) (seq _ . nil) . nil) . nil)
		      seq
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'find)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'find)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'find (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| find|
				 (lambda (test seq)
				   ((let ((| find|
					     (lambda (f)
					       (if (not (ar-false? (_alist seq)))
						 (_reclist
						   (let ((| find|
							    (lambda ()
							      (if (not
								    (ar-false?
								      (ar-call-resolve-notation
									f
									(_car __))))
								(_car __)
								'nil))))
						     | find|)
						   seq)
						 (_recstring
						   (let ((| find|
							    (lambda ()
							      (if (not
								    (ar-false?
								      (ar-call-resolve-notation
									f
									(ar-call-resolve-notation
									  seq
									  __))))
								(ar-call-resolve-notation
								  seq
								  __)
								'nil))))
						     | find|)
						   seq)))))
				      | find|)
				    (_testify test)))))
			  | find|)))
		  (namespace-set-variable-value! '_find zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'find)))
	     (_setinfixop 'find ((ar-coerce _infixop 'fn) 'find))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x y . nil) 'isa)
	   (_sref _help* 'nil 'isa)
	   (_sref _source-file* _current-load-file* 'isa)
	   (_sref
	     _source*
	     '(def isa (x y . nil) (is (type x . nil) y . nil) . nil)
	     'isa)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'isa)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'isa (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz (let ((| isa| (lambda (x y) (_is (_type x) y)))) | isa|)))
		  (namespace-set-variable-value! '_isa zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'isa)))
	     (_setinfixop 'isa ((ar-coerce _infixop 'fn) 'isa))
	     'nil)))


	((lambda ()
	   (_sref _sig '(f . seqs) 'map)
	   (_sref _help* 'nil 'map)
	   (_sref _source-file* _current-load-file* 'map)
	   (_sref
	     _source*
	     '(def
		map
		(f . seqs)
		(if (some
		      (make-br-fn (isa _ (quote string . nil) . nil) . nil)
		      seqs
		      .
		      nil)
		  (withs
		    (n
		      (apply min (map len seqs . nil) . nil)
		      new
		      (newstring n . nil)
		      .
		      nil)
		    ((afn
		       (i . nil)
		       (if (is i n . nil)
			 new
			 (do (sref
			       new
			       (apply
				 f
				 (map (make-br-fn (_ i . nil) . nil) seqs . nil)
				 .
				 nil)
			       i
			       .
			       nil)
			   (self (+ i 1 . nil) . nil)
			   .
			   nil)
			 .
			 nil)
		       .
		       nil)
		     0
		     .
		     nil)
		    .
		    nil)
		  (no (cdr seqs . nil) . nil)
		  (map1 f (car seqs . nil) . nil)
		  ((afn
		     (seqs . nil)
		     (if (some no seqs . nil)
		       nil
		       (cons
			 (apply f (map1 car seqs . nil) . nil)
			 (self (map1 cdr seqs . nil) . nil)
			 .
			 nil)
		       .
		       nil)
		     .
		     nil)
		   seqs
		   .
		   nil)
		  .
		  nil)
		.
		nil)
	     'map)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'map)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'map (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| map|
				 (lambda (f . seqs)
				   (if (not
					 (ar-false?
					   (_some
					     (let ((| map| (lambda () (_isa __ 'string))))
					       | map|)
					     seqs)))
				     ((let ((| map|
					       (lambda (n)
						 ((let ((| map|
							   (lambda (new)
							     ((let ((| map|
								       (lambda ()
									 (ar-call-resolve-notation
									   ((let ((| map|
										     (lambda (self)
										       (begin
											 (let ((zz
												 (let ((| self|
													  (lambda (i)
													    (if (not
														  (ar-false?
														    (_is
														      i
														      n)))
													      new
													      ((let ((| self|
															(lambda ()
															  (_sref
															    new
															    (_apply
															      f
															      (_map
																(let ((| self|
																	 (lambda ()
																	   (ar-call-resolve-notation
																	     __
																	     i))))
																  | self|)
																seqs))
															    i)
															  (ar-call-resolve-notation
															    self
															    (_+
															      i
															      1)))))
														 | self|))))))
												   | self|)))
											   (set! self
											     zz)
											   zz)))))
									      | map|)
									    'nil)
									   0))))
								| map|)))))
						    | map|)
						  (_newstring n)))))
					| map|)
				      (_apply _min (_map _len seqs)))
				     (if (not (ar-false? (_no (_cdr seqs))))
				       (_map1 f (_car seqs))
				       (ar-call-resolve-notation
					 ((let ((| map|
						   (lambda (self)
						     (begin
						       (let ((zz
							       (let ((| self|
									(lambda (seqs)
									  (if (not
										(ar-false?
										  (_some
										    _no
										    seqs)))
									    'nil
									    (_cons
									      (_apply
										f
										(_map1 _car seqs))
									      (ar-call-resolve-notation
										self
										(_map1
										  _cdr
										  seqs)))))))
								 | self|)))
							 (set! self zz)
							 zz)))))
					    | map|)
					  'nil)
					 seqs))))))
			  | map|)))
		  (namespace-set-variable-value! '_map zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'map)))
	     (_setinfixop 'map ((ar-coerce _infixop 'fn) 'map))
	     'nil)))


	((lambda ()
	   (_sref _sig '(f . args) 'mappend)
	   (_sref _help* 'nil 'mappend)
	   (_sref _source-file* _current-load-file* 'mappend)
	   (_sref
	     _source*
	     '(def
		mappend
		(f . args)
		(apply + nil (apply map f args . nil) . nil)
		.
		nil)
	     'mappend)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'mappend)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'mappend (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| mappend|
				 (lambda (f . args)
				   (_apply _+ 'nil (_apply _map f args)))))
			  | mappend|)))
		  (namespace-set-variable-value! '_mappend zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'mappend)))
	     (_setinfixop 'mappend ((ar-coerce _infixop 'fn) 'mappend))
	     'nil)))


	((lambda ()
	   (_sref _sig '(n xs . nil) 'firstn)
	   (_sref _help* 'nil 'firstn)
	   (_sref _source-file* _current-load-file* 'firstn)
	   (_sref
	     _source*
	     '(def
		firstn
		(n xs . nil)
		(if (no n . nil)
		  xs
		  (and (> n 0 . nil) xs . nil)
		  (cons (car xs . nil) (firstn (- n 1 . nil) (cdr xs . nil) . nil) . nil)
		  nil
		  .
		  nil)
		.
		nil)
	     'firstn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'firstn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'firstn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| firstn|
				 (lambda (n xs)
				   (if (not (ar-false? (_no n)))
				     xs
				     (if (not
					   (ar-false?
					     (if (not (ar-false? (_> n 0))) xs 'nil)))
				       (_cons (_car xs) (_firstn (_- n 1) (_cdr xs)))
				       'nil)))))
			  | firstn|)))
		  (namespace-set-variable-value! '_firstn zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'firstn)))
	     (_setinfixop 'firstn ((ar-coerce _infixop 'fn) 'firstn))
	     'nil)))


	((lambda ()
	   (_sref _sig '(n xs . nil) 'nthcdr)
	   (_sref _help* 'nil 'nthcdr)
	   (_sref _source-file* _current-load-file* 'nthcdr)
	   (_sref
	     _source*
	     '(def
		nthcdr
		(n xs . nil)
		(if (no n . nil)
		  xs
		  (> n 0 . nil)
		  (nthcdr (- n 1 . nil) (cdr xs . nil) . nil)
		  xs
		  .
		  nil)
		.
		nil)
	     'nthcdr)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'nthcdr)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'nthcdr (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| nthcdr|
				 (lambda (n xs)
				   (if (not (ar-false? (_no n)))
				     xs
				     (if (not (ar-false? (_> n 0)))
				       (_nthcdr (_- n 1) (_cdr xs))
				       xs)))))
			  | nthcdr|)))
		  (namespace-set-variable-value! '_nthcdr zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'nthcdr)))
	     (_setinfixop 'nthcdr ((ar-coerce _infixop 'fn) 'nthcdr))
	     'nil)))


	((lambda ()
	   (_sref _sig '(xs (o n 2 . nil) . nil) 'tuples)
	   (_sref _help* 'nil 'tuples)
	   (_sref _source-file* _current-load-file* 'tuples)
	   (_sref
	     _source*
	     '(def
		tuples
		(xs (o n 2 . nil) . nil)
		(if (no xs . nil)
		  nil
		  (cons (firstn n xs . nil) (tuples (nthcdr n xs . nil) n . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'tuples)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'tuples)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'tuples (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(lambda g2460
			  (let* ((xs (car g2460))
				 (n
				   (if (pair? (ar-xcdr g2460)) (car (ar-xcdr g2460)) 2)))
			    (if (not (ar-false? (_no xs)))
			      'nil
			      (_cons (_firstn n xs) (_tuples (_nthcdr n xs) n)))))))
		  (namespace-set-variable-value! '_tuples zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'tuples)))
	     (_setinfixop 'tuples ((ar-coerce _infixop 'fn) 'tuples))
	     'nil)))


	((lambda ()
	   (_sref _sig 'args 'defs)
	   (_sref _help* 'nil 'defs)
	   (_sref _source-file* _current-load-file* 'defs)
	   (_sref
	     _source*
	     '(mac
		defs
		args
		(quasiquote
		  (do
		    (unquote-splicing
		      (map
			(make-br-fn (cons (quote def . nil) _ . nil) . nil)
			(tuples args 3 . nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'defs)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'defs)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'defs (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| defs|
				   (lambda args
				     `(do
					,@(ar-nil-terminate
					    (_map
					      (let ((| defs| (lambda () (_cons 'def __))))
						| defs|)
					      (_tuples args 3)))))))
			    | defs|))))
		  (namespace-set-variable-value! '_defs zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(x val . nil) 'caris)
	   (_sref _help* 'nil 'caris)
	   (_sref _source-file* _current-load-file* 'caris)
	   (_sref
	     _source*
	     '(def
		caris
		(x val . nil)
		(and (acons x . nil) (is (car x . nil) val . nil) . nil)
		.
		nil)
	     'caris)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'caris)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'caris (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| caris|
				 (lambda (x val)
				   (if (not (ar-false? (_acons x)))
				     (_is (_car x) val)
				     'nil))))
			  | caris|)))
		  (namespace-set-variable-value! '_caris zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'caris)))
	     (_setinfixop 'caris ((ar-coerce _infixop 'fn) 'caris))
	     'nil)))


	((lambda ()
	   (_sref _sig '(msg . args) 'warn)
	   (_sref _help* 'nil 'warn)
	   (_sref _source-file* _current-load-file* 'warn)
	   (_sref
	     _source*
	     '(def
		warn
		(msg . args)
		(disp (+ "Warning: " msg ". " . nil) . nil)
		(map
		  (make-br-fn (do (write _ . nil) (disp " " . nil) . nil) . nil)
		  args
		  .
		  nil)
		(disp #\newline . nil)
		.
		nil)
	     'warn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'warn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'warn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| warn|
				 (lambda (msg . args)
				   (_disp (_+ "Warning: " msg ". "))
				   (_map
				     (let ((| warn|
					      (lambda ()
						((let ((| warn|
							  (lambda () (_write __) (_disp " "))))
						   | warn|)))))
				       | warn|)
				     args)
				   (_disp #\newline))))
			  | warn|)))
		  (namespace-set-variable-value! '_warn zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'warn)))
	     (_setinfixop 'warn ((ar-coerce _infixop 'fn) 'warn))
	     'nil)))


	((lambda ()
	   (_sref _sig 'body 'atomic)
	   (_sref _help* 'nil 'atomic)
	   (_sref _source-file* _current-load-file* 'atomic)
	   (_sref
	     _source*
	     '(mac
		atomic
		body
		(quasiquote
		  (atomic-invoke (fn nil (unquote-splicing body . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'atomic)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'atomic)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'atomic (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| atomic|
				   (lambda body
				     `(atomic-invoke (fn nil ,@(ar-nil-terminate body))))))
			    | atomic|))))
		  (namespace-set-variable-value! '_atomic zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'atlet)
	   (_sref _help* 'nil 'atlet)
	   (_sref _source-file* _current-load-file* 'atlet)
	   (_sref
	     _source*
	     '(mac
		atlet
		args
		(quasiquote
		  (atomic (let (unquote-splicing args . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'atlet)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'atlet)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'atlet (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| atlet|
				   (lambda args
				     `(atomic (let ,@(ar-nil-terminate args))))))
			    | atlet|))))
		  (namespace-set-variable-value! '_atlet zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'atwith)
	   (_sref _help* 'nil 'atwith)
	   (_sref _source-file* _current-load-file* 'atwith)
	   (_sref
	     _source*
	     '(mac
		atwith
		args
		(quasiquote
		  (atomic (with (unquote-splicing args . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'atwith)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'atwith)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'atwith (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| atwith|
				   (lambda args
				     `(atomic (with ,@(ar-nil-terminate args))))))
			    | atwith|))))
		  (namespace-set-variable-value! '_atwith zz)
		  zz))))))


	((lambda ()
	   (_sref _sig 'args 'atwiths)
	   (_sref _help* 'nil 'atwiths)
	   (_sref _source-file* _current-load-file* 'atwiths)
	   (_sref
	     _source*
	     '(mac
		atwiths
		args
		(quasiquote
		  (atomic (withs (unquote-splicing args . nil) . nil) . nil)
		  .
		  nil)
		.
		nil)
	     'atwiths)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'atwiths)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'atwiths (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| atwiths|
				   (lambda args
				     `(atomic (withs ,@(ar-nil-terminate args))))))
			    | atwiths|))))
		  (namespace-set-variable-value! '_atwiths zz)
		  zz))))))


	(begin (let ((zz (_table))) (namespace-set-variable-value! '_setter zz) zz))


	((lambda ()
	   (_sref _sig '(name parms . body) 'defset)
	   (_sref _help* 'nil 'defset)
	   (_sref _source-file* _current-load-file* 'defset)
	   (_sref
	     _source*
	     '(mac
		defset
		(name parms . body)
		(w/uniq
		  gexpr
		  (quasiquote
		    (sref
		      setter
		      (fn
			((unquote gexpr . nil) . nil)
			(let (unquote parms . nil)
			  (cdr (unquote gexpr . nil) . nil)
			  (unquote-splicing body . nil)
			  .
			  nil)
			.
			nil)
		      (quote (unquote name . nil) . nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'defset)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'defset)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'defset (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| defset|
				   (lambda (name parms . body)
				     ((let ((| defset|
					       (lambda (gexpr)
						 `(sref
						    setter
						    (fn
						      (,gexpr)
						      (let ,parms
							(cdr ,gexpr)
							,@(ar-nil-terminate body)))
						    ',name))))
					| defset|)
				      (_uniq)))))
			    | defset|))))
		  (namespace-set-variable-value! '_defset zz)
		  zz))))))


	(_sref
	  _setter
	  (lambda (g2461)
	    ((lambda g2462
	       (let* ((x (ar-xcar (car g2462))))
		 ((lambda (g) (_list (_list g x) `(car ,g) `(fn (val) (scar ,g val))))
		  (_uniq))))
	     (_cdr g2461)))
	  'car)


	(_sref
	  _setter
	  (lambda (g2463)
	    ((lambda g2464
	       (let* ((x (ar-xcar (car g2464))))
		 ((lambda (g) (_list (_list g x) `(cdr ,g) `(fn (val) (scdr ,g val))))
		  (_uniq))))
	     (_cdr g2463)))
	  'cdr)


	(_sref
	  _setter
	  (lambda (g2465)
	    ((lambda g2466
	       (let* ((x (ar-xcar (car g2466))))
		 ((lambda (g)
		    (_list (_list g x) `(caar ,g) `(fn (val) (scar (car ,g) val))))
		  (_uniq))))
	     (_cdr g2465)))
	  'caar)


	(_sref
	  _setter
	  (lambda (g2467)
	    ((lambda g2468
	       (let* ((x (ar-xcar (car g2468))))
		 ((lambda (g)
		    (_list (_list g x) `(cadr ,g) `(fn (val) (scar (cdr ,g) val))))
		  (_uniq))))
	     (_cdr g2467)))
	  'cadr)


	(_sref
	  _setter
	  (lambda (g2469)
	    ((lambda g2470
	       (let* ((x (ar-xcar (car g2470))))
		 ((lambda (g)
		    (_list (_list g x) `(cddr ,g) `(fn (val) (scdr (cdr ,g) val))))
		  (_uniq))))
	     (_cdr g2469)))
	  'cddr)


	((lambda ()
	   (_sref _sig '(expr0 . nil) 'setforms)
	   (_sref _help* 'nil 'setforms)
	   (_sref _source-file* _current-load-file* 'setforms)
	   (_sref
	     _source*
	     '(def
		setforms
		(expr0 . nil)
		(let expr (macex expr0 . nil)
		  (if (isa expr (quote sym . nil) . nil)
		    (if (ssyntax expr . nil)
		      (setforms (ssexpand expr . nil) . nil)
		      (w/uniq
			(g h . nil)
			(list
			  (list g expr . nil)
			  g
			  (quasiquote
			    (fn
			      ((unquote h . nil) . nil)
			      (assign (unquote expr . nil) (unquote h . nil) . nil)
			      .
			      nil)
			    .
			    nil)
			  .
			  nil)
			.
			nil)
		      .
		      nil)
		    (and (acons expr . nil) (metafn (car expr . nil) . nil) . nil)
		    (setforms
		      (expand-metafn-call
			(ssexpand (car expr . nil) . nil)
			(cdr expr . nil)
			.
			nil)
		      .
		      nil)
		    (and (acons expr . nil)
			 (acons (car expr . nil) . nil)
			 (is (caar expr . nil) (quote get . nil) . nil)
			 .
			 nil)
		    (setforms
		      (list (cadr expr . nil) (cadr (car expr . nil) . nil) . nil)
		      .
		      nil)
		    (let f (setter (car expr . nil) . nil)
		      (if f
			(f expr . nil)
			(do (when
			      (caris (car expr . nil) (quote fn . nil) . nil)
			      (warn
				"Inverting what looks like a function call"
				expr0
				expr
				.
				nil)
			      .
			      nil)
			  (w/uniq
			    (g h . nil)
			    (let argsyms (map
					   (make-br-fn (uniq . nil) . nil)
					   (cdr expr . nil)
					   .
					   nil)
			      (list
				(+
				  (list g (car expr . nil) . nil)
				  (mappend list argsyms (cdr expr . nil) . nil)
				  .
				  nil)
				(quasiquote
				  ((unquote g . nil)
				   (unquote-splicing argsyms . nil)
				   .
				   nil)
				  .
				  nil)
				(quasiquote
				  (fn
				    ((unquote h . nil) . nil)
				    (sref
				      (unquote g . nil)
				      (unquote h . nil)
				      (unquote (car argsyms . nil) . nil)
				      .
				      nil)
				    .
				    nil)
				  .
				  nil)
				.
				nil)
			      .
			      nil)
			    .
			    nil)
			  .
			  nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'setforms)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'setforms)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'setforms (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| setforms|
				 (lambda (expr0)
				   ((let ((| setforms|
					     (lambda (expr)
					       (if (not (ar-false? (_isa expr 'sym)))
						 (if (not (ar-false? (_ssyntax expr)))
						   (_setforms (_ssexpand expr))
						   ((let ((| setforms|
							     (lambda (g h)
							       (_list
								 (_list g expr)
								 g
								 `(fn
								    (,h)
								    (assign ,expr ,h))))))
						      | setforms|)
						    (_uniq)
						    (_uniq)))
						 (if (not
						       (ar-false?
							 (if (not (ar-false? (_acons expr)))
							   (_metafn (_car expr))
							   'nil)))
						   (_setforms
						     (_expand-metafn-call
						       (_ssexpand (_car expr))
						       (_cdr expr)))
						   (if (not
							 (ar-false?
							   (if (not (ar-false? (_acons expr)))
							     (if (not
								   (ar-false?
								     (_acons (_car expr))))
							       (_is (_caar expr) 'get)
							       'nil)
							     'nil)))
						     (_setforms
						       (_list
							 (_cadr expr)
							 (_cadr (_car expr))))
						     ((let ((| setforms|
							       (lambda (f)
								 (if (not (ar-false? f))
								   (ar-call-resolve-notation
								     f
								     expr)
								   ((let ((| setforms|
									     (lambda ()
									       (if (not
										     (ar-false?
										       (_caris
											 (_car
											   expr)
											 'fn)))
										 ((let ((| setforms|
											   (lambda ()
											     (_warn
											       "Inverting what looks like a function call"
											       expr0
											       expr))))
										    | setforms|))
										 'nil)
									       ((let ((| setforms|
											 (lambda (g
												   h)
											   ((let ((| setforms|
												     (lambda (argsyms)
												       (_list
													 (_+
													   (_list
													     g
													     (_car
													       expr))
													   (_mappend
													     _list
													     argsyms
													     (_cdr
													       expr)))
													 `(,g
													    ,@(ar-nil-terminate
														argsyms))
													 `(fn
													    (,h)
													    (sref
													      ,g
													      ,h
													      ,(_car
														 argsyms)))))))
											      | setforms|)
											    (_map
											      (let ((| argsyms|
												       (lambda ()
													 (_uniq))))
												| argsyms|)
											      (_cdr
												expr))))))
										  | setforms|)
										(_uniq)
										(_uniq)))))
								      | setforms|))))))
							| setforms|)
						      (ar-call-resolve-notation
							_setter
							(_car expr)))))))))
				      | setforms|)
				    (_macex expr0)))))
			  | setforms|)))
		  (namespace-set-variable-value! '_setforms zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'setforms)))
	     (_setinfixop 'setforms ((ar-coerce _infixop 'fn) 'setforms))
	     'nil)))


	((lambda ()
	   (_sref _sig '(x . nil) 'metafn)
	   (_sref _help* 'nil 'metafn)
	   (_sref _source-file* _current-load-file* 'metafn)
	   (_sref
	     _source*
	     '(def
		metafn
		(x . nil)
		(or (ssyntax x . nil)
		    (and (acons x . nil)
			 (in
			   (car x . nil)
			   (quote compose . nil)
			   (quote complement . nil)
			   .
			   nil)
			 .
			 nil)
		    .
		    nil)
		.
		nil)
	     'metafn)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'metafn)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'metafn (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| metafn|
				 (lambda (x)
				   ((let ((| metafn|
					     (lambda (g2471)
					       (if (not (ar-false? g2471))
						 g2471
						 ((let ((| metafn|
							   (lambda (g2472)
							     (if (not (ar-false? g2472))
							       g2472
							       'nil))))
						    | metafn|)
						  (if (not (ar-false? (_acons x)))
						    ((let ((| g2472|
							      (lambda (g2473)
								((let ((| g2472|
									  (lambda (g2474)
									    (if (not
										  (ar-false?
										    g2474))
									      g2474
									      ((let ((| g2472|
											(lambda (g2475)
											  (if (not
												(ar-false?
												  g2475))
											    g2475
											    'nil))))
										 | g2472|)
									       (_is
										 g2473
										 'complement))))))
								   | g2472|)
								 (_is g2473 'compose)))))
						       | g2472|)
						     (_car x))
						    'nil))))))
				      | metafn|)
				    (_ssyntax x)))))
			  | metafn|)))
		  (namespace-set-variable-value! '_metafn zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'metafn)))
	     (_setinfixop 'metafn ((ar-coerce _infixop 'fn) 'metafn))
	     'nil)))


	((lambda ()
	   (_sref _sig '(f args . nil) 'expand-metafn-call)
	   (_sref _help* 'nil 'expand-metafn-call)
	   (_sref _source-file* _current-load-file* 'expand-metafn-call)
	   (_sref
	     _source*
	     '(def
		expand-metafn-call
		(f args . nil)
		(if (is (car f . nil) (quote compose . nil) . nil)
		  ((afn
		     (fs . nil)
		     (if (caris (car fs . nil) (quote compose . nil) . nil)
		       (self (join (cdr (car fs . nil) . nil) (cdr fs . nil) . nil) . nil)
		       (cdr fs . nil)
		       (list (car fs . nil) (self (cdr fs . nil) . nil) . nil)
		       (cons (car fs . nil) args . nil)
		       .
		       nil)
		     .
		     nil)
		   (cdr f . nil)
		   .
		   nil)
		  (is (car f . nil) (quote no . nil) . nil)
		  (err "Can't invert " (cons f args . nil) . nil)
		  (cons f args . nil)
		  .
		  nil)
		.
		nil)
	     'expand-metafn-call)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'expand-metafn-call)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'expand-metafn-call (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| expand-metafn-call|
				 (lambda (f args)
				   (if (not (ar-false? (_is (_car f) 'compose)))
				     (ar-call-resolve-notation
				       ((let ((| expand-metafn-call|
						 (lambda (self)
						   (begin
						     (let ((zz
							     (let ((| self|
								      (lambda (fs)
									(if (not
									      (ar-false?
										(_caris
										  (_car fs)
										  'compose)))
									  (ar-call-resolve-notation
									    self
									    (_join
									      (_cdr (_car fs))
									      (_cdr fs)))
									  (if (not
										(ar-false?
										  (_cdr fs)))
									    (_list
									      (_car fs)
									      (ar-call-resolve-notation
										self
										(_cdr fs)))
									    (_cons
									      (_car fs)
									      args))))))
							       | self|)))
						       (set! self zz)
						       zz)))))
					  | expand-metafn-call|)
					'nil)
				       (_cdr f))
				     (if (not (ar-false? (_is (_car f) 'no)))
				       (_err "Can't invert " (_cons f args))
				       (_cons f args))))))
			  | expand-metafn-call|)))
		  (namespace-set-variable-value! '_expand-metafn-call zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'expand-metafn-call)))
	     (_setinfixop
	       'expand-metafn-call
	       ((ar-coerce _infixop 'fn) 'expand-metafn-call))
	     'nil)))


	((lambda ()
	   (_sref _sig '(place val . nil) 'expand=)
	   (_sref _help* 'nil 'expand=)
	   (_sref _source-file* _current-load-file* 'expand=)
	   (_sref
	     _source*
	     '(def
		expand=
		(place val . nil)
		(if (and (isa place (quote sym . nil) . nil)
			 (~ssyntax place . nil)
			 .
			 nil)
		  (quasiquote
		    (assign (unquote place . nil) (unquote val . nil) . nil)
		    .
		    nil)
		  (let (vars prev setter . nil)
		    (setforms place . nil)
		    (w/uniq
		      g
		      (quasiquote
			(atwith
			  (unquote (+ vars (list g val . nil) . nil) . nil)
			  ((unquote setter . nil) (unquote g . nil) . nil)
			  .
			  nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'expand=)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'expand=)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'expand= (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| expand=|
				 (lambda (place val)
				   (if (not
					 (ar-false?
					   (if (not (ar-false? (_isa place 'sym)))
					     (_no (_ssyntax place))
					     'nil)))
				     `(assign ,place ,val)
				     ((lambda g2476
					(let* ((vars (ar-xcar (car g2476)))
					       (prev (ar-xcar (ar-xcdr (car g2476))))
					       (setter
						 (ar-xcar
						   (ar-xcdr (ar-xcdr (car g2476))))))
					  ((let ((| expand=|
						    (lambda (g)
						      `(atwith
							 ,(_+ vars (_list g val))
							 (,setter ,g)))))
					     | expand=|)
					   (_uniq))))
				      (_setforms place))))))
			  | expand=|)))
		  (namespace-set-variable-value! '_expand= zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'expand=)))
	     (_setinfixop 'expand= ((ar-coerce _infixop 'fn) 'expand=))
	     'nil)))


	((lambda ()
	   (_sref _sig '(terms . nil) 'expand=list)
	   (_sref _help* 'nil 'expand=list)
	   (_sref _source-file* _current-load-file* 'expand=list)
	   (_sref
	     _source*
	     '(def
		expand=list
		(terms . nil)
		(quasiquote
		  (do
		    (unquote-splicing
		      (map
			(fn ((p v . nil) . nil) (expand= p v . nil) . nil)
			(pair terms . nil)
			.
			nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'expand=list)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'expand=list)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'expand=list (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(let ((| expand=list|
				 (lambda (terms)
				   `(do
				      ,@(ar-nil-terminate
					  (_map
					    (lambda g2477
					      (let* ((p (ar-xcar (car g2477)))
						     (v (ar-xcar (ar-xcdr (car g2477)))))
						(_expand= p v)))
					    (_pair terms)))))))
			  | expand=list|)))
		  (namespace-set-variable-value! '_expand=list zz)
		  zz))))
	   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'expand=list)))
	     (_setinfixop 'expand=list ((ar-coerce _infixop 'fn) 'expand=list))
	     'nil)))


	((lambda ()
	   (_sref _sig 'args '=)
	   (_sref _help* 'nil '=)
	   (_sref _source-file* _current-load-file* '=)
	   (_sref _source* '(mac = args (expand=list args . nil) . nil) '=)
	   ((lambda ()
	      (if (not (ar-false? (_bound '=)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp '= (_stderr))
		   (_disp #\newline (_stderr))))
		'nil)
	      (begin
		(let ((zz
			(_annotate
			  'mac
			  (let ((| =| (lambda args (_expand=list args)))) | =|))))
		  (namespace-set-variable-value! '_= zz)
		  zz))))))


	((lambda ()
	   (_sref _sig '(start test update . body) 'loop)
	   (_sref _help* 'nil 'loop)
	   (_sref _source-file* _current-load-file* 'loop)
	   (_sref
	     _source*
	     '(mac
		loop
		(start test update . body)
		(w/uniq
		  (gfn gparm . nil)
		  (quasiquote
		    (do (unquote start . nil)
		      ((rfn
			 (unquote gfn . nil)
			 ((unquote gparm . nil) . nil)
			 (if (unquote gparm . nil)
			   (do (unquote-splicing body . nil)
			     (unquote update . nil)
			     ((unquote gfn . nil) (unquote test . nil) . nil)
			     .
			     nil)
			   .
			   nil)
			 .
			 nil)
		       (unquote test . nil)
		       .
		       nil)
		      .
		      nil)
		    .
		    nil)
		  .
		  nil)
		.
		nil)
	     'loop)
	   ((lambda ()
	      (if (not (ar-false? (_bound 'loop)))
		((lambda ()
		   (_disp "*** redefining " (_stderr))
		   (_disp 'loop (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| loop|
                       (lambda (start test update . body)
                         ((let ((| loop|
                                 (lambda (gfn gparm)
                                   `(do ,start
                                        ((rfn
                                          ,gfn
                                          (,gparm)
                                          (if ,gparm
                                            (do ,@(ar-nil-terminate body)
                                                ,update
                                              (,gfn ,test))))
                                         ,test)))))
                            | loop|)
                          (_uniq)
                          (_uniq)))))
                  | loop|))))
          (namespace-set-variable-value! '_loop zz)
          zz))))))


((lambda ()
   (_sref _sig '(v init max . body) 'for)
   (_sref _help* 'nil 'for)
   (_sref _source-file* _current-load-file* 'for)
   (_sref
    _source*
    '(mac
      for
      (v init max . body)
      (w/uniq
       (gi gm . nil)
       (quasiquote
        (with
         ((unquote v . nil)
          nil
          (unquote gi . nil)
          (unquote init . nil)
          (unquote gm . nil)
          (+ (unquote max . nil) 1 . nil)
          .
          nil)
         (loop
          (assign (unquote v . nil) (unquote gi . nil) . nil)
          (< (unquote v . nil) (unquote gm . nil) . nil)
          (assign (unquote v . nil) (+ (unquote v . nil) 1 . nil) . nil)
          (unquote-splicing body . nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'for)
   ((lambda ()
      (if (not (ar-false? (_bound 'for)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'for (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| for|
                       (lambda (v init max . body)
                         ((let ((| for|
                                 (lambda (gi gm)
                                   `(with
                                     (,v nil ,gi ,init ,gm (+ ,max 1))
                                     (loop
                                      (assign ,v ,gi)
                                      (< ,v ,gm)
                                      (assign ,v (+ ,v 1))
                                      ,@(ar-nil-terminate body))))))
                            | for|)
                          (_uniq)
                          (_uniq)))))
                  | for|))))
          (namespace-set-variable-value! '_for zz)
          zz))))))


((lambda ()
   (_sref _sig '(v init min . body) 'down)
   (_sref _help* 'nil 'down)
   (_sref _source-file* _current-load-file* 'down)
   (_sref
    _source*
    '(mac
      down
      (v init min . body)
      (w/uniq
       (gi gm . nil)
       (quasiquote
        (with
         ((unquote v . nil)
          nil
          (unquote gi . nil)
          (unquote init . nil)
          (unquote gm . nil)
          (- (unquote min . nil) 1 . nil)
          .
          nil)
         (loop
          (assign (unquote v . nil) (unquote gi . nil) . nil)
          (> (unquote v . nil) (unquote gm . nil) . nil)
          (assign (unquote v . nil) (- (unquote v . nil) 1 . nil) . nil)
          (unquote-splicing body . nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'down)
   ((lambda ()
      (if (not (ar-false? (_bound 'down)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'down (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| down|
                       (lambda (v init min . body)
                         ((let ((| down|
                                 (lambda (gi gm)
                                   `(with
                                     (,v nil ,gi ,init ,gm (- ,min 1))
                                     (loop
                                      (assign ,v ,gi)
                                      (> ,v ,gm)
                                      (assign ,v (- ,v 1))
                                      ,@(ar-nil-terminate body))))))
                            | down|)
                          (_uniq)
                          (_uniq)))))
                  | down|))))
          (namespace-set-variable-value! '_down zz)
          zz))))))


((lambda ()
   (_sref _sig '(n . body) 'repeat)
   (_sref _help* 'nil 'repeat)
   (_sref _source-file* _current-load-file* 'repeat)
   (_sref
    _source*
    '(mac
      repeat
      (n . body)
      (quasiquote
       (for
        (unquote (uniq . nil) . nil)
        1
        (unquote n . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'repeat)
   ((lambda ()
      (if (not (ar-false? (_bound 'repeat)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'repeat (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| repeat|
                       (lambda (n . body)
                         `(for ,(_uniq) 1 ,n ,@(ar-nil-terminate body)))))
                  | repeat|))))
          (namespace-set-variable-value! '_repeat zz)
          zz))))))


((lambda ()
   (_sref _sig '(seq func . nil) 'walk)
   (_sref _help* 'nil 'walk)
   (_sref _source-file* _current-load-file* 'walk)
   (_sref
    _source*
    '(def
      walk
      (seq func . nil)
      (if alist.seq
        ((afn
          (l . nil)
          (when (acons l . nil)
            (func (car l . nil) . nil)
            (self (cdr l . nil) . nil)
            .
            nil)
          .
          nil)
         seq
         .
         nil)
        (isa seq (quote table . nil) . nil)
        (maptable
         (fn (k v . nil) (func (list k v . nil) . nil) . nil)
         seq
         .
         nil)
        (for i 0 (- (len seq . nil) 1 . nil) (func (seq i . nil) . nil) . nil)
        .
        nil)
      .
      nil)
    'walk)
   ((lambda ()
      (if (not (ar-false? (_bound 'walk)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'walk (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| walk|
                      (lambda (seq func)
                        (if (not (ar-false? (_alist seq)))
                          (ar-call-resolve-notation
                           ((let ((| walk|
                                   (lambda (self)
                                     (begin
                                       (let ((zz
                                              (let ((| self|
                                                     (lambda (l)
                                                       (if (not
                                                            (ar-false?
                                                             (_acons l)))
                                                         ((let ((| self|
                                                                 (lambda ()
                                                                   (ar-call-resolve-notation
                                                                    func
                                                                    (_car l))
                                                                   (ar-call-resolve-notation
                                                                    self
                                                                    (_cdr
                                                                     l)))))
                                                            | self|))
                                                         'nil))))
                                                | self|)))
                                         (set! self zz)
                                         zz)))))
                              | walk|)
                            'nil)
                           seq)
                          (if (not (ar-false? (_isa seq 'table)))
                            (_maptable
                             (let ((| walk|
                                    (lambda (k v)
                                      (ar-call-resolve-notation
                                       func
                                       (_list k v)))))
                               | walk|)
                             seq)
                            ((let ((| walk|
                                    (lambda (i g2478 g2479)
                                      ((let ((| walk|
                                              (lambda ()
                                                (begin
                                                  (let ((zz g2478))
                                                    (set! i zz)
                                                    zz))
                                                (ar-call-resolve-notation
                                                 ((let ((| walk|
                                                         (lambda (g2480)
                                                           (begin
                                                             (let ((zz
                                                                    (let ((| g2480|
                                                                           (lambda (g2481)
                                                                             (if (not
                                                                                  (ar-false?
                                                                                   g2481))
                                                                               ((let ((| g2480|
                                                                                       (lambda ()
                                                                                         (ar-call-resolve-notation
                                                                                          func
                                                                                          (ar-call-resolve-notation
                                                                                           seq
                                                                                           i))
                                                                                         (begin
                                                                                           (let ((zz
                                                                                                  (_+
                                                                                                   i
                                                                                                   1)))
                                                                                             (set! i
                                                                                               zz)
                                                                                             zz))
                                                                                         (ar-call-resolve-notation
                                                                                          g2480
                                                                                          (_<
                                                                                           i
                                                                                           g2479)))))
                                                                                  | g2480|))
                                                                               'nil))))
                                                                      | g2480|)))
                                                               (set! g2480 zz)
                                                               zz)))))
                                                    | walk|)
                                                  'nil)
                                                 (_< i g2479)))))
                                         | walk|)))))
                               | walk|)
                             'nil
                             0
                             (_+ (_- (_len seq) 1) 1)))))))
                 | walk|)))
          (namespace-set-variable-value! '_walk zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'walk)))
     (_setinfixop 'walk ((ar-coerce _infixop 'fn) 'walk))
     'nil)))


((lambda ()
   (_sref _sig '(var expr . body) 'each)
   (_sref _help* 'nil 'each)
   (_sref _source-file* _current-load-file* 'each)
   (_sref
    _source*
    '(mac
      each
      (var expr . body)
      (quasiquote
       (walk
        (unquote expr . nil)
        (fn ((unquote var . nil) . nil) (unquote-splicing body . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'each)
   ((lambda ()
      (if (not (ar-false? (_bound 'each)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'each (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| each|
                       (lambda (var expr . body)
                         `(walk ,expr (fn (,var) ,@(ar-nil-terminate body))))))
                  | each|))))
          (namespace-set-variable-value! '_each zz)
          zz))))))


((lambda ()
   (_sref _sig '(seq start (o end . nil) . nil) 'cut)
   (_sref _help* 'nil 'cut)
   (_sref _source-file* _current-load-file* 'cut)
   (_sref
    _source*
    '(def
      cut
      (seq start (o end . nil) . nil)
      (let end (if
                (no end . nil)
                (len seq . nil)
                (< end 0 . nil)
                (+ (len seq . nil) end . nil)
                end
                .
                nil)
        (if (isa seq (quote string . nil) . nil)
          (let s2 (newstring (- end start . nil) . nil)
            (for
             i
             0
             (- end start 1 . nil)
             (= (s2 i . nil) (seq (+ start i . nil) . nil) . nil)
             .
             nil)
            s2
            .
            nil)
          (firstn (- end start . nil) (nthcdr start seq . nil) . nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'cut)
   ((lambda ()
      (if (not (ar-false? (_bound 'cut)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'cut (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2482
                 (let* ((seq (car g2482))
                        (start (car (ar-xcdr g2482)))
                        (end
                         (if (pair? (ar-xcdr (ar-xcdr g2482)))
                           (car (ar-xcdr (ar-xcdr g2482)))
                           'nil)))
                   ((let ((| cut|
                           (lambda (end)
                             (if (not (ar-false? (_isa seq 'string)))
                               ((let ((| cut|
                                       (lambda (s2)
                                         ((let ((| cut|
                                                 (lambda (i g2483 g2484)
                                                   ((let ((| cut|
                                                           (lambda ()
                                                             (begin
                                                               (let ((zz
                                                                      g2483))
                                                                 (set! i zz)
                                                                 zz))
                                                             (ar-call-resolve-notation
                                                              ((let ((| cut|
                                                                      (lambda (g2485)
                                                                        (begin
                                                                          (let ((zz
                                                                                 (let ((| g2485|
                                                                                        (lambda (g2486)
                                                                                          (if (not
                                                                                               (ar-false?
                                                                                                g2486))
                                                                                            ((let ((| g2485|
                                                                                                    (lambda ()
                                                                                                      ((let ((| g2485|
                                                                                                              (lambda ()
                                                                                                                (_atomic-invoke
                                                                                                                 (let ((| g2485|
                                                                                                                        (lambda ()
                                                                                                                          ((let ((| g2485|
                                                                                                                                  (lambda (g2487
                                                                                                                                           g2489
                                                                                                                                           g2490)
                                                                                                                                    ((let ((| g2485|
                                                                                                                                            (lambda (g2488)
                                                                                                                                              (_sref
                                                                                                                                               g2487
                                                                                                                                               g2488
                                                                                                                                               g2489))))
                                                                                                                                       | g2485|)
                                                                                                                                     g2490))))
                                                                                                                             | g2485|)
                                                                                                                           s2
                                                                                                                           i
                                                                                                                           (ar-call-resolve-notation
                                                                                                                            seq
                                                                                                                            (_+
                                                                                                                             start
                                                                                                                             i))))))
                                                                                                                   | g2485|)))))
                                                                                                         | g2485|))
                                                                                                      (begin
                                                                                                        (let ((zz
                                                                                                               (_+
                                                                                                                i
                                                                                                                1)))
                                                                                                          (set! i
                                                                                                            zz)
                                                                                                          zz))
                                                                                                      (ar-call-resolve-notation
                                                                                                       g2485
                                                                                                       (_<
                                                                                                        i
                                                                                                        g2484)))))
                                                                                               | g2485|))
                                                                                            'nil))))
                                                                                   | g2485|)))
                                                                            (set! g2485
                                                                              zz)
                                                                            zz)))))
                                                                 | cut|)
                                                               'nil)
                                                              (_< i g2484)))))
                                                      | cut|)))))
                                            | cut|)
                                          'nil
                                          0
                                          (_+ (_- end start 1) 1))
                                         s2)))
                                  | cut|)
                                (_newstring (_- end start)))
                               (_firstn (_- end start) (_nthcdr start seq))))))
                      | cut|)
                    (if (not (ar-false? (_no end)))
                      (_len seq)
                      (if (not (ar-false? (_< end 0)))
                        (_+ (_len seq) end)
                        end)))))))
          (namespace-set-variable-value! '_cut zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'cut)))
     (_setinfixop 'cut ((ar-coerce _infixop 'fn) 'cut))
     'nil)))


((lambda ()
   (_sref _sig '(var test . body) 'whilet)
   (_sref _help* 'nil 'whilet)
   (_sref _source-file* _current-load-file* 'whilet)
   (_sref
    _source*
    '(mac
      whilet
      (var test . body)
      (w/uniq
       (gf gp . nil)
       (quasiquote
        ((rfn
          (unquote gf . nil)
          ((unquote gp . nil) . nil)
          (let (unquote var . nil)
            (unquote gp . nil)
            (when (unquote var . nil)
              (unquote-splicing body . nil)
              ((unquote gf . nil) (unquote test . nil) . nil)
              .
              nil)
            .
            nil)
          .
          nil)
         (unquote test . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'whilet)
   ((lambda ()
      (if (not (ar-false? (_bound 'whilet)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'whilet (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| whilet|
                       (lambda (var test . body)
                         ((let ((| whilet|
                                 (lambda (gf gp)
                                   `((rfn
                                      ,gf
                                      (,gp)
                                      (let ,var
                                        ,gp
                                        (when ,var
                                          ,@(ar-nil-terminate body)
                                          (,gf ,test))))
                                     ,test))))
                            | whilet|)
                          (_uniq)
                          (_uniq)))))
                  | whilet|))))
          (namespace-set-variable-value! '_whilet zz)
          zz))))))


((lambda ()
   (_sref _sig '(xs . nil) 'last)
   (_sref _help* 'nil 'last)
   (_sref _source-file* _current-load-file* 'last)
   (_sref
    _source*
    '(def
      last
      (xs . nil)
      (if (cdr xs . nil) (last (cdr xs . nil) . nil) (car xs . nil) . nil)
      .
      nil)
    'last)
   ((lambda ()
      (if (not (ar-false? (_bound 'last)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'last (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| last|
                      (lambda (xs)
                        (if (not (ar-false? (_cdr xs)))
                          (_last (_cdr xs))
                          (_car xs)))))
                 | last|)))
          (namespace-set-variable-value! '_last zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'last)))
     (_setinfixop 'last ((ar-coerce _infixop 'fn) 'last))
     'nil)))


((lambda ()
   (_sref _sig '(test seq . nil) 'rem)
   (_sref _help* 'nil 'rem)
   (_sref _source-file* _current-load-file* 'rem)
   (_sref
    _source*
    '(def
      rem
      (test seq . nil)
      (let f (testify test . nil)
        (if (alist seq . nil)
          ((afn
            (s . nil)
            (if (no s . nil)
              nil
              (f (car s . nil) . nil)
              (self (cdr s . nil) . nil)
              (cons (car s . nil) (self (cdr s . nil) . nil) . nil)
              .
              nil)
            .
            nil)
           seq
           .
           nil)
          (coerce
           (rem test (coerce seq (quote cons . nil) . nil) . nil)
           (quote string . nil)
           .
           nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'rem)
   ((lambda ()
      (if (not (ar-false? (_bound 'rem)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rem (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| rem|
                      (lambda (test seq)
                        ((let ((| rem|
                                (lambda (f)
                                  (if (not (ar-false? (_alist seq)))
                                    (ar-call-resolve-notation
                                     ((let ((| rem|
                                             (lambda (self)
                                               (begin
                                                 (let ((zz
                                                        (let ((| self|
                                                               (lambda (s)
                                                                 (if (not
                                                                      (ar-false?
                                                                       (_no
                                                                        s)))
                                                                   'nil
                                                                   (if (not
                                                                        (ar-false?
                                                                         (ar-call-resolve-notation
                                                                          f
                                                                          (_car
                                                                           s))))
                                                                     (ar-call-resolve-notation
                                                                      self
                                                                      (_cdr s))
                                                                     (_cons
                                                                      (_car s)
                                                                      (ar-call-resolve-notation
                                                                       self
                                                                       (_cdr
                                                                        s))))))))
                                                          | self|)))
                                                   (set! self zz)
                                                   zz)))))
                                        | rem|)
                                      'nil)
                                     seq)
                                    (_coerce
                                     (_rem test (_coerce seq 'cons))
                                     'string)))))
                           | rem|)
                         (_testify test)))))
                 | rem|)))
          (namespace-set-variable-value! '_rem zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rem)))
     (_setinfixop 'rem ((ar-coerce _infixop 'fn) 'rem))
     'nil)))


((lambda ()
   (_sref _sig '(test seq . nil) 'keep)
   (_sref _help* 'nil 'keep)
   (_sref _source-file* _current-load-file* 'keep)
   (_sref
    _source*
    '(def
      keep
      (test seq . nil)
      (rem (complement (testify test . nil) . nil) seq . nil)
      .
      nil)
    'keep)
   ((lambda ()
      (if (not (ar-false? (_bound 'keep)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'keep (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| keep|
                      (lambda (test seq)
                        (_rem
                         (let ((| keep|
                                (lambda g2491
                                  (_no (_apply (_testify test) g2491)))))
                           | keep|)
                         seq))))
                 | keep|)))
          (namespace-set-variable-value! '_keep zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'keep)))
     (_setinfixop 'keep ((ar-coerce _infixop 'fn) 'keep))
     'nil)))


((lambda ()
   (_sref _sig '(f xs . nil) 'trues)
   (_sref _help* 'nil 'trues)
   (_sref _source-file* _current-load-file* 'trues)
   (_sref
    _source*
    '(def
      trues
      (f xs . nil)
      (and xs
           (let fx (f (car xs . nil) . nil)
             (if fx
               (cons fx (trues f (cdr xs . nil) . nil) . nil)
               (trues f (cdr xs . nil) . nil)
               .
               nil)
             .
             nil)
           .
           nil)
      .
      nil)
    'trues)
   ((lambda ()
      (if (not (ar-false? (_bound 'trues)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'trues (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| trues|
                      (lambda (f xs)
                        (if (not (ar-false? xs))
                          ((let ((| trues|
                                  (lambda (fx)
                                    (if (not (ar-false? fx))
                                      (_cons fx (_trues f (_cdr xs)))
                                      (_trues f (_cdr xs))))))
                             | trues|)
                           (ar-call-resolve-notation f (_car xs)))
                          'nil))))
                 | trues|)))
          (namespace-set-variable-value! '_trues zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'trues)))
     (_setinfixop 'trues ((ar-coerce _infixop 'fn) 'trues))
     'nil)))


((lambda ()
   (_sref _sig 'args 'do1)
   (_sref _help* 'nil 'do1)
   (_sref _source-file* _current-load-file* 'do1)
   (_sref
    _source*
    '(mac
      do1
      args
      (w/uniq
       g
       (quasiquote
        (let (unquote g . nil)
          (unquote (car args . nil) . nil)
          (unquote-splicing (cdr args . nil) . nil)
          (unquote g . nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'do1)
   ((lambda ()
      (if (not (ar-false? (_bound 'do1)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'do1 (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| do1|
                       (lambda args
                         ((let ((| do1|
                                 (lambda (g)
                                   `(let ,g
                                      ,(_car args)
                                      ,@(ar-nil-terminate (_cdr args))
                                      ,g))))
                            | do1|)
                          (_uniq)))))
                  | do1|))))
          (namespace-set-variable-value! '_do1 zz)
          zz))))))


((lambda ()
   (_sref _sig '(var expr . args) 'caselet)
   (_sref _help* 'nil 'caselet)
   (_sref _source-file* _current-load-file* 'caselet)
   (_sref
    _source*
    '(mac
      caselet
      (var expr . args)
      (let ex (afn
               (args . nil)
               (if (no (cdr args . nil) . nil)
                 (car args . nil)
                 (quasiquote
                  (if (is
                       (unquote var . nil)
                       (quote (unquote (car args . nil) . nil) . nil)
                       .
                       nil)
                    (unquote (cadr args . nil) . nil)
                    (unquote (self (cddr args . nil) . nil) . nil)
                    .
                    nil)
                  .
                  nil)
                 .
                 nil)
               .
               nil)
        (quasiquote
         (let (unquote var . nil)
           (unquote expr . nil)
           (unquote (ex args . nil) . nil)
           .
           nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'caselet)
   ((lambda ()
      (if (not (ar-false? (_bound 'caselet)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'caselet (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| caselet|
                       (lambda (var expr . args)
                         ((let ((| caselet|
                                 (lambda (ex)
                                   `(let ,var
                                      ,expr
                                      ,(ar-call-resolve-notation ex args)))))
                            | caselet|)
                          ((let ((| ex|
                                  (lambda (self)
                                    (begin
                                      (let ((zz
                                             (let ((| self|
                                                    (lambda (args)
                                                      (if (not
                                                           (ar-false?
                                                            (_no (_cdr args))))
                                                        (_car args)
                                                        `(if (is
                                                              ,var
                                                              ',(_car args))
                                                           ,(_cadr args)
                                                           ,(ar-call-resolve-notation
                                                             self
                                                             (_cddr args)))))))
                                               | self|)))
                                        (set! self zz)
                                        zz)))))
                             | ex|)
                           'nil)))))
                  | caselet|))))
          (namespace-set-variable-value! '_caselet zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . args) 'case)
   (_sref _help* 'nil 'case)
   (_sref _source-file* _current-load-file* 'case)
   (_sref
    _source*
    '(mac
      case
      (expr . args)
      (quasiquote
       (caselet
        (unquote (uniq . nil) . nil)
        (unquote expr . nil)
        (unquote-splicing args . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'case)
   ((lambda ()
      (if (not (ar-false? (_bound 'case)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'case (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| case|
                       (lambda (expr . args)
                         `(caselet ,(_uniq) ,expr ,@(ar-nil-terminate args)))))
                  | case|))))
          (namespace-set-variable-value! '_case zz)
          zz))))))


((lambda ()
   (_sref _sig '(x place . nil) 'push)
   (_sref _help* 'nil 'push)
   (_sref _source-file* _current-load-file* 'push)
   (_sref
    _source*
    '(mac
      push
      (x place . nil)
      (w/uniq
       gx
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (let (unquote gx . nil)
            (unquote x . nil)
            (atwiths
             (unquote binds . nil)
             ((unquote setter . nil)
              (cons (unquote gx . nil) (unquote val . nil) . nil)
              .
              nil)
             .
             nil)
            .
            nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'push)
   ((lambda ()
      (if (not (ar-false? (_bound 'push)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'push (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| push|
                       (lambda (x place)
                         ((let ((| push|
                                 (lambda (gx)
                                   ((lambda g2492
                                      (let* ((binds (ar-xcar (car g2492)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2492))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2492))))))
                                        `(let ,gx
                                           ,x
                                           (atwiths
                                            ,binds
                                            (,setter (cons ,gx ,val))))))
                                    (_setforms place)))))
                            | push|)
                          (_uniq)))))
                  | push|))))
          (namespace-set-variable-value! '_push zz)
          zz))))))


((lambda ()
   (_sref _sig '(place1 place2 . nil) 'swap)
   (_sref _help* 'nil 'swap)
   (_sref _source-file* _current-load-file* 'swap)
   (_sref
    _source*
    '(mac
      swap
      (place1 place2 . nil)
      (w/uniq
       (g1 g2 . nil)
       (with
        ((binds1 val1 setter1 . nil)
         (setforms place1 . nil)
         (binds2 val2 setter2 . nil)
         (setforms place2 . nil)
         .
         nil)
        (quasiquote
         (atwiths
          (unquote
           (+ binds1 (list g1 val1 . nil) binds2 (list g2 val2 . nil) . nil)
           .
           nil)
          ((unquote setter1 . nil) (unquote g2 . nil) . nil)
          ((unquote setter2 . nil) (unquote g1 . nil) . nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'swap)
   ((lambda ()
      (if (not (ar-false? (_bound 'swap)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'swap (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| swap|
                       (lambda (place1 place2)
                         ((let ((| swap|
                                 (lambda (g1 g2)
                                   ((lambda g2493
                                      (let* ((binds1 (ar-xcar (car g2493)))
                                             (val1
                                              (ar-xcar (ar-xcdr (car g2493))))
                                             (setter1
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2493)))))
                                             (binds2
                                              (ar-xcar (car (ar-xcdr g2493))))
                                             (val2
                                              (ar-xcar
                                               (ar-xcdr
                                                (car (ar-xcdr g2493)))))
                                             (setter2
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr
                                                 (car (ar-xcdr g2493)))))))
                                        `(atwiths
                                          ,(_+
                                            binds1
                                            (_list g1 val1)
                                            binds2
                                            (_list g2 val2))
                                          (,setter1 ,g2)
                                          (,setter2 ,g1))))
                                    (_setforms place1)
                                    (_setforms place2)))))
                            | swap|)
                          (_uniq)
                          (_uniq)))))
                  | swap|))))
          (namespace-set-variable-value! '_swap zz)
          zz))))))


((lambda ()
   (_sref _sig 'places 'rotate)
   (_sref _help* 'nil 'rotate)
   (_sref _source-file* _current-load-file* 'rotate)
   (_sref
    _source*
    '(mac
      rotate
      places
      (with
       (vars
        (map (make-br-fn (uniq . nil) . nil) places . nil)
        forms
        (map setforms places . nil)
        .
        nil)
       (quasiquote
        (atwiths
         (unquote
          (mappend
           (fn
            (g (binds val setter . nil) . nil)
            (+ binds (list g val . nil) . nil)
            .
            nil)
           vars
           forms
           .
           nil)
          .
          nil)
         (unquote-splicing
          (map
           (fn (g (binds val setter . nil) . nil) (list setter g . nil) . nil)
           (+ (cdr vars . nil) (list (car vars . nil) . nil) . nil)
           forms
           .
           nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'rotate)
   ((lambda ()
      (if (not (ar-false? (_bound 'rotate)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rotate (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| rotate|
                       (lambda places
                         ((let ((| rotate|
                                 (lambda (vars forms)
                                   `(atwiths
                                     ,(_mappend
                                       (lambda g2494
                                         (let* ((g (car g2494))
                                                (binds
                                                 (ar-xcar
                                                  (car (ar-xcdr g2494))))
                                                (val
                                                 (ar-xcar
                                                  (ar-xcdr
                                                   (car (ar-xcdr g2494)))))
                                                (setter
                                                 (ar-xcar
                                                  (ar-xcdr
                                                   (ar-xcdr
                                                    (car (ar-xcdr g2494)))))))
                                           (_+ binds (_list g val))))
                                       vars
                                       forms)
                                     ,@(ar-nil-terminate
                                        (_map
                                         (lambda g2495
                                           (let* ((g (car g2495))
                                                  (binds
                                                   (ar-xcar
                                                    (car (ar-xcdr g2495))))
                                                  (val
                                                   (ar-xcar
                                                    (ar-xcdr
                                                     (car (ar-xcdr g2495)))))
                                                  (setter
                                                   (ar-xcar
                                                    (ar-xcdr
                                                     (ar-xcdr
                                                      (car
                                                       (ar-xcdr g2495)))))))
                                             (_list setter g)))
                                         (_+ (_cdr vars) (_list (_car vars)))
                                         forms))))))
                            | rotate|)
                          (_map
                           (let ((| vars| (lambda () (_uniq)))) | vars|)
                           places)
                          (_map _setforms places)))))
                  | rotate|))))
          (namespace-set-variable-value! '_rotate zz)
          zz))))))


((lambda ()
   (_sref _sig '(place . nil) 'pop)
   (_sref _help* 'nil 'pop)
   (_sref _source-file* _current-load-file* 'pop)
   (_sref
    _source*
    '(mac
      pop
      (place . nil)
      (w/uniq
       g
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (atwiths
           (unquote (+ binds (list g val . nil) . nil) . nil)
           (do1
            (car (unquote g . nil) . nil)
            ((unquote setter . nil) (cdr (unquote g . nil) . nil) . nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'pop)
   ((lambda ()
      (if (not (ar-false? (_bound 'pop)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'pop (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| pop|
                       (lambda (place)
                         ((let ((| pop|
                                 (lambda (g)
                                   ((lambda g2496
                                      (let* ((binds (ar-xcar (car g2496)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2496))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2496))))))
                                        `(atwiths
                                          ,(_+ binds (_list g val))
                                          (do1 (car ,g) (,setter (cdr ,g))))))
                                    (_setforms place)))))
                            | pop|)
                          (_uniq)))))
                  | pop|))))
          (namespace-set-variable-value! '_pop zz)
          zz))))))


((lambda ()
   (_sref _sig '(x xs (o test iso . nil) . nil) 'adjoin)
   (_sref _help* 'nil 'adjoin)
   (_sref _source-file* _current-load-file* 'adjoin)
   (_sref
    _source*
    '(def
      adjoin
      (x xs (o test iso . nil) . nil)
      (if (some (make-br-fn (test x _ . nil) . nil) xs . nil)
        xs
        (cons x xs . nil)
        .
        nil)
      .
      nil)
    'adjoin)
   ((lambda ()
      (if (not (ar-false? (_bound 'adjoin)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'adjoin (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2497
                 (let* ((x (car g2497))
                        (xs (car (ar-xcdr g2497)))
                        (test
                         (if (pair? (ar-xcdr (ar-xcdr g2497)))
                           (car (ar-xcdr (ar-xcdr g2497)))
                           _iso)))
                   (if (not
                        (ar-false?
                         (_some
                          (let ((| adjoin|
                                 (lambda ()
                                   (ar-call-resolve-notation test x __))))
                            | adjoin|)
                          xs)))
                     xs
                     (_cons x xs))))))
          (namespace-set-variable-value! '_adjoin zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'adjoin)))
     (_setinfixop 'adjoin ((ar-coerce _infixop 'fn) 'adjoin))
     'nil)))


((lambda ()
   (_sref _sig '(x place . args) 'pushnew)
   (_sref _help* 'nil 'pushnew)
   (_sref _source-file* _current-load-file* 'pushnew)
   (_sref
    _source*
    '(mac
      pushnew
      (x place . args)
      (w/uniq
       gx
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (atwiths
           (unquote (+ (list gx x . nil) binds . nil) . nil)
           ((unquote setter . nil)
            (adjoin
             (unquote gx . nil)
             (unquote val . nil)
             (unquote-splicing args . nil)
             .
             nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'pushnew)
   ((lambda ()
      (if (not (ar-false? (_bound 'pushnew)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'pushnew (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| pushnew|
                       (lambda (x place . args)
                         ((let ((| pushnew|
                                 (lambda (gx)
                                   ((lambda g2498
                                      (let* ((binds (ar-xcar (car g2498)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2498))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2498))))))
                                        `(atwiths
                                          ,(_+ (_list gx x) binds)
                                          (,setter
                                           (adjoin
                                            ,gx
                                            ,val
                                            ,@(ar-nil-terminate args))))))
                                    (_setforms place)))))
                            | pushnew|)
                          (_uniq)))))
                  | pushnew|))))
          (namespace-set-variable-value! '_pushnew zz)
          zz))))))


((lambda ()
   (_sref _sig '(test place . nil) 'pull)
   (_sref _help* 'nil 'pull)
   (_sref _source-file* _current-load-file* 'pull)
   (_sref
    _source*
    '(mac
      pull
      (test place . nil)
      (w/uniq
       g
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (atwiths
           (unquote (+ (list g test . nil) binds . nil) . nil)
           ((unquote setter . nil)
            (rem (unquote g . nil) (unquote val . nil) . nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'pull)
   ((lambda ()
      (if (not (ar-false? (_bound 'pull)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'pull (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| pull|
                       (lambda (test place)
                         ((let ((| pull|
                                 (lambda (g)
                                   ((lambda g2499
                                      (let* ((binds (ar-xcar (car g2499)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2499))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2499))))))
                                        `(atwiths
                                          ,(_+ (_list g test) binds)
                                          (,setter (rem ,g ,val)))))
                                    (_setforms place)))))
                            | pull|)
                          (_uniq)))))
                  | pull|))))
          (namespace-set-variable-value! '_pull zz)
          zz))))))


((lambda ()
   (_sref _sig '(x place . args) 'togglemem)
   (_sref _help* 'nil 'togglemem)
   (_sref _source-file* _current-load-file* 'togglemem)
   (_sref
    _source*
    '(mac
      togglemem
      (x place . args)
      (w/uniq
       gx
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (atwiths
           (unquote (+ (list gx x . nil) binds . nil) . nil)
           ((unquote setter . nil)
            (if (mem (unquote gx . nil) (unquote val . nil) . nil)
              (rem (unquote gx . nil) (unquote val . nil) . nil)
              (adjoin
               (unquote gx . nil)
               (unquote val . nil)
               (unquote-splicing args . nil)
               .
               nil)
              .
              nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'togglemem)
   ((lambda ()
      (if (not (ar-false? (_bound 'togglemem)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'togglemem (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| togglemem|
                       (lambda (x place . args)
                         ((let ((| togglemem|
                                 (lambda (gx)
                                   ((lambda g2500
                                      (let* ((binds (ar-xcar (car g2500)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2500))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2500))))))
                                        `(atwiths
                                          ,(_+ (_list gx x) binds)
                                          (,setter
                                           (if (mem ,gx ,val)
                                             (rem ,gx ,val)
                                             (adjoin
                                              ,gx
                                              ,val
                                              ,@(ar-nil-terminate args)))))))
                                    (_setforms place)))))
                            | togglemem|)
                          (_uniq)))))
                  | togglemem|))))
          (namespace-set-variable-value! '_togglemem zz)
          zz))))))


((lambda ()
   (_sref _sig '(place (o i 1 . nil) . nil) '++)
   (_sref _help* 'nil '++)
   (_sref _source-file* _current-load-file* '++)
   (_sref
    _source*
    '(mac
      ++
      (place (o i 1 . nil) . nil)
      (if (isa place (quote sym . nil) . nil)
        (quasiquote
         (=
          (unquote place . nil)
          (+ (unquote place . nil) (unquote i . nil) . nil)
          .
          nil)
         .
         nil)
        (w/uniq
         gi
         (let (binds val setter . nil)
           (setforms place . nil)
           (quasiquote
            (atwiths
             (unquote (+ binds (list gi i . nil) . nil) . nil)
             ((unquote setter . nil)
              (+ (unquote val . nil) (unquote gi . nil) . nil)
              .
              nil)
             .
             nil)
            .
            nil)
           .
           nil)
         .
         nil)
        .
        nil)
      .
      nil)
    '++)
   ((lambda ()
      (if (not (ar-false? (_bound '++)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp '++ (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (lambda g2501
                  (let* ((place (car g2501))
                         (i
                          (if (pair? (ar-xcdr g2501))
                            (car (ar-xcdr g2501))
                            1)))
                    (if (not (ar-false? (_isa place 'sym)))
                      `(= ,place (+ ,place ,i))
                      ((let ((| ++|
                              (lambda (gi)
                                ((lambda g2502
                                   (let* ((binds (ar-xcar (car g2502)))
                                          (val (ar-xcar (ar-xcdr (car g2502))))
                                          (setter
                                           (ar-xcar
                                            (ar-xcdr (ar-xcdr (car g2502))))))
                                     `(atwiths
                                       ,(_+ binds (_list gi i))
                                       (,setter (+ ,val ,gi)))))
                                 (_setforms place)))))
                         | ++|)
                       (_uniq))))))))
          (namespace-set-variable-value! '_++ zz)
          zz))))))


((lambda ()
   (_sref _sig '(place (o i 1 . nil) . nil) '--)
   (_sref _help* 'nil '--)
   (_sref _source-file* _current-load-file* '--)
   (_sref
    _source*
    '(mac
      --
      (place (o i 1 . nil) . nil)
      (if (isa place (quote sym . nil) . nil)
        (quasiquote
         (=
          (unquote place . nil)
          (- (unquote place . nil) (unquote i . nil) . nil)
          .
          nil)
         .
         nil)
        (w/uniq
         gi
         (let (binds val setter . nil)
           (setforms place . nil)
           (quasiquote
            (atwiths
             (unquote (+ binds (list gi i . nil) . nil) . nil)
             ((unquote setter . nil)
              (- (unquote val . nil) (unquote gi . nil) . nil)
              .
              nil)
             .
             nil)
            .
            nil)
           .
           nil)
         .
         nil)
        .
        nil)
      .
      nil)
    '--)
   ((lambda ()
      (if (not (ar-false? (_bound '--)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp '-- (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (lambda g2503
                  (let* ((place (car g2503))
                         (i
                          (if (pair? (ar-xcdr g2503))
                            (car (ar-xcdr g2503))
                            1)))
                    (if (not (ar-false? (_isa place 'sym)))
                      `(= ,place (- ,place ,i))
                      ((let ((| --|
                              (lambda (gi)
                                ((lambda g2504
                                   (let* ((binds (ar-xcar (car g2504)))
                                          (val (ar-xcar (ar-xcdr (car g2504))))
                                          (setter
                                           (ar-xcar
                                            (ar-xcdr (ar-xcdr (car g2504))))))
                                     `(atwiths
                                       ,(_+ binds (_list gi i))
                                       (,setter (- ,val ,gi)))))
                                 (_setforms place)))))
                         | --|)
                       (_uniq))))))))
          (namespace-set-variable-value! '_-- zz)
          zz))))))


((lambda ()
   (_sref _sig '(op place . args) 'zap)
   (_sref _help* 'nil 'zap)
   (_sref _source-file* _current-load-file* 'zap)
   (_sref
    _source*
    '(mac
      zap
      (op place . args)
      (with
       (gop
        (uniq . nil)
        gargs
        (map (make-br-fn (uniq . nil) . nil) args . nil)
        mix
        (afn
         seqs
         (if (some no seqs . nil)
           nil
           (+
            (map car seqs . nil)
            (apply self (map cdr seqs . nil) . nil)
            .
            nil)
           .
           nil)
         .
         nil)
        .
        nil)
       (let (binds val setter . nil)
         (setforms place . nil)
         (quasiquote
          (atwiths
           (unquote
            (+ binds (list gop op . nil) (mix gargs args . nil) . nil)
            .
            nil)
           ((unquote setter . nil)
            ((unquote gop . nil)
             (unquote val . nil)
             (unquote-splicing gargs . nil)
             .
             nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'zap)
   ((lambda ()
      (if (not (ar-false? (_bound 'zap)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'zap (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| zap|
                       (lambda (op place . args)
                         ((let ((| zap|
                                 (lambda (gop gargs mix)
                                   ((lambda g2505
                                      (let* ((binds (ar-xcar (car g2505)))
                                             (val
                                              (ar-xcar (ar-xcdr (car g2505))))
                                             (setter
                                              (ar-xcar
                                               (ar-xcdr
                                                (ar-xcdr (car g2505))))))
                                        `(atwiths
                                          ,(_+
                                            binds
                                            (_list gop op)
                                            (ar-call-resolve-notation
                                             mix
                                             gargs
                                             args))
                                          (,setter
                                           (,gop
                                            ,val
                                            ,@(ar-nil-terminate gargs))))))
                                    (_setforms place)))))
                            | zap|)
                          (_uniq)
                          (_map
                           (let ((| gargs| (lambda () (_uniq)))) | gargs|)
                           args)
                          ((let ((| mix|
                                  (lambda (self)
                                    (begin
                                      (let ((zz
                                             (let ((| self|
                                                    (lambda seqs
                                                      (if (not
                                                           (ar-false?
                                                            (_some _no seqs)))
                                                        'nil
                                                        (_+
                                                         (_map _car seqs)
                                                         (_apply
                                                          self
                                                          (_map
                                                           _cdr
                                                           seqs)))))))
                                               | self|)))
                                        (set! self zz)
                                        zz)))))
                             | mix|)
                           'nil)))))
                  | zap|))))
          (namespace-set-variable-value! '_zap zz)
          zz))))))


((lambda ()
   (_sref _sig 'args 'pr)
   (_sref _help* 'nil 'pr)
   (_sref _source-file* _current-load-file* 'pr)
   (_sref
    _source*
    '(def pr args (map1 disp args . nil) (car args . nil) . nil)
    'pr)
   ((lambda ()
      (if (not (ar-false? (_bound 'pr)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'pr (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| pr| (lambda args (_map1 _disp args) (_car args))))
                 | pr|)))
          (namespace-set-variable-value! '_pr zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'pr)))
     (_setinfixop 'pr ((ar-coerce _infixop 'fn) 'pr))
     'nil)))


((lambda ()
   (_sref _sig 'args 'prt)
   (_sref _help* 'nil 'prt)
   (_sref _source-file* _current-load-file* 'prt)
   (_sref
    _source*
    '(def
      prt
      args
      (map1 (make-br-fn (if _ (disp _ . nil) . nil) . nil) args . nil)
      (car args . nil)
      .
      nil)
    'prt)
   ((lambda ()
      (if (not (ar-false? (_bound 'prt)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'prt (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| prt|
                      (lambda args
                        (_map1
                         (let ((| prt|
                                (lambda ()
                                  (if (not (ar-false? __)) (_disp __) 'nil))))
                           | prt|)
                         args)
                        (_car args))))
                 | prt|)))
          (namespace-set-variable-value! '_prt zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'prt)))
     (_setinfixop 'prt ((ar-coerce _infixop 'fn) 'prt))
     'nil)))


((lambda ()
   (_sref _sig 'args 'prn)
   (_sref _help* 'nil 'prn)
   (_sref _source-file* _current-load-file* 'prn)
   (_sref
    _source*
    '(def
      prn
      args
      (do1 (apply pr args . nil) (pr #\newline . nil) . nil)
      .
      nil)
    'prn)
   ((lambda ()
      (if (not (ar-false? (_bound 'prn)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'prn (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| prn|
                      (lambda args
                        ((let ((| prn| (lambda (g2506) (_pr #\newline) g2506)))
                           | prn|)
                         (_apply _pr args)))))
                 | prn|)))
          (namespace-set-variable-value! '_prn zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'prn)))
     (_setinfixop 'prn ((ar-coerce _infixop 'fn) 'prn))
     'nil)))


((lambda ()
   (_sref _sig 'args 'wipe)
   (_sref _help* 'nil 'wipe)
   (_sref _source-file* _current-load-file* 'wipe)
   (_sref
    _source*
    '(mac
      wipe
      args
      (quasiquote
       (do
        (unquote-splicing
         (map
          (fn
           (a . nil)
           (quasiquote (= (unquote a . nil) nil . nil) . nil)
           .
           nil)
          args
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'wipe)
   ((lambda ()
      (if (not (ar-false? (_bound 'wipe)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'wipe (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| wipe|
                       (lambda args
                         `(do
                           ,@(ar-nil-terminate
                              (_map
                               (let ((| wipe| (lambda (a) `(= ,a nil))))
                                 | wipe|)
                               args))))))
                  | wipe|))))
          (namespace-set-variable-value! '_wipe zz)
          zz))))))


((lambda ()
   (_sref _sig 'args 'set)
   (_sref _help* 'nil 'set)
   (_sref _source-file* _current-load-file* 'set)
   (_sref
    _source*
    '(mac
      set
      args
      (quasiquote
       (do
        (unquote-splicing
         (map
          (fn (a . nil) (quasiquote (= (unquote a . nil) t . nil) . nil) . nil)
          args
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'set)
   ((lambda ()
      (if (not (ar-false? (_bound 'set)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'set (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| set|
                       (lambda args
                         `(do
                           ,@(ar-nil-terminate
                              (_map
                               (let ((| set| (lambda (a) `(= ,a t)))) | set|)
                               args))))))
                  | set|))))
          (namespace-set-variable-value! '_set zz)
          zz))))))


((lambda ()
   (_sref _sig '(var expr then . rest) 'iflet)
   (_sref _help* 'nil 'iflet)
   (_sref _source-file* _current-load-file* 'iflet)
   (_sref
    _source*
    '(mac
      iflet
      (var expr then . rest)
      (w/uniq
       gv
       (quasiquote
        (let (unquote gv . nil)
          (unquote expr . nil)
          (if (unquote gv . nil)
            (let (unquote var . nil)
              (unquote gv . nil)
              (unquote then . nil)
              .
              nil)
            (unquote-splicing rest . nil)
            .
            nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'iflet)
   ((lambda ()
      (if (not (ar-false? (_bound 'iflet)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'iflet (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| iflet|
                       (lambda (var expr then . rest)
                         ((let ((| iflet|
                                 (lambda (gv)
                                   `(let ,gv
                                      ,expr
                                      (if ,gv
                                        (let ,var ,gv ,then)
                                        ,@(ar-nil-terminate rest))))))
                            | iflet|)
                          (_uniq)))))
                  | iflet|))))
          (namespace-set-variable-value! '_iflet zz)
          zz))))))


((lambda ()
   (_sref _sig '(var expr . body) 'whenlet)
   (_sref _help* 'nil 'whenlet)
   (_sref _source-file* _current-load-file* 'whenlet)
   (_sref
    _source*
    '(mac
      whenlet
      (var expr . body)
      (quasiquote
       (iflet
        (unquote var . nil)
        (unquote expr . nil)
        (do (unquote-splicing body . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'whenlet)
   ((lambda ()
      (if (not (ar-false? (_bound 'whenlet)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'whenlet (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| whenlet|
                       (lambda (var expr . body)
                         `(iflet ,var ,expr (do ,@(ar-nil-terminate body))))))
                  | whenlet|))))
          (namespace-set-variable-value! '_whenlet zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . body) 'aif)
   (_sref _help* 'nil 'aif)
   (_sref _source-file* _current-load-file* 'aif)
   (_sref
    _source*
    '(mac
      aif
      (expr . body)
      (quasiquote
       (let it (unquote expr . nil)
         (if it
           (unquote-splicing
            (if (cddr body . nil)
              (quasiquote
               ((unquote (car body . nil) . nil)
                (aif (unquote-splicing (cdr body . nil) . nil) . nil)
                .
                nil)
               .
               nil)
              body
              .
              nil)
            .
            nil)
           .
           nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'aif)
   ((lambda ()
      (if (not (ar-false? (_bound 'aif)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'aif (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| aif|
                       (lambda (expr . body)
                         `(let it ,expr
                            (if it
                              ,@(ar-nil-terminate
                                 (if (not (ar-false? (_cddr body)))
                                   `(,(_car body)
                                     (aif ,@(ar-nil-terminate (_cdr body))))
                                   body)))))))
                  | aif|))))
          (namespace-set-variable-value! '_aif zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . body) 'awhen)
   (_sref _help* 'nil 'awhen)
   (_sref _source-file* _current-load-file* 'awhen)
   (_sref
    _source*
    '(mac
      awhen
      (expr . body)
      (quasiquote
       (let it (unquote expr . nil)
         (if it (do (unquote-splicing body . nil) . nil) . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'awhen)
   ((lambda ()
      (if (not (ar-false? (_bound 'awhen)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'awhen (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| awhen|
                       (lambda (expr . body)
                         `(let it ,expr
                            (if it (do ,@(ar-nil-terminate body)))))))
                  | awhen|))))
          (namespace-set-variable-value! '_awhen zz)
          zz))))))


((lambda ()
   (_sref _sig 'args 'aand)
   (_sref _help* 'nil 'aand)
   (_sref _source-file* _current-load-file* 'aand)
   (_sref
    _source*
    '(mac
      aand
      args
      (if (no args . nil)
        (quote t . nil)
        (no (cdr args . nil) . nil)
        (car args . nil)
        (quasiquote
         (let it (unquote (car args . nil) . nil)
           (and it
                (aand (unquote-splicing (cdr args . nil) . nil) . nil)
                .
                nil)
           .
           nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'aand)
   ((lambda ()
      (if (not (ar-false? (_bound 'aand)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'aand (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| aand|
                       (lambda args
                         (if (not (ar-false? (_no args)))
                           't
                           (if (not (ar-false? (_no (_cdr args))))
                             (_car args)
                             `(let it ,(_car args)
                                (and it
                                     (aand
                                      ,@(ar-nil-terminate (_cdr args))))))))))
                  | aand|))))
          (namespace-set-variable-value! '_aand zz)
          zz))))))


((lambda ()
   (_sref _sig '(accfn . body) 'accum)
   (_sref _help* 'nil 'accum)
   (_sref _source-file* _current-load-file* 'accum)
   (_sref
    _source*
    '(mac
      accum
      (accfn . body)
      (w/uniq
       gacc
       (quasiquote
        (withs
         ((unquote gacc . nil)
          nil
          (unquote accfn . nil)
          (make-br-fn (push _ (unquote gacc . nil) . nil) . nil)
          .
          nil)
         (unquote-splicing body . nil)
         (rev (unquote gacc . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'accum)
   ((lambda ()
      (if (not (ar-false? (_bound 'accum)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'accum (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| accum|
                       (lambda (accfn . body)
                         ((let ((| accum|
                                 (lambda (gacc)
                                   `(withs
                                     (,gacc
                                      nil
                                      ,accfn
                                      (make-br-fn (push _ ,gacc)))
                                     ,@(ar-nil-terminate body)
                                     (rev ,gacc)))))
                            | accum|)
                          (_uniq)))))
                  | accum|))))
          (namespace-set-variable-value! '_accum zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr (o eof nil . nil) . nil) 'drain)
   (_sref _help* 'nil 'drain)
   (_sref _source-file* _current-load-file* 'drain)
   (_sref
    _source*
    '(mac
      drain
      (expr (o eof nil . nil) . nil)
      (w/uniq
       (gacc gdone gres . nil)
       (quasiquote
        (with
         ((unquote gacc . nil) nil (unquote gdone . nil) nil . nil)
         (while
          (no (unquote gdone . nil) . nil)
          (let (unquote gres . nil)
            (unquote expr . nil)
            (if (is (unquote gres . nil) (unquote eof . nil) . nil)
              (= (unquote gdone . nil) t . nil)
              (push (unquote gres . nil) (unquote gacc . nil) . nil)
              .
              nil)
            .
            nil)
          .
          nil)
         (rev (unquote gacc . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'drain)
   ((lambda ()
      (if (not (ar-false? (_bound 'drain)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'drain (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (lambda g2507
                  (let* ((expr (car g2507))
                         (eof
                          (if (pair? (ar-xcdr g2507))
                            (car (ar-xcdr g2507))
                            'nil)))
                    ((let ((| drain|
                            (lambda (gacc gdone gres)
                              `(with
                                (,gacc nil ,gdone nil)
                                (while
                                 (no ,gdone)
                                 (let ,gres
                                   ,expr
                                   (if (is ,gres ,eof)
                                     (= ,gdone t)
                                     (push ,gres ,gacc))))
                                (rev ,gacc)))))
                       | drain|)
                     (_uniq)
                     (_uniq)
                     (_uniq)))))))
          (namespace-set-variable-value! '_drain zz)
          zz))))))


((lambda ()
   (_sref _sig '(var expr endval . body) 'whiler)
   (_sref _help* 'nil 'whiler)
   (_sref _source-file* _current-load-file* 'whiler)
   (_sref
    _source*
    '(mac
      whiler
      (var expr endval . body)
      (w/uniq
       gf
       (quasiquote
        (withs
         ((unquote var . nil)
          nil
          (unquote gf . nil)
          (testify (unquote endval . nil) . nil)
          .
          nil)
         (while
          (no
           ((unquote gf . nil)
            (= (unquote var . nil) (unquote expr . nil) . nil)
            .
            nil)
           .
           nil)
          (unquote-splicing body . nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'whiler)
   ((lambda ()
      (if (not (ar-false? (_bound 'whiler)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'whiler (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| whiler|
                       (lambda (var expr endval . body)
                         ((let ((| whiler|
                                 (lambda (gf)
                                   `(withs
                                     (,var nil ,gf (testify ,endval))
                                     (while
                                      (no (,gf (= ,var ,expr)))
                                      ,@(ar-nil-terminate body))))))
                            | whiler|)
                          (_uniq)))))
                  | whiler|))))
          (namespace-set-variable-value! '_whiler zz)
          zz))))))


((lambda ()
   (_sref _sig '(x y . nil) 'consif)
   (_sref _help* 'nil 'consif)
   (_sref _source-file* _current-load-file* 'consif)
   (_sref
    _source*
    '(def consif (x y . nil) (if x (cons x y . nil) y . nil) . nil)
    'consif)
   ((lambda ()
      (if (not (ar-false? (_bound 'consif)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'consif (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| consif|
                      (lambda (x y) (if (not (ar-false? x)) (_cons x y) y))))
                 | consif|)))
          (namespace-set-variable-value! '_consif zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'consif)))
     (_setinfixop 'consif ((ar-coerce _infixop 'fn) 'consif))
     'nil)))


((lambda ()
   (_sref _sig 'args 'string)
   (_sref _help* 'nil 'string)
   (_sref _source-file* _current-load-file* 'string)
   (_sref
    _source*
    '(def
      string
      args
      (apply
       +
       ""
       (map
        (make-br-fn (coerce _ (quote string . nil) . nil) . nil)
        args
        .
        nil)
       .
       nil)
      .
      nil)
    'string)
   ((lambda ()
      (if (not (ar-false? (_bound 'string)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'string (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| string|
                      (lambda args
                        (_apply
                         _+
                         ""
                         (_map
                          (let ((| string| (lambda () (_coerce __ 'string))))
                            | string|)
                          args)))))
                 | string|)))
          (namespace-set-variable-value! '_string zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'string)))
     (_setinfixop 'string ((ar-coerce _infixop 'fn) 'string))
     'nil)))


((lambda ()
   (_sref _sig 'x 'flat)
   (_sref _help* 'nil 'flat)
   (_sref _source-file* _current-load-file* 'flat)
   (_sref
    _source*
    '(def
      flat
      x
      ((afn
        (x acc . nil)
        (if (no x . nil)
          acc
          (atom x . nil)
          (cons x acc . nil)
          (self (car x . nil) (self (cdr x . nil) acc . nil) . nil)
          .
          nil)
        .
        nil)
       x
       nil
       .
       nil)
      .
      nil)
    'flat)
   ((lambda ()
      (if (not (ar-false? (_bound 'flat)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'flat (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| flat|
                      (lambda x
                        (ar-call-resolve-notation
                         ((let ((| flat|
                                 (lambda (self)
                                   (begin
                                     (let ((zz
                                            (let ((| self|
                                                   (lambda (x acc)
                                                     (if (not
                                                          (ar-false? (_no x)))
                                                       acc
                                                       (if (not
                                                            (ar-false?
                                                             (_atom x)))
                                                         (_cons x acc)
                                                         (ar-call-resolve-notation
                                                          self
                                                          (_car x)
                                                          (ar-call-resolve-notation
                                                           self
                                                           (_cdr x)
                                                           acc)))))))
                                              | self|)))
                                       (set! self zz)
                                       zz)))))
                            | flat|)
                          'nil)
                         x
                         'nil))))
                 | flat|)))
          (namespace-set-variable-value! '_flat zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'flat)))
     (_setinfixop 'flat ((ar-coerce _infixop 'fn) 'flat))
     'nil)))


((lambda ()
   (_sref _sig '(x test (o alt . nil) . nil) 'check)
   (_sref _help* 'nil 'check)
   (_sref _source-file* _current-load-file* 'check)
   (_sref
    _source*
    '(mac
      check
      (x test (o alt . nil) . nil)
      (w/uniq
       gx
       (quasiquote
        (let (unquote gx . nil)
          (unquote x . nil)
          (if ((unquote test . nil) (unquote gx . nil) . nil)
            (unquote gx . nil)
            (unquote alt . nil)
            .
            nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'check)
   ((lambda ()
      (if (not (ar-false? (_bound 'check)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'check (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (lambda g2508
                  (let* ((x (car g2508))
                         (test (car (ar-xcdr g2508)))
                         (alt
                          (if (pair? (ar-xcdr (ar-xcdr g2508)))
                            (car (ar-xcdr (ar-xcdr g2508)))
                            'nil)))
                    ((let ((| check|
                            (lambda (gx)
                              `(let ,gx ,x (if (,test ,gx) ,gx ,alt)))))
                       | check|)
                     (_uniq)))))))
          (namespace-set-variable-value! '_check zz)
          zz))))))


((lambda ()
   (_sref _sig '(test seq (o start 0 . nil) . nil) 'pos)
   (_sref _help* 'nil 'pos)
   (_sref _source-file* _current-load-file* 'pos)
   (_sref
    _source*
    '(def
      pos
      (test seq (o start 0 . nil) . nil)
      (let f (testify test . nil)
        (if (alist seq . nil)
          ((afn
            (seq n . nil)
            (if (no seq . nil)
              nil
              (f (car seq . nil) . nil)
              n
              (self (cdr seq . nil) (+ n 1 . nil) . nil)
              .
              nil)
            .
            nil)
           (nthcdr start seq . nil)
           start
           .
           nil)
          (recstring
           (make-br-fn (if (f (seq _ . nil) . nil) _ . nil) . nil)
           seq
           start
           .
           nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'pos)
   ((lambda ()
      (if (not (ar-false? (_bound 'pos)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'pos (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2509
                 (let* ((test (car g2509))
                        (seq (car (ar-xcdr g2509)))
                        (start
                         (if (pair? (ar-xcdr (ar-xcdr g2509)))
                           (car (ar-xcdr (ar-xcdr g2509)))
                           0)))
                   ((let ((| pos|
                           (lambda (f)
                             (if (not (ar-false? (_alist seq)))
                               (ar-call-resolve-notation
                                ((let ((| pos|
                                        (lambda (self)
                                          (begin
                                            (let ((zz
                                                   (let ((| self|
                                                          (lambda (seq n)
                                                            (if (not
                                                                 (ar-false?
                                                                  (_no seq)))
                                                              'nil
                                                              (if (not
                                                                   (ar-false?
                                                                    (ar-call-resolve-notation
                                                                     f
                                                                     (_car
                                                                      seq))))
                                                                n
                                                                (ar-call-resolve-notation
                                                                 self
                                                                 (_cdr seq)
                                                                 (_+ n 1)))))))
                                                     | self|)))
                                              (set! self zz)
                                              zz)))))
                                   | pos|)
                                 'nil)
                                (_nthcdr start seq)
                                start)
                               (_recstring
                                (let ((| pos|
                                       (lambda ()
                                         (if (not
                                              (ar-false?
                                               (ar-call-resolve-notation
                                                f
                                                (ar-call-resolve-notation
                                                 seq
                                                 __))))
                                           __
                                           'nil))))
                                  | pos|)
                                seq
                                start)))))
                      | pos|)
                    (_testify test))))))
          (namespace-set-variable-value! '_pos zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'pos)))
     (_setinfixop 'pos ((ar-coerce _infixop 'fn) 'pos))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'even)
   (_sref _help* 'nil 'even)
   (_sref _source-file* _current-load-file* 'even)
   (_sref
    _source*
    '(def even (n . nil) (is (mod n 2 . nil) 0 . nil) . nil)
    'even)
   ((lambda ()
      (if (not (ar-false? (_bound 'even)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'even (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| even| (lambda (n) (_is (_mod n 2) 0)))) | even|)))
          (namespace-set-variable-value! '_even zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'even)))
     (_setinfixop 'even ((ar-coerce _infixop 'fn) 'even))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'odd)
   (_sref _help* 'nil 'odd)
   (_sref _source-file* _current-load-file* 'odd)
   (_sref _source* '(def odd (n . nil) (no (even n . nil) . nil) . nil) 'odd)
   ((lambda ()
      (if (not (ar-false? (_bound 'odd)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'odd (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| odd| (lambda (n) (_no (_even n))))) | odd|)))
          (namespace-set-variable-value! '_odd zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'odd)))
     (_setinfixop 'odd ((ar-coerce _infixop 'fn) 'odd))
     'nil)))


((lambda ()
   (_sref _sig '(x . ys) 'after)
   (_sref _help* 'nil 'after)
   (_sref _source-file* _current-load-file* 'after)
   (_sref
    _source*
    '(mac
      after
      (x . ys)
      (quasiquote
       (protect
        (fn nil (unquote x . nil) . nil)
        (fn nil (unquote-splicing ys . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'after)
   ((lambda ()
      (if (not (ar-false? (_bound 'after)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'after (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| after|
                       (lambda (x . ys)
                         `(protect
                           (fn nil ,x)
                           (fn nil ,@(ar-nil-terminate ys))))))
                  | after|))))
          (namespace-set-variable-value! '_after zz)
          zz))))))


((lambda ()
   (begin
     (let ((zz (_table)))
       (namespace-set-variable-value! '_declare-fns* zz)
       zz))))


((lambda ()
   ((lambda ()
      (_sref _sig '(old new args . nil) 'decl-idfn)
      (_sref _help* 'nil 'decl-idfn)
      (_sref _source-file* _current-load-file* 'decl-idfn)
      (_sref
       _source*
       '(def decl-idfn (old new args . nil) new . nil)
       'decl-idfn)
      ((lambda ()
         (if (not (ar-false? (_bound 'decl-idfn)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'decl-idfn (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (let ((| decl-idfn| (lambda (old new args) new)))
                    | decl-idfn|)))
             (namespace-set-variable-value! '_decl-idfn zz)
             zz))))
      (if (not (ar-false? ((ar-coerce _infixop 'fn) 'decl-idfn)))
        (_setinfixop 'decl-idfn ((ar-coerce _infixop 'fn) 'decl-idfn))
        'nil)))
   ((lambda ()
      (_sref _sig '(old new args . nil) 'decl-bool)
      (_sref _help* 'nil 'decl-bool)
      (_sref _source-file* _current-load-file* 'decl-bool)
      (_sref
       _source*
       '(def decl-bool (old new args . nil) (no:no new . nil) . nil)
       'decl-bool)
      ((lambda ()
         (if (not (ar-false? (_bound 'decl-bool)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'decl-bool (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (let ((| decl-bool| (lambda (old new args) (_no (_no new)))))
                    | decl-bool|)))
             (namespace-set-variable-value! '_decl-bool zz)
             zz))))
      (if (not (ar-false? ((ar-coerce _infixop 'fn) 'decl-bool)))
        (_setinfixop 'decl-bool ((ar-coerce _infixop 'fn) 'decl-bool))
        'nil)))))


((lambda ()
   (_sref
    _sig
    '(key (o setfn decl-idfn . nil) (o default . nil) . nil)
    'declaration)
   (_sref _help* 'nil 'declaration)
   (_sref _source-file* _current-load-file* 'declaration)
   (_sref
    _source*
    '(def
      declaration
      (key (o setfn decl-idfn . nil) (o default . nil) . nil)
      (= declare-fns*.key setfn declarations*.key default . nil)
      .
      nil)
    'declaration)
   ((lambda ()
      (if (not (ar-false? (_bound 'declaration)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'declaration (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2510
                 (let* ((key (car g2510))
                        (setfn
                         (if (pair? (ar-xcdr g2510))
                           (car (ar-xcdr g2510))
                           _decl-idfn))
                        (default
                         (if (pair? (ar-xcdr (ar-xcdr g2510)))
                           (car (ar-xcdr (ar-xcdr g2510)))
                           'nil)))
                   ((let ((| declaration|
                           (lambda ()
                             (_atomic-invoke
                              (let ((| declaration|
                                     (lambda ()
                                       ((let ((| declaration|
                                               (lambda (g2511 g2513 g2514)
                                                 ((let ((| declaration|
                                                         (lambda (g2512)
                                                           (_sref
                                                            g2511
                                                            g2512
                                                            g2513))))
                                                    | declaration|)
                                                  g2514))))
                                          | declaration|)
                                        _declare-fns*
                                        key
                                        setfn))))
                                | declaration|))
                             (_atomic-invoke
                              (let ((| declaration|
                                     (lambda ()
                                       ((let ((| declaration|
                                               (lambda (g2515 g2517 g2518)
                                                 ((let ((| declaration|
                                                         (lambda (g2516)
                                                           (_sref
                                                            g2515
                                                            g2516
                                                            g2517))))
                                                    | declaration|)
                                                  g2518))))
                                          | declaration|)
                                        _declarations*
                                        key
                                        default))))
                                | declaration|)))))
                      | declaration|))))))
          (namespace-set-variable-value! '_declaration zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'declaration)))
     (_setinfixop 'declaration ((ar-coerce _infixop 'fn) 'declaration))
     'nil)))


((lambda (mklist)
   ((lambda ()
      (_sref _sig '(key val . nil) 'declare)
      (_sref _help* 'nil 'declare)
      (_sref _source-file* _current-load-file* 'declare)
      (_sref
       _source*
       '(def
         declare
         (key val . nil)
         (let (k . args)
           mklist.key
           (iflet
            f
            declare-fns*.k
            (zap f declarations*.k val args . nil)
            declerr.key
            .
            nil)
           .
           nil)
         .
         nil)
       'declare)
      ((lambda ()
         (if (not (ar-false? (_bound 'declare)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'declare (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (let ((| declare|
                         (lambda (key val)
                           ((lambda g2519
                              (let* ((k (ar-xcar (car g2519)))
                                     (args (ar-xcdr (car g2519))))
                                ((let ((| declare|
                                        (lambda (g2520)
                                          (if (not (ar-false? g2520))
                                            ((let ((| declare|
                                                    (lambda (f)
                                                      (_atomic-invoke
                                                       (let ((| declare|
                                                              (lambda ()
                                                                ((let ((| declare|
                                                                        (lambda (g2524)
                                                                          ((let ((| declare|
                                                                                  (lambda (g2526)
                                                                                    ((let ((| declare|
                                                                                            (lambda (g2521)
                                                                                              ((let ((| declare|
                                                                                                      (lambda (g2522)
                                                                                                        ((let ((| declare|
                                                                                                                (lambda (g2523)
                                                                                                                  ((let ((| declare|
                                                                                                                          (lambda ()
                                                                                                                            ((let ((| declare|
                                                                                                                                    (lambda (g2525)
                                                                                                                                      (_sref
                                                                                                                                       g2524
                                                                                                                                       g2525
                                                                                                                                       g2526))))
                                                                                                                               | declare|)
                                                                                                                             (ar-call-resolve-notation
                                                                                                                              g2521
                                                                                                                              (ar-call-resolve-notation
                                                                                                                               g2524
                                                                                                                               g2526)
                                                                                                                              g2522
                                                                                                                              g2523)))))
                                                                                                                     | declare|)))))
                                                                                                           | declare|)
                                                                                                         args))))
                                                                                                 | declare|)
                                                                                               val))))
                                                                                       | declare|)
                                                                                     f))))
                                                                             | declare|)
                                                                           k))))
                                                                   | declare|)
                                                                 _declarations*))))
                                                         | declare|)))))
                                               | declare|)
                                             g2520)
                                            (_declerr key)))))
                                   | declare|)
                                 (ar-call-resolve-notation _declare-fns* k))))
                            (ar-call-resolve-notation mklist key)))))
                    | declare|)))
             (namespace-set-variable-value! '_declare zz)
             zz))))
      (if (not (ar-false? ((ar-coerce _infixop 'fn) 'declare)))
        (_setinfixop 'declare ((ar-coerce _infixop 'fn) 'declare))
        'nil))))
 (let ((| mklist|
        (lambda (x)
          ((let ((| mklist|
                  (lambda (g2527)
                    (if (not (ar-false? (_alist g2527))) g2527 (_list x)))))
             | mklist|)
           x))))
   | mklist|))


((lambda ()
   (_sref _sig '(key . nil) 'decl)
   (_sref _help* 'nil 'decl)
   (_sref _source-file* _current-load-file* 'decl)
   (_sref
    _source*
    '(def
      decl
      (key . nil)
      (if declare-fns*.key declarations*.key declerr.key . nil)
      .
      nil)
    'decl)
   ((lambda ()
      (if (not (ar-false? (_bound 'decl)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'decl (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| decl|
                      (lambda (key)
                        (if (not
                             (ar-false?
                              (ar-call-resolve-notation _declare-fns* key)))
                          (ar-call-resolve-notation _declarations* key)
                          (_declerr key)))))
                 | decl|)))
          (namespace-set-variable-value! '_decl zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'decl)))
     (_setinfixop 'decl ((ar-coerce _infixop 'fn) 'decl))
     'nil)))


((lambda ()
   (begin
     (let ((zz
            (let ((| declerr| (lambda () (_err "Unknown declaration: " __))))
              | declerr|)))
       (namespace-set-variable-value! '_declerr zz)
       zz))))


(_map
 (lambda () (_declaration __ _decl-bool))
 '(atstrings direct-calls explicit-flush uniform-notation . nil))


((lambda (expander)
   ((lambda ()
      (_sref _sig '(var name . body) 'w/infile)
      (_sref _help* 'nil 'w/infile)
      (_sref _source-file* _current-load-file* 'w/infile)
      (_sref
       _source*
       '(mac
         w/infile
         (var name . body)
         (expander (quote infile . nil) var name body . nil)
         .
         nil)
       'w/infile)
      ((lambda ()
         (if (not (ar-false? (_bound 'w/infile)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'w/infile (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (_annotate
                   'mac
                   (let ((| w/infile|
                          (lambda (var name . body)
                            (ar-call-resolve-notation
                             expander
                             'infile
                             var
                             name
                             body))))
                     | w/infile|))))
             (namespace-set-variable-value! '_w/infile zz)
             zz))))))
   ((lambda ()
      (_sref _sig '(var name . body) 'w/outfile)
      (_sref _help* 'nil 'w/outfile)
      (_sref _source-file* _current-load-file* 'w/outfile)
      (_sref
       _source*
       '(mac
         w/outfile
         (var name . body)
         (expander (quote outfile . nil) var name body . nil)
         .
         nil)
       'w/outfile)
      ((lambda ()
         (if (not (ar-false? (_bound 'w/outfile)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'w/outfile (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (_annotate
                   'mac
                   (let ((| w/outfile|
                          (lambda (var name . body)
                            (ar-call-resolve-notation
                             expander
                             'outfile
                             var
                             name
                             body))))
                     | w/outfile|))))
             (namespace-set-variable-value! '_w/outfile zz)
             zz))))))
   ((lambda ()
      (_sref _sig '(var str . body) 'w/instring)
      (_sref _help* 'nil 'w/instring)
      (_sref _source-file* _current-load-file* 'w/instring)
      (_sref
       _source*
       '(mac
         w/instring
         (var str . body)
         (expander (quote instring . nil) var str body . nil)
         .
         nil)
       'w/instring)
      ((lambda ()
         (if (not (ar-false? (_bound 'w/instring)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'w/instring (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (_annotate
                   'mac
                   (let ((| w/instring|
                          (lambda (var str . body)
                            (ar-call-resolve-notation
                             expander
                             'instring
                             var
                             str
                             body))))
                     | w/instring|))))
             (namespace-set-variable-value! '_w/instring zz)
             zz))))))
   ((lambda ()
      (_sref _sig '(var port . body) 'w/socket)
      (_sref _help* 'nil 'w/socket)
      (_sref _source-file* _current-load-file* 'w/socket)
      (_sref
       _source*
       '(mac
         w/socket
         (var port . body)
         (expander (quote open-socket . nil) var port body . nil)
         .
         nil)
       'w/socket)
      ((lambda ()
         (if (not (ar-false? (_bound 'w/socket)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'w/socket (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (_annotate
                   'mac
                   (let ((| w/socket|
                          (lambda (var port . body)
                            (ar-call-resolve-notation
                             expander
                             'open-socket
                             var
                             port
                             body))))
                     | w/socket|))))
             (namespace-set-variable-value! '_w/socket zz)
             zz)))))))
 (let ((| expander|
        (lambda (f var name body)
          `(let ,var
             (,f ,name)
             (after (do ,@(ar-nil-terminate body)) (close ,var))))))
   | expander|))


((lambda ()
   (_sref _sig '(var . body) 'w/outstring)
   (_sref _help* 'nil 'w/outstring)
   (_sref _source-file* _current-load-file* 'w/outstring)
   (_sref
    _source*
    '(mac
      w/outstring
      (var . body)
      (quasiquote
       (let (unquote var . nil)
         (outstring . nil)
         (unquote-splicing body . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'w/outstring)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/outstring)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/outstring (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/outstring|
                       (lambda (var . body)
                         `(let ,var (outstring) ,@(ar-nil-terminate body)))))
                  | w/outstring|))))
          (namespace-set-variable-value! '_w/outstring zz)
          zz))))))


((lambda ()
   (_sref _sig '(var name . body) 'w/appendfile)
   (_sref _help* 'nil 'w/appendfile)
   (_sref _source-file* _current-load-file* 'w/appendfile)
   (_sref
    _source*
    '(mac
      w/appendfile
      (var name . body)
      (quasiquote
       (let (unquote var . nil)
         (outfile (unquote name . nil) (quote append . nil) . nil)
         (after
          (do (unquote-splicing body . nil) . nil)
          (close (unquote var . nil) . nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'w/appendfile)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/appendfile)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/appendfile (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/appendfile|
                       (lambda (var name . body)
                         `(let ,var
                            (outfile ,name 'append)
                            (after
                             (do ,@(ar-nil-terminate body))
                             (close ,var))))))
                  | w/appendfile|))))
          (namespace-set-variable-value! '_w/appendfile zz)
          zz))))))


((lambda ()
   (_sref _sig '(str . body) 'w/stdout)
   (_sref _help* 'nil 'w/stdout)
   (_sref _source-file* _current-load-file* 'w/stdout)
   (_sref
    _source*
    '(mac
      w/stdout
      (str . body)
      (quasiquote
       (call-w/stdout
        (unquote str . nil)
        (fn nil (unquote-splicing body . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'w/stdout)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/stdout)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/stdout (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/stdout|
                       (lambda (str . body)
                         `(call-w/stdout
                           ,str
                           (fn nil ,@(ar-nil-terminate body))))))
                  | w/stdout|))))
          (namespace-set-variable-value! '_w/stdout zz)
          zz))))))


((lambda ()
   (_sref _sig '(str . body) 'w/stdin)
   (_sref _help* 'nil 'w/stdin)
   (_sref _source-file* _current-load-file* 'w/stdin)
   (_sref
    _source*
    '(mac
      w/stdin
      (str . body)
      (quasiquote
       (call-w/stdin
        (unquote str . nil)
        (fn nil (unquote-splicing body . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'w/stdin)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/stdin)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/stdin (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/stdin|
                       (lambda (str . body)
                         `(call-w/stdin
                           ,str
                           (fn nil ,@(ar-nil-terminate body))))))
                  | w/stdin|))))
          (namespace-set-variable-value! '_w/stdin zz)
          zz))))))


((lambda ()
   (_sref _sig 'body 'tostring)
   (_sref _help* 'nil 'tostring)
   (_sref _source-file* _current-load-file* 'tostring)
   (_sref
    _source*
    '(mac
      tostring
      body
      (w/uniq
       gv
       (quasiquote
        (w/outstring
         (unquote gv . nil)
         (w/stdout (unquote gv . nil) (unquote-splicing body . nil) . nil)
         (inside (unquote gv . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'tostring)
   ((lambda ()
      (if (not (ar-false? (_bound 'tostring)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'tostring (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| tostring|
                       (lambda body
                         ((let ((| tostring|
                                 (lambda (gv)
                                   `(w/outstring
                                     ,gv
                                     (w/stdout ,gv ,@(ar-nil-terminate body))
                                     (inside ,gv)))))
                            | tostring|)
                          (_uniq)))))
                  | tostring|))))
          (namespace-set-variable-value! '_tostring zz)
          zz))))))


((lambda ()
   (_sref _sig '(str . body) 'fromstring)
   (_sref _help* 'nil 'fromstring)
   (_sref _source-file* _current-load-file* 'fromstring)
   (_sref
    _source*
    '(mac
      fromstring
      (str . body)
      (w/uniq
       gv
       (quasiquote
        (w/instring
         (unquote gv . nil)
         (unquote str . nil)
         (w/stdin (unquote gv . nil) (unquote-splicing body . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'fromstring)
   ((lambda ()
      (if (not (ar-false? (_bound 'fromstring)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'fromstring (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| fromstring|
                       (lambda (str . body)
                         ((let ((| fromstring|
                                 (lambda (gv)
                                   `(w/instring
                                     ,gv
                                     ,str
                                     (w/stdin
                                      ,gv
                                      ,@(ar-nil-terminate body))))))
                            | fromstring|)
                          (_uniq)))))
                  | fromstring|))))
          (namespace-set-variable-value! '_fromstring zz)
          zz))))))


((lambda ()
   (_sref _sig '(s (o eof nil . nil) . nil) 'readstring1)
   (_sref _help* 'nil 'readstring1)
   (_sref _source-file* _current-load-file* 'readstring1)
   (_sref
    _source*
    '(def
      readstring1
      (s (o eof nil . nil) . nil)
      (w/instring i s (read i eof . nil) . nil)
      .
      nil)
    'readstring1)
   ((lambda ()
      (if (not (ar-false? (_bound 'readstring1)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'readstring1 (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2528
                 (let* ((s (car g2528))
                        (eof
                         (if (pair? (ar-xcdr g2528))
                           (car (ar-xcdr g2528))
                           'nil)))
                   ((let ((| readstring1|
                           (lambda (i)
                             (_protect
                              (let ((| readstring1|
                                     (lambda ()
                                       ((let ((| readstring1|
                                               (lambda () (_read i eof))))
                                          | readstring1|)))))
                                | readstring1|)
                              (let ((| readstring1| (lambda () (_close i))))
                                | readstring1|)))))
                      | readstring1|)
                    (_instring s))))))
          (namespace-set-variable-value! '_readstring1 zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'readstring1)))
     (_setinfixop 'readstring1 ((ar-coerce _infixop 'fn) 'readstring1))
     'nil)))


((lambda ()
   (_sref _sig '((o x (stdin . nil) . nil) (o eof nil . nil) . nil) 'read)
   (_sref _help* 'nil 'read)
   (_sref _source-file* _current-load-file* 'read)
   (_sref
    _source*
    '(def
      read
      ((o x (stdin . nil) . nil) (o eof nil . nil) . nil)
      (if (isa x (quote string . nil) . nil)
        (readstring1 x eof . nil)
        (sread x eof . nil)
        .
        nil)
      .
      nil)
    'read)
   ((lambda ()
      (if (not (ar-false? (_bound 'read)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'read (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2529
                 (let* ((x (if (pair? g2529) (car g2529) (_stdin)))
                        (eof
                         (if (pair? (ar-xcdr g2529))
                           (car (ar-xcdr g2529))
                           'nil)))
                   (if (not (ar-false? (_isa x 'string)))
                     (_readstring1 x eof)
                     (_sread x eof))))))
          (namespace-set-variable-value! '_read zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'read)))
     (_setinfixop 'read ((ar-coerce _infixop 'fn) 'read))
     'nil)))


((lambda ()
   (_sref _sig '(name . nil) 'readfile)
   (_sref _help* 'nil 'readfile)
   (_sref _source-file* _current-load-file* 'readfile)
   (_sref
    _source*
    '(def
      readfile
      (name . nil)
      (w/infile s name (drain (read s . nil) . nil) . nil)
      .
      nil)
    'readfile)
   ((lambda ()
      (if (not (ar-false? (_bound 'readfile)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'readfile (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| readfile|
                      (lambda (name)
                        ((let ((| readfile|
                                (lambda (s)
                                  (_protect
                                   (let ((| readfile|
                                          (lambda ()
                                            ((let ((| readfile|
                                                    (lambda ()
                                                      ((let ((| readfile|
                                                              (lambda (g2530
                                                                       g2531)
                                                                (ar-call-resolve-notation
                                                                 ((let ((| readfile|
                                                                         (lambda (g2533)
                                                                           (begin
                                                                             (let ((zz
                                                                                    (let ((| g2533|
                                                                                           (lambda (g2534)
                                                                                             (if (not
                                                                                                  (ar-false?
                                                                                                   g2534))
                                                                                               ((let ((| g2533|
                                                                                                       (lambda ()
                                                                                                         ((let ((| g2533|
                                                                                                                 (lambda (g2532)
                                                                                                                   (if (not
                                                                                                                        (ar-false?
                                                                                                                         (_is
                                                                                                                          g2532
                                                                                                                          'nil)))
                                                                                                                     ((let ((| g2533|
                                                                                                                             (lambda ()
                                                                                                                               (begin
                                                                                                                                 (let ((zz
                                                                                                                                        _t))
                                                                                                                                   (set! g2531
                                                                                                                                     zz)
                                                                                                                                   zz)))))
                                                                                                                        | g2533|))
                                                                                                                     ((let ((| g2533|
                                                                                                                             (lambda (g2535)
                                                                                                                               (_atomic-invoke
                                                                                                                                (let ((| g2533|
                                                                                                                                       (lambda ()
                                                                                                                                         ((let ((| g2533|
                                                                                                                                                 (lambda (g2536)
                                                                                                                                                   ((let ((| g2533|
                                                                                                                                                           (lambda ()
                                                                                                                                                             ((let ((| g2533|
                                                                                                                                                                     (lambda (g2537)
                                                                                                                                                                       (begin
                                                                                                                                                                         (let ((zz
                                                                                                                                                                                g2537))
                                                                                                                                                                           (set! g2530
                                                                                                                                                                             zz)
                                                                                                                                                                           zz)))))
                                                                                                                                                                | g2533|)
                                                                                                                                                              (_cons
                                                                                                                                                               g2535
                                                                                                                                                               g2536)))))
                                                                                                                                                      | g2533|)))))
                                                                                                                                            | g2533|)
                                                                                                                                          g2530))))
                                                                                                                                  | g2533|)))))
                                                                                                                        | g2533|)
                                                                                                                      g2532)))))
                                                                                                            | g2533|)
                                                                                                          (_read
                                                                                                           s))
                                                                                                         (ar-call-resolve-notation
                                                                                                          g2533
                                                                                                          (_no
                                                                                                           g2531)))))
                                                                                                  | g2533|))
                                                                                               'nil))))
                                                                                      | g2533|)))
                                                                               (set! g2533
                                                                                 zz)
                                                                               zz)))))
                                                                    | readfile|)
                                                                  'nil)
                                                                 (_no g2531))
                                                                (_rev g2530))))
                                                         | readfile|)
                                                       'nil
                                                       'nil))))
                                               | readfile|)))))
                                     | readfile|)
                                   (let ((| readfile| (lambda () (_close s))))
                                     | readfile|)))))
                           | readfile|)
                         (_infile name)))))
                 | readfile|)))
          (namespace-set-variable-value! '_readfile zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'readfile)))
     (_setinfixop 'readfile ((ar-coerce _infixop 'fn) 'readfile))
     'nil)))


((lambda ()
   (_sref _sig '(name . nil) 'readfile1)
   (_sref _help* 'nil 'readfile1)
   (_sref _source-file* _current-load-file* 'readfile1)
   (_sref
    _source*
    '(def readfile1 (name . nil) (w/infile s name (read s . nil) . nil) . nil)
    'readfile1)
   ((lambda ()
      (if (not (ar-false? (_bound 'readfile1)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'readfile1 (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| readfile1|
                      (lambda (name)
                        ((let ((| readfile1|
                                (lambda (s)
                                  (_protect
                                   (let ((| readfile1|
                                          (lambda ()
                                            ((let ((| readfile1|
                                                    (lambda () (_read s))))
                                               | readfile1|)))))
                                     | readfile1|)
                                   (let ((| readfile1| (lambda () (_close s))))
                                     | readfile1|)))))
                           | readfile1|)
                         (_infile name)))))
                 | readfile1|)))
          (namespace-set-variable-value! '_readfile1 zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'readfile1)))
     (_setinfixop 'readfile1 ((ar-coerce _infixop 'fn) 'readfile1))
     'nil)))


((lambda ()
   (_sref _sig '(src (o eof nil . nil) . nil) 'readall)
   (_sref _help* 'nil 'readall)
   (_sref _source-file* _current-load-file* 'readall)
   (_sref
    _source*
    '(def
      readall
      (src (o eof nil . nil) . nil)
      ((afn
        (i . nil)
        (let x (read i eof . nil)
          (if (is x eof . nil) nil (cons x (self i . nil) . nil) . nil)
          .
          nil)
        .
        nil)
       (if (isa src (quote string . nil) . nil) (instring src . nil) src . nil)
       .
       nil)
      .
      nil)
    'readall)
   ((lambda ()
      (if (not (ar-false? (_bound 'readall)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'readall (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2538
                 (let* ((src (car g2538))
                        (eof
                         (if (pair? (ar-xcdr g2538))
                           (car (ar-xcdr g2538))
                           'nil)))
                   (ar-call-resolve-notation
                    ((let ((| readall|
                            (lambda (self)
                              (begin
                                (let ((zz
                                       (let ((| self|
                                              (lambda (i)
                                                ((let ((| self|
                                                        (lambda (x)
                                                          (if (not
                                                               (ar-false?
                                                                (_is x eof)))
                                                            'nil
                                                            (_cons
                                                             x
                                                             (ar-call-resolve-notation
                                                              self
                                                              i))))))
                                                   | self|)
                                                 (_read i eof)))))
                                         | self|)))
                                  (set! self zz)
                                  zz)))))
                       | readall|)
                     'nil)
                    (if (not (ar-false? (_isa src 'string)))
                      (_instring src)
                      src))))))
          (namespace-set-variable-value! '_readall zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'readall)))
     (_setinfixop 'readall ((ar-coerce _infixop 'fn) 'readall))
     'nil)))


((lambda ()
   (_sref _sig '(str . nil) 'allchars)
   (_sref _help* 'nil 'allchars)
   (_sref _source-file* _current-load-file* 'allchars)
   (_sref
    _source*
    '(def
      allchars
      (str . nil)
      (tostring
       (whiler c (readc str nil . nil) no (writec c . nil) . nil)
       .
       nil)
      .
      nil)
    'allchars)
   ((lambda ()
      (if (not (ar-false? (_bound 'allchars)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'allchars (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| allchars|
                      (lambda (str)
                        ((let ((| allchars|
                                (lambda (g2539)
                                  (_call-w/stdout
                                   g2539
                                   (let ((| allchars|
                                          (lambda ()
                                            ((let ((| allchars|
                                                    (lambda (c)
                                                      ((let ((| allchars|
                                                              (lambda (g2540)
                                                                ((let ((| allchars|
                                                                        (lambda ()
                                                                          (ar-call-resolve-notation
                                                                           ((let ((| allchars|
                                                                                   (lambda (g2541)
                                                                                     (begin
                                                                                       (let ((zz
                                                                                              (let ((| g2541|
                                                                                                     (lambda (g2542)
                                                                                                       (if (not
                                                                                                            (ar-false?
                                                                                                             g2542))
                                                                                                         ((let ((| g2541|
                                                                                                                 (lambda ()
                                                                                                                   (_writec
                                                                                                                    c)
                                                                                                                   (ar-call-resolve-notation
                                                                                                                    g2541
                                                                                                                    (_no
                                                                                                                     (ar-call-resolve-notation
                                                                                                                      g2540
                                                                                                                      ((let ((| g2541|
                                                                                                                              (lambda ()
                                                                                                                                (begin
                                                                                                                                  (let ((zz
                                                                                                                                         (_readc
                                                                                                                                          str
                                                                                                                                          'nil)))
                                                                                                                                    (set! c
                                                                                                                                      zz)
                                                                                                                                    zz)))))
                                                                                                                         | g2541|))))))))
                                                                                                            | g2541|))
                                                                                                         'nil))))
                                                                                                | g2541|)))
                                                                                         (set! g2541
                                                                                           zz)
                                                                                         zz)))))
                                                                              | allchars|)
                                                                            'nil)
                                                                           (_no
                                                                            (ar-call-resolve-notation
                                                                             g2540
                                                                             ((let ((| allchars|
                                                                                     (lambda ()
                                                                                       (begin
                                                                                         (let ((zz
                                                                                                (_readc
                                                                                                 str
                                                                                                 'nil)))
                                                                                           (set! c
                                                                                             zz)
                                                                                           zz)))))
                                                                                | allchars|))))))))
                                                                   | allchars|)))))
                                                         | allchars|)
                                                       (_testify _no)))))
                                               | allchars|)
                                             'nil))))
                                     | allchars|))
                                  (_inside g2539))))
                           | allchars|)
                         (_outstring)))))
                 | allchars|)))
          (namespace-set-variable-value! '_allchars zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'allchars)))
     (_setinfixop 'allchars ((ar-coerce _infixop 'fn) 'allchars))
     'nil)))


((lambda ()
   (_sref _sig '(name . nil) 'filechars)
   (_sref _help* 'nil 'filechars)
   (_sref _source-file* _current-load-file* 'filechars)
   (_sref
    _source*
    '(def
      filechars
      (name . nil)
      (w/infile s name (allchars s . nil) . nil)
      .
      nil)
    'filechars)
   ((lambda ()
      (if (not (ar-false? (_bound 'filechars)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'filechars (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| filechars|
                      (lambda (name)
                        ((let ((| filechars|
                                (lambda (s)
                                  (_protect
                                   (let ((| filechars|
                                          (lambda ()
                                            ((let ((| filechars|
                                                    (lambda () (_allchars s))))
                                               | filechars|)))))
                                     | filechars|)
                                   (let ((| filechars| (lambda () (_close s))))
                                     | filechars|)))))
                           | filechars|)
                         (_infile name)))))
                 | filechars|)))
          (namespace-set-variable-value! '_filechars zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'filechars)))
     (_setinfixop 'filechars ((ar-coerce _infixop 'fn) 'filechars))
     'nil)))


((lambda ()
   (_sref _sig '(val file . nil) 'writefile)
   (_sref _help* 'nil 'writefile)
   (_sref _source-file* _current-load-file* 'writefile)
   (_sref
    _source*
    '(def
      writefile
      (val file . nil)
      (let tmpfile (+ file ".tmp" . nil)
        (w/outfile o tmpfile (write val o . nil) . nil)
        (mvfile tmpfile file . nil)
        .
        nil)
      val
      .
      nil)
    'writefile)
   ((lambda ()
      (if (not (ar-false? (_bound 'writefile)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'writefile (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| writefile|
                      (lambda (val file)
                        ((let ((| writefile|
                                (lambda (tmpfile)
                                  ((let ((| writefile|
                                          (lambda (o)
                                            (_protect
                                             (let ((| writefile|
                                                    (lambda ()
                                                      ((let ((| writefile|
                                                              (lambda ()
                                                                (_write
                                                                 val
                                                                 o))))
                                                         | writefile|)))))
                                               | writefile|)
                                             (let ((| writefile|
                                                    (lambda () (_close o))))
                                               | writefile|)))))
                                     | writefile|)
                                   (_outfile tmpfile))
                                  (_mvfile tmpfile file))))
                           | writefile|)
                         (_+ file ".tmp"))
                        val)))
                 | writefile|)))
          (namespace-set-variable-value! '_writefile zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'writefile)))
     (_setinfixop 'writefile ((ar-coerce _infixop 'fn) 'writefile))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'sym)
   (_sref _help* 'nil 'sym)
   (_sref _source-file* _current-load-file* 'sym)
   (_sref
    _source*
    '(def sym (x . nil) (coerce x (quote sym . nil) . nil) . nil)
    'sym)
   ((lambda ()
      (if (not (ar-false? (_bound 'sym)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'sym (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| sym| (lambda (x) (_coerce x 'sym)))) | sym|)))
          (namespace-set-variable-value! '_sym zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'sym)))
     (_setinfixop 'sym ((ar-coerce _infixop 'fn) 'sym))
     'nil)))


((lambda ()
   (_sref _sig '(x (o b 10 . nil) . nil) 'int)
   (_sref _help* 'nil 'int)
   (_sref _source-file* _current-load-file* 'int)
   (_sref
    _source*
    '(def
      int
      (x (o b 10 . nil) . nil)
      (coerce x (quote int . nil) b . nil)
      .
      nil)
    'int)
   ((lambda ()
      (if (not (ar-false? (_bound 'int)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'int (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2543
                 (let* ((x (car g2543))
                        (b
                         (if (pair? (ar-xcdr g2543))
                           (car (ar-xcdr g2543))
                           10)))
                   (_coerce x 'int b)))))
          (namespace-set-variable-value! '_int zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'int)))
     (_setinfixop 'int ((ar-coerce _infixop 'fn) 'int))
     'nil)))


((lambda ()
   (_sref _sig 'exprs 'rand-choice)
   (_sref _help* 'nil 'rand-choice)
   (_sref _source-file* _current-load-file* 'rand-choice)
   (_sref
    _source*
    '(mac
      rand-choice
      exprs
      (quasiquote
       (case (rand (unquote (len exprs . nil) . nil) . nil)
         (unquote-splicing
          (let key -1
            (mappend
             (make-br-fn (list (++ key . nil) _ . nil) . nil)
             exprs
             .
             nil)
            .
            nil)
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'rand-choice)
   ((lambda ()
      (if (not (ar-false? (_bound 'rand-choice)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rand-choice (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| rand-choice|
                       (lambda exprs
                         `(case (rand ,(_len exprs))
                            (unquote-splicing
                             (ar-nil-terminate
                              ((let ((| rand-choice|
                                      (lambda (key)
                                        (_mappend
                                         (let ((| rand-choice|
                                                (lambda ()
                                                  (_list
                                                   ((let ((| rand-choice|
                                                           (lambda ()
                                                             (begin
                                                               (let ((zz
                                                                      (_+
                                                                       key
                                                                       1)))
                                                                 (set! key zz)
                                                                 zz)))))
                                                      | rand-choice|))
                                                   __))))
                                           | rand-choice|)
                                         exprs))))
                                 | rand-choice|)
                               -1)))))))
                  | rand-choice|))))
          (namespace-set-variable-value! '_rand-choice zz)
          zz))))))


((lambda ()
   (_sref _sig '(n expr . nil) 'n-of)
   (_sref _help* 'nil 'n-of)
   (_sref _source-file* _current-load-file* 'n-of)
   (_sref
    _source*
    '(mac
      n-of
      (n expr . nil)
      (w/uniq
       ga
       (quasiquote
        (let (unquote ga . nil)
          nil
          (repeat
           (unquote n . nil)
           (push (unquote expr . nil) (unquote ga . nil) . nil)
           .
           nil)
          (rev (unquote ga . nil) . nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'n-of)
   ((lambda ()
      (if (not (ar-false? (_bound 'n-of)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'n-of (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| n-of|
                       (lambda (n expr)
                         ((let ((| n-of|
                                 (lambda (ga)
                                   `(let ,ga
                                      nil
                                      (repeat ,n (push ,expr ,ga))
                                      (rev ,ga)))))
                            | n-of|)
                          (_uniq)))))
                  | n-of|))))
          (namespace-set-variable-value! '_n-of zz)
          zz))))))


((lambda ()
   (begin (let ((zz _n-of)) (namespace-set-variable-value! '_times zz) zz))))


((lambda ()
   (_sref _sig '(n . nil) 'rand-string)
   (_sref _help* 'nil 'rand-string)
   (_sref _source-file* _current-load-file* 'rand-string)
   (_sref
    _source*
    '(def
      rand-string
      (n . nil)
      (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        (with
         (nc 62 s (newstring n . nil) i 0 . nil)
         (w/infile
          str
          "/dev/urandom"
          (while
           (< i n . nil)
           (let x (readb str . nil)
             (unless (> x 247 . nil)
               (= (s i . nil) (c (mod x nc . nil) . nil) . nil)
               (++ i . nil)
               .
               nil)
             .
             nil)
           .
           nil)
          .
          nil)
         s
         .
         nil)
        .
        nil)
      .
      nil)
    'rand-string)
   ((lambda ()
      (if (not (ar-false? (_bound 'rand-string)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rand-string (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| rand-string|
                      (lambda (n)
                        ((let ((| rand-string|
                                (lambda (c)
                                  ((let ((| rand-string|
                                          (lambda (nc s i)
                                            ((let ((| rand-string|
                                                    (lambda (str)
                                                      (_protect
                                                       (let ((| rand-string|
                                                              (lambda ()
                                                                ((let ((| rand-string|
                                                                        (lambda ()
                                                                          (ar-call-resolve-notation
                                                                           ((let ((| rand-string|
                                                                                   (lambda (g2544)
                                                                                     (begin
                                                                                       (let ((zz
                                                                                              (let ((| g2544|
                                                                                                     (lambda (g2545)
                                                                                                       (if (not
                                                                                                            (ar-false?
                                                                                                             g2545))
                                                                                                         ((let ((| g2544|
                                                                                                                 (lambda ()
                                                                                                                   ((let ((| g2544|
                                                                                                                           (lambda (x)
                                                                                                                             (if (not
                                                                                                                                  (ar-false?
                                                                                                                                   (_no
                                                                                                                                    (_>
                                                                                                                                     x
                                                                                                                                     247))))
                                                                                                                               ((let ((| g2544|
                                                                                                                                       (lambda ()
                                                                                                                                         ((let ((| g2544|
                                                                                                                                                 (lambda ()
                                                                                                                                                   (_atomic-invoke
                                                                                                                                                    (let ((| g2544|
                                                                                                                                                           (lambda ()
                                                                                                                                                             ((let ((| g2544|
                                                                                                                                                                     (lambda (g2546
                                                                                                                                                                              g2548
                                                                                                                                                                              g2549)
                                                                                                                                                                       ((let ((| g2544|
                                                                                                                                                                               (lambda (g2547)
                                                                                                                                                                                 (_sref
                                                                                                                                                                                  g2546
                                                                                                                                                                                  g2547
                                                                                                                                                                                  g2548))))
                                                                                                                                                                          | g2544|)
                                                                                                                                                                        g2549))))
                                                                                                                                                                | g2544|)
                                                                                                                                                              s
                                                                                                                                                              i
                                                                                                                                                              (ar-call-resolve-notation
                                                                                                                                                               c
                                                                                                                                                               (_mod
                                                                                                                                                                x
                                                                                                                                                                nc))))))
                                                                                                                                                      | g2544|)))))
                                                                                                                                            | g2544|))
                                                                                                                                         ((let ((| g2544|
                                                                                                                                                 (lambda ()
                                                                                                                                                   (begin
                                                                                                                                                     (let ((zz
                                                                                                                                                            (_+
                                                                                                                                                             i
                                                                                                                                                             1)))
                                                                                                                                                       (set! i
                                                                                                                                                         zz)
                                                                                                                                                       zz)))))
                                                                                                                                            | g2544|)))))
                                                                                                                                  | g2544|))
                                                                                                                               'nil))))
                                                                                                                      | g2544|)
                                                                                                                    (_readb
                                                                                                                     str))
                                                                                                                   (ar-call-resolve-notation
                                                                                                                    g2544
                                                                                                                    (_<
                                                                                                                     i
                                                                                                                     n)))))
                                                                                                            | g2544|))
                                                                                                         'nil))))
                                                                                                | g2544|)))
                                                                                         (set! g2544
                                                                                           zz)
                                                                                         zz)))))
                                                                              | rand-string|)
                                                                            'nil)
                                                                           (_<
                                                                            i
                                                                            n)))))
                                                                   | rand-string|)))))
                                                         | rand-string|)
                                                       (let ((| rand-string|
                                                              (lambda ()
                                                                (_close str))))
                                                         | rand-string|)))))
                                               | rand-string|)
                                             (_infile "/dev/urandom"))
                                            s)))
                                     | rand-string|)
                                   62
                                   (_newstring n)
                                   0))))
                           | rand-string|)
                         "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
                 | rand-string|)))
          (namespace-set-variable-value! '_rand-string zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rand-string)))
     (_setinfixop 'rand-string ((ar-coerce _infixop 'fn) 'rand-string))
     'nil)))


((lambda ()
   (_sref _sig '(var s . body) 'forlen)
   (_sref _help* 'nil 'forlen)
   (_sref _source-file* _current-load-file* 'forlen)
   (_sref
    _source*
    '(mac
      forlen
      (var s . body)
      (quasiquote
       (for
        (unquote var . nil)
        0
        (- (len (unquote s . nil) . nil) 1 . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'forlen)
   ((lambda ()
      (if (not (ar-false? (_bound 'forlen)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'forlen (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| forlen|
                       (lambda (var s . body)
                         `(for
                           ,var
                           0
                           (- (len ,s) 1)
                           ,@(ar-nil-terminate body)))))
                  | forlen|))))
          (namespace-set-variable-value! '_forlen zz)
          zz))))))


((lambda ()
   (_sref _sig '(var s . body) 'on)
   (_sref _help* 'nil 'on)
   (_sref _source-file* _current-load-file* 'on)
   (_sref
    _source*
    '(mac
      on
      (var s . body)
      (if (is var (quote index . nil) . nil)
        (err "Can't use index as first arg to on." . nil)
        (w/uniq
         gs
         (quasiquote
          (let (unquote gs . nil)
            (unquote s . nil)
            (forlen
             index
             (unquote gs . nil)
             (let (unquote var . nil)
               ((unquote gs . nil) index . nil)
               (unquote-splicing body . nil)
               .
               nil)
             .
             nil)
            .
            nil)
          .
          nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'on)
   ((lambda ()
      (if (not (ar-false? (_bound 'on)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'on (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| on|
                       (lambda (var s . body)
                         (if (not (ar-false? (_is var 'index)))
                           (_err "Can't use index as first arg to on.")
                           ((let ((| on|
                                   (lambda (gs)
                                     `(let ,gs
                                        ,s
                                        (forlen
                                         index
                                         ,gs
                                         (let ,var
                                           (,gs index)
                                           ,@(ar-nil-terminate body)))))))
                              | on|)
                            (_uniq))))))
                  | on|))))
          (namespace-set-variable-value! '_on zz)
          zz))))))


((lambda ()
   (_sref _sig '(f seq . nil) 'best)
   (_sref _help* 'nil 'best)
   (_sref _source-file* _current-load-file* 'best)
   (_sref
    _source*
    '(def
      best
      (f seq . nil)
      (if (no seq . nil)
        nil
        (let wins (car seq . nil)
          (each
           elt
           (cdr seq . nil)
           (if (f elt wins . nil) (= wins elt . nil) . nil)
           .
           nil)
          wins
          .
          nil)
        .
        nil)
      .
      nil)
    'best)
   ((lambda ()
      (if (not (ar-false? (_bound 'best)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'best (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| best|
                      (lambda (f seq)
                        (if (not (ar-false? (_no seq)))
                          'nil
                          ((let ((| best|
                                  (lambda (wins)
                                    (_walk
                                     (_cdr seq)
                                     (let ((| best|
                                            (lambda (elt)
                                              (if (not
                                                   (ar-false?
                                                    (ar-call-resolve-notation
                                                     f
                                                     elt
                                                     wins)))
                                                ((let ((| best|
                                                        (lambda ()
                                                          (begin
                                                            (let ((zz elt))
                                                              (set! wins zz)
                                                              zz)))))
                                                   | best|))
                                                'nil))))
                                       | best|))
                                    wins)))
                             | best|)
                           (_car seq))))))
                 | best|)))
          (namespace-set-variable-value! '_best zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'best)))
     (_setinfixop 'best ((ar-coerce _infixop 'fn) 'best))
     'nil)))


((lambda ()
   (_sref _sig 'args 'max)
   (_sref _help* 'nil 'max)
   (_sref _source-file* _current-load-file* 'max)
   (_sref _source* '(def max args (best > args . nil) . nil) 'max)
   ((lambda ()
      (if (not (ar-false? (_bound 'max)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'max (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| max| (lambda args (_best _> args)))) | max|)))
          (namespace-set-variable-value! '_max zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'max)))
     (_setinfixop 'max ((ar-coerce _infixop 'fn) 'max))
     'nil)))


((lambda ()
   (_sref _sig 'args 'min)
   (_sref _help* 'nil 'min)
   (_sref _source-file* _current-load-file* 'min)
   (_sref _source* '(def min args (best < args . nil) . nil) 'min)
   ((lambda ()
      (if (not (ar-false? (_bound 'min)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'min (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| min| (lambda args (_best _< args)))) | min|)))
          (namespace-set-variable-value! '_min zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'min)))
     (_setinfixop 'min ((ar-coerce _infixop 'fn) 'min))
     'nil)))


((lambda ()
   (_sref _sig '(f seq . nil) 'most)
   (_sref _help* 'nil 'most)
   (_sref _source-file* _current-load-file* 'most)
   (_sref
    _source*
    '(def
      most
      (f seq . nil)
      (unless (no seq . nil)
        (withs
         (wins (car seq . nil) topscore (f wins . nil) . nil)
         (each
          elt
          (cdr seq . nil)
          (let score (f elt . nil)
            (if (> score topscore . nil)
              (= wins elt topscore score . nil)
              .
              nil)
            .
            nil)
          .
          nil)
         wins
         .
         nil)
        .
        nil)
      .
      nil)
    'most)
   ((lambda ()
      (if (not (ar-false? (_bound 'most)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'most (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| most|
                      (lambda (f seq)
                        (if (not (ar-false? (_no (_no seq))))
                          ((let ((| most|
                                  (lambda ()
                                    ((let ((| most|
                                            (lambda (wins)
                                              ((let ((| most|
                                                      (lambda (topscore)
                                                        ((let ((| most|
                                                                (lambda ()
                                                                  (_walk
                                                                   (_cdr seq)
                                                                   (let ((| most|
                                                                          (lambda (elt)
                                                                            ((let ((| most|
                                                                                    (lambda (score)
                                                                                      (if (not
                                                                                           (ar-false?
                                                                                            (_>
                                                                                             score
                                                                                             topscore)))
                                                                                        ((let ((| most|
                                                                                                (lambda ()
                                                                                                  (begin
                                                                                                    (let ((zz
                                                                                                           elt))
                                                                                                      (set! wins
                                                                                                        zz)
                                                                                                      zz))
                                                                                                  (begin
                                                                                                    (let ((zz
                                                                                                           score))
                                                                                                      (set! topscore
                                                                                                        zz)
                                                                                                      zz)))))
                                                                                           | most|))
                                                                                        'nil))))
                                                                               | most|)
                                                                             (ar-call-resolve-notation
                                                                              f
                                                                              elt)))))
                                                                     | most|))
                                                                  wins)))
                                                           | most|)))))
                                                 | most|)
                                               (ar-call-resolve-notation
                                                f
                                                wins)))))
                                       | most|)
                                     (_car seq)))))
                             | most|))
                          'nil))))
                 | most|)))
          (namespace-set-variable-value! '_most zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'most)))
     (_setinfixop 'most ((ar-coerce _infixop 'fn) 'most))
     'nil)))


((lambda ()
   (_sref _sig '(test elt seq . nil) 'insert-sorted)
   (_sref _help* 'nil 'insert-sorted)
   (_sref _source-file* _current-load-file* 'insert-sorted)
   (_sref
    _source*
    '(def
      insert-sorted
      (test elt seq . nil)
      (if (no seq . nil)
        (list elt . nil)
        (test elt (car seq . nil) . nil)
        (cons elt seq . nil)
        (cons
         (car seq . nil)
         (insert-sorted test elt (cdr seq . nil) . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'insert-sorted)
   ((lambda ()
      (if (not (ar-false? (_bound 'insert-sorted)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'insert-sorted (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| insert-sorted|
                      (lambda (test elt seq)
                        (if (not (ar-false? (_no seq)))
                          (_list elt)
                          (if (not
                               (ar-false?
                                (ar-call-resolve-notation
                                 test
                                 elt
                                 (_car seq))))
                            (_cons elt seq)
                            (_cons
                             (_car seq)
                             (_insert-sorted test elt (_cdr seq))))))))
                 | insert-sorted|)))
          (namespace-set-variable-value! '_insert-sorted zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'insert-sorted)))
     (_setinfixop 'insert-sorted ((ar-coerce _infixop 'fn) 'insert-sorted))
     'nil)))


((lambda ()
   (_sref _sig '(test elt seq . nil) 'insort)
   (_sref _help* 'nil 'insort)
   (_sref _source-file* _current-load-file* 'insort)
   (_sref
    _source*
    '(mac
      insort
      (test elt seq . nil)
      (quasiquote
       (zap
        (make-br-fn
         (insert-sorted (unquote test . nil) (unquote elt . nil) _ . nil)
         .
         nil)
        (unquote seq . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'insort)
   ((lambda ()
      (if (not (ar-false? (_bound 'insort)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'insort (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| insort|
                       (lambda (test elt seq)
                         `(zap
                           (make-br-fn (insert-sorted ,test ,elt _))
                           ,seq))))
                  | insort|))))
          (namespace-set-variable-value! '_insort zz)
          zz))))))


((lambda ()
   (_sref _sig '(test elt seq . nil) 'reinsert-sorted)
   (_sref _help* 'nil 'reinsert-sorted)
   (_sref _source-file* _current-load-file* 'reinsert-sorted)
   (_sref
    _source*
    '(def
      reinsert-sorted
      (test elt seq . nil)
      (if (no seq . nil)
        (list elt . nil)
        (is elt (car seq . nil) . nil)
        (reinsert-sorted test elt (cdr seq . nil) . nil)
        (test elt (car seq . nil) . nil)
        (cons elt (rem elt seq . nil) . nil)
        (cons
         (car seq . nil)
         (reinsert-sorted test elt (cdr seq . nil) . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'reinsert-sorted)
   ((lambda ()
      (if (not (ar-false? (_bound 'reinsert-sorted)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'reinsert-sorted (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| reinsert-sorted|
                      (lambda (test elt seq)
                        (if (not (ar-false? (_no seq)))
                          (_list elt)
                          (if (not (ar-false? (_is elt (_car seq))))
                            (_reinsert-sorted test elt (_cdr seq))
                            (if (not
                                 (ar-false?
                                  (ar-call-resolve-notation
                                   test
                                   elt
                                   (_car seq))))
                              (_cons elt (_rem elt seq))
                              (_cons
                               (_car seq)
                               (_reinsert-sorted test elt (_cdr seq)))))))))
                 | reinsert-sorted|)))
          (namespace-set-variable-value! '_reinsert-sorted zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'reinsert-sorted)))
     (_setinfixop 'reinsert-sorted ((ar-coerce _infixop 'fn) 'reinsert-sorted))
     'nil)))


((lambda ()
   (_sref _sig '(test elt seq . nil) 'insortnew)
   (_sref _help* 'nil 'insortnew)
   (_sref _source-file* _current-load-file* 'insortnew)
   (_sref
    _source*
    '(mac
      insortnew
      (test elt seq . nil)
      (quasiquote
       (zap
        (make-br-fn
         (reinsert-sorted (unquote test . nil) (unquote elt . nil) _ . nil)
         .
         nil)
        (unquote seq . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'insortnew)
   ((lambda ()
      (if (not (ar-false? (_bound 'insortnew)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'insortnew (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| insortnew|
                       (lambda (test elt seq)
                         `(zap
                           (make-br-fn (reinsert-sorted ,test ,elt _))
                           ,seq))))
                  | insortnew|))))
          (namespace-set-variable-value! '_insortnew zz)
          zz))))))


((lambda ()
   (_sref _sig '(f . nil) 'memo)
   (_sref _help* 'nil 'memo)
   (_sref _source-file* _current-load-file* 'memo)
   (_sref
    _source*
    '(def
      memo
      (f . nil)
      (with
       (cache (table . nil) nilcache (table . nil) . nil)
       (fn
        args
        (or (cache args . nil)
            (and (no (nilcache args . nil) . nil)
                 (aif
                  (apply f args . nil)
                  (= (cache args . nil) it . nil)
                  (do (set (nilcache args . nil) . nil) nil . nil)
                  .
                  nil)
                 .
                 nil)
            .
            nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'memo)
   ((lambda ()
      (if (not (ar-false? (_bound 'memo)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'memo (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| memo|
                      (lambda (f)
                        ((let ((| memo|
                                (lambda (cache nilcache)
                                  (let ((| memo|
                                         (lambda args
                                           ((let ((| memo|
                                                   (lambda (g2550)
                                                     (if (not
                                                          (ar-false? g2550))
                                                       g2550
                                                       ((let ((| memo|
                                                               (lambda (g2551)
                                                                 (if (not
                                                                      (ar-false?
                                                                       g2551))
                                                                   g2551
                                                                   'nil))))
                                                          | memo|)
                                                        (if (not
                                                             (ar-false?
                                                              (_no
                                                               (ar-call-resolve-notation
                                                                nilcache
                                                                args))))
                                                          ((let ((| g2551|
                                                                  (lambda (it)
                                                                    (if (not
                                                                         (ar-false?
                                                                          it))
                                                                      ((let ((| g2551|
                                                                              (lambda ()
                                                                                (_atomic-invoke
                                                                                 (let ((| g2551|
                                                                                        (lambda ()
                                                                                          ((let ((| g2551|
                                                                                                  (lambda (g2552
                                                                                                           g2554
                                                                                                           g2555)
                                                                                                    ((let ((| g2551|
                                                                                                            (lambda (g2553)
                                                                                                              (_sref
                                                                                                               g2552
                                                                                                               g2553
                                                                                                               g2554))))
                                                                                                       | g2551|)
                                                                                                     g2555))))
                                                                                             | g2551|)
                                                                                           cache
                                                                                           args
                                                                                           it))))
                                                                                   | g2551|)))))
                                                                         | g2551|))
                                                                      ((let ((| g2551|
                                                                              (lambda ()
                                                                                ((let ((| g2551|
                                                                                        (lambda ()
                                                                                          ((let ((| g2551|
                                                                                                  (lambda ()
                                                                                                    (_atomic-invoke
                                                                                                     (let ((| g2551|
                                                                                                            (lambda ()
                                                                                                              ((let ((| g2551|
                                                                                                                      (lambda (g2556
                                                                                                                               g2558
                                                                                                                               g2559)
                                                                                                                        ((let ((| g2551|
                                                                                                                                (lambda (g2557)
                                                                                                                                  (_sref
                                                                                                                                   g2556
                                                                                                                                   g2557
                                                                                                                                   g2558))))
                                                                                                                           | g2551|)
                                                                                                                         g2559))))
                                                                                                                 | g2551|)
                                                                                                               nilcache
                                                                                                               args
                                                                                                               _t))))
                                                                                                       | g2551|)))))
                                                                                             | g2551|)))))
                                                                                   | g2551|))
                                                                                'nil)))
                                                                         | g2551|))))))
                                                             | g2551|)
                                                           (_apply f args))
                                                          'nil))))))
                                              | memo|)
                                            (ar-call-resolve-notation
                                             cache
                                             args)))))
                                    | memo|))))
                           | memo|)
                         (_table)
                         (_table)))))
                 | memo|)))
          (namespace-set-variable-value! '_memo zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'memo)))
     (_setinfixop 'memo ((ar-coerce _infixop 'fn) 'memo))
     'nil)))


((lambda ()
   (_sref _sig '(name parms . body) 'defmemo)
   (_sref _help* 'nil 'defmemo)
   (_sref _source-file* _current-load-file* 'defmemo)
   (_sref
    _source*
    '(mac
      defmemo
      (name parms . body)
      (quasiquote
       (safeset
        (unquote name . nil)
        (memo
         (fn (unquote parms . nil) (unquote-splicing body . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'defmemo)
   ((lambda ()
      (if (not (ar-false? (_bound 'defmemo)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'defmemo (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| defmemo|
                       (lambda (name parms . body)
                         `(safeset
                           ,name
                           (memo (fn ,parms ,@(ar-nil-terminate body)))))))
                  | defmemo|))))
          (namespace-set-variable-value! '_defmemo zz)
          zz))))))


((lambda ()
   (_sref _sig 'args '<=)
   (_sref _help* 'nil '<=)
   (_sref _source-file* _current-load-file* '<=)
   (_sref
    _source*
    '(def
      <=
      args
      (or (no args . nil)
          (no (cdr args . nil) . nil)
          (and (no (> (car args . nil) (cadr args . nil) . nil) . nil)
               (apply <= (cdr args . nil) . nil)
               .
               nil)
          .
          nil)
      .
      nil)
    '<=)
   ((lambda ()
      (if (not (ar-false? (_bound '<=)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp '<= (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| <=|
                      (lambda args
                        ((let ((| <=|
                                (lambda (g2560)
                                  (if (not (ar-false? g2560))
                                    g2560
                                    ((let ((| <=|
                                            (lambda (g2561)
                                              (if (not (ar-false? g2561))
                                                g2561
                                                ((let ((| <=|
                                                        (lambda (g2562)
                                                          (if (not
                                                               (ar-false?
                                                                g2562))
                                                            g2562
                                                            'nil))))
                                                   | <=|)
                                                 (if (not
                                                      (ar-false?
                                                       (_no
                                                        (_>
                                                         (_car args)
                                                         (_cadr args)))))
                                                   (_apply _<= (_cdr args))
                                                   'nil))))))
                                       | <=|)
                                     (_no (_cdr args)))))))
                           | <=|)
                         (_no args)))))
                 | <=|)))
          (namespace-set-variable-value! '_<= zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) '<=)))
     (_setinfixop '<= ((ar-coerce _infixop 'fn) '<=))
     'nil)))


((lambda ()
   (_sref _sig 'args '>=)
   (_sref _help* 'nil '>=)
   (_sref _source-file* _current-load-file* '>=)
   (_sref
    _source*
    '(def
      >=
      args
      (or (no args . nil)
          (no (cdr args . nil) . nil)
          (and (no (< (car args . nil) (cadr args . nil) . nil) . nil)
               (apply >= (cdr args . nil) . nil)
               .
               nil)
          .
          nil)
      .
      nil)
    '>=)
   ((lambda ()
      (if (not (ar-false? (_bound '>=)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp '>= (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| >=|
                      (lambda args
                        ((let ((| >=|
                                (lambda (g2563)
                                  (if (not (ar-false? g2563))
                                    g2563
                                    ((let ((| >=|
                                            (lambda (g2564)
                                              (if (not (ar-false? g2564))
                                                g2564
                                                ((let ((| >=|
                                                        (lambda (g2565)
                                                          (if (not
                                                               (ar-false?
                                                                g2565))
                                                            g2565
                                                            'nil))))
                                                   | >=|)
                                                 (if (not
                                                      (ar-false?
                                                       (_no
                                                        (_<
                                                         (_car args)
                                                         (_cadr args)))))
                                                   (_apply _>= (_cdr args))
                                                   'nil))))))
                                       | >=|)
                                     (_no (_cdr args)))))))
                           | >=|)
                         (_no args)))))
                 | >=|)))
          (namespace-set-variable-value! '_>= zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) '>=)))
     (_setinfixop '>= ((ar-coerce _infixop 'fn) '>=))
     'nil)))


((lambda ()
   (begin (let ((zz _<)) (namespace-set-variable-value! '_lt zz) zz))))


(_setinfixop 'lt '<)


((lambda ()
   (begin (let ((zz _>)) (namespace-set-variable-value! '_gt zz) zz))))


(_setinfixop 'gt '>)


((lambda ()
   (begin (let ((zz _<=)) (namespace-set-variable-value! '_le zz) zz))))


(_setinfixop 'le '<=)


((lambda ()
   (begin (let ((zz _>=)) (namespace-set-variable-value! '_ge zz) zz))))


(_setinfixop 'ge '>=)


((lambda ()
   (_sref _sig '(c . nil) 'whitec)
   (_sref _help* 'nil 'whitec)
   (_sref _source-file* _current-load-file* 'whitec)
   (_sref
    _source*
    '(def whitec (c . nil) (in c #\space #\newline #\tab #\return . nil) . nil)
    'whitec)
   ((lambda ()
      (if (not (ar-false? (_bound 'whitec)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'whitec (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| whitec|
                      (lambda (c)
                        ((let ((| whitec|
                                (lambda (g2566)
                                  ((let ((| whitec|
                                          (lambda (g2567)
                                            (if (not (ar-false? g2567))
                                              g2567
                                              ((let ((| whitec|
                                                      (lambda (g2568)
                                                        (if (not
                                                             (ar-false? g2568))
                                                          g2568
                                                          ((let ((| whitec|
                                                                  (lambda (g2569)
                                                                    (if (not
                                                                         (ar-false?
                                                                          g2569))
                                                                      g2569
                                                                      ((let ((| whitec|
                                                                              (lambda (g2570)
                                                                                (if (not
                                                                                     (ar-false?
                                                                                      g2570))
                                                                                  g2570
                                                                                  'nil))))
                                                                         | whitec|)
                                                                       (_is
                                                                        g2566
                                                                        #\return))))))
                                                             | whitec|)
                                                           (_is
                                                            g2566
                                                            #\tab))))))
                                                 | whitec|)
                                               (_is g2566 #\newline))))))
                                     | whitec|)
                                   (_is g2566 #\space)))))
                           | whitec|)
                         c))))
                 | whitec|)))
          (namespace-set-variable-value! '_whitec zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'whitec)))
     (_setinfixop 'whitec ((ar-coerce _infixop 'fn) 'whitec))
     'nil)))


((lambda ()
   (_sref _sig '(c . nil) 'nonwhite)
   (_sref _help* 'nil 'nonwhite)
   (_sref _source-file* _current-load-file* 'nonwhite)
   (_sref
    _source*
    '(def nonwhite (c . nil) (no (whitec c . nil) . nil) . nil)
    'nonwhite)
   ((lambda ()
      (if (not (ar-false? (_bound 'nonwhite)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'nonwhite (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| nonwhite| (lambda (c) (_no (_whitec c)))))
                 | nonwhite|)))
          (namespace-set-variable-value! '_nonwhite zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'nonwhite)))
     (_setinfixop 'nonwhite ((ar-coerce _infixop 'fn) 'nonwhite))
     'nil)))


((lambda ()
   (_sref _sig '(c . nil) 'letter)
   (_sref _help* 'nil 'letter)
   (_sref _source-file* _current-load-file* 'letter)
   (_sref
    _source*
    '(def
      letter
      (c . nil)
      (or (<= #\a c #\z . nil) (<= #\A c #\Z . nil) . nil)
      .
      nil)
    'letter)
   ((lambda ()
      (if (not (ar-false? (_bound 'letter)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'letter (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| letter|
                      (lambda (c)
                        ((let ((| letter|
                                (lambda (g2571)
                                  (if (not (ar-false? g2571))
                                    g2571
                                    ((let ((| letter|
                                            (lambda (g2572)
                                              (if (not (ar-false? g2572))
                                                g2572
                                                'nil))))
                                       | letter|)
                                     (_<= #\A c #\Z))))))
                           | letter|)
                         (_<= #\a c #\z)))))
                 | letter|)))
          (namespace-set-variable-value! '_letter zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'letter)))
     (_setinfixop 'letter ((ar-coerce _infixop 'fn) 'letter))
     'nil)))


((lambda ()
   (_sref _sig '(c . nil) 'digit)
   (_sref _help* 'nil 'digit)
   (_sref _source-file* _current-load-file* 'digit)
   (_sref _source* '(def digit (c . nil) (<= #\0 c #\9 . nil) . nil) 'digit)
   ((lambda ()
      (if (not (ar-false? (_bound 'digit)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'digit (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| digit| (lambda (c) (_<= #\0 c #\9)))) | digit|)))
          (namespace-set-variable-value! '_digit zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'digit)))
     (_setinfixop 'digit ((ar-coerce _infixop 'fn) 'digit))
     'nil)))


((lambda ()
   (_sref _sig '(c . nil) 'alphadig)
   (_sref _help* 'nil 'alphadig)
   (_sref _source-file* _current-load-file* 'alphadig)
   (_sref
    _source*
    '(def alphadig (c . nil) (or (letter c . nil) (digit c . nil) . nil) . nil)
    'alphadig)
   ((lambda ()
      (if (not (ar-false? (_bound 'alphadig)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'alphadig (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| alphadig|
                      (lambda (c)
                        ((let ((| alphadig|
                                (lambda (g2573)
                                  (if (not (ar-false? g2573))
                                    g2573
                                    ((let ((| alphadig|
                                            (lambda (g2574)
                                              (if (not (ar-false? g2574))
                                                g2574
                                                'nil))))
                                       | alphadig|)
                                     (_digit c))))))
                           | alphadig|)
                         (_letter c)))))
                 | alphadig|)))
          (namespace-set-variable-value! '_alphadig zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'alphadig)))
     (_setinfixop 'alphadig ((ar-coerce _infixop 'fn) 'alphadig))
     'nil)))


((lambda ()
   (_sref _sig '(c . nil) 'punc)
   (_sref _help* 'nil 'punc)
   (_sref _source-file* _current-load-file* 'punc)
   (_sref
    _source*
    '(def punc (c . nil) (in c #\. #\, #\; #\: #\! #\? . nil) . nil)
    'punc)
   ((lambda ()
      (if (not (ar-false? (_bound 'punc)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'punc (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| punc|
                      (lambda (c)
                        ((let ((| punc|
                                (lambda (g2575)
                                  ((let ((| punc|
                                          (lambda (g2576)
                                            (if (not (ar-false? g2576))
                                              g2576
                                              ((let ((| punc|
                                                      (lambda (g2577)
                                                        (if (not
                                                             (ar-false? g2577))
                                                          g2577
                                                          ((let ((| punc|
                                                                  (lambda (g2578)
                                                                    (if (not
                                                                         (ar-false?
                                                                          g2578))
                                                                      g2578
                                                                      ((let ((| punc|
                                                                              (lambda (g2579)
                                                                                (if (not
                                                                                     (ar-false?
                                                                                      g2579))
                                                                                  g2579
                                                                                  ((let ((| punc|
                                                                                          (lambda (g2580)
                                                                                            (if (not
                                                                                                 (ar-false?
                                                                                                  g2580))
                                                                                              g2580
                                                                                              ((let ((| punc|
                                                                                                      (lambda (g2581)
                                                                                                        (if (not
                                                                                                             (ar-false?
                                                                                                              g2581))
                                                                                                          g2581
                                                                                                          'nil))))
                                                                                                 | punc|)
                                                                                               (_is
                                                                                                g2575
                                                                                                #\?))))))
                                                                                     | punc|)
                                                                                   (_is
                                                                                    g2575
                                                                                    #\!))))))
                                                                         | punc|)
                                                                       (_is
                                                                        g2575
                                                                        #\:))))))
                                                             | punc|)
                                                           (_is g2575 #\;))))))
                                                 | punc|)
                                               (_is g2575 #\,))))))
                                     | punc|)
                                   (_is g2575 #\.)))))
                           | punc|)
                         c))))
                 | punc|)))
          (namespace-set-variable-value! '_punc zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'punc)))
     (_setinfixop 'punc ((ar-coerce _infixop 'fn) 'punc))
     'nil)))


((lambda ()
   (_sref _sig '((o str (stdin . nil) . nil) . nil) 'readline)
   (_sref _help* 'nil 'readline)
   (_sref _source-file* _current-load-file* 'readline)
   (_sref
    _source*
    '(def
      readline
      ((o str (stdin . nil) . nil) . nil)
      (awhen
       (readc str . nil)
       (tostring
        (writec it . nil)
        (whiler
         c
         (readc str . nil)
         (make-br-fn (in _ nil #\newline . nil) . nil)
         (writec c . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'readline)
   ((lambda ()
      (if (not (ar-false? (_bound 'readline)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'readline (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2582
                 (let* ((str (if (pair? g2582) (car g2582) (_stdin))))
                   ((let ((| readline|
                           (lambda (it)
                             (if (not (ar-false? it))
                               ((let ((| readline|
                                       (lambda ()
                                         ((let ((| readline|
                                                 (lambda (g2583)
                                                   (_call-w/stdout
                                                    g2583
                                                    (let ((| readline|
                                                           (lambda ()
                                                             (_writec it)
                                                             ((let ((| readline|
                                                                     (lambda (c)
                                                                       ((let ((| readline|
                                                                               (lambda (g2584)
                                                                                 ((let ((| readline|
                                                                                         (lambda ()
                                                                                           (ar-call-resolve-notation
                                                                                            ((let ((| readline|
                                                                                                    (lambda (g2585)
                                                                                                      (begin
                                                                                                        (let ((zz
                                                                                                               (let ((| g2585|
                                                                                                                      (lambda (g2586)
                                                                                                                        (if (not
                                                                                                                             (ar-false?
                                                                                                                              g2586))
                                                                                                                          ((let ((| g2585|
                                                                                                                                  (lambda ()
                                                                                                                                    (_writec
                                                                                                                                     c)
                                                                                                                                    (ar-call-resolve-notation
                                                                                                                                     g2585
                                                                                                                                     (_no
                                                                                                                                      (ar-call-resolve-notation
                                                                                                                                       g2584
                                                                                                                                       ((let ((| g2585|
                                                                                                                                               (lambda ()
                                                                                                                                                 (begin
                                                                                                                                                   (let ((zz
                                                                                                                                                          (_readc
                                                                                                                                                           str)))
                                                                                                                                                     (set! c
                                                                                                                                                       zz)
                                                                                                                                                     zz)))))
                                                                                                                                          | g2585|))))))))
                                                                                                                             | g2585|))
                                                                                                                          'nil))))
                                                                                                                 | g2585|)))
                                                                                                          (set! g2585
                                                                                                            zz)
                                                                                                          zz)))))
                                                                                               | readline|)
                                                                                             'nil)
                                                                                            (_no
                                                                                             (ar-call-resolve-notation
                                                                                              g2584
                                                                                              ((let ((| readline|
                                                                                                      (lambda ()
                                                                                                        (begin
                                                                                                          (let ((zz
                                                                                                                 (_readc
                                                                                                                  str)))
                                                                                                            (set! c
                                                                                                              zz)
                                                                                                            zz)))))
                                                                                                 | readline|))))))))
                                                                                    | readline|)))))
                                                                          | readline|)
                                                                        (_testify
                                                                         (let ((| g2584|
                                                                                (lambda ()
                                                                                  ((let ((| g2584|
                                                                                          (lambda (g2587)
                                                                                            ((let ((| g2584|
                                                                                                    (lambda (g2588)
                                                                                                      (if (not
                                                                                                           (ar-false?
                                                                                                            g2588))
                                                                                                        g2588
                                                                                                        ((let ((| g2584|
                                                                                                                (lambda (g2589)
                                                                                                                  (if (not
                                                                                                                       (ar-false?
                                                                                                                        g2589))
                                                                                                                    g2589
                                                                                                                    'nil))))
                                                                                                           | g2584|)
                                                                                                         (_is
                                                                                                          g2587
                                                                                                          #\newline))))))
                                                                                               | g2584|)
                                                                                             (_is
                                                                                              g2587
                                                                                              'nil)))))
                                                                                     | g2584|)
                                                                                   __))))
                                                                           | g2584|))))))
                                                                | readline|)
                                                              'nil))))
                                                      | readline|))
                                                   (_inside g2583))))
                                            | readline|)
                                          (_outstring)))))
                                  | readline|))
                               'nil))))
                      | readline|)
                    (_readc str))))))
          (namespace-set-variable-value! '_readline zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'readline)))
     (_setinfixop 'readline ((ar-coerce _infixop 'fn) 'readline))
     'nil)))


((lambda ()
   (_sref _sig '(sumfn . body) 'summing)
   (_sref _help* 'nil 'summing)
   (_sref _source-file* _current-load-file* 'summing)
   (_sref
    _source*
    '(mac
      summing
      (sumfn . body)
      (w/uniq
       (gc gt . nil)
       (quasiquote
        (let (unquote gc . nil)
          0
          (let (unquote sumfn . nil)
            (fn
             ((unquote gt . nil) . nil)
             (if (unquote gt . nil) (++ (unquote gc . nil) . nil) . nil)
             .
             nil)
            (unquote-splicing body . nil)
            .
            nil)
          (unquote gc . nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'summing)
   ((lambda ()
      (if (not (ar-false? (_bound 'summing)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'summing (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| summing|
                       (lambda (sumfn . body)
                         ((let ((| summing|
                                 (lambda (gc gt)
                                   `(let ,gc
                                      0
                                      (let ,sumfn
                                        (fn (,gt) (if ,gt (++ ,gc)))
                                        ,@(ar-nil-terminate body))
                                      ,gc))))
                            | summing|)
                          (_uniq)
                          (_uniq)))))
                  | summing|))))
          (namespace-set-variable-value! '_summing zz)
          zz))))))


((lambda ()
   (_sref _sig '(f xs . nil) 'sum)
   (_sref _help* 'nil 'sum)
   (_sref _source-file* _current-load-file* 'sum)
   (_sref
    _source*
    '(def
      sum
      (f xs . nil)
      (let n 0 (each x xs (++ n (f x . nil) . nil) . nil) n . nil)
      .
      nil)
    'sum)
   ((lambda ()
      (if (not (ar-false? (_bound 'sum)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'sum (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| sum|
                      (lambda (f xs)
                        ((let ((| sum|
                                (lambda (n)
                                  (_walk
                                   xs
                                   (let ((| sum|
                                          (lambda (x)
                                            ((let ((| sum|
                                                    (lambda ()
                                                      (begin
                                                        (let ((zz
                                                               (_+
                                                                n
                                                                (ar-call-resolve-notation
                                                                 f
                                                                 x))))
                                                          (set! n zz)
                                                          zz)))))
                                               | sum|)))))
                                     | sum|))
                                  n)))
                           | sum|)
                         0))))
                 | sum|)))
          (namespace-set-variable-value! '_sum zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'sum)))
     (_setinfixop 'sum ((ar-coerce _infixop 'fn) 'sum))
     'nil)))


((lambda ()
   (_sref _sig '(f base tree . nil) 'treewise)
   (_sref _help* 'nil 'treewise)
   (_sref _source-file* _current-load-file* 'treewise)
   (_sref
    _source*
    '(def
      treewise
      (f base tree . nil)
      (if (atom tree . nil)
        (base tree . nil)
        (f
         (treewise f base (car tree . nil) . nil)
         (treewise f base (cdr tree . nil) . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'treewise)
   ((lambda ()
      (if (not (ar-false? (_bound 'treewise)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'treewise (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| treewise|
                      (lambda (f base tree)
                        (if (not (ar-false? (_atom tree)))
                          (ar-call-resolve-notation base tree)
                          (ar-call-resolve-notation
                           f
                           (_treewise f base (_car tree))
                           (_treewise f base (_cdr tree)))))))
                 | treewise|)))
          (namespace-set-variable-value! '_treewise zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'treewise)))
     (_setinfixop 'treewise ((ar-coerce _infixop 'fn) 'treewise))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'carif)
   (_sref _help* 'nil 'carif)
   (_sref _source-file* _current-load-file* 'carif)
   (_sref
    _source*
    '(def carif (x . nil) (if (atom x . nil) x (car x . nil) . nil) . nil)
    'carif)
   ((lambda ()
      (if (not (ar-false? (_bound 'carif)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'carif (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| carif|
                      (lambda (x)
                        (if (not (ar-false? (_atom x))) x (_car x)))))
                 | carif|)))
          (namespace-set-variable-value! '_carif zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'carif)))
     (_setinfixop 'carif ((ar-coerce _infixop 'fn) 'carif))
     'nil)))


((lambda ()
   (_sref _sig '(elts (o init "" . nil) (o sep ", " . nil) . nil) 'prall)
   (_sref _help* 'nil 'prall)
   (_sref _source-file* _current-load-file* 'prall)
   (_sref
    _source*
    '(def
      prall
      (elts (o init "" . nil) (o sep ", " . nil) . nil)
      (when elts
        (pr init (car elts . nil) . nil)
        (map (make-br-fn (pr sep _ . nil) . nil) (cdr elts . nil) . nil)
        elts
        .
        nil)
      .
      nil)
    'prall)
   ((lambda ()
      (if (not (ar-false? (_bound 'prall)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'prall (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2590
                 (let* ((elts (car g2590))
                        (init (if (pair? (ar-xcdr g2590))
                                (car (ar-xcdr g2590))
                                ""))
                        (sep
                         (if (pair? (ar-xcdr (ar-xcdr g2590)))
                           (car (ar-xcdr (ar-xcdr g2590)))
                           ", ")))
                   (if (not (ar-false? elts))
                     ((let ((| prall|
                             (lambda ()
                               (_pr init (_car elts))
                               (_map
                                (let ((| prall| (lambda () (_pr sep __))))
                                  | prall|)
                                (_cdr elts))
                               elts)))
                        | prall|))
                     'nil)))))
          (namespace-set-variable-value! '_prall zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'prall)))
     (_setinfixop 'prall ((ar-coerce _infixop 'fn) 'prall))
     'nil)))


((lambda ()
   (_sref _sig 'args 'prs)
   (_sref _help* 'nil 'prs)
   (_sref _source-file* _current-load-file* 'prs)
   (_sref _source* '(def prs args (prall args "" #\space . nil) . nil) 'prs)
   ((lambda ()
      (if (not (ar-false? (_bound 'prs)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'prs (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| prs| (lambda args (_prall args "" #\space)))) | prs|)))
          (namespace-set-variable-value! '_prs zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'prs)))
     (_setinfixop 'prs ((ar-coerce _infixop 'fn) 'prs))
     'nil)))


((lambda ()
   (_sref _sig '(old new tree . nil) 'tree-subst)
   (_sref _help* 'nil 'tree-subst)
   (_sref _source-file* _current-load-file* 'tree-subst)
   (_sref
    _source*
    '(def
      tree-subst
      (old new tree . nil)
      (if (is tree old . nil)
        new
        (atom tree . nil)
        tree
        (cons
         (tree-subst old new (car tree . nil) . nil)
         (tree-subst old new (cdr tree . nil) . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'tree-subst)
   ((lambda ()
      (if (not (ar-false? (_bound 'tree-subst)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'tree-subst (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| tree-subst|
                      (lambda (old new tree)
                        (if (not (ar-false? (_is tree old)))
                          new
                          (if (not (ar-false? (_atom tree)))
                            tree
                            (_cons
                             (_tree-subst old new (_car tree))
                             (_tree-subst old new (_cdr tree))))))))
                 | tree-subst|)))
          (namespace-set-variable-value! '_tree-subst zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'tree-subst)))
     (_setinfixop 'tree-subst ((ar-coerce _infixop 'fn) 'tree-subst))
     'nil)))


((lambda ()
   (_sref _sig '(f tree . nil) 'ontree)
   (_sref _help* 'nil 'ontree)
   (_sref _source-file* _current-load-file* 'ontree)
   (_sref
    _source*
    '(def
      ontree
      (f tree . nil)
      (f tree . nil)
      (unless (atom tree . nil)
        (ontree f (car tree . nil) . nil)
        (ontree f (cdr tree . nil) . nil)
        .
        nil)
      .
      nil)
    'ontree)
   ((lambda ()
      (if (not (ar-false? (_bound 'ontree)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ontree (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| ontree|
                      (lambda (f tree)
                        (ar-call-resolve-notation f tree)
                        (if (not (ar-false? (_no (_atom tree))))
                          ((let ((| ontree|
                                  (lambda ()
                                    (_ontree f (_car tree))
                                    (_ontree f (_cdr tree)))))
                             | ontree|))
                          'nil))))
                 | ontree|)))
          (namespace-set-variable-value! '_ontree zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ontree)))
     (_setinfixop 'ontree ((ar-coerce _infixop 'fn) 'ontree))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'dotted)
   (_sref _help* 'nil 'dotted)
   (_sref _source-file* _current-load-file* 'dotted)
   (_sref
    _source*
    '(def
      dotted
      (x . nil)
      (if (atom x . nil)
        nil
        (and (cdr x . nil)
             (or (atom (cdr x . nil) . nil) (dotted (cdr x . nil) . nil) . nil)
             .
             nil)
        .
        nil)
      .
      nil)
    'dotted)
   ((lambda ()
      (if (not (ar-false? (_bound 'dotted)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'dotted (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| dotted|
                      (lambda (x)
                        (if (not (ar-false? (_atom x)))
                          'nil
                          (if (not (ar-false? (_cdr x)))
                            ((let ((| dotted|
                                    (lambda (g2591)
                                      (if (not (ar-false? g2591))
                                        g2591
                                        ((let ((| dotted|
                                                (lambda (g2592)
                                                  (if (not (ar-false? g2592))
                                                    g2592
                                                    'nil))))
                                           | dotted|)
                                         (_dotted (_cdr x)))))))
                               | dotted|)
                             (_atom (_cdr x)))
                            'nil)))))
                 | dotted|)))
          (namespace-set-variable-value! '_dotted zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'dotted)))
     (_setinfixop 'dotted ((ar-coerce _infixop 'fn) 'dotted))
     'nil)))


((lambda ()
   (_sref _sig '(table data . nil) 'fill-table)
   (_sref _help* 'nil 'fill-table)
   (_sref _source-file* _current-load-file* 'fill-table)
   (_sref
    _source*
    '(def
      fill-table
      (table data . nil)
      (each (k v . nil) (pair data . nil) (= (table k . nil) v . nil) . nil)
      table
      .
      nil)
    'fill-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'fill-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'fill-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| fill-table|
                      (lambda (table data)
                        (_walk
                         (_pair data)
                         (lambda g2593
                           (let* ((k (ar-xcar (car g2593)))
                                  (v (ar-xcar (ar-xcdr (car g2593)))))
                             ((let ((| fill-table|
                                     (lambda ()
                                       (_atomic-invoke
                                        (let ((| fill-table|
                                               (lambda ()
                                                 ((let ((| fill-table|
                                                         (lambda (g2594
                                                                  g2596
                                                                  g2597)
                                                           ((let ((| fill-table|
                                                                   (lambda (g2595)
                                                                     (_sref
                                                                      g2594
                                                                      g2595
                                                                      g2596))))
                                                              | fill-table|)
                                                            g2597))))
                                                    | fill-table|)
                                                  table
                                                  k
                                                  v))))
                                          | fill-table|)))))
                                | fill-table|)))))
                        table)))
                 | fill-table|)))
          (namespace-set-variable-value! '_fill-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'fill-table)))
     (_setinfixop 'fill-table ((ar-coerce _infixop 'fn) 'fill-table))
     'nil)))


((lambda ()
   (_sref _sig '(h . nil) 'keys)
   (_sref _help* 'nil 'keys)
   (_sref _source-file* _current-load-file* 'keys)
   (_sref
    _source*
    '(def
      keys
      (h . nil)
      (accum a (each (k v . nil) h (a k . nil) . nil) . nil)
      .
      nil)
    'keys)
   ((lambda ()
      (if (not (ar-false? (_bound 'keys)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'keys (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| keys|
                      (lambda (h)
                        ((let ((| keys|
                                (lambda (g2598)
                                  ((let ((| keys|
                                          (lambda (a)
                                            ((let ((| keys|
                                                    (lambda ()
                                                      (_walk
                                                       h
                                                       (lambda g2599
                                                         (let* ((k
                                                                 (ar-xcar
                                                                  (car g2599)))
                                                                (v
                                                                 (ar-xcar
                                                                  (ar-xcdr
                                                                   (car
                                                                    g2599)))))
                                                           (ar-call-resolve-notation
                                                            a
                                                            k))))
                                                      (_rev g2598))))
                                               | keys|)))))
                                     | keys|)
                                   (let ((| a|
                                          (lambda ()
                                            ((let ((| a|
                                                    (lambda (g2600)
                                                      (_atomic-invoke
                                                       (let ((| a|
                                                              (lambda ()
                                                                ((let ((| a|
                                                                        (lambda (g2601)
                                                                          ((let ((| a|
                                                                                  (lambda ()
                                                                                    ((let ((| a|
                                                                                            (lambda (g2602)
                                                                                              (begin
                                                                                                (let ((zz
                                                                                                       g2602))
                                                                                                  (set! g2598
                                                                                                    zz)
                                                                                                  zz)))))
                                                                                       | a|)
                                                                                     (_cons
                                                                                      g2600
                                                                                      g2601)))))
                                                                             | a|)))))
                                                                   | a|)
                                                                 g2598))))
                                                         | a|)))))
                                               | a|)
                                             __))))
                                     | a|)))))
                           | keys|)
                         'nil))))
                 | keys|)))
          (namespace-set-variable-value! '_keys zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'keys)))
     (_setinfixop 'keys ((ar-coerce _infixop 'fn) 'keys))
     'nil)))


((lambda ()
   (_sref _sig '(h . nil) 'vals)
   (_sref _help* 'nil 'vals)
   (_sref _source-file* _current-load-file* 'vals)
   (_sref
    _source*
    '(def
      vals
      (h . nil)
      (accum a (each (k v . nil) h (a v . nil) . nil) . nil)
      .
      nil)
    'vals)
   ((lambda ()
      (if (not (ar-false? (_bound 'vals)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'vals (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| vals|
                      (lambda (h)
                        ((let ((| vals|
                                (lambda (g2603)
                                  ((let ((| vals|
                                          (lambda (a)
                                            ((let ((| vals|
                                                    (lambda ()
                                                      (_walk
                                                       h
                                                       (lambda g2604
                                                         (let* ((k
                                                                 (ar-xcar
                                                                  (car g2604)))
                                                                (v
                                                                 (ar-xcar
                                                                  (ar-xcdr
                                                                   (car
                                                                    g2604)))))
                                                           (ar-call-resolve-notation
                                                            a
                                                            v))))
                                                      (_rev g2603))))
                                               | vals|)))))
                                     | vals|)
                                   (let ((| a|
                                          (lambda ()
                                            ((let ((| a|
                                                    (lambda (g2605)
                                                      (_atomic-invoke
                                                       (let ((| a|
                                                              (lambda ()
                                                                ((let ((| a|
                                                                        (lambda (g2606)
                                                                          ((let ((| a|
                                                                                  (lambda ()
                                                                                    ((let ((| a|
                                                                                            (lambda (g2607)
                                                                                              (begin
                                                                                                (let ((zz
                                                                                                       g2607))
                                                                                                  (set! g2603
                                                                                                    zz)
                                                                                                  zz)))))
                                                                                       | a|)
                                                                                     (_cons
                                                                                      g2605
                                                                                      g2606)))))
                                                                             | a|)))))
                                                                   | a|)
                                                                 g2603))))
                                                         | a|)))))
                                               | a|)
                                             __))))
                                     | a|)))))
                           | vals|)
                         'nil))))
                 | vals|)))
          (namespace-set-variable-value! '_vals zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'vals)))
     (_setinfixop 'vals ((ar-coerce _infixop 'fn) 'vals))
     'nil)))


((lambda ()
   (_sref _sig '(h . nil) 'tablist)
   (_sref _help* 'nil 'tablist)
   (_sref _source-file* _current-load-file* 'tablist)
   (_sref
    _source*
    '(def
      tablist
      (h . nil)
      (accum a (maptable (fn args (a args . nil) . nil) h . nil) . nil)
      .
      nil)
    'tablist)
   ((lambda ()
      (if (not (ar-false? (_bound 'tablist)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'tablist (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| tablist|
                      (lambda (h)
                        ((let ((| tablist|
                                (lambda (g2608)
                                  ((let ((| tablist|
                                          (lambda (a)
                                            ((let ((| tablist|
                                                    (lambda ()
                                                      (_maptable
                                                       (let ((| tablist|
                                                              (lambda args
                                                                (ar-call-resolve-notation
                                                                 a
                                                                 args))))
                                                         | tablist|)
                                                       h)
                                                      (_rev g2608))))
                                               | tablist|)))))
                                     | tablist|)
                                   (let ((| a|
                                          (lambda ()
                                            ((let ((| a|
                                                    (lambda (g2609)
                                                      (_atomic-invoke
                                                       (let ((| a|
                                                              (lambda ()
                                                                ((let ((| a|
                                                                        (lambda (g2610)
                                                                          ((let ((| a|
                                                                                  (lambda ()
                                                                                    ((let ((| a|
                                                                                            (lambda (g2611)
                                                                                              (begin
                                                                                                (let ((zz
                                                                                                       g2611))
                                                                                                  (set! g2608
                                                                                                    zz)
                                                                                                  zz)))))
                                                                                       | a|)
                                                                                     (_cons
                                                                                      g2609
                                                                                      g2610)))))
                                                                             | a|)))))
                                                                   | a|)
                                                                 g2608))))
                                                         | a|)))))
                                               | a|)
                                             __))))
                                     | a|)))))
                           | tablist|)
                         'nil))))
                 | tablist|)))
          (namespace-set-variable-value! '_tablist zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'tablist)))
     (_setinfixop 'tablist ((ar-coerce _infixop 'fn) 'tablist))
     'nil)))


((lambda ()
   (_sref _sig '(al . nil) 'listtab)
   (_sref _help* 'nil 'listtab)
   (_sref _source-file* _current-load-file* 'listtab)
   (_sref
    _source*
    '(def
      listtab
      (al . nil)
      (let h (table . nil)
        (map (fn ((k v . nil) . nil) (= (h k . nil) v . nil) . nil) al . nil)
        h
        .
        nil)
      .
      nil)
    'listtab)
   ((lambda ()
      (if (not (ar-false? (_bound 'listtab)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'listtab (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| listtab|
                      (lambda (al)
                        ((let ((| listtab|
                                (lambda (h)
                                  (_map
                                   (lambda g2612
                                     (let* ((k (ar-xcar (car g2612)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2612)))))
                                       ((let ((| listtab|
                                               (lambda ()
                                                 (_atomic-invoke
                                                  (let ((| listtab|
                                                         (lambda ()
                                                           ((let ((| listtab|
                                                                   (lambda (g2613
                                                                            g2615
                                                                            g2616)
                                                                     ((let ((| listtab|
                                                                             (lambda (g2614)
                                                                               (_sref
                                                                                g2613
                                                                                g2614
                                                                                g2615))))
                                                                        | listtab|)
                                                                      g2616))))
                                                              | listtab|)
                                                            h
                                                            k
                                                            v))))
                                                    | listtab|)))))
                                          | listtab|))))
                                   al)
                                  h)))
                           | listtab|)
                         (_table)))))
                 | listtab|)))
          (namespace-set-variable-value! '_listtab zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'listtab)))
     (_setinfixop 'listtab ((ar-coerce _infixop 'fn) 'listtab))
     'nil)))


((lambda ()
   (_sref _sig 'args 'obj)
   (_sref _help* 'nil 'obj)
   (_sref _source-file* _current-load-file* 'obj)
   (_sref
    _source*
    '(mac
      obj
      args
      (quasiquote
       (listtab
        (list
         (unquote-splicing
          (map
           (fn
            ((k v . nil) . nil)
            (quasiquote
             (list (quote (unquote k . nil) . nil) (unquote v . nil) . nil)
             .
             nil)
            .
            nil)
           (pair args . nil)
           .
           nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'obj)
   ((lambda ()
      (if (not (ar-false? (_bound 'obj)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'obj (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| obj|
                       (lambda args
                         `(listtab
                           (list
                            ,@(ar-nil-terminate
                               (_map
                                (lambda g2617
                                  (let* ((k (ar-xcar (car g2617)))
                                         (v (ar-xcar (ar-xcdr (car g2617)))))
                                    `(list ',k ,v)))
                                (_pair args))))))))
                  | obj|))))
          (namespace-set-variable-value! '_obj zz)
          zz))))))


((lambda ()
   (_sref _sig '(file (o eof . nil) . nil) 'load-table)
   (_sref _help* 'nil 'load-table)
   (_sref _source-file* _current-load-file* 'load-table)
   (_sref
    _source*
    '(def
      load-table
      (file (o eof . nil) . nil)
      (w/infile i file (read-table i eof . nil) . nil)
      .
      nil)
    'load-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'load-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'load-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2618
                 (let* ((file (car g2618))
                        (eof
                         (if (pair? (ar-xcdr g2618))
                           (car (ar-xcdr g2618))
                           'nil)))
                   ((let ((| load-table|
                           (lambda (i)
                             (_protect
                              (let ((| load-table|
                                     (lambda ()
                                       ((let ((| load-table|
                                               (lambda ()
                                                 (_read-table i eof))))
                                          | load-table|)))))
                                | load-table|)
                              (let ((| load-table| (lambda () (_close i))))
                                | load-table|)))))
                      | load-table|)
                    (_infile file))))))
          (namespace-set-variable-value! '_load-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'load-table)))
     (_setinfixop 'load-table ((ar-coerce _infixop 'fn) 'load-table))
     'nil)))


((lambda ()
   (_sref _sig '((o i (stdin . nil) . nil) (o eof . nil) . nil) 'read-table)
   (_sref _help* 'nil 'read-table)
   (_sref _source-file* _current-load-file* 'read-table)
   (_sref
    _source*
    '(def
      read-table
      ((o i (stdin . nil) . nil) (o eof . nil) . nil)
      (aand
       (read i eof . nil)
       (mutable (if (alist it . nil) (listtab it . nil) it . nil) . nil)
       .
       nil)
      .
      nil)
    'read-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'read-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'read-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2619
                 (let* ((i (if (pair? g2619) (car g2619) (_stdin)))
                        (eof
                         (if (pair? (ar-xcdr g2619))
                           (car (ar-xcdr g2619))
                           'nil)))
                   ((let ((| read-table|
                           (lambda (it)
                             (if (not (ar-false? it))
                               (_mutable
                                (if (not (ar-false? (_alist it)))
                                  (_listtab it)
                                  it))
                               'nil))))
                      | read-table|)
                    (_read i eof))))))
          (namespace-set-variable-value! '_read-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'read-table)))
     (_setinfixop 'read-table ((ar-coerce _infixop 'fn) 'read-table))
     'nil)))


((lambda ()
   (_sref _sig '(file . nil) 'load-tables)
   (_sref _help* 'nil 'load-tables)
   (_sref _source-file* _current-load-file* 'load-tables)
   (_sref
    _source*
    '(def
      load-tables
      (file . nil)
      (w/infile
       i
       file
       (w/uniq eof (drain (read-table i eof . nil) eof . nil) . nil)
       .
       nil)
      .
      nil)
    'load-tables)
   ((lambda ()
      (if (not (ar-false? (_bound 'load-tables)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'load-tables (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| load-tables|
                      (lambda (file)
                        ((let ((| load-tables|
                                (lambda (i)
                                  (_protect
                                   (let ((| load-tables|
                                          (lambda ()
                                            ((let ((| load-tables|
                                                    (lambda ()
                                                      ((let ((| load-tables|
                                                              (lambda (eof)
                                                                ((let ((| load-tables|
                                                                        (lambda (g2620
                                                                                 g2621)
                                                                          (ar-call-resolve-notation
                                                                           ((let ((| load-tables|
                                                                                   (lambda (g2623)
                                                                                     (begin
                                                                                       (let ((zz
                                                                                              (let ((| g2623|
                                                                                                     (lambda (g2624)
                                                                                                       (if (not
                                                                                                            (ar-false?
                                                                                                             g2624))
                                                                                                         ((let ((| g2623|
                                                                                                                 (lambda ()
                                                                                                                   ((let ((| g2623|
                                                                                                                           (lambda (g2622)
                                                                                                                             (if (not
                                                                                                                                  (ar-false?
                                                                                                                                   (_is
                                                                                                                                    g2622
                                                                                                                                    eof)))
                                                                                                                               ((let ((| g2623|
                                                                                                                                       (lambda ()
                                                                                                                                         (begin
                                                                                                                                           (let ((zz
                                                                                                                                                  _t))
                                                                                                                                             (set! g2621
                                                                                                                                               zz)
                                                                                                                                             zz)))))
                                                                                                                                  | g2623|))
                                                                                                                               ((let ((| g2623|
                                                                                                                                       (lambda (g2625)
                                                                                                                                         (_atomic-invoke
                                                                                                                                          (let ((| g2623|
                                                                                                                                                 (lambda ()
                                                                                                                                                   ((let ((| g2623|
                                                                                                                                                           (lambda (g2626)
                                                                                                                                                             ((let ((| g2623|
                                                                                                                                                                     (lambda ()
                                                                                                                                                                       ((let ((| g2623|
                                                                                                                                                                               (lambda (g2627)
                                                                                                                                                                                 (begin
                                                                                                                                                                                   (let ((zz
                                                                                                                                                                                          g2627))
                                                                                                                                                                                     (set! g2620
                                                                                                                                                                                       zz)
                                                                                                                                                                                     zz)))))
                                                                                                                                                                          | g2623|)
                                                                                                                                                                        (_cons
                                                                                                                                                                         g2625
                                                                                                                                                                         g2626)))))
                                                                                                                                                                | g2623|)))))
                                                                                                                                                      | g2623|)
                                                                                                                                                    g2620))))
                                                                                                                                            | g2623|)))))
                                                                                                                                  | g2623|)
                                                                                                                                g2622)))))
                                                                                                                      | g2623|)
                                                                                                                    (_read-table
                                                                                                                     i
                                                                                                                     eof))
                                                                                                                   (ar-call-resolve-notation
                                                                                                                    g2623
                                                                                                                    (_no
                                                                                                                     g2621)))))
                                                                                                            | g2623|))
                                                                                                         'nil))))
                                                                                                | g2623|)))
                                                                                         (set! g2623
                                                                                           zz)
                                                                                         zz)))))
                                                                              | load-tables|)
                                                                            'nil)
                                                                           (_no
                                                                            g2621))
                                                                          (_rev
                                                                           g2620))))
                                                                   | load-tables|)
                                                                 'nil
                                                                 'nil))))
                                                         | load-tables|)
                                                       (_uniq)))))
                                               | load-tables|)))))
                                     | load-tables|)
                                   (let ((| load-tables|
                                          (lambda () (_close i))))
                                     | load-tables|)))))
                           | load-tables|)
                         (_infile file)))))
                 | load-tables|)))
          (namespace-set-variable-value! '_load-tables zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'load-tables)))
     (_setinfixop 'load-tables ((ar-coerce _infixop 'fn) 'load-tables))
     'nil)))


((lambda ()
   (_sref _sig '(h file . nil) 'save-table)
   (_sref _help* 'nil 'save-table)
   (_sref _source-file* _current-load-file* 'save-table)
   (_sref
    _source*
    '(def
      save-table
      (h file . nil)
      (writefile (tablist h . nil) file . nil)
      .
      nil)
    'save-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'save-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'save-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| save-table|
                      (lambda (h file) (_writefile (_tablist h) file))))
                 | save-table|)))
          (namespace-set-variable-value! '_save-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'save-table)))
     (_setinfixop 'save-table ((ar-coerce _infixop 'fn) 'save-table))
     'nil)))


((lambda ()
   (_sref _sig '(h (o o (stdout . nil) . nil) . nil) 'write-table)
   (_sref _help* 'nil 'write-table)
   (_sref _source-file* _current-load-file* 'write-table)
   (_sref
    _source*
    '(def
      write-table
      (h (o o (stdout . nil) . nil) . nil)
      (write (tablist h . nil) o . nil)
      .
      nil)
    'write-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'write-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'write-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2628
                 (let* ((h (car g2628))
                        (o
                         (if (pair? (ar-xcdr g2628))
                           (car (ar-xcdr g2628))
                           (_stdout))))
                   (_write (_tablist h) o)))))
          (namespace-set-variable-value! '_write-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'write-table)))
     (_setinfixop 'write-table ((ar-coerce _infixop 'fn) 'write-table))
     'nil)))


((lambda ()
   (_sref _sig '(x . args) 'copy)
   (_sref _help* 'nil 'copy)
   (_sref _source-file* _current-load-file* 'copy)
   (_sref
    _source*
    '(def
      copy
      (x . args)
      (let x2 (case
               (type x . nil)
               sym
               x
               cons
               (copylist x . nil)
               string
               (let new (newstring (len x . nil) . nil)
                 (forlen i x (= (new i . nil) (x i . nil) . nil) . nil)
                 new
                 .
                 nil)
               table
               (let new (table . nil)
                 (each (k v . nil) x (= (new k . nil) v . nil) . nil)
                 new
                 .
                 nil)
               (err "Can't copy " x . nil)
               .
               nil)
        (map
         (fn ((k v . nil) . nil) (= (x2 k . nil) v . nil) . nil)
         (pair args . nil)
         .
         nil)
        x2
        .
        nil)
      .
      nil)
    'copy)
   ((lambda ()
      (if (not (ar-false? (_bound 'copy)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'copy (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| copy|
                      (lambda (x . args)
                        ((let ((| copy|
                                (lambda (x2)
                                  (_map
                                   (lambda g2629
                                     (let* ((k (ar-xcar (car g2629)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2629)))))
                                       ((let ((| copy|
                                               (lambda ()
                                                 (_atomic-invoke
                                                  (let ((| copy|
                                                         (lambda ()
                                                           ((let ((| copy|
                                                                   (lambda (g2630
                                                                            g2632
                                                                            g2633)
                                                                     ((let ((| copy|
                                                                             (lambda (g2631)
                                                                               (_sref
                                                                                g2630
                                                                                g2631
                                                                                g2632))))
                                                                        | copy|)
                                                                      g2633))))
                                                              | copy|)
                                                            x2
                                                            k
                                                            v))))
                                                    | copy|)))))
                                          | copy|))))
                                   (_pair args))
                                  x2)))
                           | copy|)
                         ((let ((| x2|
                                 (lambda (g2634)
                                   (if (not (ar-false? (_is g2634 'sym)))
                                     x
                                     (if (not (ar-false? (_is g2634 'cons)))
                                       (_copylist x)
                                       (if (not
                                            (ar-false? (_is g2634 'string)))
                                         ((let ((| x2|
                                                 (lambda (new)
                                                   ((let ((| x2|
                                                           (lambda (i
                                                                    g2635
                                                                    g2636)
                                                             ((let ((| x2|
                                                                     (lambda ()
                                                                       (begin
                                                                         (let ((zz
                                                                                g2635))
                                                                           (set! i
                                                                             zz)
                                                                           zz))
                                                                       (ar-call-resolve-notation
                                                                        ((let ((| x2|
                                                                                (lambda (g2637)
                                                                                  (begin
                                                                                    (let ((zz
                                                                                           (let ((| g2637|
                                                                                                  (lambda (g2638)
                                                                                                    (if (not
                                                                                                         (ar-false?
                                                                                                          g2638))
                                                                                                      ((let ((| g2637|
                                                                                                              (lambda ()
                                                                                                                ((let ((| g2637|
                                                                                                                        (lambda ()
                                                                                                                          (_atomic-invoke
                                                                                                                           (let ((| g2637|
                                                                                                                                  (lambda ()
                                                                                                                                    ((let ((| g2637|
                                                                                                                                            (lambda (g2639
                                                                                                                                                     g2641
                                                                                                                                                     g2642)
                                                                                                                                              ((let ((| g2637|
                                                                                                                                                      (lambda (g2640)
                                                                                                                                                        (_sref
                                                                                                                                                         g2639
                                                                                                                                                         g2640
                                                                                                                                                         g2641))))
                                                                                                                                                 | g2637|)
                                                                                                                                               g2642))))
                                                                                                                                       | g2637|)
                                                                                                                                     new
                                                                                                                                     i
                                                                                                                                     (ar-call-resolve-notation
                                                                                                                                      x
                                                                                                                                      i)))))
                                                                                                                             | g2637|)))))
                                                                                                                   | g2637|))
                                                                                                                (begin
                                                                                                                  (let ((zz
                                                                                                                         (_+
                                                                                                                          i
                                                                                                                          1)))
                                                                                                                    (set! i
                                                                                                                      zz)
                                                                                                                    zz))
                                                                                                                (ar-call-resolve-notation
                                                                                                                 g2637
                                                                                                                 (_<
                                                                                                                  i
                                                                                                                  g2636)))))
                                                                                                         | g2637|))
                                                                                                      'nil))))
                                                                                             | g2637|)))
                                                                                      (set! g2637
                                                                                        zz)
                                                                                      zz)))))
                                                                           | x2|)
                                                                         'nil)
                                                                        (_<
                                                                         i
                                                                         g2636)))))
                                                                | x2|)))))
                                                      | x2|)
                                                    'nil
                                                    0
                                                    (_+ (_- (_len x) 1) 1))
                                                   new)))
                                            | x2|)
                                          (_newstring (_len x)))
                                         (if (not
                                              (ar-false? (_is g2634 'table)))
                                           ((let ((| x2|
                                                   (lambda (new)
                                                     (_walk
                                                      x
                                                      (lambda g2643
                                                        (let* ((k
                                                                (ar-xcar
                                                                 (car g2643)))
                                                               (v
                                                                (ar-xcar
                                                                 (ar-xcdr
                                                                  (car
                                                                   g2643)))))
                                                          ((let ((| x2|
                                                                  (lambda ()
                                                                    (_atomic-invoke
                                                                     (let ((| x2|
                                                                            (lambda ()
                                                                              ((let ((| x2|
                                                                                      (lambda (g2644
                                                                                               g2646
                                                                                               g2647)
                                                                                        ((let ((| x2|
                                                                                                (lambda (g2645)
                                                                                                  (_sref
                                                                                                   g2644
                                                                                                   g2645
                                                                                                   g2646))))
                                                                                           | x2|)
                                                                                         g2647))))
                                                                                 | x2|)
                                                                               new
                                                                               k
                                                                               v))))
                                                                       | x2|)))))
                                                             | x2|)))))
                                                     new)))
                                              | x2|)
                                            (_table))
                                           (_err "Can't copy " x))))))))
                            | x2|)
                          (_type x))))))
                 | copy|)))
          (namespace-set-variable-value! '_copy zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'copy)))
     (_setinfixop 'copy ((ar-coerce _infixop 'fn) 'copy))
     'nil)))


((lambda ()
   (_sref _sig '(n m . nil) 'shr)
   (_sref _help* 'nil 'shr)
   (_sref _source-file* _current-load-file* 'shr)
   (_sref _source* '(def shr (n m . nil) (shl n (- m . nil) . nil) . nil) 'shr)
   ((lambda ()
      (if (not (ar-false? (_bound 'shr)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'shr (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| shr| (lambda (n m) (_shl n (_- m))))) | shr|)))
          (namespace-set-variable-value! '_shr zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'shr)))
     (_setinfixop 'shr ((ar-coerce _infixop 'fn) 'shr))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'abs)
   (_sref _help* 'nil 'abs)
   (_sref _source-file* _current-load-file* 'abs)
   (_sref
    _source*
    '(def abs (n . nil) (if (< n 0 . nil) (- n . nil) n . nil) . nil)
    'abs)
   ((lambda ()
      (if (not (ar-false? (_bound 'abs)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'abs (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| abs|
                      (lambda (n) (if (not (ar-false? (_< n 0))) (_- n) n))))
                 | abs|)))
          (namespace-set-variable-value! '_abs zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'abs)))
     (_setinfixop 'abs ((ar-coerce _infixop 'fn) 'abs))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'round)
   (_sref _help* 'nil 'round)
   (_sref _source-file* _current-load-file* 'round)
   (_sref
    _source*
    '(def
      round
      (n . nil)
      (withs
       (base (trunc n . nil) rem (abs (- n base . nil) . nil) . nil)
       (if (> rem 1/2 . nil)
         ((if (> n 0 . nil) + - . nil) base 1 . nil)
         (< rem 1/2 . nil)
         base
         (odd base . nil)
         ((if (> n 0 . nil) + - . nil) base 1 . nil)
         base
         .
         nil)
       .
       nil)
      .
      nil)
    'round)
   ((lambda ()
      (if (not (ar-false? (_bound 'round)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'round (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| round|
                      (lambda (n)
                        ((let ((| round|
                                (lambda (base)
                                  ((let ((| round|
                                          (lambda (rem)
                                            ((let ((| round|
                                                    (lambda ()
                                                      (if (not
                                                           (ar-false?
                                                            (_> rem 1/2)))
                                                        (ar-call-resolve-notation
                                                         (if (not
                                                              (ar-false?
                                                               (_> n 0)))
                                                           _+
                                                           _-)
                                                         base
                                                         1)
                                                        (if (not
                                                             (ar-false?
                                                              (_< rem 1/2)))
                                                          base
                                                          (if (not
                                                               (ar-false?
                                                                (_odd base)))
                                                            (ar-call-resolve-notation
                                                             (if (not
                                                                  (ar-false?
                                                                   (_> n 0)))
                                                               _+
                                                               _-)
                                                             base
                                                             1)
                                                            base))))))
                                               | round|)))))
                                     | round|)
                                   (_abs (_- n base))))))
                           | round|)
                         (_trunc n)))))
                 | round|)))
          (namespace-set-variable-value! '_round zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'round)))
     (_setinfixop 'round ((ar-coerce _infixop 'fn) 'round))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'roundup)
   (_sref _help* 'nil 'roundup)
   (_sref _source-file* _current-load-file* 'roundup)
   (_sref
    _source*
    '(def
      roundup
      (n . nil)
      (withs
       (base (trunc n . nil) rem (abs (- n base . nil) . nil) . nil)
       (if (>= rem 1/2 . nil)
         ((if (> n 0 . nil) + - . nil) base 1 . nil)
         base
         .
         nil)
       .
       nil)
      .
      nil)
    'roundup)
   ((lambda ()
      (if (not (ar-false? (_bound 'roundup)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'roundup (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| roundup|
                      (lambda (n)
                        ((let ((| roundup|
                                (lambda (base)
                                  ((let ((| roundup|
                                          (lambda (rem)
                                            ((let ((| roundup|
                                                    (lambda ()
                                                      (if (not
                                                           (ar-false?
                                                            (_>= rem 1/2)))
                                                        (ar-call-resolve-notation
                                                         (if (not
                                                              (ar-false?
                                                               (_> n 0)))
                                                           _+
                                                           _-)
                                                         base
                                                         1)
                                                        base))))
                                               | roundup|)))))
                                     | roundup|)
                                   (_abs (_- n base))))))
                           | roundup|)
                         (_trunc n)))))
                 | roundup|)))
          (namespace-set-variable-value! '_roundup zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'roundup)))
     (_setinfixop 'roundup ((ar-coerce _infixop 'fn) 'roundup))
     'nil)))


((lambda ()
   (_sref _sig '(n quantum . nil) 'nearest)
   (_sref _help* 'nil 'nearest)
   (_sref _source-file* _current-load-file* 'nearest)
   (_sref
    _source*
    '(def
      nearest
      (n quantum . nil)
      (* (roundup (/ n quantum . nil) . nil) quantum . nil)
      .
      nil)
    'nearest)
   ((lambda ()
      (if (not (ar-false? (_bound 'nearest)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'nearest (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| nearest|
                      (lambda (n quantum)
                        (_* (_roundup (_/ n quantum)) quantum))))
                 | nearest|)))
          (namespace-set-variable-value! '_nearest zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'nearest)))
     (_setinfixop 'nearest ((ar-coerce _infixop 'fn) 'nearest))
     'nil)))


((lambda ()
   (_sref _sig '(ns . nil) 'avg)
   (_sref _help* 'nil 'avg)
   (_sref _source-file* _current-load-file* 'avg)
   (_sref
    _source*
    '(def avg (ns . nil) (/ (apply + ns . nil) (len ns . nil) . nil) . nil)
    'avg)
   ((lambda ()
      (if (not (ar-false? (_bound 'avg)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'avg (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| avg| (lambda (ns) (_/ (_apply _+ ns) (_len ns)))))
                 | avg|)))
          (namespace-set-variable-value! '_avg zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'avg)))
     (_setinfixop 'avg ((ar-coerce _infixop 'fn) 'avg))
     'nil)))


((lambda ()
   (_sref _sig '(ns (o test > . nil) . nil) 'med)
   (_sref _help* 'nil 'med)
   (_sref _source-file* _current-load-file* 'med)
   (_sref
    _source*
    '(def
      med
      (ns (o test > . nil) . nil)
      ((sort test ns . nil) (round (/ (len ns . nil) 2 . nil) . nil) . nil)
      .
      nil)
    'med)
   ((lambda ()
      (if (not (ar-false? (_bound 'med)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'med (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2648
                 (let* ((ns (car g2648))
                        (test
                         (if (pair? (ar-xcdr g2648))
                           (car (ar-xcdr g2648))
                           _>)))
                   (ar-call-resolve-notation
                    (_sort test ns)
                    (_round (_/ (_len ns) 2)))))))
          (namespace-set-variable-value! '_med zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'med)))
     (_setinfixop 'med ((ar-coerce _infixop 'fn) 'med))
     'nil)))


((lambda ()
   (_sref _sig '(test seq . nil) 'sort)
   (_sref _help* 'nil 'sort)
   (_sref _source-file* _current-load-file* 'sort)
   (_sref
    _source*
    '(def
      sort
      (test seq . nil)
      (if (alist seq . nil)
        (mergesort test (copy seq . nil) . nil)
        (coerce
         (mergesort test (coerce seq (quote cons . nil) . nil) . nil)
         (type seq . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'sort)
   ((lambda ()
      (if (not (ar-false? (_bound 'sort)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'sort (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| sort|
                      (lambda (test seq)
                        (if (not (ar-false? (_alist seq)))
                          (_mergesort test (_copy seq))
                          (_coerce
                           (_mergesort test (_coerce seq 'cons))
                           (_type seq))))))
                 | sort|)))
          (namespace-set-variable-value! '_sort zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'sort)))
     (_setinfixop 'sort ((ar-coerce _infixop 'fn) 'sort))
     'nil)))


((lambda ()
   (_sref _sig '(less? lst . nil) 'mergesort)
   (_sref _help* 'nil 'mergesort)
   (_sref _source-file* _current-load-file* 'mergesort)
   (_sref
    _source*
    '(def
      mergesort
      (less? lst . nil)
      (with
       (n (len lst . nil) . nil)
       (if (<= n 1 . nil)
         lst
         ((afn
           (n . nil)
           (if (> n 2 . nil)
             (withs
              (j
               (/ (if (even n . nil) n (- n 1 . nil) . nil) 2 . nil)
               a
               (self j . nil)
               b
               (self (- n j . nil) . nil)
               .
               nil)
              (merge less? a b . nil)
              .
              nil)
             (is n 2 . nil)
             (with
              (x (car lst . nil) y (cadr lst . nil) p lst . nil)
              (= lst (cddr lst . nil) . nil)
              (when (less? y x . nil)
                (scar p y . nil)
                (scar (cdr p . nil) x . nil)
                .
                nil)
              (scdr (cdr p . nil) nil . nil)
              p
              .
              nil)
             (is n 1 . nil)
             (with
              (p lst . nil)
              (= lst (cdr lst . nil) . nil)
              (scdr p nil . nil)
              p
              .
              nil)
             nil
             .
             nil)
           .
           nil)
          n
          .
          nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'mergesort)
   ((lambda ()
      (if (not (ar-false? (_bound 'mergesort)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'mergesort (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| mergesort|
                      (lambda (less? lst)
                        ((let ((| mergesort|
                                (lambda (n)
                                  (if (not (ar-false? (_<= n 1)))
                                    lst
                                    (ar-call-resolve-notation
                                     ((let ((| mergesort|
                                             (lambda (self)
                                               (begin
                                                 (let ((zz
                                                        (let ((| self|
                                                               (lambda (n)
                                                                 (if (not
                                                                      (ar-false?
                                                                       (_>
                                                                        n
                                                                        2)))
                                                                   ((let ((| self|
                                                                           (lambda (j)
                                                                             ((let ((| self|
                                                                                     (lambda (a)
                                                                                       ((let ((| self|
                                                                                               (lambda (b)
                                                                                                 ((let ((| self|
                                                                                                         (lambda ()
                                                                                                           (_merge
                                                                                                            less?
                                                                                                            a
                                                                                                            b))))
                                                                                                    | self|)))))
                                                                                          | self|)
                                                                                        (ar-call-resolve-notation
                                                                                         self
                                                                                         (_-
                                                                                          n
                                                                                          j))))))
                                                                                | self|)
                                                                              (ar-call-resolve-notation
                                                                               self
                                                                               j)))))
                                                                      | self|)
                                                                    (_/
                                                                     (if (not
                                                                          (ar-false?
                                                                           (_even
                                                                            n)))
                                                                       n
                                                                       (_-
                                                                        n
                                                                        1))
                                                                     2))
                                                                   (if (not
                                                                        (ar-false?
                                                                         (_is
                                                                          n
                                                                          2)))
                                                                     ((let ((| self|
                                                                             (lambda (x
                                                                                      y
                                                                                      p)
                                                                               ((let ((| self|
                                                                                       (lambda ()
                                                                                         (begin
                                                                                           (let ((zz
                                                                                                  (_cddr
                                                                                                   lst)))
                                                                                             (set! lst
                                                                                               zz)
                                                                                             zz)))))
                                                                                  | self|))
                                                                               (if (not
                                                                                    (ar-false?
                                                                                     (ar-call-resolve-notation
                                                                                      less?
                                                                                      y
                                                                                      x)))
                                                                                 ((let ((| self|
                                                                                         (lambda ()
                                                                                           (_scar
                                                                                            p
                                                                                            y)
                                                                                           (_scar
                                                                                            (_cdr
                                                                                             p)
                                                                                            x))))
                                                                                    | self|))
                                                                                 'nil)
                                                                               (_scdr
                                                                                (_cdr
                                                                                 p)
                                                                                'nil)
                                                                               p)))
                                                                        | self|)
                                                                      (_car
                                                                       lst)
                                                                      (_cadr
                                                                       lst)
                                                                      lst)
                                                                     (if (not
                                                                          (ar-false?
                                                                           (_is
                                                                            n
                                                                            1)))
                                                                       ((let ((| self|
                                                                               (lambda (p)
                                                                                 ((let ((| self|
                                                                                         (lambda ()
                                                                                           (begin
                                                                                             (let ((zz
                                                                                                    (_cdr
                                                                                                     lst)))
                                                                                               (set! lst
                                                                                                 zz)
                                                                                               zz)))))
                                                                                    | self|))
                                                                                 (_scdr
                                                                                  p
                                                                                  'nil)
                                                                                 p)))
                                                                          | self|)
                                                                        lst)
                                                                       'nil))))))
                                                          | self|)))
                                                   (set! self zz)
                                                   zz)))))
                                        | mergesort|)
                                      'nil)
                                     n)))))
                           | mergesort|)
                         (_len lst)))))
                 | mergesort|)))
          (namespace-set-variable-value! '_mergesort zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'mergesort)))
     (_setinfixop 'mergesort ((ar-coerce _infixop 'fn) 'mergesort))
     'nil)))


((lambda ()
   (_sref _sig '(less? x y . nil) 'merge)
   (_sref _help* 'nil 'merge)
   (_sref _source-file* _current-load-file* 'merge)
   (_sref
    _source*
    '(def
      merge
      (less? x y . nil)
      (if (no x . nil)
        y
        (no y . nil)
        x
        (let lup nil
          (assign
           lup
           (fn
            (r x y r-x? . nil)
            (if (less? (car y . nil) (car x . nil) . nil)
              (do (if r-x? (scdr r y . nil) . nil)
                  (if
                   (cdr y . nil)
                   (lup y x (cdr y . nil) nil . nil)
                   (scdr y x . nil)
                   .
                   nil)
                .
                nil)
              (do (if (no r-x? . nil) (scdr r x . nil) . nil)
                  (if
                   (cdr x . nil)
                   (lup x (cdr x . nil) y t . nil)
                   (scdr x y . nil)
                   .
                   nil)
                .
                nil)
              .
              nil)
            .
            nil)
           .
           nil)
          (if (less? (car y . nil) (car x . nil) . nil)
            (do (if
                 (cdr y . nil)
                 (lup y x (cdr y . nil) nil . nil)
                 (scdr y x . nil)
                 .
                 nil)
                y
              .
              nil)
            (do (if
                 (cdr x . nil)
                 (lup x (cdr x . nil) y t . nil)
                 (scdr x y . nil)
                 .
                 nil)
                x
              .
              nil)
            .
            nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'merge)
   ((lambda ()
      (if (not (ar-false? (_bound 'merge)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'merge (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| merge|
                      (lambda (less? x y)
                        (if (not (ar-false? (_no x)))
                          y
                          (if (not (ar-false? (_no y)))
                            x
                            ((let ((| merge|
                                    (lambda (lup)
                                      (begin
                                        (let ((zz
                                               (let ((| lup|
                                                      (lambda (r x y r-x?)
                                                        (if (not
                                                             (ar-false?
                                                              (ar-call-resolve-notation
                                                               less?
                                                               (_car y)
                                                               (_car x))))
                                                          ((let ((| lup|
                                                                  (lambda ()
                                                                    (if (not
                                                                         (ar-false?
                                                                          r-x?))
                                                                      (_scdr
                                                                       r
                                                                       y)
                                                                      'nil)
                                                                    (if (not
                                                                         (ar-false?
                                                                          (_cdr
                                                                           y)))
                                                                      (ar-call-resolve-notation
                                                                       lup
                                                                       y
                                                                       x
                                                                       (_cdr y)
                                                                       'nil)
                                                                      (_scdr
                                                                       y
                                                                       x)))))
                                                             | lup|))
                                                          ((let ((| lup|
                                                                  (lambda ()
                                                                    (if (not
                                                                         (ar-false?
                                                                          (_no
                                                                           r-x?)))
                                                                      (_scdr
                                                                       r
                                                                       x)
                                                                      'nil)
                                                                    (if (not
                                                                         (ar-false?
                                                                          (_cdr
                                                                           x)))
                                                                      (ar-call-resolve-notation
                                                                       lup
                                                                       x
                                                                       (_cdr x)
                                                                       y
                                                                       _t)
                                                                      (_scdr
                                                                       x
                                                                       y)))))
                                                             | lup|))))))
                                                 | lup|)))
                                          (set! lup zz)
                                          zz))
                                      (if (not
                                           (ar-false?
                                            (ar-call-resolve-notation
                                             less?
                                             (_car y)
                                             (_car x))))
                                        ((let ((| merge|
                                                (lambda ()
                                                  (if (not
                                                       (ar-false? (_cdr y)))
                                                    (ar-call-resolve-notation
                                                     lup
                                                     y
                                                     x
                                                     (_cdr y)
                                                     'nil)
                                                    (_scdr y x))
                                                  y)))
                                           | merge|))
                                        ((let ((| merge|
                                                (lambda ()
                                                  (if (not
                                                       (ar-false? (_cdr x)))
                                                    (ar-call-resolve-notation
                                                     lup
                                                     x
                                                     (_cdr x)
                                                     y
                                                     _t)
                                                    (_scdr x y))
                                                  x)))
                                           | merge|))))))
                               | merge|)
                             'nil))))))
                 | merge|)))
          (namespace-set-variable-value! '_merge zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'merge)))
     (_setinfixop 'merge ((ar-coerce _infixop 'fn) 'merge))
     'nil)))


((lambda ()
   (_sref _sig '(n f seq . nil) 'bestn)
   (_sref _help* 'nil 'bestn)
   (_sref _source-file* _current-load-file* 'bestn)
   (_sref
    _source*
    '(def bestn (n f seq . nil) (firstn n (sort f seq . nil) . nil) . nil)
    'bestn)
   ((lambda ()
      (if (not (ar-false? (_bound 'bestn)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'bestn (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| bestn| (lambda (n f seq) (_firstn n (_sort f seq)))))
                 | bestn|)))
          (namespace-set-variable-value! '_bestn zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'bestn)))
     (_setinfixop 'bestn ((ar-coerce _infixop 'fn) 'bestn))
     'nil)))


((lambda ()
   (_sref _sig '(seq pos . nil) 'split)
   (_sref _help* 'nil 'split)
   (_sref _source-file* _current-load-file* 'split)
   (_sref
    _source*
    '(def
      split
      (seq pos . nil)
      (list (cut seq 0 pos . nil) (cut seq pos . nil) . nil)
      .
      nil)
    'split)
   ((lambda ()
      (if (not (ar-false? (_bound 'split)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'split (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| split|
                      (lambda (seq pos)
                        (_list (_cut seq 0 pos) (_cut seq pos)))))
                 | split|)))
          (namespace-set-variable-value! '_split zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'split)))
     (_setinfixop 'split ((ar-coerce _infixop 'fn) 'split))
     'nil)))


((lambda ()
   (_sref _sig '(expr . nil) 'time)
   (_sref _help* 'nil 'time)
   (_sref _source-file* _current-load-file* 'time)
   (_sref
    _source*
    '(mac
      time
      (expr . nil)
      (w/uniq
       (t1 t2 . nil)
       (quasiquote
        (let (unquote t1 . nil)
          (msec . nil)
          (do1
           (unquote expr . nil)
           (let (unquote t2 . nil)
             (msec . nil)
             (prn
              "time: "
              (- (unquote t2 . nil) (unquote t1 . nil) . nil)
              " msec."
              .
              nil)
             .
             nil)
           .
           nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'time)
   ((lambda ()
      (if (not (ar-false? (_bound 'time)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'time (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| time|
                       (lambda (expr)
                         ((let ((| time|
                                 (lambda (t1 t2)
                                   `(let ,t1
                                      (msec)
                                      (do1
                                       ,expr
                                       (let ,t2
                                         (msec)
                                         (prn
                                          "time: "
                                          (- ,t2 ,t1)
                                          " msec.")))))))
                            | time|)
                          (_uniq)
                          (_uniq)))))
                  | time|))))
          (namespace-set-variable-value! '_time zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . nil) 'jtime)
   (_sref _help* 'nil 'jtime)
   (_sref _source-file* _current-load-file* 'jtime)
   (_sref
    _source*
    '(mac
      jtime
      (expr . nil)
      (quasiquote
       (do1 (quote ok . nil) (time (unquote expr . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'jtime)
   ((lambda ()
      (if (not (ar-false? (_bound 'jtime)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'jtime (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| jtime| (lambda (expr) `(do1 'ok (time ,expr)))))
                  | jtime|))))
          (namespace-set-variable-value! '_jtime zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . nil) 'time10)
   (_sref _help* 'nil 'time10)
   (_sref _source-file* _current-load-file* 'time10)
   (_sref
    _source*
    '(mac
      time10
      (expr . nil)
      (quasiquote (time (repeat 10 (unquote expr . nil) . nil) . nil) . nil)
      .
      nil)
    'time10)
   ((lambda ()
      (if (not (ar-false? (_bound 'time10)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'time10 (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| time10| (lambda (expr) `(time (repeat 10 ,expr)))))
                  | time10|))))
          (namespace-set-variable-value! '_time10 zz)
          zz))))))


((lambda ()
   (_sref _sig '(f xs ys . nil) 'union)
   (_sref _help* 'nil 'union)
   (_sref _source-file* _current-load-file* 'union)
   (_sref
    _source*
    '(def
      union
      (f xs ys . nil)
      (+
       xs
       (rem
        (fn (y . nil) (some (make-br-fn (f _ y . nil) . nil) xs . nil) . nil)
        ys
        .
        nil)
       .
       nil)
      .
      nil)
    'union)
   ((lambda ()
      (if (not (ar-false? (_bound 'union)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'union (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| union|
                      (lambda (f xs ys)
                        (_+
                         xs
                         (_rem
                          (let ((| union|
                                 (lambda (y)
                                   (_some
                                    (let ((| union|
                                           (lambda ()
                                             (ar-call-resolve-notation
                                              f
                                              __
                                              y))))
                                      | union|)
                                    xs))))
                            | union|)
                          ys)))))
                 | union|)))
          (namespace-set-variable-value! '_union zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'union)))
     (_setinfixop 'union ((ar-coerce _infixop 'fn) 'union))
     'nil)))


((lambda ()
   (begin
     (let ((zz (_table)))
       (namespace-set-variable-value! '_templates* zz)
       zz))))


((lambda ()
   (_sref _sig '(tem . fields) 'deftem)
   (_sref _help* 'nil 'deftem)
   (_sref _source-file* _current-load-file* 'deftem)
   (_sref
    _source*
    '(mac
      deftem
      (tem . fields)
      (withs
       (name
        (carif tem . nil)
        includes
        (if (acons tem . nil) (cdr tem . nil) . nil)
        .
        nil)
       (quasiquote
        (=
         (templates* (quote (unquote name . nil) . nil) . nil)
         (+
          (mappend
           templates*
           (quote (unquote (rev includes . nil) . nil) . nil)
           .
           nil)
          (list
           (unquote-splicing
            (map
             (fn
              ((k v . nil) . nil)
              (quasiquote
               (list
                (quote (unquote k . nil) . nil)
                (fn nil (unquote v . nil) . nil)
                .
                nil)
               .
               nil)
              .
              nil)
             (pair fields . nil)
             .
             nil)
            .
            nil)
           .
           nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'deftem)
   ((lambda ()
      (if (not (ar-false? (_bound 'deftem)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'deftem (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| deftem|
                       (lambda (tem . fields)
                         ((let ((| deftem|
                                 (lambda (name)
                                   ((let ((| deftem|
                                           (lambda (includes)
                                             ((let ((| deftem|
                                                     (lambda ()
                                                       `(=
                                                         (templates* ',name)
                                                         (+
                                                          (mappend
                                                           templates*
                                                           ',(_rev includes))
                                                          (list
                                                           ,@(ar-nil-terminate
                                                              (_map
                                                               (lambda g2649
                                                                 (let* ((k
                                                                         (ar-xcar
                                                                          (car
                                                                           g2649)))
                                                                        (v
                                                                         (ar-xcar
                                                                          (ar-xcdr
                                                                           (car
                                                                            g2649)))))
                                                                   `(list
                                                                     ',k
                                                                     (fn
                                                                      nil
                                                                      ,v))))
                                                               (_pair
                                                                fields)))))))))
                                                | deftem|)))))
                                      | deftem|)
                                    (if (not (ar-false? (_acons tem)))
                                      (_cdr tem)
                                      'nil)))))
                            | deftem|)
                          (_carif tem)))))
                  | deftem|))))
          (namespace-set-variable-value! '_deftem zz)
          zz))))))


((lambda ()
   (_sref _sig '(name . fields) 'addtem)
   (_sref _help* 'nil 'addtem)
   (_sref _source-file* _current-load-file* 'addtem)
   (_sref
    _source*
    '(mac
      addtem
      (name . fields)
      (quasiquote
       (=
        (templates* (quote (unquote name . nil) . nil) . nil)
        (union
         (fn (x y . nil) (is (car x . nil) (car y . nil) . nil) . nil)
         (list
          (unquote-splicing
           (map
            (fn
             ((k v . nil) . nil)
             (quasiquote
              (list
               (quote (unquote k . nil) . nil)
               (fn nil (unquote v . nil) . nil)
               .
               nil)
              .
              nil)
             .
             nil)
            (pair fields . nil)
            .
            nil)
           .
           nil)
          .
          nil)
         (templates* (quote (unquote name . nil) . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'addtem)
   ((lambda ()
      (if (not (ar-false? (_bound 'addtem)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'addtem (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| addtem|
                       (lambda (name . fields)
                         `(=
                           (templates* ',name)
                           (union
                            (fn (x y) (is (car x) (car y)))
                            (list
                             ,@(ar-nil-terminate
                                (_map
                                 (lambda g2650
                                   (let* ((k (ar-xcar (car g2650)))
                                          (v (ar-xcar (ar-xcdr (car g2650)))))
                                     `(list ',k (fn nil ,v))))
                                 (_pair fields))))
                            (templates* ',name))))))
                  | addtem|))))
          (namespace-set-variable-value! '_addtem zz)
          zz))))))


((lambda ()
   (_sref _sig '(tem . args) 'inst)
   (_sref _help* 'nil 'inst)
   (_sref _source-file* _current-load-file* 'inst)
   (_sref
    _source*
    '(def
      inst
      (tem . args)
      (let x (table . nil)
        (each
         (k v . nil)
         (if (acons tem . nil) tem (templates* tem . nil) . nil)
         (unless (no v . nil) (= (x k . nil) (v . nil) . nil) . nil)
         .
         nil)
        (each (k v . nil) (pair args . nil) (= (x k . nil) v . nil) . nil)
        x
        .
        nil)
      .
      nil)
    'inst)
   ((lambda ()
      (if (not (ar-false? (_bound 'inst)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'inst (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| inst|
                      (lambda (tem . args)
                        ((let ((| inst|
                                (lambda (x)
                                  (_walk
                                   (if (not (ar-false? (_acons tem)))
                                     tem
                                     (ar-call-resolve-notation
                                      _templates*
                                      tem))
                                   (lambda g2651
                                     (let* ((k (ar-xcar (car g2651)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2651)))))
                                       (if (not (ar-false? (_no (_no v))))
                                         ((let ((| inst|
                                                 (lambda ()
                                                   ((let ((| inst|
                                                           (lambda ()
                                                             (_atomic-invoke
                                                              (let ((| inst|
                                                                     (lambda ()
                                                                       ((let ((| inst|
                                                                               (lambda (g2652
                                                                                        g2654
                                                                                        g2655)
                                                                                 ((let ((| inst|
                                                                                         (lambda (g2653)
                                                                                           (_sref
                                                                                            g2652
                                                                                            g2653
                                                                                            g2654))))
                                                                                    | inst|)
                                                                                  g2655))))
                                                                          | inst|)
                                                                        x
                                                                        k
                                                                        (v)))))
                                                                | inst|)))))
                                                      | inst|)))))
                                            | inst|))
                                         'nil))))
                                  (_walk
                                   (_pair args)
                                   (lambda g2656
                                     (let* ((k (ar-xcar (car g2656)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2656)))))
                                       ((let ((| inst|
                                               (lambda ()
                                                 (_atomic-invoke
                                                  (let ((| inst|
                                                         (lambda ()
                                                           ((let ((| inst|
                                                                   (lambda (g2657
                                                                            g2659
                                                                            g2660)
                                                                     ((let ((| inst|
                                                                             (lambda (g2658)
                                                                               (_sref
                                                                                g2657
                                                                                g2658
                                                                                g2659))))
                                                                        | inst|)
                                                                      g2660))))
                                                              | inst|)
                                                            x
                                                            k
                                                            v))))
                                                    | inst|)))))
                                          | inst|)))))
                                  x)))
                           | inst|)
                         (_table)))))
                 | inst|)))
          (namespace-set-variable-value! '_inst zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'inst)))
     (_setinfixop 'inst ((ar-coerce _infixop 'fn) 'inst))
     'nil)))


((lambda ()
   (_sref _sig '(tem (o str (stdin . nil) . nil) . nil) 'temread)
   (_sref _help* 'nil 'temread)
   (_sref _source-file* _current-load-file* 'temread)
   (_sref
    _source*
    '(def
      temread
      (tem (o str (stdin . nil) . nil) . nil)
      (templatize tem (read str . nil) . nil)
      .
      nil)
    'temread)
   ((lambda ()
      (if (not (ar-false? (_bound 'temread)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'temread (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2661
                 (let* ((tem (car g2661))
                        (str
                         (if (pair? (ar-xcdr g2661))
                           (car (ar-xcdr g2661))
                           (_stdin))))
                   (_templatize tem (_read str))))))
          (namespace-set-variable-value! '_temread zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'temread)))
     (_setinfixop 'temread ((ar-coerce _infixop 'fn) 'temread))
     'nil)))


((lambda ()
   (_sref _sig '(tem raw . nil) 'templatize)
   (_sref _help* 'nil 'templatize)
   (_sref _source-file* _current-load-file* 'templatize)
   (_sref
    _source*
    '(def
      templatize
      (tem raw . nil)
      (with
       (x
        (inst tem . nil)
        fields
        (if (acons tem . nil) tem (templates* tem . nil) . nil)
        .
        nil)
       (each
        (k v . nil)
        raw
        (when (assoc k fields . nil) (= (x k . nil) v . nil) . nil)
        .
        nil)
       x
       .
       nil)
      .
      nil)
    'templatize)
   ((lambda ()
      (if (not (ar-false? (_bound 'templatize)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'templatize (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| templatize|
                      (lambda (tem raw)
                        ((let ((| templatize|
                                (lambda (x fields)
                                  (_walk
                                   raw
                                   (lambda g2662
                                     (let* ((k (ar-xcar (car g2662)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2662)))))
                                       (if (not (ar-false? (_assoc k fields)))
                                         ((let ((| templatize|
                                                 (lambda ()
                                                   ((let ((| templatize|
                                                           (lambda ()
                                                             (_atomic-invoke
                                                              (let ((| templatize|
                                                                     (lambda ()
                                                                       ((let ((| templatize|
                                                                               (lambda (g2663
                                                                                        g2665
                                                                                        g2666)
                                                                                 ((let ((| templatize|
                                                                                         (lambda (g2664)
                                                                                           (_sref
                                                                                            g2663
                                                                                            g2664
                                                                                            g2665))))
                                                                                    | templatize|)
                                                                                  g2666))))
                                                                          | templatize|)
                                                                        x
                                                                        k
                                                                        v))))
                                                                | templatize|)))))
                                                      | templatize|)))))
                                            | templatize|))
                                         'nil))))
                                  x)))
                           | templatize|)
                         (_inst tem)
                         (if (not (ar-false? (_acons tem)))
                           tem
                           (ar-call-resolve-notation _templates* tem))))))
                 | templatize|)))
          (namespace-set-variable-value! '_templatize zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'templatize)))
     (_setinfixop 'templatize ((ar-coerce _infixop 'fn) 'templatize))
     'nil)))


((lambda ()
   (_sref _sig '(tem file . nil) 'temload)
   (_sref _help* 'nil 'temload)
   (_sref _source-file* _current-load-file* 'temload)
   (_sref
    _source*
    '(def
      temload
      (tem file . nil)
      (w/infile i file (temread tem i . nil) . nil)
      .
      nil)
    'temload)
   ((lambda ()
      (if (not (ar-false? (_bound 'temload)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'temload (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| temload|
                      (lambda (tem file)
                        ((let ((| temload|
                                (lambda (i)
                                  (_protect
                                   (let ((| temload|
                                          (lambda ()
                                            ((let ((| temload|
                                                    (lambda ()
                                                      (_temread tem i))))
                                               | temload|)))))
                                     | temload|)
                                   (let ((| temload| (lambda () (_close i))))
                                     | temload|)))))
                           | temload|)
                         (_infile file)))))
                 | temload|)))
          (namespace-set-variable-value! '_temload zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'temload)))
     (_setinfixop 'temload ((ar-coerce _infixop 'fn) 'temload))
     'nil)))


((lambda ()
   (_sref _sig '(tem file . nil) 'temloadall)
   (_sref _help* 'nil 'temloadall)
   (_sref _source-file* _current-load-file* 'temloadall)
   (_sref
    _source*
    '(def
      temloadall
      (tem file . nil)
      (map
       (fn (pairs . nil) (templatize tem pairs . nil) . nil)
       (w/infile in file (readall in . nil) . nil)
       .
       nil)
      .
      nil)
    'temloadall)
   ((lambda ()
      (if (not (ar-false? (_bound 'temloadall)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'temloadall (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| temloadall|
                      (lambda (tem file)
                        (_map
                         (let ((| temloadall|
                                (lambda (pairs) (_templatize tem pairs))))
                           | temloadall|)
                         ((let ((| temloadall|
                                 (lambda (in)
                                   (_protect
                                    (let ((| temloadall|
                                           (lambda ()
                                             ((let ((| temloadall|
                                                     (lambda ()
                                                       (_readall in))))
                                                | temloadall|)))))
                                      | temloadall|)
                                    (let ((| temloadall|
                                           (lambda () (_close in))))
                                      | temloadall|)))))
                            | temloadall|)
                          (_infile file))))))
                 | temloadall|)))
          (namespace-set-variable-value! '_temloadall zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'temloadall)))
     (_setinfixop 'temloadall ((ar-coerce _infixop 'fn) 'temloadall))
     'nil)))


((lambda ()
   (_sref _sig '(n . nil) 'number)
   (_sref _help* 'nil 'number)
   (_sref _source-file* _current-load-file* 'number)
   (_sref
    _source*
    '(def
      number
      (n . nil)
      (in (type n . nil) (quote int . nil) (quote num . nil) . nil)
      .
      nil)
    'number)
   ((lambda ()
      (if (not (ar-false? (_bound 'number)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'number (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| number|
                      (lambda (n)
                        ((let ((| number|
                                (lambda (g2667)
                                  ((let ((| number|
                                          (lambda (g2668)
                                            (if (not (ar-false? g2668))
                                              g2668
                                              ((let ((| number|
                                                      (lambda (g2669)
                                                        (if (not
                                                             (ar-false? g2669))
                                                          g2669
                                                          'nil))))
                                                 | number|)
                                               (_is g2667 'num))))))
                                     | number|)
                                   (_is g2667 'int)))))
                           | number|)
                         (_type n)))))
                 | number|)))
          (namespace-set-variable-value! '_number zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'number)))
     (_setinfixop 'number ((ar-coerce _infixop 'fn) 'number))
     'nil)))


((lambda ()
   (_sref _sig '(t1 . nil) 'since)
   (_sref _help* 'nil 'since)
   (_sref _source-file* _current-load-file* 'since)
   (_sref
    _source*
    '(def since (t1 . nil) (- (seconds . nil) t1 . nil) . nil)
    'since)
   ((lambda ()
      (if (not (ar-false? (_bound 'since)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'since (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| since| (lambda (t1) (_- (_seconds) t1)))) | since|)))
          (namespace-set-variable-value! '_since zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'since)))
     (_setinfixop 'since ((ar-coerce _infixop 'fn) 'since))
     'nil)))


((lambda ()
   (_sref _sig '(t1 . nil) 'minutes-since)
   (_sref _help* 'nil 'minutes-since)
   (_sref _source-file* _current-load-file* 'minutes-since)
   (_sref
    _source*
    '(def minutes-since (t1 . nil) (/ (since t1 . nil) 60 . nil) . nil)
    'minutes-since)
   ((lambda ()
      (if (not (ar-false? (_bound 'minutes-since)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'minutes-since (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| minutes-since| (lambda (t1) (_/ (_since t1) 60))))
                 | minutes-since|)))
          (namespace-set-variable-value! '_minutes-since zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'minutes-since)))
     (_setinfixop 'minutes-since ((ar-coerce _infixop 'fn) 'minutes-since))
     'nil)))


((lambda ()
   (_sref _sig '(t1 . nil) 'hours-since)
   (_sref _help* 'nil 'hours-since)
   (_sref _source-file* _current-load-file* 'hours-since)
   (_sref
    _source*
    '(def hours-since (t1 . nil) (/ (since t1 . nil) 3600 . nil) . nil)
    'hours-since)
   ((lambda ()
      (if (not (ar-false? (_bound 'hours-since)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'hours-since (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| hours-since| (lambda (t1) (_/ (_since t1) 3600))))
                 | hours-since|)))
          (namespace-set-variable-value! '_hours-since zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'hours-since)))
     (_setinfixop 'hours-since ((ar-coerce _infixop 'fn) 'hours-since))
     'nil)))


((lambda ()
   (_sref _sig '(t1 . nil) 'days-since)
   (_sref _help* 'nil 'days-since)
   (_sref _source-file* _current-load-file* 'days-since)
   (_sref
    _source*
    '(def days-since (t1 . nil) (/ (since t1 . nil) 86400 . nil) . nil)
    'days-since)
   ((lambda ()
      (if (not (ar-false? (_bound 'days-since)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'days-since (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| days-since| (lambda (t1) (_/ (_since t1) 86400))))
                 | days-since|)))
          (namespace-set-variable-value! '_days-since zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'days-since)))
     (_setinfixop 'days-since ((ar-coerce _infixop 'fn) 'days-since))
     'nil)))


((lambda ()
   (_sref _sig '(timef valf . nil) 'cache)
   (_sref _help* 'nil 'cache)
   (_sref _source-file* _current-load-file* 'cache)
   (_sref
    _source*
    '(def
      cache
      (timef valf . nil)
      (with
       (cached nil gentime nil . nil)
       (fn
        nil
        (unless (and
                 cached
                 (< (since gentime . nil) (timef . nil) . nil)
                 .
                 nil)
          (= cached (valf . nil) gentime (seconds . nil) . nil)
          .
          nil)
        cached
        .
        nil)
       .
       nil)
      .
      nil)
    'cache)
   ((lambda ()
      (if (not (ar-false? (_bound 'cache)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'cache (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| cache|
                      (lambda (timef valf)
                        ((let ((| cache|
                                (lambda (cached gentime)
                                  (let ((| cache|
                                         (lambda ()
                                           (if (not
                                                (ar-false?
                                                 (_no
                                                  (if (not (ar-false? cached))
                                                    (_<
                                                     (_since gentime)
                                                     (timef))
                                                    'nil))))
                                             ((let ((| cache|
                                                     (lambda ()
                                                       ((let ((| cache|
                                                               (lambda ()
                                                                 (begin
                                                                   (let ((zz
                                                                          (valf)))
                                                                     (set! cached
                                                                       zz)
                                                                     zz))
                                                                 (begin
                                                                   (let ((zz
                                                                          (_seconds)))
                                                                     (set! gentime
                                                                       zz)
                                                                     zz)))))
                                                          | cache|)))))
                                                | cache|))
                                             'nil)
                                           cached)))
                                    | cache|))))
                           | cache|)
                         'nil
                         'nil))))
                 | cache|)))
          (namespace-set-variable-value! '_cache zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'cache)))
     (_setinfixop 'cache ((ar-coerce _infixop 'fn) 'cache))
     'nil)))


((lambda ()
   (_sref _sig '(name lasts . body) 'defcache)
   (_sref _help* 'nil 'defcache)
   (_sref _source-file* _current-load-file* 'defcache)
   (_sref
    _source*
    '(mac
      defcache
      (name lasts . body)
      (quasiquote
       (safeset
        (unquote name . nil)
        (cache
         (fn nil (unquote lasts . nil) . nil)
         (fn nil (unquote-splicing body . nil) . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'defcache)
   ((lambda ()
      (if (not (ar-false? (_bound 'defcache)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'defcache (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| defcache|
                       (lambda (name lasts . body)
                         `(safeset
                           ,name
                           (cache
                            (fn nil ,lasts)
                            (fn nil ,@(ar-nil-terminate body)))))))
                  | defcache|))))
          (namespace-set-variable-value! '_defcache zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . nil) 'errsafe)
   (_sref _help* 'nil 'errsafe)
   (_sref _source-file* _current-load-file* 'errsafe)
   (_sref
    _source*
    '(mac
      errsafe
      (expr . nil)
      (quasiquote
       (on-err
        (fn (c . nil) nil . nil)
        (fn nil (unquote expr . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'errsafe)
   ((lambda ()
      (if (not (ar-false? (_bound 'errsafe)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'errsafe (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| errsafe|
                       (lambda (expr) `(on-err (fn (c) nil) (fn nil ,expr)))))
                  | errsafe|))))
          (namespace-set-variable-value! '_errsafe zz)
          zz))))))


((lambda ()
   (_sref _sig '(arg . nil) 'saferead)
   (_sref _help* 'nil 'saferead)
   (_sref _source-file* _current-load-file* 'saferead)
   (_sref
    _source*
    '(def saferead (arg . nil) (errsafe:read arg . nil) . nil)
    'saferead)
   ((lambda ()
      (if (not (ar-false? (_bound 'saferead)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'saferead (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| saferead|
                      (lambda (arg)
                        (_on-err
                         (let ((| saferead| (lambda (c) 'nil))) | saferead|)
                         (let ((| saferead| (lambda () (_read arg))))
                           | saferead|)))))
                 | saferead|)))
          (namespace-set-variable-value! '_saferead zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'saferead)))
     (_setinfixop 'saferead ((ar-coerce _infixop 'fn) 'saferead))
     'nil)))


((lambda ()
   (_sref _sig '(filename . nil) 'safe-load-table)
   (_sref _help* 'nil 'safe-load-table)
   (_sref _source-file* _current-load-file* 'safe-load-table)
   (_sref
    _source*
    '(def
      safe-load-table
      (filename . nil)
      (or (errsafe:load-table filename . nil) (table . nil) . nil)
      .
      nil)
    'safe-load-table)
   ((lambda ()
      (if (not (ar-false? (_bound 'safe-load-table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'safe-load-table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| safe-load-table|
                      (lambda (filename)
                        ((let ((| safe-load-table|
                                (lambda (g2670)
                                  (if (not (ar-false? g2670))
                                    g2670
                                    ((let ((| safe-load-table|
                                            (lambda (g2671)
                                              (if (not (ar-false? g2671))
                                                g2671
                                                'nil))))
                                       | safe-load-table|)
                                     (_table))))))
                           | safe-load-table|)
                         (_on-err
                          (let ((| g2670| (lambda (c) 'nil))) | g2670|)
                          (let ((| g2670| (lambda () (_load-table filename))))
                            | g2670|))))))
                 | safe-load-table|)))
          (namespace-set-variable-value! '_safe-load-table zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'safe-load-table)))
     (_setinfixop 'safe-load-table ((ar-coerce _infixop 'fn) 'safe-load-table))
     'nil)))


((lambda ()
   (_sref _sig '(path . nil) 'ensure-dir)
   (_sref _help* 'nil 'ensure-dir)
   (_sref _source-file* _current-load-file* 'ensure-dir)
   (_sref
    _source*
    '(def
      ensure-dir
      (path . nil)
      (unless (dir-exists path . nil)
        (system (string "mkdir -p " path . nil) . nil)
        .
        nil)
      .
      nil)
    'ensure-dir)
   ((lambda ()
      (if (not (ar-false? (_bound 'ensure-dir)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ensure-dir (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| ensure-dir|
                      (lambda (path)
                        (if (not (ar-false? (_no (_dir-exists path))))
                          ((let ((| ensure-dir|
                                  (lambda ()
                                    (_system (_string "mkdir -p " path)))))
                             | ensure-dir|))
                          'nil))))
                 | ensure-dir|)))
          (namespace-set-variable-value! '_ensure-dir zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ensure-dir)))
     (_setinfixop 'ensure-dir ((ar-coerce _infixop 'fn) 'ensure-dir))
     'nil)))


((lambda ()
   (_sref _sig '((o s (seconds . nil) . nil) . nil) 'date)
   (_sref _help* 'nil 'date)
   (_sref _source-file* _current-load-file* 'date)
   (_sref
    _source*
    '(def
      date
      ((o s (seconds . nil) . nil) . nil)
      (rev (nthcdr 3 (timedate s . nil) . nil) . nil)
      .
      nil)
    'date)
   ((lambda ()
      (if (not (ar-false? (_bound 'date)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'date (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2672
                 (let* ((s (if (pair? g2672) (car g2672) (_seconds))))
                   (_rev (_nthcdr 3 (_timedate s)))))))
          (namespace-set-variable-value! '_date zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'date)))
     (_setinfixop 'date ((ar-coerce _infixop 'fn) 'date))
     'nil)))


((lambda ()
   (_sref
    _sig
    '(year mon day (o hour 0 . nil) (o min 0 . nil) (o sec 0 . nil) . nil)
    'unixtime)
   (_sref _help* 'nil 'unixtime)
   (_sref _source-file* _current-load-file* 'unixtime)
   (_sref
    _source*
    '(def
      unixtime
      (year mon day (o hour 0 . nil) (o min 0 . nil) (o sec 0 . nil) . nil)
      (if (<= mon 2 . nil)
        (unixtime (- year 1 . nil) (+ mon 12 . nil) day hour min sec . nil)
        (((((year
             /
             4
             -
             year
             /
             100
             +
             year
             /
             400
             +
             367
             *
             (mon - 2 . nil)
             /
             12
             +
             day
             .
             nil)
            +
            year
            *
            365
            -
            719499
            .
            nil)
           *
           24
           +
           hour
           .
           nil)
          *
          60
          +
          min
          .
          nil)
         *
         60
         +
         sec
         .
         nil)
        .
        nil)
      .
      nil)
    'unixtime)
   ((lambda ()
      (if (not (ar-false? (_bound 'unixtime)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'unixtime (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2673
                 (let* ((year (car g2673))
                        (mon (car (ar-xcdr g2673)))
                        (day (car (ar-xcdr (ar-xcdr g2673))))
                        (hour
                         (if (pair? (ar-xcdr (ar-xcdr (ar-xcdr g2673))))
                           (car (ar-xcdr (ar-xcdr (ar-xcdr g2673))))
                           0))
                        (min
                         (if (pair?
                              (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2673)))))
                           (car (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2673)))))
                           0))
                        (sec
                         (if (pair?
                              (ar-xcdr
                               (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2673))))))
                           (car
                            (ar-xcdr
                             (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2673))))))
                           0)))
                   (if (not (ar-false? (_<= mon 2)))
                     (_unixtime (_- year 1) (_+ mon 12) day hour min sec)
                     (ar-call-resolve-notation
                      (ar-call-resolve-notation
                       (ar-call-resolve-notation
                        (ar-call-resolve-notation
                         (ar-call-resolve-notation
                          year
                          _/
                          4
                          _-
                          year
                          _/
                          100
                          _+
                          year
                          _/
                          400
                          _+
                          367
                          _*
                          (ar-call-resolve-notation mon _- 2)
                          _/
                          12
                          _+
                          day)
                         _+
                         year
                         _*
                         365
                         _-
                         719499)
                        _*
                        24
                        _+
                        hour)
                       _*
                       60
                       _+
                       min)
                      _*
                      60
                      _+
                      sec))))))
          (namespace-set-variable-value! '_unixtime zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'unixtime)))
     (_setinfixop 'unixtime ((ar-coerce _infixop 'fn) 'unixtime))
     'nil)))


((lambda ()
   (_sref _sig '((o s (seconds . nil) . nil) . nil) 'datestring)
   (_sref _help* 'nil 'datestring)
   (_sref _source-file* _current-load-file* 'datestring)
   (_sref
    _source*
    '(def
      datestring
      ((o s (seconds . nil) . nil) . nil)
      (let (y m d . nil)
        (date s . nil)
        (string
         y
         "-"
         (if (< m 10 . nil) "0" . nil)
         m
         "-"
         (if (< d 10 . nil) "0" . nil)
         d
         .
         nil)
        .
        nil)
      .
      nil)
    'datestring)
   ((lambda ()
      (if (not (ar-false? (_bound 'datestring)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'datestring (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2674
                 (let* ((s (if (pair? g2674) (car g2674) (_seconds))))
                   ((lambda g2675
                      (let* ((y (ar-xcar (car g2675)))
                             (m (ar-xcar (ar-xcdr (car g2675))))
                             (d (ar-xcar (ar-xcdr (ar-xcdr (car g2675))))))
                        (_string
                         y
                         "-"
                         (if (not (ar-false? (_< m 10))) "0" 'nil)
                         m
                         "-"
                         (if (not (ar-false? (_< d 10))) "0" 'nil)
                         d)))
                    (_date s))))))
          (namespace-set-variable-value! '_datestring zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'datestring)))
     (_setinfixop 'datestring ((ar-coerce _infixop 'fn) 'datestring))
     'nil)))


((lambda ()
   (_sref _sig '(test x . nil) 'count)
   (_sref _help* 'nil 'count)
   (_sref _source-file* _current-load-file* 'count)
   (_sref
    _source*
    '(def
      count
      (test x . nil)
      (with
       (n 0 testf (testify test . nil) . nil)
       (each elt x (if (testf elt . nil) (++ n . nil) . nil) . nil)
       n
       .
       nil)
      .
      nil)
    'count)
   ((lambda ()
      (if (not (ar-false? (_bound 'count)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'count (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| count|
                      (lambda (test x)
                        ((let ((| count|
                                (lambda (n testf)
                                  (_walk
                                   x
                                   (let ((| count|
                                          (lambda (elt)
                                            (if (not
                                                 (ar-false?
                                                  (ar-call-resolve-notation
                                                   testf
                                                   elt)))
                                              ((let ((| count|
                                                      (lambda ()
                                                        (begin
                                                          (let ((zz (_+ n 1)))
                                                            (set! n zz)
                                                            zz)))))
                                                 | count|))
                                              'nil))))
                                     | count|))
                                  n)))
                           | count|)
                         0
                         (_testify test)))))
                 | count|)))
          (namespace-set-variable-value! '_count zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'count)))
     (_setinfixop 'count ((ar-coerce _infixop 'fn) 'count))
     'nil)))


((lambda ()
   (_sref _sig '(str (o limit 80 . nil) . nil) 'ellipsize)
   (_sref _help* 'nil 'ellipsize)
   (_sref _source-file* _current-load-file* 'ellipsize)
   (_sref
    _source*
    '(def
      ellipsize
      (str (o limit 80 . nil) . nil)
      (if (<= (len str . nil) limit . nil)
        str
        (+ (cut str 0 limit . nil) "..." . nil)
        .
        nil)
      .
      nil)
    'ellipsize)
   ((lambda ()
      (if (not (ar-false? (_bound 'ellipsize)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ellipsize (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2676
                 (let* ((str (car g2676))
                        (limit
                         (if (pair? (ar-xcdr g2676))
                           (car (ar-xcdr g2676))
                           80)))
                   (if (not (ar-false? (_<= (_len str) limit)))
                     str
                     (_+ (_cut str 0 limit) "..."))))))
          (namespace-set-variable-value! '_ellipsize zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ellipsize)))
     (_setinfixop 'ellipsize ((ar-coerce _infixop 'fn) 'ellipsize))
     'nil)))


((lambda ()
   (_sref _sig '(seq . nil) 'rand-elt)
   (_sref _help* 'nil 'rand-elt)
   (_sref _source-file* _current-load-file* 'rand-elt)
   (_sref
    _source*
    '(def rand-elt (seq . nil) (seq (rand (len seq . nil) . nil) . nil) . nil)
    'rand-elt)
   ((lambda ()
      (if (not (ar-false? (_bound 'rand-elt)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rand-elt (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| rand-elt|
                      (lambda (seq)
                        (ar-call-resolve-notation seq (_rand (_len seq))))))
                 | rand-elt|)))
          (namespace-set-variable-value! '_rand-elt zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rand-elt)))
     (_setinfixop 'rand-elt ((ar-coerce _infixop 'fn) 'rand-elt))
     'nil)))


((lambda ()
   (_sref _sig '(len (o init 0 . nil) . nil) 'randseq)
   (_sref _help* 'nil 'randseq)
   (_sref _source-file* _current-load-file* 'randseq)
   (_sref
    _source*
    '(def
      randseq
      (len (o init 0 . nil) . nil)
      (and (len > 0 . nil)
           (cons
            init
            (randseq (len - 1 . nil) (+ init (rand 3 . nil) -1 . nil) . nil)
            .
            nil)
           .
           nil)
      .
      nil)
    'randseq)
   ((lambda ()
      (if (not (ar-false? (_bound 'randseq)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'randseq (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2677
                 (let* ((len (car g2677))
                        (init (if (pair? (ar-xcdr g2677))
                                (car (ar-xcdr g2677))
                                0)))
                   (if (not (ar-false? (ar-call-resolve-notation len _> 0)))
                     (_cons
                      init
                      (_randseq
                       (ar-call-resolve-notation len _- 1)
                       (_+ init (_rand 3) -1)))
                     'nil)))))
          (namespace-set-variable-value! '_randseq zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'randseq)))
     (_setinfixop 'randseq ((ar-coerce _infixop 'fn) 'randseq))
     'nil)))


((lambda ()
   (_sref _sig '(test . body) 'until)
   (_sref _help* 'nil 'until)
   (_sref _source-file* _current-load-file* 'until)
   (_sref
    _source*
    '(mac
      until
      (test . body)
      (quasiquote
       (while
        (no (unquote test . nil) . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'until)
   ((lambda ()
      (if (not (ar-false? (_bound 'until)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'until (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| until|
                       (lambda (test . body)
                         `(while (no ,test) ,@(ar-nil-terminate body)))))
                  | until|))))
          (namespace-set-variable-value! '_until zz)
          zz))))))


((lambda ()
   (_sref _sig '(x y seq (o i 0 . nil) . nil) 'before)
   (_sref _help* 'nil 'before)
   (_sref _source-file* _current-load-file* 'before)
   (_sref
    _source*
    '(def
      before
      (x y seq (o i 0 . nil) . nil)
      (with
       (xp (pos x seq i . nil) yp (pos y seq i . nil) . nil)
       (and xp (or (no yp . nil) (< xp yp . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'before)
   ((lambda ()
      (if (not (ar-false? (_bound 'before)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'before (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2678
                 (let* ((x (car g2678))
                        (y (car (ar-xcdr g2678)))
                        (seq (car (ar-xcdr (ar-xcdr g2678))))
                        (i
                         (if (pair? (ar-xcdr (ar-xcdr (ar-xcdr g2678))))
                           (car (ar-xcdr (ar-xcdr (ar-xcdr g2678))))
                           0)))
                   ((let ((| before|
                           (lambda (xp yp)
                             (if (not (ar-false? xp))
                               ((let ((| before|
                                       (lambda (g2679)
                                         (if (not (ar-false? g2679))
                                           g2679
                                           ((let ((| before|
                                                   (lambda (g2680)
                                                     (if (not
                                                          (ar-false? g2680))
                                                       g2680
                                                       'nil))))
                                              | before|)
                                            (_< xp yp))))))
                                  | before|)
                                (_no yp))
                               'nil))))
                      | before|)
                    (_pos x seq i)
                    (_pos y seq i))))))
          (namespace-set-variable-value! '_before zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'before)))
     (_setinfixop 'before ((ar-coerce _infixop 'fn) 'before))
     'nil)))


((lambda ()
   (_sref _sig 'fns 'orf)
   (_sref _help* 'nil 'orf)
   (_sref _source-file* _current-load-file* 'orf)
   (_sref
    _source*
    '(def
      orf
      fns
      (fn
       args
       ((afn
         (fs . nil)
         (and fs
              (or (apply (car fs . nil) args . nil)
                  (self (cdr fs . nil) . nil)
                  .
                  nil)
              .
              nil)
         .
         nil)
        fns
        .
        nil)
       .
       nil)
      .
      nil)
    'orf)
   ((lambda ()
      (if (not (ar-false? (_bound 'orf)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'orf (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| orf|
                      (lambda fns
                        (let ((| orf|
                               (lambda args
                                 (ar-call-resolve-notation
                                  ((let ((| orf|
                                          (lambda (self)
                                            (begin
                                              (let ((zz
                                                     (let ((| self|
                                                            (lambda (fs)
                                                              (if (not
                                                                   (ar-false?
                                                                    fs))
                                                                ((let ((| self|
                                                                        (lambda (g2681)
                                                                          (if (not
                                                                               (ar-false?
                                                                                g2681))
                                                                            g2681
                                                                            ((let ((| self|
                                                                                    (lambda (g2682)
                                                                                      (if (not
                                                                                           (ar-false?
                                                                                            g2682))
                                                                                        g2682
                                                                                        'nil))))
                                                                               | self|)
                                                                             (ar-call-resolve-notation
                                                                              self
                                                                              (_cdr
                                                                               fs)))))))
                                                                   | self|)
                                                                 (_apply
                                                                  (_car fs)
                                                                  args))
                                                                'nil))))
                                                       | self|)))
                                                (set! self zz)
                                                zz)))))
                                     | orf|)
                                   'nil)
                                  fns))))
                          | orf|))))
                 | orf|)))
          (namespace-set-variable-value! '_orf zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'orf)))
     (_setinfixop 'orf ((ar-coerce _infixop 'fn) 'orf))
     'nil)))


((lambda ()
   (_sref _sig 'fns 'andf)
   (_sref _help* 'nil 'andf)
   (_sref _source-file* _current-load-file* 'andf)
   (_sref
    _source*
    '(def
      andf
      fns
      (fn
       args
       ((afn
         (fs . nil)
         (if (no fs . nil)
           t
           (no (cdr fs . nil) . nil)
           (apply (car fs . nil) args . nil)
           (and (apply (car fs . nil) args . nil)
                (self (cdr fs . nil) . nil)
                .
                nil)
           .
           nil)
         .
         nil)
        fns
        .
        nil)
       .
       nil)
      .
      nil)
    'andf)
   ((lambda ()
      (if (not (ar-false? (_bound 'andf)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'andf (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| andf|
                      (lambda fns
                        (let ((| andf|
                               (lambda args
                                 (ar-call-resolve-notation
                                  ((let ((| andf|
                                          (lambda (self)
                                            (begin
                                              (let ((zz
                                                     (let ((| self|
                                                            (lambda (fs)
                                                              (if (not
                                                                   (ar-false?
                                                                    (_no fs)))
                                                                _t
                                                                (if (not
                                                                     (ar-false?
                                                                      (_no
                                                                       (_cdr
                                                                        fs))))
                                                                  (_apply
                                                                   (_car fs)
                                                                   args)
                                                                  (if (not
                                                                       (ar-false?
                                                                        (_apply
                                                                         (_car
                                                                          fs)
                                                                         args)))
                                                                    (ar-call-resolve-notation
                                                                     self
                                                                     (_cdr fs))
                                                                    'nil))))))
                                                       | self|)))
                                                (set! self zz)
                                                zz)))))
                                     | andf|)
                                   'nil)
                                  fns))))
                          | andf|))))
                 | andf|)))
          (namespace-set-variable-value! '_andf zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'andf)))
     (_setinfixop 'andf ((ar-coerce _infixop 'fn) 'andf))
     'nil)))


((lambda ()
   (_sref _sig '(i s . nil) 'atend)
   (_sref _help* 'nil 'atend)
   (_sref _source-file* _current-load-file* 'atend)
   (_sref
    _source*
    '(def atend (i s . nil) (> i (- (len s . nil) 2 . nil) . nil) . nil)
    'atend)
   ((lambda ()
      (if (not (ar-false? (_bound 'atend)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'atend (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| atend| (lambda (i s) (_> i (_- (_len s) 2)))))
                 | atend|)))
          (namespace-set-variable-value! '_atend zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'atend)))
     (_setinfixop 'atend ((ar-coerce _infixop 'fn) 'atend))
     'nil)))


((lambda ()
   (_sref _sig '(x y . nil) 'multiple)
   (_sref _help* 'nil 'multiple)
   (_sref _source-file* _current-load-file* 'multiple)
   (_sref
    _source*
    '(def multiple (x y . nil) (is 0 (mod x y . nil) . nil) . nil)
    'multiple)
   ((lambda ()
      (if (not (ar-false? (_bound 'multiple)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'multiple (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| multiple| (lambda (x y) (_is 0 (_mod x y)))))
                 | multiple|)))
          (namespace-set-variable-value! '_multiple zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'multiple)))
     (_setinfixop 'multiple ((ar-coerce _infixop 'fn) 'multiple))
     'nil)))


((lambda ()
   (_sref _sig 'args 'nor)
   (_sref _help* 'nil 'nor)
   (_sref _source-file* _current-load-file* 'nor)
   (_sref
    _source*
    '(mac
      nor
      args
      (quasiquote (no (or (unquote-splicing args . nil) . nil) . nil) . nil)
      .
      nil)
    'nor)
   ((lambda ()
      (if (not (ar-false? (_bound 'nor)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'nor (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| nor|
                       (lambda args `(no (or ,@(ar-nil-terminate args))))))
                  | nor|))))
          (namespace-set-variable-value! '_nor zz)
          zz))))))


((lambda ()
   (_sref _sig '(comparer scorer . nil) 'compare)
   (_sref _help* 'nil 'compare)
   (_sref _source-file* _current-load-file* 'compare)
   (_sref
    _source*
    '(def
      compare
      (comparer scorer . nil)
      (fn (x y . nil) (comparer (scorer x . nil) (scorer y . nil) . nil) . nil)
      .
      nil)
    'compare)
   ((lambda ()
      (if (not (ar-false? (_bound 'compare)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'compare (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| compare|
                      (lambda (comparer scorer)
                        (let ((| compare|
                               (lambda (x y)
                                 (ar-call-resolve-notation
                                  comparer
                                  (ar-call-resolve-notation scorer x)
                                  (ar-call-resolve-notation scorer y)))))
                          | compare|))))
                 | compare|)))
          (namespace-set-variable-value! '_compare zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'compare)))
     (_setinfixop 'compare ((ar-coerce _infixop 'fn) 'compare))
     'nil)))


((lambda ()
   (_sref _sig '(f . nil) 'only)
   (_sref _help* 'nil 'only)
   (_sref _source-file* _current-load-file* 'only)
   (_sref
    _source*
    '(def
      only
      (f . nil)
      (fn args (if (car args . nil) (apply f args . nil) . nil) . nil)
      .
      nil)
    'only)
   ((lambda ()
      (if (not (ar-false? (_bound 'only)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'only (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| only|
                      (lambda (f)
                        (let ((| only|
                               (lambda args
                                 (if (not (ar-false? (_car args)))
                                   (_apply f args)
                                   'nil))))
                          | only|))))
                 | only|)))
          (namespace-set-variable-value! '_only zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'only)))
     (_setinfixop 'only ((ar-coerce _infixop 'fn) 'only))
     'nil)))


((lambda ()
   (_sref _sig '(f x y . nil) 'conswhen)
   (_sref _help* 'nil 'conswhen)
   (_sref _source-file* _current-load-file* 'conswhen)
   (_sref
    _source*
    '(mac
      conswhen
      (f x y . nil)
      (w/uniq
       (gf gx . nil)
       (quasiquote
        (with
         ((unquote gf . nil)
          (unquote f . nil)
          (unquote gx . nil)
          (unquote x . nil)
          .
          nil)
         (if ((unquote gf . nil) (unquote gx . nil) . nil)
           (cons (unquote gx . nil) (unquote y . nil) . nil)
           (unquote y . nil)
           .
           nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'conswhen)
   ((lambda ()
      (if (not (ar-false? (_bound 'conswhen)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'conswhen (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| conswhen|
                       (lambda (f x y)
                         ((let ((| conswhen|
                                 (lambda (gf gx)
                                   `(with
                                     (,gf ,f ,gx ,x)
                                     (if (,gf ,gx) (cons ,gx ,y) ,y)))))
                            | conswhen|)
                          (_uniq)
                          (_uniq)))))
                  | conswhen|))))
          (namespace-set-variable-value! '_conswhen zz)
          zz))))))


((lambda ()
   (_sref _sig '(n f xs . nil) 'retrieve)
   (_sref _help* 'nil 'retrieve)
   (_sref _source-file* _current-load-file* 'retrieve)
   (_sref
    _source*
    '(def
      retrieve
      (n f xs . nil)
      (if (no n . nil)
        (keep f xs . nil)
        (or (<= n 0 . nil) (no xs . nil) . nil)
        nil
        (f (car xs . nil) . nil)
        (cons
         (car xs . nil)
         (retrieve (- n 1 . nil) f (cdr xs . nil) . nil)
         .
         nil)
        (retrieve n f (cdr xs . nil) . nil)
        .
        nil)
      .
      nil)
    'retrieve)
   ((lambda ()
      (if (not (ar-false? (_bound 'retrieve)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'retrieve (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| retrieve|
                      (lambda (n f xs)
                        (if (not (ar-false? (_no n)))
                          (_keep f xs)
                          (if (not
                               (ar-false?
                                ((let ((| retrieve|
                                        (lambda (g2683)
                                          (if (not (ar-false? g2683))
                                            g2683
                                            ((let ((| retrieve|
                                                    (lambda (g2684)
                                                      (if (not
                                                           (ar-false? g2684))
                                                        g2684
                                                        'nil))))
                                               | retrieve|)
                                             (_no xs))))))
                                   | retrieve|)
                                 (_<= n 0))))
                            'nil
                            (if (not
                                 (ar-false?
                                  (ar-call-resolve-notation f (_car xs))))
                              (_cons
                               (_car xs)
                               (_retrieve (_- n 1) f (_cdr xs)))
                              (_retrieve n f (_cdr xs))))))))
                 | retrieve|)))
          (namespace-set-variable-value! '_retrieve zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'retrieve)))
     (_setinfixop 'retrieve ((ar-coerce _infixop 'fn) 'retrieve))
     'nil)))


((lambda ()
   (_sref _sig '(xs . nil) 'dedup)
   (_sref _help* 'nil 'dedup)
   (_sref _source-file* _current-load-file* 'dedup)
   (_sref
    _source*
    '(def
      dedup
      (xs . nil)
      (with
       (h (table . nil) acc nil . nil)
       (each
        x
        xs
        (unless (h x . nil) (push x acc . nil) (set (h x . nil) . nil) . nil)
        .
        nil)
       (rev acc . nil)
       .
       nil)
      .
      nil)
    'dedup)
   ((lambda ()
      (if (not (ar-false? (_bound 'dedup)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'dedup (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| dedup|
                      (lambda (xs)
                        ((let ((| dedup|
                                (lambda (h acc)
                                  (_walk
                                   xs
                                   (let ((| dedup|
                                          (lambda (x)
                                            (if (not
                                                 (ar-false?
                                                  (_no
                                                   (ar-call-resolve-notation
                                                    h
                                                    x))))
                                              ((let ((| dedup|
                                                      (lambda ()
                                                        ((let ((| dedup|
                                                                (lambda (g2685)
                                                                  (_atomic-invoke
                                                                   (let ((| dedup|
                                                                          (lambda ()
                                                                            ((let ((| dedup|
                                                                                    (lambda (g2686)
                                                                                      ((let ((| dedup|
                                                                                              (lambda ()
                                                                                                ((let ((| dedup|
                                                                                                        (lambda (g2687)
                                                                                                          (begin
                                                                                                            (let ((zz
                                                                                                                   g2687))
                                                                                                              (set! acc
                                                                                                                zz)
                                                                                                              zz)))))
                                                                                                   | dedup|)
                                                                                                 (_cons
                                                                                                  g2685
                                                                                                  g2686)))))
                                                                                         | dedup|)))))
                                                                               | dedup|)
                                                                             acc))))
                                                                     | dedup|)))))
                                                           | dedup|)
                                                         x)
                                                        ((let ((| dedup|
                                                                (lambda ()
                                                                  ((let ((| dedup|
                                                                          (lambda ()
                                                                            (_atomic-invoke
                                                                             (let ((| dedup|
                                                                                    (lambda ()
                                                                                      ((let ((| dedup|
                                                                                              (lambda (g2688
                                                                                                       g2690
                                                                                                       g2691)
                                                                                                ((let ((| dedup|
                                                                                                        (lambda (g2689)
                                                                                                          (_sref
                                                                                                           g2688
                                                                                                           g2689
                                                                                                           g2690))))
                                                                                                   | dedup|)
                                                                                                 g2691))))
                                                                                         | dedup|)
                                                                                       h
                                                                                       x
                                                                                       _t))))
                                                                               | dedup|)))))
                                                                     | dedup|)))))
                                                           | dedup|)))))
                                                 | dedup|))
                                              'nil))))
                                     | dedup|))
                                  (_rev acc))))
                           | dedup|)
                         (_table)
                         'nil))))
                 | dedup|)))
          (namespace-set-variable-value! '_dedup zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'dedup)))
     (_setinfixop 'dedup ((ar-coerce _infixop 'fn) 'dedup))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'single)
   (_sref _help* 'nil 'single)
   (_sref _source-file* _current-load-file* 'single)
   (_sref
    _source*
    '(def
      single
      (x . nil)
      (and (acons x . nil) (no (cdr x . nil) . nil) . nil)
      .
      nil)
    'single)
   ((lambda ()
      (if (not (ar-false? (_bound 'single)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'single (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| single|
                      (lambda (x)
                        (if (not (ar-false? (_acons x)))
                          (_no (_cdr x))
                          'nil))))
                 | single|)))
          (namespace-set-variable-value! '_single zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'single)))
     (_setinfixop 'single ((ar-coerce _infixop 'fn) 'single))
     'nil)))


((lambda ()
   (_sref _sig '(x ys . nil) 'intersperse)
   (_sref _help* 'nil 'intersperse)
   (_sref _source-file* _current-load-file* 'intersperse)
   (_sref
    _source*
    '(def
      intersperse
      (x ys . nil)
      (and ys
           (cons
            (car ys . nil)
            (mappend (make-br-fn (list x _ . nil) . nil) (cdr ys . nil) . nil)
            .
            nil)
           .
           nil)
      .
      nil)
    'intersperse)
   ((lambda ()
      (if (not (ar-false? (_bound 'intersperse)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'intersperse (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| intersperse|
                      (lambda (x ys)
                        (if (not (ar-false? ys))
                          (_cons
                           (_car ys)
                           (_mappend
                            (let ((| intersperse| (lambda () (_list x __))))
                              | intersperse|)
                            (_cdr ys)))
                          'nil))))
                 | intersperse|)))
          (namespace-set-variable-value! '_intersperse zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'intersperse)))
     (_setinfixop 'intersperse ((ar-coerce _infixop 'fn) 'intersperse))
     'nil)))


((lambda ()
   (_sref _sig '(seq (o c (table . nil) . nil) . nil) 'counts)
   (_sref _help* 'nil 'counts)
   (_sref _source-file* _current-load-file* 'counts)
   (_sref
    _source*
    '(def
      counts
      (seq (o c (table . nil) . nil) . nil)
      (if (no seq . nil)
        c
        (do (++ (c (car seq . nil) 0 . nil) . nil)
            (counts (cdr seq . nil) c . nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'counts)
   ((lambda ()
      (if (not (ar-false? (_bound 'counts)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'counts (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2692
                 (let* ((seq (car g2692))
                        (c
                         (if (pair? (ar-xcdr g2692))
                           (car (ar-xcdr g2692))
                           (_table))))
                   (if (not (ar-false? (_no seq)))
                     c
                     ((let ((| counts|
                             (lambda ()
                               (_atomic-invoke
                                (let ((| counts|
                                       (lambda ()
                                         ((let ((| counts|
                                                 (lambda (g2694)
                                                   ((let ((| counts|
                                                           (lambda (g2696)
                                                             ((let ((| counts|
                                                                     (lambda (g2697)
                                                                       ((let ((| counts|
                                                                               (lambda (g2693)
                                                                                 ((let ((| counts|
                                                                                         (lambda ()
                                                                                           ((let ((| counts|
                                                                                                   (lambda (g2695)
                                                                                                     (_sref
                                                                                                      g2694
                                                                                                      g2695
                                                                                                      g2696))))
                                                                                              | counts|)
                                                                                            (_+
                                                                                             (ar-call-resolve-notation
                                                                                              g2694
                                                                                              g2696
                                                                                              g2697)
                                                                                             g2693)))))
                                                                                    | counts|)))))
                                                                          | counts|)
                                                                        1))))
                                                                | counts|)
                                                              0))))
                                                      | counts|)
                                                    (_car seq)))))
                                            | counts|)
                                          c))))
                                  | counts|))
                               (_counts (_cdr seq) c))))
                        | counts|)))))))
          (namespace-set-variable-value! '_counts zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'counts)))
     (_setinfixop 'counts ((ar-coerce _infixop 'fn) 'counts))
     'nil)))


((lambda ()
   (_sref _sig '(seq . nil) 'commonest)
   (_sref _help* 'nil 'commonest)
   (_sref _source-file* _current-load-file* 'commonest)
   (_sref
    _source*
    '(def
      commonest
      (seq . nil)
      (with
       (winner nil n 0 . nil)
       (each
        (k v . nil)
        (counts seq . nil)
        (when (> v n . nil) (= winner k n v . nil) . nil)
        .
        nil)
       (list winner n . nil)
       .
       nil)
      .
      nil)
    'commonest)
   ((lambda ()
      (if (not (ar-false? (_bound 'commonest)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'commonest (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| commonest|
                      (lambda (seq)
                        ((let ((| commonest|
                                (lambda (winner n)
                                  (_walk
                                   (_counts seq)
                                   (lambda g2698
                                     (let* ((k (ar-xcar (car g2698)))
                                            (v
                                             (ar-xcar (ar-xcdr (car g2698)))))
                                       (if (not (ar-false? (_> v n)))
                                         ((let ((| commonest|
                                                 (lambda ()
                                                   ((let ((| commonest|
                                                           (lambda ()
                                                             (begin
                                                               (let ((zz k))
                                                                 (set! winner
                                                                   zz)
                                                                 zz))
                                                             (begin
                                                               (let ((zz v))
                                                                 (set! n zz)
                                                                 zz)))))
                                                      | commonest|)))))
                                            | commonest|))
                                         'nil))))
                                  (_list winner n))))
                           | commonest|)
                         'nil
                         0))))
                 | commonest|)))
          (namespace-set-variable-value! '_commonest zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'commonest)))
     (_setinfixop 'commonest ((ar-coerce _infixop 'fn) 'commonest))
     'nil)))


((lambda ()
   (_sref _sig '(f xs . nil) 'reduce)
   (_sref _help* 'nil 'reduce)
   (_sref _source-file* _current-load-file* 'reduce)
   (_sref
    _source*
    '(def
      reduce
      (f xs . nil)
      (if (cddr xs . nil)
        (reduce
         f
         (cons (f (car xs . nil) (cadr xs . nil) . nil) (cddr xs . nil) . nil)
         .
         nil)
        (apply f xs . nil)
        .
        nil)
      .
      nil)
    'reduce)
   ((lambda ()
      (if (not (ar-false? (_bound 'reduce)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'reduce (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| reduce|
                      (lambda (f xs)
                        (if (not (ar-false? (_cddr xs)))
                          (_reduce
                           f
                           (_cons
                            (ar-call-resolve-notation f (_car xs) (_cadr xs))
                            (_cddr xs)))
                          (_apply f xs)))))
                 | reduce|)))
          (namespace-set-variable-value! '_reduce zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'reduce)))
     (_setinfixop 'reduce ((ar-coerce _infixop 'fn) 'reduce))
     'nil)))


((lambda ()
   (_sref _sig '(f xs . nil) 'rreduce)
   (_sref _help* 'nil 'rreduce)
   (_sref _source-file* _current-load-file* 'rreduce)
   (_sref
    _source*
    '(def
      rreduce
      (f xs . nil)
      (if (cddr xs . nil)
        (f (car xs . nil) (rreduce f (cdr xs . nil) . nil) . nil)
        (apply f xs . nil)
        .
        nil)
      .
      nil)
    'rreduce)
   ((lambda ()
      (if (not (ar-false? (_bound 'rreduce)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rreduce (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| rreduce|
                      (lambda (f xs)
                        (if (not (ar-false? (_cddr xs)))
                          (ar-call-resolve-notation
                           f
                           (_car xs)
                           (_rreduce f (_cdr xs)))
                          (_apply f xs)))))
                 | rreduce|)))
          (namespace-set-variable-value! '_rreduce zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rreduce)))
     (_setinfixop 'rreduce ((ar-coerce _infixop 'fn) 'rreduce))
     'nil)))


((lambda (argsym)
   ((lambda ()
      (_sref _sig '(str . nil) 'parse-format)
      (_sref _help* 'nil 'parse-format)
      (_sref _source-file* _current-load-file* 'parse-format)
      (_sref
       _source*
       '(def
         parse-format
         (str . nil)
         (accum
          a
          (with
           (chars nil i -1 . nil)
           (w/instring
            s
            str
            (whilet
             c
             (readc s . nil)
             (case c
               #\#
               (do
                (a (coerce (rev chars . nil) (quote string . nil) . nil) . nil)
                (wipe chars . nil)
                (a (read s . nil) . nil)
                .
                nil)
               #\~
               (do
                (a (coerce (rev chars . nil) (quote string . nil) . nil) . nil)
                (wipe chars . nil)
                (readc s . nil)
                (a (list argsym (++ i . nil) . nil) . nil)
                .
                nil)
               (push c chars . nil)
               .
               nil)
             .
             nil)
            .
            nil)
           (when chars
             (a (coerce (rev chars . nil) (quote string . nil) . nil) . nil)
             .
             nil)
           .
           nil)
          .
          nil)
         .
         nil)
       'parse-format)
      ((lambda ()
         (if (not (ar-false? (_bound 'parse-format)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'parse-format (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (let ((| parse-format|
                         (lambda (str)
                           ((let ((| parse-format|
                                   (lambda (g2699)
                                     ((let ((| parse-format|
                                             (lambda (a)
                                               ((let ((| parse-format|
                                                       (lambda ()
                                                         ((let ((| parse-format|
                                                                 (lambda (chars
                                                                          i)
                                                                   ((let ((| parse-format|
                                                                           (lambda (s)
                                                                             (_protect
                                                                              (let ((| parse-format|
                                                                                     (lambda ()
                                                                                       ((let ((| parse-format|
                                                                                               (lambda ()
                                                                                                 (ar-call-resolve-notation
                                                                                                  ((let ((| parse-format|
                                                                                                          (lambda (g2700)
                                                                                                            (begin
                                                                                                              (let ((zz
                                                                                                                     (let ((| g2700|
                                                                                                                            (lambda (g2701)
                                                                                                                              ((let ((| g2700|
                                                                                                                                      (lambda (c)
                                                                                                                                        (if (not
                                                                                                                                             (ar-false?
                                                                                                                                              c))
                                                                                                                                          ((let ((| g2700|
                                                                                                                                                  (lambda ()
                                                                                                                                                    ((let ((| g2700|
                                                                                                                                                            (lambda (g2702)
                                                                                                                                                              (if (not
                                                                                                                                                                   (ar-false?
                                                                                                                                                                    (_is
                                                                                                                                                                     g2702
                                                                                                                                                                     '#\#)))
                                                                                                                                                                ((let ((| g2700|
                                                                                                                                                                        (lambda ()
                                                                                                                                                                          (ar-call-resolve-notation
                                                                                                                                                                           a
                                                                                                                                                                           (_coerce
                                                                                                                                                                            (_rev
                                                                                                                                                                             chars)
                                                                                                                                                                            'string))
                                                                                                                                                                          ((let ((| g2700|
                                                                                                                                                                                  (lambda ()
                                                                                                                                                                                    ((let ((| g2700|
                                                                                                                                                                                            (lambda ()
                                                                                                                                                                                              (begin
                                                                                                                                                                                                (let ((zz
                                                                                                                                                                                                       'nil))
                                                                                                                                                                                                  (set! chars
                                                                                                                                                                                                    zz)
                                                                                                                                                                                                  zz)))))
                                                                                                                                                                                       | g2700|)))))
                                                                                                                                                                             | g2700|))
                                                                                                                                                                          (ar-call-resolve-notation
                                                                                                                                                                           a
                                                                                                                                                                           (_read
                                                                                                                                                                            s)))))
                                                                                                                                                                   | g2700|))
                                                                                                                                                                (if (not
                                                                                                                                                                     (ar-false?
                                                                                                                                                                      (_is
                                                                                                                                                                       g2702
                                                                                                                                                                       '#\~)))
                                                                                                                                                                  ((let ((| g2700|
                                                                                                                                                                          (lambda ()
                                                                                                                                                                            (ar-call-resolve-notation
                                                                                                                                                                             a
                                                                                                                                                                             (_coerce
                                                                                                                                                                              (_rev
                                                                                                                                                                               chars)
                                                                                                                                                                              'string))
                                                                                                                                                                            ((let ((| g2700|
                                                                                                                                                                                    (lambda ()
                                                                                                                                                                                      ((let ((| g2700|
                                                                                                                                                                                              (lambda ()
                                                                                                                                                                                                (begin
                                                                                                                                                                                                  (let ((zz
                                                                                                                                                                                                         'nil))
                                                                                                                                                                                                    (set! chars
                                                                                                                                                                                                      zz)
                                                                                                                                                                                                    zz)))))
                                                                                                                                                                                         | g2700|)))))
                                                                                                                                                                               | g2700|))
                                                                                                                                                                            (_readc
                                                                                                                                                                             s)
                                                                                                                                                                            (ar-call-resolve-notation
                                                                                                                                                                             a
                                                                                                                                                                             (_list
                                                                                                                                                                              argsym
                                                                                                                                                                              ((let ((| g2700|
                                                                                                                                                                                      (lambda ()
                                                                                                                                                                                        (begin
                                                                                                                                                                                          (let ((zz
                                                                                                                                                                                                 (_+
                                                                                                                                                                                                  i
                                                                                                                                                                                                  1)))
                                                                                                                                                                                            (set! i
                                                                                                                                                                                              zz)
                                                                                                                                                                                            zz)))))
                                                                                                                                                                                 | g2700|)))))))
                                                                                                                                                                     | g2700|))
                                                                                                                                                                  ((let ((| g2700|
                                                                                                                                                                          (lambda (g2703)
                                                                                                                                                                            (_atomic-invoke
                                                                                                                                                                             (let ((| g2700|
                                                                                                                                                                                    (lambda ()
                                                                                                                                                                                      ((let ((| g2700|
                                                                                                                                                                                              (lambda (g2704)
                                                                                                                                                                                                ((let ((| g2700|
                                                                                                                                                                                                        (lambda ()
                                                                                                                                                                                                          ((let ((| g2700|
                                                                                                                                                                                                                  (lambda (g2705)
                                                                                                                                                                                                                    (begin
                                                                                                                                                                                                                      (let ((zz
                                                                                                                                                                                                                             g2705))
                                                                                                                                                                                                                        (set! chars
                                                                                                                                                                                                                          zz)
                                                                                                                                                                                                                        zz)))))
                                                                                                                                                                                                             | g2700|)
                                                                                                                                                                                                           (_cons
                                                                                                                                                                                                            g2703
                                                                                                                                                                                                            g2704)))))
                                                                                                                                                                                                   | g2700|)))))
                                                                                                                                                                                         | g2700|)
                                                                                                                                                                                       chars))))
                                                                                                                                                                               | g2700|)))))
                                                                                                                                                                     | g2700|)
                                                                                                                                                                   c))))))
                                                                                                                                                       | g2700|)
                                                                                                                                                     c)
                                                                                                                                                    (ar-call-resolve-notation
                                                                                                                                                     g2700
                                                                                                                                                     (_readc
                                                                                                                                                      s)))))
                                                                                                                                             | g2700|))
                                                                                                                                          'nil))))
                                                                                                                                 | g2700|)
                                                                                                                               g2701))))
                                                                                                                       | g2700|)))
                                                                                                                (set! g2700
                                                                                                                  zz)
                                                                                                                zz)))))
                                                                                                     | parse-format|)
                                                                                                   'nil)
                                                                                                  (_readc
                                                                                                   s)))))
                                                                                          | parse-format|)))))
                                                                                | parse-format|)
                                                                              (let ((| parse-format|
                                                                                     (lambda ()
                                                                                       (_close
                                                                                        s))))
                                                                                | parse-format|)))))
                                                                      | parse-format|)
                                                                    (_instring
                                                                     str))
                                                                   (if (not
                                                                        (ar-false?
                                                                         chars))
                                                                     ((let ((| parse-format|
                                                                             (lambda ()
                                                                               (ar-call-resolve-notation
                                                                                a
                                                                                (_coerce
                                                                                 (_rev
                                                                                  chars)
                                                                                 'string)))))
                                                                        | parse-format|))
                                                                     'nil))))
                                                            | parse-format|)
                                                          'nil
                                                          -1)
                                                         (_rev g2699))))
                                                  | parse-format|)))))
                                        | parse-format|)
                                      (let ((| a|
                                             (lambda ()
                                               ((let ((| a|
                                                       (lambda (g2706)
                                                         (_atomic-invoke
                                                          (let ((| a|
                                                                 (lambda ()
                                                                   ((let ((| a|
                                                                           (lambda (g2707)
                                                                             ((let ((| a|
                                                                                     (lambda ()
                                                                                       ((let ((| a|
                                                                                               (lambda (g2708)
                                                                                                 (begin
                                                                                                   (let ((zz
                                                                                                          g2708))
                                                                                                     (set! g2699
                                                                                                       zz)
                                                                                                     zz)))))
                                                                                          | a|)
                                                                                        (_cons
                                                                                         g2706
                                                                                         g2707)))))
                                                                                | a|)))))
                                                                      | a|)
                                                                    g2699))))
                                                            | a|)))))
                                                  | a|)
                                                __))))
                                        | a|)))))
                              | parse-format|)
                            'nil))))
                    | parse-format|)))
             (namespace-set-variable-value! '_parse-format zz)
             zz))))
      (if (not (ar-false? ((ar-coerce _infixop 'fn) 'parse-format)))
        (_setinfixop 'parse-format ((ar-coerce _infixop 'fn) 'parse-format))
        'nil)))
   ((lambda ()
      (_sref _sig '(str . args) 'prf)
      (_sref _help* 'nil 'prf)
      (_sref _source-file* _current-load-file* 'prf)
      (_sref
       _source*
       '(mac
         prf
         (str . args)
         (quasiquote
          (let (unquote argsym . nil)
            (list (unquote-splicing args . nil) . nil)
            (pr (unquote-splicing (parse-format str . nil) . nil) . nil)
            .
            nil)
          .
          nil)
         .
         nil)
       'prf)
      ((lambda ()
         (if (not (ar-false? (_bound 'prf)))
           ((lambda ()
              (_disp "*** redefining " (_stderr))
              (_disp 'prf (_stderr))
              (_disp #\newline (_stderr))))
           'nil)
         (begin
           (let ((zz
                  (_annotate
                   'mac
                   (let ((| prf|
                          (lambda (str . args)
                            `(let ,argsym
                               (list ,@(ar-nil-terminate args))
                               (pr
                                ,@(ar-nil-terminate (_parse-format str)))))))
                     | prf|))))
             (namespace-set-variable-value! '_prf zz)
             zz)))))))
 (_uniq))


((lambda ()
   ((lambda ()
      (begin
        (let ((zz 'nil))
          (namespace-set-variable-value! '_load-file-stack* zz)
          zz))))))


((lambda ()
   (_sref _sig '(file . nil) 'load)
   (_sref _help* 'nil 'load)
   (_sref _source-file* _current-load-file* 'load)
   (_sref
    _source*
    '(def
      load
      (file . nil)
      (push current-load-file* load-file-stack* . nil)
      (= current-load-file* file . nil)
      (after
       (w/infile
        f
        file
        (w/uniq
         eof
         (whiler e (read f eof . nil) eof (eval e . nil) . nil)
         .
         nil)
        .
        nil)
       (= current-load-file* (pop load-file-stack* . nil) . nil)
       .
       nil)
      .
      nil)
    'load)
   ((lambda ()
      (if (not (ar-false? (_bound 'load)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'load (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| load|
                      (lambda (file)
                        ((let ((| load|
                                (lambda (g2709)
                                  (_atomic-invoke
                                   (let ((| load|
                                          (lambda ()
                                            ((let ((| load|
                                                    (lambda (g2710)
                                                      ((let ((| load|
                                                              (lambda ()
                                                                ((let ((| load|
                                                                        (lambda (g2711)
                                                                          (begin
                                                                            (let ((zz
                                                                                   g2711))
                                                                              (namespace-set-variable-value!
                                                                               '_load-file-stack*
                                                                               zz)
                                                                              zz)))))
                                                                   | load|)
                                                                 (_cons
                                                                  g2709
                                                                  g2710)))))
                                                         | load|)))))
                                               | load|)
                                             _load-file-stack*))))
                                     | load|)))))
                           | load|)
                         _current-load-file*)
                        ((let ((| load|
                                (lambda ()
                                  (begin
                                    (let ((zz file))
                                      (namespace-set-variable-value!
                                       '_current-load-file*
                                       zz)
                                      zz)))))
                           | load|))
                        (_protect
                         (let ((| load|
                                (lambda ()
                                  ((let ((| load|
                                          (lambda (f)
                                            (_protect
                                             (let ((| load|
                                                    (lambda ()
                                                      ((let ((| load|
                                                              (lambda ()
                                                                ((let ((| load|
                                                                        (lambda (eof)
                                                                          ((let ((| load|
                                                                                  (lambda (e)
                                                                                    ((let ((| load|
                                                                                            (lambda (g2712)
                                                                                              ((let ((| load|
                                                                                                      (lambda ()
                                                                                                        (ar-call-resolve-notation
                                                                                                         ((let ((| load|
                                                                                                                 (lambda (g2713)
                                                                                                                   (begin
                                                                                                                     (let ((zz
                                                                                                                            (let ((| g2713|
                                                                                                                                   (lambda (g2714)
                                                                                                                                     (if (not
                                                                                                                                          (ar-false?
                                                                                                                                           g2714))
                                                                                                                                       ((let ((| g2713|
                                                                                                                                               (lambda ()
                                                                                                                                                 (_eval
                                                                                                                                                  e)
                                                                                                                                                 (ar-call-resolve-notation
                                                                                                                                                  g2713
                                                                                                                                                  (_no
                                                                                                                                                   (ar-call-resolve-notation
                                                                                                                                                    g2712
                                                                                                                                                    ((let ((| g2713|
                                                                                                                                                            (lambda ()
                                                                                                                                                              (begin
                                                                                                                                                                (let ((zz
                                                                                                                                                                       (_read
                                                                                                                                                                        f
                                                                                                                                                                        eof)))
                                                                                                                                                                  (set! e
                                                                                                                                                                    zz)
                                                                                                                                                                  zz)))))
                                                                                                                                                       | g2713|))))))))
                                                                                                                                          | g2713|))
                                                                                                                                       'nil))))
                                                                                                                              | g2713|)))
                                                                                                                       (set! g2713
                                                                                                                         zz)
                                                                                                                       zz)))))
                                                                                                            | load|)
                                                                                                          'nil)
                                                                                                         (_no
                                                                                                          (ar-call-resolve-notation
                                                                                                           g2712
                                                                                                           ((let ((| load|
                                                                                                                   (lambda ()
                                                                                                                     (begin
                                                                                                                       (let ((zz
                                                                                                                              (_read
                                                                                                                               f
                                                                                                                               eof)))
                                                                                                                         (set! e
                                                                                                                           zz)
                                                                                                                         zz)))))
                                                                                                              | load|))))))))
                                                                                                 | load|)))))
                                                                                       | load|)
                                                                                     (_testify
                                                                                      eof)))))
                                                                             | load|)
                                                                           'nil))))
                                                                   | load|)
                                                                 (_uniq)))))
                                                         | load|)))))
                                               | load|)
                                             (let ((| load|
                                                    (lambda () (_close f))))
                                               | load|)))))
                                     | load|)
                                   (_infile file)))))
                           | load|)
                         (let ((| load|
                                (lambda ()
                                  ((let ((| load|
                                          (lambda ()
                                            (begin
                                              (let ((zz
                                                     (_atomic-invoke
                                                      (let ((| current-load-file*|
                                                             (lambda ()
                                                               ((let ((| current-load-file*|
                                                                       (lambda (g2716)
                                                                         ((let ((| current-load-file*|
                                                                                 (lambda (g2715)
                                                                                   ((let ((| current-load-file*|
                                                                                           (lambda ()
                                                                                             ((let ((| current-load-file*|
                                                                                                     (lambda (g2718)
                                                                                                       ((let ((| current-load-file*|
                                                                                                               (lambda (g2717)
                                                                                                                 (begin
                                                                                                                   (let ((zz
                                                                                                                          g2717))
                                                                                                                     (namespace-set-variable-value!
                                                                                                                      '_load-file-stack*
                                                                                                                      zz)
                                                                                                                     zz)))))
                                                                                                          | current-load-file*|)
                                                                                                        (_cdr
                                                                                                         g2715))
                                                                                                       g2718)))
                                                                                                | current-load-file*|)
                                                                                              (_car
                                                                                               g2715)))))
                                                                                      | current-load-file*|)))))
                                                                            | current-load-file*|)
                                                                          g2716))))
                                                                  | current-load-file*|)
                                                                _load-file-stack*))))
                                                        | current-load-file*|))))
                                                (namespace-set-variable-value!
                                                 '_current-load-file*
                                                 zz)
                                                zz)))))
                                     | load|)))))
                           | load|)))))
                 | load|)))
          (namespace-set-variable-value! '_load zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'load)))
     (_setinfixop 'load ((ar-coerce _infixop 'fn) 'load))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'positive)
   (_sref _help* 'nil 'positive)
   (_sref _source-file* _current-load-file* 'positive)
   (_sref
    _source*
    '(def positive (x . nil) (and (number x . nil) (> x 0 . nil) . nil) . nil)
    'positive)
   ((lambda ()
      (if (not (ar-false? (_bound 'positive)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'positive (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| positive|
                      (lambda (x)
                        (if (not (ar-false? (_number x))) (_> x 0) 'nil))))
                 | positive|)))
          (namespace-set-variable-value! '_positive zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'positive)))
     (_setinfixop 'positive ((ar-coerce _infixop 'fn) 'positive))
     'nil)))


((lambda ()
   (_sref _sig '(var . body) 'w/table)
   (_sref _help* 'nil 'w/table)
   (_sref _source-file* _current-load-file* 'w/table)
   (_sref
    _source*
    '(mac
      w/table
      (var . body)
      (quasiquote
       (let (unquote var . nil)
         (table . nil)
         (unquote-splicing body . nil)
         (unquote var . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'w/table)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/table)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/table (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/table|
                       (lambda (var . body)
                         `(let ,var (table) ,@(ar-nil-terminate body) ,var))))
                  | w/table|))))
          (namespace-set-variable-value! '_w/table zz)
          zz))))))


((lambda ()
   (_sref _sig '(x . nil) 'mutable)
   (_sref _help* 'nil 'mutable)
   (_sref _source-file* _current-load-file* 'mutable)
   (_sref
    _source*
    '(def
      mutable
      (x . nil)
      (case (type x . nil)
        cons
        (cons
         (mutable (car x . nil) . nil)
         (mutable (cdr x . nil) . nil)
         .
         nil)
        table
        (w/table
         h
         (each (k v . nil) x (= (h k . nil) (mutable v . nil) . nil) . nil)
         .
         nil)
        string
        (copy x . nil)
        x
        .
        nil)
      .
      nil)
    'mutable)
   ((lambda ()
      (if (not (ar-false? (_bound 'mutable)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'mutable (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| mutable|
                      (lambda (x)
                        ((let ((| mutable|
                                (lambda (g2719)
                                  (if (not (ar-false? (_is g2719 'cons)))
                                    (_cons
                                     (_mutable (_car x))
                                     (_mutable (_cdr x)))
                                    (if (not (ar-false? (_is g2719 'table)))
                                      ((let ((| mutable|
                                              (lambda (h)
                                                (_walk
                                                 x
                                                 (lambda g2720
                                                   (let* ((k
                                                           (ar-xcar
                                                            (car g2720)))
                                                          (v
                                                           (ar-xcar
                                                            (ar-xcdr
                                                             (car g2720)))))
                                                     ((let ((| mutable|
                                                             (lambda ()
                                                               (_atomic-invoke
                                                                (let ((| mutable|
                                                                       (lambda ()
                                                                         ((let ((| mutable|
                                                                                 (lambda (g2721
                                                                                          g2723
                                                                                          g2724)
                                                                                   ((let ((| mutable|
                                                                                           (lambda (g2722)
                                                                                             (_sref
                                                                                              g2721
                                                                                              g2722
                                                                                              g2723))))
                                                                                      | mutable|)
                                                                                    g2724))))
                                                                            | mutable|)
                                                                          h
                                                                          k
                                                                          (_mutable
                                                                           v)))))
                                                                  | mutable|)))))
                                                        | mutable|)))))
                                                h)))
                                         | mutable|)
                                       (_table))
                                      (if (not (ar-false? (_is g2719 'string)))
                                        (_copy x)
                                        x))))))
                           | mutable|)
                         (_type x)))))
                 | mutable|)))
          (namespace-set-variable-value! '_mutable zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'mutable)))
     (_setinfixop 'mutable ((ar-coerce _infixop 'fn) 'mutable))
     'nil)))


((lambda ()
   (_sref _sig 'args 'ero)
   (_sref _help* 'nil 'ero)
   (_sref _source-file* _current-load-file* 'ero)
   (_sref
    _source*
    '(def
      ero
      args
      (w/stdout
       (stderr . nil)
       (each a args (write a . nil) (writec #\space . nil) . nil)
       (writec #\newline . nil)
       .
       nil)
      (car args . nil)
      .
      nil)
    'ero)
   ((lambda ()
      (if (not (ar-false? (_bound 'ero)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ero (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| ero|
                      (lambda args
                        (_call-w/stdout
                         (_stderr)
                         (let ((| ero|
                                (lambda ()
                                  (_walk
                                   args
                                   (let ((| ero|
                                          (lambda (a)
                                            (_write a)
                                            (_writec #\space))))
                                     | ero|))
                                  (_writec #\newline))))
                           | ero|))
                        (_car args))))
                 | ero|)))
          (namespace-set-variable-value! '_ero zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ero)))
     (_setinfixop 'ero ((ar-coerce _infixop 'fn) 'ero))
     'nil)))


((lambda ()
   (_sref _sig 'nil 'queue)
   (_sref _help* 'nil 'queue)
   (_sref _source-file* _current-load-file* 'queue)
   (_sref _source* '(def queue nil (list nil nil 0 . nil) . nil) 'queue)
   ((lambda ()
      (if (not (ar-false? (_bound 'queue)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'queue (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| queue| (lambda () (_list 'nil 'nil 0)))) | queue|)))
          (namespace-set-variable-value! '_queue zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'queue)))
     (_setinfixop 'queue ((ar-coerce _infixop 'fn) 'queue))
     'nil)))


((lambda ()
   (_sref _sig '(obj q . nil) 'enq)
   (_sref _help* 'nil 'enq)
   (_sref _source-file* _current-load-file* 'enq)
   (_sref
    _source*
    '(def
      enq
      (obj q . nil)
      (atomic
       (++ (q 2 . nil) . nil)
       (if (no (car q . nil) . nil)
         (= (cadr q . nil) (= (car q . nil) (list obj . nil) . nil) . nil)
         (=
          (cdr (cadr q . nil) . nil)
          (list obj . nil)
          (cadr q . nil)
          (cdr (cadr q . nil) . nil)
          .
          nil)
         .
         nil)
       (car q . nil)
       .
       nil)
      .
      nil)
    'enq)
   ((lambda ()
      (if (not (ar-false? (_bound 'enq)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'enq (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| enq|
                      (lambda (obj q)
                        (_atomic-invoke
                         (let ((| enq|
                                (lambda ()
                                  (_atomic-invoke
                                   (let ((| enq|
                                          (lambda ()
                                            ((let ((| enq|
                                                    (lambda (g2726)
                                                      ((let ((| enq|
                                                              (lambda (g2728)
                                                                ((let ((| enq|
                                                                        (lambda (g2725)
                                                                          ((let ((| enq|
                                                                                  (lambda ()
                                                                                    ((let ((| enq|
                                                                                            (lambda (g2727)
                                                                                              (_sref
                                                                                               g2726
                                                                                               g2727
                                                                                               g2728))))
                                                                                       | enq|)
                                                                                     (_+
                                                                                      (ar-call-resolve-notation
                                                                                       g2726
                                                                                       g2728)
                                                                                      g2725)))))
                                                                             | enq|)))))
                                                                   | enq|)
                                                                 1))))
                                                         | enq|)
                                                       2))))
                                               | enq|)
                                             q))))
                                     | enq|))
                                  (if (not (ar-false? (_no (_car q))))
                                    ((let ((| enq|
                                            (lambda ()
                                              (_atomic-invoke
                                               (let ((| enq|
                                                      (lambda ()
                                                        ((let ((| enq|
                                                                (lambda (g2729
                                                                         g2730)
                                                                  ((let ((| enq|
                                                                          (lambda (val)
                                                                            (_scar
                                                                             (_cdr
                                                                              g2729)
                                                                             val))))
                                                                     | enq|)
                                                                   g2730))))
                                                           | enq|)
                                                         q
                                                         ((let ((| g2730|
                                                                 (lambda ()
                                                                   (_atomic-invoke
                                                                    (let ((| g2730|
                                                                           (lambda ()
                                                                             ((let ((| g2730|
                                                                                     (lambda (g2731
                                                                                              g2732)
                                                                                       ((let ((| g2730|
                                                                                               (lambda (val)
                                                                                                 (_scar
                                                                                                  g2731
                                                                                                  val))))
                                                                                          | g2730|)
                                                                                        g2732))))
                                                                                | g2730|)
                                                                              q
                                                                              (_list
                                                                               obj)))))
                                                                      | g2730|)))))
                                                            | g2730|))))))
                                                 | enq|)))))
                                       | enq|))
                                    ((let ((| enq|
                                            (lambda ()
                                              (_atomic-invoke
                                               (let ((| enq|
                                                      (lambda ()
                                                        ((let ((| enq|
                                                                (lambda (g2733
                                                                         g2734)
                                                                  ((let ((| enq|
                                                                          (lambda (val)
                                                                            (_scdr
                                                                             g2733
                                                                             val))))
                                                                     | enq|)
                                                                   g2734))))
                                                           | enq|)
                                                         (_cadr q)
                                                         (_list obj)))))
                                                 | enq|))
                                              (_atomic-invoke
                                               (let ((| enq|
                                                      (lambda ()
                                                        ((let ((| enq|
                                                                (lambda (g2735
                                                                         g2736)
                                                                  ((let ((| enq|
                                                                          (lambda (val)
                                                                            (_scar
                                                                             (_cdr
                                                                              g2735)
                                                                             val))))
                                                                     | enq|)
                                                                   g2736))))
                                                           | enq|)
                                                         q
                                                         (_cdr (_cadr q))))))
                                                 | enq|)))))
                                       | enq|)))
                                  (_car q))))
                           | enq|)))))
                 | enq|)))
          (namespace-set-variable-value! '_enq zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'enq)))
     (_setinfixop 'enq ((ar-coerce _infixop 'fn) 'enq))
     'nil)))


((lambda ()
   (_sref _sig '(q . nil) 'deq)
   (_sref _help* 'nil 'deq)
   (_sref _source-file* _current-load-file* 'deq)
   (_sref
    _source*
    '(def
      deq
      (q . nil)
      (atomic
       (unless (is (q 2 . nil) 0 . nil) (-- (q 2 . nil) . nil) . nil)
       (pop (car q . nil) . nil)
       .
       nil)
      .
      nil)
    'deq)
   ((lambda ()
      (if (not (ar-false? (_bound 'deq)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'deq (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| deq|
                      (lambda (q)
                        (_atomic-invoke
                         (let ((| deq|
                                (lambda ()
                                  (if (not
                                       (ar-false?
                                        (_no
                                         (_is
                                          (ar-call-resolve-notation q 2)
                                          0))))
                                    ((let ((| deq|
                                            (lambda ()
                                              (_atomic-invoke
                                               (let ((| deq|
                                                      (lambda ()
                                                        ((let ((| deq|
                                                                (lambda (g2738)
                                                                  ((let ((| deq|
                                                                          (lambda (g2740)
                                                                            ((let ((| deq|
                                                                                    (lambda (g2737)
                                                                                      ((let ((| deq|
                                                                                              (lambda ()
                                                                                                ((let ((| deq|
                                                                                                        (lambda (g2739)
                                                                                                          (_sref
                                                                                                           g2738
                                                                                                           g2739
                                                                                                           g2740))))
                                                                                                   | deq|)
                                                                                                 (_-
                                                                                                  (ar-call-resolve-notation
                                                                                                   g2738
                                                                                                   g2740)
                                                                                                  g2737)))))
                                                                                         | deq|)))))
                                                                               | deq|)
                                                                             1))))
                                                                     | deq|)
                                                                   2))))
                                                           | deq|)
                                                         q))))
                                                 | deq|)))))
                                       | deq|))
                                    'nil)
                                  (_atomic-invoke
                                   (let ((| deq|
                                          (lambda ()
                                            ((let ((| deq|
                                                    (lambda (g2742)
                                                      ((let ((| deq|
                                                              (lambda (g2741)
                                                                ((let ((| deq|
                                                                        (lambda ()
                                                                          ((let ((| deq|
                                                                                  (lambda (g2743)
                                                                                    ((let ((| deq|
                                                                                            (lambda (val)
                                                                                              (_scar
                                                                                               g2742
                                                                                               val))))
                                                                                       | deq|)
                                                                                     (_cdr
                                                                                      g2741))
                                                                                    g2743)))
                                                                             | deq|)
                                                                           (_car
                                                                            g2741)))))
                                                                   | deq|)))))
                                                         | deq|)
                                                       (_car g2742)))))
                                               | deq|)
                                             q))))
                                     | deq|)))))
                           | deq|)))))
                 | deq|)))
          (namespace-set-variable-value! '_deq zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'deq)))
     (_setinfixop 'deq ((ar-coerce _infixop 'fn) 'deq))
     'nil)))


((lambda ()
   (_sref _sig '(q . nil) 'qlen)
   (_sref _help* 'nil 'qlen)
   (_sref _source-file* _current-load-file* 'qlen)
   (_sref _source* '(def qlen (q . nil) (q 2 . nil) . nil) 'qlen)
   ((lambda ()
      (if (not (ar-false? (_bound 'qlen)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'qlen (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| qlen| (lambda (q) (ar-call-resolve-notation q 2))))
                 | qlen|)))
          (namespace-set-variable-value! '_qlen zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'qlen)))
     (_setinfixop 'qlen ((ar-coerce _infixop 'fn) 'qlen))
     'nil)))


((lambda ()
   (_sref _sig '(q . nil) 'qlist)
   (_sref _help* 'nil 'qlist)
   (_sref _source-file* _current-load-file* 'qlist)
   (_sref _source* '(def qlist (q . nil) (car q . nil) . nil) 'qlist)
   ((lambda ()
      (if (not (ar-false? (_bound 'qlist)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'qlist (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| qlist| (lambda (q) (_car q)))) | qlist|)))
          (namespace-set-variable-value! '_qlist zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'qlist)))
     (_setinfixop 'qlist ((ar-coerce _infixop 'fn) 'qlist))
     'nil)))


((lambda ()
   (_sref _sig '(val q (o limit 1000 . nil) . nil) 'enq-limit)
   (_sref _help* 'nil 'enq-limit)
   (_sref _source-file* _current-load-file* 'enq-limit)
   (_sref
    _source*
    '(def
      enq-limit
      (val q (o limit 1000 . nil) . nil)
      (atomic
       (unless (< (qlen q . nil) limit . nil) (deq q . nil) . nil)
       (enq val q . nil)
       .
       nil)
      .
      nil)
    'enq-limit)
   ((lambda ()
      (if (not (ar-false? (_bound 'enq-limit)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'enq-limit (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2744
                 (let* ((val (car g2744))
                        (q (car (ar-xcdr g2744)))
                        (limit
                         (if (pair? (ar-xcdr (ar-xcdr g2744)))
                           (car (ar-xcdr (ar-xcdr g2744)))
                           1000)))
                   (_atomic-invoke
                    (let ((| enq-limit|
                           (lambda ()
                             (if (not (ar-false? (_no (_< (_qlen q) limit))))
                               ((let ((| enq-limit| (lambda () (_deq q))))
                                  | enq-limit|))
                               'nil)
                             (_enq val q))))
                      | enq-limit|))))))
          (namespace-set-variable-value! '_enq-limit zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'enq-limit)))
     (_setinfixop 'enq-limit ((ar-coerce _infixop 'fn) 'enq-limit))
     'nil)))


((lambda ()
   (_sref _sig '(ns . nil) 'median)
   (_sref _help* 'nil 'median)
   (_sref _source-file* _current-load-file* 'median)
   (_sref
    _source*
    '(def
      median
      (ns . nil)
      ((sort > ns . nil) (trunc (/ (len ns . nil) 2 . nil) . nil) . nil)
      .
      nil)
    'median)
   ((lambda ()
      (if (not (ar-false? (_bound 'median)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'median (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| median|
                      (lambda (ns)
                        (ar-call-resolve-notation
                         (_sort _> ns)
                         (_trunc (_/ (_len ns) 2))))))
                 | median|)))
          (namespace-set-variable-value! '_median zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'median)))
     (_setinfixop 'median ((ar-coerce _infixop 'fn) 'median))
     'nil)))


((lambda ()
   (_sref _sig '(n var val . body) 'noisy-each)
   (_sref _help* 'nil 'noisy-each)
   (_sref _source-file* _current-load-file* 'noisy-each)
   (_sref
    _source*
    '(mac
      noisy-each
      (n var val . body)
      (w/uniq
       (gn gc . nil)
       (quasiquote
        (with
         ((unquote gn . nil) (unquote n . nil) (unquote gc . nil) 0 . nil)
         (each
          (unquote var . nil)
          (unquote val . nil)
          (when (multiple
                 (++ (unquote gc . nil) . nil)
                 (unquote gn . nil)
                 .
                 nil)
            (pr "." . nil)
            (flushout . nil)
            .
            nil)
          (unquote-splicing body . nil)
          .
          nil)
         (prn . nil)
         (flushout . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'noisy-each)
   ((lambda ()
      (if (not (ar-false? (_bound 'noisy-each)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'noisy-each (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| noisy-each|
                       (lambda (n var val . body)
                         ((let ((| noisy-each|
                                 (lambda (gn gc)
                                   `(with
                                     (,gn ,n ,gc 0)
                                     (each
                                      ,var
                                      ,val
                                      (when (multiple (++ ,gc) ,gn)
                                        (pr ".")
                                        (flushout))
                                      ,@(ar-nil-terminate body))
                                     (prn)
                                     (flushout)))))
                            | noisy-each|)
                          (_uniq)
                          (_uniq)))))
                  | noisy-each|))))
          (namespace-set-variable-value! '_noisy-each zz)
          zz))))))


((lambda ()
   (_sref _sig '(name . body) 'point)
   (_sref _help* 'nil 'point)
   (_sref _source-file* _current-load-file* 'point)
   (_sref
    _source*
    '(mac
      point
      (name . body)
      (w/uniq
       (g p . nil)
       (quasiquote
        (ccc
         (fn
          ((unquote g . nil) . nil)
          (let (unquote name . nil)
            (fn
             ((o (unquote p . nil) . nil) . nil)
             ((unquote g . nil) (unquote p . nil) . nil)
             .
             nil)
            (unquote-splicing body . nil)
            .
            nil)
          .
          nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'point)
   ((lambda ()
      (if (not (ar-false? (_bound 'point)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'point (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| point|
                       (lambda (name . body)
                         ((let ((| point|
                                 (lambda (g p)
                                   `(ccc
                                     (fn
                                      (,g)
                                      (let ,name
                                        (fn ((o ,p)) (,g ,p))
                                        ,@(ar-nil-terminate body)))))))
                            | point|)
                          (_uniq)
                          (_uniq)))))
                  | point|))))
          (namespace-set-variable-value! '_point zz)
          zz))))))


((lambda ()
   (_sref _sig 'body 'catch)
   (_sref _help* 'nil 'catch)
   (_sref _source-file* _current-load-file* 'catch)
   (_sref
    _source*
    '(mac
      catch
      body
      (quasiquote (point throw (unquote-splicing body . nil) . nil) . nil)
      .
      nil)
    'catch)
   ((lambda ()
      (if (not (ar-false? (_bound 'catch)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'catch (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| catch|
                       (lambda body `(point throw ,@(ar-nil-terminate body)))))
                  | catch|))))
          (namespace-set-variable-value! '_catch zz)
          zz))))))


((lambda ()
   (_sref _sig '(x . nil) 'downcase)
   (_sref _help* 'nil 'downcase)
   (_sref _source-file* _current-load-file* 'downcase)
   (_sref
    _source*
    '(def
      downcase
      (x . nil)
      (let downc (fn
                  (c . nil)
                  (let n (coerce c (quote int . nil) . nil)
                    (if (or (< 64 n 91 . nil)
                            (< 191 n 215 . nil)
                            (< 215 n 223 . nil)
                            .
                            nil)
                      (coerce (+ n 32 . nil) (quote char . nil) . nil)
                      c
                      .
                      nil)
                    .
                    nil)
                  .
                  nil)
        (case (type x . nil)
          string
          (map downc x . nil)
          char
          (downc x . nil)
          sym
          (sym (map downc (coerce x (quote string . nil) . nil) . nil) . nil)
          (err "Can't downcase" x . nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'downcase)
   ((lambda ()
      (if (not (ar-false? (_bound 'downcase)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'downcase (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| downcase|
                      (lambda (x)
                        ((let ((| downcase|
                                (lambda (downc)
                                  ((let ((| downcase|
                                          (lambda (g2745)
                                            (if (not
                                                 (ar-false?
                                                  (_is g2745 'string)))
                                              (_map downc x)
                                              (if (not
                                                   (ar-false?
                                                    (_is g2745 'char)))
                                                (ar-call-resolve-notation
                                                 downc
                                                 x)
                                                (if (not
                                                     (ar-false?
                                                      (_is g2745 'sym)))
                                                  (_sym
                                                   (_map
                                                    downc
                                                    (_coerce x 'string)))
                                                  (_err
                                                   "Can't downcase"
                                                   x)))))))
                                     | downcase|)
                                   (_type x)))))
                           | downcase|)
                         (let ((| downc|
                                (lambda (c)
                                  ((let ((| downc|
                                          (lambda (n)
                                            (if (not
                                                 (ar-false?
                                                  ((let ((| downc|
                                                          (lambda (g2746)
                                                            (if (not
                                                                 (ar-false?
                                                                  g2746))
                                                              g2746
                                                              ((let ((| downc|
                                                                      (lambda (g2747)
                                                                        (if (not
                                                                             (ar-false?
                                                                              g2747))
                                                                          g2747
                                                                          ((let ((| downc|
                                                                                  (lambda (g2748)
                                                                                    (if (not
                                                                                         (ar-false?
                                                                                          g2748))
                                                                                      g2748
                                                                                      'nil))))
                                                                             | downc|)
                                                                           (_<
                                                                            215
                                                                            n
                                                                            223))))))
                                                                 | downc|)
                                                               (_<
                                                                191
                                                                n
                                                                215))))))
                                                     | downc|)
                                                   (_< 64 n 91))))
                                              (_coerce (_+ n 32) 'char)
                                              c))))
                                     | downc|)
                                   (_coerce c 'int)))))
                           | downc|)))))
                 | downcase|)))
          (namespace-set-variable-value! '_downcase zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'downcase)))
     (_setinfixop 'downcase ((ar-coerce _infixop 'fn) 'downcase))
     'nil)))


((lambda ()
   (_sref _sig '(x . nil) 'upcase)
   (_sref _help* 'nil 'upcase)
   (_sref _source-file* _current-load-file* 'upcase)
   (_sref
    _source*
    '(def
      upcase
      (x . nil)
      (let upc (fn
                (c . nil)
                (let n (coerce c (quote int . nil) . nil)
                  (if (or (< 96 n 123 . nil)
                          (< 223 n 247 . nil)
                          (< 247 n 255 . nil)
                          .
                          nil)
                    (coerce (- n 32 . nil) (quote char . nil) . nil)
                    c
                    .
                    nil)
                  .
                  nil)
                .
                nil)
        (case (type x . nil)
          string
          (map upc x . nil)
          char
          (upc x . nil)
          sym
          (sym (map upc (coerce x (quote string . nil) . nil) . nil) . nil)
          (err "Can't upcase" x . nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'upcase)
   ((lambda ()
      (if (not (ar-false? (_bound 'upcase)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'upcase (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| upcase|
                      (lambda (x)
                        ((let ((| upcase|
                                (lambda (upc)
                                  ((let ((| upcase|
                                          (lambda (g2749)
                                            (if (not
                                                 (ar-false?
                                                  (_is g2749 'string)))
                                              (_map upc x)
                                              (if (not
                                                   (ar-false?
                                                    (_is g2749 'char)))
                                                (ar-call-resolve-notation
                                                 upc
                                                 x)
                                                (if (not
                                                     (ar-false?
                                                      (_is g2749 'sym)))
                                                  (_sym
                                                   (_map
                                                    upc
                                                    (_coerce x 'string)))
                                                  (_err "Can't upcase" x)))))))
                                     | upcase|)
                                   (_type x)))))
                           | upcase|)
                         (let ((| upc|
                                (lambda (c)
                                  ((let ((| upc|
                                          (lambda (n)
                                            (if (not
                                                 (ar-false?
                                                  ((let ((| upc|
                                                          (lambda (g2750)
                                                            (if (not
                                                                 (ar-false?
                                                                  g2750))
                                                              g2750
                                                              ((let ((| upc|
                                                                      (lambda (g2751)
                                                                        (if (not
                                                                             (ar-false?
                                                                              g2751))
                                                                          g2751
                                                                          ((let ((| upc|
                                                                                  (lambda (g2752)
                                                                                    (if (not
                                                                                         (ar-false?
                                                                                          g2752))
                                                                                      g2752
                                                                                      'nil))))
                                                                             | upc|)
                                                                           (_<
                                                                            247
                                                                            n
                                                                            255))))))
                                                                 | upc|)
                                                               (_<
                                                                223
                                                                n
                                                                247))))))
                                                     | upc|)
                                                   (_< 96 n 123))))
                                              (_coerce (_- n 32) 'char)
                                              c))))
                                     | upc|)
                                   (_coerce c 'int)))))
                           | upc|)))))
                 | upcase|)))
          (namespace-set-variable-value! '_upcase zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'upcase)))
     (_setinfixop 'upcase ((ar-coerce _infixop 'fn) 'upcase))
     'nil)))


((lambda ()
   (_sref _sig '(x (o n 1 . nil) . nil) 'inc)
   (_sref _help* 'nil 'inc)
   (_sref _source-file* _current-load-file* 'inc)
   (_sref
    _source*
    '(def
      inc
      (x (o n 1 . nil) . nil)
      (coerce
       (+ (coerce x (quote int . nil) . nil) n . nil)
       (type x . nil)
       .
       nil)
      .
      nil)
    'inc)
   ((lambda ()
      (if (not (ar-false? (_bound 'inc)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'inc (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (lambda g2753
                 (let* ((x (car g2753))
                        (n
                         (if (pair? (ar-xcdr g2753)) (car (ar-xcdr g2753)) 1)))
                   (_coerce (_+ (_coerce x 'int) n) (_type x))))))
          (namespace-set-variable-value! '_inc zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'inc)))
     (_setinfixop 'inc ((ar-coerce _infixop 'fn) 'inc))
     'nil)))


((lambda ()
   (_sref _sig '(start end . nil) 'range)
   (_sref _help* 'nil 'range)
   (_sref _source-file* _current-load-file* 'range)
   (_sref
    _source*
    '(def
      range
      (start end . nil)
      (if (> start end . nil)
        nil
        (cons start (range (inc start . nil) end . nil) . nil)
        .
        nil)
      .
      nil)
    'range)
   ((lambda ()
      (if (not (ar-false? (_bound 'range)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'range (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| range|
                      (lambda (start end)
                        (if (not (ar-false? (_> start end)))
                          'nil
                          (_cons start (_range (_inc start) end))))))
                 | range|)))
          (namespace-set-variable-value! '_range zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'range)))
     (_setinfixop 'range ((ar-coerce _infixop 'fn) 'range))
     'nil)))


((lambda ()
   (_sref _sig '(s1 s2 . nil) 'mismatch)
   (_sref _help* 'nil 'mismatch)
   (_sref _source-file* _current-load-file* 'mismatch)
   (_sref
    _source*
    '(def
      mismatch
      (s1 s2 . nil)
      (catch
       (on
        c
        s1
        (when (isnt c (s2 index . nil) . nil) (throw index . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'mismatch)
   ((lambda ()
      (if (not (ar-false? (_bound 'mismatch)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'mismatch (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| mismatch|
                      (lambda (s1 s2)
                        (_ccc
                         (let ((| mismatch|
                                (lambda (g2754)
                                  ((let ((| mismatch|
                                          (lambda (throw)
                                            ((let ((| mismatch|
                                                    (lambda (g2756)
                                                      ((let ((| mismatch|
                                                              (lambda (index
                                                                       g2757
                                                                       g2758)
                                                                ((let ((| mismatch|
                                                                        (lambda ()
                                                                          (begin
                                                                            (let ((zz
                                                                                   g2757))
                                                                              (set! index
                                                                                zz)
                                                                              zz))
                                                                          (ar-call-resolve-notation
                                                                           ((let ((| mismatch|
                                                                                   (lambda (g2759)
                                                                                     (begin
                                                                                       (let ((zz
                                                                                              (let ((| g2759|
                                                                                                     (lambda (g2760)
                                                                                                       (if (not
                                                                                                            (ar-false?
                                                                                                             g2760))
                                                                                                         ((let ((| g2759|
                                                                                                                 (lambda ()
                                                                                                                   ((let ((| g2759|
                                                                                                                           (lambda (c)
                                                                                                                             (if (not
                                                                                                                                  (ar-false?
                                                                                                                                   (_isnt
                                                                                                                                    c
                                                                                                                                    (ar-call-resolve-notation
                                                                                                                                     s2
                                                                                                                                     index))))
                                                                                                                               ((let ((| g2759|
                                                                                                                                       (lambda ()
                                                                                                                                         (ar-call-resolve-notation
                                                                                                                                          throw
                                                                                                                                          index))))
                                                                                                                                  | g2759|))
                                                                                                                               'nil))))
                                                                                                                      | g2759|)
                                                                                                                    (ar-call-resolve-notation
                                                                                                                     g2756
                                                                                                                     index))
                                                                                                                   (begin
                                                                                                                     (let ((zz
                                                                                                                            (_+
                                                                                                                             index
                                                                                                                             1)))
                                                                                                                       (set! index
                                                                                                                         zz)
                                                                                                                       zz))
                                                                                                                   (ar-call-resolve-notation
                                                                                                                    g2759
                                                                                                                    (_<
                                                                                                                     index
                                                                                                                     g2758)))))
                                                                                                            | g2759|))
                                                                                                         'nil))))
                                                                                                | g2759|)))
                                                                                         (set! g2759
                                                                                           zz)
                                                                                         zz)))))
                                                                              | mismatch|)
                                                                            'nil)
                                                                           (_<
                                                                            index
                                                                            g2758)))))
                                                                   | mismatch|)))))
                                                         | mismatch|)
                                                       'nil
                                                       0
                                                       (_+
                                                        (_- (_len g2756) 1)
                                                        1)))))
                                               | mismatch|)
                                             s1))))
                                     | mismatch|)
                                   (lambda g2761
                                     (let* ((g2755
                                             (if (pair? g2761)
                                               (car g2761)
                                               'nil)))
                                       (ar-call-resolve-notation
                                        g2754
                                        g2755)))))))
                           | mismatch|)))))
                 | mismatch|)))
          (namespace-set-variable-value! '_mismatch zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'mismatch)))
     (_setinfixop 'mismatch ((ar-coerce _infixop 'fn) 'mismatch))
     'nil)))


((lambda ()
   (_sref _sig '(ks . nil) 'memtable)
   (_sref _help* 'nil 'memtable)
   (_sref _source-file* _current-load-file* 'memtable)
   (_sref
    _source*
    '(def
      memtable
      (ks . nil)
      (let h (table . nil) (each k ks (set (h k . nil) . nil) . nil) h . nil)
      .
      nil)
    'memtable)
   ((lambda ()
      (if (not (ar-false? (_bound 'memtable)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'memtable (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| memtable|
                      (lambda (ks)
                        ((let ((| memtable|
                                (lambda (h)
                                  (_walk
                                   ks
                                   (let ((| memtable|
                                          (lambda (k)
                                            ((let ((| memtable|
                                                    (lambda ()
                                                      ((let ((| memtable|
                                                              (lambda ()
                                                                (_atomic-invoke
                                                                 (let ((| memtable|
                                                                        (lambda ()
                                                                          ((let ((| memtable|
                                                                                  (lambda (g2762
                                                                                           g2764
                                                                                           g2765)
                                                                                    ((let ((| memtable|
                                                                                            (lambda (g2763)
                                                                                              (_sref
                                                                                               g2762
                                                                                               g2763
                                                                                               g2764))))
                                                                                       | memtable|)
                                                                                     g2765))))
                                                                             | memtable|)
                                                                           h
                                                                           k
                                                                           _t))))
                                                                   | memtable|)))))
                                                         | memtable|)))))
                                               | memtable|)))))
                                     | memtable|))
                                  h)))
                           | memtable|)
                         (_table)))))
                 | memtable|)))
          (namespace-set-variable-value! '_memtable zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'memtable)))
     (_setinfixop 'memtable ((ar-coerce _infixop 'fn) 'memtable))
     'nil)))


((lambda ()
   (begin (let ((zz " | ")) (namespace-set-variable-value! '_bar* zz) zz))))


((lambda ()
   (_sref _sig 'body 'w/bars)
   (_sref _help* 'nil 'w/bars)
   (_sref _source-file* _current-load-file* 'w/bars)
   (_sref
    _source*
    '(mac
      w/bars
      body
      (w/uniq
       (out needbars . nil)
       (quasiquote
        (let (unquote needbars . nil)
          nil
          (do
           (unquote-splicing
            (map
             (fn
              (e . nil)
              (quasiquote
               (let (unquote out . nil)
                 (tostring (unquote e . nil) . nil)
                 (unless (is (unquote out . nil) "" . nil)
                   (if (unquote needbars . nil)
                     (pr bar* (unquote out . nil) . nil)
                     (do (set (unquote needbars . nil) . nil)
                         (pr (unquote out . nil) . nil)
                       .
                       nil)
                     .
                     nil)
                   .
                   nil)
                 .
                 nil)
               .
               nil)
              .
              nil)
             body
             .
             nil)
            .
            nil)
           .
           nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'w/bars)
   ((lambda ()
      (if (not (ar-false? (_bound 'w/bars)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'w/bars (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| w/bars|
                       (lambda body
                         ((let ((| w/bars|
                                 (lambda (out needbars)
                                   `(let ,needbars
                                      nil
                                      (do
                                       ,@(ar-nil-terminate
                                          (_map
                                           (let ((| w/bars|
                                                  (lambda (e)
                                                    `(let ,out
                                                       (tostring ,e)
                                                       (unless (is ,out "")
                                                         (if ,needbars
                                                           (pr bar* ,out)
                                                           (do (set ,needbars)
                                                               (pr ,out))))))))
                                             | w/bars|)
                                           body)))))))
                            | w/bars|)
                          (_uniq)
                          (_uniq)))))
                  | w/bars|))))
          (namespace-set-variable-value! '_w/bars zz)
          zz))))))


((lambda ()
   (_sref _sig '(x n . nil) 'len<)
   (_sref _help* 'nil 'len<)
   (_sref _source-file* _current-load-file* 'len<)
   (_sref
    _source*
    '(def len< (x n . nil) (< (len x . nil) n . nil) . nil)
    'len<)
   ((lambda ()
      (if (not (ar-false? (_bound 'len<)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'len< (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| len<| (lambda (x n) (_< (_len x) n)))) | len<|)))
          (namespace-set-variable-value! '_len< zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'len<)))
     (_setinfixop 'len< ((ar-coerce _infixop 'fn) 'len<))
     'nil)))


((lambda ()
   (_sref _sig '(x n . nil) 'len>)
   (_sref _help* 'nil 'len>)
   (_sref _source-file* _current-load-file* 'len>)
   (_sref
    _source*
    '(def len> (x n . nil) (> (len x . nil) n . nil) . nil)
    'len>)
   ((lambda ()
      (if (not (ar-false? (_bound 'len>)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'len> (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz (let ((| len>| (lambda (x n) (_> (_len x) n)))) | len>|)))
          (namespace-set-variable-value! '_len> zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'len>)))
     (_setinfixop 'len> ((ar-coerce _infixop 'fn) 'len>))
     'nil)))


((lambda ()
   (_sref _sig 'body 'thread)
   (_sref _help* 'nil 'thread)
   (_sref _source-file* _current-load-file* 'thread)
   (_sref
    _source*
    '(mac
      thread
      body
      (quasiquote
       (new-thread (fn nil (unquote-splicing body . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'thread)
   ((lambda ()
      (if (not (ar-false? (_bound 'thread)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'thread (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| thread|
                       (lambda body
                         `(new-thread (fn nil ,@(ar-nil-terminate body))))))
                  | thread|))))
          (namespace-set-variable-value! '_thread zz)
          zz))))))


((lambda ()
   (_sref _sig '(x . fs) 'trav)
   (_sref _help* 'nil 'trav)
   (_sref _source-file* _current-load-file* 'trav)
   (_sref
    _source*
    '(mac
      trav
      (x . fs)
      (w/uniq
       g
       (quasiquote
        ((afn
          ((unquote g . nil) . nil)
          (when (unquote g . nil)
            (unquote-splicing
             (map (make-br-fn (list _ g . nil) . nil) fs . nil)
             .
             nil)
            .
            nil)
          .
          nil)
         (unquote x . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'trav)
   ((lambda ()
      (if (not (ar-false? (_bound 'trav)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'trav (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| trav|
                       (lambda (x . fs)
                         ((let ((| trav|
                                 (lambda (g)
                                   `((afn
                                      (,g)
                                      (when ,g
                                        ,@(ar-nil-terminate
                                           (_map
                                            (let ((| trav|
                                                   (lambda () (_list __ g))))
                                              | trav|)
                                            fs))))
                                     ,x))))
                            | trav|)
                          (_uniq)))))
                  | trav|))))
          (namespace-set-variable-value! '_trav zz)
          zz))))))


((lambda ()
   (_sref _sig '(place expr . nil) 'or=)
   (_sref _help* 'nil 'or=)
   (_sref _source-file* _current-load-file* 'or=)
   (_sref
    _source*
    '(mac
      or=
      (place expr . nil)
      (let (binds val setter . nil)
        (setforms place . nil)
        (quasiquote
         (atwiths
          (unquote binds . nil)
          (or (unquote val . nil)
              ((unquote setter . nil) (unquote expr . nil) . nil)
              .
              nil)
          .
          nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'or=)
   ((lambda ()
      (if (not (ar-false? (_bound 'or=)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'or= (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| or=|
                       (lambda (place expr)
                         ((lambda g2766
                            (let* ((binds (ar-xcar (car g2766)))
                                   (val (ar-xcar (ar-xcdr (car g2766))))
                                   (setter
                                    (ar-xcar (ar-xcdr (ar-xcdr (car g2766))))))
                              `(atwiths ,binds (or ,val (,setter ,expr)))))
                          (_setforms place)))))
                  | or=|))))
          (namespace-set-variable-value! '_or= zz)
          zz))))))


((lambda ()
   (begin
     (let ((zz (_table))) (namespace-set-variable-value! '_hooks* zz) zz))))


((lambda ()
   (_sref _sig '(name . args) 'hook)
   (_sref _help* 'nil 'hook)
   (_sref _source-file* _current-load-file* 'hook)
   (_sref
    _source*
    '(def
      hook
      (name . args)
      (aif (hooks* name . nil) (apply it args . nil) . nil)
      .
      nil)
    'hook)
   ((lambda ()
      (if (not (ar-false? (_bound 'hook)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'hook (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| hook|
                      (lambda (name . args)
                        ((let ((| hook|
                                (lambda (it)
                                  (if (not (ar-false? it))
                                    (_apply it args)
                                    'nil))))
                           | hook|)
                         (ar-call-resolve-notation _hooks* name)))))
                 | hook|)))
          (namespace-set-variable-value! '_hook zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'hook)))
     (_setinfixop 'hook ((ar-coerce _infixop 'fn) 'hook))
     'nil)))


((lambda ()
   (_sref _sig '(name . rest) 'defhook)
   (_sref _help* 'nil 'defhook)
   (_sref _source-file* _current-load-file* 'defhook)
   (_sref
    _source*
    '(mac
      defhook
      (name . rest)
      (quasiquote
       (=
        (hooks* (quote (unquote name . nil) . nil) . nil)
        (fn (unquote-splicing rest . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'defhook)
   ((lambda ()
      (if (not (ar-false? (_bound 'defhook)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'defhook (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| defhook|
                       (lambda (name . rest)
                         `(= (hooks* ',name) (fn ,@(ar-nil-terminate rest))))))
                  | defhook|))))
          (namespace-set-variable-value! '_defhook zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr . nil) 'out)
   (_sref _help* 'nil 'out)
   (_sref _source-file* _current-load-file* 'out)
   (_sref
    _source*
    '(mac
      out
      (expr . nil)
      (quasiquote
       (pr (unquote (tostring (eval expr . nil) . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'out)
   ((lambda ()
      (if (not (ar-false? (_bound 'out)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'out (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| out|
                       (lambda (expr)
                         `(pr
                           ,((let ((| out|
                                    (lambda (g2767)
                                      (_call-w/stdout
                                       g2767
                                       (let ((| out| (lambda () (_eval expr))))
                                         | out|))
                                      (_inside g2767))))
                               | out|)
                             (_outstring))))))
                  | out|))))
          (namespace-set-variable-value! '_out zz)
          zz))))))


((lambda ()
   (_sref _sig '(index . nil) 'get)
   (_sref _help* 'nil 'get)
   (_sref _source-file* _current-load-file* 'get)
   (_sref
    _source*
    '(def get (index . nil) (make-br-fn (_ index . nil) . nil) . nil)
    'get)
   ((lambda ()
      (if (not (ar-false? (_bound 'get)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'get (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| get|
                      (lambda (index)
                        (let ((| get|
                               (lambda ()
                                 (ar-call-resolve-notation __ index))))
                          | get|))))
                 | get|)))
          (namespace-set-variable-value! '_get zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'get)))
     (_setinfixop 'get ((ar-coerce _infixop 'fn) 'get))
     'nil)))


((lambda ()
   (begin
     (let ((zz (_table))) (namespace-set-variable-value! '_savers* zz) zz))))


((lambda ()
   (_sref _sig '(var file init load save . nil) 'fromdisk)
   (_sref _help* 'nil 'fromdisk)
   (_sref _source-file* _current-load-file* 'fromdisk)
   (_sref
    _source*
    '(mac
      fromdisk
      (var file init load save . nil)
      (w/uniq
       (gf gv . nil)
       (quasiquote
        (unless (bound (quote (unquote var . nil) . nil) . nil)
          (do1
           (=
            (unquote var . nil)
            (iflet
             (unquote gf . nil)
             (file-exists (unquote file . nil) . nil)
             ((unquote load . nil) (unquote gf . nil) . nil)
             (unquote init . nil)
             .
             nil)
            .
            nil)
           (=
            (savers* (quote (unquote var . nil) . nil) . nil)
            (fn
             ((unquote gv . nil) . nil)
             ((unquote save . nil)
              (unquote gv . nil)
              (unquote file . nil)
              .
              nil)
             .
             nil)
            .
            nil)
           .
           nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'fromdisk)
   ((lambda ()
      (if (not (ar-false? (_bound 'fromdisk)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'fromdisk (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| fromdisk|
                       (lambda (var file init load save)
                         ((let ((| fromdisk|
                                 (lambda (gf gv)
                                   `(unless (bound ',var)
                                      (do1
                                       (=
                                        ,var
                                        (iflet
                                         ,gf
                                         (file-exists ,file)
                                         (,load ,gf)
                                         ,init))
                                       (=
                                        (savers* ',var)
                                        (fn (,gv) (,save ,gv ,file))))))))
                            | fromdisk|)
                          (_uniq)
                          (_uniq)))))
                  | fromdisk|))))
          (namespace-set-variable-value! '_fromdisk zz)
          zz))))))


((lambda ()
   (_sref _sig '(var file . nil) 'diskvar)
   (_sref _help* 'nil 'diskvar)
   (_sref _source-file* _current-load-file* 'diskvar)
   (_sref
    _source*
    '(mac
      diskvar
      (var file . nil)
      (quasiquote
       (fromdisk
        (unquote var . nil)
        (unquote file . nil)
        nil
        readfile1
        writefile
        .
        nil)
       .
       nil)
      .
      nil)
    'diskvar)
   ((lambda ()
      (if (not (ar-false? (_bound 'diskvar)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'diskvar (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| diskvar|
                       (lambda (var file)
                         `(fromdisk ,var ,file nil readfile1 writefile))))
                  | diskvar|))))
          (namespace-set-variable-value! '_diskvar zz)
          zz))))))


((lambda ()
   (_sref _sig '(var file . nil) 'disktable)
   (_sref _help* 'nil 'disktable)
   (_sref _source-file* _current-load-file* 'disktable)
   (_sref
    _source*
    '(mac
      disktable
      (var file . nil)
      (quasiquote
       (fromdisk
        (unquote var . nil)
        (unquote file . nil)
        (table . nil)
        load-table
        save-table
        .
        nil)
       .
       nil)
      .
      nil)
    'disktable)
   ((lambda ()
      (if (not (ar-false? (_bound 'disktable)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'disktable (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| disktable|
                       (lambda (var file)
                         `(fromdisk
                           ,var
                           ,file
                           (table)
                           load-table
                           save-table))))
                  | disktable|))))
          (namespace-set-variable-value! '_disktable zz)
          zz))))))


((lambda ()
   (_sref _sig '(var (o expr var . nil) . nil) 'todisk)
   (_sref _help* 'nil 'todisk)
   (_sref _source-file* _current-load-file* 'todisk)
   (_sref
    _source*
    '(mac
      todisk
      (var (o expr var . nil) . nil)
      (quasiquote
       ((savers* (quote (unquote var . nil) . nil) . nil)
        (unquote
         (if (is var expr . nil)
           var
           (quasiquote
            (= (unquote var . nil) (unquote expr . nil) . nil)
            .
            nil)
           .
           nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'todisk)
   ((lambda ()
      (if (not (ar-false? (_bound 'todisk)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'todisk (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (lambda g2768
                  (let* ((var (car g2768))
                         (expr
                          (if (pair? (ar-xcdr g2768))
                            (car (ar-xcdr g2768))
                            var)))
                    `((savers* ',var)
                      ,(if (not (ar-false? (_is var expr)))
                         var
                         `(= ,var ,expr))))))))
          (namespace-set-variable-value! '_todisk zz)
          zz))))))


((lambda ()
   (_sref _sig '(expr test . nil) 'evtil)
   (_sref _help* 'nil 'evtil)
   (_sref _source-file* _current-load-file* 'evtil)
   (_sref
    _source*
    '(mac
      evtil
      (expr test . nil)
      (w/uniq
       gv
       (quasiquote
        (let (unquote gv . nil)
          (unquote expr . nil)
          (while
           (no ((unquote test . nil) (unquote gv . nil) . nil) . nil)
           (= (unquote gv . nil) (unquote expr . nil) . nil)
           .
           nil)
          (unquote gv . nil)
          .
          nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'evtil)
   ((lambda ()
      (if (not (ar-false? (_bound 'evtil)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'evtil (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| evtil|
                       (lambda (expr test)
                         ((let ((| evtil|
                                 (lambda (gv)
                                   `(let ,gv
                                      ,expr
                                      (while (no (,test ,gv)) (= ,gv ,expr))
                                      ,gv))))
                            | evtil|)
                          (_uniq)))))
                  | evtil|))))
          (namespace-set-variable-value! '_evtil zz)
          zz))))))


((lambda ()
   (_sref _sig '(fun arg . nil) 'of)
   (_sref _help* 'nil 'of)
   (_sref _source-file* _current-load-file* 'of)
   (_sref
    _source*
    '(mac
      of
      (fun arg . nil)
      (quasiquote ((unquote fun . nil) (unquote arg . nil) . nil) . nil)
      .
      nil)
    'of)
   ((lambda ()
      (if (not (ar-false? (_bound 'of)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'of (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| of| (lambda (fun arg) `(,fun ,arg)))) | of|))))
          (namespace-set-variable-value! '_of zz)
          zz))))))


((lambda ()
   (_sref _sig '(arg fun . nil) 'go)
   (_sref _help* 'nil 'go)
   (_sref _source-file* _current-load-file* 'go)
   (_sref
    _source*
    '(mac
      go
      (arg fun . nil)
      (quasiquote ((unquote fun . nil) (unquote arg . nil) . nil) . nil)
      .
      nil)
    'go)
   ((lambda ()
      (if (not (ar-false? (_bound 'go)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'go (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| go| (lambda (arg fun) `(,fun ,arg)))) | go|))))
          (namespace-set-variable-value! '_go zz)
          zz))))))


((lambda ()
   (begin (let ((zz _go)) (namespace-set-variable-value! '_-> zz) zz))))


((lambda ()
   (begin (let ((zz _go)) (namespace-set-variable-value! '_under zz) zz))))


((lambda ()
   (_sref _sig '(h . nil) 'rand-key)
   (_sref _help* 'nil 'rand-key)
   (_sref _source-file* _current-load-file* 'rand-key)
   (_sref
    _source*
    '(def
      rand-key
      (h . nil)
      (if (empty h . nil)
        nil
        (let n (rand (len h . nil) . nil)
          (catch
           (each
            (k v . nil)
            h
            (when (is (-- n . nil) -1 . nil) (throw k . nil) . nil)
            .
            nil)
           .
           nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'rand-key)
   ((lambda ()
      (if (not (ar-false? (_bound 'rand-key)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'rand-key (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| rand-key|
                      (lambda (h)
                        (if (not (ar-false? (_empty h)))
                          'nil
                          ((let ((| rand-key|
                                  (lambda (n)
                                    (_ccc
                                     (let ((| rand-key|
                                            (lambda (g2769)
                                              ((let ((| rand-key|
                                                      (lambda (throw)
                                                        (_walk
                                                         h
                                                         (lambda g2771
                                                           (let* ((k
                                                                   (ar-xcar
                                                                    (car
                                                                     g2771)))
                                                                  (v
                                                                   (ar-xcar
                                                                    (ar-xcdr
                                                                     (car
                                                                      g2771)))))
                                                             (if (not
                                                                  (ar-false?
                                                                   (_is
                                                                    ((let ((| rand-key|
                                                                            (lambda ()
                                                                              (begin
                                                                                (let ((zz
                                                                                       (_-
                                                                                        n
                                                                                        1)))
                                                                                  (set! n
                                                                                    zz)
                                                                                  zz)))))
                                                                       | rand-key|))
                                                                    -1)))
                                                               ((let ((| rand-key|
                                                                       (lambda ()
                                                                         (ar-call-resolve-notation
                                                                          throw
                                                                          k))))
                                                                  | rand-key|))
                                                               'nil)))))))
                                                 | rand-key|)
                                               (lambda g2772
                                                 (let* ((g2770
                                                         (if (pair? g2772)
                                                           (car g2772)
                                                           'nil)))
                                                   (ar-call-resolve-notation
                                                    g2769
                                                    g2770)))))))
                                       | rand-key|)))))
                             | rand-key|)
                           (_rand (_len h)))))))
                 | rand-key|)))
          (namespace-set-variable-value! '_rand-key zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'rand-key)))
     (_setinfixop 'rand-key ((ar-coerce _infixop 'fn) 'rand-key))
     'nil)))


((lambda ()
   (_sref _sig '(test xs . nil) 'ratio)
   (_sref _help* 'nil 'ratio)
   (_sref _source-file* _current-load-file* 'ratio)
   (_sref
    _source*
    '(def
      ratio
      (test xs . nil)
      (if (empty xs . nil)
        0
        (/ (count test xs . nil) (len xs . nil) . nil)
        .
        nil)
      .
      nil)
    'ratio)
   ((lambda ()
      (if (not (ar-false? (_bound 'ratio)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ratio (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| ratio|
                      (lambda (test xs)
                        (if (not (ar-false? (_empty xs)))
                          0
                          (_/ (_count test xs) (_len xs))))))
                 | ratio|)))
          (namespace-set-variable-value! '_ratio zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ratio)))
     (_setinfixop 'ratio ((ar-coerce _infixop 'fn) 'ratio))
     'nil)))


((lambda ()
   (_sref _sig '(f . args) 'ofn)
   (_sref
    _help*
    '"Partial execution (Fills f with initial arg)s Example: ((ofn + 1 2) 3 4) => 10"
    'ofn)
   (_sref _source-file* _current-load-file* 'ofn)
   (_sref
    _source*
    '(def
      ofn
      (f . args)
      (fn lst (apply f (+ args lst . nil) . nil) . nil)
      .
      nil)
    'ofn)
   ((lambda ()
      (if (not (ar-false? (_bound 'ofn)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'ofn (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| ofn|
                      (lambda (f . args)
                        (let ((| ofn| (lambda lst (_apply f (_+ args lst)))))
                          | ofn|))))
                 | ofn|)))
          (namespace-set-variable-value! '_ofn zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'ofn)))
     (_setinfixop 'ofn ((ar-coerce _infixop 'fn) 'ofn))
     'nil)))


((lambda ()
   (_sref _sig '(f . nil) 'r)
   (_sref _help* 'nil 'r)
   (_sref _source-file* _current-load-file* 'r)
   (_sref
    _source*
    '(def r (f . nil) (fn args (apply f (rev args . nil) . nil) . nil) . nil)
    'r)
   ((lambda ()
      (if (not (ar-false? (_bound 'r)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'r (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| r|
                      (lambda (f)
                        (let ((| r| (lambda args (_apply f (_rev args)))))
                          | r|))))
                 | r|)))
          (namespace-set-variable-value! '_r zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'r)))
     (_setinfixop 'r ((ar-coerce _infixop 'fn) 'r))
     'nil)))


(_declaration 'test)


(_declare 'test 'nil)


((lambda ()
   (begin
     (let ((zz (_table))) (namespace-set-variable-value! '_examples* zz) zz))))


((lambda ()
   (_sref _sig '(sym expr . nil) 'example)
   (_sref _help* 'nil 'example)
   (_sref _source-file* _current-load-file* 'example)
   (_sref
    _source*
    '(mac
      example
      (sym expr . nil)
      (if (or (no:decl (quote test . nil) . nil) (eval expr . nil) . nil)
        (quasiquote
         (zap
          (make-br-fn (cons B A . nil) . nil)
          (examples* (quote (unquote sym . nil) . nil) . nil)
          (quote (unquote expr . nil) . nil)
          .
          nil)
         .
         nil)
        (err
         sym
         (string
          "Wrong "
          sym
          " example:\n"
          (tostring (ppr expr . nil) . nil)
          (and ((car expr . nil) is (quote is . nil) . nil)
               (string
                ":\n"
                (65 "=" . nil)
                "\n"
                (tostring:ppr (eval (car (cdr expr . nil) . nil) . nil) . nil)
                "\n!=\n"
                (tostring:ppr
                 (eval (car (cdr (cdr expr . nil) . nil) . nil) . nil)
                 .
                 nil)
                "\n"
                (65 "=" . nil)
                .
                nil)
               .
               nil)
          .
          nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'example)
   ((lambda ()
      (if (not (ar-false? (_bound 'example)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'example (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (_annotate
                'mac
                (let ((| example|
                       (lambda (sym expr)
                         (if (not
                              (ar-false?
                               ((let ((| example|
                                       (lambda (g2773)
                                         (if (not (ar-false? g2773))
                                           g2773
                                           ((let ((| example|
                                                   (lambda (g2774)
                                                     (if (not
                                                          (ar-false? g2774))
                                                       g2774
                                                       'nil))))
                                              | example|)
                                            (_eval expr))))))
                                  | example|)
                                (_no (_decl 'test)))))
                           `(zap
                             (make-br-fn (cons B A))
                             (examples* ',sym)
                             ',expr)
                           (_err
                            sym
                            (_string
                             "Wrong "
                             sym
                             " example:\n"
                             ((let ((| example|
                                     (lambda (g2775)
                                       (_call-w/stdout
                                        g2775
                                        (let ((| example|
                                               (lambda () (_ppr expr))))
                                          | example|))
                                       (_inside g2775))))
                                | example|)
                              (_outstring))
                             (if (not
                                  (ar-false?
                                   (ar-call-resolve-notation
                                    (_car expr)
                                    _is
                                    'is)))
                               (_string
                                ":\n"
                                ((ar-coerce 65 'fn) "=")
                                "\n"
                                ((let ((| example|
                                        (lambda (g2776)
                                          (_call-w/stdout
                                           g2776
                                           (let ((| example|
                                                  (lambda ()
                                                    (_ppr
                                                     (_eval
                                                      (_car (_cdr expr)))))))
                                             | example|))
                                          (_inside g2776))))
                                   | example|)
                                 (_outstring))
                                "\n!=\n"
                                ((let ((| example|
                                        (lambda (g2777)
                                          (_call-w/stdout
                                           g2777
                                           (let ((| example|
                                                  (lambda ()
                                                    (_ppr
                                                     (_eval
                                                      (_car
                                                       (_cdr (_cdr expr))))))))
                                             | example|))
                                          (_inside g2777))))
                                   | example|)
                                 (_outstring))
                                "\n"
                                ((ar-coerce 65 'fn) "="))
                               'nil)))))))
                  | example|))))
          (namespace-set-variable-value! '_example zz)
          zz))))))


((lambda ()
   (_sref _sig '(sym . nil) 'test)
   (_sref _help* 'nil 'test)
   (_sref _source-file* _current-load-file* 'test)
   (_sref
    _source*
    '(def
      test
      (sym . nil)
      (or (prall
           (keep no:eval (examples* sym . nil) . nil)
           (string sym ": Following tests failed:\n" . nil)
           "\n"
           .
           nil)
          (prn sym ": OK" . nil)
          .
          nil)
      .
      nil)
    'test)
   ((lambda ()
      (if (not (ar-false? (_bound 'test)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'test (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| test|
                      (lambda (sym)
                        ((let ((| test|
                                (lambda (g2778)
                                  (if (not (ar-false? g2778))
                                    g2778
                                    ((let ((| test|
                                            (lambda (g2779)
                                              (if (not (ar-false? g2779))
                                                g2779
                                                'nil))))
                                       | test|)
                                     (_prn sym ": OK"))))))
                           | test|)
                         (_prall
                          (_keep
                           (let ((| g2778|
                                  (lambda g2780 (_no (_apply _eval g2780)))))
                             | g2778|)
                           (ar-call-resolve-notation _examples* sym))
                          (_string sym ": Following tests failed:\n")
                          "\n")))))
                 | test|)))
          (namespace-set-variable-value! '_test zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'test)))
     (_setinfixop 'test ((ar-coerce _infixop 'fn) 'test))
     'nil)))


((lambda ()
   (_sref _sig '(fp . nil) 'file)
   (_sref _help* 'nil 'file)
   (_sref _source-file* _current-load-file* 'file)
   (_sref
    _source*
    '(def
      file
      (fp . nil)
      (if (isa fp (quote string . nil) . nil)
        (file (infile fp . nil) . nil)
        ((afn
          ((o str "" . nil) (o ls . nil) . nil)
          (if str
            (self (readline fp . nil) (cons str ls . nil) . nil)
            (cdr (rev ls . nil) . nil)
            .
            nil)
          .
          nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'file)
   ((lambda ()
      (if (not (ar-false? (_bound 'file)))
        ((lambda ()
           (_disp "*** redefining " (_stderr))
           (_disp 'file (_stderr))
           (_disp #\newline (_stderr))))
        'nil)
      (begin
        (let ((zz
               (let ((| file|
                      (lambda (fp)
                        (if (not (ar-false? (_isa fp 'string)))
                          (_file (_infile fp))
                          (((let ((| file|
                                   (lambda (self)
                                     (begin
                                       (let ((zz
                                              (lambda g2781
                                                (let* ((str
                                                        (if (pair? g2781)
                                                          (car g2781)
                                                          ""))
                                                       (ls
                                                        (if (pair?
                                                             (ar-xcdr g2781))
                                                          (car (ar-xcdr g2781))
                                                          'nil)))
                                                  (if (not (ar-false? str))
                                                    (ar-call-resolve-notation
                                                     self
                                                     (_readline fp)
                                                     (_cons str ls))
                                                    (_cdr (_rev ls)))))))
                                         (set! self zz)
                                         zz)))))
                              | file|)
                            'nil))))))
                 | file|)))
          (namespace-set-variable-value! '_file zz)
          zz))))
   (if (not (ar-false? ((ar-coerce _infixop 'fn) 'file)))
     (_setinfixop 'file ((ar-coerce _infixop 'fn) 'file))
     'nil)))


((lambda ()
   ((lambda ()
      (begin
        (let ((zz 'nil))
          (namespace-set-variable-value! '_current-load-file* zz)
          zz))))))


(_load "help/arc.arc")
