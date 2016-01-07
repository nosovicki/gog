((lambda ()
   ((ar-coerce _sref 'fn) _sig '(r g b . nil) 'color)
   ((ar-coerce _sref 'fn) _help* 'nil 'color)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'color)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      color
      (r g b . nil)
      (with
       (c
        (table . nil)
        f
        (fn (x . nil) (if (< x 0 . nil) 0 (> x 255 . nil) 255 x . nil) . nil)
        .
        nil)
       (=
        (c (quote r . nil) . nil)
        (f r . nil)
        (c (quote g . nil) . nil)
        (f g . nil)
        (c (quote b . nil) . nil)
        (f b . nil)
        .
        nil)
       c
       .
       nil)
      .
      nil)
    'color)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'color)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'color ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| color|
                      (lambda (r g b)
                        ((let ((| color|
                                (lambda (c f)
                                  ((let ((| color|
                                          (lambda ()
                                            ((ar-coerce _atomic-invoke 'fn)
                                             (let ((| color|
                                                    (lambda ()
                                                      ((let ((| color|
                                                              (lambda (g2453
                                                                       g2455
                                                                       g2456)
                                                                ((let ((| color|
                                                                        (lambda (g2454)
                                                                          ((ar-coerce
                                                                            _sref
                                                                            'fn)
                                                                           g2453
                                                                           g2454
                                                                           g2455))))
                                                                   | color|)
                                                                 g2456))))
                                                         | color|)
                                                       c
                                                       'r
                                                       ((ar-coerce f 'fn)
                                                        r)))))
                                               | color|))
                                            ((ar-coerce _atomic-invoke 'fn)
                                             (let ((| color|
                                                    (lambda ()
                                                      ((let ((| color|
                                                              (lambda (g2457
                                                                       g2459
                                                                       g2460)
                                                                ((let ((| color|
                                                                        (lambda (g2458)
                                                                          ((ar-coerce
                                                                            _sref
                                                                            'fn)
                                                                           g2457
                                                                           g2458
                                                                           g2459))))
                                                                   | color|)
                                                                 g2460))))
                                                         | color|)
                                                       c
                                                       'g
                                                       ((ar-coerce f 'fn)
                                                        g)))))
                                               | color|))
                                            ((ar-coerce _atomic-invoke 'fn)
                                             (let ((| color|
                                                    (lambda ()
                                                      ((let ((| color|
                                                              (lambda (g2461
                                                                       g2463
                                                                       g2464)
                                                                ((let ((| color|
                                                                        (lambda (g2462)
                                                                          ((ar-coerce
                                                                            _sref
                                                                            'fn)
                                                                           g2461
                                                                           g2462
                                                                           g2463))))
                                                                   | color|)
                                                                 g2464))))
                                                         | color|)
                                                       c
                                                       'b
                                                       ((ar-coerce f 'fn)
                                                        b)))))
                                               | color|)))))
                                     | color|))
                                  c)))
                           | color|)
                         ((ar-coerce _table 'fn))
                         (let ((| f|
                                (lambda (x)
                                  (if (not
                                       (ar-false? ((ar-coerce _< 'fn) x 0)))
                                    0
                                    (if (not
                                         (ar-false?
                                          ((ar-coerce _> 'fn) x 255)))
                                      255
                                      x)))))
                           | f|)))))
                 | color|)))
          (namespace-set-variable-value! '_color zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(str . nil) 'dehex)
   ((ar-coerce _sref 'fn) _help* 'nil 'dehex)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'dehex)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      dehex
      (str . nil)
      (errsafe (coerce str (quote int . nil) 16 . nil) . nil)
      .
      nil)
    'dehex)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'dehex)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'dehex ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| dehex|
                      (lambda (str)
                        ((ar-coerce _on-err 'fn)
                         (let ((| dehex| (lambda (c) 'nil))) | dehex|)
                         (let ((| dehex|
                                (lambda ()
                                  ((ar-coerce _coerce 'fn) str 'int 16))))
                           | dehex|)))))
                 | dehex|)))
          (namespace-set-variable-value! '_dehex zz)
          zz))))))


((lambda ()
   (if (not (ar-false? ((ar-coerce _bound 'fn) 'hex>color)))
     ((lambda ()
        ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) 'hex>color ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
     'nil)
   (begin
     (let ((zz
            ((ar-coerce _memo 'fn)
             (let ((| hex>color|
                    (lambda (str)
                      (if (not
                           (ar-false?
                            ((ar-coerce _is 'fn)
                             ((ar-coerce _len 'fn) str)
                             6)))
                        ((let ((| hex>color|
                                (lambda (r g b)
                                  (if (not (ar-false? r))
                                    (if (not (ar-false? g))
                                      (if (not (ar-false? b))
                                        ((ar-coerce _color 'fn) r g b)
                                        'nil)
                                      'nil)
                                    'nil))))
                           | hex>color|)
                         ((ar-coerce _dehex 'fn)
                          ((ar-coerce _cut 'fn) str 0 2))
                         ((ar-coerce _dehex 'fn)
                          ((ar-coerce _cut 'fn) str 2 4))
                         ((ar-coerce _dehex 'fn)
                          ((ar-coerce _cut 'fn) str 4 6)))
                        'nil))))
               | hex>color|))))
       (namespace-set-variable-value! '_hex>color zz)
       zz))))


((lambda ()
   (if (not (ar-false? ((ar-coerce _bound 'fn) 'gray)))
     ((lambda ()
        ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) 'gray ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
     'nil)
   (begin
     (let ((zz
            ((ar-coerce _memo 'fn)
             (let ((| gray| (lambda (n) ((ar-coerce _color 'fn) n n n))))
               | gray|))))
       (namespace-set-variable-value! '_gray zz)
       zz))))


((lambda ()
   (begin
     (let ((zz ((ar-coerce _gray 'fn) 255)))
       (namespace-set-variable-value! '_white zz)
       zz))
   (begin
     (let ((zz ((ar-coerce _gray 'fn) 0)))
       (namespace-set-variable-value! '_black zz)
       zz))
   (begin
     (let ((zz ((ar-coerce _color 'fn) 0 0 190)))
       (namespace-set-variable-value! '_linkblue zz)
       zz))
   (begin
     (let ((zz ((ar-coerce _color 'fn) 255 102 0)))
       (namespace-set-variable-value! '_orange zz)
       zz))
   (begin
     (let ((zz ((ar-coerce _color 'fn) 180 0 0)))
       (namespace-set-variable-value! '_darkred zz)
       zz))
   (begin
     (let ((zz ((ar-coerce _color 'fn) 0 0 120)))
       (namespace-set-variable-value! '_darkblue zz)
       zz))))


((lambda ()
   (begin
     (let ((zz ((ar-coerce _table 'fn))))
       (namespace-set-variable-value! '_opmeths* zz)
       zz))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'opmeth)
   ((ar-coerce _sref 'fn) _help* 'nil 'opmeth)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opmeth)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      opmeth
      args
      (quasiquote
       (opmeths* (list (unquote-splicing args . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'opmeth)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opmeth)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opmeth ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| opmeth|
                       (lambda args
                         `(opmeths* (list ,@(ar-nil-terminate args))))))
                  | opmeth|))))
          (namespace-set-variable-value! '_opmeth zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(tag opt f . nil) 'attribute)
   ((ar-coerce _sref 'fn) _help* 'nil 'attribute)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'attribute)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      attribute
      (tag opt f . nil)
      (quasiquote
       (=
        (opmeths*
         (list
          (quote (unquote tag . nil) . nil)
          (quote (unquote opt . nil) . nil)
          .
          nil)
         .
         nil)
        (unquote f . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'attribute)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'attribute)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'attribute ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| attribute|
                       (lambda (tag opt f)
                         `(= (opmeths* (list ',tag ',opt)) ,f))))
                  | attribute|))))
          (namespace-set-variable-value! '_attribute zz)
          zz))))))


((lambda ()
   (begin
     (let ((zz ((ar-coerce _table 'fn))))
       (namespace-set-variable-value! '_hexreps zz)
       zz))))


((lambda (i g2465 g2466)
   ((lambda ()
      (begin (let ((zz g2465)) (set! i zz) zz))
      ((ar-coerce
        ((lambda (g2467)
           (begin
             (let ((zz
                    (let ((| g2467|
                           (lambda (g2468)
                             (if (not (ar-false? g2468))
                               ((let ((| g2467|
                                       (lambda ()
                                         ((let ((| g2467|
                                                 (lambda ()
                                                   ((ar-coerce
                                                     _atomic-invoke
                                                     'fn)
                                                    (let ((| g2467|
                                                           (lambda ()
                                                             ((let ((| g2467|
                                                                     (lambda (g2469
                                                                              g2471
                                                                              g2472)
                                                                       ((let ((| g2467|
                                                                               (lambda (g2470)
                                                                                 ((ar-coerce
                                                                                   _sref
                                                                                   'fn)
                                                                                  g2469
                                                                                  g2470
                                                                                  g2471))))
                                                                          | g2467|)
                                                                        g2472))))
                                                                | g2467|)
                                                              _hexreps
                                                              i
                                                              ((let ((| g2472|
                                                                      (lambda (s)
                                                                        (if (not
                                                                             (ar-false?
                                                                              ((ar-coerce
                                                                                _is
                                                                                'fn)
                                                                               ((ar-coerce
                                                                                 _len
                                                                                 'fn)
                                                                                s)
                                                                               1)))
                                                                          ((ar-coerce
                                                                            _+
                                                                            'fn)
                                                                           "0"
                                                                           s)
                                                                          s))))
                                                                 | g2472|)
                                                               ((ar-coerce
                                                                 _coerce
                                                                 'fn)
                                                                i
                                                                'string
                                                                16))))))
                                                      | g2467|)))))
                                            | g2467|))
                                         (begin
                                           (let ((zz ((ar-coerce _+ 'fn) i 1)))
                                             (set! i zz)
                                             zz))
                                         ((ar-coerce g2467 'fn)
                                          ((ar-coerce _< 'fn) i g2466)))))
                                  | g2467|))
                               'nil))))
                      | g2467|)))
               (set! g2467 zz)
               zz)))
         'nil)
        'fn)
       ((ar-coerce _< 'fn) i g2466)))))
 'nil
 0
 ((ar-coerce _+ 'fn) 255 1))


((lambda ()
   (if (not (ar-false? ((ar-coerce _bound 'fn) 'hexrep)))
     ((lambda ()
        ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) 'hexrep ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
     'nil)
   (begin
     (let ((zz
            ((ar-coerce _memo 'fn)
             (let ((| hexrep|
                    (lambda (col)
                      ((ar-coerce _+ 'fn)
                       ((ar-coerce _hexreps 'fn) ((ar-coerce col 'fn) 'r))
                       ((ar-coerce _hexreps 'fn) ((ar-coerce col 'fn) 'g))
                       ((ar-coerce _hexreps 'fn) ((ar-coerce col 'fn) 'b))))))
               | hexrep|))))
       (namespace-set-variable-value! '_hexrep zz)
       zz))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opcolor)
   ((ar-coerce _sref 'fn) _help* 'nil 'opcolor)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opcolor)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opcolor
      (key val . nil)
      (w/uniq
       gv
       (quasiquote
        (whenlet
         (unquote gv . nil)
         (unquote val . nil)
         (pr
          (unquote (string " " key "=#" . nil) . nil)
          (hexrep (unquote gv . nil) . nil)
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
    'opcolor)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opcolor)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opcolor ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opcolor|
                      (lambda (key val)
                        ((let ((| opcolor|
                                (lambda (gv)
                                  `(whenlet
                                    ,gv
                                    ,val
                                    (pr
                                     ,((ar-coerce _string 'fn) " " key "=#")
                                     (hexrep ,gv))))))
                           | opcolor|)
                         ((ar-coerce _uniq 'fn))))))
                 | opcolor|)))
          (namespace-set-variable-value! '_opcolor zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opstring)
   ((ar-coerce _sref 'fn) _help* 'nil 'opstring)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opstring)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opstring
      (key val . nil)
      (quasiquote
       (aif
        (unquote val . nil)
        (pr (unquote (+ " " key "=\"" . nil) . nil) it #\" . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'opstring)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opstring)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opstring ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opstring|
                      (lambda (key val)
                        `(aif
                          ,val
                          (pr ,((ar-coerce _+ 'fn) " " key "=\"") it #\")))))
                 | opstring|)))
          (namespace-set-variable-value! '_opstring zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opnum)
   ((ar-coerce _sref 'fn) _help* 'nil 'opnum)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opnum)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opnum
      (key val . nil)
      (quasiquote
       (aif
        (unquote val . nil)
        (pr (unquote (+ " " key "=" . nil) . nil) it . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'opnum)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opnum)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opnum ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opnum|
                      (lambda (key val)
                        `(aif
                          ,val
                          (pr ,((ar-coerce _+ 'fn) " " key "=") it)))))
                 | opnum|)))
          (namespace-set-variable-value! '_opnum zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opsym)
   ((ar-coerce _sref 'fn) _help* 'nil 'opsym)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opsym)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opsym
      (key val . nil)
      (quasiquote
       (pr (unquote (+ " " key "=" . nil) . nil) (unquote val . nil) . nil)
       .
       nil)
      .
      nil)
    'opsym)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opsym)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opsym ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opsym|
                      (lambda (key val)
                        `(pr ,((ar-coerce _+ 'fn) " " key "=") ,val))))
                 | opsym|)))
          (namespace-set-variable-value! '_opsym zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opsel)
   ((ar-coerce _sref 'fn) _help* 'nil 'opsel)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opsel)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opsel
      (key val . nil)
      (quasiquote (if (unquote val . nil) (pr " selected" . nil) . nil) . nil)
      .
      nil)
    'opsel)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opsel)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opsel ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opsel| (lambda (key val) `(if ,val (pr " selected")))))
                 | opsel|)))
          (namespace-set-variable-value! '_opsel zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opcheck)
   ((ar-coerce _sref 'fn) _help* 'nil 'opcheck)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opcheck)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opcheck
      (key val . nil)
      (quasiquote (if (unquote val . nil) (pr " checked" . nil) . nil) . nil)
      .
      nil)
    'opcheck)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opcheck)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opcheck ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opcheck|
                      (lambda (key val) `(if ,val (pr " checked")))))
                 | opcheck|)))
          (namespace-set-variable-value! '_opcheck zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(key val . nil) 'opesc)
   ((ar-coerce _sref 'fn) _help* 'nil 'opesc)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'opesc)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      opesc
      (key val . nil)
      (quasiquote
       (awhen
        (unquote val . nil)
        (pr (unquote (string " " key "=\"" . nil) . nil) . nil)
        (if (isa it (quote string . nil) . nil)
          (pr-escaped it . nil)
          (pr it . nil)
          .
          nil)
        (pr #\" . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'opesc)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'opesc)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'opesc ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| opesc|
                      (lambda (key val)
                        `(awhen
                          ,val
                          (pr ,((ar-coerce _string 'fn) " " key "=\""))
                          (if (isa it 'string) (pr-escaped it) (pr it))
                          (pr #\")))))
                 | opesc|)))
          (namespace-set-variable-value! '_opesc zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(x . nil) 'pr-escaped)
   ((ar-coerce _sref 'fn) _help* 'nil 'pr-escaped)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'pr-escaped)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      pr-escaped
      (x . nil)
      (each
       c
       x
       (pr
        (case c #\< "&#60;" #\> "&#62;" #\" "&#34;" #\& "&#38;" c . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'pr-escaped)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'pr-escaped)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'pr-escaped ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| pr-escaped|
                      (lambda (x)
                        ((ar-coerce _walk 'fn)
                         x
                         (let ((| pr-escaped|
                                (lambda (c)
                                  ((ar-coerce _pr 'fn)
                                   ((let ((| pr-escaped|
                                           (lambda (g2473)
                                             (if (not
                                                  (ar-false?
                                                   ((ar-coerce _is 'fn)
                                                    g2473
                                                    '#\<)))
                                               "&#60;"
                                               (if (not
                                                    (ar-false?
                                                     ((ar-coerce _is 'fn)
                                                      g2473
                                                      '#\>)))
                                                 "&#62;"
                                                 (if (not
                                                      (ar-false?
                                                       ((ar-coerce _is 'fn)
                                                        g2473
                                                        '#\")))
                                                   "&#34;"
                                                   (if (not
                                                        (ar-false?
                                                         ((ar-coerce _is 'fn)
                                                          g2473
                                                          '#\&)))
                                                     "&#38;"
                                                     c)))))))
                                      | pr-escaped|)
                                    c)))))
                           | pr-escaped|)))))
                 | pr-escaped|)))
          (namespace-set-variable-value! '_pr-escaped zz)
          zz))))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2474 g2476 g2477)
         ((lambda (g2475) ((ar-coerce _sref 'fn) g2474 g2475 g2476)) g2477))
       _opmeths*
       ((ar-coerce _list 'fn) 'a 'href)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2478 g2480 g2481)
         ((lambda (g2479) ((ar-coerce _sref 'fn) g2478 g2479 g2480)) g2481))
       _opmeths*
       ((ar-coerce _list 'fn) 'a 'rel)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2482 g2484 g2485)
         ((lambda (g2483) ((ar-coerce _sref 'fn) g2482 g2483 g2484)) g2485))
       _opmeths*
       ((ar-coerce _list 'fn) 'a 'class)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2486 g2488 g2489)
         ((lambda (g2487) ((ar-coerce _sref 'fn) g2486 g2487 g2488)) g2489))
       _opmeths*
       ((ar-coerce _list 'fn) 'a 'id)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2490 g2492 g2493)
         ((lambda (g2491) ((ar-coerce _sref 'fn) g2490 g2491 g2492)) g2493))
       _opmeths*
       ((ar-coerce _list 'fn) 'a 'onclick)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2494 g2496 g2497)
         ((lambda (g2495) ((ar-coerce _sref 'fn) g2494 g2495 g2496)) g2497))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'alink)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2498 g2500 g2501)
         ((lambda (g2499) ((ar-coerce _sref 'fn) g2498 g2499 g2500)) g2501))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'bgcolor)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2502 g2504 g2505)
         ((lambda (g2503) ((ar-coerce _sref 'fn) g2502 g2503 g2504)) g2505))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'leftmargin)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2506 g2508 g2509)
         ((lambda (g2507) ((ar-coerce _sref 'fn) g2506 g2507 g2508)) g2509))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'link)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2510 g2512 g2513)
         ((lambda (g2511) ((ar-coerce _sref 'fn) g2510 g2511 g2512)) g2513))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'marginheight)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2514 g2516 g2517)
         ((lambda (g2515) ((ar-coerce _sref 'fn) g2514 g2515 g2516)) g2517))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'marginwidth)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2518 g2520 g2521)
         ((lambda (g2519) ((ar-coerce _sref 'fn) g2518 g2519 g2520)) g2521))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'topmargin)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2522 g2524 g2525)
         ((lambda (g2523) ((ar-coerce _sref 'fn) g2522 g2523 g2524)) g2525))
       _opmeths*
       ((ar-coerce _list 'fn) 'body 'vlink)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2526 g2528 g2529)
         ((lambda (g2527) ((ar-coerce _sref 'fn) g2526 g2527 g2528)) g2529))
       _opmeths*
       ((ar-coerce _list 'fn) 'font 'color)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2530 g2532 g2533)
         ((lambda (g2531) ((ar-coerce _sref 'fn) g2530 g2531 g2532)) g2533))
       _opmeths*
       ((ar-coerce _list 'fn) 'font 'face)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2534 g2536 g2537)
         ((lambda (g2535) ((ar-coerce _sref 'fn) g2534 g2535 g2536)) g2537))
       _opmeths*
       ((ar-coerce _list 'fn) 'font 'size)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2538 g2540 g2541)
         ((lambda (g2539) ((ar-coerce _sref 'fn) g2538 g2539 g2540)) g2541))
       _opmeths*
       ((ar-coerce _list 'fn) 'form 'action)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2542 g2544 g2545)
         ((lambda (g2543) ((ar-coerce _sref 'fn) g2542 g2543 g2544)) g2545))
       _opmeths*
       ((ar-coerce _list 'fn) 'form 'method)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2546 g2548 g2549)
         ((lambda (g2547) ((ar-coerce _sref 'fn) g2546 g2547 g2548)) g2549))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'align)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2550 g2552 g2553)
         ((lambda (g2551) ((ar-coerce _sref 'fn) g2550 g2551 g2552)) g2553))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'border)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2554 g2556 g2557)
         ((lambda (g2555) ((ar-coerce _sref 'fn) g2554 g2555 g2556)) g2557))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'height)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2558 g2560 g2561)
         ((lambda (g2559) ((ar-coerce _sref 'fn) g2558 g2559 g2560)) g2561))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'width)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2562 g2564 g2565)
         ((lambda (g2563) ((ar-coerce _sref 'fn) g2562 g2563 g2564)) g2565))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'vspace)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2566 g2568 g2569)
         ((lambda (g2567) ((ar-coerce _sref 'fn) g2566 g2567 g2568)) g2569))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'hspace)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2570 g2572 g2573)
         ((lambda (g2571) ((ar-coerce _sref 'fn) g2570 g2571 g2572)) g2573))
       _opmeths*
       ((ar-coerce _list 'fn) 'img 'src)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2574 g2576 g2577)
         ((lambda (g2575) ((ar-coerce _sref 'fn) g2574 g2575 g2576)) g2577))
       _opmeths*
       ((ar-coerce _list 'fn) 'input 'name)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2578 g2580 g2581)
         ((lambda (g2579) ((ar-coerce _sref 'fn) g2578 g2579 g2580)) g2581))
       _opmeths*
       ((ar-coerce _list 'fn) 'input 'size)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2582 g2584 g2585)
         ((lambda (g2583) ((ar-coerce _sref 'fn) g2582 g2583 g2584)) g2585))
       _opmeths*
       ((ar-coerce _list 'fn) 'input 'type)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2586 g2588 g2589)
         ((lambda (g2587) ((ar-coerce _sref 'fn) g2586 g2587 g2588)) g2589))
       _opmeths*
       ((ar-coerce _list 'fn) 'input 'value)
       _opesc)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2590 g2592 g2593)
         ((lambda (g2591) ((ar-coerce _sref 'fn) g2590 g2591 g2592)) g2593))
       _opmeths*
       ((ar-coerce _list 'fn) 'input 'checked)
       _opcheck)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2594 g2596 g2597)
         ((lambda (g2595) ((ar-coerce _sref 'fn) g2594 g2595 g2596)) g2597))
       _opmeths*
       ((ar-coerce _list 'fn) 'select 'name)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2598 g2600 g2601)
         ((lambda (g2599) ((ar-coerce _sref 'fn) g2598 g2599 g2600)) g2601))
       _opmeths*
       ((ar-coerce _list 'fn) 'option 'selected)
       _opsel)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2602 g2604 g2605)
         ((lambda (g2603) ((ar-coerce _sref 'fn) g2602 g2603 g2604)) g2605))
       _opmeths*
       ((ar-coerce _list 'fn) 'table 'bgcolor)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2606 g2608 g2609)
         ((lambda (g2607) ((ar-coerce _sref 'fn) g2606 g2607 g2608)) g2609))
       _opmeths*
       ((ar-coerce _list 'fn) 'table 'border)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2610 g2612 g2613)
         ((lambda (g2611) ((ar-coerce _sref 'fn) g2610 g2611 g2612)) g2613))
       _opmeths*
       ((ar-coerce _list 'fn) 'table 'cellpadding)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2614 g2616 g2617)
         ((lambda (g2615) ((ar-coerce _sref 'fn) g2614 g2615 g2616)) g2617))
       _opmeths*
       ((ar-coerce _list 'fn) 'table 'cellspacing)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2618 g2620 g2621)
         ((lambda (g2619) ((ar-coerce _sref 'fn) g2618 g2619 g2620)) g2621))
       _opmeths*
       ((ar-coerce _list 'fn) 'table 'width)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2622 g2624 g2625)
         ((lambda (g2623) ((ar-coerce _sref 'fn) g2622 g2623 g2624)) g2625))
       _opmeths*
       ((ar-coerce _list 'fn) 'textarea 'cols)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2626 g2628 g2629)
         ((lambda (g2627) ((ar-coerce _sref 'fn) g2626 g2627 g2628)) g2629))
       _opmeths*
       ((ar-coerce _list 'fn) 'textarea 'name)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2630 g2632 g2633)
         ((lambda (g2631) ((ar-coerce _sref 'fn) g2630 g2631 g2632)) g2633))
       _opmeths*
       ((ar-coerce _list 'fn) 'textarea 'rows)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2634 g2636 g2637)
         ((lambda (g2635) ((ar-coerce _sref 'fn) g2634 g2635 g2636)) g2637))
       _opmeths*
       ((ar-coerce _list 'fn) 'textarea 'wrap)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2638 g2640 g2641)
         ((lambda (g2639) ((ar-coerce _sref 'fn) g2638 g2639 g2640)) g2641))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'align)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2642 g2644 g2645)
         ((lambda (g2643) ((ar-coerce _sref 'fn) g2642 g2643 g2644)) g2645))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'bgcolor)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2646 g2648 g2649)
         ((lambda (g2647) ((ar-coerce _sref 'fn) g2646 g2647 g2648)) g2649))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'colspan)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2650 g2652 g2653)
         ((lambda (g2651) ((ar-coerce _sref 'fn) g2650 g2651 g2652)) g2653))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'width)
       _opnum)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2654 g2656 g2657)
         ((lambda (g2655) ((ar-coerce _sref 'fn) g2654 g2655 g2656)) g2657))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'valign)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2658 g2660 g2661)
         ((lambda (g2659) ((ar-coerce _sref 'fn) g2658 g2659 g2660)) g2661))
       _opmeths*
       ((ar-coerce _list 'fn) 'td 'class)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2662 g2664 g2665)
         ((lambda (g2663) ((ar-coerce _sref 'fn) g2662 g2663 g2664)) g2665))
       _opmeths*
       ((ar-coerce _list 'fn) 'tr 'bgcolor)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2666 g2668 g2669)
         ((lambda (g2667) ((ar-coerce _sref 'fn) g2666 g2667 g2668)) g2669))
       _opmeths*
       ((ar-coerce _list 'fn) 'hr 'color)
       _opcolor)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2670 g2672 g2673)
         ((lambda (g2671) ((ar-coerce _sref 'fn) g2670 g2671 g2672)) g2673))
       _opmeths*
       ((ar-coerce _list 'fn) 'span 'class)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2674 g2676 g2677)
         ((lambda (g2675) ((ar-coerce _sref 'fn) g2674 g2675 g2676)) g2677))
       _opmeths*
       ((ar-coerce _list 'fn) 'span 'align)
       _opstring)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2678 g2680 g2681)
         ((lambda (g2679) ((ar-coerce _sref 'fn) g2678 g2679 g2680)) g2681))
       _opmeths*
       ((ar-coerce _list 'fn) 'span 'id)
       _opsym)))))


((lambda ()
   ((ar-coerce _atomic-invoke 'fn)
    (lambda ()
      ((lambda (g2682 g2684 g2685)
         ((lambda (g2683) ((ar-coerce _sref 'fn) g2682 g2683 g2684)) g2685))
       _opmeths*
       ((ar-coerce _list 'fn) 'rss 'version)
       _opstring)))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'gentag)
   ((ar-coerce _sref 'fn) _help* 'nil 'gentag)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'gentag)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac gentag args (start-tag args . nil) . nil)
    'gentag)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'gentag)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'gentag ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| gentag|
                       (lambda args ((ar-coerce _start-tag 'fn) args))))
                  | gentag|))))
          (namespace-set-variable-value! '_gentag zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(spec . body) 'tag)
   ((ar-coerce _sref 'fn) _help* 'nil 'tag)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tag)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      tag
      (spec . body)
      (quasiquote
       (do (unquote (start-tag spec . nil) . nil)
           (unquote-splicing body . nil)
         (unquote (end-tag spec . nil) . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'tag)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'tag)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'tag ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| tag|
                       (lambda (spec . body)
                         `(do ,((ar-coerce _start-tag 'fn) spec)
                              ,@(ar-nil-terminate body)
                            ,((ar-coerce _end-tag 'fn) spec)))))
                  | tag|))))
          (namespace-set-variable-value! '_tag zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(test spec . body) 'tag-if)
   ((ar-coerce _sref 'fn) _help* 'nil 'tag-if)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tag-if)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      tag-if
      (test spec . body)
      (quasiquote
       (if (unquote test . nil)
         (tag (unquote spec . nil) (unquote-splicing body . nil) . nil)
         (do (unquote-splicing body . nil) . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'tag-if)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'tag-if)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'tag-if ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| tag-if|
                       (lambda (test spec . body)
                         `(if ,test
                            (tag ,spec ,@(ar-nil-terminate body))
                            (do ,@(ar-nil-terminate body))))))
                  | tag-if|))))
          (namespace-set-variable-value! '_tag-if zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(spec . nil) 'start-tag)
   ((ar-coerce _sref 'fn) _help* 'nil 'start-tag)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'start-tag)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      start-tag
      (spec . nil)
      (if (atom spec . nil)
        (quasiquote
         (pr (unquote (string "<" spec ">" . nil) . nil) . nil)
         .
         nil)
        (let opts (tag-options
                   (car spec . nil)
                   (pair (cdr spec . nil) . nil)
                   .
                   nil)
          (if (all
               (make-br-fn (isa _ (quote string . nil) . nil) . nil)
               opts
               .
               nil)
            (quasiquote
             (pr
              (unquote
               (string
                "<"
                (car spec . nil)
                (apply string opts . nil)
                ">"
                .
                nil)
               .
               nil)
              .
              nil)
             .
             nil)
            (quasiquote
             (do (pr (unquote (string "<" (car spec . nil) . nil) . nil) . nil)
                 (unquote-splicing
                  (map
                   (fn
                    (opt . nil)
                    (if (isa opt (quote string . nil) . nil)
                      (quasiquote (pr (unquote opt . nil) . nil) . nil)
                      opt
                      .
                      nil)
                    .
                    nil)
                   opts
                   .
                   nil)
                  .
                  nil)
               (pr ">" . nil)
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
    'start-tag)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'start-tag)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'start-tag ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| start-tag|
                      (lambda (spec)
                        (if (not (ar-false? ((ar-coerce _atom 'fn) spec)))
                          `(pr ,((ar-coerce _string 'fn) "<" spec ">"))
                          ((let ((| start-tag|
                                  (lambda (opts)
                                    (if (not
                                         (ar-false?
                                          ((ar-coerce _all 'fn)
                                           (let ((| start-tag|
                                                  (lambda (_)
                                                    ((ar-coerce _isa 'fn)
                                                     _
                                                     'string))))
                                             | start-tag|)
                                           opts)))
                                      `(pr
                                        ,((ar-coerce _string 'fn)
                                          "<"
                                          ((ar-coerce _car 'fn) spec)
                                          ((ar-coerce _apply 'fn) _string opts)
                                          ">"))
                                      `(do (pr
                                            ,((ar-coerce _string 'fn)
                                              "<"
                                              ((ar-coerce _car 'fn) spec)))
                                           (unquote-splicing
                                            (ar-nil-terminate
                                             ((ar-coerce _map 'fn)
                                              (let ((| start-tag|
                                                     (lambda (opt)
                                                       (if (not
                                                            (ar-false?
                                                             ((ar-coerce
                                                               _isa
                                                               'fn)
                                                              opt
                                                              'string)))
                                                         `(pr ,opt)
                                                         opt))))
                                                | start-tag|)
                                              opts)))
                                         (pr ">"))))))
                             | start-tag|)
                           ((ar-coerce _tag-options 'fn)
                            ((ar-coerce _car 'fn) spec)
                            ((ar-coerce _pair 'fn)
                             ((ar-coerce _cdr 'fn) spec))))))))
                 | start-tag|)))
          (namespace-set-variable-value! '_start-tag zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(spec . nil) 'end-tag)
   ((ar-coerce _sref 'fn) _help* 'nil 'end-tag)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'end-tag)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      end-tag
      (spec . nil)
      (quasiquote
       (pr (unquote (string "</" (carif spec . nil) ">" . nil) . nil) . nil)
       .
       nil)
      .
      nil)
    'end-tag)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'end-tag)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'end-tag ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| end-tag|
                      (lambda (spec)
                        `(pr
                          ,((ar-coerce _string 'fn)
                            "</"
                            ((ar-coerce _carif 'fn) spec)
                            ">")))))
                 | end-tag|)))
          (namespace-set-variable-value! '_end-tag zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(x . nil) 'literal)
   ((ar-coerce _sref 'fn) _help* 'nil 'literal)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'literal)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      literal
      (x . nil)
      (case (type x . nil)
        sym
        (in x nil t . nil)
        cons
        (caris x (quote quote . nil) . nil)
        t
        .
        nil)
      .
      nil)
    'literal)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'literal)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'literal ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| literal|
                      (lambda (x)
                        ((let ((| literal|
                                (lambda (g2686)
                                  (if (not
                                       (ar-false?
                                        ((ar-coerce _is 'fn) g2686 'sym)))
                                    ((let ((| literal|
                                            (lambda (g2687)
                                              ((let ((| literal|
                                                      (lambda (g2688)
                                                        (if (not
                                                             (ar-false? g2688))
                                                          g2688
                                                          ((let ((| literal|
                                                                  (lambda (g2689)
                                                                    (if (not
                                                                         (ar-false?
                                                                          g2689))
                                                                      g2689
                                                                      'nil))))
                                                             | literal|)
                                                           ((ar-coerce _is 'fn)
                                                            g2687
                                                            _t))))))
                                                 | literal|)
                                               ((ar-coerce _is 'fn)
                                                g2687
                                                'nil)))))
                                       | literal|)
                                     x)
                                    (if (not
                                         (ar-false?
                                          ((ar-coerce _is 'fn) g2686 'cons)))
                                      ((ar-coerce _caris 'fn) x 'quote)
                                      _t)))))
                           | literal|)
                         ((ar-coerce _type 'fn) x)))))
                 | literal|)))
          (namespace-set-variable-value! '_literal zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(spec options . nil) 'tag-options)
   ((ar-coerce _sref 'fn) _help* 'nil 'tag-options)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tag-options)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      tag-options
      (spec options . nil)
      (if (no options . nil)
        (quote nil . nil)
        (let ((opt val . nil) . rest)
          options
          (let meth (if
                     (is opt (quote style . nil) . nil)
                     opstring
                     (opmeth spec opt . nil)
                     .
                     nil)
            (if meth
              (if val
                (cons
                 (if (precomputable-tagopt val . nil)
                   (tostring (eval (meth opt val . nil) . nil) . nil)
                   (meth opt val . nil)
                   .
                   nil)
                 (tag-options spec rest . nil)
                 .
                 nil)
                (tag-options spec rest . nil)
                .
                nil)
              (do (pr "<!-- ignoring " opt " for " spec "-->" . nil)
                  (tag-options spec rest . nil)
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
    'tag-options)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'tag-options)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'tag-options ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| tag-options|
                      (lambda (spec options)
                        (if (not (ar-false? ((ar-coerce _no 'fn) options)))
                          'nil
                          ((lambda g2690
                             (let* ((opt (ar-xcar (ar-xcar (car g2690))))
                                    (val
                                     (ar-xcar (ar-xcdr (ar-xcar (car g2690)))))
                                    (rest (ar-xcdr (car g2690))))
                               ((let ((| tag-options|
                                       (lambda (meth)
                                         (if (not (ar-false? meth))
                                           (if (not (ar-false? val))
                                             ((ar-coerce _cons 'fn)
                                              (if (not
                                                   (ar-false?
                                                    ((ar-coerce
                                                      _precomputable-tagopt
                                                      'fn)
                                                     val)))
                                                ((let ((| tag-options|
                                                        (lambda (g2691)
                                                          ((ar-coerce
                                                            _call-w/stdout
                                                            'fn)
                                                           g2691
                                                           (let ((| tag-options|
                                                                  (lambda ()
                                                                    ((ar-coerce
                                                                      _eval
                                                                      'fn)
                                                                     ((ar-coerce
                                                                       meth
                                                                       'fn)
                                                                      opt
                                                                      val)))))
                                                             | tag-options|))
                                                          ((ar-coerce
                                                            _inside
                                                            'fn)
                                                           g2691))))
                                                   | tag-options|)
                                                 ((ar-coerce _outstring 'fn)))
                                                ((ar-coerce meth 'fn) opt val))
                                              ((ar-coerce _tag-options 'fn)
                                               spec
                                               rest))
                                             ((ar-coerce _tag-options 'fn)
                                              spec
                                              rest))
                                           ((let ((| tag-options|
                                                   (lambda ()
                                                     ((ar-coerce _pr 'fn)
                                                      "<!-- ignoring "
                                                      opt
                                                      " for "
                                                      spec
                                                      "-->")
                                                     ((ar-coerce
                                                       _tag-options
                                                       'fn)
                                                      spec
                                                      rest))))
                                              | tag-options|))))))
                                  | tag-options|)
                                (if (not
                                     (ar-false?
                                      ((ar-coerce _is 'fn) opt 'style)))
                                  _opstring
                                  ((ar-coerce _opmeths* 'fn)
                                   ((ar-coerce _list 'fn) spec opt))))))
                           options)))))
                 | tag-options|)))
          (namespace-set-variable-value! '_tag-options zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(val . nil) 'precomputable-tagopt)
   ((ar-coerce _sref 'fn) _help* 'nil 'precomputable-tagopt)
   ((ar-coerce _sref 'fn)
    _source-file*
    _current-load-file*
    'precomputable-tagopt)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      precomputable-tagopt
      (val . nil)
      (and (literal val . nil)
           (no
            (and (is (type val . nil) (quote string . nil) . nil)
                 (find #\@ val . nil)
                 .
                 nil)
            .
            nil)
           .
           nil)
      .
      nil)
    'precomputable-tagopt)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'precomputable-tagopt)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn)
            'precomputable-tagopt
            ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| precomputable-tagopt|
                      (lambda (val)
                        (if (not (ar-false? ((ar-coerce _literal 'fn) val)))
                          ((ar-coerce _no 'fn)
                           (if (not
                                (ar-false?
                                 ((ar-coerce _is 'fn)
                                  ((ar-coerce _type 'fn) val)
                                  'string)))
                             ((ar-coerce _find 'fn) #\@ val)
                             'nil))
                          'nil))))
                 | precomputable-tagopt|)))
          (namespace-set-variable-value! '_precomputable-tagopt zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '((o n 1 . nil) . nil) 'br)
   ((ar-coerce _sref 'fn) _help* 'nil 'br)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'br)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      br
      ((o n 1 . nil) . nil)
      (repeat n (pr "<br>" . nil) . nil)
      (prn . nil)
      .
      nil)
    'br)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'br)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'br ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2692
                 (let* ((n (if (pair? g2692) (car g2692) 1)))
                   ((let ((| br|
                           (lambda (g2693 g2694 g2695)
                             ((let ((| br|
                                     (lambda ()
                                       (begin
                                         (let ((zz g2694)) (set! g2693 zz) zz))
                                       ((ar-coerce
                                         ((let ((| br|
                                                 (lambda (g2696)
                                                   (begin
                                                     (let ((zz
                                                            (let ((| g2696|
                                                                   (lambda (g2697)
                                                                     (if (not
                                                                          (ar-false?
                                                                           g2697))
                                                                       ((let ((| g2696|
                                                                               (lambda ()
                                                                                 ((ar-coerce
                                                                                   _pr
                                                                                   'fn)
                                                                                  "<br>")
                                                                                 (begin
                                                                                   (let ((zz
                                                                                          ((ar-coerce
                                                                                            _+
                                                                                            'fn)
                                                                                           g2693
                                                                                           1)))
                                                                                     (set! g2693
                                                                                       zz)
                                                                                     zz))
                                                                                 ((ar-coerce
                                                                                   g2696
                                                                                   'fn)
                                                                                  ((ar-coerce
                                                                                    _<
                                                                                    'fn)
                                                                                   g2693
                                                                                   g2695)))))
                                                                          | g2696|))
                                                                       'nil))))
                                                              | g2696|)))
                                                       (set! g2696 zz)
                                                       zz)))))
                                            | br|)
                                          'nil)
                                         'fn)
                                        ((ar-coerce _< 'fn) g2693 g2695)))))
                                | br|)))))
                      | br|)
                    'nil
                    1
                    ((ar-coerce _+ 'fn) n 1))
                   ((ar-coerce _prn 'fn))))))
          (namespace-set-variable-value! '_br zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'nil 'br2)
   ((ar-coerce _sref 'fn) _help* 'nil 'br2)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'br2)
   ((ar-coerce _sref 'fn)
    _source*
    '(def br2 nil (prn "<br><br>" . nil) . nil)
    'br2)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'br2)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'br2 ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| br2| (lambda () ((ar-coerce _prn 'fn) "<br><br>"))))
                 | br2|)))
          (namespace-set-variable-value! '_br2 zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'center)
   ((ar-coerce _sref 'fn) _help* 'nil 'center)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'center)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      center
      body
      (quasiquote (tag center (unquote-splicing body . nil) . nil) . nil)
      .
      nil)
    'center)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'center)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'center ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| center|
                       (lambda body `(tag center ,@(ar-nil-terminate body)))))
                  | center|))))
          (namespace-set-variable-value! '_center zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'underline)
   ((ar-coerce _sref 'fn) _help* 'nil 'underline)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'underline)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      underline
      body
      (quasiquote (tag u (unquote-splicing body . nil) . nil) . nil)
      .
      nil)
    'underline)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'underline)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'underline ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| underline|
                       (lambda body `(tag u ,@(ar-nil-terminate body)))))
                  | underline|))))
          (namespace-set-variable-value! '_underline zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'tab)
   ((ar-coerce _sref 'fn) _help* 'nil 'tab)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tab)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      tab
      body
      (quasiquote
       (tag (table border 0 . nil) (unquote-splicing body . nil) . nil)
       .
       nil)
      .
      nil)
    'tab)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'tab)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'tab ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| tab|
                       (lambda body
                         `(tag (table border 0) ,@(ar-nil-terminate body)))))
                  | tab|))))
          (namespace-set-variable-value! '_tab zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'tr)
   ((ar-coerce _sref 'fn) _help* 'nil 'tr)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tr)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      tr
      body
      (quasiquote (tag tr (unquote-splicing body . nil) . nil) . nil)
      .
      nil)
    'tr)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'tr)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'tr ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| tr|
                       (lambda body `(tag tr ,@(ar-nil-terminate body)))))
                  | tr|))))
          (namespace-set-variable-value! '_tr zz)
          zz))))))


((lambda (pratoms)
   ((lambda ()
      ((ar-coerce _sref 'fn) _sig 'body 'td)
      ((ar-coerce _sref 'fn) _help* 'nil 'td)
      ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'td)
      ((ar-coerce _sref 'fn)
       _source*
       '(mac
         td
         body
         (quasiquote
          (tag td (unquote-splicing (pratoms body . nil) . nil) . nil)
          .
          nil)
         .
         nil)
       'td)
      ((lambda ()
         (if (not (ar-false? ((ar-coerce _bound 'fn) 'td)))
           ((lambda ()
              ((ar-coerce _disp 'fn)
               "*** redefining "
               ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) 'td ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
           'nil)
         (begin
           (let ((zz
                  ((ar-coerce _annotate 'fn)
                   'mac
                   (let ((| td|
                          (lambda body
                            `(tag
                              td
                              ,@(ar-nil-terminate
                                 ((ar-coerce pratoms 'fn) body))))))
                     | td|))))
             (namespace-set-variable-value! '_td zz)
             zz))))))
   ((lambda ()
      ((ar-coerce _sref 'fn) _sig 'body 'trtd)
      ((ar-coerce _sref 'fn) _help* 'nil 'trtd)
      ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'trtd)
      ((ar-coerce _sref 'fn)
       _source*
       '(mac
         trtd
         body
         (quasiquote
          (tr (td (unquote-splicing (pratoms body . nil) . nil) . nil) . nil)
          .
          nil)
         .
         nil)
       'trtd)
      ((lambda ()
         (if (not (ar-false? ((ar-coerce _bound 'fn) 'trtd)))
           ((lambda ()
              ((ar-coerce _disp 'fn)
               "*** redefining "
               ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) 'trtd ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
           'nil)
         (begin
           (let ((zz
                  ((ar-coerce _annotate 'fn)
                   'mac
                   (let ((| trtd|
                          (lambda body
                            `(tr
                              (td
                               ,@(ar-nil-terminate
                                  ((ar-coerce pratoms 'fn) body)))))))
                     | trtd|))))
             (namespace-set-variable-value! '_trtd zz)
             zz))))))
   ((lambda ()
      ((ar-coerce _sref 'fn) _sig 'body 'tdr)
      ((ar-coerce _sref 'fn) _help* 'nil 'tdr)
      ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tdr)
      ((ar-coerce _sref 'fn)
       _source*
       '(mac
         tdr
         body
         (quasiquote
          (tag
           (td align (quote right . nil) . nil)
           (unquote-splicing (pratoms body . nil) . nil)
           .
           nil)
          .
          nil)
         .
         nil)
       'tdr)
      ((lambda ()
         (if (not (ar-false? ((ar-coerce _bound 'fn) 'tdr)))
           ((lambda ()
              ((ar-coerce _disp 'fn)
               "*** redefining "
               ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) 'tdr ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
           'nil)
         (begin
           (let ((zz
                  ((ar-coerce _annotate 'fn)
                   'mac
                   (let ((| tdr|
                          (lambda body
                            `(tag
                              (td align 'right)
                              ,@(ar-nil-terminate
                                 ((ar-coerce pratoms 'fn) body))))))
                     | tdr|))))
             (namespace-set-variable-value! '_tdr zz)
             zz))))))
   ((lambda ()
      ((ar-coerce _sref 'fn) _sig '(col . body) 'tdcolor)
      ((ar-coerce _sref 'fn) _help* 'nil 'tdcolor)
      ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'tdcolor)
      ((ar-coerce _sref 'fn)
       _source*
       '(mac
         tdcolor
         (col . body)
         (quasiquote
          (tag
           (td bgcolor (unquote col . nil) . nil)
           (unquote-splicing (pratoms body . nil) . nil)
           .
           nil)
          .
          nil)
         .
         nil)
       'tdcolor)
      ((lambda ()
         (if (not (ar-false? ((ar-coerce _bound 'fn) 'tdcolor)))
           ((lambda ()
              ((ar-coerce _disp 'fn)
               "*** redefining "
               ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) 'tdcolor ((ar-coerce _stderr 'fn)))
              ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
           'nil)
         (begin
           (let ((zz
                  ((ar-coerce _annotate 'fn)
                   'mac
                   (let ((| tdcolor|
                          (lambda (col . body)
                            `(tag
                              (td bgcolor ,col)
                              ,@(ar-nil-terminate
                                 ((ar-coerce pratoms 'fn) body))))))
                     | tdcolor|))))
             (namespace-set-variable-value! '_tdcolor zz)
             zz)))))))
 (let ((| pratoms|
        (lambda (body)
          (if (not
               (ar-false?
                ((let ((| pratoms|
                        (lambda (g2698)
                          (if (not (ar-false? g2698))
                            g2698
                            ((let ((| pratoms|
                                    (lambda (g2699)
                                      (if (not (ar-false? g2699))
                                        g2699
                                        'nil))))
                               | pratoms|)
                             ((ar-coerce _all 'fn)
                              (let ((| g2699|
                                     (lambda (_)
                                       (if (not
                                            (ar-false?
                                             ((ar-coerce _acons 'fn) _)))
                                         ((ar-coerce _isnt 'fn)
                                          ((ar-coerce _car 'fn) _)
                                          'quote)
                                         'nil))))
                                | g2699|)
                              body))))))
                   | pratoms|)
                 ((ar-coerce _no 'fn) body))))
            body
            `((pr ,@(ar-nil-terminate body)))))))
   | pratoms|))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'row)
   ((ar-coerce _sref 'fn) _help* 'nil 'row)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'row)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      row
      args
      (quasiquote
       (tr
        (unquote-splicing
         (map (make-br-fn (list (quote td . nil) _ . nil) . nil) args . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'row)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'row)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'row ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| row|
                       (lambda args
                         `(tr
                           ,@(ar-nil-terminate
                              ((ar-coerce _map 'fn)
                               (let ((| row|
                                      (lambda (_)
                                        ((ar-coerce _list 'fn) 'td _))))
                                 | row|)
                               args))))))
                  | row|))))
          (namespace-set-variable-value! '_row zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'prrow)
   ((ar-coerce _sref 'fn) _help* 'nil 'prrow)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'prrow)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      prrow
      args
      (w/uniq
       g
       (quasiquote
        (tr
         (unquote-splicing
          (map
           (fn
            (a . nil)
            (quasiquote
             (let (unquote g . nil)
               (unquote a . nil)
               (if (number (unquote g . nil) . nil)
                 (tdr (pr (unquote g . nil) . nil) . nil)
                 (td (pr (unquote g . nil) . nil) . nil)
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
    'prrow)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'prrow)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'prrow ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| prrow|
                       (lambda args
                         ((let ((| prrow|
                                 (lambda (g)
                                   `(tr
                                     ,@(ar-nil-terminate
                                        ((ar-coerce _map 'fn)
                                         (let ((| prrow|
                                                (lambda (a)
                                                  `(let ,g
                                                     ,a
                                                     (if (number ,g)
                                                       (tdr (pr ,g))
                                                       (td (pr ,g)))))))
                                           | prrow|)
                                         args))))))
                            | prrow|)
                          ((ar-coerce _uniq 'fn))))))
                  | prrow|))))
          (namespace-set-variable-value! '_prrow zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'prbold)
   ((ar-coerce _sref 'fn) _help* 'nil 'prbold)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'prbold)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      prbold
      body
      (quasiquote (tag b (pr (unquote-splicing body . nil) . nil) . nil) . nil)
      .
      nil)
    'prbold)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'prbold)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'prbold ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| prbold|
                       (lambda body `(tag b (pr ,@(ar-nil-terminate body))))))
                  | prbold|))))
          (namespace-set-variable-value! '_prbold zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'para)
   ((ar-coerce _sref 'fn) _help* 'nil 'para)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'para)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      para
      args
      (gentag p . nil)
      (when args (apply pr args . nil) . nil)
      .
      nil)
    'para)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'para)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'para ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| para|
                      (lambda args
                        ((ar-coerce _pr 'fn) "<p>")
                        (if (not (ar-false? args))
                          ((let ((| para|
                                  (lambda ()
                                    ((ar-coerce _apply 'fn) _pr args))))
                             | para|))
                          'nil))))
                 | para|)))
          (namespace-set-variable-value! '_para zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(name items (o sel nil . nil) . nil) 'menu)
   ((ar-coerce _sref 'fn) _help* 'nil 'menu)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'menu)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      menu
      (name items (o sel nil . nil) . nil)
      (tag
       (select name name . nil)
       (each
        i
        items
        (tag (option selected (is i sel . nil) . nil) (pr i . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'menu)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'menu)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'menu ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2700
                 (let* ((name (car g2700))
                        (items (car (ar-xcdr g2700)))
                        (sel
                         (if (pair? (ar-xcdr (ar-xcdr g2700)))
                           (car (ar-xcdr (ar-xcdr g2700)))
                           'nil)))
                   ((let ((| menu|
                           (lambda ()
                             ((let ((| menu|
                                     (lambda ()
                                       ((ar-coerce _pr 'fn) "<select")
                                       ((let ((| menu|
                                               (lambda (it)
                                                 (if (not (ar-false? it))
                                                   ((ar-coerce _pr 'fn)
                                                    " name=\""
                                                    it
                                                    #\")
                                                   'nil))))
                                          | menu|)
                                        name)
                                       ((ar-coerce _pr 'fn) ">"))))
                                | menu|))
                             ((ar-coerce _walk 'fn)
                              items
                              (let ((| menu|
                                     (lambda (i)
                                       ((let ((| menu|
                                               (lambda ()
                                                 ((let ((| menu|
                                                         (lambda ()
                                                           ((ar-coerce _pr 'fn)
                                                            "<option")
                                                           (if (not
                                                                (ar-false?
                                                                 ((ar-coerce
                                                                   _is
                                                                   'fn)
                                                                  i
                                                                  sel)))
                                                             ((ar-coerce
                                                               _pr
                                                               'fn)
                                                              " selected")
                                                             'nil)
                                                           ((ar-coerce _pr 'fn)
                                                            ">"))))
                                                    | menu|))
                                                 ((ar-coerce _pr 'fn) i)
                                                 ((ar-coerce _pr 'fn)
                                                  "</option>"))))
                                          | menu|)))))
                                | menu|))
                             ((ar-coerce _pr 'fn) "</select>"))))
                      | menu|))))))
          (namespace-set-variable-value! '_menu zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'whitepage)
   ((ar-coerce _sref 'fn) _help* 'nil 'whitepage)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'whitepage)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      whitepage
      body
      (quasiquote
       (tag
        html
        (tag
         (body bgcolor white alink linkblue . nil)
         (unquote-splicing body . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'whitepage)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'whitepage)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'whitepage ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| whitepage|
                       (lambda body
                         `(tag
                           html
                           (tag
                            (body bgcolor white alink linkblue)
                            ,@(ar-nil-terminate body))))))
                  | whitepage|))))
          (namespace-set-variable-value! '_whitepage zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'errpage)
   ((ar-coerce _sref 'fn) _help* 'nil 'errpage)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'errpage)
   ((ar-coerce _sref 'fn)
    _source*
    '(def errpage args (whitepage (apply prn args . nil) . nil) . nil)
    'errpage)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'errpage)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'errpage ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| errpage|
                      (lambda args
                        ((let ((| errpage|
                                (lambda ()
                                  ((ar-coerce _pr 'fn) "<html>")
                                  ((let ((| errpage|
                                          (lambda ()
                                            ((let ((| errpage|
                                                    (lambda ()
                                                      ((ar-coerce _pr 'fn)
                                                       "<body")
                                                      ((let ((| errpage|
                                                              (lambda (g2703)
                                                                (if (not
                                                                     (ar-false?
                                                                      g2703))
                                                                  ((let ((| errpage|
                                                                          (lambda (g2701)
                                                                            ((let ((| errpage|
                                                                                    (lambda ()
                                                                                      ((ar-coerce
                                                                                        _pr
                                                                                        'fn)
                                                                                       " bgcolor=#"
                                                                                       ((ar-coerce
                                                                                         _hexrep
                                                                                         'fn)
                                                                                        g2701)))))
                                                                               | errpage|)))))
                                                                     | errpage|)
                                                                   g2703)
                                                                  'nil))))
                                                         | errpage|)
                                                       _white)
                                                      ((let ((| errpage|
                                                              (lambda (g2704)
                                                                (if (not
                                                                     (ar-false?
                                                                      g2704))
                                                                  ((let ((| errpage|
                                                                          (lambda (g2702)
                                                                            ((let ((| errpage|
                                                                                    (lambda ()
                                                                                      ((ar-coerce
                                                                                        _pr
                                                                                        'fn)
                                                                                       " alink=#"
                                                                                       ((ar-coerce
                                                                                         _hexrep
                                                                                         'fn)
                                                                                        g2702)))))
                                                                               | errpage|)))))
                                                                     | errpage|)
                                                                   g2704)
                                                                  'nil))))
                                                         | errpage|)
                                                       _linkblue)
                                                      ((ar-coerce _pr 'fn)
                                                       ">"))))
                                               | errpage|))
                                            ((ar-coerce _apply 'fn) _prn args)
                                            ((ar-coerce _pr 'fn) "</body>"))))
                                     | errpage|))
                                  ((ar-coerce _pr 'fn) "</html>"))))
                           | errpage|)))))
                 | errpage|)))
          (namespace-set-variable-value! '_errpage zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'nil 'blank-url)
   ((ar-coerce _sref 'fn) _help* 'nil 'blank-url)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'blank-url)
   ((ar-coerce _sref 'fn)
    _source*
    '(def blank-url nil "s.gif" . nil)
    'blank-url)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'blank-url)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'blank-url ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz (let ((| blank-url| (lambda () "s.gif"))) | blank-url|)))
          (namespace-set-variable-value! '_blank-url zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(n . nil) 'hspace)
   ((ar-coerce _sref 'fn) _help* 'nil 'hspace)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'hspace)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      hspace
      (n . nil)
      (gentag img src (blank-url . nil) height 1 width n . nil)
      .
      nil)
    'hspace)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'hspace)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'hspace ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| hspace|
                      (lambda (n)
                        ((let ((| hspace|
                                (lambda ()
                                  ((ar-coerce _pr 'fn) "<img")
                                  ((let ((| hspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " src=\""
                                               it
                                               #\")
                                              'nil))))
                                     | hspace|)
                                   ((ar-coerce _blank-url 'fn)))
                                  ((ar-coerce _pr 'fn) " height=1")
                                  ((let ((| hspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " width="
                                               it)
                                              'nil))))
                                     | hspace|)
                                   n)
                                  ((ar-coerce _pr 'fn) ">"))))
                           | hspace|)))))
                 | hspace|)))
          (namespace-set-variable-value! '_hspace zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(n . nil) 'vspace)
   ((ar-coerce _sref 'fn) _help* 'nil 'vspace)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'vspace)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      vspace
      (n . nil)
      (gentag img src (blank-url . nil) height n width 0 . nil)
      .
      nil)
    'vspace)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'vspace)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'vspace ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| vspace|
                      (lambda (n)
                        ((let ((| vspace|
                                (lambda ()
                                  ((ar-coerce _pr 'fn) "<img")
                                  ((let ((| vspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " src=\""
                                               it
                                               #\")
                                              'nil))))
                                     | vspace|)
                                   ((ar-coerce _blank-url 'fn)))
                                  ((let ((| vspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " height="
                                               it)
                                              'nil))))
                                     | vspace|)
                                   n)
                                  ((ar-coerce _pr 'fn) " width=0")
                                  ((ar-coerce _pr 'fn) ">"))))
                           | vspace|)))))
                 | vspace|)))
          (namespace-set-variable-value! '_vspace zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(h w . nil) 'vhspace)
   ((ar-coerce _sref 'fn) _help* 'nil 'vhspace)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'vhspace)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      vhspace
      (h w . nil)
      (gentag img src (blank-url . nil) height h width w . nil)
      .
      nil)
    'vhspace)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'vhspace)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'vhspace ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| vhspace|
                      (lambda (h w)
                        ((let ((| vhspace|
                                (lambda ()
                                  ((ar-coerce _pr 'fn) "<img")
                                  ((let ((| vhspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " src=\""
                                               it
                                               #\")
                                              'nil))))
                                     | vhspace|)
                                   ((ar-coerce _blank-url 'fn)))
                                  ((let ((| vhspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " height="
                                               it)
                                              'nil))))
                                     | vhspace|)
                                   h)
                                  ((let ((| vhspace|
                                          (lambda (it)
                                            (if (not (ar-false? it))
                                              ((ar-coerce _pr 'fn)
                                               " width="
                                               it)
                                              'nil))))
                                     | vhspace|)
                                   w)
                                  ((ar-coerce _pr 'fn) ">"))))
                           | vhspace|)))))
                 | vhspace|)))
          (namespace-set-variable-value! '_vhspace zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(n . nil) 'new-hspace)
   ((ar-coerce _sref 'fn) _help* 'nil 'new-hspace)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'new-hspace)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      new-hspace
      (n . nil)
      (if (number n . nil)
        (quasiquote
         (pr
          (unquote
           (string "<span style=\"padding-left:" n "px\" />" . nil)
           .
           nil)
          .
          nil)
         .
         nil)
        (quasiquote
         (pr "<span style=\"padding-left:" (unquote n . nil) "px\" />" . nil)
         .
         nil)
        .
        nil)
      .
      nil)
    'new-hspace)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'new-hspace)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'new-hspace ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| new-hspace|
                       (lambda (n)
                         (if (not (ar-false? ((ar-coerce _number 'fn) n)))
                           `(pr
                             ,((ar-coerce _string 'fn)
                               "<span style=\"padding-left:"
                               n
                               "px\" />"))
                           `(pr "<span style=\"padding-left:" ,n "px\" />")))))
                  | new-hspace|))))
          (namespace-set-variable-value! '_new-hspace zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(h . nil) 'spacerow)
   ((ar-coerce _sref 'fn) _help* 'nil 'spacerow)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'spacerow)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      spacerow
      (h . nil)
      (pr "<tr style=\"height:" h "px\"></tr>" . nil)
      .
      nil)
    'spacerow)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'spacerow)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'spacerow ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| spacerow|
                      (lambda (h)
                        ((ar-coerce _pr 'fn)
                         "<tr style=\"height:"
                         h
                         "px\"></tr>"))))
                 | spacerow|)))
          (namespace-set-variable-value! '_spacerow zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'zerotable)
   ((ar-coerce _sref 'fn) _help* 'nil 'zerotable)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'zerotable)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      zerotable
      body
      (quasiquote
       (tag
        (table border 0 cellpadding 0 cellspacing 0 . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'zerotable)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'zerotable)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'zerotable ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| zerotable|
                       (lambda body
                         `(tag
                           (table border 0 cellpadding 0 cellspacing 0)
                           ,@(ar-nil-terminate body)))))
                  | zerotable|))))
          (namespace-set-variable-value! '_zerotable zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'sptab)
   ((ar-coerce _sref 'fn) _help* 'nil 'sptab)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'sptab)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      sptab
      body
      (quasiquote
       (tag
        (table style "border-spacing: 7px 0px;" . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'sptab)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'sptab)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'sptab ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| sptab|
                       (lambda body
                         `(tag
                           (table style "border-spacing: 7px 0px;")
                           ,@(ar-nil-terminate body)))))
                  | sptab|))))
          (namespace-set-variable-value! '_sptab zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(w . body) 'widtable)
   ((ar-coerce _sref 'fn) _help* 'nil 'widtable)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'widtable)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      widtable
      (w . body)
      (quasiquote
       (tag
        (table width (unquote w . nil) . nil)
        (tr (td (unquote-splicing body . nil) . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'widtable)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'widtable)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'widtable ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| widtable|
                       (lambda (w . body)
                         `(tag
                           (table width ,w)
                           (tr (td ,@(ar-nil-terminate body)))))))
                  | widtable|))))
          (namespace-set-variable-value! '_widtable zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(x . nil) 'cellpr)
   ((ar-coerce _sref 'fn) _help* 'nil 'cellpr)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'cellpr)
   ((ar-coerce _sref 'fn)
    _source*
    '(def cellpr (x . nil) (pr (or x "&nbsp;" . nil) . nil) . nil)
    'cellpr)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'cellpr)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'cellpr ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| cellpr|
                      (lambda (x)
                        ((ar-coerce _pr 'fn)
                         ((let ((| cellpr|
                                 (lambda (g2705)
                                   (if (not (ar-false? g2705))
                                     g2705
                                     ((let ((| cellpr|
                                             (lambda (g2706)
                                               (if (not (ar-false? g2706))
                                                 g2706
                                                 'nil))))
                                        | cellpr|)
                                      "&nbsp;")))))
                            | cellpr|)
                          x)))))
                 | cellpr|)))
          (namespace-set-variable-value! '_cellpr zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn)
    _sig
    '((o text "submit" . nil) (o name nil . nil) . nil)
    'but)
   ((ar-coerce _sref 'fn) _help* 'nil 'but)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'but)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      but
      ((o text "submit" . nil) (o name nil . nil) . nil)
      (gentag input type (quote submit . nil) name name value text . nil)
      .
      nil)
    'but)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'but)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'but ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2707
                 (let* ((text (if (pair? g2707) (car g2707) "submit"))
                        (name
                         (if (pair? (ar-xcdr g2707))
                           (car (ar-xcdr g2707))
                           'nil)))
                   ((let ((| but|
                           (lambda ()
                             ((ar-coerce _pr 'fn) "<input")
                             ((ar-coerce _pr 'fn) " type=submit")
                             ((let ((| but|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((ar-coerce _pr 'fn)
                                          " name=\""
                                          it
                                          #\")
                                         'nil))))
                                | but|)
                              name)
                             ((let ((| but|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((let ((| but|
                                                 (lambda ()
                                                   ((ar-coerce _pr 'fn)
                                                    " value=\"")
                                                   (if (not
                                                        (ar-false?
                                                         ((ar-coerce _isa 'fn)
                                                          it
                                                          'string)))
                                                     ((ar-coerce
                                                       _pr-escaped
                                                       'fn)
                                                      it)
                                                     ((ar-coerce _pr 'fn) it))
                                                   ((ar-coerce _pr 'fn) #\"))))
                                            | but|))
                                         'nil))))
                                | but|)
                              text)
                             ((ar-coerce _pr 'fn) ">"))))
                      | but|))))))
          (namespace-set-variable-value! '_but zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '((o val "submit" . nil) . nil) 'submit)
   ((ar-coerce _sref 'fn) _help* 'nil 'submit)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'submit)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      submit
      ((o val "submit" . nil) . nil)
      (gentag input type (quote submit . nil) value val . nil)
      .
      nil)
    'submit)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'submit)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'submit ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2708
                 (let* ((val (if (pair? g2708) (car g2708) "submit")))
                   ((let ((| submit|
                           (lambda ()
                             ((ar-coerce _pr 'fn) "<input")
                             ((ar-coerce _pr 'fn) " type=submit")
                             ((let ((| submit|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((let ((| submit|
                                                 (lambda ()
                                                   ((ar-coerce _pr 'fn)
                                                    " value=\"")
                                                   (if (not
                                                        (ar-false?
                                                         ((ar-coerce _isa 'fn)
                                                          it
                                                          'string)))
                                                     ((ar-coerce
                                                       _pr-escaped
                                                       'fn)
                                                      it)
                                                     ((ar-coerce _pr 'fn) it))
                                                   ((ar-coerce _pr 'fn) #\"))))
                                            | submit|))
                                         'nil))))
                                | submit|)
                              val)
                             ((ar-coerce _pr 'fn) ">"))))
                      | submit|))))))
          (namespace-set-variable-value! '_submit zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(name . texts) 'buts)
   ((ar-coerce _sref 'fn) _help* 'nil 'buts)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'buts)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      buts
      (name . texts)
      (if (no texts . nil)
        (but . nil)
        (do (but (car texts . nil) name . nil)
            (each
             text
             (cdr texts . nil)
             (pr " " . nil)
             (but text name . nil)
             .
             nil)
          .
          nil)
        .
        nil)
      .
      nil)
    'buts)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'buts)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'buts ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| buts|
                      (lambda (name . texts)
                        (if (not (ar-false? ((ar-coerce _no 'fn) texts)))
                          ((ar-coerce _but 'fn))
                          ((let ((| buts|
                                  (lambda ()
                                    ((ar-coerce _but 'fn)
                                     ((ar-coerce _car 'fn) texts)
                                     name)
                                    ((ar-coerce _walk 'fn)
                                     ((ar-coerce _cdr 'fn) texts)
                                     (let ((| buts|
                                            (lambda (text)
                                              ((ar-coerce _pr 'fn) " ")
                                              ((ar-coerce _but 'fn)
                                               text
                                               name))))
                                       | buts|)))))
                             | buts|))))))
                 | buts|)))
          (namespace-set-variable-value! '_buts zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(n . body) 'spanrow)
   ((ar-coerce _sref 'fn) _help* 'nil 'spanrow)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'spanrow)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      spanrow
      (n . body)
      (quasiquote
       (tr
        (tag
         (td colspan (unquote n . nil) . nil)
         (unquote-splicing body . nil)
         .
         nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'spanrow)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'spanrow)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'spanrow ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| spanrow|
                       (lambda (n . body)
                         `(tr
                           (tag (td colspan ,n) ,@(ar-nil-terminate body))))))
                  | spanrow|))))
          (namespace-set-variable-value! '_spanrow zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(action . body) 'form)
   ((ar-coerce _sref 'fn) _help* 'nil 'form)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'form)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      form
      (action . body)
      (quasiquote
       (tag
        (form method "post" action (unquote action . nil) . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'form)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'form)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'form ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| form|
                       (lambda (action . body)
                         `(tag
                           (form method "post" action ,action)
                           ,@(ar-nil-terminate body)))))
                  | form|))))
          (namespace-set-variable-value! '_form zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(name rows cols . body) 'textarea)
   ((ar-coerce _sref 'fn) _help* 'nil 'textarea)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'textarea)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      textarea
      (name rows cols . body)
      (quasiquote
       (tag
        (textarea
         name
         (unquote name . nil)
         rows
         (unquote rows . nil)
         cols
         (unquote cols . nil)
         .
         nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'textarea)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'textarea)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'textarea ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| textarea|
                       (lambda (name rows cols . body)
                         `(tag
                           (textarea name ,name rows ,rows cols ,cols)
                           ,@(ar-nil-terminate body)))))
                  | textarea|))))
          (namespace-set-variable-value! '_textarea zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn)
    _sig
    '(name (o val "" . nil) (o size 10 . nil) . nil)
    'input)
   ((ar-coerce _sref 'fn) _help* 'nil 'input)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'input)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      input
      (name (o val "" . nil) (o size 10 . nil) . nil)
      (gentag
       input
       type
       (quote text . nil)
       name
       name
       value
       val
       size
       size
       .
       nil)
      .
      nil)
    'input)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'input)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'input ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2709
                 (let* ((name (car g2709))
                        (val
                         (if (pair? (ar-xcdr g2709)) (car (ar-xcdr g2709)) ""))
                        (size
                         (if (pair? (ar-xcdr (ar-xcdr g2709)))
                           (car (ar-xcdr (ar-xcdr g2709)))
                           10)))
                   ((let ((| input|
                           (lambda ()
                             ((ar-coerce _pr 'fn) "<input")
                             ((ar-coerce _pr 'fn) " type=text")
                             ((let ((| input|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((ar-coerce _pr 'fn)
                                          " name=\""
                                          it
                                          #\")
                                         'nil))))
                                | input|)
                              name)
                             ((let ((| input|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((let ((| input|
                                                 (lambda ()
                                                   ((ar-coerce _pr 'fn)
                                                    " value=\"")
                                                   (if (not
                                                        (ar-false?
                                                         ((ar-coerce _isa 'fn)
                                                          it
                                                          'string)))
                                                     ((ar-coerce
                                                       _pr-escaped
                                                       'fn)
                                                      it)
                                                     ((ar-coerce _pr 'fn) it))
                                                   ((ar-coerce _pr 'fn) #\"))))
                                            | input|))
                                         'nil))))
                                | input|)
                              val)
                             ((let ((| input|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((ar-coerce _pr 'fn) " size=" it)
                                         'nil))))
                                | input|)
                              size)
                             ((ar-coerce _pr 'fn) ">"))))
                      | input|))))))
          (namespace-set-variable-value! '_input zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'args 'inputs)
   ((ar-coerce _sref 'fn) _help* 'nil 'inputs)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'inputs)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      inputs
      args
      (quasiquote
       (tag
        (table border 0 . nil)
        (unquote-splicing
         (map
          (fn
           ((name label len text . nil) . nil)
           (w/uniq
            (gl gt . nil)
            (quasiquote
             (let (unquote gl . nil)
               (unquote len . nil)
               (tr
                (td (pr (quote (unquote label . nil) . nil) ":" . nil) . nil)
                (if (isa (unquote gl . nil) (quote cons . nil) . nil)
                  (td
                   (textarea
                    (quote (unquote name . nil) . nil)
                    (car (unquote gl . nil) . nil)
                    (cadr (unquote gl . nil) . nil)
                    (let (unquote gt . nil)
                      (unquote text . nil)
                      (if (unquote gt . nil)
                        (pr (unquote gt . nil) . nil)
                        .
                        nil)
                      .
                      nil)
                    .
                    nil)
                   .
                   nil)
                  (td
                   (gentag
                    input
                    type
                    (quote
                     (unquote
                      (if (is label (quote password . nil) . nil)
                        (quote password . nil)
                        (quote text . nil)
                        .
                        nil)
                      .
                      nil)
                     .
                     nil)
                    name
                    (quote (unquote name . nil) . nil)
                    size
                    (unquote len . nil)
                    value
                    (unquote text . nil)
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
          (tuples args 4 . nil)
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
    'inputs)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'inputs)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'inputs ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| inputs|
                       (lambda args
                         `(tag
                           (table border 0)
                           ,@(ar-nil-terminate
                              ((ar-coerce _map 'fn)
                               (lambda g2710
                                 (let* ((name (ar-xcar (car g2710)))
                                        (label (ar-xcar (ar-xcdr (car g2710))))
                                        (len
                                         (ar-xcar
                                          (ar-xcdr (ar-xcdr (car g2710)))))
                                        (text
                                         (ar-xcar
                                          (ar-xcdr
                                           (ar-xcdr (ar-xcdr (car g2710)))))))
                                   ((let ((| inputs|
                                           (lambda (gl gt)
                                             `(let ,gl
                                                ,len
                                                (tr
                                                 (td (pr ',label ":"))
                                                 (if (isa ,gl 'cons)
                                                   (td
                                                    (textarea
                                                     ',name
                                                     (car ,gl)
                                                     (cadr ,gl)
                                                     (let ,gt
                                                       ,text
                                                       (if ,gt (pr ,gt)))))
                                                   (td
                                                    (gentag
                                                     input
                                                     type
                                                     ',(if (not
                                                            (ar-false?
                                                             ((ar-coerce
                                                               _is
                                                               'fn)
                                                              label
                                                              'password)))
                                                         'password
                                                         'text)
                                                     name
                                                     ',name
                                                     size
                                                     ,len
                                                     value
                                                     ,text))))))))
                                      | inputs|)
                                    ((ar-coerce _uniq 'fn))
                                    ((ar-coerce _uniq 'fn)))))
                               ((ar-coerce _tuples 'fn) args 4)))))))
                  | inputs|))))
          (namespace-set-variable-value! '_inputs zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn)
    _sig
    '(label name chars btext (o pwd . nil) . nil)
    'single-input)
   ((ar-coerce _sref 'fn) _help* 'nil 'single-input)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'single-input)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      single-input
      (label name chars btext (o pwd . nil) . nil)
      (pr label . nil)
      (gentag
       input
       type
       (if pwd (quote password . nil) (quote text . nil) . nil)
       name
       name
       size
       chars
       .
       nil)
      (sp . nil)
      (submit btext . nil)
      .
      nil)
    'single-input)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'single-input)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'single-input ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2711
                 (let* ((label (car g2711))
                        (name (car (ar-xcdr g2711)))
                        (chars (car (ar-xcdr (ar-xcdr g2711))))
                        (btext (car (ar-xcdr (ar-xcdr (ar-xcdr g2711)))))
                        (pwd
                         (if (pair?
                              (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2711)))))
                           (car (ar-xcdr (ar-xcdr (ar-xcdr (ar-xcdr g2711)))))
                           'nil)))
                   ((ar-coerce _pr 'fn) label)
                   ((let ((| single-input|
                           (lambda ()
                             ((ar-coerce _pr 'fn) "<input")
                             ((ar-coerce _pr 'fn)
                              " type="
                              (if (not (ar-false? pwd)) 'password 'text))
                             ((let ((| single-input|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((ar-coerce _pr 'fn)
                                          " name=\""
                                          it
                                          #\")
                                         'nil))))
                                | single-input|)
                              name)
                             ((let ((| single-input|
                                     (lambda (it)
                                       (if (not (ar-false? it))
                                         ((ar-coerce _pr 'fn) " size=" it)
                                         'nil))))
                                | single-input|)
                              chars)
                             ((ar-coerce _pr 'fn) ">"))))
                      | single-input|))
                   ((ar-coerce _sp 'fn))
                   ((ar-coerce _submit 'fn) btext)))))
          (namespace-set-variable-value! '_single-input zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'body 'cdata)
   ((ar-coerce _sref 'fn) _help* 'nil 'cdata)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'cdata)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      cdata
      body
      (quasiquote
       (do (pr "<![CDATA[" . nil)
           (unquote-splicing body . nil)
         (pr "]]>" . nil)
         .
         nil)
       .
       nil)
      .
      nil)
    'cdata)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'cdata)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'cdata ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| cdata|
                       (lambda body
                         `(do (pr "<![CDATA[")
                              ,@(ar-nil-terminate body)
                            (pr "]]>")))))
                  | cdata|))))
          (namespace-set-variable-value! '_cdata zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(str . nil) 'eschtml)
   ((ar-coerce _sref 'fn) _help* 'nil 'eschtml)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'eschtml)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      eschtml
      (str . nil)
      (tostring
       (each
        c
        str
        (pr
         (case c
           #\<
           "&#60;"
           #\>
           "&#62;"
           #\"
           "&#34;"
           #\'
           "&#39;"
           #\&
           "&#38;"
           c
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
    'eschtml)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'eschtml)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'eschtml ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| eschtml|
                      (lambda (str)
                        ((let ((| eschtml|
                                (lambda (g2712)
                                  ((ar-coerce _call-w/stdout 'fn)
                                   g2712
                                   (let ((| eschtml|
                                          (lambda ()
                                            ((ar-coerce _walk 'fn)
                                             str
                                             (let ((| eschtml|
                                                    (lambda (c)
                                                      ((ar-coerce _pr 'fn)
                                                       ((let ((| eschtml|
                                                               (lambda (g2713)
                                                                 (if (not
                                                                      (ar-false?
                                                                       ((ar-coerce
                                                                         _is
                                                                         'fn)
                                                                        g2713
                                                                        '#\<)))
                                                                   "&#60;"
                                                                   (if (not
                                                                        (ar-false?
                                                                         ((ar-coerce
                                                                           _is
                                                                           'fn)
                                                                          g2713
                                                                          '#\>)))
                                                                     "&#62;"
                                                                     (if (not
                                                                          (ar-false?
                                                                           ((ar-coerce
                                                                             _is
                                                                             'fn)
                                                                            g2713
                                                                            '#\")))
                                                                       "&#34;"
                                                                       (if (not
                                                                            (ar-false?
                                                                             ((ar-coerce
                                                                               _is
                                                                               'fn)
                                                                              g2713
                                                                              '#\')))
                                                                         "&#39;"
                                                                         (if (not
                                                                              (ar-false?
                                                                               ((ar-coerce
                                                                                 _is
                                                                                 'fn)
                                                                                g2713
                                                                                '#\&)))
                                                                           "&#38;"
                                                                           c))))))))
                                                          | eschtml|)
                                                        c)))))
                                               | eschtml|)))))
                                     | eschtml|))
                                  ((ar-coerce _inside 'fn) g2712))))
                           | eschtml|)
                         ((ar-coerce _outstring 'fn))))))
                 | eschtml|)))
          (namespace-set-variable-value! '_eschtml zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(str . nil) 'esc-tags)
   ((ar-coerce _sref 'fn) _help* 'nil 'esc-tags)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'esc-tags)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      esc-tags
      (str . nil)
      (tostring
       (each
        c
        str
        (pr (case c #\< "&#60;" #\> "&#62;" #\& "&#38;" c . nil) . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'esc-tags)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'esc-tags)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'esc-tags ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| esc-tags|
                      (lambda (str)
                        ((let ((| esc-tags|
                                (lambda (g2714)
                                  ((ar-coerce _call-w/stdout 'fn)
                                   g2714
                                   (let ((| esc-tags|
                                          (lambda ()
                                            ((ar-coerce _walk 'fn)
                                             str
                                             (let ((| esc-tags|
                                                    (lambda (c)
                                                      ((ar-coerce _pr 'fn)
                                                       ((let ((| esc-tags|
                                                               (lambda (g2715)
                                                                 (if (not
                                                                      (ar-false?
                                                                       ((ar-coerce
                                                                         _is
                                                                         'fn)
                                                                        g2715
                                                                        '#\<)))
                                                                   "&#60;"
                                                                   (if (not
                                                                        (ar-false?
                                                                         ((ar-coerce
                                                                           _is
                                                                           'fn)
                                                                          g2715
                                                                          '#\>)))
                                                                     "&#62;"
                                                                     (if (not
                                                                          (ar-false?
                                                                           ((ar-coerce
                                                                             _is
                                                                             'fn)
                                                                            g2715
                                                                            '#\&)))
                                                                       "&#38;"
                                                                       c))))))
                                                          | esc-tags|)
                                                        c)))))
                                               | esc-tags|)))))
                                     | esc-tags|))
                                  ((ar-coerce _inside 'fn) g2714))))
                           | esc-tags|)
                         ((ar-coerce _outstring 'fn))))))
                 | esc-tags|)))
          (namespace-set-variable-value! '_esc-tags zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig 'nil 'nbsp)
   ((ar-coerce _sref 'fn) _help* 'nil 'nbsp)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'nbsp)
   ((ar-coerce _sref 'fn)
    _source*
    '(def nbsp nil (pr "&nbsp;" . nil) . nil)
    'nbsp)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'nbsp)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'nbsp ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| nbsp| (lambda () ((ar-coerce _pr 'fn) "&nbsp;"))))
                 | nbsp|)))
          (namespace-set-variable-value! '_nbsp zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn)
    _sig
    '(text (o dest text . nil) (o color . nil) . nil)
    'link)
   ((ar-coerce _sref 'fn) _help* 'nil 'link)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'link)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      link
      (text (o dest text . nil) (o color . nil) . nil)
      (tag
       (a href dest . nil)
       (tag-if color (font color color . nil) (pr text . nil) . nil)
       .
       nil)
      .
      nil)
    'link)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'link)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'link ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2716
                 (let* ((text (car g2716))
                        (dest
                         (if (pair? (ar-xcdr g2716))
                           (car (ar-xcdr g2716))
                           text))
                        (color
                         (if (pair? (ar-xcdr (ar-xcdr g2716)))
                           (car (ar-xcdr (ar-xcdr g2716)))
                           'nil)))
                   ((let ((| link|
                           (lambda ()
                             ((let ((| link|
                                     (lambda ()
                                       ((ar-coerce _pr 'fn) "<a")
                                       ((let ((| link|
                                               (lambda (it)
                                                 (if (not (ar-false? it))
                                                   ((ar-coerce _pr 'fn)
                                                    " href=\""
                                                    it
                                                    #\")
                                                   'nil))))
                                          | link|)
                                        dest)
                                       ((ar-coerce _pr 'fn) ">"))))
                                | link|))
                             (if (not (ar-false? color))
                               ((let ((| link|
                                       (lambda ()
                                         ((let ((| link|
                                                 (lambda ()
                                                   ((ar-coerce _pr 'fn)
                                                    "<font")
                                                   ((let ((| link|
                                                           (lambda (g2718)
                                                             (if (not
                                                                  (ar-false?
                                                                   g2718))
                                                               ((let ((| link|
                                                                       (lambda (g2717)
                                                                         ((let ((| link|
                                                                                 (lambda ()
                                                                                   ((ar-coerce
                                                                                     _pr
                                                                                     'fn)
                                                                                    " color=#"
                                                                                    ((ar-coerce
                                                                                      _hexrep
                                                                                      'fn)
                                                                                     g2717)))))
                                                                            | link|)))))
                                                                  | link|)
                                                                g2718)
                                                               'nil))))
                                                      | link|)
                                                    color)
                                                   ((ar-coerce _pr 'fn) ">"))))
                                            | link|))
                                         ((ar-coerce _pr 'fn) text)
                                         ((ar-coerce _pr 'fn) "</font>"))))
                                  | link|))
                               ((let ((| link|
                                       (lambda () ((ar-coerce _pr 'fn) text))))
                                  | link|)))
                             ((ar-coerce _pr 'fn) "</a>"))))
                      | link|))))))
          (namespace-set-variable-value! '_link zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(text (o dest text . nil) . nil) 'underlink)
   ((ar-coerce _sref 'fn) _help* 'nil 'underlink)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'underlink)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      underlink
      (text (o dest text . nil) . nil)
      (tag (a href dest . nil) (tag u (pr text . nil) . nil) . nil)
      .
      nil)
    'underlink)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'underlink)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'underlink ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (lambda g2719
                 (let* ((text (car g2719))
                        (dest
                         (if (pair? (ar-xcdr g2719))
                           (car (ar-xcdr g2719))
                           text)))
                   ((let ((| underlink|
                           (lambda ()
                             ((let ((| underlink|
                                     (lambda ()
                                       ((ar-coerce _pr 'fn) "<a")
                                       ((let ((| underlink|
                                               (lambda (it)
                                                 (if (not (ar-false? it))
                                                   ((ar-coerce _pr 'fn)
                                                    " href=\""
                                                    it
                                                    #\")
                                                   'nil))))
                                          | underlink|)
                                        dest)
                                       ((ar-coerce _pr 'fn) ">"))))
                                | underlink|))
                             ((let ((| underlink|
                                     (lambda ()
                                       ((ar-coerce _pr 'fn) "<u>")
                                       ((ar-coerce _pr 'fn) text)
                                       ((ar-coerce _pr 'fn) "</u>"))))
                                | underlink|))
                             ((ar-coerce _pr 'fn) "</a>"))))
                      | underlink|))))))
          (namespace-set-variable-value! '_underlink zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(s . nil) 'striptags)
   ((ar-coerce _sref 'fn) _help* 'nil 'striptags)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'striptags)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      striptags
      (s . nil)
      (let intag nil
        (tostring
         (each
          c
          s
          (if (is c #\< . nil)
            (set intag . nil)
            (is c #\> . nil)
            (wipe intag . nil)
            (no intag . nil)
            (pr c . nil)
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
    'striptags)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'striptags)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'striptags ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| striptags|
                      (lambda (s)
                        ((let ((| striptags|
                                (lambda (intag)
                                  ((let ((| striptags|
                                          (lambda (g2720)
                                            ((ar-coerce _call-w/stdout 'fn)
                                             g2720
                                             (let ((| striptags|
                                                    (lambda ()
                                                      ((ar-coerce _walk 'fn)
                                                       s
                                                       (let ((| striptags|
                                                              (lambda (c)
                                                                (if (not
                                                                     (ar-false?
                                                                      ((ar-coerce
                                                                        _is
                                                                        'fn)
                                                                       c
                                                                       #\<)))
                                                                  ((let ((| striptags|
                                                                          (lambda ()
                                                                            ((let ((| striptags|
                                                                                    (lambda ()
                                                                                      (begin
                                                                                        (let ((zz
                                                                                               _t))
                                                                                          (set! intag
                                                                                            zz)
                                                                                          zz)))))
                                                                               | striptags|)))))
                                                                     | striptags|))
                                                                  (if (not
                                                                       (ar-false?
                                                                        ((ar-coerce
                                                                          _is
                                                                          'fn)
                                                                         c
                                                                         #\>)))
                                                                    ((let ((| striptags|
                                                                            (lambda ()
                                                                              ((let ((| striptags|
                                                                                      (lambda ()
                                                                                        (begin
                                                                                          (let ((zz
                                                                                                 'nil))
                                                                                            (set! intag
                                                                                              zz)
                                                                                            zz)))))
                                                                                 | striptags|)))))
                                                                       | striptags|))
                                                                    (if (not
                                                                         (ar-false?
                                                                          ((ar-coerce
                                                                            _no
                                                                            'fn)
                                                                           intag)))
                                                                      ((ar-coerce
                                                                        _pr
                                                                        'fn)
                                                                       c)
                                                                      'nil))))))
                                                         | striptags|)))))
                                               | striptags|))
                                            ((ar-coerce _inside 'fn) g2720))))
                                     | striptags|)
                                   ((ar-coerce _outstring 'fn))))))
                           | striptags|)
                         'nil))))
                 | striptags|)))
          (namespace-set-variable-value! '_striptags zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(u . nil) 'clean-url)
   ((ar-coerce _sref 'fn) _help* 'nil 'clean-url)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'clean-url)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      clean-url
      (u . nil)
      (rem (make-br-fn (in _ #\" #\' #\< #\> . nil) . nil) u . nil)
      .
      nil)
    'clean-url)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'clean-url)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'clean-url ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| clean-url|
                      (lambda (u)
                        ((ar-coerce _rem 'fn)
                         (let ((| clean-url|
                                (lambda (_)
                                  ((let ((| clean-url|
                                          (lambda (g2721)
                                            ((let ((| clean-url|
                                                    (lambda (g2722)
                                                      (if (not
                                                           (ar-false? g2722))
                                                        g2722
                                                        ((let ((| clean-url|
                                                                (lambda (g2723)
                                                                  (if (not
                                                                       (ar-false?
                                                                        g2723))
                                                                    g2723
                                                                    ((let ((| clean-url|
                                                                            (lambda (g2724)
                                                                              (if (not
                                                                                   (ar-false?
                                                                                    g2724))
                                                                                g2724
                                                                                ((let ((| clean-url|
                                                                                        (lambda (g2725)
                                                                                          (if (not
                                                                                               (ar-false?
                                                                                                g2725))
                                                                                            g2725
                                                                                            'nil))))
                                                                                   | clean-url|)
                                                                                 ((ar-coerce
                                                                                   _is
                                                                                   'fn)
                                                                                  g2721
                                                                                  #\>))))))
                                                                       | clean-url|)
                                                                     ((ar-coerce
                                                                       _is
                                                                       'fn)
                                                                      g2721
                                                                      #\<))))))
                                                           | clean-url|)
                                                         ((ar-coerce _is 'fn)
                                                          g2721
                                                          #\'))))))
                                               | clean-url|)
                                             ((ar-coerce _is 'fn)
                                              g2721
                                              #\")))))
                                     | clean-url|)
                                   _))))
                           | clean-url|)
                         u))))
                 | clean-url|)))
          (namespace-set-variable-value! '_clean-url zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(url . nil) 'shortlink)
   ((ar-coerce _sref 'fn) _help* 'nil 'shortlink)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'shortlink)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      shortlink
      (url . nil)
      (unless (or (no url . nil) (< (len url . nil) 7 . nil) . nil)
        (link (cut url 7 . nil) url . nil)
        .
        nil)
      .
      nil)
    'shortlink)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'shortlink)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'shortlink ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| shortlink|
                      (lambda (url)
                        (if (not
                             (ar-false?
                              ((ar-coerce _no 'fn)
                               ((let ((| shortlink|
                                       (lambda (g2726)
                                         (if (not (ar-false? g2726))
                                           g2726
                                           ((let ((| shortlink|
                                                   (lambda (g2727)
                                                     (if (not
                                                          (ar-false? g2727))
                                                       g2727
                                                       'nil))))
                                              | shortlink|)
                                            ((ar-coerce _< 'fn)
                                             ((ar-coerce _len 'fn) url)
                                             7))))))
                                  | shortlink|)
                                ((ar-coerce _no 'fn) url)))))
                          ((let ((| shortlink|
                                  (lambda ()
                                    ((ar-coerce _link 'fn)
                                     ((ar-coerce _cut 'fn) url 7)
                                     url))))
                             | shortlink|))
                          'nil))))
                 | shortlink|)))
          (namespace-set-variable-value! '_shortlink zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(str . nil) 'parafy)
   ((ar-coerce _sref 'fn) _help* 'nil 'parafy)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'parafy)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      parafy
      (str . nil)
      (let ink nil
        (tostring
         (each
          c
          str
          (pr c . nil)
          (unless (whitec c . nil) (set ink . nil) . nil)
          (when (is c #\newline . nil)
            (unless ink (pr "<p>" . nil) . nil)
            (wipe ink . nil)
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
    'parafy)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'parafy)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'parafy ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| parafy|
                      (lambda (str)
                        ((let ((| parafy|
                                (lambda (ink)
                                  ((let ((| parafy|
                                          (lambda (g2728)
                                            ((ar-coerce _call-w/stdout 'fn)
                                             g2728
                                             (let ((| parafy|
                                                    (lambda ()
                                                      ((ar-coerce _walk 'fn)
                                                       str
                                                       (let ((| parafy|
                                                              (lambda (c)
                                                                ((ar-coerce
                                                                  _pr
                                                                  'fn)
                                                                 c)
                                                                (if (not
                                                                     (ar-false?
                                                                      ((ar-coerce
                                                                        _no
                                                                        'fn)
                                                                       ((ar-coerce
                                                                         _whitec
                                                                         'fn)
                                                                        c))))
                                                                  ((let ((| parafy|
                                                                          (lambda ()
                                                                            ((let ((| parafy|
                                                                                    (lambda ()
                                                                                      ((let ((| parafy|
                                                                                              (lambda ()
                                                                                                (begin
                                                                                                  (let ((zz
                                                                                                         _t))
                                                                                                    (set! ink
                                                                                                      zz)
                                                                                                    zz)))))
                                                                                         | parafy|)))))
                                                                               | parafy|)))))
                                                                     | parafy|))
                                                                  'nil)
                                                                (if (not
                                                                     (ar-false?
                                                                      ((ar-coerce
                                                                        _is
                                                                        'fn)
                                                                       c
                                                                       #\newline)))
                                                                  ((let ((| parafy|
                                                                          (lambda ()
                                                                            (if (not
                                                                                 (ar-false?
                                                                                  ((ar-coerce
                                                                                    _no
                                                                                    'fn)
                                                                                   ink)))
                                                                              ((let ((| parafy|
                                                                                      (lambda ()
                                                                                        ((ar-coerce
                                                                                          _pr
                                                                                          'fn)
                                                                                         "<p>"))))
                                                                                 | parafy|))
                                                                              'nil)
                                                                            ((let ((| parafy|
                                                                                    (lambda ()
                                                                                      ((let ((| parafy|
                                                                                              (lambda ()
                                                                                                (begin
                                                                                                  (let ((zz
                                                                                                         'nil))
                                                                                                    (set! ink
                                                                                                      zz)
                                                                                                    zz)))))
                                                                                         | parafy|)))))
                                                                               | parafy|)))))
                                                                     | parafy|))
                                                                  'nil))))
                                                         | parafy|)))))
                                               | parafy|))
                                            ((ar-coerce _inside 'fn) g2728))))
                                     | parafy|)
                                   ((ar-coerce _outstring 'fn))))))
                           | parafy|)
                         'nil))))
                 | parafy|)))
          (namespace-set-variable-value! '_parafy zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(name . body) 'spanclass)
   ((ar-coerce _sref 'fn) _help* 'nil 'spanclass)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'spanclass)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      spanclass
      (name . body)
      (quasiquote
       (tag
        (span class (quote (unquote name . nil) . nil) . nil)
        (unquote-splicing body . nil)
        .
        nil)
       .
       nil)
      .
      nil)
    'spanclass)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'spanclass)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'spanclass ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| spanclass|
                       (lambda (name . body)
                         `(tag
                           (span class ',name)
                           ,@(ar-nil-terminate body)))))
                  | spanclass|))))
          (namespace-set-variable-value! '_spanclass zz)
          zz))))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(text . nil) 'pagemessage)
   ((ar-coerce _sref 'fn) _help* 'nil 'pagemessage)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'pagemessage)
   ((ar-coerce _sref 'fn)
    _source*
    '(def
      pagemessage
      (text . nil)
      (when text (prn text . nil) (br2 . nil) . nil)
      .
      nil)
    'pagemessage)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'pagemessage)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'pagemessage ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               (let ((| pagemessage|
                      (lambda (text)
                        (if (not (ar-false? text))
                          ((let ((| pagemessage|
                                  (lambda ()
                                    ((ar-coerce _prn 'fn) text)
                                    ((ar-coerce _br2 'fn)))))
                             | pagemessage|))
                          'nil))))
                 | pagemessage|)))
          (namespace-set-variable-value! '_pagemessage zz)
          zz))))))


((lambda ()
   (if (not (ar-false? ((ar-coerce _bound 'fn) 'valid-url)))
     ((lambda ()
        ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) 'valid-url ((ar-coerce _stderr 'fn)))
        ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
     'nil)
   (begin
     (let ((zz
            ((ar-coerce _memo 'fn)
             (let ((| valid-url|
                    (lambda (url)
                      (if (not (ar-false? ((ar-coerce _len> 'fn) url 10)))
                        (if (not
                             (ar-false?
                              ((let ((| valid-url|
                                      (lambda (g2729)
                                        (if (not (ar-false? g2729))
                                          g2729
                                          ((let ((| valid-url|
                                                  (lambda (g2730)
                                                    (if (not (ar-false? g2730))
                                                      g2730
                                                      'nil))))
                                             | valid-url|)
                                           ((ar-coerce _begins 'fn)
                                            url
                                            "https://"))))))
                                 | valid-url|)
                               ((ar-coerce _begins 'fn) url "http://"))))
                          ((ar-coerce _no 'fn)
                           ((ar-coerce _find 'fn)
                            (let ((| valid-url|
                                   (lambda (_)
                                     ((let ((| valid-url|
                                             (lambda (g2731)
                                               ((let ((| valid-url|
                                                       (lambda (g2732)
                                                         (if (not
                                                              (ar-false?
                                                               g2732))
                                                           g2732
                                                           ((let ((| valid-url|
                                                                   (lambda (g2733)
                                                                     (if (not
                                                                          (ar-false?
                                                                           g2733))
                                                                       g2733
                                                                       ((let ((| valid-url|
                                                                               (lambda (g2734)
                                                                                 (if (not
                                                                                      (ar-false?
                                                                                       g2734))
                                                                                   g2734
                                                                                   ((let ((| valid-url|
                                                                                           (lambda (g2735)
                                                                                             (if (not
                                                                                                  (ar-false?
                                                                                                   g2735))
                                                                                               g2735
                                                                                               'nil))))
                                                                                      | valid-url|)
                                                                                    ((ar-coerce
                                                                                      _is
                                                                                      'fn)
                                                                                     g2731
                                                                                     #\'))))))
                                                                          | valid-url|)
                                                                        ((ar-coerce
                                                                          _is
                                                                          'fn)
                                                                         g2731
                                                                         #\"))))))
                                                              | valid-url|)
                                                            ((ar-coerce
                                                              _is
                                                              'fn)
                                                             g2731
                                                             #\>))))))
                                                  | valid-url|)
                                                ((ar-coerce _is 'fn)
                                                 g2731
                                                 #\<)))))
                                        | valid-url|)
                                      _))))
                              | valid-url|)
                            url))
                          'nil)
                        'nil))))
               | valid-url|))))
       (namespace-set-variable-value! '_valid-url zz)
       zz))))


((lambda ()
   ((ar-coerce _sref 'fn) _sig '(c . body) 'fontcolor)
   ((ar-coerce _sref 'fn) _help* 'nil 'fontcolor)
   ((ar-coerce _sref 'fn) _source-file* _current-load-file* 'fontcolor)
   ((ar-coerce _sref 'fn)
    _source*
    '(mac
      fontcolor
      (c . body)
      (w/uniq
       g
       (quasiquote
        (let (unquote g . nil)
          (unquote c . nil)
          (if (unquote g . nil)
            (tag
             (font color (unquote g . nil) . nil)
             (unquote-splicing body . nil)
             .
             nil)
            (do (unquote-splicing body . nil) . nil)
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
    'fontcolor)
   ((lambda ()
      (if (not (ar-false? ((ar-coerce _bound 'fn) 'fontcolor)))
        ((lambda ()
           ((ar-coerce _disp 'fn) "*** redefining " ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) 'fontcolor ((ar-coerce _stderr 'fn)))
           ((ar-coerce _disp 'fn) #\newline ((ar-coerce _stderr 'fn)))))
        'nil)
      (begin
        (let ((zz
               ((ar-coerce _annotate 'fn)
                'mac
                (let ((| fontcolor|
                       (lambda (c . body)
                         ((let ((| fontcolor|
                                 (lambda (g)
                                   `(let ,g
                                      ,c
                                      (if ,g
                                        (tag
                                         (font color ,g)
                                         ,@(ar-nil-terminate body))
                                        (do ,@(ar-nil-terminate body)))))))
                            | fontcolor|)
                          ((ar-coerce _uniq 'fn))))))
                  | fontcolor|))))
          (namespace-set-variable-value! '_fontcolor zz)
          zz))))))


