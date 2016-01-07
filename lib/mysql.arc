;;;Mysql support for arc
;;;

($ (require (planet jaz/mysql:1)))

($ (odef 'mysql-connect
         '(user-str password-str host-str
           (o port-int 3306) (o schema-str nil) (o encoding-sym nil))
         (lambda (u pw h p s e)
           (let ((c (apply connect
                           (map (lambda (x) (and (not (eq? 'nil x)) x))
                                (list h p u pw)))))
             (and (not (eq? 'nil s))
                  (query0 (string-append "use " s)))
             (and (not (eq? 'nil e))
                  (query0 (string-append "set names " (symbol->string e))))
             c))))

($ (odef 'mysql-query '(connection proc-fn pred-fn sql-str ...)
      (lambda (c f ? . s)
        (ac-niltree
          (begin
            (current-connection c)
            (query/map/filter
              (lambda x (ac-denil (apply f x)))
              (lambda x (if (eq? 'nil (apply ? x)) #f #t))
              (apply string-append s)))))))

($ (odef 'mysql-null '(x) (lambda (x) (ac-niltree (sql-null? x)))))

(def connect (type user password host (o port 3306) (o schema) (o encoding))
     "Signature: (sym str str str [int str sym]) -->
     ((str ...)-->bool (str ...)-->list . str) --> list
     Connects to and returns function for quering the server.
     Returned function requires a predicate and a worker function.
     Example: (= db (connect 'mysql \"john\" \"doe\" \"db.com\"))
                 (db idfn idfn \"select ... where \" a \" and \" b) 
     Supplied functions receive each selected column as a separate argument."
     (case type
       mysql
         (withs
           (con (fn () (mysql-connect user password host port schema encoding))
            curcon (con)
            q [apply mysql-query (cons curcon _)])
           (fn ls (on-err [do (= curcon (con)) (q ls)]
                          (fn () (q ls)))))))
