					; This library converts a scheme sql like language represented as a quoted list
					; to a string representing SQL.
					;  (select (c1 c2 c3 c4) (from t1) (where (and (= c1 1) (= c2 'hello'))))
					; It is based on an evaluator, as used for implementing Lisp interpreters.

(library (ssql)
  (export trace-ssql ssql)
  (import (chezscheme))
  (import (swish imports))

  (define (ssql-list exp)
    (cond ((null? exp) '())
	  (else (cons (ssql (car exp)) (ssql-list (cdr exp))))))
  
  (define (ssql exp)
    (define (list-join list sep)
      (join (ssql-list list) sep))
    
    (cond
     ;[(eq?  exp '?) (format "?")]
     [(number? exp)    (format "~a" exp)]
     [(symbol? exp)    (format "~a" exp)]
     [(string? exp)    (format "'~a'" exp)]
     [(list? exp)
      (match exp
	[(create ,table (,col . ,rest))
	 (format "create ~a (~a)"
		 (ssql table)
		 (join (map (lambda (c) (list-join c " ")) (cons col rest)) ", "))
	 ]
	
	[(into ,table ,cols)
	 (format "into ~a (~a)" (ssql table) (ssql cols))	  ]
	[(values ,vals)
	 (format "values (~a)" (ssql vals))	  ]
	[(set ,assignments . ,rest)
	 (format "set ~{~a~^, ~}"
		 (ssql-list (map (lambda (assignment) (cons '= assignment))
				 (cons assignments rest))))
	]
	[(,keyword . ,rest)
	 (guard (sql-keyword? keyword))
	 (format "~a ~a" (ssql keyword) (list-join rest " "))
	 ]
	[(,op ,op1 ,op2 )
	 (guard (sql-operator? op))
	 (format "~a ~a ~a" (ssql op1) (ssql op) (ssql op2))
	 ]
	[(,fn ,exp)
	 (guard (sql-function? fn))
	 (format "~a(~a)" (ssql fn) (ssql exp))
	 ]
	[(,type ,exp)
	 (guard (sql-type? type))
	 (format "~a(~a)" (ssql type) (ssql exp))]
	[,_ (list-join exp ", ")])]
     [(vector? exp)
      (ssql (vector->list exp))
      ]
     [else (error "could not interpret ssql: " exp)]))
  
  (define (sql-type? exp)
    (case exp
      ((varchar)
       #t)
      (else #f)))
  
  (define (sql-keyword? exp)
    (case exp
      ((alter create drop select from inner
	      outer join on where limit offset
	      group by insert into values update set
	      order table)
       #t)
      (else #f)))
  
  (define (sql-operator? exp)
    (case exp
      ((= > != >= < <= and or not) #t)
      (else #f )))
  
  (define (sql-function? exp)
    (case exp
      ((min max avg count pragma_table_info) #t)
      (else #f)))
  
  (define (trace-ssql exp)
    (let ((sql-str (ssql exp)))
      (format #t "SSQL|~s~%" exp)
      (format #t "SSQL|~s~%" sql-str)
      sql-str)))
