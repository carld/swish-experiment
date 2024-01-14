#!chezscheme
(library (mvc)
  (export
   mvc:table
   mvc:action
   mvc:controller
   mvc:view
   mvc:model
   mvc:url-handler
   )
  (import (chezscheme))
  (import (swish imports))
  (import (json))


					; in the initization of the gen-server, construct the
					; (controller (view (model params)))
					; using these combinators

					; actions are typically query or command
  (define (mvc:action controller view model)
    (lambda  (params form-data)
      (controller (view (model params form-data)))))

  (define (mvc:controller controller)
    (lambda  (view-result)
      (match view-result
	[#(ok ,view ,params) `#(ok ,(controller view params))]
	[,err         err]
	)))

  (define (mvc:view view)
    (lambda  (model-result)
      (match model-result
	[#(ok ,model ,params) `#(ok ,(view model params) ,params)]
	[,err          err]
	)))

  (define (mvc:model model)
    (lambda (params form-data)
      `#(ok ,(model params form-data) ,params)))

  (define (mvc:route-matches method path table-entry)
    (define matches (pregexp-match (vector-ref table-entry 1) path))
    (if (and (pair? matches)
	     (eq? method (vector-ref table-entry 0)))
	matches
	#f))

  (define (mvc:find-route routes method path)
    (cond
     [(null? routes) #f   ]
     [(pair? routes)
      (let* ([entry   (car routes)]
	     [matches (mvc:route-matches method path entry)])
	(if matches
	    `#(ok ,entry ,matches)
	    (mvc:find-route (cdr routes) method path)))]))

  (define (mvc:table routes)
					; TODO cache requests to avoid regexp-match for same path
					;    (define cache (ht:make string-ci-hash string-ci=? string?))
    (lambda (method path params)
      (define entry (mvc:find-route routes method path))
      (match entry
	[#(ok #(,m ,p ,action ,capture-names) ,matches)
	 `#(ok ,action ,(json:set-list params capture-names (cdr matches)))]

	[#(ok #(,method ,path-regexp ,action) ,matches)
	 `#(ok ,action ,params) ]
	[,err   err])))

  (define (mvc:url-handler route-table-dispatch)
    (http:url-handler

     (http:call-with-form
      conn header 1048576 0 '()
      (lambda (form-data)
	(match (route-table-dispatch (<request> method request)
				     (<request> path request)
				     params)
	  [#(ok ,action ,params)
	   (match (action params form-data)
	     [#(ok #(,code ,header ,content))
	      (http:respond conn code header content)])]
	  [,err (http:respond conn
			      404
			      `(("Content-Type" . "text/plain"))
			      (string->utf8 "Not Found"))])))))


  )

(import (mvc))
					; examples/tests
(define c1 (mvc:controller (lambda (v p) (format "result: ~a" v) )))
(define v1 (mvc:view  (lambda (m p) (number->string m) )))
(define m1 (mvc:model (lambda (p fd) (+ 1 p) )))
(define a1 (mvc:action c1 v1 m1))
					;(a1 `#(ok 6))

(define t1 (mvc:table `(#(GET "^/test/path$" ,a1))))
(define entry (t1 'GET "/test/path" (json:make-object )))
(define result
  (match entry
    [#(ok ,action ,params) (action 6 '()) ]))
(display  (cond [(equal? result `#(ok "result: 7")) `#(test-ok ,result)]
		[else `#(test-failed ,result )]))
(newline)
