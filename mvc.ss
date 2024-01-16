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
    (http:url-handler
     (match (controller (view (model params)))
       [#(ok #(,code ,headers ,content))
	(http:respond conn code headers content)])))

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
    (lambda (params)
      `#(ok ,(model params) ,params)))

  (define (mvc:route-matches method path table-entry)
    (define regexp/names (vector-ref table-entry 1))
    (define names   (cdr regexp/names))
    (define matches (pregexp-match (car regexp/names) path))
    (cond [(and (pair? matches)
		(eq? method (vector-ref table-entry 0)))
	   (assert (eq? (length names) (length (cdr matches))))
	   (map (lambda (name match) `(,name . ,match)) names (cdr matches))]
	  [else #f]))

  (define (mvc:find-route routes method path)
    (cond [(null? routes) #f]
	  [(pair? routes)
	   (let* ([entry   (car routes)]
		  [names+matches (mvc:route-matches method path entry)])
	     (cond [names+matches
		    `#(ok ,entry ,names+matches)]
		   [else 
		    (mvc:find-route (cdr routes) method path)]))]))

  (define (mvc:table routes)
					; TODO cache requests to avoid regexp-match for same path
					;    (define cache (ht:make string-ci-hash string-ci=? string?))

    
    (lambda (method path)
      (define found (mvc:find-route routes method path))
      (match found
	[#(ok #(,m ,p ,action) ,names+matches)
	 `#(ok ,action ,names+matches)]
	[,err   err])))
  
  (define (mvc:url-handler route-table-dispatch)
    (http:url-handler
     (match (route-table-dispatch (<request> method request)
				  (<request> path request))
       [#(ok ,action ,params-from-path-match)
	; set the captured matches from the path regexp in params
	(json:set-list params params-from-path-match)
	(http:call-url-handler action)]
       [,err (http:respond
	      conn
	      404
	      `(("Content-Type" . "text/plain"))
	      (string->utf8 "Not Found"))])))

  )

(import (mvc))
					; examples/tests
(define c1 (mvc:controller (lambda (v p) (format "result: ~a" v) )))
(define v1 (mvc:view  (lambda (m p) (number->string m) )))
(define m1 (mvc:model (lambda (p) (+ 1 p) )))
(define a1 (mvc:action c1 v1 m1))
					;(a1 `#(ok 6))

(define t1 (mvc:table `(#(GET ("^/([a-z]+)/path$" file) ,a1))))
(define entry (t1 'GET "/test/path" ))
(format #t "DEBUG: ~s ~%" entry)
(define result
  (match entry
    [#(ok ,action ,name+matches)
     (assert (assoc 'file name+matches))
     (assert (procedure? action))
     ]))

