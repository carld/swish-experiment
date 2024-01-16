#!chezscheme
(library (middleware)
  (export
   middleware:form-data
   middleware:logging
   middleware:compose
   )
  (import (chezscheme))
  (import (swish imports))

  (import (json))
  
  (define-syntax (def-middleware stx)
    (syntax-case stx ()
      [(k name before after)
       #`(define (name url-handler)
	   (http:url-handler
	    before
	    (let* ([r (http:call-url-handler url-handler)])
	      after
	      r)))]))


					; example middleware that logs the environment and the response
  (define (middleware:logging url-handler)
    (http:url-handler
     (format #t "LOG: middleware:logging env: ~a ~a ~a~%"
	     (<request> method request)
	     (<request> path request)
	     (<request> params request))
     (let ((resp (http:call-url-handler url-handler)))
       (format #t "LOG: middleware:logging resp: ~a~%" resp)
       resp)))

  (define (middleware:form-data url-handler)
    (define content-limit (expt 2 20))
    (define timeout  3000)
    (http:url-handler
     (http:call-with-json conn header content-limit
			  (lambda (form-data)
					; add form-data to params
			    (json:set! params 'form-data form-data)
			    (http:call-url-handler url-handler))
			  timeout)))
 
  (define (middleware:compose url-handler . rest)
    (cond [(null? rest) url-handler]
	  [else (apply middleware:compose
		  ((car rest) url-handler)
		  (cdr rest))]))
  
  )


(define (x h . r)
  (format #t "h: ~a   r:  ~a ~%" h r))

#;(x 1 'a)
#;(x 2 'a 'b 'c)
#;(x 3 '(a b))
#;(x 4)
(apply x 5 '(a b c d))
(apply x 5 (cdr '(a)))

(import (middleware))
(define test-request (<request> make [method 'GET] [original-path "/test"] [path "/test"] [params #f] [host #f] [header #f]))
(define test-handler (http:url-handler
		      (format #t "In url-handler~%")
		      #t))
(define (mw1 h)
  (http:url-handler
   (format #t "In middleware 1: ~a~%" h)
   (http:call-url-handler h)))
(define (mw2 h)
  (http:url-handler
   (format #t "In middleware 2: ~a~%" h)
   (http:call-url-handler h)))
(define (mw3 h)
  (http:url-handler
   (format #t "In middleware 3:~a~%" h)
   (http:call-url-handler h)))

(define m (mw1 (mw2 (mw3 test-handler))))
(define mx (middleware:compose test-handler mw1 mw2 mw3))

(let ([conn 'conn]
      [request test-request]
      [header (json:make-object)]
      [params (json:make-object)])
 (http:call-url-handler m)
 (http:call-url-handler mx)
  )
