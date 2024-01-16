#!chezscheme
(library (middleware)
  (export
   middleware:read-json-content
   middleware:log
   middleware:audit
   middleware:authorize
   middleware:authenticate
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

					; refer to common log format
					;host ident authuser date request status bytes
					;127.0.0.1 user-identifier frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
  (define (middleware:log url-handler)
    (http:url-handler
     (let ([resp (http:call-url-handler url-handler)])
       (format #t "~a - - [~a] \"~a ~a\" ~a - ~%"
	       (<request> host request)
	       (date-and-time (current-date 0))
	       (<request> method request)
	       (<request> path request)
	       resp)
       resp)))

  (define (middleware:read-json-content url-handler)
    (define content-limit (expt 2 20))
    (define timeout  3000)
    (http:url-handler
     (http:call-with-json conn header content-limit
			  (lambda (form-data)
					; add form-data to params
			    (json:set! params 'form-data form-data)
			    (http:call-url-handler url-handler))
			  timeout)))

  (define (middleware:audit url-handler)
    (http:url-handler
     (http:call-url-handler url-handler)))

  (define (middleware:authenticate url-handler)
    (http:url-handler
     (http:call-url-handler url-handler)))

  (define (middleware:authorize url-handler)
    (http:url-handler
     (http:call-url-handler url-handler)))
  
  (define (middleware:compose url-handler . rest)
    (cond [(null? rest) url-handler]
	  [else (apply middleware:compose
		       ((car rest) url-handler)
		       (cdr rest))]))
  
  )

(import (middleware))
(define test-request (<request> make [method 'GET] [original-path "/test"] [path "/test"] [params #f] [host #f] [header #f]))
(define test-handler (http:url-handler
;		      (format #t "In url-handler~%")
		      #t))
(define (mw1 h)
  (http:url-handler
;   (format #t "In middleware 1: ~a~%" h)
   (http:call-url-handler h)))
(define (mw2 h)
  (http:url-handler
;   (format #t "In middleware 2: ~a~%" h)
   (http:call-url-handler h)))
(define (mw3 h)
  (http:url-handler
;   (format #t "In middleware 3:~a~%" h)
   (http:call-url-handler h)))

;(define m (mw1 (mw2 (mw3 test-handler))))
(define mx (middleware:compose test-handler mw1 mw2 mw3))

(let ([conn 'conn]
      [request test-request]
      [header (json:make-object)]
      [params (json:make-object)])
; (http:call-url-handler m)
 (http:call-url-handler mx)
  )
