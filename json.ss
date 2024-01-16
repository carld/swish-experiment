#!chezscheme
(library (json)
  (export
   http:call-with-json
   json:set-list
   )
  (import (chezscheme))
  (import (swish imports))

  (define (get-chunk! ip bv n)
    (let ([count (get-bytevector-n! ip bv 0 n)])
      (when (or (eof-object? count) (not (= count n)))
        (throw 'unexpected-eof))))
  
  (define http:call-with-json
    (case-lambda
      [(conn header content-limit f timeout)
       (arg-check 'http:call-with-json
		  [header json:object?]
		  [content-limit (lambda (x) (and (fixnum? x) (fx>= x 0)))]
		  [f procedure?]
		  [timeout (lambda (x) (and (fixnum? x) (fx> x 0)))])
       (let* ([len (http:get-content-length header)]
              [type (or (http:find-header 'content-type header) "none")]
              [data
               (cond
		[(not len) (json:make-object)]
		[(starts-with-ci? type "application/json")
		 (when (> len content-limit)
                   (throw 'http-content-limit-exceeded))
		 (http:call-with-ports conn
				       (lambda (ip op)
					 (let ([content (make-bytevector len)])
					   (get-chunk! ip content len)
					   (json:bytevector->object content))))]
		[else (json:make-object)])])
	 (f data))]))
  
  (define json:set-list
    (case-lambda
      [(obj names values)
       (for-all (lambda (n v) (json:set! obj n v)) names values)
       obj]
      [(obj pairs)
       (for-each (lambda (p)
		   (match p
		     [(,n . ,v) (json:set! obj n v)]))
		 pairs)
       obj
       ]))

  )

(import (json))

(define ob3 (json:make-object [a 7]))
(json:set-list ob3 `(z) `(99))
(cond [(equal? (json:ref ob3 'z #f) 99)
       (format #t "test passed~%")]
      [else (error 'json-unit-test "failed" ob3)])
(define ob4 (json:make-object))
(json:set-list ob4 `( (a . 1) (b . 2)))
(assert (equal? (json:ref ob4 'b #f) 2))
(assert (equal? (json:ref ob4 'a #f) 1))
