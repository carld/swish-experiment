#!chezscheme
(library (json)
  (export
   json:set-list
   )
  (import (chezscheme))
  (import (swish imports))

  (define json:set-list
    (case-lambda
      [(obj names values)
       (for-all (lambda (n v) (json:set! obj n v)) names values)
       obj]
      [(obj pairs)
       (for-each (lambda (p) (match p [(,n . ,v) (json:set! obj n v)])) pairs)
       obj
       ]))

  )

(import (json))

(define ob3 (json:make-object [a 7]))
(json:set-list ob3 `(z) `(99))
(cond [(equal? (json:ref ob3 'z #f) 99)
       (format #t "test passed~%")]
      [else (error 'json-unit-test "failed" ob3)])
