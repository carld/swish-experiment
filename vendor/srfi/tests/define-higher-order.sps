;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: ./tests/define-higher-order.sps
#!r6rs
;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: MIT
(import
  (except (rnrs) define)
  (srfi :64 testing)
  (srfi :219))
(test-begin "srfi-219")
(let ()
  (define ((greet-with-prefix prefix) suffix)
    (string-append prefix " " suffix))
  (let ([greet (greet-with-prefix "Hello")])
    (test-equal "Hello there!" (greet "there!"))))
(let ()
  (define ((append-to . a) . b) (apply append (append a b)))
  (test-equal '() ((append-to '()) '()))
  (test-equal
    '(1 2 3 4 5 6 7 8)
    ((append-to '(1 2) '(3 4)) '(5 6) '(7 8))))
(let ()
  (define (((jenga a b) c d)) (list a b c d))
  (test-equal '(1 2 3 4) (((jenga 1 2) 3 4))))
(test-end)
