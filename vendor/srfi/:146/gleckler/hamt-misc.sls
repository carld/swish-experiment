;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a146/gleckler/hamt-misc.sls
;;;; Utilities used by HAMT
(library (srfi :146 gleckler hamt-misc)
  (export
    assert
    do-list
    make-string-hash-table
    with-output-to-string)
  (import (except (rnrs) string-hash assert) (srfi :6) (srfi :39)
    (only (srfi :43) vector-copy!) (only (srfi :69) string-hash)
    (only (srfi :125) make-hash-table)
    (only (srfi :128) make-comparator))
  (define-syntax assert
    (syntax-rules ()
      [(_ (operator argument ...))
       (unless (operator argument ...)
         (error "Assertion failed:"
           '(operator argument ...)
           (list 'operator argument ...)))]
      [(_ expression)
       (unless expression
         (error "Assertion failed:" 'expression))]))
  (define-syntax do-list
    (syntax-rules ()
      [(_ (variable list) body ...)
       (do ([remaining list (cdr remaining)])
           ((null? remaining))
         (let ([variable (car remaining)]) body ...))]
      [(_ (element-variable index-variable list) body ...)
       (do ([remaining list (cdr remaining)]
            [index-variable 0 (+ index-variable 1)])
           ((null? remaining))
         (let ([element-variable (car remaining)]) body ...))]))
  (define string-comparator
    (make-comparator string? string=? #f string-hash))
  (define (make-string-hash-table)
    (make-hash-table string-comparator))
  (define (with-output-to-string thunk)
    (parameterize ([current-output-port (open-output-string)])
      (thunk)
      (get-output-string (current-output-port)))))