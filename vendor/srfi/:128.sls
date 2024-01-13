;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a128.sls
(library (srfi :128)
  (export comparator? comparator-ordered? comparator-hashable?
   make-comparator make-pair-comparator make-list-comparator
   make-vector-comparator make-eq-comparator
   make-eqv-comparator make-equal-comparator boolean-hash
   char-hash char-ci-hash string-hash string-ci-hash
   symbol-hash number-hash make-default-comparator default-hash
   comparator-register-default! comparator-type-test-predicate
   comparator-equality-predicate comparator-ordering-predicate
   comparator-hash-function comparator-test-type
   comparator-check-type comparator-hash hash-bound hash-salt
   =? <? >? <=? >=? comparator-if<=>)
  (import (srfi :128 comparators)))
