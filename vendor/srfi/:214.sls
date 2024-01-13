;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a214.sls
#!r6rs
(library (srfi :214)
  (export make-flexvector flexvector flexvector-unfold
   flexvector-unfold-right flexvector-copy
   flexvector-reverse-copy flexvector-append
   flexvector-concatenate flexvector-append-subvectors
   flexvector? flexvector-empty? flexvector=? flexvector-ref
   flexvector-front flexvector-back flexvector-length
   flexvector-add! flexvector-add-front! flexvector-add-back!
   flexvector-remove! flexvector-remove-front!
   flexvector-remove-back! flexvector-add-all!
   flexvector-remove-range! flexvector-clear! flexvector-set!
   flexvector-swap! flexvector-fill! flexvector-reverse!
   flexvector-copy! flexvector-reverse-copy! flexvector-append!
   flexvector-fold flexvector-fold-right flexvector-map
   flexvector-map! flexvector-map/index flexvector-map/index!
   flexvector-append-map flexvector-append-map/index
   flexvector-filter flexvector-filter! flexvector-filter/index
   flexvector-filter/index! flexvector-for-each
   flexvector-for-each/index flexvector-count
   flexvector-cumulate flexvector-index flexvector-index-right
   flexvector-skip flexvector-skip-right
   flexvector-binary-search flexvector-any flexvector-every
   flexvector-partition flexvector->vector flexvector->list
   flexvector->string vector->flexvector list->flexvector
   string->flexvector reverse-flexvector->list
   reverse-list->flexvector generator->flexvector
   flexvector->generator)
  (import (srfi :214 impl)))
