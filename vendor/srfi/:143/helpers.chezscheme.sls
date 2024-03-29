;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a143/helpers.chezscheme.sls
;; SRFI-143 Chez Scheme helper implementation
;;
;; Imports the fxabs, fxremainder, and fxquotient procedures from Chez Scheme
;; for the SRFI-143 functions.
;;
;; Copyright (c) 2018 - 2020 Andrew W. Keep
(library (srfi :143 helpers)
  (export fxabs fxremainder fxquotient)
  (import (chezscheme)))
