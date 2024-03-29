;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a98/os-environment-variables.ypsilon.sls
#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :98 os-environment-variables)
  (export
    (rename
      (lookup-process-environment get-environment-variable)
      (process-environment->alist get-environment-variables)))
  (import
    (only
      (core)
      lookup-process-environment
      process-environment->alist)))
