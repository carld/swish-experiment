;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a19/time/compat.larceny.sls
#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :19 time compat)
  (export time-resolution
    (rename (my:timezone-offset timezone-offset)) current-time
    cumulative-thread-time cumulative-process-time
    cumulative-gc-time time-nanosecond time-second)
  (import
    (rnrs base)
    (primitives r5rs:require current-utc-time timezone-offset)
    (srfi :19 time not-implemented))
  (define dummy (begin (r5rs:require 'time) #f))
  (define time-resolution 1000)
  (define my:timezone-offset
    (let-values ([(secs _) (current-utc-time)])
      (timezone-offset secs)))
  (define (current-time)
    (let-values ([(secs micros) (current-utc-time)])
      (cons secs (* micros 1000))))
  (define time-nanosecond cdr)
  (define time-second car))
