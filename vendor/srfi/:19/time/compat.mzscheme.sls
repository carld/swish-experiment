;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a19/time/compat.mzscheme.sls
#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.
(library (srfi :19 time compat)
  (export time-resolution timezone-offset current-time
    cumulative-thread-time cumulative-process-time
    cumulative-gc-time time-nanosecond time-second)
  (import
    (rnrs base)
    (only (scheme base) current-seconds seconds->date
      date-time-zone-offset current-inexact-milliseconds
      current-thread current-process-milliseconds
      current-gc-milliseconds))
  (define time-resolution 1000000)
  (define timezone-offset
    (date-time-zone-offset (seconds->date (current-seconds))))
  (define (millis->repr x)
    (let-values ([(d m) (div-and-mod x 1000)])
      (cons d (* m 1000000))))
  (define (current-time)
    (millis->repr
      (exact (floor (current-inexact-milliseconds)))))
  (define (cumulative-thread-time)
    (millis->repr
      (current-process-milliseconds (current-thread))))
  (define (cumulative-process-time)
    (millis->repr (current-process-milliseconds #f)))
  (define (cumulative-gc-time)
    (millis->repr (current-gc-milliseconds)))
  (define time-nanosecond cdr)
  (define time-second car))
