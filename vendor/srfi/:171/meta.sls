;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a171/meta.sls
#!r6rs
(library (srfi :171 meta)
  (export reduced reduced? unreduce ensure-reduced preserving-reduced
    list-reduce vector-reduce string-reduce bytevector-u8-reduce
    port-reduce generator-reduce)
  (import
    (except (rnrs) define-record-type)
    (srfi :9 records))
  (define-record-type <reduced>
    (reduced val)
    reduced?
    (val unreduce))
  (define (ensure-reduced x) (if (reduced? x) x (reduced x)))
  (define (preserving-reduced reducer)
    (lambda (a b)
      (let ([return (reducer a b)])
        (if (reduced? return) (reduced return) return))))
  (define (list-reduce f identity lst)
    (if (null? lst)
        identity
        (let ([v (f identity (car lst))])
          (if (reduced? v)
              (unreduce v)
              (list-reduce f v (cdr lst))))))
  (define (vector-reduce f identity vec)
    (let ([len (vector-length vec)])
      (let loop ([i 0] [acc identity])
        (if (= i len)
            acc
            (let ([acc (f acc (vector-ref vec i))])
              (if (reduced? acc) (unreduce acc) (loop (+ i 1) acc)))))))
  (define (string-reduce f identity str)
    (let ([len (string-length str)])
      (let loop ([i 0] [acc identity])
        (if (= i len)
            acc
            (let ([acc (f acc (string-ref str i))])
              (if (reduced? acc) (unreduce acc) (loop (+ i 1) acc)))))))
  (define (bytevector-u8-reduce f identity vec)
    (let ([len (bytevector-length vec)])
      (let loop ([i 0] [acc identity])
        (if (= i len)
            acc
            (let ([acc (f acc (bytevector-u8-ref vec i))])
              (if (reduced? acc) (unreduce acc) (loop (+ i 1) acc)))))))
  (define (port-reduce f identity reader port)
    (let loop ([val (reader port)] [acc identity])
      (if (eof-object? val)
          acc
          (let ([acc (f acc val)])
            (if (reduced? acc)
                (unreduce acc)
                (loop (reader port) acc))))))
  (define (generator-reduce f identity gen)
    (let loop ([val (gen)] [acc identity])
      (if (eof-object? val)
          acc
          (let ([acc (f acc val)])
            (if (reduced? acc) (unreduce acc) (loop (gen) acc)))))))
