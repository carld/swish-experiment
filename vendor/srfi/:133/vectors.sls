;; DO NOT EDIT THIS FILE!!
;; This inlined chez-srfi library code is autogenerated using command:
;; $ ./install.chezscheme.sps /usr/share/r6rs
;; Source origin: https://github.com/arcfide/chez-srfi
;; Please refer to project site for full credits and original code.
;;;;;; File header: %3a133/vectors.sls
;;;;;; File header: %3a133/vectors-impl.scm
;;;;;; SRFI 43: Vector library                           -*- Scheme -*-
;;;
;;; $Id$
;;;
;;; Taylor Campbell wrote this code; he places it in the public domain.
;;; Will Clinger [wdc] made some corrections, also in the public domain.
;;; John Cowan modified this code for SRFI 133; his changes are also in
;;; the public domain.  However, in jurisdictions where it is not possible
;;; to dedicate something to the public domain, the entire implementation
;;; is made available under the same license as SRFI 133.
(library (srfi :133 vectors)
  (export vector-unfold vector-unfold-right vector-copy
   vector-reverse-copy vector-append vector-concatenate
   vector-append-subvectors vector-empty? vector= vector-fold
   vector-fold-right vector-map vector-map! vector-for-each
   vector-count vector-cumulate vector-index vector-skip
   vector-index-right vector-skip-right vector-binary-search
   vector-any vector-every vector-partition vector-swap!
   vector-fill! vector-reverse! vector-copy!
   vector-reverse-copy! vector-unfold! vector-unfold-right!
   vector->list reverse-vector->list list->vector
   reverse-list->vector vector->string string->vector)
  (import
    (rename
      (rnrs)
      (vector-fill! rnrs:vector-fill!)
      (vector->list rnrs:vector->list)
      (list->vector rnrs:list->vector))
    (rnrs mutable-strings)
    (srfi private include))
  (begin
    (define-syntax receive
      (syntax-rules ()
        [(receive ?formals ?producer ?body1 ?body2 ...)
         (call-with-values
           (lambda () ?producer)
           (lambda ?formals ?body1 ?body2 ...))]))
    (define-syntax let*-optionals
      (syntax-rules ()
        [(let*-optionals (?x ...) ((?var ?default) ...) ?body1
           ?body2 ...)
         (let ([args (?x ...)])
           (let*-optionals args ((?var ?default) ...) ?body1 ?body2
             ...))]
        [(let*-optionals ?args ((?var ?default) ...) ?body1 ?body2
           ...)
         (let*-optionals:aux ?args ?args ((?var ?default) ...) ?body1
           ?body2 ...)]))
    (define-syntax let*-optionals:aux
      (syntax-rules ()
        [(aux ?orig-args-var ?args-var () ?body1 ?body2 ...)
         (if (null? ?args-var)
             (let () ?body1 ?body2 ...)
             (error "too many arguments"
               (length ?orig-args-var)
               ?orig-args-var))]
        [(aux ?orig-args-var ?args-var ((?var ?default) ?more ...)
              ?body1 ?body2 ...)
         (if (null? ?args-var)
             (let* ([?var ?default] ?more ...) ?body1 ?body2 ...)
             (let ([?var (car ?args-var)] [new-args (cdr ?args-var)])
               (let*-optionals:aux ?orig-args-var new-args (?more ...)
                 ?body1 ?body2 ...)))]))
    (define (nonneg-int? x)
      (and (integer? x) (not (negative? x))))
    (define (between? x y z) (and (< x y) (<= y z)))
    (define (unspecified-value) (if #f #f))
    (define (vectors-ref vectors i)
      (map (lambda (v) (vector-ref v i)) vectors))
    (define (check-type pred? value callee)
      (if (pred? value)
          value
          (check-type
            pred?
            (error "erroneous value"
              (list pred? value)
              `(while calling ,callee))
            callee)))
    (define (check-index vec index callee)
      (let ([index (check-type integer? index callee)])
        (cond
          [(< index 0)
           (check-index
             vec
             (error "vector index too low"
               index
               `(into vector ,vec)
               `(while calling ,callee))
             callee)]
          [(>= index (vector-length vec))
           (check-index
             vec
             (error "vector index too high"
               index
               `(into vector ,vec)
               `(while calling ,callee))
             callee)]
          [else index])))
    (define (check-indices vec start start-name end end-name
             callee)
      (let ([lose (lambda things
                    (apply
                      error
                      "vector range out of bounds"
                      (append things `(vector was ,vec) `(,start-name was ,start)
                        `(,end-name was ,end) `(while calling ,callee))))]
            [start (check-type integer? start callee)]
            [end (check-type integer? end callee)])
        (cond
          [(> start end)
           (receive
             (new-start new-end)
             (lose `(,end-name < ,start-name))
             (check-indices vec new-start start-name new-end end-name
               callee))]
          [(< start 0)
           (check-indices vec (lose `(,start-name < 0)) start-name end
             end-name callee)]
          [(>= start (vector-length vec))
           (check-indices vec
             (lose `(,start-name > len) `(len was ,(vector-length vec)))
             start-name end end-name callee)]
          [(> end (vector-length vec))
           (check-indices vec start start-name
             (lose `(,end-name > len) `(len was ,(vector-length vec)))
             end-name callee)]
          [else (values start end)])))
    (define (vector-parse-start+end vec args start-name end-name
             callee)
      (let ([len (vector-length vec)])
        (cond
          [(null? args) (values 0 len)]
          [(null? (cdr args))
           (check-indices vec (car args) start-name len end-name
             callee)]
          [(null? (cddr args))
           (check-indices vec (car args) start-name (cadr args)
             end-name callee)]
          [else
           (error "too many arguments"
             `(extra args were ,(cddr args))
             `(while calling ,callee))])))
    (define-syntax let-vector-start+end
      (syntax-rules ()
        [(let-vector-start+end ?callee ?vec ?args (?start ?end)
           ?body1 ?body2 ...)
         (let ([?vec (check-type vector? ?vec ?callee)])
           (receive (?start ?end)
             (vector-parse-start+end ?vec ?args '?start '?end ?callee)
             ?body1 ?body2 ...))]))
    (define %smallest-length
      (letrec ([loop (lambda (vector-list length callee)
                       (if (null? vector-list)
                           length
                           (loop
                             (cdr vector-list)
                             (min (vector-length
                                    (check-type
                                      vector?
                                      (car vector-list)
                                      callee))
                                  length)
                             callee)))])
        loop))
    (define %vector-copy!
      (letrec ([loop/l->r (lambda (target source send i j)
                            (cond
                              [(< i send)
                               (vector-set! target j (vector-ref source i))
                               (loop/l->r target source send (+ i 1)
                                 (+ j 1))]))]
               [loop/r->l (lambda (target source sstart i j)
                            (cond
                              [(>= i sstart)
                               (vector-set! target j (vector-ref source i))
                               (loop/r->l target source sstart (- i 1)
                                 (- j 1))]))])
        (lambda (target tstart source sstart send)
          (if (> sstart tstart)
              (loop/l->r target source send sstart tstart)
              (loop/r->l target source sstart (- send 1)
                (+ -1 tstart send (- sstart)))))))
    (define %vector-reverse-copy!
      (letrec ([loop (lambda (target source sstart i j)
                       (cond
                         [(>= i sstart)
                          (vector-set! target j (vector-ref source i))
                          (loop target source sstart (- i 1) (+ j 1))]))])
        (lambda (target tstart source sstart send)
          (loop target source sstart (- send 1) tstart))))
    (define %vector-reverse!
      (letrec ([loop (lambda (vec i j)
                       (cond
                         [(<= i j)
                          (let ([v (vector-ref vec i)])
                            (vector-set! vec i (vector-ref vec j))
                            (vector-set! vec j v)
                            (loop vec (+ i 1) (- j 1)))]))])
        (lambda (vec start end) (loop vec start (- end 1)))))
    (define %vector-fold1
      (letrec ([loop (lambda (kons knil len vec i)
                       (if (= i len)
                           knil
                           (loop kons (kons knil (vector-ref vec i)) len
                             vec (+ i 1))))])
        (lambda (kons knil len vec) (loop kons knil len vec 0))))
    (define %vector-fold2+
      (letrec ([loop (lambda (kons knil len vectors i)
                       (if (= i len)
                           knil
                           (loop kons
                             (apply kons knil (vectors-ref vectors i)) len
                             vectors (+ i 1))))])
        (lambda (kons knil len vectors)
          (loop kons knil len vectors 0))))
    (define %vector-map1!
      (letrec ([loop (lambda (f target vec i)
                       (if (zero? i)
                           target
                           (let ([j (- i 1)])
                             (vector-set! target j (f (vector-ref vec j)))
                             (loop f target vec j))))])
        (lambda (f target vec len) (loop f target vec len))))
    (define %vector-map2+!
      (letrec ([loop (lambda (f target vectors i)
                       (if (zero? i)
                           target
                           (let ([j (- i 1)])
                             (vector-set!
                               target
                               j
                               (apply f (vectors-ref vectors j)))
                             (loop f target vectors j))))])
        (lambda (f target vectors len)
          (loop f target vectors len))))
    (define (vector-unfold f length . initial-seeds)
      (define vec (make-vector length))
      (apply vector-unfold! f vec 0 length initial-seeds)
      vec)
    (define vector-unfold!
      (letrec ([tabulate! (lambda (f vec i len)
                            (cond
                              [(< i len)
                               (vector-set! vec i (f i))
                               (tabulate! f vec (+ i 1) len)]))]
               [unfold1! (lambda (f vec i len seed)
                           (if (< i len)
                               (receive
                                 (elt new-seed)
                                 (f i seed)
                                 (vector-set! vec i elt)
                                 (unfold1! f vec (+ i 1) len new-seed))))]
               [unfold2+! (lambda (f vec i len seeds)
                            (if (< i len)
                                (receive
                                  (elt . new-seeds)
                                  (apply f i seeds)
                                  (vector-set! vec i elt)
                                  (unfold2+! f vec (+ i 1) len
                                    new-seeds))))])
        (lambda (f vec start end . initial-seeds)
          (let ([f (check-type procedure? f vector-unfold!)]
                [start (check-type nonneg-int? start vector-unfold!)]
                [end (check-type nonneg-int? end vector-unfold!)])
            (let ()
              (cond
                [(null? initial-seeds) (tabulate! f vec start end)]
                [(null? (cdr initial-seeds))
                 (unfold1! f vec start end (car initial-seeds))]
                [else (unfold2+! f vec start end initial-seeds)]))))))
    (define (vector-unfold-right f len . initial-seeds)
      (define vec (make-vector len))
      (apply vector-unfold-right! f vec 0 len initial-seeds)
      vec)
    (define (vector-unfold-right! f vec start end .
             initial-seeds)
      (letrec ([tabulate! (lambda (f vec i)
                            (cond
                              [(>= i start)
                               (vector-set! vec i (f i))
                               (tabulate! f vec (- i 1))]))]
               [unfold1! (lambda (f vec i seed)
                           (if (>= i start)
                               (receive
                                 (elt new-seed)
                                 (f i seed)
                                 (vector-set! vec i elt)
                                 (unfold1! f vec (- i 1) new-seed))))]
               [unfold2+! (lambda (f vec i seeds)
                            (if (>= i start)
                                (receive
                                  (elt . new-seeds)
                                  (apply f i seeds)
                                  (vector-set! vec i elt)
                                  (unfold2+! f vec (- i 1) new-seeds))))])
        (let ([f (check-type procedure? f vector-unfold-right!)]
              [start (check-type nonneg-int? start vector-unfold-right!)]
              [end (check-type nonneg-int? end vector-unfold-right!)])
          (let ([i (- end 1)])
            (cond
              [(null? initial-seeds) (tabulate! f vec i)]
              [(null? (cdr initial-seeds))
               (unfold1! f vec i (car initial-seeds))]
              [else (unfold2+! f vec i initial-seeds)])))))
    (define (vector-copy vec . args)
      (let ([vec (check-type vector? vec vector-copy)])
        (receive
          (start end fill)
          (vector-copy:parse-args vec args)
          (let ([new-vector (make-vector (- end start) fill)])
            (%vector-copy! new-vector 0 vec start
              (if (> end (vector-length vec)) (vector-length vec) end))
            new-vector))))
    (define (vector-copy:parse-args vec args)
      (define (parse-args start end n fill)
        (let ([start (check-type nonneg-int? start vector-copy)]
              [end (check-type nonneg-int? end vector-copy)])
          (cond
            [(and (<= 0 start end) (<= start n))
             (values start end fill)]
            [else
             (error "illegal arguments"
               `(while calling ,vector-copy)
               `(start was ,start)
               `(end was ,end)
               `(vector was ,vec))])))
      (let ([n (vector-length vec)])
        (cond
          [(null? args) (parse-args 0 n n (unspecified-value))]
          [(null? (cdr args))
           (parse-args (car args) n n (unspecified-value))]
          [(null? (cddr args))
           (parse-args (car args) (cadr args) n (unspecified-value))]
          [(null? (cdddr args))
           (parse-args (car args) (cadr args) n (caddr args))]
          [else
           (error "too many arguments" vector-copy (cdddr args))])))
    (define (vector-reverse-copy vec . maybe-start+end)
      (let-vector-start+end vector-reverse-copy vec maybe-start+end (start end)
        (let ([new (make-vector (- end start))])
          (%vector-reverse-copy! new 0 vec start end)
          new)))
    (define (vector-append . vectors)
      (vector-concatenate:aux vectors vector-append))
    (define (vector-concatenate vector-list)
      (vector-concatenate:aux vector-list vector-concatenate))
    (define vector-concatenate:aux
      (letrec ([compute-length (lambda (vectors len callee)
                                 (if (null? vectors)
                                     len
                                     (let ([vec (check-type
                                                  vector?
                                                  (car vectors)
                                                  callee)])
                                       (compute-length
                                         (cdr vectors)
                                         (+ (vector-length vec) len)
                                         callee))))]
               [concatenate! (lambda (vectors target to)
                               (if (null? vectors)
                                   target
                                   (let* ([vec1 (car vectors)]
                                          [len (vector-length vec1)])
                                     (%vector-copy! target to vec1 0 len)
                                     (concatenate!
                                       (cdr vectors)
                                       target
                                       (+ to len)))))])
        (lambda (vectors callee)
          (cond
            [(null? vectors) (make-vector 0)]
            [(null? (cdr vectors))
             (let* ([vec (check-type vector? (car vectors) callee)]
                    [len (vector-length vec)]
                    [new (make-vector len)])
               (%vector-copy! new 0 vec 0 len)
               new)]
            [else
             (let ([new-vector (make-vector
                                 (compute-length vectors 0 callee))])
               (concatenate! vectors new-vector 0)
               new-vector)]))))
    (define (vector-append-subvectors . args)
      (define (gather-args args)
        (let loop ([args args] [vecs '()] [starts '()] [ends '()])
          (if (null? args)
              (values (reverse vecs) (reverse starts) (reverse ends))
              (loop
                (cdddr args)
                (cons (car args) vecs)
                (cons (cadr args) starts)
                (cons (caddr args) ends)))))
      (define (total-length starts ends)
        (let loop ([count 0] [starts starts] [ends ends])
          (if (null? starts)
              count
              (let ([start (car starts)] [end (car ends)])
                (loop (+ count (- end start)) (cdr starts) (cdr ends))))))
      (define (copy-each! result vecs starts ends)
        (let loop ([at 0] [vecs vecs] [starts starts] [ends ends])
          (if (null? vecs)
              result
              (let ([vec (car vecs)]
                    [start (car starts)]
                    [end (car ends)])
                (%vector-copy! result at vec start end)
                (loop
                  (+ at (- end start))
                  (cdr vecs)
                  (cdr starts)
                  (cdr ends))))))
      (receive
        (vecs starts ends)
        (gather-args args)
        (define result (make-vector (total-length starts ends)))
        (copy-each! result vecs starts ends)))
    (define (vector-empty? vec)
      (let ([vec (check-type vector? vec vector-empty?)])
        (zero? (vector-length vec))))
    (define (vector= elt=? . vectors)
      (let ([elt=? (check-type procedure? elt=? vector=)])
        (cond
          [(null? vectors) #t]
          [(null? (cdr vectors))
           (check-type vector? (car vectors) vector=)
           #t]
          [else
           (let loop ([vecs vectors])
             (let ([vec1 (check-type vector? (car vecs) vector=)]
                   [vec2+ (cdr vecs)])
               (or (null? vec2+)
                   (and (binary-vector= elt=? vec1 (car vec2+))
                        (loop vec2+)))))])))
    (define (binary-vector= elt=? vector-a vector-b)
      (or (eq? vector-a vector-b)
          (let ([length-a (vector-length vector-a)]
                [length-b (vector-length vector-b)])
            (letrec ([loop (lambda (i)
                             (or (= i length-a)
                                 (and (< i length-b)
                                      (test
                                        (vector-ref vector-a i)
                                        (vector-ref vector-b i)
                                        i))))]
                     [test (lambda (elt-a elt-b i)
                             (and (or (eq? elt-a elt-b)
                                      (elt=? elt-a elt-b))
                                  (loop (+ i 1))))])
              (and (= length-a length-b) (loop 0))))))
    (define (vector-fold kons knil vec . vectors)
      (let ([kons (check-type procedure? kons vector-fold)]
            [vec (check-type vector? vec vector-fold)])
        (if (null? vectors)
            (%vector-fold1 kons knil (vector-length vec) vec)
            (%vector-fold2+
              kons
              knil
              (%smallest-length vectors (vector-length vec) vector-fold)
              (cons vec vectors)))))
    (define vector-fold-right
      (letrec ([loop1 (lambda (kons knil vec i)
                        (if (negative? i)
                            knil
                            (loop1
                              kons
                              (kons knil (vector-ref vec i))
                              vec
                              (- i 1))))]
               [loop2+ (lambda (kons knil vectors i)
                         (if (negative? i)
                             knil
                             (loop2+
                               kons
                               (apply kons knil (vectors-ref vectors i))
                               vectors
                               (- i 1))))])
        (lambda (kons knil vec . vectors)
          (let ([kons (check-type procedure? kons vector-fold-right)]
                [vec (check-type vector? vec vector-fold-right)])
            (if (null? vectors)
                (loop1 kons knil vec (- (vector-length vec) 1))
                (loop2+
                  kons
                  knil
                  (cons vec vectors)
                  (- (%smallest-length
                       vectors
                       (vector-length vec)
                       vector-fold-right)
                     1)))))))
    (define (vector-map! f vec . vectors)
      (let ([f (check-type procedure? f vector-map!)]
            [vec (check-type vector? vec vector-map!)])
        (if (null? vectors)
            (%vector-map1! f vec vec (vector-length vec))
            (%vector-map2+!
              f
              vec
              (cons vec vectors)
              (%smallest-length vectors (vector-length vec) vector-map!)))
        (unspecified-value)))
    (define (vector-count pred? vec . vectors)
      (let ([pred? (check-type procedure? pred? vector-count)]
            [vec (check-type vector? vec vector-count)])
        (if (null? vectors)
            (%vector-fold1
              (lambda (count elt) (if (pred? elt) (+ count 1) count))
              0
              (vector-length vec)
              vec)
            (%vector-fold2+
              (lambda (count . elts)
                (if (apply pred? elts) (+ count 1) count))
              0
              (%smallest-length vectors (vector-length vec) vector-count)
              (cons vec vectors)))))
    (define (vector-cumulate f vec knil)
      (let* ([len (vector-length vec)] [result (make-vector len)])
        (let loop ([i 0] [left knil])
          (if (= i len)
              result
              (let* ([right (vector-ref vec i)] [r (f left right)])
                (vector-set! result i r)
                (loop (+ i 1) r))))))
    (define (vector-index pred? vec . vectors)
      (vector-index/skip pred? vec vectors vector-index))
    (define (vector-skip pred? vec . vectors)
      (vector-index/skip
        (lambda elts (not (apply pred? elts)))
        vec
        vectors
        vector-skip))
    (define vector-index/skip
      (letrec ([loop1 (lambda (pred? vec len i)
                        (cond
                          [(= i len) #f]
                          [(pred? (vector-ref vec i)) i]
                          [else (loop1 pred? vec len (+ i 1))]))]
               [loop2+ (lambda (pred? vectors len i)
                         (cond
                           [(= i len) #f]
                           [(apply pred? (vectors-ref vectors i)) i]
                           [else (loop2+ pred? vectors len (+ i 1))]))])
        (lambda (pred? vec vectors callee)
          (let ([pred? (check-type procedure? pred? callee)]
                [vec (check-type vector? vec callee)])
            (if (null? vectors)
                (loop1 pred? vec (vector-length vec) 0)
                (loop2+
                  pred?
                  (cons vec vectors)
                  (%smallest-length vectors (vector-length vec) callee)
                  0))))))
    (define (vector-index-right pred? vec . vectors)
      (vector-index/skip-right
        pred?
        vec
        vectors
        vector-index-right))
    (define (vector-skip-right pred? vec . vectors)
      (vector-index/skip-right
        (lambda elts (not (apply pred? elts)))
        vec
        vectors
        vector-index-right))
    (define vector-index/skip-right
      (letrec ([loop1 (lambda (pred? vec i)
                        (cond
                          [(negative? i) #f]
                          [(pred? (vector-ref vec i)) i]
                          [else (loop1 pred? vec (- i 1))]))]
               [loop2+ (lambda (pred? vectors i)
                         (cond
                           [(negative? i) #f]
                           [(apply pred? (vectors-ref vectors i)) i]
                           [else (loop2+ pred? vectors (- i 1))]))])
        (lambda (pred? vec vectors callee)
          (let ([pred? (check-type procedure? pred? callee)]
                [vec (check-type vector? vec callee)])
            (if (null? vectors)
                (loop1 pred? vec (- (vector-length vec) 1))
                (loop2+
                  pred?
                  (cons vec vectors)
                  (- (%smallest-length vectors (vector-length vec) callee)
                     1)))))))
    (define (vector-binary-search vec value cmp .
             maybe-start+end)
      (let ([cmp (check-type
                   procedure?
                   cmp
                   vector-binary-search)])
        (let-vector-start+end vector-binary-search vec maybe-start+end (start end)
          (let loop ([start start] [end end] [j #f])
            (let ([i (div (+ start end) 2)])
              (if (or (= start end) (and j (= i j)))
                  #f
                  (let ([comparison (check-type
                                      integer?
                                      (cmp (vector-ref vec i) value)
                                      `(,cmp for ,vector-binary-search))])
                    (cond
                      [(zero? comparison) i]
                      [(positive? comparison) (loop start i i)]
                      [else (loop i end i)]))))))))
    (define vector-any
      (letrec ([loop1 (lambda (pred? vec i len len-1)
                        (and (not (= i len))
                             (if (= i len-1)
                                 (pred? (vector-ref vec i))
                                 (or (pred? (vector-ref vec i))
                                     (loop1 pred? vec (+ i 1) len
                                       len-1)))))]
               [loop2+ (lambda (pred? vectors i len len-1)
                         (and (not (= i len))
                              (if (= i len-1)
                                  (apply pred? (vectors-ref vectors i))
                                  (or (apply pred? (vectors-ref vectors i))
                                      (loop2+ pred? vectors (+ i 1) len
                                        len-1)))))])
        (lambda (pred? vec . vectors)
          (let ([pred? (check-type procedure? pred? vector-any)]
                [vec (check-type vector? vec vector-any)])
            (if (null? vectors)
                (let ([len (vector-length vec)])
                  (loop1 pred? vec 0 len (- len 1)))
                (let ([len (%smallest-length
                             vectors
                             (vector-length vec)
                             vector-any)])
                  (loop2+ pred? (cons vec vectors) 0 len (- len 1))))))))
    (define vector-every
      (letrec ([loop1 (lambda (pred? vec i len len-1)
                        (or (= i len)
                            (if (= i len-1)
                                (pred? (vector-ref vec i))
                                (and (pred? (vector-ref vec i))
                                     (loop1 pred? vec (+ i 1) len
                                       len-1)))))]
               [loop2+ (lambda (pred? vectors i len len-1)
                         (or (= i len)
                             (if (= i len-1)
                                 (apply pred? (vectors-ref vectors i))
                                 (and (apply pred? (vectors-ref vectors i))
                                      (loop2+ pred? vectors (+ i 1) len
                                        len-1)))))])
        (lambda (pred? vec . vectors)
          (let ([pred? (check-type procedure? pred? vector-every)]
                [vec (check-type vector? vec vector-every)])
            (if (null? vectors)
                (let ([len (vector-length vec)])
                  (loop1 pred? vec 0 len (- len 1)))
                (let ([len (%smallest-length
                             vectors
                             (vector-length vec)
                             vector-every)])
                  (loop2+ pred? (cons vec vectors) 0 len (- len 1))))))))
    (define (vector-partition pred? vec)
      (let* ([len (vector-length vec)]
             [cnt (vector-count pred? vec)]
             [result (make-vector len)])
        (let loop ([i 0] [yes 0] [no cnt])
          (if (= i len)
              (values result cnt)
              (let ([elem (vector-ref vec i)])
                (if (pred? elem)
                    (begin
                      (vector-set! result yes elem)
                      (loop (+ i 1) (+ yes 1) no))
                    (begin
                      (vector-set! result no elem)
                      (loop (+ i 1) yes (+ no 1)))))))))
    (define (vector-swap! vec i j)
      (let ([vec (check-type vector? vec vector-swap!)])
        (let ([i (check-index vec i vector-swap!)]
              [j (check-index vec j vector-swap!)])
          (let ([x (vector-ref vec i)])
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j x)))))
    (define $check-types
      (case-lambda
        [(who v start)
         (check-type vector? v who)
         (check-indices v start 'start (vector-length v) 'end who)]
        [(who v start end)
         (check-type vector? v who)
         (check-indices v start 'start end 'end who)]))
    (define vector-fill!
      (let ()
        (define $vector-fill!
          (lambda (vec value start end)
            (do ([i start (+ i 1)])
                ((= i end))
              (vector-set! vec i value))))
        (case-lambda
          [(vec value) (rnrs:vector-fill! vec value)]
          [(vec value start)
           ($check-types 'vector-fill! vec start)
           ($vector-fill! vec value start (vector-length vec))]
          [(vec value start end)
           ($check-types 'vector-fill! vec start end)
           ($vector-fill! vec value start end)])))
    (define (vector-copy! target tstart source .
             maybe-sstart+send)
      (define (doit! sstart send source-length)
        (let ([tstart (check-type nonneg-int? tstart vector-copy!)]
              [sstart (check-type nonneg-int? sstart vector-copy!)]
              [send (check-type nonneg-int? send vector-copy!)])
          (cond
            [(and (<= 0 sstart send source-length)
                  (<= (+ tstart (- send sstart)) (vector-length target)))
             (%vector-copy! target tstart source sstart send)]
            [else
             (error "illegal arguments"
               `(while calling ,vector-copy!)
               `(target was ,target)
               `(target-length was ,(vector-length target))
               `(tstart was ,tstart)
               `(source was ,source)
               `(source-length was ,source-length)
               `(sstart was ,sstart)
               `(send was ,send))])))
      (let ([n (vector-length source)])
        (cond
          [(null? maybe-sstart+send) (doit! 0 n n)]
          [(null? (cdr maybe-sstart+send))
           (doit! (car maybe-sstart+send) n n)]
          [(null? (cddr maybe-sstart+send))
           (doit! (car maybe-sstart+send) (cadr maybe-sstart+send) n)]
          [else
           (error "too many arguments"
             vector-copy!
             (cddr maybe-sstart+send))])))
    (define (vector-reverse-copy! target tstart source .
             maybe-sstart+send)
      (define (doit! sstart send source-length)
        (let ([tstart (check-type
                        nonneg-int?
                        tstart
                        vector-reverse-copy!)]
              [sstart (check-type
                        nonneg-int?
                        sstart
                        vector-reverse-copy!)]
              [send (check-type nonneg-int? send vector-reverse-copy!)])
          (cond
            [(and (eq? target source)
                  (or (between? sstart tstart send)
                      (between? tstart sstart (+ tstart (- send sstart)))))
             (error "vector range for self-copying overlaps"
               vector-reverse-copy!
               `(vector was ,target)
               `(tstart was ,tstart)
               `(sstart was ,sstart)
               `(send was ,send))]
            [(and (<= 0 sstart send source-length)
                  (<= (+ tstart (- send sstart)) (vector-length target)))
             (%vector-reverse-copy! target tstart source sstart send)]
            [else
             (error "illegal arguments"
               `(while calling ,vector-reverse-copy!)
               `(target was ,target)
               `(target-length was ,(vector-length target))
               `(tstart was ,tstart)
               `(source was ,source)
               `(source-length was ,source-length)
               `(sstart was ,sstart)
               `(send was ,send))])))
      (let ([n (vector-length source)])
        (cond
          [(null? maybe-sstart+send) (doit! 0 n n)]
          [(null? (cdr maybe-sstart+send))
           (doit! (car maybe-sstart+send) n n)]
          [(null? (cddr maybe-sstart+send))
           (doit! (car maybe-sstart+send) (cadr maybe-sstart+send) n)]
          [else
           (error "too many arguments"
             vector-reverse-copy!
             (cddr maybe-sstart+send))])))
    (define (vector-reverse! vec . start+end)
      (let-vector-start+end vector-reverse! vec start+end
        (start end) (%vector-reverse! vec start end)))
    (define vector->list
      (let ()
        (define ($vector->list vec start end)
          (do ([i (- end 1) (- i 1)]
               [result '() (cons (vector-ref vec i) result)])
              ((< i start) result)))
        (case-lambda
          [(vec) (rnrs:vector->list vec)]
          [(vec start)
           ($check-types 'vector->list vec start)
           ($vector->list vec start (vector-length vec))]
          [(vec start end)
           ($check-types 'vector->list vec start end)
           ($vector->list vec start (vector-length vec))])))
    (define (reverse-vector->list vec . maybe-start+end)
      (let-vector-start+end reverse-vector->list vec maybe-start+end (start end)
        (do ([i start (+ i 1)]
             [result '() (cons (vector-ref vec i) result)])
            ((= i end) result))))
    (define list->vector
      (case-lambda
        [(lst) (rnrs:list->vector lst)]
        [(lst start)
         (check-type nonneg-int? start list->vector)
         (rnrs:list->vector (list-tail lst start))]
        [(lst start end)
         (check-type nonneg-int? start list->vector)
         (check-type nonneg-int? end list->vector)
         (unless (<= start end)
           (error 'list->vector "start is greater than end" start end))
         (let ([len (- end start)])
           (let ([v (make-vector len)])
             (let loop ([i 0] [ls (list-tail lst start)])
               (unless (= i len)
                 (unless (pair? ls)
                   (if (null? ls)
                       (error 'list->vector "list too short" lst start end)
                       (error 'list->vector "improper list" lst)))
                 (vector-set! v i (car ls))
                 (loop (fx+ i 1) (cdr ls))))
             v))]))
    (define (reverse-list->vector lst . maybe-start+end)
      (let*-optionals
        maybe-start+end
        ((start 0) (end (length lst)))
        (let ([start (check-type
                       nonneg-int?
                       start
                       reverse-list->vector)]
              [end (check-type nonneg-int? end reverse-list->vector)])
          ((lambda (f)
             (vector-unfold-right f (- end start) (list-tail lst start)))
            (lambda (index l)
              (cond
                [(null? l)
                 (error "list too short"
                   `(list was ,lst)
                   `(attempted end was ,end)
                   `(while calling ,reverse-list->vector))]
                [(pair? l) (values (car l) (cdr l))]
                [else
                 (error "erroneous value"
                   (list list? lst)
                   `(while calling ,reverse-list->vector))]))))))
    (define (vector->string vec . maybe-start+end)
      (let* ([len (vector-length vec)]
             [start (if (null? maybe-start+end) 0 (car maybe-start+end))]
             [end (if (null? maybe-start+end)
                      len
                      (if (null? (cdr maybe-start+end))
                          len
                          (cadr maybe-start+end)))]
             [size (- end start)])
        (define result (make-string size))
        (let loop ([at 0] [i start])
          (if (= i end)
              result
              (begin
                (string-set! result at (vector-ref vec i))
                (loop (+ at 1) (+ i 1)))))))
    (define (string->vector str . maybe-start+end)
      (let* ([len (string-length str)]
             [start (if (null? maybe-start+end) 0 (car maybe-start+end))]
             [end (if (null? maybe-start+end)
                      len
                      (if (null? (cdr maybe-start+end))
                          len
                          (cadr maybe-start+end)))]
             [size (- end start)])
        (define result (make-vector size))
        (let loop ([at 0] [i start])
          (if (= i end)
              result
              (begin
                (vector-set! result at (string-ref str i))
                (loop (+ at 1) (+ i 1)))))))))
