#lang racket/base

(require racket/contract racket/mutability syntax/parse/define
         "control.rkt"
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit racket/vector))

(provide
 do-hash-table
 (contract-out
  [hash->vector (-> hash? (vectorof pair?))]
  [hash-keys/vector (-> hash? vector?)]
  [hash-values/vector (-> hash? vector?)]
  [hash->immutable-hash (-> hash? immutable-hash?)]
  ))

(define (hash->vector htab)
  (for/vector #:length (hash-count htab) ([k+v (in-hash-pairs htab)]) k+v))

(define (hash-keys/vector htab)
  (for/vector #:length (hash-count htab) ([k (in-hash-keys htab)]) k))

(define (hash-values/vector htab)
  (for/vector #:length (hash-count htab) ([v (in-hash-values htab)]) v))

(define (hash->immutable-hash htab)
  (if (immutable? htab)
      htab
      (hash-map/copy htab values #:kind 'immutable)))

(define-syntax-parse-rule (do-hash-table (key:id value:id table:expr (~optional result:expr)) body:expr ...+)
  (block nil
         (for ([(key value) (in-hash table)]) body ...)
         (~? (let ([key '()] [value '()]) result))))

(module+ test
  (define htab '#hasheq((a . 1) (b . 2) (c . 3)))
  (check-equal? (vector-sort (hash->vector htab) symbol<? #:key car) '#((a . 1) (b . 2) (c . 3)))
  (check-equal? (vector-sort (hash-keys/vector htab) symbol<?) '#(a b c))
  (check-equal? (vector-sort (hash-values/vector htab) <) #(1 2 3))
  (check-eq? (hash->immutable-hash htab) htab)
  (define total 0)
  (check-equal? (do-hash-table (k v htab total) (set! total (+ total v))) 6)
  )
