#lang racket/base

(require racket/contract)
(module+ test (require rackunit racket/vector))

(provide
 (contract-out
  [hash->vector (-> hash? (vectorof pair?))]
  [hash-keys/vector (-> hash? vector?)]
  [hash-values/vector (-> hash? vector?)]
  [hash->immutable-hash (-> hash? (and/c hash? immutable?))]
  ))

(define (hash->vector htab)
  (for/vector #:length (hash-count htab) ([k+v (in-hash-pairs htab)]) k+v))

(define (hash-keys/vector htab)
  (for/vector #:length (hash-count htab) ([k (in-hash-keys htab)]) k))

(define (hash-values/vector htab)
  (for/vector #:length (hash-count htab) ([v (in-hash-values htab)]) v))

(define (base-hash-table htab)
  (cond
    ((hash-equal? htab) (hash))
    ((hash-eqv? htab) (hasheqv))
    ((hash-eq? htab) (hasheq))
    ((hash-equal-always? htab) (hashalw))))

(define (hash->immutable-hash htab)
  (if (immutable? htab)
      htab
      (for/fold ([new-htab (base-hash-table htab)])
                ([(k v) (in-hash htab)])
        (hash-set new-htab k v))))

(module+ test
  (define htab '#hasheq((a . 1) (b . 2) (c . 3)))
  (check-equal? (vector-sort (hash->vector htab) symbol<? #:key car) '#((a . 1) (b . 2) (c . 3)))
  (check-equal? (vector-sort (hash-keys/vector htab) symbol<?) '#(a b c))
  (check-equal? (vector-sort (hash-values/vector htab) <) #(1 2 3))
  (check-eq? (hash->immutable-hash htab) htab)

  )
