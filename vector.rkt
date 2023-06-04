#lang racket/base

(require racket/contract srfi/133)

(define (mutable-vector? v)
  (and (vector? v) (not (immutable? v))))

(provide
 (contract-out
  [vector-shuffle (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-shuffle! (->* ((and/c vector? (not/c immutable?))) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  ))


(define (vector-shuffle vec [start 0] [end (vector-length vec)])
  (let ([copy (vector-copy vec start end)])
    (vector-shuffle! copy start end)
    copy))

(define (vector-shuffle! vec [start 0] [end (vector-length vec)])
  (for ([i (in-inclusive-range (- end 1) start -1)])
    (vector-swap! vec i (random 0 (+ i 1)))))

