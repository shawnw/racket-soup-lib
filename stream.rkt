#lang racket/base

(require racket/contract racket/stream)
(module+ test (require rackunit))

(provide
 (contract-out
  [stream-filter-map (-> (-> any/c any/c) stream? stream?)]
  [stream-append-map (-> (-> any/c stream?) stream? stream?)]
  ))

(define (stream-filter-map f stream)
  (cond
    [(stream-empty? stream) empty-stream]
    [(f (stream-first stream)) => (lambda (val) (stream-cons #:eager val (stream-filter-map f (stream-rest stream))))]
    [else (stream-filter-map f (stream-rest stream))]))

(define (stream-append-map f stream)
  (for*/stream ([elem (in-stream stream)]
                [new-elem (in-stream (f elem))])
    new-elem))

(module+ test
  (check-equal? (stream->list (stream-take (stream-filter-map (lambda (n) (if (odd? n) (* n n) #f)) (in-naturals)) 5))
                '(1 9 25 49 81))

  (check-equal? (stream->list (stream-append-map (lambda (x) (stream x (* x x))) (stream 1 2 3)))
                '(1 1 2 4 3 9))

  )