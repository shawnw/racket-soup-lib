#lang racket/base

(require json racket/generic)
(module+ test (require rackunit))

(provide gen:struct->jsexpr ->jsexpr struct->jsexpr?)

(define-generics struct->jsexpr
  [->jsexpr struct->jsexpr]
  #:defaults
  ([jsexpr?
    (define (->jsexpr js) js)]))

(module+ test
  (check-equal? (->jsexpr (string-copy "foo")) "foo")
  (check-equal? (->jsexpr #t) #t)
  (check-equal? (->jsexpr (hasheq 'a 1 'b 2)) (hasheq 'a 1 'b 2))

  (struct example (foo bar)
    #:transparent
    #:methods gen:struct->jsexpr
    [(define (->jsexpr ex) (hasheq 'foo (example-foo ex) 'bar (example-bar ex)))])
  (define ex (example "apple" 1))
  (check-true (struct->jsexpr? ex))
  (check-equal? (->jsexpr ex) (hasheq 'foo "apple" 'bar 1)))
