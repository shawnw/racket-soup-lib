#lang racket/base

(require syntax/parse/define srfi/214
         (for-syntax racket/base syntax/for-body))
(module+ test (require rackunit))
(provide for/string for*/string)

(define-syntax-parse-rule (for/string clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ([chars (flexvector)]
                              #:result (flexvector->string chars))
    clauses
    pre-body ...
    (flexvector-add-back! chars (let () post-body ...))))

(define-syntax-parse-rule (for*/string clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ([chars (flexvector)]
                              #:result (flexvector->string chars))
    clauses
    pre-body ...
    (flexvector-add-back! chars (let () post-body ...))))

(define-syntax-parse-rule (for/max clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ([current-max -inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (> n current-max) n current-max)))

(define-syntax-parse-rule (for*/max clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ([current-max -inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (> n current-max) n current-max)))

(define-syntax-parse-rule (for/min clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ([current-min +inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (< n current-min) n current-min)))

(define-syntax-parse-rule (for*/min clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ([current-min +inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (< n current-min) n current-min)))

(module+ test
  (check-equal? (for/string ([ch (in-list '(#\a #\b #\c #\d))]) ch) "abcd")
  (check-equal? (for/min ([n (in-range 1 10)]) (* n 2)) 2)
  (check-equal? (for/max ([n (in-range 1 11)]) (* n 2)) 20))