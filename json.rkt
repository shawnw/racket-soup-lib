#lang racket/base

(require json racket/generic (for-syntax racket/base racket/syntax syntax/parse))
(module+ test (require rackunit))

(provide gen:struct->jsexpr ->jsexpr struct->jsexpr? json-match)

(define-generics struct->jsexpr
  [->jsexpr struct->jsexpr]
  #:defaults
  ([jsexpr?
    (define (->jsexpr js) js)]))

(begin-for-syntax
  (define-syntax-class json-type
    (pattern (~or* (~literal string) (~literal number) (~literal array) (~literal object) (~literal true) (~literal false) (~literal boolean) (~literal null)))))

(define-syntax (json-match stx)
  (define (compile-clauses jsexpr clauses-stx)
    (let loop ([clauses (syntax->list clauses-stx)]
               [string-clause #f] [number-clause #f] [array-clause #f] [object-clause #f]
               [true-clause #f] [false-clause #f] [boolean-clause #f] [null-clause #f])
      (if (null? clauses)
          (datum->syntax clauses-stx
                         (filter values (list string-clause number-clause array-clause object-clause true-clause
                                              false-clause boolean-clause null-clause)))
          (let ([clause (syntax->list (car clauses))])
            (case (syntax-e (car clause))
              ((string)
               (if string-clause
                   (raise-syntax-error 'json-match "duplicate clause" (car clause))
                   (loop (cdr clauses) #`((string? #,jsexpr) #,@(cdr clause)) number-clause array-clause object-clause true-clause false-clause
                         boolean-clause null-clause)))
              ((number)
               (if number-clause
                   (raise-syntax-error 'json-match "duplicate clause" (car clause))
                   (loop (cdr clauses) string-clause #`((real? #,jsexpr) #,@(cdr clause)) array-clause object-clause true-clause false-clause
                         boolean-clause null-clause)))
              ((array)
               (if array-clause
                   (raise-syntax-error 'json-match "duplicate clause" (car clause))
                   (loop (cdr clauses) string-clause number-clause #`((list? #,jsexpr) #,@(cdr clause)) object-clause true-clause false-clause
                         boolean-clause null-clause)))
              ((object)
               (if object-clause
                   (raise-syntax-error 'json-match "duplicate clause" (car clause))
                   (loop (cdr clauses) string-clause number-clause array-clause #`((hash? #,jsexpr) #,@(cdr clause)) true-clause false-clause
                         boolean-clause null-clause)))
              ((true)
               (cond
                 (true-clause
                  (raise-syntax-error 'json-match "duplicate clause" (car clause)))
                 (boolean-clause
                  (raise-syntax-error 'json-match "can't have both true and boolean clauses" (car clause)))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause #`((eq? #,jsexpr #t) #,@(cdr clause)) false-clause
                        boolean-clause null-clause))))
              ((false)
               (cond
                 (false-clause
                  (raise-syntax-error 'json-match "duplicate clause" (car clause)))
                 (boolean-clause
                  (raise-syntax-error 'json-match "can't have both false and boolean clauses" (car clause)))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause #`((eq? #,jsexpr #f) #,@(cdr clause))
                        boolean-clause null-clause))))
              ((boolean)
               (cond
                 (boolean-clause
                  (raise-syntax-error 'json-match "duplicate clause" (car clause)))
                 (true-clause
                  (raise-syntax-error 'json-match "can't have both true and boolean clauses" (car clause)))
                 (false-clause
                  (raise-syntax-error 'json-match "can't have both false and boolean clauses" (car clause)))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause false-clause
                        #`((boolean? #,jsexpr) #,@(cdr clause)) null-clause))))
              ((null)
               (if null-clause
                   (raise-syntax-error 'json-match "duplicate clause" (car clause))
                   (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause false-clause
                         boolean-clause #`((eq? #,jsexpr (json-null)) #,@(cdr clause)))))
              (else ; Shouldn't be reached
               (raise-syntax-error 'json-match "unknown clause" (car clause))))))))
  (syntax-parse stx
    #:literals (else)
    ((json-match
         (~optional (~seq #:unsafe unsafe:boolean) #:defaults [(unsafe #'#f)])
       jsexpr:expr
       (type:json-type ex:expr ...+) ...
       (~optional (else else-ex:expr ...+)))
     (with-syntax* ([js-id (datum->syntax #'jsexpr (generate-temporary 'jsexpr))]
                    [(clause ...) (compile-clauses #'js-id #'((type ex ...) ...))])
       #`(let ([js-id jsexpr])
           #,(unless (syntax-e #'unsafe)
                 #'(unless (jsexpr? js-id)
                     (raise-argument-error 'json-match "jsexpr?" js-id)))
           (cond clause ... (~? (else else-ex ...) (else (raise-arguments-error 'json-match "no matching clause for value" "jsexpr" js-id)))))))))

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
  (check-equal? (->jsexpr ex) (hasheq 'foo "apple" 'bar 1))

  (check-equal? (json-match "foo" (number 'num) (string 'str) (boolean 'bool) (else 'other)) 'str)
  (check-equal? (json-match #t (number 'num) (string 'str) (boolean 'bool) (else 'other)) 'bool)
  (check-equal? (json-match 1.2 (string 'str) (boolean 'bool) (else 'other)) 'other)
  )
