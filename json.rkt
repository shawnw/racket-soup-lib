#lang racket/base

(require json racket/generic (for-syntax racket/base))
(module+ test (require rackunit))

(provide gen:struct->jsexpr ->jsexpr struct->jsexpr? json-match)

(define-generics struct->jsexpr
  [->jsexpr struct->jsexpr]
  #:defaults
  ([jsexpr?
    (define (->jsexpr js) js)]))


(define-syntax (json-match stx)
  (define (compile-clauses jsexpr clauses)
    (let loop ([clauses (syntax-e clauses)] [string-clause #f] [number-clause #f] [array-clause #f] [object-clause #f]
                                 [true-clause #f] [false-clause #f] [boolean-clause #f] [null-clause #f]
                                 [else-clause #f])
      (if (null? clauses)
          (append
           (filter values (list string-clause number-clause array-clause object-clause true-clause
                                false-clause boolean-clause null-clause))
           (if else-clause (list else-clause) #'((else (raise-arguments-error 'json-match "no matching clause for jsexpr value")))))
          (let ([clause (syntax-e (car clauses))])
            (case (syntax-e (car clause))
              ((string)
               (if string-clause
                   (raise-syntax-error 'json-match "duplicate string clause")
                   (loop (cdr clauses) (cons #`(string? #,jsexpr) (cdr clause)) number-clause array-clause object-clause true-clause false-clause
                         boolean-clause null-clause else-clause)))
              ((number)
               (if number-clause
                   (raise-syntax-error 'json-match "duplicate number clause")
                   (loop (cdr clauses) string-clause (cons #`(real? #,jsexpr) (cdr clause)) array-clause object-clause true-clause false-clause
                         boolean-clause null-clause else-clause)))
              ((array)
               (if array-clause
                   (raise-syntax-error 'json-match "duplicate array clause")
                   (loop (cdr clauses) string-clause number-clause (cons #`(list? #,jsexpr) (cdr clause)) object-clause true-clause false-clause
                         boolean-clause null-clause else-clause)))
              ((object)
               (if object-clause
                   (raise-syntax-error 'json-match "duplicate object clause")
                   (loop (cdr clauses) string-clause number-clause array-clause (cons #`(hash? #,jsexpr) (cdr clause)) true-clause false-clause
                         boolean-clause null-clause else-clause)))
              ((true)
               (cond
                 (true-clause
                  (raise-syntax-error 'json-match "duplicate true clause"))
                 (boolean-clause
                  (raise-syntax-error 'json-match "can't have true and boolean clauses"))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause (cons #`(eq? #,jsexpr #t) (cdr clause)) false-clause
                        boolean-clause null-clause else-clause))))
              ((false)
               (cond
                 (false-clause
                  (raise-syntax-error 'json-match "duplicate false clause"))
                 (boolean-clause
                  (raise-syntax-error 'json-match "can't have false and boolean clauses"))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause (cons #`(eq? #,jsexpr #f) (cdr clause))
                        boolean-clause null-clause else-clause))))
              ((boolean)
               (cond
                 (boolean-clause
                (raise-syntax-error 'json-match "duplicate boolean clause"))
                 (true-clause
                  (raise-syntax-error 'json-match "can't have true and boolean clauses"))
                 (false-clause
                  (raise-syntax-error 'json-match "can't have false and boolean clauses"))
                 (else
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause false-clause
                        (cons #`(boolean? #,jsexpr) (cdr clause)) null-clause else-clause))))
              ((null)
               (if null-clause
                   (raise-syntax-error 'json-match "duplicate object clause")
                   (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause false-clause
                         boolean-clause (cons #`(eq? #,jsexpr (json-null)) (cdr clause)) else-clause)))
              ((else)
               (cond
                 (else-clause
                  (raise-syntax-error 'json-match "duplicate else clause"))
                 ((null? (cdr clauses))
                  (loop (cdr clauses) string-clause number-clause array-clause object-clause true-clause false-clause
                        boolean-clause null-clause (car clauses)))
                 (else
                  (raise-syntax-error 'json-match "else must be the last clause"))))
              (else
               (raise-syntax-error 'json-match "unknown clause" (caar clauses))))))))
  (syntax-case stx (string number array object true false boolean null else)
    ((json-case jsexpr clause ...)
       (with-syntax ([(clause ...) (compile-clauses #'jsexpr #'(clause ...))])
         #'(cond clause ...)))))

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
  )

