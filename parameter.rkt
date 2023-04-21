#lang racket/base

(require (only-in srfi/235 [boolean make-boolean])
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(provide define-parameter define-boolean-parameter parameterize*)

(define-syntax (define-parameter stx)
  (syntax-parse stx
    ((define-parameter name:id initial-value:expr)
     #'(define name (make-parameter initial-value #f (quote name))))
    ((define-parameter name:id initial-value:expr guard:expr)
     #'(define name (make-parameter initial-value guard (quote name))))
    ((define-parameter name:id initial-value:expr guard:expr obj-name:id)
     #'(define name (make-parameter initial-value guard obj-name)))))

(define-syntax (define-boolean-parameter stx)
  (syntax-parse stx
    ((define-boolean-parameter name:id)
     #'(define-parameter name #t make-boolean))
    ((define-boolean-parameter name:id initial-value:boolean)
     #'(define-parameter name initial-value make-boolean))
    ((define-boolean-parameter name:id initial-value:boolean obj-name:id)
     #'(define-parameter name initial-value make-boolean obj-name))))

(define-syntax (parameterize* stx)
  (syntax-parse stx
    ((_ () body:expr ...)
     #'(let () body ...))
    ((_ ((param:id val:expr) . rest-params) body:expr ...)
     #'(parameterize ((param val))
         (parameterize* rest-params body ...)))))

(module+ test
  (define-boolean-parameter foo)
  (check-equal? (foo) #t)
  (parameterize ([foo 'bar])
    (check-equal? (foo) #t))
  (parameterize ([foo #f])
    (check-equal? (foo) #f))
  (check-equal? (object-name foo) 'foo)


  (define-parameter a 2)
  (define-parameter b #f)
  (check-equal?
   (parameterize* ([a 4]
                   [b (* (a) 2)])
     (b))
   8))
