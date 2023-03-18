#lang racket/base

(require (only-in srfi/235 [boolean make-boolean])
         (for-syntax racket/base syntax/parse))

(provide define-parameter define-boolean-parameter)

(define-syntax (define-parameter stx)
  (syntax-parse stx
    ((define-parameter name:id initial-value:expr)
     #'(define name (make-parameter initial-value)))
    ((define-parameter name:id initial-value:expr guard)
     #'(define name (make-parameter initial-value guard)))
    ((define-parameter name:id initial-value:expr guard obj-name:id)
     #'(define name (make-parameter initial-value guard obj-name)))))

(define-syntax (define-boolean-parameter stx)
  (syntax-parse stx
    ((define-boolean-parameter name:id)
     #'(define-parameter name #t make-boolean))
    ((define-boolean-parameter name:id default-value:boolean)
     #'(define-parameter name default-value make-boolean))
    ((define-boolean-parameter name:id default-value:boolean obj-name:id)
     #'(define-parameter name default-value make-boolean obj-name))))


