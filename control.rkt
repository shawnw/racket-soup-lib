#lang racket

(require syntax/parse/define srfi/210
         (for-syntax racket/base racket/list))
(provide let/comp lret lret* named-let-values)

(define-syntax-parse-rule (let/comp (~optional (~seq #:prompt prompt-tag:expr)) k:id body:expr ...+)
  (call-with-composable-continuation
   (lambda (k) body ...)
   (~? prompt-tag)))

(define-syntax-parse-rule (lret ((name:id init:expr) ...) body:expr ...)
  (let ((name init) ...)
    body ...
    (values name ...)))

(define-syntax-parse-rule (lret* ((name:id init:expr) ...) body:expr ...)
  (let* ((name init) ...)
    body ...
    (values name ...)))

; Combine named let with let-values
(define-syntax (named-let-values stx)
  (syntax-parse stx
    [(_ name:id ([(var:id ...) producer:expr] ...) body:expr ...+)
     (let ([varnames (append* (map syntax->list (syntax->list #'((var ...) ...))))])
       #`(local [(define (name #,@varnames) body ...)]
           (call/mv name producer ...)))]))