#lang racket

(require syntax/parse/define)
(provide let/comp lret lret*)

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
