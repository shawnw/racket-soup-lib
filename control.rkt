#lang racket

(require syntax/parse/define)
(provide let/comp)

(define-syntax-parse-rule (let/comp (~optional (~seq #:prompt prompt-tag:expr)) k:id body:expr ...+)
  (call-with-composable-continuation
   (lambda (k) body ...)
   (~? prompt-tag)))
