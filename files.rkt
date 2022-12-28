#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [delete-file* (-> path-string? boolean?)]))

(define (delete-file* filename)
  (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
    (delete-file filename)
    #t))