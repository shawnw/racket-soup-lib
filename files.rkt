#lang racket/base

(require racket/contract racket/function racket/path "tree.rkt")

(provide
 (contract-out
  [delete-file* (-> path-string? boolean?)]
  [directory-tree (->* (path-string?) (#:follow-links? any/c)
                       (listof (flat-rec-contract entry path? (cons/c path? (listof entry))))
                       )]))

(define (delete-file* filename)
  (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
    (delete-file filename)
    #t))

(define (directory-tree dirname #:follow-links? [follow-links? #t])
  (unless (directory-exists? dirname)
    (raise-argument-error 'directory-tree "directory-exists?" dirname))
  (build-directory-tree (simple-form-path dirname) follow-links? (make-hasheqv)))

(define (build-directory-tree dirname follow-links? seen)
  (define (process-directory)
    (let* ([nodeid (file-or-directory-identity dirname)]
           [existing-tree (hash-ref seen nodeid #f)])
      (if existing-tree
          (cons dirname (leaf-map (curry build-path dirname) existing-tree))
          (let ([new-tree (map (curryr build-directory-tree follow-links? seen) (directory-list dirname #:build? #t))])
            (hash-set! seen nodeid (leaf-map (curryr shrink-path-wrt (list dirname)) new-tree))
            (cons dirname new-tree)))))
  (case (file-or-directory-type dirname)
    ((file link) dirname)
    ((directory) (process-directory))
    ((directory-link)
     (if follow-links?
         (process-directory)
         dirname))))
