#lang racket/base

(require racket/contract racket/control racket/function racket/undefined "list.rkt")

(module+ test (require rackunit))

(define traverse-type/c (or/c 'preorder 'postorder 'inorder))

(provide
 copy-tree tree-equal? subst subst-if ; from soup-lib/list
 (contract-out
  [map-tree (->* ((-> any/c any/c) any/c) (#:tag continuation-prompt-tag? #:traversal traverse-type/c) any/c)]
  [walk-tree (->* ((-> any/c any) any/c) (#:tag continuation-prompt-tag? #:traversal traverse-type/c) void?)]
  [leaf-map (-> (-> any/c any/c) any/c any/c)]
  [leaf-walk (-> (-> any/c any) any/c void?)]
  [occurs-if (->* ((-> any/c any/c) any/c) (#:traversal traverse-type/c #:key (-> any/c any/c)) (values any/c boolean?))]
  [occurs (->* (any/c any/c) (#:traversal traverse-type/c #:key (-> any/c any/c) #:test (-> any/c any/c any/c)) (values any/c boolean?))]
  ))

(define (map-tree fun tree #:tag [tag (default-continuation-prompt-tag)] #:traversal [traverse 'preorder])
  (case traverse
    ((preorder) (map-tree/preorder fun tree tag))
    ((postorder) (map-tree/postorder fun tree tag))
    ((inorder) (map-tree/inorder fun tree tag))))

(define (walk-tree fun tree #:tag [tag (default-continuation-prompt-tag)] #:traversal [traverse 'preorder])
  (void (map-tree (lambda (subtree)
              (fun subtree)
              subtree)
            tree
            #:tag tag
            #:traversal traverse)))

(define (map-tree/preorder fun tree prompt)
  (call-with-continuation-prompt
   (thunk
    (let ([tree2 (fun tree)])
      (if (pair? tree2)
          (reuse-cons (map-tree/preorder fun (car tree2) prompt)
                      (map-tree/preorder fun (cdr tree2) prompt)
                      tree2)
          tree2)))
   prompt
   identity))

(define (map-tree/postorder fun tree prompt)
    (call-with-continuation-prompt
     (thunk
      (if (pair? tree)
          (let* ([left (map-tree/postorder fun (car tree) prompt)]
                   [right (map-tree/postorder fun (cdr tree) prompt)]
                   [tree2 (reuse-cons left right tree)])
            (fun tree2))
          (fun tree)))
     prompt
     identity))

(define (map-tree/inorder fun tree prompt)
  (call-with-continuation-prompt
   (thunk
    (if (pair? tree)
        (let* ([left (map-tree/inorder fun (car tree) prompt)]
               [tree2 (map-tree/inorder fun (reuse-cons left (cdr tree) tree) prompt)])
          (reuse-cons (car tree2) (map-tree/inorder fun (cdr tree2) prompt) tree2))
        (fun tree)))
   prompt
   identity))

(define (leaf-walk fun tree)
  (walk-tree
   (lambda (node)
     (unless (or (pair? node) (null? node))
       (fun node)))
   tree))

(define (leaf-map fun tree)
  (map-tree
   (lambda (node)
     (if (or (pair? node) (null? node))
         node
         (fun node)))
   tree))

(define (occurs-if pred? tree #:key [key identity] #:traversal [traverse 'preorder])
  (let ([tree-prompt (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (thunk
      (walk-tree
       (lambda (subtree)
         (when (pred? (key subtree))
           (abort/cc tree-prompt subtree #t)))
       tree
       #:traversal traverse)
      (values undefined #f))
     tree-prompt
     values)))

(define (occurs node tree #:key [key identity] #:test [test? eqv?] #:traversal [traverse 'preorder])
  (occurs-if (curry test? node) tree #:key key #:traversal traverse))

(module+ test

  (define-syntax-rule (test-values-equal? name test expected ...)
    (test-equal? name (call-with-values
                       (thunk test)
                       list)
                 (list expected ...)))

  (test-equal? "leaf-map"
               (leaf-map (compose round sqrt) '(((4 1) 25) (9 100) 64))
               '(((2 1) 5) (3 10) 8))

  (test-equal? "map-tree"
               (map-tree (lambda (subtree)
                           (if (and (pair? subtree)
                                    (eq? (car subtree) 'skip-me))
                               (abort/cc (default-continuation-prompt-tag) 'skipped)
                               subtree))
                         '((a (b) (c (skip-me d (e f))))))
               '((a (b) (c skipped))))


  (test-values-equal? "occurs found" (occurs 'b '((a (b) (c d)))) 'b #t)
  (test-values-equal? "occurs not found" (occurs 'q '((a (b) (c d)))) undefined #f)
  
  )
