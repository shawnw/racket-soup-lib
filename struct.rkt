#lang racket/base

(provide with-slots)
(require (for-syntax racket/base racket/list syntax/datum syntax/parse syntax/parse/class/struct-id
                     (only-in "control.rkt" if-let when-let)))

(begin-for-syntax
  (define-syntax-class slot-id
    (pattern (var:id name:id))
    (pattern name:id #:attr var #'name)))

(define-syntax (with-slots stx)
  (syntax-parse stx
    [(_ sid:struct-id (slot:slot-id ...+) instance:expr body:expr ...+)
     (when-let ([duplicate-id (check-duplicate-identifier (syntax->list #'(slot.name ...)))])
       (raise-syntax-error 'with-slots "duplicate field name" duplicate-id))
     (let* ([struct-fields (datum (sid.field-sym ...))]
            [slot-positions (for/list ([requested (in-list (syntax->list #'(slot.name ...)))])
                              (if-let ([pos (index-of struct-fields (syntax-e requested))])
                                      pos
                                      (raise-syntax-error 'with-slots "unknown field name" requested)))]
            [accessors (syntax->list #'(sid.accessor-id ...))]
            [mutators (syntax->list #'((~? sid.mutator-id #f) ...))]
            [getters (for/list ([pos (in-list slot-positions)]) (list-ref accessors pos))]
            [setters (for/list ([pos (in-list slot-positions)]) (list-ref mutators pos))]
            [make-transformer-stx
             (lambda (obj getter setter)
               #`(make-set!-transformer
                  (lambda (stx)
                    (syntax-parse stx #:literals (set!)
                      [(set! name:id val:expr)
                       #,(if (syntax-e setter) #`#'(#,setter #,obj val) #'(raise-syntax-error 'with-slots "not a mutable field" #'name))]
                      [_:id #'(#,getter #,obj)]))))])
       #`(let ((obj instance))
           (let-syntax #,(for/list ([name (in-list (syntax->list #'(slot.var ...)))]
                                    [getter (in-list getters)]
                                    [setter (in-list setters)])
                           (list name (make-transformer-stx #'obj getter setter)))
             body ...)))]))
