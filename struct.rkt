#lang racket/base

(provide with-slots make-struct-type)
(require (for-syntax racket/base racket/list racket/syntax syntax/datum syntax/parse syntax/parse/class/struct-id syntax/transformer
                     (only-in "control.rkt" if-let when-let)))
(module+ test (require rackunit))

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
               #`(make-variable-like-transformer
                  #'(#,getter #,obj)
                  #,(if (syntax-e setter) #`#'(lambda (v) (#,setter #,obj v)) #f)))
             #;(lambda (obj getter setter)
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

(define (make-struct-type* #:name name #:init-field-count init-field-cnt #:super-type [super-type #f] #:auto-field-count [auto-field-cnt 0]
                           #:auto-v [auto-v #f] #:props [props '()] #:inspector [inspector (current-inspector)] #:proc-spec [proc-spec #f]
                           #:immutables [immutables '()] #:guard [guard #f] #:constructor-name [constructor-name #f])
  (make-struct-type name super-type init-field-cnt auto-field-cnt auto-v props inspector proc-spec immutables guard constructor-name))

(module+ test
  (struct example (foo bar [baz #:mutable]) #:transparent)
  (define demo (example 1 2 3))
  (with-slots example (foo (biff bar) baz) demo
    (check-eqv? foo 1)
    (check-eqv? biff 2)
    (check-eqv? baz 3)
    (set! baz 'b)
    (check-eqv? baz 'b)
    (check-equal? demo (example 1 2 'b))))
