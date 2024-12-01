#lang racket/base

(require (for-syntax racket/base racket/list syntax/parse))
(provide register-groups-bind)
(module+ test (require rackunit))

;;; From Common Lisp's cl-ppcre

(begin-for-syntax
  (define-syntax-class rgb-binding
    #:attributes ((names 1) (transformers 1))
    (pattern #f #:attr (names 1) (list #'#f) #:attr (transformers 1) (list #'#f))
    (pattern var:id #:attr (names 1) (list #'var) #:attr (transformers 1) (list #'#f))
    (pattern (f:id (~and var (~or name:id #f)) ...+) #:attr (names 1) (syntax->list #'(var ...)) #:attr (transformers 1) (make-list (length (syntax-e #'(var ...))) #'f))))

(define-syntax (register-groups-bind stx)
  (syntax-parse stx
    [(_ (bindings:rgb-binding ...) (regex:expr target-string:expr (~alt (~optional (~seq #:start start-pos)) (~optional (~seq #:end end-pos))) ...) body:expr ...+)
     (let ([vars (append* (map syntax->list (syntax->list #'((bindings.names ...) ...))))]
           [transformers (append* (map syntax->list (syntax->list #'((bindings.transformers ...) ...))))])
       #`(let ((match (regexp-match regex target-string (~? start-pos 0) (~? end-pos #f))))
         (when match
           (let #,(for/list ([i (in-naturals 1)]
                             [var (in-list vars)]
                             [transformer (in-list transformers)]
                             #:unless (eq? (syntax-e var) #f))
                    (if (syntax-e transformer)
                        #`(#,var (#,transformer (list-ref match #,i)))
                        #`(#,var (list-ref match #,i))))
             body ...))))]))
                        

(module+ test
  (check-equal?
   (register-groups-bind (first second third fourth)
                             (#px"((a)|(b)|(c))+" "abababc" #:end 7 #:start 0)
                             (list first second third fourth))
   '("c" "a" "b" "c"))
    (check-equal?
     (register-groups-bind (first second #f fourth)
                             (#px"((a)|(b)|(c))+" "abababc")
                             (list first second fourth))
   '("c" "a" "c"))

  (check-equal?
   (register-groups-bind (first (string->number a b))
                         ("(.)(.)(.)" "a12")
                         (list first a b))
   '("a" 1 2))

  (check-equal?
   (register-groups-bind (first (string->number #f b))
                         ("(.)(.)(.)" "a12")
                         (list first b))
   '("a" 2))

  )
