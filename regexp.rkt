#lang racket/base

(require (for-syntax racket/base racket/list syntax/parse) "control.rkt")
(provide register-groups-bind do-register-groups do-scans do-matches do-matches-as-strings)
(module+ test (require rackunit "list.rkt"))

;;; From Common Lisp's cl-ppcre

(begin-for-syntax
  (define-syntax-class rgb-binding
    #:attributes ((names 1) (transformers 1))
    (pattern #f #:attr (names 1) (list #'#f) #:attr (transformers 1) (list #'#f))
    (pattern var:id #:attr (names 1) (list #'var) #:attr (transformers 1) (list #'#f))
    (pattern (f:id (~and var (~or name:id #f)) ...+)
      #:attr (names 1) (syntax->list #'(var ...))
      #:attr (transformers 1) (make-list (length (syntax-e #'(var ...))) #'f))))

(define-syntax (register-groups-bind stx)
  (syntax-parse stx
    [(_ (bindings:rgb-binding ...)
        (regex:expr target:expr (~alt (~optional (~seq #:start start-pos:expr)) (~optional (~seq #:end end-pos:expr))) ...)
        body:expr ...+)
     (let ([vars (append* (map syntax->list (syntax->list #'((bindings.names ...) ...))))]
           [transformers (append* (map syntax->list (syntax->list #'((bindings.transformers ...) ...))))])
       #`(let ((match (regexp-match regex target (~? start-pos 0) (~? end-pos #f))))
         (when match
           (let #,(for/list ([i (in-naturals 1)]
                             [var (in-list vars)]
                             [transformer (in-list transformers)]
                             #:when (syntax-e var))
                    (if (syntax-e transformer)
                        #`(#,var (#,transformer (list-ref match #,i)))
                        #`(#,var (list-ref match #,i))))
             body ...))))]))

(define-syntax (do-register-groups stx)
  (syntax-parse stx
    [(_ (bindings:rgb-binding ...)
        (regex:expr target:expr (~alt (~optional result:expr) (~optional (~seq #:start start-pos:expr)) (~optional (~seq #:end end-pos:expr))) ...)
        body:expr ...+)
     (let ([vars (append* (map syntax->list (syntax->list #'((bindings.names ...) ...))))]
           [transformers (append* (map syntax->list (syntax->list #'((bindings.transformers ...) ...))))])
       #`(dolist (match (regexp-match* regex target (~? start-pos 0) (~? end-pos #f) #:match-select cdr) (~? result))
                 (let #,(for/list ([i (in-naturals 0)]
                                   [var (in-list vars)]
                                   [transformer (in-list transformers)]
                                   #:when (syntax-e var))
                          (if (syntax-e transformer)
                              #`(#,var (#,transformer (list-ref match #,i)))
                              #`(#,var (list-ref match #,i))))
                   body ...)))]))

(define-syntax (do-scans stx)
  (syntax-parse stx
    [(_ (match-start:id match-end:id reg-starts:id reg-ends:id regex:expr target:expr (~alt (~optional result:expr) (~optional (~seq #:start start-pos:expr)) (~optional (~seq #:end end-pos:expr))) ...)
        body:expr ...+)
     #'(dolist (match (regexp-match-positions* regex target (~? start-pos 0) (~? end-pos #f) #:match-select values) (~? result))
               (let ([nsubgroups (length (cdr match))])
                 (let ([match-start (caar match)]
                       [match-end (cdar match)]
                       [reg-starts (for/vector #:length nsubgroups ([group (in-list (cdr match))]) (if (pair? group) (car group) #f))]
                       [reg-ends (for/vector #:length nsubgroups ([group (in-list (cdr match))]) (if (pair? group) (cdr group) #f))])
                   body ...)))]))

(define-syntax (do-matches stx)
  (syntax-parse stx
    [(_ (match-start:id match-end:id regex:expr target:expr (~alt (~optional result:expr) (~optional (~seq #:start start-pos:expr)) (~optional (~seq #:end end-pos:expr))) ...)
        body:expr ...+)
     #'(dolist (match (regexp-match-positions* regex target (~? start-pos 0) (~? end-pos #f)) (~? result))
               (let ([match-start (car match)] [match-end (cdr match)])
                 body ...))]))

(define-syntax (do-matches-as-strings stx)
  (syntax-parse stx
    [(_ (var:id regex:expr target:expr (~alt (~optional result:expr) (~optional (~seq #:start start-pos:expr)) (~optional (~seq #:end end-pos:expr))) ...)
        body:expr ...+)
     #'(dolist (var (regexp-match* regex target (~? start-pos 0) (~? end-pos #f)) (~? result))
               body ...)]))

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

  (define (foo regex target-string #:start (start 0) #:end (end (string-length target-string)))
    (let ([sum 0])
      (do-matches (s e regex target-string #:start start #:end end (* 100 (/ sum (- end start))))
                  (set! sum (+ sum (- e s))))))
  (test-equal? "do-matches 1" (foo #rx"a" "abcabcabc") 100/3)
  (test-equal? "do-matches 2" (foo #rx"aa|b" "aacabcbbc") 500/9)

  (define (crossfoot s [start 0] [end (string-length s)])
    (let ([sum 0])
      (do-matches-as-strings (m #px"\\d" s #:start start #:end end)
                             (set! sum (+ sum (string->number m))))
      (if (< sum 10)
          sum
          (crossfoot (number->string sum)))))
  (test-equal? "do-matches-as-strings 1" (crossfoot "bar") 0)
  (test-equal? "do-matches-as-strings 2" (crossfoot "a3x") 3)
  (test-equal? "do-matches-as-strings 3" (crossfoot "12345") 6)

  (check-equal?
   (collecting
    (do-register-groups (first second third fourth)
                        (#rx"((a)|(b)|(c))" "abababc" #:start 2)
                        (collect (list first second third fourth))))
   '(("a" "a" #f #f)
     ("b" #f "b" #f)
     ("a" "a" #f #f)
     ("b" #f "b" #f)
     ("c" #f #f "c")))

  )
