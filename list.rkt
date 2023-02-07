#lang racket/base

(require racket/contract racket/function racket/list racket/sequence (only-in srfi/1 append-reverse) srfi/141)
(module+ test (require rackunit))
(provide
 (contract-out
  [lmin (->* ((and/c list? (not/c null?))) ((-> any/c any/c any/c)) any/c)]
  [lmax (->* ((and/c list? (not/c null?))) ((-> any/c any/c any/c)) any/c)]
  [chunk (-> list? exact-positive-integer? (listof list?))]
  [slice (-> list? exact-positive-integer? (listof list?))]
  [maplist (->i ([proc (lists) (and/c (unconstrained-domain-> any/c)
                                      (lambda (p) (procedure-arity-includes? p (+ (length lists) 1))))]
                 [list1 list?]) #:rest [lists (listof list?)] [_ list?])]
  [append-maplist (->i ([proc (lists) (and/c (unconstrained-domain-> list?)
                                             (lambda (p) (procedure-arity-includes? p (+ (length lists) 1))))]
                        [list1 list?]) #:rest [lists (listof list?)] [_ list?])]
  [adjoin (->* (any/c list?) (#:key (-> any/c any/c) #:test (-> any/c any/c any/c)) list?)]
  [pairlis (->i ([keys list?] [values list?]) ([alist (or/c (listof pair?) any/c)])
                #:pre/name (keys values) "lists must be equal length" (= (length keys) (length values))
                [_ (or/c (listof pair?) any/c)])]
  [sublis (->* ((listof pair?) any/c) (#:key (-> any/c any/c) #:test (-> any/c any/c any/c)) any/c)]
  [subst (->* (any/c any/c any/c) (#:key (-> any/c any/c) #:test (-> any/c any/c any/c)) any/c)]
  [subst-if (->* (any/c (-> any/c any/c) any/c) (#:key (-> any/c any/c)) any/c)]
  (tail? (-> any/c (or/c pair? null?) boolean?))
  [ldiff (-> (or/c pair? null?) any/c (or/c pair? null?))]
  ))

(define (any-null? lol) (ormap null? lol))
(define (cars lol) (map car lol))
(define (cdrs lol) (map cdr lol))

(define (maplist proc . lists)
  (if (= (length lists) 1)
      (maplist-1 proc (car lists)) ; Fast track
      (maplists proc lists)))

(define (maplist-1 proc list)
  (if (null? list)
      '()
      (cons (proc list) (maplist-1 proc (cdr list)))))

(define (maplists proc lists)
  (if (any-null? lists)
      '()
      (cons (apply proc lists) (maplists proc (cdrs lists)))))

(define (append-maplist proc . lists)
  (if (= (length lists) 1)
      (append* (maplist-1 proc (car lists))) ; Fast track
      (append* (maplists proc lists))))

(define (adjoin elem lst #:key [key identity] #:test [test eqv?])
  (cond
    ((null? list)
     (list elem))
    ((memf (lambda (x) (test elem (key x))) lst)
     lst)
    (else (cons elem lst))))

(define (pairlis keys values [alist '()])
  (foldl (lambda (key val alist) (cons (cons key val) alist)) alist keys values))

(define (sublis alist tree #:test [test eqv?] #:key [key identity])
  (cond
    ((assoc (key tree) alist test) => cdr)
    ((pair? tree)
     (let ([new-car (sublis alist (car tree) #:test test #:key key)]
           [new-cdr (sublis alist (cdr tree) #:test test #:key key)])
       (if (and (eqv? (car tree) new-car)
                (eqv? (cdr tree) new-cdr))
           tree
           (cons new-car new-cdr))))
    (else tree)))

(define (subst new old tree #:test [test eqv?] #:key [key identity])
  (cond
    ((test old (key tree)) new)
    ((pair? tree)
     (let ([new-car (subst new old (car tree) #:test test #:key key)]
           [new-cdr (subst new old (cdr tree) #:test test #:key key)])
       (if (and (eqv? (car tree) new-car)
                (eqv? (cdr tree) new-cdr))
           tree
           (cons new-car new-cdr))))
    (else tree)))

(define (subst-if new pred? tree #:key [key identity])
  (cond
    ((pred? (key tree)) new)
    ((pair? tree)
     (let ([new-car (subst-if new pred? (car tree) #:key key)]
           [new-cdr (subst-if new pred? (cdr tree) #:key key)])
       (if (and (eqv? (car tree) new-car)
                (eqv? (cdr tree) new-cdr))
           tree
           (cons new-car new-cdr))))
    (else tree)))
             
(define (tail? obj list)
  (let loop ([tail list])
    (cond
      ((eqv? obj tail) #t)
      ((pair? tail) (loop (cdr tail)))
      (else #f))))

(define (ldiff list obj)
  (let loop ([tail list]
             [res '()])
    (cond
      ((eqv? obj tail) (reverse res))
      ((pair? tail) (loop (cdr tail) (cons (car tail) res)))
      ((null? tail) (reverse res))
      (else (append-reverse res tail)))))

(define (lmin list [< <])
  (foldl (lambda (elem curr-min)
           (if (< elem curr-min)
               elem
               curr-min))
         (car list)
         (cdr list)))

(define (lmax list [< <])
  (foldl (lambda (elem curr-max)
           (if (< curr-max elem)
               elem
               curr-max))
         (car list)
         (cdr list)))

(define (slice list n)
  (for/list ([chunk (in-slice n list)]) chunk))

(define (chunk list n)
  (slice list (ceiling-quotient (length list) n)))

(module+ test
  (check-equal? (lmax '(1 2 3 4 5)) 5)
  (check-equal? (lmax '("a" "z" "b") string<?) "z")

  (check-equal? (lmin '(1 2 3 4 5)) 1)
  (check-equal? (lmin '("a" "z" "b") string<?) "a")

  (check-equal? (slice '(a b c d e f g) 3) '((a b c) (d e f) (g)))

  (check-equal? (chunk '(a b c d e f g) 2) '((a b c d) (e f g)))

  (check-equal? (maplist append '(1 2 3 4) '(1 2) '(1 2 3))
                '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))
  (check-equal? (maplist (lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
                '(0 0 1 0 1 1 1))

  (check-equal? (append-maplist list '(1 2 3 4))
                '((1 2 3 4) (2 3 4) (3 4) (4)))

  (check-equal? (adjoin 'x '(a b c)) '(x a b c))
  (check-equal? (adjoin 'b '(a b c)) '(a b c))
  (check-equal? (adjoin "CAT" '("DOG" "cat") #:key string-upcase #:test string=?) '("DOG" "cat"))
  (check-equal? (adjoin "cAt" '("DOG" "cat") #:test string-ci=?) '("DOG" "cat"))

  (check-equal? (pairlis '(1 2 3) '("one" "two" "three"))
                '((3 . "three") (2 . "two") (1 . "one")))
  (check-equal? (pairlis '(1 2 3) '("one" "two" "three") '((4 . "four")))
                '((3 . "three") (2 . "two") (1 . "one") (4 . "four")))

  (define tree1 '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))
  (define tree2 '("one" ("one" "two") (("one" "Two" "three"))))
  
  (check-equal?
   (sublis '((x . 100) (z . zprime))
           '(plus x (minus g z x p) 4 . x))
   '(plus 100 (minus g zprime 100 p) 4 . 100))
  (check-equal?
   (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
           '(* (/ (+ x y) (+ x p)) (- x y))
           #:test equal?)
   '(* (/ (- x y) (+ x p)) (+ x y)))
  (check-equal? (sublis '((3 . "three")) tree1)
                '(1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))
  (check-equal?  (sublis '((#t . "string"))
                         (sublis '((1 . "") (4 . 44)) tree1)
                         #:key string?)
                 '("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44)))))
  ; Got to make sure the search string isn't interned here
  (check-equal? (sublis `((,(string-copy "two") . 2)) tree2)
                '("one" ("one" "two") (("one" "Two" "three"))))
  (check-equal? (sublis `((,(string-copy "two") . 2)) tree2 #:test equal?)
                '("one" ("one" 2) (("one" "Two" "three"))))

  (define tree3 '(1 (1 2) (1 2 3) (1 2 3 4)))
  (check-equal? (subst "two" 2 tree3)
                '(1 (1 "two") (1 "two" 3) (1 "two" 3 4)))
  (check-equal? (subst "five" 5 tree3)
                '(1 (1 2) (1 2 3) (1 2 3 4)))
  (check-equal? (subst 'tempest 'hurricane
                       '(shakespeare wrote (the hurricane)))
                '(shakespeare wrote (the tempest)))
  (check-equal? (subst 'foo '() '(shakespeare wrote (twelfth night)))
                '(shakespeare wrote (twelfth night . foo) . foo))
  (check-equal? (subst '(a . cons) '(old . pair)
                       '((old . spice) ((old . shoes) old . pair) (old . pair))
                       #:test equal?)
                '((old . spice) ((old . shoes) a . cons) (a . cons)))

  (check-equal? (subst-if 5 list? tree1) 5)

  (define list1 '(a b c))
  (define list2 '(a b c . d))
  (check-true (tail? list1 list1))
  (check-true (tail? (cddr list1) list1))
  (check-false (tail? '(c) list1))
  (check-false (tail? '(f g h) list1))
  (check-true (tail? '() list1))
  (check-false (tail? 'd list1))
  (check-false (tail? 'x list1))
  (check-true (tail? list2 list2))
  (check-true (tail? (cddr list2) list2))
  (check-false (tail? (cons 'c 'd) list2))
  (check-false (tail? '(f g h) list2))
  (check-false (tail? '() list2))
  (check-true (tail? 'd list2))
  (check-false (tail? 'x list2))
  
  (check-equal? (ldiff list1 list1) '())
  (check-equal? (ldiff list1 (cddr list1)) '(a b))
  (check-equal? (ldiff list1 '(c)) '(a b c))
  (check-equal? (ldiff list1 '(f g h)) '(a b c))
  (check-equal? (ldiff list1 '()) '(a b c))
  (check-equal? (ldiff list1 'd) '(a b c))
  (check-equal? (ldiff list1 'x) '(a b c))
  (check-equal? (ldiff list2 list2) '())
  (check-equal? (ldiff list2 (cddr list2)) '(a b))
  (check-equal? (ldiff list2 (cons 'c 'd)) '(a b c . d))
  (check-equal? (ldiff list2 '(f g h)) '(a b c . d))
  (check-equal? (ldiff list2 '()) '(a b c . d))
  (check-equal? (ldiff list2 'd) '(a b c))
  (check-equal? (ldiff list2 'x) '(a b c . d))  
  )