#lang racket/base

(require racket/contract srfi/133 srfi/160/fx racket/unsafe/ops
         (for-syntax racket/base))

(define (mutable-vector? v)
  (and (vector? v) (not (immutable? v))))

(provide
 (contract-out
  [vector-shuffle (->* (vector?) (exact-nonnegative-integer? exact-nonnegative-integer?) vector?)]
  [vector-shuffle! (->* ((and/c vector? (not/c immutable?))) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [fxvector-sort! (->* (fxvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) void?)]
  [fxvector-sort (->* (fxvector?) (exact-nonnegative-integer? exact-nonnegative-integer?) fxvector?)]
  ))

(define (vector-shuffle vec [start 0] [end (vector-length vec)])
  (let ([copy (vector-copy vec start end)])
    (vector-shuffle! copy start end)
    copy))

(define (vector-shuffle! vec [start 0] [end (vector-length vec)])
  (for ([i (in-inclusive-range (- end 1) start -1)])
    (vector-swap! vec i (random 0 (+ i 1)))))

(define (fxcompare-and-swap-less! fxv i j)
  (when (unsafe-fx< (unsafe-fxvector-ref fxv j) (unsafe-fxvector-ref fxv i))
      (fxvector-swap! fxv i j)))

(define (compare-and-swap-less! vec < i j)
  (when (< (vector-ref vec j) (vector-ref vec i))
      (vector-swap! vec i j)))

(define-syntax (make-sorting-network stx)
  (syntax-case stx ()
    ([_ fxv start . steps]
     #`(begin
         #,@(for*/list ([group (in-list (syntax->list #'steps))]
                        [swap (in-list (syntax->list group))])
              #`(fxcompare-and-swap-less! fxv (unsafe-fx+ start #,(car (syntax->list swap))) (unsafe-fx+ start #,(cadr (syntax->list swap)))))))))

(define (fxvector-partition! fxv pivot start end)
  (define elt (cdr pivot))
  (fxvector-swap! fxv (car pivot) (unsafe-fx- end 1))
  (let loop ([i (unsafe-fx- start 1)]
             [j start])
    (cond
      ((unsafe-fx= j (unsafe-fx- end 1))
       (let ([i (unsafe-fx+ i 1)])
         (fxvector-swap! fxv i (unsafe-fx- end 1))
         i))
      ((unsafe-fx<= (unsafe-fxvector-ref fxv j) elt)
       (let ([i (unsafe-fx+ i 1)])
         (fxvector-swap! fxv i j)
         (loop i (unsafe-fx+ j 1))))
      (else
       (loop i (unsafe-fx+ j 1))))))

(define (sort3! vec <)
  (compare-and-swap-less! vec < 0 2)
  (compare-and-swap-less! vec < 0 1)
  (compare-and-swap-less! vec < 1 2)
  vec)

; Sort a fxvector in ascending order, using a hybrid quicksort that uses fixed sorting networks for small spans
(define (fxvector-sort! fxv [start 0] [end (fxvector-length fxv)])
  (unless (or (unsafe-fx< start 0) (unsafe-fx>= start end))
    (case (unsafe-fx- end start)
      ((0 1) (void))
      ((2)
       (make-sorting-network fxv start [(0 1)]))
      ((3)
       (make-sorting-network fxv start [(0 2)] [(0 1)] [(1 2)]))
      ((4)
       (make-sorting-network fxv start [(0 2) (1 3)] [(0 1) (2 3)] [(1 2)]))
      ((5)
       (make-sorting-network fxv start [(0 3) (1 4)] [(0 2) (1 2)] [(0 1) (2 4)] [(1 2) (3 4)] [(2 3)]))
      (else
       ; Median-of-3 Quicksort
       (define middle (unsafe-fx+ start (unsafe-fxquotient (unsafe-fx- end start) 2)))
       (define median
         (sort3! (vector (cons start (unsafe-fxvector-ref fxv start))
                         (cons (unsafe-fx- end 1) (unsafe-fxvector-ref fxv (unsafe-fx- end 1)))
                         (cons middle (unsafe-fxvector-ref fxv middle)))
                 (lambda (a b) (unsafe-fx< (unsafe-cdr a) (unsafe-cdr b)))))
       (define split-idx (fxvector-partition! fxv (vector-ref median 1) start end))
       (fxvector-sort! fxv start split-idx)
       (fxvector-sort! fxv (unsafe-fx+ split-idx 1) end)))))

(define (fxvector-sort fxv [start 0] [end (fxvector-length fxv)])
  (let ([new-fxv (fxvector-copy fxv start end)])
    (fxvector-sort! new-fxv)
    new-fxv))

