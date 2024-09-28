#lang racket/base

(require syntax/parse/define racket/contract racket/fixnum racket/function racket/match racket/sequence racket/unsafe/ops
         (for-syntax racket/base syntax/for-body))
(module+ test (require rackunit))
(provide for/string for*/string for/bytes for*/bytes for/max for*/max for/min for*/min
         for/list/mv for*/list/mv for/count for*/count
         (contract-out
          ;; These contracts might be overkill
          [in-regexp-positions (->i ([re (or/c regexp? byte-regexp? string? bytes?)]
                                     [source (re) (if (or (regexp? re) (string? re))
                                                      string?
                                                      bytes?)])
                                    [_ sequence? #;(sequence/c (cons/c (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
                                                                       (listof (or/c (cons/c exact-integer? exact-integer?) #f))))])]
          [in-regexp-matches (->i ([re (or/c regexp? byte-regexp? string? bytes?)]
                                   [source (re) (if (or (regexp? re) (string? re))
                                                    string?
                                                    bytes?)])
                                  [_ (source)
                                     sequence?
                                     #;(if (string? source)
                                         (sequence/c (cons/c string? (listof (or/c string? #f))))
                                         (sequence/c (cons/c bytes? (listof (or/c bytes? f)))))])]
          [in-char-range (-> char? char? sequence? #;(sequence/c char?))]
          [in-conses (-> list? sequence? #;(sequence/c list?))]))

(define-syntax-parse-rule (for/string (~optional (~seq #:length slen:expr)) clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  #:with rlen #'(~? slen 32)
  (for/fold/derived original ([chars (make-string rlen)]
                              [len rlen]
                              [n 0]
                              #:result (if (unsafe-fx= n len) chars (substring chars 0 n)))
    clauses
    pre-body ...
    (let ([ch (let () post-body ...)])
      (cond
        ((char? ch)
         (cond
           ((unsafe-fx< n len)
            (unsafe-string-set! chars n ch)
            (values chars len (unsafe-fx+ n 1)))
           (else
            (let ([new-chars (make-string (fl->fx (* len 1.5)))])
              (string-copy! new-chars 0 chars)
              (unsafe-string-set! new-chars n ch)
              (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n 1))))))
        ((string? ch)
         (let ([xlen (unsafe-string-length ch)])
           (cond
             ((unsafe-fx< (unsafe-fx+ n xlen) len)
              (string-copy! chars n ch)
              (values chars len (unsafe-fx+ n xlen)))
             (else
              (let ([new-chars (make-string (unsafe-fx+ (fl->fx (* len 1.5)) xlen))])
                (string-copy! new-chars 0 chars)
                (string-copy! new-chars n ch)
                (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n xlen)))))))
        (else
         (raise-arguments-error 'for/string "body must return a string or character" "value" ch))))))


(define-syntax-parse-rule (for*/string (~optional (~seq #:length slen:expr)) clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  #:with rlen #'(~? slen 32)
  (for*/fold/derived original ([chars (make-string rlen)]
                               [len rlen]
                               [n 0]
                               #:result (if (unsafe-fx= n len) chars (substring chars 0 n)))
    clauses
    pre-body ...
    (let ([ch (let () post-body ...)])
      (cond
        ((char? ch)
         (cond
           ((unsafe-fx< n len)
            (unsafe-string-set! chars n ch)
            (values chars len (unsafe-fx+ n 1)))
           (else
            (let ([new-chars (make-string (fl->fx (* len 1.5)))])
              (string-copy! new-chars 0 chars)
              (unsafe-string-set! new-chars n ch)
              (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n 1))))))
        ((string? ch)
         (let ([xlen (unsafe-string-length ch)])
           (cond
             ((unsafe-fx< (unsafe-fx+ n xlen) len)
              (string-copy! chars n ch)
              (values chars len (unsafe-fx+ n xlen)))
             (else
              (let ([new-chars (make-string (unsafe-fx+ (fl->fx (* len 1.5)) xlen))])
                (string-copy! new-chars 0 chars)
                (string-copy! new-chars n ch)
                (values new-chars (unsafe-string-length new-chars) (unsafe-fx+ n xlen)))))))
        (else
         (raise-arguments-error 'for*/string "body must return a string or character" "value" ch))))))

(define-syntax-parse-rule (for/bytes (~optional (~seq #:length slen:expr)) clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  #:with rlen #'(~? slen 32)
  (for/fold/derived original ([bytes (make-bytes rlen)]
                              [len rlen]
                              [n 0]
                              #:result (if (unsafe-fx= n len) bytes (subbytes bytes 0 n)))
    clauses
    pre-body ...
    (let ([ch (let () post-body ...)])
      (unless (byte? ch)
        (raise-arguments-error 'for/bytes "body must return a byte" "value" ch))
      (if (unsafe-fx< n len)
          (begin
            (unsafe-bytes-set! bytes n ch)
            (values bytes len (unsafe-fx+ n 1)))
          (let ([new-bytes (make-bytes (fl->fx (* len 1.5)))])
            (bytes-copy! new-bytes 0 bytes)
            (unsafe-bytes-set! new-bytes n ch)
            (values new-bytes (unsafe-bytes-length new-bytes) (unsafe-fx+ n 1)))))))

(define-syntax-parse-rule (for*/bytes (~optional (~seq #:length slen:expr)) clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  #:with rlen #'(~? slen 32)
  (for*/fold/derived original ([bytes (make-bytes rlen)]
                               [len rlen]
                               [n 0]
                               #:result (if (unsafe-fx= n len) bytes (subbytes bytes 0 n)))
    clauses
    pre-body ...
    (let ([ch (let () post-body ...)])
      (unless (byte? ch)
        (raise-arguments-error 'for*/bytes "body must return a byte" "value" ch))
      (if (unsafe-fx< n len)
          (begin
            (unsafe-bytes-set! bytes n ch)
            (values bytes len (unsafe-fx+ n 1)))
          (let ([new-bytes (make-bytes (fl->fx (* len 1.5)))])
            (bytes-copy! new-bytes 0 butes)
            (unsafe-bytes-set! new-bytes n ch)
            (values new-bytes (unsafe-bytes-length new-chars) (unsafe-fx+ n 1)))))))

(define-syntax-parse-rule (for/max clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ([current-max -inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (> n current-max) n current-max)))

(define-syntax-parse-rule (for*/max clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ([current-max -inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (> n current-max) n current-max)))

(define-syntax-parse-rule (for/min clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original ([current-min +inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (< n current-min) n current-min)))

(define-syntax-parse-rule (for*/min clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original ([current-min +inf.0])
    clauses
    pre-body ...
    (define n (let () post-body ...))
    (if (< n current-min) n current-min)))

;;; difference lists
(define ((list->dlist list-a) list-b) (append list-a list-b))
(define (dlist-append f g) (compose1 f g))
(define (dlist->list dl) (dl '()))

(define-syntax-parse-rule (for/list/mv clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original
    ([result (list->dlist '())] #:result (dlist->list result))
    clauses
    pre-body ...
    (call-with-values
     (lambda () post-body ...)
     (lambda vals
       (dlist-append result (list->dlist vals))))))

(define-syntax-parse-rule (for*/list/mv clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original
    ([result (list->dlist '())] #:result (dlist->list result))
    clauses
    pre-body ...
    (call-with-values
     (lambda () post-body ...)
     (lambda vals
       (dlist-append result (list->dlist vals))))))

(define-syntax-parse-rule (for/count clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for/fold/derived original
    ([count 0])
    clauses
    pre-body ...
    (if (let () post-body ...)
        (add1 count)
        count)))

(define-syntax-parse-rule (for*/count clauses body ... tail-expr)
  #:with original this-syntax
  #:with ((pre-body ...) (post-body ...)) (split-for-body this-syntax #'(body ... tail-expr))
  (for*/fold/derived original
    ([count 0])
    clauses
    pre-body ...
    (if (let () post-body ...)
        (add1 count)
        count)))

(define (in-char-range start end)
  (make-do-sequence
   (thunk
    (initiate-sequence
     #:pos->element integer->char
     #:next-pos (if (char<=? start end) add1 sub1)
     #:init-pos (char->integer start)
     #:continue-after-pos+val? (lambda (pos val) (not (char=? val end)))))))

(define (in-conses list)
  (make-do-sequence
   (thunk
    (initiate-sequence
     #:pos->element values
     #:next-pos cdr
     #:init-pos list
     #:continue-with-pos? (negate null?)))))


(define (in-regexp-matches re s)
  (define (make-mapper sub)
    (lambda (positions)
      (map (lambda (pos)
             (match pos
               [(cons start end)
                (sub s start end)]
               [#f #f]))
           positions)))
  (sequence-map (make-mapper (if (string? s) substring subbytes)) (in-regexp-positions re s)))

(define (in-regexp-positions re s)
  (define current-position 0)
  (in-producer
   (lambda ()
     (match (regexp-match-positions re s current-position)
       [#f #f]
       [(and (cons (cons _ end-pos) _) positions)
        (set! current-position end-pos)
        positions]))
   #f))

(module+ test
  (check-equal? (for/string ([ch (in-char-range #\a #\d)]) ch) "abcd")
  (check-equal? (for/string #:length (string-length "abcd") ([ch (in-list '(#\a #\b #\c #\d))]) (char-upcase ch)) "ABCD")
  (check-equal? (for/string ([i (in-range 3)]) "AA") "AAAAAA")
  (check-equal? (for/bytes ([ch (in-char-range #\a #\d)]) (char->integer ch)) #"abcd")
  (check-equal? (for/bytes #:length 4 ([ch (in-char-range #\a #\d)]) (char->integer (char-upcase ch))) #"ABCD")

  (check-equal? (for/min ([n (in-range 1 10)]) (* n 2)) 2)
  (check-equal? (for/max ([n (in-range 1 11)]) (* n 2)) 20)

  (check-equal? (for/list/mv ([elem (in-list '(a b c))]
                              [pos (in-naturals 1)])
                  (values elem pos))
                '(a 1 b 2 c 3))

  (check-equal? (for/count ([i (in-range 1 11)]) (even? i)) 5)

  (check-equal? (for/list ([x (in-conses '(1 2 3))]) (cons 'foo x))
                '((foo 1 2 3) (foo 2 3) (foo 3)))

  (check-equal? (for/list ([m (in-regexp-matches  #rx".(..)" "foobar")]) m) '(("foo" "oo") ("bar" "ar")))
  (check-equal? (for/list ([m (in-regexp-matches  #rx#"...|(dog)" #"foobar")]) m) '((#"foo" #f) (#"bar" #f)))


  )
