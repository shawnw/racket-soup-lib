#lang racket/base

(require racket/contract racket/dict racket/unsafe/ops racket/string racket/vector
         "for.rkt")
(module+ test (require rackunit))

(provide
 (contract-out
  [string->vector (-> string? vector?)]
  [vector->string (-> (vectorof char?) string?)]
  [string-join/vector (->* ((vectorof string?)) (string?) string?)]
  [string-sort (->* (string?) ((-> char? char? any/c)) string?)]
  [string-sort! (->* ((and/c string? (not/c immutable?))) ((-> char? char? any/c)) void?)]
  [string-escape (->* (string? (or/c dict? (-> char? (or/c string? #f)))) (exact-nonnegative-integer? exact-nonnegative-integer?) string?)]
  ))

; (-> (and/c string? (not/c immutable?)) fixnum? string? (values string? fixnum?))
(define (string-append! buffer end-idx s)
  (cond
    ((unsafe-fx<= (unsafe-fx+ end-idx (unsafe-string-length s)) (unsafe-string-length buffer))
     (string-copy! buffer end-idx s)
     (values buffer (unsafe-fx+ end-idx (unsafe-string-length s))))
    (else
     (let ([new-buffer (make-string (unsafe-fxquotient (unsafe-fx* (unsafe-fx+ (unsafe-string-length buffer) (unsafe-string-length s)) 3) 2))])
       (string-copy! new-buffer 0 buffer 0 end-idx)
       (string-copy! new-buffer end-idx s)
       (values new-buffer (unsafe-fx+ end-idx (unsafe-string-length s)))))))

(define (string-join/vector vs [sep " "])
  (for/fold ([buffer (make-string 32)]
             [end-idx 0]
             #:result (if (unsafe-fx< end-idx (unsafe-string-length buffer)) (substring buffer 0 end-idx) buffer))
            ([n (in-range (unsafe-vector-length vs))])
    (let-values ([(buffer end-idx) (if (unsafe-fx> n 0) (string-append! buffer end-idx sep) (values buffer end-idx))])
      (string-append! buffer end-idx (unsafe-vector-ref vs n)))))

(define (string->vector s)
  (build-vector (unsafe-string-length s) (lambda (i) (unsafe-string-ref s i))))

(define (vector->string vc)
  (build-string (unsafe-vector-length vc) (lambda (i) (unsafe-vector-ref vc i))))

(define (string-sort s [<? unsafe-char<?])
  (let ([v (string->vector s)])
    (vector-sort! v <?)
    (vector->string v)))

(define (string-sort! s [<? unsafe-char<?])
  (let ([v (string->vector s)])
    (vector-sort! v <?)
    (for ([i (in-range (unsafe-string-length s))])
      (unsafe-string-set! s i (unsafe-vector-ref v i)))))

(define (string-escape str escape-map [start 0] [stop (string-length str)])
  (if (procedure? escape-map)
      (for*/string ([ch (in-string str start stop)]
                    #:do [(define replacement (escape-map ch))]
                    [r (if (string? replacement) (in-string replacement) (in-value ch))])
        r)
      (string-escape str (lambda (ch) (dict-ref escape-map ch #f)) start stop)))

(module+ test
  (check-equal? (string-join/vector #("a" "b" "c")) "a b c")
  (check-equal? (string-join/vector #("a" "longer" "series" "of strings" "to join" "together")) "a longer series of strings to join together")
  (check-equal? (string->vector "foobar") #(#\f #\o #\o #\b #\a #\r))
  (check-equal? (vector->string #(#\f #\o #\o #\b #\a #\r)) "foobar")
  (check-equal? (string-sort "defab") "abdef")
  (define s (string-copy "defab"))
  (string-sort! s)
  (check-equal? s "abdef")
  (check-equal? (string-escape "foo\tbar" '((#\tab . "\\t"))) "foo\\tbar")
  (check-equal? (string-escape "foo\tbar" (lambda (ch) (if (char=? ch #\tab) "\\t" #f))) "foo\\tbar")
  )
