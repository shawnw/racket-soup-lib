#lang racket/base

(require racket/contract racket/unsafe/ops)
(module+ test (require rackunit))

(provide
 (contract-out
  [string->vector (-> string? vector?)]
  [vector->string (-> (vectorof char?) string?)]
  [string-join/vector (->* ((vectorof string?)) (string?) string?)]))

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

(module+ test
  (check-equal? (string-join/vector #("a" "b" "c")) "a b c")
  (check-equal? (string-join/vector #("a" "longer" "series" "of strings" "to join" "together")) "a longer series of strings to join together")
  (check-equal? (string->vector "foobar") #(#\f #\o #\o #\b #\a #\r))
  (check-equal? (vector->string #(#\f #\o #\o #\b #\a #\r)) "foobar"))
