#lang racket/base

(require racket/contract "for.rkt")
(module+ test (require rackunit))

(provide
 (contract-out
  [read-bytes-up-to (-> input-port? byte? (or/c bytes? eof-object?))]
  [read-string-up-to (-> input-port? char? (or/c string? eof-object?))]))

(define (read-bytes-up-to port delim)
  (if (eof-object? (peek-byte port))
      eof
      (for/bytes ([b (in-input-port-bytes port)]
                  #:break (= b delim))
        b)))

(define (read-string-up-to port delim)
  (if (eof-object? (peek-char port))
      eof
      (for/string ([c (in-input-port-chars port)]
                   #:break (char=? c delim))
        c)))


(module+ test
  (define input-bs #"abcd123")
  (check-equal? (read-bytes-up-to (open-input-bytes input-bs) (char->integer #\d)) #"abc"))
