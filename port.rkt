#lang racket/base

(require racket/contract racket/port syntax/parse/define "for.rkt"
         (for-syntax racket/base syntax/parse))
(module+ test (require rackunit))

(provide
 with-input-file with-output-file
 with-null-input with-null-output
 with-input with-output
 (contract-out
  [read-bytes-up-to (-> input-port? byte? (or/c bytes? eof-object?))]
  [read-string-up-to (-> input-port? char? (or/c string? eof-object?))]
  [call-with-input (->* ((or/c input-port? string? bytes? path? boolean?) (-> input-port? any)) (#:mode (or/c 'binary 'text)) any)]
  [call-with-output (->* ((or/c output-port? path-string? boolean?) (-> output-port? any))
                         (#:mode (or/c 'binary 'text)
                          #:exists symbol?
                          #:permissions (integer-in 0 65535)
                          #:replace-permissions? any/c)
                         any)]
  [call-with-null-input (-> (-> input-port? any) any)]
  [call-with-null-output (-> (-> output-port? any) any)]
  [with-input-from-null (-> (-> any) any)]
  [with-output-to-null (-> (-> any) any)]))

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

(define (call-with-null-input proc)
  (call-with-input-bytes #"" proc))

(define (call-with-null-output proc)
  (proc (open-output-nowhere)))

(define (with-input-from-null thunk)
  (parameterize ([current-input-port (open-input-bytes #"")])
    (thunk)))

(define (with-output-to-null thunk)
  (parameterize ([current-output-port (open-output-nowhere)])
    (thunk)))

(define (call-with-input input proc #:mode [mode 'binary])
  (cond
    [(input-port? input) (proc input)]
    [(string? input) (call-with-input-string input proc)]
    [(bytes? input) (call-with-input-bytes input proc)]
    [(path? input) (call-with-input-file input proc #:mode mode)]
    [(boolean? input) (proc (current-input-port))]
    [else (raise-argument-error 'call-with-input "(or/c input-port? string? bytes? path? boolean?)" input)]))

(define (call-with-output output proc
                          #:mode [mode 'binary]
                          #:exists [exists 'error]
                          #:permissions [permissions #o666]
                          #:replace-permissions? [replace-permissions? #f])
  (cond
    [(output-port? output) (proc output)]
    [(path-string? output) (call-with-output-file output proc #:mode mode #:exists exists #:permissions permissions #:replace-permissions? replace-permissions?)]
    [(eq? output #t) (proc (current-output-port))]
    [(eq? output #f) (call-with-output-string proc)]
    [else (raise-argument-error 'call-with-output "(or/c output-port? path-string? boolean?)" output)]))

(define-syntax-parse-rule (with-input-file (var:id pathname:expr (~optional (~seq #:mode mode:expr))) body:expr ...+)
  (call-with-input-file pathname (lambda (var) body ...) #:mode (~? mode 'binary)))

(define-syntax-parse-rule (with-output-file (var:id pathname:expr (~and (~seq (~seq kw:keyword val:expr) ...)
                                                                        (~seq kw-args ...)))
                            body:expr ...+)
  (call-with-output-file pathname (lambda (var) body ...) kw-args ...))

(define-syntax-parse-rule (with-null-input (var:id (~optional (~seq #:mode mode:expr))) body:expr ...+)
  (call-with-null-input (lambda (var) body ...)))

(define-syntax-parse-rule (with-null-output (var:id (~and (~seq (~seq kw:keyword val:expr) ...)
                                                          (~seq kw-args ...)))
                            body:expr ...+)
  (call-with-null-output (lambda (var) body ...)))


;;; Special case literal input values
(define-syntax (with-input stx)
  (syntax-parse stx
    [(with-input (var:id input:string (~optional (~seq #:mode mode:expr))) body:expr ...+)
     #'(call-with-input-string input (lambda (var) body ...))]
    [(with-input (var:id input:bytes (~optional (~seq #:mode mode:expr))) body:expr ...+)
     #'(call-with-input-bytes input (lambda (var) body ...))]
    [(with-input (var:id input:boolean (~optional (~seq #:mode mode:expr))) body:expr ...+)
     #'((lambda (var) body ...) (current-input-port))]
    [(with-input (var:id input:expr (~optional (~seq #:mode mode:expr))) body:expr ...+)
     #'(call-with-input input (lambda (var) body ...) #:mode (~? mode 'binary))]))

;;; Special case literal output values
(define-syntax (with-output stx)
  (syntax-parse stx
    [(with-output (var:id #t (~and (~seq (~seq kw:keyword val:expr) ...)
                                   (~seq kw-args ...))) body:expr ...+)
     #'((lambda (var) body ...) (current-output-port))]
    [(with-output (var:id #f (~and (~seq (~seq kw:keyword val:expr) ...)
                                   (~seq kw-args ...))) body:expr ...+)
     #'(call-with-output-string (lambda (var) body ...))]
    [(with-output (var:id output:string (~and (~seq (~seq kw:keyword val:expr) ...)
                                              (~seq kw-args ...))) body:expr ...+)
     #'(call-with-output-file output (lambda (var) body ...) kw-args ...)]
    [(with-output (var:id output:expr (~and (~seq (~seq kw:keyword val:expr) ...)
                                            (~seq kw-args ...)))
       body:expr ...+)
     #'(call-with-output output (lambda (var) body ...) kw-args ...)]))

(module+ test
  (define input-bs #"abcd123")
  (check-equal? (read-bytes-up-to (open-input-bytes input-bs) (char->integer #\d)) #"abc")
  (check-equal? (with-input (in "text\n") (read-line in)) "text")
  (check-equal? (with-input (in #"text\n") (read-line in)) "text")
  (check-equal? (with-input (in input-bs) (read-line in)) "abcd123")
  (check-equal? (with-output (out #f) (display "text" out)) "text")
  (check-equal? (with-output (out (if #t #f #t) #:exists 'replace) (display "text" out)) "text")
  (check-equal? (call-with-null-input read) eof)
  )
