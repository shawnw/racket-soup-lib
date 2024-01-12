#lang racket

(require syntax/parse/define srfi/210
         (for-syntax racket/base racket/list))
(provide let/comp lret lret* named-let-values if-let when-let
         block return return-from do do* dotimes dolist prog prog*)

(define-syntax-parse-rule (let/comp (~optional (~seq #:prompt prompt-tag:expr)) k:id body:expr ...+)
  (call-with-composable-continuation
   (lambda (k) body ...)
   (~? prompt-tag)))

(define-syntax-parse-rule (lret ((name:id init:expr) ...) body:expr ...)
  (let ((name init) ...)
    body ...
    (values name ...)))

(define-syntax-parse-rule (lret* ((name:id init:expr) ...) body:expr ...)
  (let* ((name init) ...)
    body ...
    (values name ...)))

; Combine named let with let-values
(define-syntax (named-let-values stx)
  (syntax-parse stx
    [(_ name:id ([(var:id ...) producer:expr] ...) body:expr ...+)
     (let ([varnames (append* (map syntax->list (syntax->list #'((var ...) ...))))])
       #`(local [(define (name #,@varnames) body ...)]
           (call/mv name producer ...)))]))


(define *default-return-prompt* (make-continuation-prompt-tag))

(define-syntax-parse-rule (return (~optional result:expr))
  (abort-current-continuation *default-return-prompt* (lambda () (~? result (void)))))

(define %return-prompts% (make-parameter (hasheq 'nil *default-return-prompt*)))

(define nil '())

(define-syntax-parse-rule (return-from name:id (~optional result:expr))
  (abort-current-continuation
   (hash-ref (%return-prompts%) 'name (lambda () (raise-arguments-error 'return-from "no such block tag" "name" 'name)))
   (lambda () (~? result (void)))))

(define (hash-ref* htab key default-thunk)
  (if (hash-has-key? htab key)
      (values (hash-ref htab key) htab)
      (let ([new-value (default-thunk)])
        (values new-value (hash-set htab key new-value)))))

(define-syntax (block stx)
  (syntax-parse stx
    #:literals (nil)
    [(_ nil body:expr ...)
     #'(call-with-continuation-prompt
        (lambda () body ...)
        *default-return-prompt*
        (lambda (results) (results)))]
    [(_ name:id body:expr ...)
     #'(let-values ([(prompt-tag extended-prompts) (hash-ref* (%return-prompts%) 'name make-continuation-prompt-tag)])
         (parameterize ([%return-prompts% extended-prompts])
           (call-with-continuation-prompt
            (lambda () body ...)
            prompt-tag
            (lambda (results) (results)))))]))

(define-syntax-parse-rule (dotimes (var:id count:expr (~optional result:expr)) body:expr ...)
  (let ([max-val count])
    (unless (<= max-val 0)
      (block nil
             (let loop ([var 0])
               (if (= var max-val)
                   (~? result (void))
                   (let ()
                     body ...
                     (loop (+ var 1)))))))))

(define-syntax-parse-rule (dolist (var:id list-form:expr (~optional result:expr)) body:expr ...+)
  (block nil
         (for-each (lambda (var) body ...) list-form)
         (~? (let ([var '()]) result))))

(begin-for-syntax
  (define-syntax-class prog-var
    (pattern name:id #:attr init #'(void))
    (pattern (name:id) #:attr init #'(void))
    (pattern (name:id init:expr))))

(define-syntax-parse-rule (prog (var:prog-var ...) body:expr ...+)
  (block nil (void (let ([var.name var.init] ...) body ...))))

(define-syntax-parse-rule (prog* (var:prog-var ...) body:expr ...+)
  (block nil (void (let* ([var.name var.init] ...) body ...))))

(begin-for-syntax
  (define-syntax-class do-var
    (pattern name:id #:attr init #'(void) #:attr incr #'name)
    (pattern (name:id init:expr) #:attr incr #'name)
    (pattern (name:id init:expr incr:expr))))

(define-syntax-parse-rule (do (var:do-var ...) (end-test:expr (~optional result:expr)) body:expr ...)
  (block nil
         (let loop ([var.name var.init] ...)
           (cond
             (end-test
              (~? result (void)))
             (else
              body ...
              (loop var.incr ...))))))

(define-syntax-parse-rule (do* (var:do-var ...) (end-test:expr (~optional result:expr)) body:expr ...)
  (block nil
         (let* ([var.name var.init] ...)
           (let loop ([var.name var.name] ...)
             (cond
               (end-test
                (~? result (void)))
               (else
                body ...
                (let* ([var.name var.incr] ...)
                  (loop var.name ...))))))))

(define-syntax-parse-rule (if-let ((name:id value:expr) ...) true-case:expr false-case:expr)
  (let ((name value) ...)
    (if (and name ...)
        true-case
        false-case)))

(define-syntax-parse-rule (when-let ((name:id value:expr) ...) body:expr ...+)
  (let ((name value) ...)
    (when (and name ...)
      body ...)))

(module+ test
  (require rackunit)

  (define-syntax-parse-rule (check-equal-values? test-form:expr expected:expr ...+)
    (call-with-values
     (lambda () test-form)
     (lambda results
       (check-equal? results (list expected ...)))))

  (define (palindrome? str [start 0] [end (string-length str)])
    (dotimes (k (quotient (- end start) 2) #t)
             (unless (char-ci=? (string-ref str (+ start k)) (string-ref str (- end k 1)))
               (return #f))))

  (check-equal? (dotimes (temp-one 10 temp-one)) 10)
  (check-true (palindrome? "Able was I ere I saw Elba"))
  (check-equal-values? (block whocares (values 1 2) (values 3 4)) 3 4)
  (check-equal-values? (block early (return-from early (values 1 2)) (values 3 4)) 1 2)
  (check-equal? (block outer (block inner (return-from outer 1)) 2) 1)
  (check-equal? (block twin (block twin (return-from twin 1)) 2) 2)


  (check-equal? (do ((temp-one 1 (add1 temp-one))
                     (temp-two 0 (sub1 temp-two)))
                  ((> (- temp-one temp-two) 5) temp-one))
                4)

  (check-equal?  (do ((temp-one 1 (add1 temp-one))
                      (temp-two 0 (add1 temp-one)))
                   ((= 3 temp-two) temp-one))
                 3)

  (check-equal?  (do* ((temp-one 1 (add1 temp-one))
                      (temp-two 0 (add1 temp-one)))
                   ((= 3 temp-two) temp-one))
                 2)

  (define a 1)
  (check-equal? (prog ((a 2) (b a)) (return (if (= a b) '= '/=)))
                '/=)
  (check-equal? (prog* ((a 2) (b a)) (return (if (= a b) '= '/=)))
                '=)
  (check-equal? (prog () 'no-return-value) (void))
  (check-true (prog (x) (return (void? x))))


  (check-true (if-let ((a #t) (b #t)) b #f))
  (check-false (if-let ((a #t) (b #f)) #t b))
  (check-true (when-let ((a #t) (b #t)) 1 2 b))
  )
