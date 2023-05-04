#lang typed/racket/base

(provide integer-log)

;;; Integer log function taken from Oleg's fast Haskell versions described
;;; at https://okmij.org/ftp/Haskell/AlgorithmsH1.html#basewidth
;;; and ported to Typed Racket by yours truely.

(struct basep ([bk : Integer] [k : Integer]) #:type-name BaseP)

(: mul-bp : BaseP BaseP -> BaseP)
(define (mul-bp bp1 bp2)
  (basep (* (basep-bk bp1) (basep-bk bp2))
         (+ (basep-k bp1) (basep-k bp2))))

(: integer-log : Integer Integer -> Integer)
(define (integer-log b n)
  (: major-bit : (Pairof BaseP (Listof BaseP)) -> Integer)
  (define (major-bit bases)
    (let* ([bp (car bases)]
           [bps (cdr bases)]
           [bk (basep-bk bp)]
           [k (basep-k bp)]
           [bpnext (mul-bp bp bp)]
           [bk2 (basep-bk bpnext)]
           [k2 (basep-k bpnext)])
      (cond
        ((= bk2 n) k2)
        ((> bk2 n) (other-bits k (quotient n bk) bps))
        (else (major-bit (cons bpnext bases))))))

  (: other-bits : Integer Integer (Listof BaseP) -> Integer)
  (define (other-bits i n bases)
    (if (null? bases)
        i
        (let* ([bphalf (car bases)]
               [bps (cdr bases)]
               [bk (basep-bk bphalf)]
               [k (basep-k bphalf)])
          (cond
            ((= bk n) (+ i k))
            ((> bk n) (other-bits i n bps))
            (else (other-bits (+ i k) (quotient n bk) bps))))))

  (cond
    ((<= b 1) (raise-argument-error 'basewidth-div "(>/c 1)" b))
    ((< n b) 0)
    ((= n b) 1)
    (else (major-bit (list (basep b 1))))))

