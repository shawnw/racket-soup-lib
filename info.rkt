#lang info
(define collection "soup-lib")
(define deps '("base" "typed-racket-lib" "srfi-lite-lib" "extra-srfi-libs"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/soup-lib.scrbl" ())))
(define pkg-desc "Assorted useful routines")
(define version "0.0")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
