#lang s-exp "lazy-racket.rkt"

(define (f a b)
  42)

(f (/ 1 0) (first '()))