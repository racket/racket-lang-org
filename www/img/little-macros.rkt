#lang racket

(define-syntax-rule
  (swap x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

(let ((x 1) (y 2))
  (swap x y)
  (values x y))