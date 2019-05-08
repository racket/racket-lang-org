#lang racket/gui

(require "racket-lang-picture.rkt")

(define a-macro
  (code
    (define-syntax-rule
      (swap x y)
      (let ((tmp x))
        (set! x y)
        (set! y tmp)))))

(define a-use
  (code
   (let ((x 1) (y 2))
     (swap x y)
     (values x y))))

(racket-lang-picture "little-macros.png" a-macro a-use)