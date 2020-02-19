#lang racket/base
(require pict)

(provide (all-defined-out))

(define scheme-color "purple")
(define racket-color "blue")
(define c-color "red")
(define scrbl-color "forestgreen")

(define linklet-color "SlateBlue")

(define r6-color "tomato")
(define cify-color "firebrick")
(define cs/jit-color "RoyalBlue")
(define r-jit-color "tomato")
(define plain-c-color "brown")

(define dim-color "brown")

(define (background p c #:fade [fade 0.3] #:inset [ins-amt 5])
  (let ([p (inset p ins-amt)])
    (cc-superimpose (cellophane
                     (colorize (filled-rectangle (pict-width p) (pict-height p))
                               c)
                     fade)
                    p)))
