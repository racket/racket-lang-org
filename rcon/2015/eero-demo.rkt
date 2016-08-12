#lang racket
(require math plot)
;; variations on the Gateway Arch
;; see https://en.wikipedia.org/wiki/Gateway_Arch#Mathematical_elements
(define fc 625.0925)
(define Qb 1262.6651)
(define Qt 125.1406)
(define L 299.2239)
(define A (/ fc (sub1 (/ Qb Qt))))
(define C (acosh (/ Qb Qt)))
(define (eero x) (* -1 A (sub1 (cosh (/ (* C x) L)))))
(define (normalized-eero w) (inverse (λ (x) (/ (eero (* w x)) w)) -1 1))
(define spread 700)
(parameterize ([plot-decorations? #f]
               [line-samples 100])
  (plot (for/list ([x (in-range (- spread) spread 6)])
          (parameterize ([line-color (random 128)]
                         [line-width (random)])
            (normalized-eero x)))
        #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1
        #:width 800 #:height 400))
