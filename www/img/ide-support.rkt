#lang typed/racket

(: d/dx (-> (Real -> Real) (Real -> Real)))
;; a numeric differentiation operator 
(define (d/dx f (ϵ : Real .001))

  (: fprime (Real -> Real))
  ;; the differentiated f
  (define (fprime x)
    (/ (- (f (+ x ϵ)) (f (- x ϵ)))
       (* 2 ϵ)))

  fprime)

((d/dx sin) (/ pi 2))