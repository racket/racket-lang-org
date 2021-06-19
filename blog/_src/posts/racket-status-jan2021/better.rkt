#lang slideshow

(provide title/better)

(define (title/better content #:shorter-better? [shorter-better? #t])
  (let ([p (titlet content)])
    (refocus (hbl-append gap-size p (scale (t (format "(~a is better)" (if shorter-better?
                                                                           "shorter"
                                                                           "longer")))
                                           0.8))
             p)))
