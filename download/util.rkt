#lang racket/base

(provide get-human-size)

(define (get-human-size size)
  (define mb (/ size (* 1024 1024)))
  (if (< mb 10)
      (let-values ([(q r) (quotient/remainder (round (* mb 10)) 10)])
        (format "~a.~aM" q r))
      (format "~aM" (round mb))))
