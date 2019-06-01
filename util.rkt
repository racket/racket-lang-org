#lang racket
(require racket/runtime-path racket/draw)
(define-runtime-path img/ "www/img")
(provide img/size)
(define (img/size which #:scale [scale-factor 1])
  (define bmp (read-bitmap (build-path img/ which)))
  (define (->i n)
    (inexact->exact (round (* scale-factor n))))
  (define w (->i (send bmp get-width)))
  (define h (->i (send bmp get-width)))
  `(img ((src ,(~a "img/" which))
         (style ,(~a "image-width: " w "px;"
                     "image-height: " h "px")))))