#lang racket
(require racket/runtime-path racket/draw)
(define-runtime-path img/ "img")
(provide
 (contract-out
  [img/size
   (->* (string? #:alt string?)
        (#:scale real? #:size (cons/c natural? natural?) #:class (or/c #f string?))
        any/c)]))

(define (img/size which #:alt alt #:scale [scale-factor 1] #:size [w/h #f]
                  #:class [img-class #f])
  (define bmp (read-bitmap (build-path img/ which)))
  (define (->i n)
    (inexact->exact (round (* scale-factor n))))
  (define-values (w h)
    (if w/h
        (values (car w/h) (cdr w/h))
        (values (->i (send bmp get-width))
                (->i (send bmp get-height)))))
  `(img ((src ,(~a "img/" which))
         (alt ,alt)
         (width ,(~a w))
         (height ,(~a h))
         ,@(if img-class
               (list `(class ,img-class))
               (list)))))
