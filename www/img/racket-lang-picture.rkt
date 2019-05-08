#lang racket/gui

(provide
 (except-out (all-from-out slideshow/code) code)
 (rename-out (new-code code))
 
 ;; String Code ... -> Pict
 ;; ERROR if something goes wrong with writing the picture 
 racket-lang-picture)

;; -----------------------------------------------------------------------------
(require  slideshow slideshow/code)

(define WIDTH  500)
(define HEIGHT 500)
(define FSIZE   16)

(define BACKGROUND "white" #;"gray") 
(define SEP     20)

(current-font-size FSIZE)

(define (racket-lang-picture name . codes)
  (define codes-pict
    (let* ([b (apply vl-append SEP codes)]
           [s (filled-rectangle WIDTH HEIGHT #:color BACKGROUND)]
           [s (pin-over s 30 30 b)]
           [x (colorize (t "(a stand-in for graphics that Jay will propose)") "red")]
           [s (pin-over s 30 (- HEIGHT 30) x)])
      s))
  (define bm (pict->bitmap codes-pict))
  (or (and (send bm save-file name 'png 75 #:unscaled? #t)
           codes-pict)
      (error 'racket-lang-picture "something went wrong with writing ~a" name)))

(define-code new-code typeset-code unquote)

(module+ test
  (racket-lang-picture "racket-lang-picture.png"
                       (code
                        (define x 10)
                        (define y 20))
                       (code
                        (* x y))))
