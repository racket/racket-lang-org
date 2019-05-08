#lang racket/gui

(provide
 (except-out (all-from-out slideshow/code) code)
 (rename-out (new-code code))
 
 ;; String Code ... -> Pict
 ;; ERROR if something goes wrong with writing the picture 
 racket-lang-picture)

;; -----------------------------------------------------------------------------
(require  slideshow slideshow/code)

(current-font-size 18)

(define (racket-lang-picture name . codes)
  (define codes-pict 
    (cc-superimpose 
     (apply vl-append 10 codes)
     (blank 400 600)))
  (define bm (pict->bitmap codes-pict))
  (or (and (send bm save-file name 'png 75 #:unscaled? #t)
           codes-pict)
      (error 'racket-lang-picture "something went wrong with writing ~a" name)))

(define-code new-code typeset-code unquote)