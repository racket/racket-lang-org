#lang racket
(require math plot file/gzip)
;; variations on the Gateway Arch
;; see https://en.wikipedia.org/wiki/Gateway_Arch#Mathematical_elements
(let* ([fc 625.0925]
       [Qb 1262.6651]
       [Qt 125.1406]
       [L 299.2239]
       [A (/ fc (sub1 (/ Qb Qt)))]
       [C (acosh (/ Qb Qt))]
       [eero (λ(x) (* -1 A (sub1 (cosh (/ (* C x) L)))))]
       [normalized-eero (λ(w) (inverse (λ(x) (/ (eero (* w x)) w)) -1 1))]
       [hsize 800] [vsize 400] [spread 700])
  (plot-decorations? #f)
  (line-samples 100)
  (define op (open-output-string))
  (plot (map (λ(x)
               (parameterize ([line-color (inexact->exact (floor (* 128 (random))))]
                              [line-width (random)])
                 (normalized-eero x))) (range (* -1 spread) spread 6))
        #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1
        #:width hsize #:height vsize))