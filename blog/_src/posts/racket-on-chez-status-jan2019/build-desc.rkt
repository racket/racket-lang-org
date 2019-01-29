#lang racket/base
(require pict
         racket/math
         "color.rkt")

(provide describe-build-plot)

(define (describe-build-plot orig-p
                             #:t t
                             #:gap-size gap-size
                             #:max-time [max-time "2h11m"])
  (let* ([p orig-p]
         [p (refocus (vl-append
                      (colorize (t "compile") "orange")
                      p)
                     p)]
         [p (refocus (vr-append
                      (colorize (inset
                                 (vl-append -6
                                            (t "doc")
                                            (t "run"))
                                 0 0 (* (pict-width p) 0.23) 0)
                                "cyan")
                      p)
                     p)]
         [p (refocus (vr-append
                      (colorize (inset
                                 (vl-append -6
                                            (t "doc")
                                            (t "render"))
                                 0 0 (* (pict-width p) 0.05) 0)
                                "green")
                      p)
                     p)]
         [p (refocus (vr-append
                      (let ([s (colorize (t "re-render") "pink")])
                        (inset s 0 0 (- (* (pict-width p) 0.03) (pict-width s)) 0))
                      p)
                     p)]
         [arrow-len (* (pict-height orig-p) 1/3)]
         [arrow-sep (* gap-size 1.5)]
         [extend (lambda (p w h)
                   (inset p
                          0
                          0
                          (max 0 (- w (pict-width p)))
                          (max 0 (- h (pict-height p)))))]
         [p (refocus (hb-append
                      arrow-sep
                      (colorize
                       (let ([m (extend
                                 (inset (rotate (t "memory use") (/ pi 2))
                                        0 0 (/ gap-size 2) 0)
                                 0 arrow-len)])
                         (pin-arrow-line (/ gap-size 2)
                                         m
                                         m rb-find
                                         m rt-find
                                         #:line-width 3))
                       dim-color)
                      p)
                     p)]
         [p (refocus (vl-append
                      arrow-sep
                      p
                      (colorize
                       (let ([m (extend
                                 (inset (t "time")
                                        0 (/ gap-size 2) 0 0)
                                 arrow-len 0)])
                         (pin-arrow-line (/ gap-size 2)
                                         m
                                         m lt-find
                                         m rt-find
                                         #:line-width 3))
                       dim-color))
                     p)]
         [p (refocus (ht-append gap-size
                                (inset (colorize (scale (t "1.2 GB") 0.8) dim-color)
                                       0 (* (pict-height p) 0.15) 0 0)
                                p)
                     p)]
         [p (refocus (vr-append gap-size
                                p
                                (let ([s (colorize (scale (t max-time) 0.8) dim-color)])
                                  (inset s (- (/ (pict-width s) 2)) 0)))
                     p)]
         #;
         [p (refocus (table 2
                            (list (scale (tt "raco setup") 1.2) (blank)
                                  (blank) p)
                            cc-superimpose cc-superimpose
                            (* 2 gap-size) 0)
                     p)])
    p))
