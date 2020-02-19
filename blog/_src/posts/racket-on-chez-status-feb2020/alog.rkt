#lang racket/base
(require racket/list
         racket/date
         racket/draw
         racket/class
         pict)

(provide alog-plots)

(define scales '())
(define (learn-scale! as bs)
  (for ([a (in-list as)]
        [b (in-list bs)])
    (set! scales (cons (/ a b) scales)))
  as)
(define (use-scale bs)
  (define s (/ (apply + scales) (length scales)))
  (for/list ([b (in-list bs)])
    (* b s)))
  
(define alog2-times
  (list
   (list '(2018 12 6)
         '(245.54
           4.69
           21.16
           59.86
           93.34
           171.27
           95.77)
         '(508.48
           15.37
           37.51
           59.71
           150.3
           176.41
           143.08))
   (list '(2019 1 19)
         '(226.43
           4.53
           20.24
           59.76
           88.13
           150.26
           92.82)
         '(486.16
           14.57
           34.2
           63.13
           139.7
           173.36
           127.29))
   #;
   (list '(2019 1 27)
         '(224
           4.56
           20.98
           59.8
           92.59
           159.78
           97.79)
         '(509.96
           11.84
           35.48
           85.69
           142.56
           180.47
           134.55))
   (list '(2019 6 20) ; 7.3
         (learn-scale!
          '(231.18 ; original
            4.84
            20.68
            59.87
            91.16
            164.89
            95)
          '(229.03 ; newer
            4.51
            17.82
            46.05
            83.19
            158.38
            85.36))
         '(320.7
           6.66
           25.44
           60.57
           93.78
           185.25
           108.58))
   (list '(2019 10 18) ; 7.4
         (learn-scale!
          '(223.05 ; original
            4.37
            19.47
            46.73
            89.16
            159.05
            88.11)
          '(207.77 ; newer
            3.72
            16.49
            59.69
            71.97
            137.55
            75.10))
         (learn-scale!
          '(320.07 ; original
            6.7
            24.21
            58.6
            83.57
            150.12
            97.93)
          '(266.55 ; newer
            5.68
            20.68
            49.53
            77.72
            133.56
            85.77)))
   (list '(2020 02 14)
         (use-scale
          '(207.70
            3.72
            16.97
            59.52
            74.77
            130.26
            76.01))
         (use-scale
          '(234.99
            4.37
            16.71
            62.67
            66.75
            93.61
            73.39)))))

(define alog2-labels
  '("build"
    "df-test"
    "db-test"
    "db-upgrade-test"
    "trends-test"
    "aggregate-test"
    "fit-test"))

(define (stamp->seconds l)
  (find-seconds 0 0 0 (caddr l) (cadr l) (car l)))

(define (date->pict dt)
  (define mo
    (case (cadr dt)
      [(12) "Dec"]
      [(10) "Oct"]
      [(6) "Jun"]
      [(1) "Jan"]
      [(2) "Feb"]))
  (vc-append 1
             (text (format "~a" (car dt)) 'swiss 12)
             (text mo 'swiss 12)))

(define (plots)
  (define dot (filled-ellipse 20 20))
  (for/list ([label (in-list alog2-labels)]
             [i (in-naturals)])
    (define start-x (stamp->seconds (car (car alog2-times))))
    (define end-x (stamp->seconds (car (last alog2-times))))
    (define xs (for/list ([at (in-list alog2-times)])
                 (stamp->seconds (car at))))
    (define r-ys (for/list ([at (in-list alog2-times)])
                   (list-ref (cadr at) i)))
    (define rcs-ys (for/list ([at (in-list alog2-times)])
                     (list-ref (caddr at) i)))
    (define hi-y (* 1.1 (apply max (append r-ys rcs-ys))))
    (define H 250)
    (define W 400)
    (define-values (r rcs)
      (for/fold ([r null]
                 [rcs null])
                ([x (in-list xs)]
                 [r-y (in-list r-ys)]
                 [rcs-y (in-list rcs-ys)])
        (define dx (* (- x start-x) (/ W (- end-x start-x))))
        (values (cons (cons dx
                            (- H (* r-y (/ H hi-y))))
                      r)
                (cons (cons dx
                            (- H (* rcs-y (/ H hi-y))))
                      rcs))))
    (define p
      (dc (lambda (dc x y)
            (define p (send dc get-pen))
            (send dc draw-line x y x (+ y H))
            (send dc draw-line x (+ y H) (+ x W) (+ y H))
            (send dc set-pen (make-pen #:color "red" #:width 5))
            (send dc draw-lines r x y)
            (send dc set-pen (make-pen #:color "blue" #:width 5))
            (send dc draw-lines rcs x y)
            (send dc set-pen p))
          W H))
    (define q (for/fold ([p p]) ([x (in-list (reverse (map car r)))]
                                 [at (in-list alog2-times)])
                (define dt (car at))
                (define lbl (vc-append (vline 0 5) (date->pict dt)))
                (pin-over p
                          x H
                          (inset lbl (* -1/2 (pict-width lbl)) 0))))
    (define (add-label p lbl pts)
      (define left? (> (/ (abs (- (cdr (last r)) (cdr (last rcs)))) H) 0.03))
      (cond
        [left?
         (pin-over p
                   (- (car (last pts)) 5 (pict-width lbl)) (- (cdr (last pts)) 5)
                   lbl)]
        [else
         (pin-over p
                   (+ (car (first pts)) 5) (- (cdr (first pts)) 5)
                   lbl)]))
    (let* ([q (add-label q (colorize (text "R/BC" '(bold . swiss) 12) "red") r)]
           [q (add-label q (colorize (text "R/CS" '(bold . swiss) 12) "blue") rcs)]
           [q (rt-superimpose q (text label 'swiss 24))]
           [q (refocus (ht-append (text (format "~a s " (inexact->exact (round hi-y))) 'swiss 12)
                                  q)
                       q)])
      (inset q 45 0 35 40))))

(define (alog-plots)
  (define l (plots))
  (scale (table 2
                (cons (car l) (cons (blank) (cdr l)))
                cc-superimpose cc-superimpose
                12 12)
         0.6))

(module+ main
  (require slideshow)
  (for-each (lambda (p) (slide (scale p 2))) (plots)))
