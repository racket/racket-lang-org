#lang racket/base
(require racket/runtime-path
         racket/include)

(define-runtime-path log-cs-orig-txt "bmtimes-jan20/log-cs0.txt")
(define-runtime-path log-cs-txt "bmtimes-jan20/log-cs.txt")
(define-runtime-path log-rcs-txt "bmtimes-jan20/log-rcs.txt")
(define-runtime-path log-r-txt "bmtimes-jan20/log-r.txt")

(define-runtime-path sho-rcs-txt "bmtimes-jan20/sho-rcs.txt")
(define-runtime-path sho-r-txt "bmtimes-jan20/sho-r.txt")

;; Not updated:

(define cs-load-time (+ 352 56))
(define r-load-time (+ 258 32))
(define r/all-load-time (+ 407 44))

(define cs-compile-time (+ 43682 392))
(define r-compile-time (+ 31843 644))
(define r/all-compile-time (+ 31274 760))

(define cs-base 44068)
(define r-base (+ 4410 1580))
(define drmax-mem 883000)
(define cs-dr-mem (- 770195 cs-base))
(define r-dr-mem (- (+ 322012 32028 (+ 69546 6072)) r-base))
(define r-all-dr-mem (- (+ 377443 32100 (+ 121990 6824)) r-base))
(define r-jit-dr-mem (- (+ 369162 189880 (+ 133636 74600)) r-base))  

(define r-build-time (+ (* 1 60) 3))
(define cs-build-time (+ (* 1 60) 19))

(define cs-build-mem 1270774)
(define r-build-mem 1053808)

(define Racket-abbrev "R/BC")

(include "bm-x.rktl")

(module+ slide
  (require "bm.rkt")
  (provide october-benchmark-slides)

  (define traditional-cs (get-times log-cs-txt))
  (define traditional-r/cs (get-times log-rcs-txt))
  (define traditional-r (get-times log-r-txt))
  (define shootout-r/cs (get-times sho-rcs-txt))
  (define shootout-r (get-times sho-r-txt))

  (define (october-benchmark-slides)
    (benchmark-slides #:traditional-cs traditional-cs
                      #:traditional-r/cs traditional-r/cs
                      #:traditional-r traditional-r
                      #:shootout-r/cs shootout-r/cs
                      #:shootout-r shootout-r
                      #:racket-cs-label "Racket on Chez Scheme")))
(module+ main
  (slide
   (vl-append
    (* 2 gap-size)
    (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                            #:width 500
                            #:label (t "Traditional Scheme benchmarks")
                            #:alt-cs? #f)
    (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                         #:width 500
                         #:label (t "Shootout Racket benchmarks")))))
(module+ pict
  (provide traditional-benchmarks-table*
           shootout-benchmarks-table*

           one-plot
           compile-time-plot
           compile-time-detail-plot
           recompile-time-plot
           general-plot
           build-desc)
  (define (traditional-benchmarks-table*)
    (scale (traditional-benchmarks-table)1.5))
  (define (shootout-benchmarks-table*)
    (scale (shootout-benchmarks-table) 1.5))

  (define plot-width 700)
  (define plot-height 300)

  (define (add-desc desc p)
    (rt-superimpose
     (hbl-append (scale (it desc) 0.75) (t ""))
     p))

  (define (general-plot labels
                        colors
                        tables
                        #:vertical-bars? [vertical-bars? #f]
                        #:desc [desc ""]
                        #:suffix [suffix ""])
    (scale
     (add-desc
      desc
      (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep
            #:columns 1
            #:width (* 1/2 plot-width)
            #:vertical-bars? vertical-bars?
            #:suffix suffix
            (map t labels) colors tables))
     (* 2 scale-amt)))

  (define (build-desc #:base [base (blank) #;(bitmap build-rcs-png)]
                      #:max-time [max-time "2h32m"]
                      #:max-memory [max-memory "1.2 GB"])
    (scale (inset (describe-build-plot base
                                       #:t t
                                       #:gap-size gap-size
                                       #:max-time max-time
                                       #:max-memory max-memory)
                  (* gap-size 4) (* gap-size 2) (* gap-size 6) (* gap-size 4))
           0.5))

  (define (add-diff ht)
    (hash-set ht 'compile-linklet-minus-regalloc
              (- (hash-ref ht 'compile-linklet)
                 (hash-ref ht 'regalloc))))

  (define r/cs-compile
    (hash '|racket -cl racket|
          (+ 40282 264) ; user + sys
          '|racket -cl racket/base|
          (+ 3583 48)))
  (define r/cs-compile-detail ; may
    (hash '|racket -cl racket|
          (add-diff
           (hash 'expand 22446
                 'compile 26665
                 'schemify 2250
                 'compile-linklet 23419
                 'regalloc 8007
                 'read 857))
          '|racket -cl racket/base|
          (add-diff
           (hash 'expand 1966
                 'compile 3088 ; includes 'schemify and 'compile-linklet
                 'schemify 282
                 'compile-linklet 2673
                 'regalloc 877 ; part of 'compile-linklet
                 'read 131))))
  (define r/cs-recompile
    (hash '|racket -Ml racket|
          (+ 19398 304)
          '|racket -Ml racket/base|
          (+ 2555 80)))
  (define r-compile ; may
    (hash '|racket -cl racket|
          (+ 30972 700)
          '|racket -cl racket/base|
          (+ 2798 72)))
  (define r-compile-detail ; may
    (hash '|racket -cl racket|
          (add-diff
           (hash 'expand 22043
                 'compile 3580
                 'compile-linklet 2709
                 'regalloc 0
                 'read 1266
                 'jit 362))
          '|racket -cl racket/base|
          (add-diff
           (hash 'expand 1569
                 'compile 341
                 'compile-linklet 255 ; i.e., compile from linklet 
                 'regalloc 0
                 'read 205
                 'jit 93))))
  (define r-recompile
    (hash '|racket -Ml racket|
          (+ 1829 88)
          '|racket -Ml racket/base|
          (+ 296 32)))
  (define r-jit-compile ; may
    (hash '|racket -cl racket|
          (+ 35464 724)
          '|racket -cl racket/base|
          (+ 3252 88)))
  (define r-jit-compile-detail ; may
    (hash '|racket -cl racket|
          (add-diff
           (hash 'expand 25443
                 'compile 3410
                 'compile-linklet 2572
                 'regalloc 0
                 'read 1407
                 'jit 752))
          '|racket -cl racket/base|
          (add-diff
           (hash 'expand 2300
                 'compile 410
                 'compile-linklet 311
                 'regalloc 0
                 'read 231
                 'jit 169))))
  (define r-jit-recompile
    (hash '|racket -Ml racket|
          (+ 2393 100)
          '|racket -Ml racket/base|
          (+ 457 40)))

  (define ghost-label (t "R/CS JIT"))

  (define expand-color "darkgray")
  (define compile-color "brown")
  (define regalloc-color "chocolate")
  (define jit-color "gold")
  (define schemify-color "slateblue")

  (define compile-key
    (scale
     (let ([sq (let ([s (* 0.8 gap-size)])
                 (filled-rectangle s s))])
       (vl-append
        3
        (hbl-append (colorize sq expand-color) (t " = expand"))
        (hbl-append (colorize sq schemify-color) (t " = schemify"))
        (hbl-append (colorize sq compile-color) (t " = compile"))
        (hbl-append (colorize sq regalloc-color) (t " = register allocate"))
        (hbl-append (colorize sq jit-color) (t " = JIT"))))
     1/2))

  (define key-sep 20)

  (define (compile-time-plot)
    (hc-append
     key-sep
     (scale
      (add-desc
       "load-from-source time"
       (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep
             #:normalize-max? #t
             #:max (hash-ref r/cs-compile '|racket -cl racket|)
             #:reverse? #t
             #:columns 1
             #:width plot-width
             #:ghost-label ghost-label
             #:suffix " msec"
             (list (t "R/CS") (t "R/BC"))
             (list racket-color c-color)
             (list r/cs-compile
                   r-compile)))
      scale-amt)
     (ghost compile-key)))

  (define (compile-time-detail-plot)
    (hc-append
     key-sep
     (scale
      (add-desc
       "load-from-source time"
       (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep
             #:normalize-max? #t
             #:max (hash-ref r/cs-compile '|racket -cl racket|)
             #:reverse? #t
             #:columns 1
             #:width plot-width
             #:ghost-label ghost-label
             #:suffix " msec"
             (list (t "R/CS") (t "R/BC"))
             (list racket-color c-color r-jit-color)
             (list r/cs-compile
                   r-compile)
             #:details (list r/cs-compile-detail
                             r-compile-detail
                             r-jit-compile-detail)
             #:detail-spec (list (cons 'jit jit-color)
                                 (cons 'regalloc regalloc-color)
                                 (cons 'compile-linklet-minus-regalloc compile-color)
                                 (cons 'schemify schemify-color)
                                 (cons 'expand expand-color))))
      (* 2 scale-amt))
     compile-key))

  (define (recompile-time-plot)
    (hc-append
     key-sep
     (scale
      (add-desc
       "load-from-expanded time"
       (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep
             #:normalize-max? #t
             #:max (hash-ref r/cs-compile '|racket -cl racket|)
             #:reverse? #t
             #:columns 1
             #:width plot-width
             #:ghost-label ghost-label
             #:suffix " msec"
             (list (t "R/CS") (t "R/BC") (t "R/jit!"))
             (list racket-color c-color r-jit-color)
             (list r/cs-recompile
                   r-recompile
                   r-jit-recompile)))
      (* 2 scale-amt))
     (ghost compile-key))))
