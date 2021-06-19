#lang slideshow
(require racket/runtime-path
         "color.rkt"
         "config.rkt"
         "better.rkt"
         "plot.rkt")

(provide benchmark-slides
         load-compile-slides
         expander-modify-plot
         compilation-compare-slides)

(define-runtime-path log-cs1-txt "bmtimes-sep18/log-cs1.txt")
(define-runtime-path log-rcs1-txt "bmtimes-sep18/log-rcs1.txt")
(define-runtime-path log-r1-txt "bmtimes-sep18/log-r1.txt")
(define-runtime-path sho-rcs1-txt "bmtimes-sep18/sho-rcs1.txt")
(define-runtime-path sho-r1-txt "bmtimes-sep18/sho-r1.txt")

(define traditional-cs (get-times log-cs1-txt))
(define traditional-r/cs (get-times log-rcs1-txt))
(define traditional-r (get-times log-r1-txt))
(define shootout-r/cs (get-times sho-rcs1-txt))
(define shootout-r (get-times sho-r1-txt))

(define (fit p)
  (define w (pict-width p))
  (define h (pict-height p))
  (scale p (min 1 (/ (pict-width titleless-page) w) (/ (pict-height titleless-page) h))))

(define (benchmark-slides #:traditional-cs [traditional-cs traditional-cs]
                          #:traditional-r/cs [traditional-r/cs traditional-r/cs]
                          #:traditional-r [traditional-r traditional-r]
                          #:shootout-r/cs [shootout-r/cs shootout-r/cs]
                          #:shootout-r [shootout-r shootout-r]
                          #:racket-cs-label [racket-cs-label "Racket-on-Chez"])
  (define r/cs-def 
    (para #:fill? #f (colorize (t "R/CS") racket-color) "=" (colorize (t racket-cs-label) racket-color)))
  (define r-def
    (para #:fill? #f (colorize (t "R") c-color) "=" (colorize (t "Racket") c-color)))
  
  (slide
   #:title (title/better "Traditional Scheme Benchmarks" #:shorter-better? #t)
   #:name "Traditional Scheme Benchmarks"
   #:layout 'tall
   (fit
    (rb-superimpose
     (plot #:t t #:tt tt #:gap-size gap-size
           #:columns 6
           #:sort-ratio '(1 . 2)
           (list (t "CS") (t "R/CS") (t "R"))
           (list scheme-color racket-color c-color)
           (list traditional-cs traditional-r/cs traditional-r))
     (vl-append
      (para #:fill? #f (colorize (filled-rectangle 20 20) r5rs-color) "= mutable pairs")
      (para #:fill? #f (colorize (t "CS") scheme-color) "=" (colorize (t "Chez Scheme") scheme-color))
      r/cs-def
      r-def)
     (vl-append
      (t "x86_64 Linux")
      (t "Core i7-2600 3.4GHz")
      (blank 0 (* client-h 0.53))))))

  (slide
   #:title "Shootout Benchmarks"
   #:layout 'tall
   (fit
    (rb-superimpose
     (plot #:t t #:tt tt #:gap-size gap-size
           #:columns 6
           #:sort-ratio '(0 . 1)
           (list (t "R/CS") (t "R"))
           (list "blue" "red")
           (list shootout-r/cs shootout-r))
     (vl-append
      r/cs-def
      r-def)))))

(define (reason figure . why)
  (ct-superimpose
   figure
   (let ([p (inset (scale (para #:fill? #f #:width (* client-w 1/4)
                                why)
                          0.8)
                   (/ gap-size 2))])
     (para #:align 'right
           #:width (* 0.9 client-w)
           (cc-superimpose
            (colorize (filled-rectangle (pict-width p) (pict-height p))
                      "beige")
            p)))))

(define (add-diff ht)
  (hash-set ht 'compile-linklet-minus-regalloc
            (- (hash-ref ht 'compile-linklet)
               (hash-ref ht 'regalloc))))

(define (load-compile-slides #:show-cify? [show-cify? show-cify?]
                             #:show-jit!? [show-jit!? #f]
                             #:bm-date [bm-date 'sep18]
                             #:report-compressed? [report-compressed? #f])
  (define (cify-list a)
    (if show-cify?
        (list a)
        null))
  (define (jit!-list a)
    (if show-jit!?
        (list a)
        null))

  (define r/cs-compile
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (+ 59183 444) ; user + sys
             '|racket -cl racket/base|
             (+ 5738 76))]
      [(sep18)
       (hash '|racket -cl racket|
             (+ 56540 500) ; user + sys
             '|racket -cl racket/base|
             (+ 5524 64))]))
  (define r/cs-compile-detail
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (add-diff
              (hash 'expand 23955
                    'compile 29913
                    'compile/no-cptypes 29019 ; [from separate run]
                    'schemify 1995
                    'compile-linklet 26920
                    'regalloc 9180
                    'read 838))
             '|racket -cl racket/base|
             (add-diff
              (hash 'expand 2084
                    'compile 3641 ; includes 'schemify and 'compile-linklet
                    'compile/no-cptypes 3567 ; [from separate run]
                    'schemify 258
                    'compile-linklet 3248
                    'regalloc 1120 ; part of 'compile-linklet
                    'read 137)))]
      [(sep18)
       (hash '|racket -cl racket|
             (add-diff
              (hash 'expand 24804
                    'compile 23641
                    'schemify 2053
                    'compile-linklet 22300
                    'regalloc 8037
                    'read 1176))
             '|racket -cl racket/base|
             (add-diff
              (hash 'expand 2259
                    'compile 2801 ; includes 'schemify and 'compile-linklet
                    'schemify 247
                    'compile-linklet 2432
                    'regalloc 899 ; part of 'compile-linklet
                    'read 213)))]))
  (define r/cs/jit-compile
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (+ 44768 248)
             '|racket -cl racket/base|
             (+ 3923 56))]
      [(sep18)
       (hash '|racket -cl racket|
             (+ 41920 292)
             '|racket -cl racket/base|
             (+ 3780 80))]))
  (define r-compile
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (+ 30937 664)
             '|racket -cl racket/base|
             (+ 2776 84))]
      [(sep18)
       (hash '|racket -cl racket|
             (+ 31236 828)
             '|racket -cl racket/base|
             (+ 2932 100))]))
  (define r-compile-detail
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (add-diff
              (hash 'expand 22451
                    'compile 3536
                    'compile-linklet 2739
                    'regalloc 0
                    'read 1389
                    'jit 473
                    'jit-eager 782))
             '|racket -cl racket/base|
             (add-diff
              (hash 'expand 2034
                    'compile 392
                    'compile-linklet 312
                    'regalloc 0
                    'read 222
                    'jit 100
                    'jit-eager 172)))]
      [(sep18)
       (hash '|racket -cl racket|
             (add-diff
              (hash 'expand 22060
                    'compile 7338
                    'compile-linklet 2756
                    'regalloc 0
                    'read 1389
                    'jit 428
                    'jit-eager 876))
             '|racket -cl racket/base|
             (add-diff
              (hash 'expand 2105
                    'compile 395
                    'compile-linklet 292
                    'regalloc 0
                    'read 264
                    'jit 128
                    'jit-eager 228)))]))
  (define r/jit!-compile
    (case bm-date
      [(mar19)
       (hash '|racket -cl racket|
             (+ 35337 720)
             '|racket -cl racket/base|
             (+ 3195 92))]
      [(sep18)
       (hash '|racket -cl racket|
             0
             '|racket -cl racket/base|
             0)]))
  (define r/cify-compile
    (hash '|racket -cl racket|
          (+ 35072 948)
          '|racket -cl racket/base|
          (+ 3076 104)))
  (define r6-compile
    (hash '|racket -cl racket|
          (+ 18812 780)
          '|racket -cl racket/base|
          (+ 1696 44)))

  (define r/cs-load
    (case bm-date
      [(mar19)
       (hash '|racket -l racket|
             (+ 404 60)
             '|racket -l racket/base|
             (+ 121 25))]
      [(sep18)
       (hash '|racket -l racket|
             (+ 591 56)
             '|racket -l racket/base|
             (+ 196 80))]))
  (define r/cs/jit-load
    (case bm-date
      [(mar19)
       (hash '|racket -l racket|
             (+ 494 44)
             '|racket -l racket/base|
             (+ 107 28))]
      [(sep18)
       (hash '|racket -l racket|
             (+ 632 76)
             '|racket -l racket/base|
             (+ 248 40))]))
  (define r-load
    (case bm-date
      [(mar19)
       (hash '|racket -l racket|
             (+ 237 28)
             '|racket -l racket/base|
             (+ 82 12))]
      [(sep18)
       (hash '|racket -l racket|
             (+ 280 40)
             '|racket -l racket/base|
             (+ 116 12))]))
  (define r-d-load ; using -d
    (case bm-date
      [(mar19)
       (hash '|racket -l racket|
             (+ 375 32)
             '|racket -l racket/base|
             (+ 96 24))]
      [(sep18)
       (hash '|racket -l racket|
             (+ 432 32)
             '|racket -l racket/base|
             (+ 160 16))]))
  (define r-d-jit!-load ; using -d and PLT_EAGER_JIT
    (case bm-date
      [(mar19)
       (hash '|racket -l racket|
             (+ 1042 60)
             '|racket -l racket/base|
             (+ 254 24))]
      [(sep18)
       (hash '|racket -l racket|
             0
             '|racket -l racket/base|
             0)]))
  (define r/cify-load
    (hash '|racket -l racket|
          (+ 244 48)
          '|racket -l racket/base|
          (+ 80 4)))
  (define r6-load
    (hash '|racket -l racket|
          (+ 208 36)
          '|racket -l racket/base|
          (+ 48 16)))

  (define plot-s 0.8)
  (define plot-w (* 0.5 client-w))

  (define ghost-label (t "R/CS JIT"))

  (define expand-color "darkgray")
  (define compile-color "brown")
  (define regalloc-color "chocolate")
  (define jit-color "salmon")
  (define schemify-color "slateblue")

  (slide
   #:title "Compile Time"
   'alts
   (list*
    (list
     (scale
      (plot #:t t #:tt tt #:gap-size gap-size
            #:normalize-max? #t
            #:reverse? #t
            #:columns 1
            #:width plot-w
            #:ghost-label ghost-label
            (list (t "R/CS") (t "R"))
            (list racket-color c-color)
            (list r/cs-compile
                  r-compile))
      plot-s))
    (list
     (reason
      (scale
       (plot #:t t #:tt tt #:gap-size gap-size
             #:normalize-max? #t
             #:reverse? #t
             #:columns 1
             #:width plot-w
             #:ghost-label ghost-label
             (list (t "R/CS") (t "R"))
             (list racket-color c-color)
             (list r/cs-compile
                   r-compile)
             #:details (list r/cs-compile-detail
                             r-compile-detail)
             #:detail-spec (list (cons 'jit jit-color)
                                 (cons 'regalloc regalloc-color)
                                 (cons 'compile-linklet-minus-regalloc compile-color)
                                 (cons 'schemify schemify-color)
                                 (cons 'expand expand-color)))
       plot-s)
      (let ([sq (let ([s (* 0.6 (current-font-size))])
                  (filled-rectangle s s))])
        (vl-append
         (current-line-sep)
         (para #:fill? #f (colorize sq expand-color) "= expand")
         (para #:fill? #f (colorize sq schemify-color) "= schemify")
         (para #:fill? #f (colorize sq compile-color) "= compile")
         (para #:fill? #f (colorize sq regalloc-color) "= register allocate")
         (para #:fill? #f (colorize sq jit-color) "= JIT")))))
    (list
     (reason
      (scale
       (plot #:t t #:tt tt #:gap-size gap-size
             #:normalize-max? #t
             #:reverse? #t
             #:columns 1
             #:width plot-w
             #:ghost-label ghost-label
             (list (t "R/CS") (t "R") (t "Rv6"))
             (list racket-color c-color r6-color)
             (list r/cs-compile
                   r-compile
                   r6-compile))
       plot-s)
      (colorize (t "Racket expander") c-color) "⇒ x1.5-2"
      "compared to" (colorize (t "C expander") r6-color)))
    (list
     (reason
      (scale
       (plot #:t t #:tt tt #:gap-size gap-size
             #:normalize-max? #t
             #:reverse? #t
             #:columns 1
             #:width plot-w
             #:ghost-label ghost-label
             (list (t "R/CS") (t "R/CS JIT") (t "R") (t "Rv6"))
             (list racket-color cs/jit-color c-color r6-color)
             (list r/cs-compile
                   r/cs/jit-compile
                   r-compile
                   r6-compile))
       plot-s)
      (colorize (t "JIT") cs/jit-color) "is on-demand" (hbl-append (t "per-") (tt "lambda"))
      "call to" (tt "compile")))
    (append
     (jit!-list
      (list
       (reason
        (scale
         (plot #:t t #:tt tt #:gap-size gap-size
               #:normalize-max? #t
               #:reverse? #t
               #:columns 1
               #:width plot-w
               #:ghost-label ghost-label
               (list (t "R/CS") (t "R/CS JIT") (t "R/JIT!") (t "R") (t "Rv6"))
               (list racket-color cs/jit-color r-jit-color c-color r6-color)
               (list r/cs-compile
                     r/cs/jit-compile
                     r/jit!-compile
                     r-compile
                     r6-compile))
         plot-s)
        "Forcing" (colorize (t "JIT") r-jit-color) "at bytecode compile makes a small difference")))
     (cify-list
      (list
       (reason
        (scale
         (plot #:t t #:tt tt #:gap-size gap-size
               #:normalize-max? #t
               #:reverse? #t
               #:columns 1
               #:width plot-w
               #:ghost-label ghost-label
               (list (t "R/CS") (t "R/CS JIT") (t "R") (t "R cify") (t "Rv6"))
               (list racket-color cs/jit-color c-color cify-color r6-color)
               (list r/cs-compile
                     r/cs/jit-compile
                     r-compile
                     r/cify-compile
                     r6-compile))
         plot-s)
        (colorize (t "cify Racket expander") cify-color) "to support all platforms"))))))

  (slide
   #:title "Compile Time versus Load Time"
   (let ([compare
          (lambda (label -color -compile -load)
            (vl-append
             (current-line-sep)
             (colorize label -color)
             (scale
              (plot #:t t #:tt tt #:gap-size gap-size
                    #:normalize-max? #t
                    #:reverse? #t
                    #:columns 1
                    #:width plot-w
                    #:ghost-label ghost-label
                    (list (t "compile") (t "load"))
                    (list -color -color)
                    (list (hash 'racket
                                (hash-ref -compile '|racket -cl racket|)
                                'racket/base
                                (hash-ref -compile '|racket -cl racket/base|))
                          (hash 'racket
                                (hash-ref -load '|racket -l racket|)
                                'racket/base
                                (hash-ref -load '|racket -l racket/base|))))
              plot-s)))])
     (vc-append (* 3 gap-size)
                (compare (bt "Racket-on-Chez") racket-color r/cs-compile r/cs-load)
                (compare (bt "Racket")  c-color r-compile r-load))))

  (define load-time-max (hash-ref r/cs/jit-load '|racket -l racket|))
  
  (slide
   #:title "Load Time"
   'alts
   (append
    (list
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (list (t "R/CS") (t "R"))
              (list racket-color c-color)
              (list r/cs-load
                    r-load))
        plot-s)
       (let ([u (para #:fill? #f #:width (* client-w 1/4)
                      (colorize (t "Uncompressed machine code") racket-color) "is 6x" (colorize (t "bytecode") c-color) "size")])
         (if report-compressed?
             (vl-append
              gap-size
              (para #:fill? #f #:width (* client-w 1/4)
                      (colorize (t "Compressed machine code") racket-color) "is 2.5x" (colorize (t "bytecode") c-color) "size")
              u)
             u))))
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (list (t "R/CS") (t "R") (t "Rv6"))
              (list racket-color c-color r6-color)
              (list r/cs-load
                    r-load
                    r6-load))
        plot-s)
       (colorize (t "Racket expander") c-color) " ⇒ bytecode startup over"
       (colorize (t "C expander") r6-color)))
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (list (t "R/CS") (t "R -d") (t "R") (t "Rv6"))
              (list racket-color c-color c-color r6-color)
              (list r/cs-load
                    r-d-load
                    r-load
                    r6-load))
        plot-s)
       (colorize (t "Racket") c-color) "load time benefits from lazy bytecode parsing;"
       (tt "-d") "flag disables it")))
    (cify-list
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (list (t "R/CS") (t "R -d") (t "R") (t "R cify") (t "Rv6"))
              (list racket-color c-color c-color cify-color r6-color)
              (list r/cs-load
                    r-d-load
                    r-load
                    r/cify-load
                    r6-load))
        plot-s)
       (colorize (t "cify Racket expander") cify-color) "reduces startup overhead")))
    (list
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (append (list (t "R/CS") (t "R/CS JIT") (t "R -d") (t "R")) (cify-list (t "R cify")) (list (t "Rv6")))
              (append (list racket-color cs/jit-color c-color c-color) (cify-list cify-color) (list r6-color))
              (append (list r/cs-load
                            r/cs/jit-load
                            r-d-load
                            r-load)
                      (cify-list r/cify-load)
                      (list r6-load)))
        plot-s)
       "Cache" (colorize (t "JIT") cs/jit-color) "compilation ⇒ startup close to machine code")))
    (jit!-list
     (list
      (reason
       (scale
        (plot #:t t #:tt tt #:gap-size gap-size
              #:normalize-max? #f
              #:pin-max? #t
              #:max load-time-max
              #:reverse? #t
              #:columns 1
              #:width plot-w
              #:ghost-label ghost-label
              (append (list (t "R/CS") (t "R/CS JIT") (t "R/JIT!") (t "R -d") (t "R")) (cify-list (t "R cify")) (list (t "Rv6")))
              (append (list racket-color cs/jit-color r-jit-color c-color c-color) (cify-list cify-color) (list r6-color))
              (append (list r/cs-load
                            r/cs/jit-load
                            r-d-jit!-load
                            r-d-load
                            r-load)
                      (cify-list r/cify-load)
                      (list r6-load)))
        plot-s)
       "Can force" (colorize (t "JIT") r-jit-color) "on load (no cache)"))))))

(define (memory-use-slides)
  (define cs-base 42775)
  (define r-base (+ 9478 612))
  (define max-mem (- 119626 cs-base))

  (define (show extras?)
    (slide
     #:title "Memory Use"
     (apply
      reason
      (vl-append
       (* 2 gap-size)
       (single-plot "racket -l racket/base"
                    #:t t #:tt tt #:gap-size gap-size
                    #:cs (- 52788 cs-base) #:r (- (+ 9747 1680) r-base)
                    #:r-all (and extras? (- (+ 12336 1680) r-base))
                    #:r-jit (and extras? (- (+ 10606 17137) r-base))
                    #:max max-mem
                    #:display-scale 1/1024
                    #:suffix " MB"
                    #:vertical-bars? #t
                    #:widen? #f
                    #:pad-left 100)
       (single-plot "racket -l racket"
                    #:t t #:tt tt #:gap-size gap-size
                    #:cs (- 101682 cs-base) #:r (- (+ 32370 3096) r-base)
                    #:r-all (and extras? (- (+ 52381 3104) r-base))
                    #:r-jit (and extras? (- (+ 47755 39028) r-base))
                    #:max max-mem
                    #:display-scale 1/1024
                    #:suffix " MB"
                    #:vertical-bars? #t
                    #:widen? #f
                    #:pad-left 100))
      (if extras?
          (list (colorize (t "JIT") r-jit-color) "output on similar scale")
          (list "Relative to startup ⇒ mostly code size")))))
  (show #f)
  (show #t))

(define (compilation-compare-slides)
  (define bar-sep (* (/ 5 24) gap-size))
  (define expander '||)

  (define (general-plot labels
                        colors
                        tables
                        #:vertical-bars? [vertical-bars? #f]
                        #:desc [desc ""]
                        #:suffix [suffix ""]
                        #:display-scale display-scale)
    (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep (if vertical-bars? bar-sep 0)
          #:columns 1
          #:vertical-bars? vertical-bars?
          #:suffix suffix
          #:display-scale display-scale
          #:decimal-places 1
          (map t labels) colors tables))

  (define (/~ n) (* 100 (floor (/ n 100))))
  (define (// n) (* 100 (floor (/ n 1024 100))))
  
  (slide
   #:title "Reasonable Compile Times"
   (para "Compile time for similar expander implementations")
   (reason
    (general-plot
     #:suffix " sec"
     #:desc "compile time"
     #:display-scale 1/1000
     (list "CS" "R" "C")
     (list scheme-color c-color plain-c-color)
     (list
      (hash expander (/~ 12480))
      (hash expander (/~ 1390))
      (hash expander (/~ (- (+ 74261 1928) (+ 65513 1885))))))
    (colorize (t "CS") scheme-color) "and" (colorize (t "R") c-color)
    "are the same implementation, post-expansion"))

  (slide
   #:title "Reasonable Code Size"
   (para "Machine-code size for similar expander implementations")
   (reason
    (general-plot
     #:suffix " MB"
     #:display-scale 1/1024
     #:desc "machine code size"
     #:vertical-bars? #t
     (list "CS" "R/JIT!" "R/JIT!/no" "R/cify" "C")
     (list scheme-color r-jit-color r-jit-color cify-color plain-c-color)
     (list
      (hash expander (// (- 3331312 925568))) ; object-count diffs after pre-reqs; compiling as program instead of library cuts 925568
      (hash expander (// 4761632)) ; via PLT_EAGER_JIT=y PLT_LINKLET_TIMES=y
      (hash expander (// 3018048)) ; initialize inline-fuel to 0, rebuild cstartup.inc
      (hash expander (// 1809823)) ; cify: cify 6081768 minus non-cify 4,955,680 plus .zo size 683,735
      (hash expander (// 1006323)))) ; delta of v6.12 5,278,268 and v7.2.03 4,955,680 minus .zo size 683,735
    (colorize (t "R/JIT!/no") r-jit-color) "has inlining disabled in bytecode compiler"
    (blank (* client-w 1/5) gap-size)
    (colorize (t "R/cify") cify-color) "is" (colorize (t "Racket") racket-color)
    "implementation compiled to" (colorize (t "C") c-color)))

  (void))

(define (expander-modify-plot #:width [width (* 0.5 client-w)]
                              #:years? [years? #f]
                              #:explain? [explain? #t])
  (plot #:t t #:tt tt #:gap-size gap-size
        #:width  width
        (list (t " Racket") (t "C"))
        (list racket-color c-color)
        #:key->pict (lambda (k)
                      (define p (para #:fill? #f "Racketeers who have modified the expander"))
                      (cond
                        [explain?
                         (define b (it "longer is better"))
                         (inset (refocus (ht-append (* 3 gap-size) p b)
                                         p)
                                0 0 0 gap-size)]
                        [else p]))
        #:notes (and years? (list " first 2 years" " first 16 years"))
        (list
         (hash 'people 6)    ; Matthew, Ryan, Alexis, Michael, William
         (hash 'people 2)))) ; Matthew, Ryan

(module+ main
  (compilation-compare-slides)
  (memory-use-slides)
  (benchmark-slides)
  (load-compile-slides)
  (slide (expander-modify-plot)))

