(require pict
         racket/draw
         (prefix-in color: "color.rkt")
         "plot.rkt"
         "build-desc.rkt")

(provide traditional-benchmarks
         shootout-benchmarks
         r5rs?)

(define traditional-cs-orig (get-times log-cs-orig-txt))
(define traditional-cs (get-times log-cs-txt))
(define traditional-r/cs (get-times log-rcs-txt))
(define traditional-r (get-times log-r-txt))

(define shootout-r/cs (get-times sho-rcs-txt))
(define shootout-r (get-times sho-r-txt))

(define b&w? #f)

(define scheme-color (if b&w? (make-color 90 90 90) color:scheme-color))
(define racket-color (if b&w? (make-color 70 70 70) color:racket-color))
(define c-color (if b&w? (make-color 50 50 50) color:c-color))
(define r-jit-color (if b&w? (make-color 100 100 100) color:r-jit-color))

(define (r/cs-def t)
  (hbl-append (colorize (t "R/CS") racket-color) (t " = ") (colorize (t "Racket CS") racket-color)))
(define (r-def t)
  (hbl-append (colorize (t "Racket") c-color) (t " = ") (colorize (t "Racket") c-color)))

(define (machine t)
  (vl-append
   (t "x86_64 Linux")
   (t "Core i7-2600 3.4GHz")))

(define (geometric-mean ht wrt-ht #:keep? [keep? (lambda (k) #t)])
  (define keys
    (for/list ([k (in-hash-keys ht)]
               #:when (keep? k))
      k))
  (define prod
    (for/product ([k (in-list keys)])
      (define v (hash-ref ht k))
      (/ v (hash-ref wrt-ht k))))
  (hash 'traditional (expt prod (/ 1 (length keys)))))

(define (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:bar-sep [bar-sep 0]
                                #:bar-text-scale [bar-text-scale 0.5]
                                #:mean? [mean? #t] #:label [label #f] #:width [width 200]
                                #:keep? [keep? (lambda (k) (not (r5rs? k)))]
                                #:alt-cs? [alt-cs? #t])
  (rb-superimpose
   (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep #:bar-t bt
         #:bar-text-scale bar-text-scale
         #:columns (if mean? 1 6)
         #:sort-ratio '(0 . 3)
         #:prefix (if mean? "x" "")
         #:suffix (if mean? "" " msec")
         #:decimal-places (if mean? 2 0)
         #:key->pict (lambda (k) label)
         #:width width
         (append (list (t "CS"))
                 (if alt-cs?
                     (list (t "CS′"))
                     null)
                 (list (t "R/CS")
                       (t Racket-abbrev)))
         (append (list scheme-color)
                 (if alt-cs?
                     (list scheme-color)
                     null)
                 (list racket-color
                       c-color))
         (if mean?
             (append
              (list (geometric-mean traditional-cs-orig traditional-r #:keep? keep?))
              (if alt-cs?
                  (list (geometric-mean traditional-cs traditional-r #:keep? keep?))
                  null)
              (list (geometric-mean traditional-r/cs traditional-r #:keep? keep?)
                    (geometric-mean traditional-r traditional-r #:keep? keep?)))
             (append
              (list traditional-cs-orig)
              (if alt-cs?
                  (list traditional-cs)
                  null)
              (list traditional-r/cs
                    traditional-r))))
   #;
   (hc-append
    (* 3 gap-size)
    (machine t)
    (vl-append
     (r/cs-def t)
     (r-def t)))))

(define (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:bar-sep [bar-sep 0]
                             #:bar-text-scale [bar-text-scale 0.5]
                             #:mean? [mean? #t] #:label [label #f] #:width [width 200])
  (rb-superimpose
   (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep #:bar-t bt
         #:bar-text-scale bar-text-scale
         #:columns 6
         #:sort-ratio '(0 . 1)
         #:prefix (if mean? "x" "")
         #:suffix (if mean? "" " msec")
         #:decimal-places (if mean? 2 0)
         #:key->pict (lambda (k) label)
         #:width width
         (list (t "R/CS") (t Racket-abbrev) (t "old"))
         (list racket-color c-color)
         (if mean?
             (list (geometric-mean shootout-r/cs shootout-r)
                   (geometric-mean shootout-r shootout-r))
             (list shootout-r/cs shootout-r)))
   #;
   (hc-append
    (* 3 gap-size)
    (machine t)
    (vl-append
     (r/cs-def t)
     (r-def t)))))

;; ----------------------------------------

(module+ pict
  (require (submod "bm-jan19.rkt" pict))
  (provide all-paper-benchmarks
           traditional-benchmarks-table
           shootout-benchmarks-table)

  (define (t s) (text s 'swiss 32))
  (define (bt s) (text s '(bold . swiss) 32))
  (define (it s) (text s '(italic . swiss) 32))
  (define (tt s) (text s '(bold . modern) 32))
  (define gap-size 24)
  (define width 200)
  (define height 150)
  (define long-width 400)
  (define bar-sep 5)
  (define scale-amt (* 2 0.3))

  (define bar-text-scale 0.75)
  
  (define (label s #:sub [sub "geometric mean of run time relative to Racket"])
    (vl-append 2
               (if (pict? s) s (t s))
               (scale (it sub) 0.8)
               (blank)))
  
  (define traditional-benchmarks/mean
    (scale
     (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                             #:width long-width #:bar-sep bar-sep #:bar-text-scale bar-text-scale
                             #:label (label "Scheme benchmarks / immutable pairs"))
     scale-amt))
  (define traditional-r5rs-benchmarks/mean
    (scale
     (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:keep? r5rs?
                             #:width long-width #:bar-sep bar-sep #:bar-text-scale bar-text-scale
                             #:label (label "Scheme benchmarks / mutable pairs"))
     scale-amt))
  (define shootout-benchmarks/mean
    (scale
     (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                          #:width long-width  #:bar-sep bar-sep #:bar-text-scale bar-text-scale
                          #:label (label "Shootout benchmarks"))
     scale-amt))

  (define load-time
    (one-plot "load"
              #:width width #:bar-t bt #:bar-text-scale bar-text-scale
              #:racket-color racket-color #:c-color c-color #:r-jit-color r-jit-color
              #:cs cs-load-time
              #:r r-load-time
              #:r-all r/all-load-time
              #:max cs-load-time
              #:suffix " msec"
              #:label (label (tt "racket -l racket")
                             #:sub "startup+load time")))

  (define compile-time
    (one-plot "load"
              #:width width #:bar-t bt #:bar-text-scale bar-text-scale
              #:racket-color racket-color #:c-color c-color #:r-jit-color r-jit-color
              #:cs cs-compile-time
              #:r r-compile-time
              #:r-all r/all-compile-time
              #:max cs-compile-time
              #:display-scale 1/1000
              #:decimal-places 2
              #:suffix " seconds"
              #:label (label (tt "racket -cl racket")
                             #:sub "compile from source time")))

  (define drracket-memory
    (one-plot "drracket"
              #:label (label "DrRacket"
                             #:sub "startup+exit peak memory use")
              #:width height #:bar-t bt #:bar-text-scale bar-text-scale
              #:racket-color racket-color #:c-color c-color #:r-jit-color r-jit-color
              #:cs cs-dr-mem
              #:r r-dr-mem
              #:r-all r-all-dr-mem
              #:r-jit r-jit-dr-mem
              #:max r-jit-dr-mem
              #:display-scale 1/1024
              #:suffix " MB"
              #:vertical-bars? #t
              #:widen? #f))

  (define build-time
    (one-plot "build"
              #:width width #:bar-t bt #:bar-text-scale bar-text-scale
              #:racket-color racket-color #:c-color c-color #:r-jit-color r-jit-color
              #:cs cs-build-time
              #:r r-build-time
              #:max cs-build-time
              #:suffix " minutes"
              #:label (label "Build distribution"
                             #:sub "build time")))

  (define build-memory
    (one-plot "build"
              #:label (label "Build distribution"
                             #:sub "peak memory use")
              #:width height #:bar-t bt #:bar-text-scale bar-text-scale
              #:racket-color racket-color #:c-color c-color #:r-jit-color r-jit-color
              #:cs cs-build-mem
              #:r r-build-mem
              #:max cs-build-mem
              #:display-scale 1/1024
              #:suffix " MB"
              #:vertical-bars? #t
              #:widen? #f))

  (define all-paper-benchmarks
    (scale
     (vl-append
      30
      (ht-append 40
                 traditional-benchmarks/mean
                 traditional-r5rs-benchmarks/mean
                 shootout-benchmarks/mean)
      (ht-append 30
                 load-time
                 compile-time
                 drracket-memory
                 build-time
                 build-memory))
     0.45))

  (define (traditional-benchmarks-table)
    (scale (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:mean? #f) 0.20))

  (define (shootout-benchmarks-table)
    (scale (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:mean? #f) 0.20)))

(module one-plot racket/base
  (require (prefix-in in: (submod "bm-jan19.rkt" pict))
           pict
           racket/draw
           racket/class)

  (provide one-plot
           general-plot
           compile-time-plot
           compile-time-detail-plot
           recompile-time-plot
           build-desc
           rdc)

  (define S 0.5)

  (define one-plot
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (scale (keyword-apply in:one-plot kws kw-args args) S))))

  (define general-plot
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (scale (keyword-apply in:general-plot kws kw-args args) S))))

  (define (compile-time-plot) (scale (in:compile-time-plot) S))
  (define (compile-time-detail-plot) (scale (in:compile-time-detail-plot) S))
  (define (recompile-time-plot) (scale (in:recompile-time-plot) S))

  (define (build-desc path)
    (scale (in:build-desc #:base (scale (rdc path) 4)
                          #:max-time "1h55m"
                          #:max-memory "1.2 GB")
           0.5))

  (define (rdc fn)
    (define l (call-with-input-file* fn read))
    (define proc (recorded-datum->procedure l))
    (scale (dc (lambda (dc x y)
                 (define t (send dc get-transformation))
                 (send dc translate x y)
                 (proc dc)
                 (send dc set-transformation t))
               800 600)
           0.25)))

(module+ main
  (require slideshow
           (submod ".." pict))
  (slide (scale all-paper-benchmarks 2))
  (slide
   (scale (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:mean? #f) 0.5))
  (slide
   (scale (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:mean? #f) 0.5))
  (slide
   (scale (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                                  #:width 500
                                  #:label (t "Scheme benchmarks, geometric mean"))
          0.5)
   (scale (traditional-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size #:keep? r5rs?
                                  #:width 500
                                  #:label (t "Mutable-pair benchmarks, geometric mean"))
          0.5)
   (scale (shootout-benchmarks #:t t #:bt bt #:tt tt #:gap-size gap-size
                               #:width 500
                               #:label (t "Shootout benchmarks, geometric mean"))
          0.5)))
