#lang racket/base
(require pict
         racket/list
         racket/format
         "color.rkt")

(provide plot
         single-plot
         get-times

         r5rs-color
         r5rs?)

(define (get-times f)
  (define all-times
    (call-with-input-file*
     f
     (lambda (i)
       (for/fold ([ht '#hasheq()]) ([i (in-port read i)])
         (define k (rename (cadr i)))
         (define t (caaddr i))
         (define e (hash-ref ht k null))
         (if t
             (hash-set ht k (cons t e))
             ht)))))
  (define (median l)
    (list-ref (sort l <) (quotient (length l) 2)))
  (for/hasheq ([(k e) (in-hash all-times)]
               #:unless (or (eq? k 'nothing)
                            (eq? k 'hello)))
    (values k (median e))))

(define (rename k)
  (hash-ref #hasheq((mandelbrot-generic . mandelbrot-g)
                    (reversecomplement . reversecomp)
                    (spectralnorm-generic . spectralnorm-g)
                    (nbody-vec-generic . nbody-vec-g)
                    (cheapconcurrency . cheapconcur))
            k
            k))

(define r5rs-color "forestgreen")

(define r5rs-keys
  '(conform destruct dynamic lattice maze peval psyntax scheme-c scheme-i scheme sort1))
(define (r5rs? key) (memq key r5rs-keys))

(define (plot names colors timess
              #:t t #:tt tt #:gap-size gap-size
              #:bar-t [bar-t t]
              #:columns [columns 2]
              #:width [W 200]
              #:bar-sep [bar-sep 0]
              #:bar-text-scale [bar-text-scale 0.5]
              #:normalize-max? [normalize-max? #f]
              #:pin-max? [pin-max? #f]
              #:max [starting-max 0]
              #:sort-ratio [sort-ratio #f]
              #:key->pict [key->pict (lambda (k) #f)]
              #:reverse? [reverse? #f]
              #:ghost-label [ghost-label #f]
              #:details [details (map (lambda (v) #hasheq()) timess)]
              #:detail-spec [detail-spec #f]
              #:display-scale [display-scale 1]
              #:decimal-places [decimal-places 0]
              #:vertical-bars? [vertical-bars? #f]
              #:fill-vertical? [fill-vertical? #f]
              #:widen? [widen? vertical-bars?]
              #:pad-left [pad-left 0]
              #:prefix [prefix ""]
              #:suffix [suffix ""]
              #:notes [notes #f])
  (define H (* 2 bar-text-scale (if vertical-bars? 70 30)))
  (define keys ((if reverse? reverse values)
                (sort (hash-keys (car timess))
                      (lambda (a b)
                        (cond
                          [(and (r5rs? a) (not (r5rs? b))) #f]
                          [(and (r5rs? b) (not (r5rs? a))) #t]
                          [sort-ratio
                           (define (ratio a)
                             (/ (hash-ref (list-ref timess (car sort-ratio)) a)
                                (hash-ref (list-ref timess (cdr sort-ratio)) a)))
                           (< (ratio a) (ratio b))]
                          [else (symbol<? a b)])))))
  (define base-max (if normalize-max?
                       (for*/fold ([mx starting-max]) ([times (in-list timess)]
                                                       [t (in-hash-values times)])
                         (max mx t))
                       starting-max))
  (define (overlay-detail bar total detail key)
    (cond
      [(not detail-spec) bar]
      [else
       (let loop ([bar bar] [delta 0] [spec detail-spec])
         (cond
           [(null? spec) bar]
           [else
            (define t (hash-ref (hash-ref detail key) (caar spec) 0))
            (define dbar (colorize (if vertical-bars?
                                       (filled-rectangle H
                                                         (* (pict-height bar) (/ t total)))
                                       (filled-rectangle (* (pict-width bar) (/ t total))
                                                         H))
                                   (cdar spec)))
            (loop (if vertical-bars?
                      (lb-superimpose bar
                                      (inset dbar 0 0 0 delta))
                      (rb-superimpose bar
                                      (inset dbar 0 0 delta 0)))
                  (+ delta (if vertical-bars? (pict-height dbar) (pict-width dbar)))
                  (cdr spec))]))]))
  (define (widen p) (if widen?
                        (let ([a (* 2/3 (pict-width p))])
                          (inset p (+ a pad-left) 0 a 0))
                        (if pad-left
                            (inset p pad-left 0 0 0)
                            p)))
  (define plots (for/list ([key (in-list keys)])
                  (define max-time (if pin-max?
                                       base-max
                                       (for/fold ([mx base-max]) ([times (in-list timess)])
                                         (max mx (hash-ref times key)))))
                  (vl-append
                   2
                   (or (key->pict key)
                       (let ([p (tt (symbol->string key))])
                         (if (r5rs? key)
                             (colorize p r5rs-color)
                             p)))
                   (widen
                    (apply
                     (if vertical-bars? hb-append vr-append)
                     bar-sep
                     (for/list ([name (in-list names)]
                                [color (in-list colors)]
                                [times (in-list timess)]
                                [detail (in-list details)]
                                [note (in-list (or notes (map (lambda (n) #f) names)))])
                       (define time (hash-ref times key))
                       ((if vertical-bars? (lambda (sep a b) (vc-append sep b a)) hc-append)
                        5
                        (let ([l (colorize name color)])
                          (let ([p (if ghost-label
                                       (if vertical-bars?
                                           (ctl-superimpose l (ghost ghost-label))
                                           (rtl-superimpose l (ghost ghost-label)))
                                       l)])
                            (if vertical-bars?
                                (let ([p2 (scale p (min 1 (/ H (pict-width p))))])
                                  (cc-superimpose p2 (blank 0 (pict-height p))))
                                p)))
                        (let ([lbl (scale (let ([tp (t (format "~a~a~a"
                                                             prefix
                                                             (~r (* time display-scale) #:precision decimal-places)
                                                             suffix))])
                                            tp)
                                          0.5)]
                              [bar (let ([bar (overlay-detail
                                               (colorize (if vertical-bars?
                                                             (filled-rectangle H (* W (/ time (max 1 max-time))))
                                                             (filled-rectangle (* W (/ time (max 1 max-time))) H))
                                                         color)
                                               time detail key)])
                                     (if (and pin-max? (time . > . max-time))
                                         (if vertical-bars?
                                             (inset bar 0 (- W (pict-height bar)) 0 0)
                                             (inset bar 0 0 (- W (pict-width bar)) 0))
                                         bar))])
                          (define (add-note bar note)
                            (if note
                                (refocus (hc-append 5
                                                    (lt-superimpose bar (blank W H))
                                                    (colorize (t note) color))
                                         bar)
                                bar))
                          (define labelled-bar
                             ((if vertical-bars? cb-superimpose lb-superimpose)
                              bar
                              (inset (colorize lbl "white")
                                     4)))
                          ((if vertical-bars? cb-superimpose lt-superimpose)
                           (pin-under (add-note (clip (refocus labelled-bar bar)) note)
                                      lbl lt-find
                                      (colorize lbl color))
                           (if vertical-bars?
                               (blank H (if fill-vertical? W 0))
                               (blank W H)))))))))))
  (define (pad plots)
    (append plots
            (let ([n (remainder (length plots) columns)])
              (if (zero? n)
                  null
                  (make-list (- columns n) (blank))))))
  (define spaced-plots ; start R5RS on new row
    (if (ormap r5rs? keys)
        (let loop ([keys keys] [plots plots] [rev-plots '()])
          (cond
            [(r5rs? (car keys))
             (append (pad (reverse rev-plots)) plots)]
            [else (loop (cdr keys) (cdr plots) (cons (car plots) rev-plots))]))
        plots))
  (table columns
         (pad spaced-plots)
         cc-superimpose cc-superimpose
         (* 2 gap-size) gap-size))

(define (single-plot label-str
                     #:t t #:tt tt #:it [it #f] #:gap-size gap-size
                     #:cs cs
                     #:r r
                     #:r-cify [r-cify #f]
                     #:r6 [r6 #f]
                     #:r-all [r-all #f]
                     #:r-jit [r-jit #f]
                     #:max max
                     #:display-scale [display-scale 1]
                     #:suffix [suffix ""]
                     #:vertical-bars? [vertical-bars? #f]
                     #:fill-vertical? [fill-vertical? #t]
                     #:widen? [widen? vertical-bars?]
                     #:width [width 200]
                     #:pad-left [pad-left 0]
                     #:label [label #f]
                     #:desc [desc ""]
                     #:desc-indest [desc-inset 0]
                     #:decimal-places [decimal-places 0])
  (define key (string->symbol label-str))
  (define (mk n) (if n (list (hash key n)) null))
  (define (add-desc desc p)
    (rt-superimpose
     (hbl-append (scale ((or it t) desc) 0.75) (t ""))
     (inset p desc-inset 0)))
  (define bar-sep (* (/ 5 24) gap-size))
  (add-desc
   desc
   (plot #:t t #:tt tt #:gap-size gap-size #:bar-sep bar-sep
         #:normalize-max? #t
         #:max max
         #:columns 1
         #:width width
         #:display-scale display-scale
         #:suffix suffix
         #:vertical-bars? vertical-bars?
         #:fill-vertical? fill-vertical?
         #:widen? widen?
         #:pad-left pad-left
         #:key->pict (lambda (k) label)
         #:decimal-places decimal-places
         (append
          (list (t "R/CS") (t "R"))
          (if r-cify (list (t "R/cify")) null)
          (if r6 (list (t "Rv6")) null)
          (if r-all (list (t "R -d")) null)
          (if r-jit (list (t "R/JIT!")) null))
         (append
          (list racket-color c-color)
          (if r-cify (list plain-c-color) null)
          (if r6 (list r6-color) null)
          (if r-all (list r-jit-color) null)
          (if r-jit (list r-jit-color) null))
         (append
          (list (hash key cs)
                (hash key r))
          (mk r-cify)
          (mk r6)
          (mk r-all)
          (mk r-jit)))))
