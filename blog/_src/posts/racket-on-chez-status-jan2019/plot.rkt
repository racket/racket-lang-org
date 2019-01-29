#lang racket/base
(require pict
         racket/list
         "color.rkt")

(provide plot
         get-times

         r5rs-color)

(define (get-times f)
  (struct entry (total count))
  (define all-times
    (call-with-input-file*
     f
     (lambda (i)
       (for/fold ([ht '#hasheq()]) ([i (in-port read i)])
         (define k (rename (cadr i)))
         (define t (caaddr i))
         (define e (hash-ref ht k (entry 0 0)))
         (if t
             (hash-set ht k (entry (+ (entry-total e) t)
                                   (+ (entry-count e) 1)))
             ht)))))
  (for/hasheq ([(k e) (in-hash all-times)]
               #:unless (or (eq? k 'nothing)
                            (eq? k 'hello)))
    (values k (/ (entry-total e) (entry-count e)))))

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
              #:columns [columns 2]
              #:width [W 200]
              #:bar-sep [bar-sep 0]
              #:normalize-max? [normalize-max? #f]
              #:max [starting-max 0]
              #:sort-ratio [sort-ratio #f]
              #:key->pict [key->pict (lambda (k) #f)]
              #:reverse? [reverse? #f]
              #:ghost-label [ghost-label #f]
              #:details [details (map (lambda (v) #hasheq()) timess)]
              #:detail-spec [detail-spec #f]
              #:display-scale [display-scale 1]
              #:vertical-bars? [vertical-bars? #f]
              #:suffix [suffix ""])
  (define H (if vertical-bars? 70 30))
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
            (define dbar (colorize (filled-rectangle (* (pict-width bar) (/ t total))
                                                     H)
                                   (cdar spec)))
            (loop (rb-superimpose bar
                                  (inset dbar 0 0 delta 0))
                  (+ delta (pict-width dbar))
                  (cdr spec))]))]))
  (define (widen p) (if vertical-bars?
                        (inset p (* 2/3 (pict-width p)) 0)
                        p))
  (define plots (for/list ([key (in-list keys)])
                  (define max-time (for/fold ([mx base-max]) ([times (in-list timess)])
                                     (max mx (hash-ref times key))))
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
                                [detail (in-list details)])
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
                        ((if vertical-bars? cb-superimpose lt-superimpose)
                         ((if vertical-bars? cb-superimpose lb-superimpose)
                          (overlay-detail
                           (colorize (if vertical-bars?
                                         (filled-rectangle H (* W (/ time (max 1 max-time))))
                                         (filled-rectangle (* W (/ time (max 1 max-time))) H))
                                     color)
                           time detail key)
                          (inset (scale (let ([tp (t (format "~a~a" (floor (* time display-scale)) suffix))])
                                          (lt-superimpose
                                           (inset (colorize tp color) -1 -1 1 1)
                                           (inset (colorize tp color) 1 1 -1 -1)
                                           (colorize tp "white")))
                                        0.5)
                                 4))
                         (if vertical-bars?
                             (blank H 0)
                             (blank W H))))))))))
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
