#lang racket/base
(require pict)

(provide make-stacks
         racket-color
         c-color
         scheme-color
         scrbl-color)

(define racket-color "blue")
(define scheme-color "purple")
(define linklet-color "SlateBlue")
(define c-color "red")
(define scrbl-color "forestgreen")

(define (make-stacks #:t [t (lambda (s) (text s 'swiss 12))]
                     #:tt [tt (lambda (s) (text s 'modern 12))]
                     #:stack-h [stack-h 100]
                     #:block-w [block-w 30]
                     #:block-sep [block-sep 2]
                     #:all-block-w [all-block-w block-w]
                     #:r [r values])

  ;; Racket 6.10.1.3
  ;;  C in src+foreign+gc: 219k [+libffi and other little pieces]
  (define racket-runtime-size 234)
  ;;              gc: 11k
  (define racket-runtime-size/gc 11)
  ;;              fun.c + struct.c: 17k
  (define racket-runtime-size/fun 11)
  ;;              thread + sema + future: 15k
  (define racket-runtime-size/thread 15)
  ;;              port + portfun + network: 15k
  (define racket-runtime-size/io 15)
  (define racket-runtime-size/rktio 14)
  ;;              regexp: 6k
  (define racket-runtime-size/regexp 6)
  ;;              expander + reader: 30k [about 6 files]
  (define racket-runtime-size/expander 30)
  ;;              compiler: 25k [estimate from compile.c + jit*.c]
  (define racket-runtime-size/compiler 23)
  ;;  Racket for distribution: 885k
  (define racket-distro-size 885)
  ;;           collects: 150k
  (define racket-collects-size 150)
  ;;           collects: 150k
  (define racket-docs-size 270)
  ;;           .scrbl: 270

  ;; Chez Scheme [maybe 1/4 is redundant]
  ;;  C: 18k [+libz]
  (define chez-scheme-kernel-size 18)
  (define chez-scheme-kernel-size/gc 2)
  ;;  Scheme: 97k [72 files]
  (define chez-scheme-size 97)
  ;;     compiler: 30k
  ;;       [= cpnanopass.ss cp0.ss x86.ss x86_64.ss arm32.ss np-languages.ss base-lang.ss cpletrec.ss cpvalid.ss]
  (define chez-scheme-size/compiler 30)
  ;;     expander & reader: 8k
  ;;       [first part of "syntax.ss" plus "read.ss"]
  (define chez-scheme-size/expander 8)

  ;; Racket7
  ;;  C in src+foreign+gc: 183k [delta = 37k]
  ;;  Racket:
  ;;    expander: 30k [4k is reader; about 225 files]
  ;;       [compare to 6k for Chez Scheme "syntax.ss" first half]
  ;;       [99k lines expanded + Schemified]
  (define expander-size 30)

  ;; Racket-on-Chez:
  ;;  Scheme:
  ;;    rumble: 15k [hash tables, control, FFI, struct, chaperones & impersonators]
  (define rumble-size 15)
  ;;  Racket:
  ;;    expander: 30k [4k is reader]
  ;;    thread: 5k [34 files]
  (define thread-size 5)
  ;;    io: 15k [> 143 files]
  (define io-size 15)
  ;;    regexp: 4k [31 files]
  (define regexp-size 4)
  ;;    schemify: 4k
  (define schemify-size 4)

  (define (stack title l
                 #:height [height (stack-height l)]
                 #:block-w [block-w block-w]
                 #:scale [sc 1]
                 #:align? [align? #t])
    (define h (stack-height l))
    (define blocks (map (block (/ height sc) block-w) l))
    (apply vc-append
           (if (pict? title) title (t title))
           (if (and align?
                    (h . < . height))
               (cons (ghost ((block height block-w) (part #:size (- height h))))
                     blocks)
               blocks)))

  (define (stack-height l)
    (apply + (map part-height l)))

  (define ((block height block-w) spec)
    (define h (* stack-h (/ (part-height spec) height)))
    (define cell (frame (cellophane
                         (colorize (filled-rectangle block-w (max 1 (- h block-sep block-sep)))
                                   (part-color spec))
                         0.5)
                        #:color (part-color spec)))
    (inset (let ([p (refocus (cc-superimpose cell
                                             (let ([lbl (t (part-name spec))])
                                               (scale lbl (min 1 (* 2 (/ (pict-height cell) (pict-height lbl)))))))
                             cell)])
             (refocus (ht-append 5 p (let ([dy (part-size-dy spec)])
                                       (inset (scale (t (format "~ak" (part-height spec))) 0.5)
                                              0 dy 0 (- dy))))
                      cell))
           0 block-sep))

  (define (part #:name [name ""]
                #:size size
                #:color [color racket-color]
                #:size-dy [size-dy 0])
    (hash 'name name 'size size 'color color 'size-dy size-dy))

  (define (part-height p)
    (hash-ref p 'size))

  (define (part-name p)
    (hash-ref p 'name))

  (define (part-color p)
    (hash-ref p 'color))

  (define (part-size-dy p)
    (hash-ref p 'size-dy))

  (define racket-v6-all-stack
    (list (part #:name "docs" #:size racket-docs-size #:color scrbl-color)
          (part #:name "pkgs" #:size (- racket-distro-size racket-collects-size))
          (part #:name "collects" #:size racket-collects-size)
          (part #:name "core" #:size racket-runtime-size #:color c-color)))

  (define racket-v6-all
    (stack "Racket v6" racket-v6-all-stack
           #:block-w all-block-w))

  (define racket-runtime-size/other
    (- racket-runtime-size
       racket-runtime-size/gc
       racket-runtime-size/fun
       racket-runtime-size/thread
       racket-runtime-size/io
       racket-runtime-size/rktio
       racket-runtime-size/regexp
       racket-runtime-size/expander
       racket-runtime-size/compiler))

  (define rktio
    (part #:name "rktio" #:size racket-runtime-size/rktio #:color c-color))

  (define expand/read/module "expander")
  
  (define racket-v6-stack
    (list (part #:name expand/read/module #:size racket-runtime-size/expander #:color c-color)
          (part #:name "regexp" #:size racket-runtime-size/regexp #:color c-color)
          (part #:name "io" #:size racket-runtime-size/io #:color c-color)
          rktio
          (part #:name "threads" #:size racket-runtime-size/thread #:color c-color)
          (part #:name "control+structs" #:size racket-runtime-size/fun #:color c-color)
          (part #:name "builtins" #:size racket-runtime-size/other #:color c-color)
          (part #:name "compiler+JIT" #:size racket-runtime-size/compiler #:color c-color)
          (part #:name "GC" #:size racket-runtime-size/gc #:color c-color)))

  (define racket-v7-stack
    (cons (part #:name expand/read/module #:size racket-runtime-size/expander)
          (cdr racket-v6-stack)))
  
  (define racket-v7 (stack (tt "cd racket7; make")
                           racket-v7-stack))
  
  (define chez-scheme-size/other
    (- chez-scheme-size
       chez-scheme-size/expander
       chez-scheme-size/compiler))

  (define chez-kernel-part
    (part #:name "kernel" #:size chez-scheme-kernel-size #:color c-color))

  (define chez-scheme-kernel-size/other
    (- chez-scheme-kernel-size
       chez-scheme-kernel-size/gc))

  (define chez-kernel-stack
    (list (part #:name "kernel" #:size chez-scheme-kernel-size/other #:color c-color)
          (part #:name "GC" #:size chez-scheme-kernel-size/gc #:color c-color)))

  (define racket-v6-core (stack "Racket v6 Core"
                                racket-v6-stack))

  (define (racket-v6-all-stretched n)
    (let ([p (stack "Racket v6" racket-v6-all-stack)])
      (refocus (cb-superimpose
                (ghost p)
                (stack "Racket v6"
                       racket-v6-all-stack
                       #:scale (let ([s (/ (stack-height racket-v6-all-stack)
                                           racket-runtime-size)])
                                 (+ 1 (* n (- s 1))))))
               p)))
  
  ;; ----------------------------------------

  (define chez-scheme-stack
    (append
     (list (part #:name expand/read/module #:size chez-scheme-size/expander #:color scheme-color)
           (part #:name "builtins" #:size chez-scheme-size/other #:color scheme-color)
           (part #:name "compiler" #:size chez-scheme-size/compiler #:color scheme-color))
     chez-kernel-stack))
  
  (define chez-scheme
    (stack "Chez Scheme"
           #:height (stack-height racket-v6-stack)
           chez-scheme-stack))

  (define basic-chez-scheme-stack
    (list (part #:name "compiler+builtins" #:size chez-scheme-size #:color scheme-color)
          chez-kernel-part))

  (define basic-chez-scheme
    (stack "Chez Scheme"
           #:height (stack-height racket-v6-stack)
           basic-chez-scheme-stack))

  ;; ----------------------------------------

  (define sources+rktio
    (list
     (part #:name expand/read/module #:size expander-size)
     (part #:name "schemify" #:size schemify-size #:size-dy -3)
     (part #:name "regexp" #:size regexp-size)
     (part #:name "io" #:size io-size)
     rktio
     (part #:name "threads" #:size thread-size)))

  (define sources
    (remq rktio sources+rktio))

  (define (make-racket-on-chez-stack chez-scheme-stack)
    (append
     (list
      (part #:name "main" #:size 1 #:color scheme-color #:size-dy -10))
     sources+rktio
     (list
      (part #:name "rumble" #:size rumble-size #:color scheme-color))
     chez-scheme-stack))

  (define racket-on-chez-stack (make-racket-on-chez-stack basic-chez-scheme-stack))
  (define detailed-racket-on-chez-stack (make-racket-on-chez-stack chez-scheme-stack))
  
  (define racket-on-chez
    (stack "Racket-on-Chez"
           #:height (stack-height racket-v6-stack)
           racket-on-chez-stack))

  (define detailed-racket-on-chez
    (stack "Racket-on-Chez"
           #:height (stack-height racket-v6-stack)
           detailed-racket-on-chez-stack))

  (values (r racket-v6-all) racket-v6-all-stretched
          (r racket-v6-core)
          (r racket-v7)
          (r chez-scheme)
          (r basic-chez-scheme)
          (r racket-on-chez)
          (r detailed-racket-on-chez)))
