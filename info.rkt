#lang info

(define collection "racket-lang-org")

(define deps '("base"
               "plt-web-lib"
               "at-exp-lib"
               "net-lib"
               "racket-index"
               "scribble-lib"
               "syntax-color-lib"
               "plot-gui-lib"
               "math-lib"
               "pollen")) ; for rcon

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth stamourv))

(define test-omit-paths '("sync.rkt"))
