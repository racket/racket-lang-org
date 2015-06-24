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
               "plot-lib"
               "math-lib"
               "pollen" ; for rcon
               "sugar"
               "txexpr"))

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth stamourv))

(define test-omit-paths '("sync.rkt"))
