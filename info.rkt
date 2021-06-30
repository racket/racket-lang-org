#lang info

(define collection "racket-lang-org")

(define deps
  '("slideshow-lib"
     "csv-reading"
     "typed-racket-lib"
     "datalog"
     "graph"
     "gui-lib"
     "base"
     ("plt-web-lib" #:version "1.3")
     "at-exp-lib"
     "net-lib"
     "racket-index"
     ("scribble-lib" #:version "1.28")
     "syntax-color-lib"
     "plot-gui-lib"
     "plot-lib"
     "math-lib"
     "pollen"
     "css-tools"
     "sugar"
     "txexpr"
     "gregor-lib"
     "frog"
     "rackunit-lib"
     "pict-lib"
     "ppict"
     "draw-lib"
     ("s3-sync" #:version "1.11")))

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth stamourv))

(define test-omit-paths '("sync.rkt"))
