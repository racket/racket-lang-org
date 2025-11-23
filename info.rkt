#lang info

(define collection "racket-lang-org")

(define deps
  '("at-exp-lib"
    "base"
    "css-tools"
    "csv-reading"
    "datalog"
    "draw-lib"
    "frog"
    "graph"
    "gregor-lib"
    "gui-lib"
    "math-lib"
    "net-lib"
    "pict-lib"
    "plot-gui-lib"
    "plot-lib"
    ["plt-web-lib" #:version "1.3"]
    "pollen"
    "ppict"
    "racket-index"
    "rackunit-lib"
    "rash-demos"
    ["s3-sync" #:version "1.13"]
    "aws"
    ["scribble-lib" #:version "1.28"]
    "slideshow-lib"
    "sugar"
    "syntax-color-lib"
    "txexpr"
    "typed-racket-lib"))

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth stamourv))

(define test-omit-paths '("sync.rkt" "www/hello-world.rkt"))
