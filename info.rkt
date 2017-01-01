#lang info

(define collection "racket-lang-org")

(define deps '("graph"
               "gui-lib"
               "base"
               ("plt-web-lib" #:version "1.2")
               "at-exp-lib"
               "net-lib"
               "racket-index"
               "scribble-lib"
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
               "openssl/sha1"
               ("s3-sync" #:version "1.10")))

(define pkg-desc "Sources for http://racket-lang.org")

(define pkg-authors '(eli mflatt samth stamourv))

(define test-omit-paths '("sync.rkt"))
