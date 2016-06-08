#lang at-exp racket/base
(require "svg.rkt")
(provide (all-defined-out))
(define metas (make-hash))
(define doc (make-doc-from-file "speakers-raw.svg"))
