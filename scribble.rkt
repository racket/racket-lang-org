#lang racket/base
(require scribble/render)

(provide scribble-page)

(define (scribble-page .scrbl dest-dir
                       #:name [name "index"])
  (define doc (dynamic-require .scrbl 'doc))
  (render (list doc)
          (list name)
          #:dest-dir dest-dir))
