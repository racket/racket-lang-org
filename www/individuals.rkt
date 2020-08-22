#lang racket/base

(require racket/list)
(require racket/string)

(define names
  '(
"David Alkire"
"Andrew Blinn"
"Bogdan Popa"
"Andrei Formiga"
"Martin Connolly"
"Mario Rodas"
"Joel Dueck"
"Sam Tobin-Hochstadt"
"John Donnellan"
"Sean Jensen-Grey"
"Brian Adkins"
"James Emmott"
"Xu Xue"
"Dyllon Gagnier"
"Stephen DeGabrielle"
"Ross McKinlay"
"Fred Fu"
"Jack Firth"
"S Furnace"
"Markéta Lisová"
"Robert Postill"
"Alex Harsányi"
"Hari Krish"
))

(define sorted (sort names string<=? #:key (compose second string-split)))

(for-each displayln sorted)
