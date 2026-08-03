#lang at-exp racket/base

;; Definitions of common styling elements

(require txexpr
         (prefix-in gregor: gregor)
         "lib.rkt")

(provide (all-defined-out))

(define-div main
  [font-family "'Lexend', sans-serif"]
  #;[background "#3e5ba9"]
  [background "WhiteSmoke"]
  [color "black"]
  [margin "0px"]
  [font-weight "200"]
  [font-size "14pt"])

(define-div content
  ,@centered
  [background "whitesmoke"]
  [margin-left "10ex"]
  [margin-right "10ex"]
  [padding-top "2ex"]
  [padding-bottom "2ex"])

(define-div column
  ,@centered
  [max-width "45em"])

(define-div banner
  [padding-top "1ex"]
  ,@centered)

(define-div title-container
  [display inline-block]
  [border-radius "25px"]
  [padding "8px"]
  [background "#f5dd06"]
  (margin-left auto)
  (margin-right auto)
  [text-align left]
  [margin-bottom "1em"])

(define-div title-append
  [display flex])

(define-div pagetitle
  [font-size "40pt"]
  [color "#005d00"]
  [font-family ,monospace])

(define header-font
  `(#;[font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size "32pt"]
  [font-weight "bold"]
  ,@header-font)

(define-div subsubtitle
  ,@centered
  [font-size "20pt"]
  ,@header-font)

(define-div section
  [margin-top "3em"])
(define-div sectionHeader
  [font-size "24pt"]
  [margin-bottom "1em"]
  [border-radius "5px"]
  #;[background "#CCCCEE"]
  [background "#9f1d20"]
  [color "#f5dd06"]
  [margin-left "-10ex"]
  [margin-right "-10ex"]
  ,@header-font)

(define-div top-section
  [margin-top "1em"])

(define-div speaker-a
  [color "firebrick"])
(define-a unaffiliated
  [color "inherit"])
(define-a h-card
  [color "inherit"])

(define-span activity)

(define-div talk
  [font-weight bold]
  #;
  [font-style "italic"]
  [font-size "24pt"]
  [margin-top "0.25em"]
  [margin-bottom "0.5em"]
  [color "gray"])

(define-div place
  [font-weight bold]
  #;
  [font-style "italic"]
  [font-size "24pt"]
  [margin-top "0.25em"]
  [color "gray"])

(define-div fromplace
  [font-size "16pt"]
  [font-weight "bold"]
  [text-align "left"]
  [margin-top "2ex"]
  ,@header-font)

(define-div place-address
  [margin-bottom "0.5em"])

(define-div abstract
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])

(define-div paragraph
  [text-align "left"])
(define-div center
  [text-align "center"])
(define para paragraph)

(define-div vpara
  [text-align "left"]
  [margin-top "2ex"])

(define-div plain)

(define-div joint
  [color "gray"])

(define-div larger
  [font-size "24pt"])

(define-div featuring
  [white-space "nowrap"]
  [font-size "18pt"]
  [margin-top "1em"]
  [color "blue"])

(define-span featured
  [font-weight bold])

(define-span bold
  [font-weight bold])

(define-span emph
  [font-style "italic"])
(define-span book-title
  [font-style "italic"])

(define-span tt
  [font-family ,monospace])

(define-span faded
  [color "gray"])

(define-span nop)

(define-div talk-time-div
  [font-weight bold]
  [position absolute]
  [color "gray"])

(define-div live-link
  [position absolute]
  [right 0]
  [top 0])

(define-div speech
  [margin-top "3em"]
  [position relative])

(define-div first-speech
  [margin-torp "1em"]
  [position relative])

(define-div bio-div
  [margin-top "0.5em"]
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])


(define-span bio-label
  [font-weight "bold"]
  [color "gray"])

(define-div keynote-speaker
  [font-size "24pt"]
  [font-weight "bold"])

(define-div nb
  [text-align "center"]
  [font-style "italic"])

(define-div p-location
  [font-size "24pt"]
  [font-weight "bold"])

(define-div specific-location
  [font-size "18pt"]
  [margin-top "0.25em"])

(define-div specific-location-cotd
  [font-size "18pt"])

(define-div picture
  [margin-top "2em"])

(define (script . contents)
 `(script ,@(map (λ (x) (cdata #f #f x)) contents)))

(define (code content)
 `(code () ,content))

(define mailto:con-organizers
  @(a #:href "mailto:con-organizers@racket-lang.org" "con-organizers@racket-lang.org"))
