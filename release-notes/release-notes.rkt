#lang at-exp racket

;; this file represents the release notes, and generates both
;; the txt file format used for the announcement.txt file and
;; the markdown used for the blog post.

(require "render-release-notes.rkt"
         "check-links.rkt")

(define major-v 9)
(define minor-v 1)

(define version (~a "v"major-v"."minor-v))


;; call (go) to generate the release-notes files; this overwrites several paths in
;; /tmp

(define txt-file-path "/tmp/release-notes.txt")
(define md-file-path "/tmp/release-notes.md")

(define blog-post-url
  (match* (major-v minor-v) 
    [(8 16) "https://blog.racket-lang.org/2025/01/racket-v8-16.html"]
    [(8 17) "https://blog.racket-lang.org/2025/05/racket-v8-17.html"]
    [(8 18) "https://blog.racket-lang.org/2025/08/racket-v8-18.html"]
    [(9 0) "https://blog.racket-lang.org/2025/11/racket-v9-0.html"]
    [(9 1) "https://blog.racket-lang.org/2026/02/racket-v9-1.html"]
    [(9 2) "https://blog.racket-lang.org/2026/05/racket-v9-1.html"]))


;; inferred url abstraction...

(define (dur str)
  (string-append "https://docs.racket-lang.org/" str))
(define (rur str)
  (dur (string-append "reference/" str)))

(define dr-core-url
  "https://github.com/racket/drracket/commit/ae16d6bc6e00a9498313cff035537ac98ef71194")

(define bfs-url
  (rur "generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._bitwise-first-bit-set%29%29"))

(define racket-lang-core-url
  "https://racket-lang.org")

(define bullets
  (list

   @bullet{Typed Racket's types for the `asin` and `acos` procedures correctly handle
 situations where the function produces a complex number, avoiding unsound results
 that were previously possible. This repair could cause existing code to fail.}

   @bullet{The updated `match` form checks that when non-linear patterns
 (patterns where the same variable is used multiple times) are used with `...`, the two parts of the
 matched value actually are equal. Additionally, match rejects non-linear patterns
 where one use of the variable is used with `...` and another is not. This repair
 could cause existing code to fail.}

   @bullet{Racket 9.2 uses Unicode 17.0 for character and string operations.}


   @bullet{This release includes internal support for a more static "ffi2" FFI
 (to be used in a future package).}

   @bullet{The `terminal-file-position` function counts bytes written to ports connected to
  a terminal, such as `stdin` and `stderr`.}

   @bullet{Cross-phase persistent modules allow more types of `quote`d
   data.}

   @bullet{The `#%foreign-inline` core syntactic form provides unsafe access to
 facilities provided at the linklet layer by a Racket implementation.}

   @bullet{The implementations of `member`, `memw`, `when`, `unless`,
  `let/ec`, and `cond` are rewritten to use only racket/kernel syntax}

  @bullet{The `impersonator-property-predicate-procedure?` function identifies
  procedures created by `make-impersonator-property`.}

  @bullet{In Typed Racket, polymorphic struct types are printed using type arguments
(e.g., `(Array Byte)`) rather than exposing an internal representation.}

  @bullet{The stepper's display of numbers better matches the language settings.}

   @bullet{Scribble documents that do not use the Racket-manual style get an
   `initial-scale` of 1.0, instead of the manual style's 0.8, but this
   can be configured using the `initial-scale` property.}

   @bullet{By default, margin notes appear inline for narrow
 displays in all styles, not just in the Racket-manual style.}


  ;; htdp

  @bullet{Big-bang programs distributed as .dmg files correctly handle the `close-on-stop`
feature.}

@bullet{There are many other repairs and documentation improvements!}
  
  ))

(define contributors
  '("Alexander Shopov"
    "Alexis King"
    "Asilo"
    "Bert De Ketelaere"
    "Bob Burger"
    "Bogdan Popa"
    "Chung-chieh Shan"
    "François-René Rideau"
    "Gustavo Massaccesi"
    "Ilya Klyuchnikov"
    "Jade Sailor"
    "Jamie Taylor"
    "John Clements"
    "Jonathan Simpson"
    "LS_Hower"
    "Matthew Flatt"
    "Matthias Felleisen"
    "Mike Sperber"
    "Pavel Panchekha"
    "Philippe Meunier"
    "RMOlive"
    "Robby Findler"
    "Roman Klochkov"
    "Sam Tobin-Hochstadt"
    "Shu-Hung You"
    "Tejas Sanap"
    "Vincent Lee"
    "Wing Hei Chan"))


(define (go)
  ;; abstraction between these two OBVIOUSLY possible, waiting on this until the first time
  ;; we need to change them...
  (with-output-to-file txt-file-path
    #:exists 'truncate
    (λ ()(displayln horizontal-bar)
      (newline)
      (for-each display-lines (map txt-render-bullet bullets))
      (newline)
      (displayln "The following people contributed to this release:")
      (newline)
      (for-each displayln (render-contributors contributors))
      (newline)
      (displayln horizontal-bar)))
  (with-output-to-file md-file-path
    #:exists 'truncate
    (λ ()(displayln horizontal-bar)
      (display-lines
       (list
        ""
        (~a "We are pleased to announce Racket "version" is now available from [https://download.racket-lang.org/](https://download.racket-lang.org).")
        ""
        "## As of this release:"
        ""))
      (for-each display-lines (map md-render-bullet bullets))
      (newline)
      (displayln "## Thank you")
      (newline)
      (displayln "The following people contributed to this release:")
      (newline)
      (displayln (render-contributors-md contributors))
      (newline)
      (displayln markdown-closing-block)
      (map displayln share-block))))

(define markdown-closing-block
  #<<|
**Racket** is a community developed open source project and we welcome new contributors. See [racket/README.md](https://github.com/racket/racket/blob/master/README.md#contributing)
to learn how you can be a part of this amazing project.

## Feedback Welcome

Questions and discussion welcome at the Racket community on [Discourse](https://racket.discourse.group/invites/VxkBcXY7yL) or
[Discord](https://discord.gg/6Zq8sH5).

## Please share

If you can  - please help get the word out to users and platform specific repo packagers

|
)

(define no-v-version (~a major-v"."minor-v))
(define blog-post-url-line
  (~a "Racket - the Language-Oriented Programming Language - version "no-v-version" is now available from https://download.racket-lang.org"))

(define blog-post-reference-line
  (~a "See "blog-post-url" for the release announcement and highlights."))


(define share-block
  (list
   "```"
   blog-post-url-line
   ""
   blog-post-reference-line
   "```"))

;; ensure that all links contained in the release bullets are "live", in
;; the sense that a head request returns a 200 okay response from the
;; corresponting server
(define (check-links)
  (define links
    (apply append
           (map bullet-links bullets)))
  (for ([l links])
    (define response (url-str-response l))
    (when (not (equal? response 'okay))
      (eprintf "fail:\n ~v\n ~v\n\n"
               response l))))
