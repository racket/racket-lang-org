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
    [(9 1) "https://blog.racket-lang.org/2026/02/racket-v9-1.html"]))


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


   @bullet{Documentation organization and navigation can be specialized by
language family, to allow users to interact with documentation in a way that
is tailored to that language family. This is currently used by Rhombus.}

@bullet{The `for` form and its variants accept an `#:on-length-mismatch` specifier.}

@bullet{DrRacket improves the handling of the dark mode preferences,
and improves the GUI for choosing color schemes.}

@bullet{DrRacket has curved syntax arrows. The degree of curvature
indicates the relative left- or right-displacement of the arrow's target.}

@bullet{DrRacket's "Insert Large Letters" uses characters that match the
comment syntax of the buffer's language.}

@bullet{The `exn-classify-errno` maps network and filesystem error numbers on various platforms
 to posix-standard symbols, to enable more portable code.}

@bullet{The behavior of Racket BC on certain character operations (most notably `eq?`) is changed
 to match that of Racket CS, with a small performance penalty for these operations for BC programs.}

@bullet{The `make-struct-type` procedure can inherit the current inspector using a `'current`
flag. This is the default behavior, but there are situations in which it's not possible
to refer to the current inspector.}

@bullet{Bundle configurations can better control the conventions for locating shared object
files with the `--enable-sofind=<conv>` flags.}

@bullet{The `system-type` function can report on platform and shared-object-library conventions
with new flags.}

@bullet{The `openssl/legacy` library makes it possible to access OpenSSL's built-in "legacy"
provider, to get access to insecure and outdated algorithms.}

@bullet{DrRacket's contour window width has become sensitive to the width
of the text in the window it is tracking.}

@bullet{Typed Racket improves expected type propagation for keyword argument functions.}

@bullet{There are many other repairs and documentation improvements!}
  
  ))

(define contributors
  '("Alexander Shopov"
    "beast-hacker"
    "Bob Burger"
    "Brad Lucier"
    "Cadence Ember"
    "David Van Horn"
    "evan"
    "François-René Rideau"
    "Gustavo Massaccesi"
    "Jacqueline Firth"
    "Jade Sailor"
    "Jason Hemann"
    "Jens Axel Søgaard"
    "John Clements"
    "Jonas Rinke"
    "Matthew Flatt"
    "Matthias Felleisen"
    "Mike Sperber"
    "Noah Ma"
    "Pavel Panchekha"
    "Rob Durst"
    "Robby Findler"
    "Ryan Culpepper"
    "Sam Tobin-Hochstadt"
    "Stephen De Gabrielle"
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
