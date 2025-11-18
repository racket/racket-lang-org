#lang at-exp racket

;; this file represents the release notes, and generates both
;; the txt file format used for the announcement.txt file and
;; the markdown used for the blog post.

(require "render-release-notes.rkt"
         "check-links.rkt")

(define major-v 9)
(define minor-v 0)

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
    [(9 0) "https://blog.racket-lang.org/2025/11/racket-v9-0.html"]))




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


@bullet{Racket supports parallel threads.}

@sub-bullet{Parallel threads can be created using the `#:pool` argument
 to thread creation.}
   
@sub-bullet{Threads created with `#:keep` set to `'results` will record their results
for later retrieval with `thread-wait`.}

@sub-bullet{A larger set of primitives can be used without blocking.}

@sub-bullet{Uninterruptible mode is enriched and constrained to work with
parallel threads, with adjustments around the use of `equal?`-based hash
tables and semaphores.}

@bullet{The `black-box` wrapper prevents the optimizing
 compiler from optimizing away certain computations entirely.
 This can be helpful in ensuring that benchmarks are
 accurate.}

@bullet{The `decompile-linklet` function can map linklets back to s-expressions.}

@bullet{When using BC Racket, the `processor-count` function is changed
 to always return the parallel count.}

@bullet{We now distribute "natipkg" packages for AArch64, useful for package-build
  and package-testing infrastructure.}

@bullet{Check Syntax tracks identifiers more deeply nested in the
 "origin" field of syntax objects.}

@bullet{The `math` library includes Weibull distributions.}

@bullet{There are many other repairs and documentation improvements!}
  
  ))

(define contributors
  '("Alexander Shopov"
    "Anthony Carrico"
    "Bert De Ketelaere"
    "Bogdan Popa"
    "Cadence Ember"
    "David Van Horn"
    "Gustavo Massaccesi"
    "Jade Sailor"
    "Jakub Zalewski"
    "Jens Axel Søgaard"
    "jestarray"
    "John Clements"
    "Jordan Johnson"
    "Matthew Flatt"
    "Matthias Felleisen"
    "Mike Sperber"
    "Philip McGrath"
    "RMOlive"
    "Robby Findler"
    "Ruifeng Xie"
    "Ryan Culpepper"
    "Sam Phillips"
    "Sam Tobin-Hochstadt"
    "Sebastian Rakel"
    "shenleban tongying"
    "Shu-Hung You"
    "Stephen De Gabrielle"
    "Steve Byan"
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
