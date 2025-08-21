#lang at-exp racket

;; this file represents the release notes, and generates both
;; the txt file format used for the announcement.txt file and
;; the markdown used for the blog post.

(require "render-release-notes.rkt"
         "check-links.rkt")

(define major-v 8)
(define minor-v 18)

(define version (~a "v"major-v"."minor-v))

;; call (go) to generate the release-notes files; this overwrites several paths in
;; /tmp

(define blog-post-url
  (match* (major-v minor-v) 
    [(8 16) "https://blog.racket-lang.org/2025/01/racket-v8-16.html"]
    [(8 17) "https://blog.racket-lang.org/2025/05/racket-v8-17.html"]
    [(8 18) "https://blog.racket-lang.org/2025/08/racket-v8-18.html"]))




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

@bullet{The @link[racket-lang-core-url]{racket-lang.org} website no longer distributes Racket BC bundles, but it includes pre-built
 bundles for two flavors of ARM linux, AArch64 and 32-bit ARMv6 VFP.}

@bullet{@link["https://docs.racket-lang.org/xml/index.html"]{XML structures} are serializable.}

@bullet{@link["https://docs.racket-lang.org/scribble/index.html"]{Scribble's} HTML generation conforms better to modern standards.}

@bullet{Racket uses Unicode 16.0 for character and string operations.}

@bullet{The @link["https://docs.racket-lang.org/redex/Testing.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._redex-check%29%29"]{`redex-check`}
 default generation strategy always uses random generation to supplement the enumerator.}

@bullet{@link["https://docs.racket-lang.org/drracket/editor.html"]{DrRacket} supports the use of shift-tab to go backward to previous indentation positions.}

@bullet{The @link["https://docs.racket-lang.org/macro-debugger/index.html#%28part._.Macro_.Stepper%29"]{macro stepper}
 supports the @link["https://docs.racket-lang.org/string-constants/index.html"]{string-constants library},
 allowing internationalization of the stepper itself.}

@bullet{The @link["https://docs.racket-lang.org/reference/define-struct.html"]{`struct`} form supports
 @link["https://docs.racket-lang.org/reference/define-struct.html#:~:text=The%20%23%3Aproperties%20option%2C%20which%20can%20be%20supplied%20multiple%20times%2C%20accepts%20multiple%20properties%20and%20their%20values%20as%20an%20association%20list."]{`#:properties prop-list-expr`},
 making it more convenient to attach multiple property values to a structure type.}

@bullet{Build-system improvements support containers registered at @link["https://hub.docker.com/u/racket"]{Docker Hub}
 to build for all platforms that have downloads from the main Racket download site; improvements also
 support Unix-style builds for Mac OS in the style of MacPorts.}

@bullet{The @link["https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._expt%29%29"]{`expt`}
 function produces a more accurate result when its first argument is a flonum and its
 second argument is an exact integer that has no equivalent flonum representation than it did in prior
 versions.}

@bullet{@link["https://docs.racket-lang.org/reference/tcp.html"]{TCP ports} use `SO_KEEPALIVE` correctly.}

@bullet{Unsafe code can use @link["https://docs.racket-lang.org/foreign/Atomic_Execution.html#%28tech._uninterruptible._mode%29"]{“uninterruptible mode”}
 instead of “atomic mode” to allow futures to run concurrently while preventing interruptions from other threads.}

@bullet{The @link["https://docs.racket-lang.org/net/imap.html"]{`net/imap`} library supports
 @link["https://docs.racket-lang.org/net/imap.html#%28def._%28%28lib._net%2Fimap..rkt%29._imap-move%29%29"]{IMAP's `move`} operation.}

@bullet{There are many other repairs and documentation improvements!}



@;{   @sub-bullet{@link[(rur "treelist.html#%28part._.Mutable_.Treelists%29")]{Mutable treelists} are @link[(rur "serialization.html")]{serializable}.}}




  
  ))

(define contributors
  (list "Bob Burger" "Bogdan Popa" "Carl Gay" "Chloé Vulquin" "D. Ben Knoble" "Dario Hamidi"
        "Gustavo Massaccesi" "Jacqueline Firth" "Jade Sailor" "Jarhmander" "Jason Hemann"
        "Jens Axel Søgaard" "Joel Dueck" "John Clements" "jyn" "Jörgen Brandt" "Mao Yifu" "Marc Nieper-Wißkirchen"
        "Matthew Flatt" "Matthias Felleisen" "Mike Sperber" "Noah Ma" "paralogismos" "Pavel Panchekha"
        "Philip McGrath" "Robby Findler" "Ryan Culpepper" "Sam Tobin-Hochstadt" "Shalok Shalom"
        "Steve Byan" "Vincent Lee" "Wing Hei Chan" "ZC Findler"))

(define (go)
  ;; abstraction between these two OBVIOUSLY possible, waiting on this until the first time
  ;; we need to change them...
  (with-output-to-file "/tmp/release-notes.txt"
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
  (with-output-to-file "/tmp/release-notes.md"
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
      (for-each displayln (render-contributors contributors))
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


