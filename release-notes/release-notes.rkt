#lang at-exp racket

;; this file represents the release notes, and generates both
;; the txt file format used for the announcement.txt file and
;; the markdown used for the blog post.

(require "render-release-notes.rkt"
         "check-links.rkt")

(define major-v 8)
(define minor-v 17)

(define version (~a "v"major-v"."minor-v))

;; call (go) to generate the release-notes files; this overwrites several paths in
;; /tmp

(define blog-post-url
  (match* (major-v minor-v) 
    [(8 16) "https://blog.racket-lang.org/2025/01/racket-v8-16.html"]
    [(8 17) "https://blog.racket-lang.org/2025/05/racket-v8-17.html"]))




;; inferred url abstraction...

(define (dur str)
  (string-append "https://docs.racket-lang.org/" str))
(define (rur str)
  (dur (string-append "reference/" str)))

(define dr-core-url
  "https://github.com/racket/drracket/commit/ae16d6bc6e00a9498313cff035537ac98ef71194")

(define bfs-url
  (rur "generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._bitwise-first-bit-set%29%29"))


(define bullets
  (list

@bullet{The new @link[dr-core-url]{`drracket-core`} package provides a version of drracket with
   a smaller set of dependencies.}

@bullet{Typed Racket has support for @link[(rur "treelist.html")]{treelists}.}

@bullet{The package manager computes @link[(dur "pkg/Package_Concepts.html")]{checksums} for packages when required,
   allowing the use and automatic upgrade of packages without them.}

@bullet{The @link[bfs-url]{`bitwise-first-bit-set`} function returns the smallest bit that is
   set in the twos-complement representation of the given number.}

@bullet{The updated @link[(rur "Module_Names_and_Loading.html#%28def._%28%28quote._~23~25kernel%29._dynamic-require%29%29")]{`dynamic-require`} function makes it easier to use
   syntax bindings by allowing a syntax-thunk (or 'eval) to be used for them.}

@bullet{The @link[(rur "exns.html#%28def._%28%28quote._~23~25kernel%29._error-module-path-~3estring-handler%29%29")]{`error-module-path->string-handler`} parameter allows the customization
   of the display of module-paths in error messages.}

@bullet{Precision of certain @link[(rur "generic-numbers.html")]{numeric functions} (`sin`, `cos`, and others) is
   improved on Windows platforms by using the MSVCRT/UCRT libraries.}
  
@bullet{The @link[(rur "strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29")]{`string-append`} function has improved performance and reduced memory
   use for long lists of strings in the Racket CS implementation.
   Differences are clearly noticeable for lists of length 1 million.}

@bullet{@link[(rur "tcp.html")]{TCP ports} use `SO_KEEPALIVE`, instructing the kernel to send periodic
   messages while waiting for data to check whether the connection
   is still responsive.}

@bullet{Racket code using a terminal in Windows can receive mouse events as
 virtual terminal characters after using SetConsoleMode. (This is also
 already possible on macOS and Linux.) See the
 @link["https://docs.racket-lang.org/tui-term/index.html"]{tui-term} package
 for related example code.}

@bullet{The @link[(dur "json/index.html#%28part._.Parsing_.J.S.O.N_.Text_into_.J.S-.Expressions%29")]{`#:replace-malformed-surrogate?`} keyword can be used to specify
   a replacement for malformed unicode surrogates in JSON input}

@bullet{The @link[(dur "http-client/index.html")]{http-client} module no longer sends "Content-Length: 0" for
   requests without a body.}

@bullet{The @link[(dur "raco/demod.html")]{demodularizer} (`compiler/demod`) can prune more unused assignments}

@bullet{Several judgment rendering forms in @link[(dur "redex/index.html")]{Redex} are replaced by functions, allowing
   more convenient abstraction.}

@bullet{When a distribution includes no teaching languages, DrRacket’s language-dialog
 configuration moves into the preferences dialog and the “Language” menu disappears.}

@bullet{The @link[(dur "math/index.html")]{math library} has better support for block-diagonal matrices, including
 both Racket and Typed Racket.}

@bullet{The @link[(dur "math/index.html")]{math library} contains improved implementations of acos and
   matrix-(cos-)angle.}

@bullet{The @link[(dur "stepper/index.html")]{stepper} again works for @link[(dur "teachpack/2htdpuniverse.html#(part._world._interactive)")]{`big-bang`} programs.}

@bullet{There are many other repairs and documentation imprevements!
}



@;{   @sub-bullet{@link[(rur "treelist.html#%28part._.Mutable_.Treelists%29")]{Mutable treelists} are @link[(rur "serialization.html")]{serializable}.}}




  
  ))

(define contributors
  (list "Alexander Shopov" "Andrei Dorian Duma" "Bert De Ketelaere" "Bob Burger" "Bogdan Popa"
        "Bogdana Vereha" "Cameron Moy" "Chung-chieh Shan" "Cutie Deng" "D. Ben Knoble" "Dario Hamidi"
        "Dominik Pantůček" "Gustavo Massaccesi" "halfminami" "Jacqueline Firth" "Jason Hemann"
        "Jens Axel Søgaard" "Joel Dueck" "John Clements" "Jordan Harman" "Marc Nieper-Wißkirchen"
        "Matthew Flatt" "Matthias Felleisen" "Mike Sperber" "Noah Ma" "owaddell-ib" "Philippe Meunier"
        "Robby Findler" "Ryan Culpepper" "Ryan Ficklin" "Sam Phillips" "Sam Tobin-Hochstadt" "Shu-Hung You"
        "sogaiu" "Sorawee Porncharoenwase" "Stephen De Gabrielle" "Vincent Lee" "Wing Hei Chan"))

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


