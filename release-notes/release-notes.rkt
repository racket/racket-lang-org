#lang at-exp racket

;; this file represents the release notes, and generates both
;; the txt file format used for the announcement.txt file and
;; the markdown used for the blog post.

(require "render-release-notes.rkt"
         "check-links.rkt"
         rackunit)

(define major-v 9)
(define minor-v 3)

(define version (~a "v"major-v"."minor-v))


;; call (go) to generate the release-notes files; this overwrites several paths in
;; /tmp

(define txt-file-path "/tmp/release-notes.txt")
(define md-file-path "/tmp/release-notes.md")

(define blog-post-url
  (let ()
    (match-define (list year-str month-str)
      (match* (major-v minor-v) 
        [(8 16) "https://blog.racket-lang.org/2025/01/racket-v8-16.html"]
        [(8 17) "https://blog.racket-lang.org/2025/05/racket-v8-17.html"]
        [(8 18) "https://blog.racket-lang.org/2025/08/racket-v8-18.html"]
        [(9 0) "https://blog.racket-lang.org/2025/11/racket-v9-0.html"]
        ;; lines above this need to be edited, if ever needed...
        [(9 1) '("2026" "02")]
        [(9 2) '("2026" "05")]
        [(9 3) '("2026" "08")]))
    (~a "https://blog.racket-lang.org/"year-str"/"
        month-str"/racket-v"major-v"-"minor-v".html")))


;; inferred url abstraction...

;; prepend docs/ url prefix:
(define (durl str)
  (string-append "https://docs.racket-lang.org/" str))
;; given pkg and str, prepend docs/ url, then pkg then / then str
(define (pkg-url pkg str) (durl (string-append pkg "/" str)))
;; fix previous for reference
(define (rurl str)        (pkg-url "reference" str))

(define dr-core-url
  "https://github.com/racket/drracket/commit/ae16d6bc6e00a9498313cff035537ac98ef71194")

(define bfs-url
  (rurl "generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._bitwise-first-bit-set%29%29"))

;; transforms a nested list into a string using left-parens ("&28") and right-parens ("&29")
(define (url-flatten d)
  (cond [(list? d)
         (string-append LP (apply string-append (map url-flatten d)) RP)]
        [else
         d]))

(define LP "%28")
(define RP "%29")
(define DU "._")
(define qk  (list "quote._~23~25kernel"))
(define lrm (list "lib._racket%2Fmatch..rkt"))
(define shp (list "lib._scribble%2Fhtml-properties..rkt"))

(define (maker page kind lib name)
  (apply
   string-append
   (map url-flatten (list page ".html" "#" (list kind DU (list lib DU name))))))


(define (rmaker page kind lib name)
  (rurl (maker page kind lib name)))

;; e.g. (rmaker "generic-numbers" "def"  qk  "bitwise-first-bit-set")
;; yields "https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._bitwise-first-bit-set%29%29"

(define bfbs-url  (rmaker "generic-numbers" "def"  qk  "bitwise-first-bit-set"))
(define tfp-url   (rmaker "port-buffers"    "def"  qk  "terminal-file-position"))
(define match-url (rmaker "match"           "form" lrm "match"))
(define fi-url    (rmaker "foreign-inline"  "form" qk  "~23~25foreign-inline"))
(define ippp-url  (rmaker "chaperones"      "def"  qk
                          "impersonator-property-predicate-procedure~3f"))

(define is-url (pkg-url "scribble" (maker "core" "def" shp "initial-scale")))

(define kernel-url   (rurl "Kernel_Forms_and_Functions.html"))
(define stepper-url  (durl "stepper/index.html"))
(define scribble-url (durl "scribble/index.html"))

(define (allthesame . args)
  (cond [(or (= (length args) 1) (apply equal? args))
         (first args)]
        [else
         (eprintf "all ~v are not the same!\n" (length args))
         (map displayln args)]))


(define raco-setup-url (pkg-url "raco" "running.html"))
(define teaching-langs-url "https://docs.racket-lang.org/htdp-langs/index.html")
(define check-stx-button-url "https://docs.racket-lang.org/drracket/buttons.html#%28idx._%28gentag._8._%28lib._scribblings%2Fdrracket%2Fdrracket..scrbl%29%29%29")
(define raco-pkg-install-url "https://docs.racket-lang.org/pkg/cmdline.html#%28part._raco-pkg-install%29")
(define define-runtime-lib-url "https://docs.racket-lang.org/foreign/runtime-lib.html#%28form._%28%28lib._ffi%2Funsafe%2Fruntime-lib..rkt%29._define-runtime-lib%29%29")
(define prompt-tag-c-url "https://docs.racket-lang.org/reference/data-structure-contracts.html#%28form._%28%28lib._racket%2Fcontract%2Fprivate%2Fmisc..rkt%29._prompt-tag%2Fc%29%29")
(define impersonate-prompt-tag-url
 "https://docs.racket-lang.org/reference/chaperones.html#%28def._%28%28quote._~23~25kernel%29._impersonate-prompt-tag%29%29")
(define es2sh-url "https://docs.racket-lang.org/reference/exns.html#%28def._%28%28quote._~23~25kernel%29._error-syntax-~3esrcloc-handler%29%29")
(define tcp-listen-url "https://docs.racket-lang.org/reference/tcp.html#%28def._%28%28lib._racket%2Ftcp..rkt%29._tcp-listen%29%29")
(define racket-base-url "https://docs.racket-lang.org/reference/index.html")
(define zip-entry-url "https://docs.racket-lang.org/file/zip.html#%28def._%28%28lib._file%2Fzip..rkt%29._zip-entry~3f%29%29")

(check-equal?
 tfp-url
 "https://docs.racket-lang.org/reference/port-buffers.html#%28def._\
%28%28quote._~23~25kernel%29._terminal-file-position%29%29")

(define racket-lang-core-url
  "https://racket-lang.org")

(define (l url term)
  (link url (string-append "`" term "`")))

(define bullets
  (list
      @bullet{The @l[raco-setup-url]{`raco setup`} command can generate markdown
 documentation, using the `--doc-markdown` option.}
      @bullet{The "#lang" @l[teaching-langs-url]{teaching languages} (BSL, ..., ISL+;
 plus DeinProgram) have reached parity with the ones chosen using the
 Language dialog, and are the recommended choice.}
   @bullet{DrRacket's background expansion disables errortrace
 annotations, for faster @l[check-stx-button-url]{syntax checking}.}
   @bullet{The @l[raco-pkg-install-url]{`raco pkg install`} command includes
 new options that provide
 more install-time configuration flexibility: `--adjacent-deps`, `--destdir`,
 and `--attach`, and a refined `--skip-installed`.}
   @bullet{The `ffi/unsafe/runtime-lib` library provides a @l[define-runtime-lib-url]{`define-runtime-lib`}
 mechanism similar to `define-runtime-path`, allowing location of libraries located relative
to a source file.}
   @bullet{The @l[prompt-tag-c-url]{`prompt-tag/c`} contract generator no longer performs checking on `call/cc`
when the `#:call/cc` option is not present.}
   @bullet{The @l[impersonate-prompt-tag-url]{`impersonate-prompt-tag`} function takes an additional argument that allows
checking and update of results for composable continuations.}
   @bullet{The @l[es2sh-url]`error-syntax->srcloc-handler` parameter provides control over the mapping
from syntactic forms to source locations for error handling.}
   @bullet{Uses of `(@l[tcp-listen-url]{tcp-listen} 0)` will retry when it fails with "address in use".}
   @bullet{The @l[racket-base-url]{`racket/base`} module requires fewer internal modules and instantiations.}
   @bullet{The `file/zip` package provides a @l[zip-entry-url]{new mechanism} for greatly increased control
over zip file generation, allowing in-memory file sources and per-file compression control.}

   

   )
  #;(list

   @bullet{The @l[match-url]{match} form checks that when non-linear patterns
 (patterns where the same variable is used multiple times) are used with `...`, the two parts of the
 matched value actually are equal. Additionally, match rejects non-linear patterns
 where one use of the variable is used with `...` and another is not. @link["https://github.com/racket/racket/pull/5467"]{This repair
 could cause existing code to fail}.}
  
  ))

;; probably always add Stephen De Gabrielle!
(define contributors
  '("Alex Knauth"
    "Alexander Shopov"
    "Aris Spathis"
    "Bert De Ketelaere"
    "Bob Burger"
    "Caleb Mazalevskis"
    "Cameron Moy"
    "Geoffrey J. Teale"
    "Gustavo Massaccesi"
    "Hannes Braun"
    "Jade Sailor"
    "Jason Hemann"
    "Jens Axel Søgaard"
    "John Clements"
    "Jordan Johnson"
    "Matthew Flatt"
    "Matthias Felleisen"
    "Mike Sperber"
    "Nathan Dykman"
    "Noah Ma"
    "Philip McGrath"
    "Robby Findler"
    "Romeo Ahmed"
    "Sam Tobin-Hochstadt"
    "Shu-Hung You"
    "Stefan Schwarzer"
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
