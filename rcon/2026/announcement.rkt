#lang at-exp racket/base

(require racket/match
         racket/string
         xml
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt")

(provide page)

(define-div main
  [font-family "'Lexend', sans-serif"]
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
  [margin-top "1em"]
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

;; ------------------------------------------------------------
(define (meta #:itemprop [itemprop #f]
              content)
  (define elem (txexpr* 'meta (list (list 'content content))))
  (cond [(non-empty-string? itemprop)
         (attr-set elem 'itemprop itemprop)]
        [else elem]))

(define saturday (gregor:date 2026 10 3))
(define sunday (gregor:date 2026 10 4))
(define location "Oakland, California, USA")

(define page
  (html #:lang "en"
   (head
    (head-meta #:http-equiv "content-type" #:content "text/html; charset=utf-8")
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.3/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.34/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(sixteenth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/New_York").format("dddd, h:mma zz")
  $(this).html(localTime); }); }); })
    (body
     #:class "main h-event"
     #:itemscope ""
     #:itemtype "https://schema.org/Event"
     (meta #:itemprop "startDate" (gregor:~t saturday "y-MM-d"))
     (meta #:itemprop "endDate" (gregor:~t sunday "y-MM-d"))
     (meta #:itemprop "location" location)
(content
     (banner
      (title-container
       (title-append
        @pagetitle[(img #:style "width:140px; float: right"
                        #:src "rcon2026logo.png"
                        #:alt "The Racket logo")]
        @pagetitle["(sixteenth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp]))
      @subtitle{October 3-4, 2026}
      @specific-location{@p-location{Oakland, California, USA}}
      @specific-location{Oakstop, @a[#:href "localinfo.html"]{Broadway Gallery Suite}}
      )

(txexpr* 'time `((class "dt-start") (hidden "") (datetime ,(gregor:~t saturday "y-MM-dd"))))
(txexpr* 'time `((class "dt-end") (hidden "") (datetime ,(gregor:~t sunday "y-MM-dd"))))

(column
 (top-section
   @para{RacketCon is a public gathering dedicated to fostering a
   vibrant, innovative, and inclusive community around the Racket
   programming language. We aim to create an exciting and enjoyable
   conference open to anyone interested in Racket, filled with inspiring
   content, reaching and engaging both the Racket community and the
   wider programming world.})

 (section
  @sectionHeader{Call for Presentations}
  @para{We are looking for @emph{you!} If you have an idea for
  a presentation you’d like to give, please submit your proposal using @(a #:href "https://forms.gle/4YG57adx5snEwVe27" "this form") or
  write to @(a #:href "mailto:con-organizers@racket-lang.org" #:title "Send mail to the
  RacketCon organizer" "the RacketCon organizers") for consideration.

 For more information about presentation format, video streaming details, volunteering and sponsorships, please
  see our detailed call for participation @(a #:href "https://racket.discourse.group/t/racketcon-2026-call-for-participation/4211" "here").
  All Racket-y ideas are welcome. We’d love to have you!})

 (top-section
   @para{@emph{As in previous years, RacketCon will be streamed for those unable to attend in person.
               Recordings will also be made available on YouTube some time after the conference.
               Streaming users will have the option to purchase a remote participation ticket to
               support the livestream. Previous RacketCon presentations can be found @(a #:href "https://www.youtube.com/racketlang/playlists" "here").}})

  (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon will take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Organisation}
   @paragraph{
              The RacketCon 2026 is organised by a team of volunteers.
              The organizers may be reached at @|mailto:con-organizers|.})

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2025 2024 2023 2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year)))))))))))))