#lang at-exp racket/base

;; Announcing a given upcoming RCon including a simple call for participation

(require racket/match
         racket/string
         txexpr
         (prefix-in gregor: gregor)
         "lib.rkt"
         "commonelements.rkt")

(provide page)

;; ------------------------------------------------------------
(define (meta #:itemprop [itemprop #f]
              content)
  (define elem (txexpr* 'meta (list (list 'content content))))
  (cond [(non-empty-string? itemprop)
         (attr-set elem 'itemprop itemprop)]
        [else elem]))


(define page
  (html #:lang "en"
   (head
    (head-meta #:http-equiv "content-type" #:content "text/html; charset=utf-8")
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
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
