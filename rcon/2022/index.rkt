#lang at-exp racket/base
(require racket/match
         xml
         "lib.rkt")

(define-div column
  ,@centered
  [width "45em"])

(define-div banner
  ,@centered)

(define-div title-append
  [display inline-block]
  (margin-left auto)
  (margin-right auto)
  [text-align left]
  [margin-bottom "1em"])

(define-div pagetitle
  [font-size 64]
  [font-family "'Source Code Pro', monospace"])

(define-div main
  [font-family "'Raleway', sans-serif"])

(define header-font
  `([font-weight "bold"]))

(define-div subtitle
  ,@centered
  [font-size 40]
  ,@header-font)

(define-div subsubtitle
  ,@centered
  [font-size 20]
  ,@header-font)

(define-div section
  [margin-top "3em"])
(define-div sectionHeader
  [font-size 24]
  [margin-bottom "1em"]
  ,@header-font)

(define-a speaker-a
  [font-size 24]
  [color "firebrick"])
(define-div talk
  [font-style "italic"]
  [font-size 24]
  [margin-top "0.25em"]
  [margin-bottom "1em"])

(define-div abstract
  [text-align "left"]
  [margin-left "5em"]
  [margin-right "5em"])

(define-div paragraph)
(define para paragraph)

(define-div plain)

(define-div larger
  [font-size 24])

(define-span bold
  [font-weight bold])

(define-span emph
  [font-style "italic"])

(define-span tt
  [font-family "'Source Code Pro', monospace"])

(define-span faded
  [color "gray"])

(define-div talk-time-div
  [font-weight bold]
  [position absolute]
  [color "gray"])

(define-div live-link
  [position absolute]
  [right 0]
  [top 0])

(define-div speech
  [margin-top "2em"]
  [position relative])

(define-div bio-div
  [margin-top "0.5em"])

(define (script . contents)
 `(script ,@(map (λ (x) (cdata #f #f x)) contents)))

(define (code content)
 `(code () ,content))

(define reg-form-url "https://form.typeform.com/to/oNbZByZQ")

;; ------------------------------------------------------------

(define which 0)
(define (speaker . x)
  (set! which (add1 which))
  (define label (format "slot~a" which))
  (apply speaker-a #:id label #:href (format "#~a" label) x))

(define (lecture #:when when
                 #:who who
                 #:link [l #f]
                 #:what [what ""]
                 #:more [more ""])
  (speech when
          who
          (if l
           (live-link "" (a #:href l "talk link"))
           "")
          what
          more))

(define (hallway when)
 (lecture #:when when #:who @speaker{@bold{Hallway}}))

(define (bio . contents)
 (apply bio-div @bold{Bio: } contents))

(define (q content)
  `(q '() ,content))

(define slot-number 0)
(define (talk-time dtime)
 (set! slot-number (add1 slot-number))
 (local-require racket/string gregor)
 (match-define (list day times) (string-split dtime ","))
 (define d (match day
             ["Friday"   (date 2022 10 28)]
             ["Saturday" (date 2022 10 28)]
             ["Sunday"   (date 2022 10 30)]))
 (define t (parse-time times " h:mmaa"))
 (define tz (with-timezone (on-date t d) "America/New_York"))
 (define m (adjust-timezone tz "Etc/UTC"))
 (talk-time-div
  `(span ([data-slot-time ,(moment->iso8601 m)])
    ,(~t tz "EEEE, h:mma zz"))))

;; ------------------------------------------------------------

(define page
  (html
   (head
    (link #:href fonts-url
          #:rel "stylesheet")
    (style (cdata #f #f (classes->string)))
    (style (cdata #f #f "a { text-decoration: none; } "))
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.3/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.34/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(twelfth RacketCon)}
    @script{
$(document).ready(function () {
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, "America/New_York").format("dddd, h:mma zz")
  $(this).html(localTime); }); }); }
    (body
     #:class "main"
     (banner
      (title-append
       @pagetitle[(img #:style "width:80px; float: right"
                       #:src "https://racket-lang.org/img/racket-logo.svg")]
       @pagetitle["(twelfth" (br) 'nbsp "RacketCon)" 'nbsp 'nbsp 'nbsp])
      @subtitle{October 28-30, 2022}
      @subtitle{Brown University}
      @subsubtitle{Providence, Rhode Island, USA})

(column

 (section
  @sectionHeader{Friday, October 28th}

  @paragraph{Friday afternoon is for hackathons! More details to follow.}

  @paragraph{This a tentative list. If you would like to suggest another one, please complete the @(a #:href reg-form-url "registration form").}

  @lecture[
#:when
@talk-time{Friday, 2:00pm}
#:who @speaker{Internals Nerds Unite}
#:what @talk{Hacking Racket Internals}
#:more
@abstract{Your chance to dig in to Racket internals and help contribute to Racket development! As the Racket community grows, there’s an increasing interest in not just making cool Racket programs, but getting into the guts of Racket itself. If that sounds interesting to you, this is your hackathon! Sam Tobin-Hochstadt will offer a brief kick-off crash course and stick around for answering questions.}
]

)

 (section
  @sectionHeader{Saturday, October 29th}

  @paragraph{The following speaker list incomplete! And for the talks that are listed, the talk times are just stub values. Come back later. If you would like to be a speaker, or nominate a speaker, please complete the @(a #:href reg-form-url "registration form").}

  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@(a #:href "https://pp.ipd.kit.edu/person.php?id=144" "Sebastian Ullrich")}
#:what
@talk{Metaprograms and Proofs: Macros in Lean 4}
#:more
@abstract{

A core feature of the @(a #:href "https://leanprover.github.io" "Lean 4 programming language") and theorem prover is
an expressive macro system, taking heavy inspiration from Racket. In
this talk, we give an overview of macros in Lean and discuss the ideas
we took from Racket as well as the problems we decided to solve in a
different way. In particular, we talk about recent work on @(q "typed macros")
that prevent many common mistakes by Lean macro authors.
}
]

  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@(a #:href "http://cs.brown.edu/people/bgreenma/" "Ben Greenman")}
#:what
@talk{TBA}
]

  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@(a #:href "https://www.igalia.com/team/pmatos" "Paulo Matos")}
#:what
@talk{TBA}
]

  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@(a #:href "https://www.shu.edu/profiles/marcomorazan.cfm" "Marco Morazán")}
#:what
@talk{TBA}
]

  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{@(a #:href "https://github.com/jackfirth" "Jack Firth")}
#:what
@talk{Resyntax}]

)

 (section
  @sectionHeader{Sunday, October 30th}

  @paragraph{The following list is incomplete and will be updated. If you would like to be a speaker, or nominate a speaker, please complete the @(a #:href reg-form-url "registration form").}

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{@(a #:href "http://cs.brown.edu/people/bgreenma/" "Ben Greenman")}
#:what
@talk{Summary of the Summer of @code{#lang}}
]

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{Sam Tobin-Hochstadt}
#:what
@talk{The State of Racket}
]

  @lecture[
#:when
@talk-time{Sunday, 10:30am}
#:who
@speaker{Racket Management}
#:what
@talk{Racket Town Hall}
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

       )

 (section
   @sectionHeader{Registration}
   @paragraph{To help gauge interest in the event, please complete the @(a #:href reg-form-url "registration form") if you would like to attend.}
  )

 (section
   @sectionHeader{Friendly Policy}
   @paragraph{The proceedings of RacketCon are expected to take place under the Racket @(a #:href "https://racket-lang.org/friendly.html" "Friendly Environment Policy").}
  )

 (section
   @sectionHeader{Getting There}
   @paragraph{The Brown Computer Science Department has put together @(a #:href "https://cs.brown.edu/about/directions/" "a great page") with instructions for how to reach the department by plane, train, or car. Once you’re at the right building, RacketCon signs will guide you to the RacketCon place.}
 )

 (section
  @sectionHeader{Accommodation}
  @paragraph{Stay wherever you want! There are plenty of hotels in the area.}

  @paragraph{That said, we have reserved blocks at two local hotels. In no particular order:}

  @ul{
    @li{@bold{Hampton Inn  & Suites} by Hilton Providence Downtown: 30 rooms for Friday, October 28, 2022-Monday, October 31, 2022. $149.00, per room, per night, plus 13% tax. Room options: 2 queen beds, or 1 king bed with sofabed. @bold{Deadline:} September 28, 2022. Amenities: Complimentary full hot breakfast, Wi-Fi, business center and fitness center. Valet parking $30.00 a day. @(a #:href "https://www.hilton.com/en/attend-my-event/pvdwyhx-bcr-103bbe07-4367-4aef-8cd5-0a6f6c5f418c/" "Booking Link")}
    @li{@bold{Homewood Suites} by Hilton Providence Downtown: 35 King Studio suites, or Friday, October 28, 2022-Monday, October 31, 2022. $189.00 per room per night plus 13% tax. King suites come with a king size bed, full size pull out sofa bed and full size kitchen. The Homewood requires a two night minimum on the Saturday, guests requested to either stay Friday & Saturday, or Saturday & Sunday. @bold{Deadline:} September 28, 2022. Amenities: Complimentary full hot breakfast, Wi-Fi, business center and fitness center. Valet parking is $30.00 a day. @(a #:href "www.my-event.hilton.com/pvdexhw-brg-9e543c15-7dce-4fb0-aaa5-1d1c1f4020ec/" "Booking Link")}
  }

  @paragraph{Guests are not required to book their room for all 3 nights. However, please note the 2-night requirement for Saturday October 29 at Homewood.}
 )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2021 2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
                   (list " ∙ "
                         (a #:href (format "https://con.racket-lang.org/~a/" year)
                            (format "~a" year)))))))))))))

;; ------------------------------------------------------------

(provide make)
(define (make p)
  (with-output-to-file
    (build-path p "index.html")
    #:exists 'replace
    (λ ()
      (write-xexpr page))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path here ".")
  (make here))
