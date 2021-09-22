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

(define slot-number 0)
(define (talk-time dtime)
 (set! slot-number (add1 slot-number))
 (local-require racket/string gregor)
 (match-define (list day times) (string-split dtime ","))
 (define d (match day
             ["Friday"   (date 2021 11 5)]
             ["Saturday" (date 2021 11 6)]
             ["Sunday"   (date 2021 11 7)]))
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
    `(script ([src      "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.27.0/moment.min.js"]) "")
    `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/moment-timezone/0.5.31/moment-timezone-with-data-10-year-range.js"]) "")
    @title{(chaperone (eleventh RacketCon))}
    @script{
$(document).ready(function () {
 var zone = moment.tz.guess();
 $("[data-slot-time]").each(function() {
  var date = new Date($(this).data("slot-time"));
  var localTime = moment.tz(date, zone).format("dddd, h:mma zz")
  $(this).html(localTime); }); }); }
    (body
     #:class "main"
     (banner
      (title-append
       @pagetitle[(faded "(chaperone")
                  (img #:style "width:80px; float: right"
                       #:src "https://racket-lang.org/img/racket-logo.svg")]
       @pagetitle[@'nbsp "(eleventh RacketCon)" (faded ")")])
      @subtitle{November 5-7, 2021}
      @subtitle[@faded{Online}])
(column

 (section
  @sectionHeader{Attending}

  @paragraph{Join the @a[#:href "https://gather.town/app/POxm4HbriLKltrzP/racketlang"]{Gather space}; everything is there!}

  @paragraph{@br{}}

  @paragraph{You don't need to register.}

  @paragraph{Really, everything you need to do is in the Gather.}

  @paragraph{Trust me, join it and you'll see people and the talks and everything you need.}

  )

 (section
  @sectionHeader{Friday, November 5th}

  @lecture[
#:when
@talk-time{Friday, 2:00pm}
#:who
@speaker{@bold{Virtual Biergarten}}
]

)

 (section
  @sectionHeader{Saturday, November 6th}
  
  @lecture[
#:when
@talk-time{Saturday, 10:00am}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]
  
  @lecture[
#:when
@talk-time{Saturday, 10:30am}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Saturday, 11:00am}]
  
  @lecture[
#:when
@talk-time{Saturday, 12:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @lecture[
#:when
@talk-time{Saturday, 1:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Saturday, 1:30pm}]
  
  @lecture[
#:when
@talk-time{Saturday, 2:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @lecture[
#:when
@talk-time{Saturday, 3:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Saturday, 3:30pm}]

  @lecture[
#:when
@talk-time{Saturday, 4:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]
  @lecture[
#:when
@talk-time{Saturday, 5:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Saturday, 5:30pm}]

)

 (section
  @sectionHeader{Sunday, November 7th}

  @lecture[
#:when
@talk-time{Sunday, 10:00am}
#:who
@speaker{Sam Tobin-Hochstadt}
#:link
#f
#:what
@talk{The State of Racket}
#:more
@abstract{

}]

  @lecture[
#:when
@talk-time{Sunday, 10:30am}
#:who
@speaker{Racket Management}
#:link
#f
#:what
@talk{Racket Town Hall}
#:more
@abstract{

Please come with your big questions and discussion topics.

}]

  @hallway[@talk-time{Sunday, 11:00am}]

  @lecture[
#:when
@talk-time{Sunday, 12:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]
  @lecture[
#:when
@talk-time{Sunday, 1:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Sunday, 1:30pm}]

  @lecture[
#:when
@talk-time{Sunday, 2:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]
  @lecture[
#:when
@talk-time{Sunday, 3:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Sunday, 3:30pm}]
  
  @lecture[
#:when
@talk-time{Sunday, 4:30pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]
  @lecture[
#:when
@talk-time{Sunday, 5:00pm}
#:who
@speaker{XXX}
#:link
#f
#:what
@talk{XXX}
#:more
@abstract{

XXX

}]

  @hallway[@talk-time{Sunday, 5:30pm}]

       )

 (section
  @sectionHeader{Previous RacketCons}
  @(apply larger
               (cdr
                (apply
                 append
                 (for/list ([year '(2020 2019 2018 2017 2016 2015 2014 2013 2012 2011)])
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
