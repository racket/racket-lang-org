#lang pollen

◊string->svg{ninth 
RacketCon}

◊(define this-rc-date "13–14 July 2019")

◊div[#:class "menu-container"]{
◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊xlink{Venue}}
◊h2{◊xlink{Speakers}}
◊h2{◊◊xlink{Register}}
◊;h2{◊xlink{Schedule}}
}

◊gap[1]


(ninth RacketCon) is the meeting for everyone interested in ◊link["https://racket-lang.org"]{Racket} — a ◊link["https://docs.racket-lang.org/quick/index.html"]{general-purpose programming language} that's also the ◊link["https://felleisen.org/matthias/manifesto/"]{world’s first ecosystem} for language-oriented programming. 

RacketCon is for developers, contributors, programmers, educators, and bystanders. It's an opportunity for all of us to share plans, ideas, and enthusiasm, and help shape the future of Racket.

◊(define heading-width 16)

◊;string->svg{keynote speaker}
◊;string->svg{Niko Matsakis}

◊gap[2]


◊h3{◊xtarget["venue"]{◊string->svg[#:width heading-width]{>(venue)}}}

Two firsts this year: 

1) RacketCon is located in Salt Lake City, Utah. We'll be at the Tessman Auditorium at ◊link["https://www.slcpl.org/"]{The City Library}. 

2) RacketCon happens right after Racket School. All Racket School tickets also include admission to RacketCon. For more info, visit the ◊link["https://school.racket-lang.org/"]{Racket School web page}.

◊gap[2]

◊h3{◊xtarget["register"]{◊string->svg[#:width heading-width]{>(register)}}}

◊schedule[
◊row{◊at{RacketCon only (2 days)} ◊desc{$69 base
$99 patron
$49 academic}}

◊row{◊at{◊link["https://school.racket-lang.org/#brw"]{Beautiful Racket workshop} + RacketCon (5 days total)} ◊desc{$299 base
$499 patron
$149 academic}}

◊row{◊at{◊link["https://school.racket-lang.org/#htdl"]{How to Design Languages} + RacketCon (7 days total)} ◊desc{$499 base
$699 patron
$249 academic}}
]

◊button[#:id "eventbrite-widget-modal-trigger-55663521090" #:type "button" #:class "buy-button"]{Buy ticket at Eventbrite}

◊strong{Base} tickets are available to all. 

◊strong{Patron} tickets cover our full cost of offering these events, plus a little extra to help support Racket development at large. Racket is part of the ◊link["https://sfconservancy.org/"]{Software Freedom Conservancy}.

◊strong{Academic} tickets are available to participants from academic institutions that need a subsidy.

Eventbrite fees are added during checkout.

◊gap[2]


◊h3{◊xtarget["speakers"]{◊string->svg[#:width heading-width]{>(speakers)}}}

To be announced soon!

◊gap[1]

◊h3{◊xtarget["speakers"]{◊string->svg[#:width (* heading-width 1.5)]{>(previous RacketCons)}}}

◊(define (conlink year) 
  (link (format "https://con.racket-lang.org/~a" year) year))

◊inline-list['div]{
◊conlink{2018}
◊conlink{2017}
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}

◊gap[1]

◊em{My dominant feeling at RacketCon was: “wait hold on are you trying to tell me you people been here doing this the whole time?”} 

Satisfied Customer, RacketCon 2018