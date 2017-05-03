#lang pollen
◊(require racket/file pollen/cache)



◊head["a"]{Seventh}
◊head["b"]{RacketCon}
◊head["c"]{  Seattle 2017}

◊gap[3]

◊(define this-rc-date "7–8 October 2017")

◊h2{◊xlink["register"]{◊this-rc-date}}
◊h2{◊link["https://goo.gl/maps/Dja57qoLBvs"]{University of Washington}}
◊;{◊h2{◊xlink{Speakers}}}
◊;{◊h2{◊xlink{Register}}}
◊;{◊h2{◊xlink{Schedule}}}


(seventh RacketCon) is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊gap[1]

◊h3{◊xtarget["speakers"]{◊head["a"]{Keynote}}}

◊head["b"]{Dan Friedman}
◊head["b"]{& Will Byrd}

◊gap[2]

◊;{

Keynote description

◊bio{Speaker bio}


◊gap[1.5]


◊h3{◊xtarget["speakers"]{Speakers}}

◊gap[0]

◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["pollenpub.com"]{Matthew Butterick}}
}

◊gap[0]

◊h3{◊xtarget["register"]{Register}}

Registration is not open yet. Stay tuned!

RacketCon attendees benefit from a ◊link["https://gc.synxis.com/rez.aspx?Hotel=76675&Chain=10069&arrive=10/6/2017&depart=10/8/2017&adult=1&child=0&group=4AM98Y"]{group rate} at the ◊link["http://www.hoteldeca.com/"]{Hotel Deca} near the venue.

◊gap[0]

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2016}
◊conlink{2015}
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}
