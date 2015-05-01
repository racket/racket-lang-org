#lang pollen

◊h1{(fifth RacketCon)}
◊(define rc-2015-date "27 Sept 2015")
◊h2{◊xlink["venue"]{◊rc-2015-date}}
◊h2{◊xlink["venue"]{St. Louis}}
◊h2{◊xlink{Speakers}}
◊;h2{◊xlink{Register}}

◊image["eero.svg"]

◊h3{◊xtarget["venue"]{◊rc-2015-date in St. Louis (the day after ◊link["https://thestrangeloop.com"]{Strange Loop})}}

RacketCon is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊h3{◊xtarget["speakers"]{Keynote speaker}}
◊speaker["" ◊link["www.ccs.neu.edu/home/matthias"]{Matthias Felleisen}]{The Racket Manifesto}


◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["pollenpub.com"]{Matthew Butterick}
◊link["markshead.com"]{Mark Shead}
◊link["http://www.brinckerhoff.org/clements/"]{John Clements}
◊link["robotic.de"]{DLR}}

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}