#lang pollen

◊h1{(fifth RacketCon)}
◊(define rc-2015-date "27 Sept 2015")
◊h2{◊xlink["venue"]{◊rc-2015-date}}
◊h2{◊xlink["venue"]{St. Louis}}
◊h2{◊xlink{Speakers}}
◊h2{◊xlink{Register}}

◊image["eero.svg"]

◊h3{◊xtarget["venue"]{◊rc-2015-date in St. Louis (the day after ◊link["http://thestrangeloop.com"]{Strange Loop})}}

RacketCon is a public meeting for everyone interested in Racket: developers, contributors, programmers, educators, and bystanders. It's an opportunity for all members of the community to come together to share plans, ideas, and enthusiasm. RacketCon will enable the entire Racket community to mingle: to update each other, to exchange ideas, to collaborate, and to help shape the future of Racket.

◊h3{◊xtarget["register"]{Register}}
◊link["https://eventbrite.com/event/16825218682/"]{Via Eventbrite.}

◊h3{◊xtarget["speakers"]{Keynote speaker}}
◊keynote-speaker["" ◊link["www.ccs.neu.edu/home/matthias"]{Matthias Felleisen}]{The Racket Manifesto}

◊h3{◊xtarget["speakers"]{Confirmed speakers}}
◊speaker["" ◊link["https://github.com/sabauma"]{Spenser Bauman}]{JIT Compilation for Racket}
◊speaker["" ◊link["https://github.com/m4burns"]{Marc Burns}]{TBD}
◊speaker["" ◊link["https://twitter.com/daviesaz"]{Byron Davies}]{Rexcel: A Racket-based spreadsheet processing system}
◊speaker["" ◊link["http://codepen.io/Universalist/"]{Jack Firth}]{Generic Syntax Expanders and Extensible Macros}
◊speaker["" ◊link["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt}]{Binding as Sets of Scopes}
◊speaker["" ◊link["https://github.com/florence"]{Spencer Florence}]{Code Coverage Outside of DrRacket}
◊speaker["" ◊link["http://www.ccs.neu.edu/home/types/"]{Ben Greenman}]{A #lang for All Seasons}
◊speaker["" ◊link["http://andmkent.com/"]{Andrew Kent}]{Practical Dependently Typed Racket}
◊speaker["" ◊link["https://github.com/lexi-lambda"]{Alexis King}]{TBD}
◊speaker["" ◊link["https://jeapostrophe.github.io"]{Jay McCarthy}]{Bithoven and the NES Chamber Orchestra}
◊speaker["" ◊link["http://www.math.grin.edu/~rebelsky/"]{Samuel Rebelsky}]{TBD}
◊speaker["" ◊link["https://www.twistedplane.com/"]{Vishesh Yadav}]{TBD}
And more to be announced!


◊h3{Sponsors}

◊inline-list['sponsor]{
◊link["pollenpub.com"]{Matthew Butterick}
◊link["blog.markshead.com"]{Mark Shead}
◊link["robotic.de"]{DLR}
◊link["wearable.com"]{Wearable}}

◊h3{Previous RacketCons}

◊(define (conlink year) 
  (link (format "con.racket-lang.org/~a" year) year))

◊inline-list['con]{
◊conlink{2014}
◊conlink{2013}
◊conlink{2012}
◊conlink{2011}}
